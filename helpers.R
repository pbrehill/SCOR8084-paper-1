library(policytree)
library(labelled)
library(fuzzyjoin)


impute_covars <- function(X) {
  X_imp <- X %>%
    missMethods::impute_median()
  
  X_imp[, colSums(is.na(X_imp)) < nrow(X_imp), drop = FALSE]
}


create_cowplot <- function(plot_data, variable_list, tau1) {
  if(!is.data.frame(tau1)) tau1 <- as.data.frame(tau1)
  
  num_treats <- ncol(tau1)
  plots <- vector(mode = "list", length = num_treats * length(variable_list))
  
  # Use an inner and outer map to map over variables then treatments within variables
  new_plots <- map(
    variable_list, 
    function(variable) {
      # Set number of options in question for later use
      num_options <- length(unique(pull(plot_data[variable])))
      map(tau1, function(tau) {
        if (num_options > 10) {
          ggplot(data = plot_data, aes(x=!!rlang::sym(variable), y = tau)) +
            geom_jitter(alpha = 0.5) +
            geom_smooth(color = "red") +
            geom_hline(yintercept = 0, color = "blue", linetype = "dashed") +
            xlab(variable)
          
        } else {
          ggplot(data = plot_data, aes(x= !!rlang::sym(variable), y = tau)) +
            geom_jitter(alpha = 0.5) +
            geom_violin(aes(group = !!rlang::sym(variable)), alpha = 0.25) +
            stat_summary(fun = "mean",
                         geom = "point",
                         color = "red",
                         size = 6) +
            geom_hline(yintercept = 0, color = "blue", linetype = "dashed") +
            xlab(variable)
        }
      })
    }
  )
  

  plot_grid(plotlist = new_plots %>% unlist(recursive = F), 
            ncol = num_treats)
}


qinish <- function(forest, treat_costs, learning_vars = NULL) {
  # Get net benefit (benefit minus cost)
  treat_tau <- forest$predictions %>% as.data.frame()
  
  dfs <- map(treat_tau, function(tau) {
    if (!is.vector(treat_tau)) treat_tau <- pull(treat_tau)
    if (length(treat_costs) == 1) treat_costs <- rep.int(treat_costs, length(treat_tau))
    cb_ratio = treat_costs / treat_tau
    cb_ratio[cb_ratio < 0] <- Inf
    df <- data.frame(cb_point = cb_ratio, port_treat = 0)
    for (i in 1:length(cb_ratio)) {
      selector <- cb_ratio[i] <= cb_ratio
      df[i, "port_treat"] <- sum(selector) / length(cb_ratio)
    }
    df
  })
  
  names(dfs) <- paste0("treat", ncol(treat_tau))
  
  first_degree <- bind_rows(dfs, .id = "treatment")
  
  # Look at policy learner
  
  # Get covariates
  if (is.null(learning_vars)) {
    learning_vars <- forest$X.orig
  }
  
  # Get DR scores
  dr_scores <- double_robust_scores(forest)
  
  # Rerun analysis for learnt policy
  first_degree %>%
    mutate(
      pct_treated = hybrid_policy_tree(
        learning_vars %>% impute_covars(),
        dr_scores[,c(1, as.numeric(treatment) + 1)],
        search.depth = 1
      ) %>%
        predict(learning_vars %>% impute_covars()) %>%
        as.tibble() %>%
        mutate(dr = dr_scores[,c(1, as.numeric(treatment) + 1)]) %>%
        filter(value == 2) %>%
        nrow()
        ,
      pct_treated1 = pct_treated / nrow(treat_tau)
    )
}



# Join maths to baseline based on names
join_on_names <- function(baseline, endline, maths) {

  maths$hhid_endline <- maths$hhid_endline %>%
    as_factor() %>%
    as.character()
  maths %>%
    filter(!is.na(hhid_endline) & hhid_endline != "")
  
  endline$hhid_endline <- endline$hhid_endline %>%
    as_factor() %>%
    as.character()
  
  baseline$endline
  
  out_df <- endline %>%
    filter(!is.na(hhid_endline) & hhid_endline != "") %>%
    labelled::look_for_and_select("Child's first name") %>%
    bind_cols(endline["hhid_endline"]) %>%
    gather(key = "Variable", value = "Name", -hhid_endline) %>%
    left_join(maths, by = "hhid_endline") %>%
    group_by(hhid_endline) %>%
    mutate(str_dist = stringdist::stringdist(prenom_enf_test, Name, method = "lv") / nchar(prenom_enf_test)) %>%
    arrange(str_dist) %>%
    slice(1) %>%
    ungroup()
    
  out_df_filtered <- out_df %>%  
    filter(str_dist <= 0.25)
  
  if (nrow(out_df) != nrow(out_df_filtered)) {
    warning(paste0("Some rows did not have a valid match at threshold of 0.25. ",
                   nrow(out_df) - nrow(out_df_filtered),
                   " rows did not have a match."))
  }
  
  out_df_filtered
  
}


gather_w_labels <- function(data, key = "key", value = "value") {
  labels <- data %>% 
    labelled::get_variable_labels() %>%
    unlist()
  
  gathered_data <- data %>%
    gather(key, value, -hhid)
  
  gathered_data$label <- labels[gathered_data$key]
  gathered_data
}


get_vars_by_regex <- function(df, selector, num_selector, person_num, suffix = "_self") {
  df[paste0(person_num, "join")] <- df[person_num] %>% 
    pull() %>%
    as.numeric()
  
  df %>%
    select(hhid, matches(selector)) %>%
    gather("name", "value", -hhid)%>%
    transmute(value = value, 
              person = str_extract(name, num_selector) %>% str_extract("[:digit:]+") %>% as.numeric(),
              variable = str_replace(name, num_selector, ""),
              variable = paste0(variable, suffix),
              hhid = hhid
              ) %>%
    pivot_wider(names_from = variable, values_from = value) %>%
    right_join(df, by = c("hhid", "person" = paste0(person_num, "join")))
}



