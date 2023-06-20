library(grf)
plot_het <- function(X, predictions) {
    het_plots <- map2(X, names(X), function (x, xname) { #%>%
      # mutate(value = ifelse(x == "dob", as.Date.numeric(value, origin = "1970-01-01"), value))
      out_data <- bind_cols(x, predictions)
      names(out_data) <- c('Variable', 'Prediction')
      
      outplot <- out_data %>%
        ggplot(aes(x = Variable, y = Prediction)) +
        geom_jitter(alpha = 0.1) +
        labs(x = str_wrap(xname, 40), y = "Prediction")
      
      if (length(unique(out_data$Variable)) > 9) {
        outplot +
          geom_smooth(se = FALSE, color = "red")
      } else {
        outplot +
          stat_summary(fun=mean, geom="point", aes(group=1), color ="red", size=3.5)
      }
    })
    
    names(het_plots) <- names(X)
    het_plots
}


multi_outcome_plots <- function(X, predictions) {
  map(1:ncol(predictions), ~plot_het(X, predictions[,.x]))
}

test_det <- function(X, predictions) {
  predictions <- as.data.frame(predictions)
  
  map(predictions, function (prediction) {
    map_df(X, function(x) {
      if (length(unique(x)) != 2) {
        x <- x > median(x, na.rm = TRUE)
      }
      output <- t.test(prediction[x == min(x, na.rm = TRUE)], prediction[x == max(x, na.rm = TRUE)])
      c(output$estimate, output$statistic, output$stderr, output$p.value)
    }) %>% 
      t() %>%
      as.data.frame() %>%
      `names<-`(c("low_est", "high_est", "t", "stderr", "p")) %>%
      mutate(difference =  high_est - low_est) %>%
      rownames_to_column(var = "variable") %>%
      select(variable, difference, t, stderr, p, low_est, high_est)
  })
}

# test <- multi_outcome_plots(morocco[X_vars], indiv_treat$predictions, variable_importance(indiv_treat))

binary_forests <- function(forest) {map(sort(unique(forest$W.orig))[-1], function(x) {
  selector <- forest$W.orig %in% c(1, x)
  
  bin_forest <- causal_forest(forest$X.orig[selector,], 
                              as.numeric(forest$Y.orig)[selector], 
                              as.numeric(forest$W.orig)[selector],
                              Y.hat = NULL , W.hat = NULL,
                              num.trees = 1000
  )
  
  # bin_forest$selector <- selector

  return(bin_forest)
})
}


