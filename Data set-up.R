
library(tidyverse)
library(magrittr)
library(haven)
library(grf)
library(car)
# library(pcaMethods)
library(cowplot)
library(stargazer)
# library(lm.beta)

maths <- read_dta("LCT Morroco/cct_aser_an.dta")
baseline <- read_dta("LCT Morroco/cct_baseline_an.dta")
endline <- read_dta("LCT Morroco/cct_endline_an.dta")
school_baseline <- read_dta("LCT Morroco/cct_preliminary_survey_an.dta")
dropout <- read_dta('LCT Morroco/cct_school_visits_an.dta')
admin_data <- read_dta('LCT Morroco/cct_tayssir_admin_data_an.dta')

dropouts <- dropout %>% 
  select(ends_with('_c12'))

dropouts1 <- dropouts == 2
dropouts2 <- rowSums(dropouts1, na.rm = TRUE) # Importantly excluding NAs
dropouts3 <- dropouts2 > 0
dropout$any_drops <- dropouts3

joined <- baseline %>%
  left_join(endline, by = "hhid", suffix = c("", ".y")) %>%
  left_join(maths, by = "hhid", suffix = c("", ".y")) %>%
  left_join(school_baseline, by = "schoolid", suffix = c("", ".y")) %>%
  select_at(
    vars(-ends_with(".y"))
  )

baseline %<>%
  mutate(schoolunitid = factor(schoolunitid),
         group = car::recode(group, "0 = NA;"),
         group_cond = car::recode(group, "1=1; else=2")
  )



# Get hhh variables
intermediate <- baseline %>%
  select(hhid, matches("^a[1-9]")) %>%
  gather(-hhid, key = "question", value = "value") %>%
  tidyr::separate(question, c("question", "person", "response"), sep = "_") 

temp <- intermediate %>%
  filter(question == 'a4' |
           question == 'a13' |
           (question == 'a15' & response == 1) |
           (question == 'a3' & is.na(response))
  ) %>%
  select(-response) %>%
  spread(question, value) %>%
  filter(a3 == 1) %>%
  select(hhid, a4, a13, a15)



week_to_month <- function (x) {
  (x / 7) * 30
}

year_to_month <- function (x) {
  x / 12
}


expenditure <- baseline %>%
  select(starts_with('h1_'),
         starts_with('h3_'),
         starts_with('h4_')
  ) %>%
  select_if(is.numeric) %>%
  mutate_at(
    vars(starts_with('h1_')),
    week_to_month
  ) %>%
  mutate_at(
    vars(starts_with('h4_')),
    week_to_month
  )

expenditure[expenditure < 0] <- 0

baseline$monthly_spending <- rowSums(expenditure, na.rm = TRUE) %>% log()

num_kids <- baseline %>%
  select(hhid, matches("^d1_[0-9]{1,2}")) %>%
  gather(-hhid, key = "question", value = "value") %>%
  mutate(hhid = hhid %>% as_factor() %>% as.character()) %>%
  group_by(hhid) %>%
  dplyr::summarise(est_num_kids = sum(value == 1, na.rm = T))


assets <- baseline %>%
  select(b10_5, b10_6, b10_20, b10_26,  b10_21, b10_28, b4_1, b5_1, b9_5, monthly_spending) %>%
  mutate(b4_1 = car::recode(b4_1, "1:3 = 1; 4:5 = 2; -99 = NA"),
         b5_1 = car::recode(b5_1, "1:3 = 1; 5 = 1; 4 = 2; 6 = 2; -99 = NA"),
         b9_5 = car::recode(b9_5, "1 = 1; 2 = 2; -99:-1 = NA")
  )

assets[assets==-99]<-NA

maths_results <- rowSums(maths[c('t6', 't5_1', 't4_1', 't3_1')])




joined1 <- baseline[c('hhid', 'group', 'group_cond', 'monthly_spending')] %>%
  left_join(num_kids, by = "hhid") %>%
  left_join(temp, by = "hhid") %>%
  filter(!duplicated(hhid))

joined2 <- baseline %>%
  left_join(num_kids, by = "hhid") %>%
  left_join(temp, by = "hhid") %>%
  filter(!duplicated(hhid))

# joined1$PCA <- pca_results@scores %>% c()

maths$maths_results <- rowSums(maths[c('t6', 't5_1', 't4_1', 't3_1')])

joined1 %<>%
  left_join(maths, by = "hhid")

joined2 %<>%
  left_join(maths, by = "hhid")

# Add endline ID to be able to join to maths data
joined1 %<>%
  dplyr::inner_join(endline[c('hhid', 'hhid_endline')], by = 'hhid', suffix = c("", ".y")) %>%
  inner_join(admin_data[c('hhid_endline', 'stud_id_tayssir', 'datenaiseleve', 'genre_el')], by = 'hhid_endline', suffix = c("", ".y")) %>%
  inner_join(dropout[c('stud_id_tayssir', 'any_drops')], by = "stud_id_tayssir", suffix = c("", ".y")) %>%
  select_at(
    vars(-ends_with(".y"))
  )

joined2 %<>%
  dplyr::inner_join(endline[c('hhid', 'hhid_endline')], by = 'hhid', suffix = c("", ".y")) %>%
  inner_join(admin_data[c('hhid_endline', 'stud_id_tayssir', 'datenaiseleve', 'genre_el')], by = 'hhid_endline', suffix = c("", ".y")) %>%
  inner_join(dropout[c('stud_id_tayssir', 'any_drops')], by = "stud_id_tayssir", suffix = c("", ".y")) %>%
  select_at(
    vars(-ends_with(".y"))
  )

covariates <- c('hhh_gender', 'hhh_age', 'monthly_spending', 'any_drops', 'hhh_literacy', 'age', 'gender', 'hhid', 'group', 'group_cond', 'benef')

X_vars <- c('hhh_gender', 'hhh_age', 'monthly_spending', 'hhh_literacy', 'age', 'gender', 'benef')

joined1 %<>%
  dplyr::rename(hhh_age = a13,
                hhh_gender = a4,
                hhh_literacy = a15,
                age = datenaiseleve,
                gender = genre_el,
                group = group.x)

joined1 <- joined1[covariates]



joined_num <- joined1 %>%
  #   mutate(cluster = as.factor(hhid)) %>%
  mutate(gender = factor(gender),
         hhid = as.factor(hhid),
         group = factor(group),
         benef = factor(benef)
  ) %>%
  mutate(age = difftime(as.Date("2010-01-01"), age)) %>%
  mutate_at(vars(-hhid), as.numeric) %>%
  mutate(any_drops = (any_drops - 1) * -1)

joined_num[joined_num < 0] <- NA
joined_num <- joined_num[complete.cases(joined_num),]


joined_num2 <- joined2 %>%
dplyr::rename(hhh_age = a13,
              hhh_gender = a4,
              hhh_literacy = a15,
              age = datenaiseleve,
              gender = genre_el,
              group = group.x,
              benef = benef.x) %>%
  #   mutate(cluster = as.factor(hhid)) %>%
  mutate(gender = factor(gender),
         hhid = as.factor(hhid),
         group = factor(group),
         benef = factor(benef)
  ) %>%
  mutate(age = difftime(as.Date("2010-01-01"), age)) %>%
  mutate_at(vars(-hhid), as.numeric) %>%
  mutate(any_drops = (any_drops - 1) * -1)

joined_num2[joined_num2 < 0] <- NA


X <- joined_num[X_vars]

joined_num %>% write_rds('morocco.rds')
joined_num2 %>% write_rds('morocco2.rds')
