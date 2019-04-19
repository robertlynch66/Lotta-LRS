age_seq <- seq (from = 0, to = 1, length.out=32)
# make a matrix of zeroes to replace the varying intercepts
a_bplace_zeroes <- (matrix(0,1000,938))
# time to repro
attach(ttr)
lotta_time_to_repro <- tidyr::crossing(
  birth_cat = 1L, # the "L" makes the value an integer, avoiding possible errors
  age = 0.096,
  lotta = c(0L),
  birthplaceid_seq =1,
  agriculture = mean(agriculture),
  education = mean(education),
  repro_within_2_years = mean(repro_within_2_years)) %>%
  as.data.frame()
detach(ttr)
library(tidybayes.rethinking)

preds <- tidy_link(lotta_time_to_repro, M1, replace=list(birthplace_id_seq=a_bplace_zeroes)) %>% as.data.frame()


# get the prediction 
mean(preds$lambda)
PI(preds$lambda)
# put ages back on number scale


# birth ints
#By default crossing in tidyr package makes a tibble, so you have to convert
#back to data frame.
# 0.7=55,0.6=45,0.5=35,0.35=25,0.28=15,0.21=5
attach(ibi)
mean_ibi <- tidyr::crossing(
  birth_cat = 1,
  repro_cat = mean(repro_within_2_years),# the "L" makes the value an integer, avoiding possible errors
  age = 21,
  lotta = c(1L),
  birthplaceid_seq =1,
  agriculture = mean(agriculture),
  education = mean(education),
  repro_within_2_years = mean(repro_within_2_years)) %>%
  as.data.frame()
detach(ibi)
library(tidybayes.rethinking)

preds <- tidy_link(mean_ibi, M2,replace=list(birthplace_id_seq=a_bplace_zeroes)) %>% as.data.frame()

mean(preds$lambda)
PI(preds$lambda)
# total kids post war
# 0.7=55,0.6=45,0.5=35,0.35=25,0.28=15,0.21=5
attach(data)
lotta_kids_after_war <- tidyr::crossing(
  birth_cat = mean(birth_cat), # the "L" makes the value an integer, avoiding possible errors
  age = 21,
  lotta = c(1L),
  birthplace_id =1,
  agriculture = mean(agriculture),
  education = mean(education),
  repro_cat = mean(repro_within_2_years)) %>%
  as.data.frame()
detach(data)
library(tidybayes.rethinking)

preds <- tidy_link(lotta_kids_after_war, Kids_after_war_includes_non_repros,replace=list(birthplace_id_seq=a_bplace_zeroes))
mean(preds$lambda)
PI(preds$lambda)

%>% as.data.frame()) %>% as.data.frame()