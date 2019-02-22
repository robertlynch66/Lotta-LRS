modelB <- readRDS("Birth_ints_w_censored_data.rds")
install.packages ("bayesplot")


install.packages("tidyr")
install.packages("ggpubr")
install.packages("cowplot")
install.packages("gridExtra")
install.packages("ggplot2")
library ("bayesplot")
library(rethinking)
library(dplyr)
library(tidyr)
library(ggpubr)
library(cowplot)
library(gridExtra)
library(ggplot2)

# get posteriors and select columns
post_modelB <- extract.samples(modelB) %>% as.data.frame()

saveRDS(post_modelB,"results/birth_ints_posteriors")


# get the fucker down to a manageable size
birth_ints_posteriors2 <- readRDS("C:/Users/robert/Dropbox/Github/Lottas_2/model results/birth_ints_posteriors2.rds")
post_model1 <- birth_ints_posteriors %>% select (1:6)
post_model1 <- post_model1 [c(1,4,5,2,3)]

#post_model1 <- post_model1 %>% rename(Age = ba, Agriculture= bag, Educated=bed, Sisters=bs, Brothers=bb)

color_scheme_set("blue")
p1 <- mcmc_areas(post_model1,prob = 0.8, prob_outer = 1)
p1


path<- (paste0("results/"))
filename <- "Trump_16_model.rds"

saveRDS(model, paste0(path, filename))
post_model1 <- post_model1 %>% select (1:5)
post_model1 <- post_model1 [c(1,4,5,2,3)]


###
attach(p1)
bros <- tidyr::crossing(
  age = mean(age),
  brothers = c(0L,1L,2L,3L,4L,5L),
  education=mean(education),
  never_married=0L,
  agriculture = mean(agriculture),
  sisters = mean(sisters)) %>%
  as.data.frame()
detach(p1)
library(tidybayes.rethinking)

df_bros <- tidy_link(bros, model_1) %>% as.data.frame()