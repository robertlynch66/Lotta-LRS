---
title: "README.md"
author: "Robert Lynch"
date: "April 19, 2019"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

## R Markdown

This is an R Markdown document. Markdown is a simple formatting syntax for authoring HTML, PDF, and MS Word documents. For more details on using R Markdown see <http://rmarkdown.rstudio.com>.

When you click the **Knit** button a document will be generated that includes both content as well as the output of any embedded R code chunks within the document. You can embed an R code chunk like this:

# Installation of STAN and Rethinking

You'll need to install rstan first. Go to http://mc-stan.org and follow the instructions for your platform. The biggest challenge is getting a C++ compiler configured to work with your installation of R. The instructions at https://github.com/stan-dev/rstan/wiki/RStan-Getting-Started are quite thorough. Obey them, and you'll likely succeed.

Then you can install rethinking from within R using:
```{r}
install.packages(c("coda","mvtnorm","devtools","loo"))
library(devtools)
devtools::install_github("rmcelreath/rethinking")
```


## Usage on random sample of 200 rows to make sure everything is working

Read in random sample of actual data (200 random rows from data frame) - from dataframe 'data_sample.rds'
See 'simple exmaple.R' for code

Next make data list from data frame:

- Use these data to run a 'map2stan' model in the 'rethinking' package in R

Code in 'simple example.R' file uses 4 chains with 200 iterations and 100 warmup iterations.

Warnings will be shown but code should run if Stan and rethinking were properly installed in previous steps 

-expected output
```{r output}
 precis(model)
101 vector or matrix parameters omitted in display. Use depth=2 to show them.
                        Mean StdDev lower 0.89 upper 0.89 n_eff Rhat
Intercept               0.15   0.18      -0.12       0.46   319 0.99
b_age                  -0.42   0.28      -0.82       0.07   249 0.99
b_birth_cat             0.63   0.15       0.39       0.88   382 1.00
b_education            -0.24   0.26      -0.62       0.17   277 1.01
b_agriculture           0.40   0.13       0.22       0.62   411 1.00
b_repro_within_2_years  0.32   0.17       0.07       0.59   608 1.00
b_lotta_X_age          -0.37   0.52      -1.18       0.41    78 1.02
sigma                   0.21   0.09       0.06       0.34     6 1.56
b_lotta                -0.08   0.26      -0.49       0.31   132 1.01
```
#### Main Full models
For Full models and data use data frame 'data.rds' for time to reproduction models and mean interbirth intervals models
( reproductive women only) and use 'data2.rds' for all total reproductive after war models (includes non reprodcutive women)
For 3 main models use 'main_models.R' code which is heavily commnented.

#### All other analyses
Use 'Final models in rethinking.R' which is externsively commented for all analyses used in the paper including survival models.

Use 'sisters code.R' for analysis with subset of linked data only using sisters,
This file is also extensively commented.