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


## Usage on random sample to make sure evfrything is working

Read in random sample of actual data (200 random rows from data frame) - from dataframe 'data_sample.rds'
See 'simple exmaple.R' for code

Next make data list from data frame

- use these data to run 'map2stan' model in rethinking

Code in 'simple example.R' file uses 4 chains with 200 iterations and 100 warmup iterations.

Warnings will be shown but code should run if Stan and rethinking were properly installed in previous steps 

-expected output
```{r output}
> precis(model)
101 vector or matrix parameters omitted in display. Use depth=2 to show them.
                        Mean StdDev lower 0.89 upper 0.89 n_eff Rhat
Intercept               0.34   0.33      -0.12       0.93   150 1.01
b_age                  -0.01   0.01      -0.03       0.00   154 1.01
b_birth_cat             0.64   0.15       0.40       0.85   295 1.00
b_education            -0.24   0.26      -0.64       0.21   303 1.00
b_agriculture           0.40   0.13       0.18       0.60   515 1.00
b_repro_within_2_years  0.33   0.16       0.07       0.58   591 1.00
b_lotta_X_age          -0.02   0.02      -0.05       0.01   109 1.01
sigma                   0.22   0.09       0.09       0.36    10 1.63
b_lotta                 0.23   0.53      -0.65       0.94    96 1.02
```
#### Full models
For Full models and data use data frame 'person_data.rds' and for all models run for analyses use 
'Final models in rethinking.R' file which is extrensively commented