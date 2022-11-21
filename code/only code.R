
## Improving propensity score weighting using machine learning.
## Package ('propensityml')

## (1) Installation
library(stats)
library(magrittr)
library(data.table)
library(Matrix)
library(tibble)
library(dplyr)
library(randomForest)
library(rpart)
library(e1071)
library(stringr)
library(mvtnorm)
library(rlang)
library(ggplot2)
library(foreach)
library(parallel)
library(remotes)
library(covr)
library(testthat)
library(readr)
library(usethis)
remotes::install_github("ygeunkim/propensityml", force=TRUE)

library(propensityml)

## Dataset
devtools::install_github("r-lib/usethis")
chemical <-
  readr::read_table(
    "C:/Users/sarah/Desktop/대학원 2학기/응용통계세미나/프로젝트/poisox.txt",
    col_names = c("age", "sex", "prior", "poisox", "after", "mortal")
  ) %>%
  dplyr::mutate(
    sex = factor(sex),
    poisox = factor(poisox),
    blood = after - prior,
    mortal = factor(mortal)
  ) %>%
  dplyr::select(-after, -prior)

chemical

## weighting.R : add_propensity, add_ipw_wt, compute_sipw, add_weighting

### add_propensity : adds propensity score to a data frame

chemical

data = chemical





