R_LIBS_SITE="C:\\Program Files\\R\\R-4.2.2\\library"
 
#install.packages("remotes")
#remotes::install_github("ygeunkim/propensityml")
#install.packages("dplyr")
#install.packages("sas7bdat")

library(propensityml)
library(dplyr)
library(sas7bdat)

#dataset 불러오기
raw_data<- read.sas7bdat("C:/Users/Kyungseon/snu_causal/main/input/rawdata/HN20_ALL.sas7bdat")
dim(raw_data) %>% head()
raw_data %>% head(1)

column=[]

install.packages("base64enc", repos = "https://cran.rstudio.com/")
