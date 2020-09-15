library(tidyverse)
library(readxl)
library(purrr)
library(lubridate)
library(xts)
#library(asrsMethods)

# read the configuration 
apt_sheets=excel_sheets("modera_decatur.xlsx")

apt_config=apt_sheets %>%
  as.list() %>%
  map(~read_excel("modera_decatur.xlsx",sheet=.x))

apt_config=set_names(apt_config,apt_sheets)      

#calculate unlevered cash flows

#predevelopment
predevelopment = list()
specs=apt_config[["Predevelopment"]]
n_month=filter(specs,name=="Permit_issued")$n_month
time_index=filter(specs,name=="Start_date")$date+
  months(0:(-1+n_month))
#monthly items
specs_monthly=filter(specs,!is.na(value_per_mth))
templist=map(as.list(specs_monthly$value_per_mth),
                      ~xts(rep(.x,n_month),time_index)) %>%
  set_names(specs_monthly$name)
predevelopment=c(predevelopment,templist)
#predevelopment budget
specs_predev=filter(specs,!is.na(value),
                    !is.na(n_month))
templist=map(as.list(specs_predev$value),
             ~xts(rep(.x/n_month,n_month),time_index)) %>%
  set_names(specs_predev$name)
predevelopment=c(predevelopment,templist)
#one time items
specs_onetime=filter(specs,!is.na(value),
                    !is.na(month_incur))
templist=map(as.list(1:nrow(specs_onetime)),
             ~xts(specs_onetime$value[.x],time_index[specs_onetime$month_incur[.x]])) %>%
  set_names(specs_predev$name)
predevelopment=c(predevelopment,templist)



