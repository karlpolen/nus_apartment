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
specsub=filter(specs,!is.na(value_per_mth))
templist=map(as.list(specsub$value_per_mth),
                      ~xts(rep(.x,n_month),time_index)) %>%
  set_names(specsub$name)
predevelopment=c(predevelopment,templist)
#predevelopment budget
specsub=filter(specs,!is.na(value),
                    !is.na(n_month))
templist=map(as.list(specsub$value),
             ~xts(rep(.x/n_month,n_month),time_index)) %>%
  set_names(specsub$name)
predevelopment=c(predevelopment,templist)
#one time items
specs_onetime=filter(specs,!is.na(value),
                    !is.na(month_incur))
templist=map(as.list(1:nrow(specsub)),
             ~xts(specsub$value[.x],time_index[specsub$month_incur[.x]])) %>%
  set_names(specs_onetime$name)
predevelopment=c(predevelopment,templist)
permit_issued=time_index[1]+months(filter(specs,name=="Permit_issued")$n_month)
#construction
construction=list()
specs=apt_config[["Construction"]]

time_index=permit_issued+months(0:(-1+filter(specs,name=="Total_time")$end_month))
specsub=filter(specs,!is.na(value))
specsub=specsub%>%
  mutate(dur=end_month-start_month+1) %>%
  mutate(cost_per_mth=value/dur)
templist=map(as.list(1:nrow(specsub)),
             ~xts(rep(specsub$cost_per_mth[.x],specsub$dur[.x]),
                  time_index[(specsub$start_month[.x]):(specsub$end_month[.x])])) %>%
  set_names(specsub$name)
construction=c(constrction,templist)