library(tidyverse)
library(readxl)
library(purrr)
library(lubridate)
library(xts)
library(timetk)
#library(asrsMethods)

# read the configuration 
apt_sheets=excel_sheets("modera_decatur.xlsx")

apt_config=apt_sheets %>%
  as.list() %>%
  map(~read_excel("modera_decatur.xlsx",sheet=.x))

apt_config=set_names(apt_config,apt_sheets)      
specsid=apt_config[["Identification"]]
model_length=filter(specsid,name=="model_length")$n_month

#calculate unlevered cash flows

#predevelopment
predevelopment = list()
specs=apt_config[["Predevelopment"]]
delay=filter(specs,name=="Delay")$n_month
specs=mutate(specs,rev_end_month=end_month+delay)
monthsdur=max(specs$rev_end_month,na.rm=TRUE)
time_index=filter(specs,name=="Start_date")$date+
  months(0:(-1+monthsdur))
Start_date=time_index[1]
total_time_index=Start_date+months(0:(model_length-1))
#monthly items
specsub=filter(specs,!is.na(value_per_mth))
templist=map(as.list(specsub$value_per_mth),
                      ~xts(rep(.x,monthsdur),time_index)) %>%
  set_names(specsub$name)
predevelopment=c(predevelopment,templist)
#predevelopment budget
specsub=filter(specs,!is.na(value))
specsub=specsub %>% 
  mutate(ndur=1+rev_end_month-start_month) %>% 
  mutate(cost_per_mth=value/ndur)
templist=map(as.list(1:(nrow(specsub))),
             ~xts(rep(specsub$cost_per_mth[.x],specsub$ndur[.x]),
                  time_index[specsub$start_month[.x]:(specsub$rev_end_month[.x])])) %>%
  set_names(specsub$name)
predevelopment=c(predevelopment,templist)
permit_issued=time_index[1]+months(filter(specs,name=="Permit_issued")$rev_end_month)
#construction
construction=list()
specs=apt_config[["Construction"]]
delay=filter(specs,name=="Delay")$n_month
overrun=1+filter(specs,name=="Cost_overrun")$pct
specs=mutate(specs,rev_end_month=end_month+delay)
monthsdur=max(specs$rev_end_month,na.rm=TRUE)
time_index=permit_issued+ months(1:monthsdur)
#monthly items
specsub=filter(specs,!is.na(value_per_mth))
templist=map(as.list(specsub$value_per_mth*overrun),
             ~xts(rep(.x,monthsdur),time_index)) %>%
  set_names(specsub$name)
construction=c(construction,templist)
#construction budget
specsub=filter(specs,!is.na(value))
specsub=specsub%>%
  mutate(dur=end_month-start_month+1) %>%
  mutate(cost_per_mth=overrun*value/dur)
templist=map(as.list(1:nrow(specsub)),
             ~xts(rep(specsub$cost_per_mth[.x],specsub$dur[.x]),
                  time_index[(specsub$start_month[.x]):(specsub$end_month[.x])])) %>%
  set_names(specsub$name)
construction=c(construction,templist)
CofO_date=tail(time_index,1)
#revenue
#ignores any category in configuration file beyond 3 items of apt, com and other
#future upgrade possible
specs=apt_config[["Revenue"]]
apt_sf=filter(specsid,name=="apt_sf")$num
com_sf=filter(specsid,name=="com_sf")$num
rent_sensitivity=1+filter(specs,name=="revenue_sensitivity")$pct
specstrend=apt_config[["RentTrend"]]
months_so_far=interval(Start_date,CofO_date) / months(1)
months_to_go=model_length-months_so_far
op_time_index=CofO_date+months(1:months_to_go)
rentindex=exp(cumsum(log(rep(1+specstrend$pct_per_yr/12,specstrend$n_month))))
rentindex=xts(rentindex,total_time_index)
apt_rent_rate=xts(rentindex*filter(specs,name=="apt_rent")$rent_psf_mth,total_time_index)
com_rent_rate=xts(rentindex*filter(specs,name=="com_rent")$rent_psf_mth,total_time_index)
other_rev_per_mth=xts(rentindex*filter(specs,name=="other_rev")$value_per_month,
                      total_time_index)
apt_occ_stable=1-filter(specs,name=="apt_rent")$vac_pct
com_occ_stable=1-filter(specs,name=="com_rent")$vac_pct
apt_lease_mths=filter(specs,name=="apt_rent")$leaseup_mths
com_lease_mths=filter(specs,name=="com_rent")$leaseup_mths
apt_occ=c(apt_occ_stable/apt_lease_mths*(1:apt_lease_mths),
          rep(apt_occ_stable,months_to_go-apt_lease_mths))
apt_occ=xts(apt_occ,op_time_index)
com_occ=c(com_occ_stable/com_lease_mths*(1:com_lease_mths),
          rep(com_occ_stable,months_to_go-com_lease_mths))
com_occ=xts(com_occ,op_time_index)
apt_rent=apt_occ*apt_rent_rate*apt_sf*rent_sensitivity
com_rent=com_occ*com_rent_rate*com_sf*rent_sensitivity
other_rev=apt_occ*other_rev_per_mth*rent_sensitivity
total_rent=apt_rent+com_rent+other_rev
Revenue=list(apt_rent=apt_rent,com_rent=com_rent,other_rev=other_rev)


#Expenses
specs=apt_config[["Expense"]]
specscap=apt_config[["CapMarkets"]]
cpi=filter(specscap,name=="cpi")$pct_per_year
expense_sensitivity=1+filter(specs,name="expense_sensitivity")$pct
Expense=list()
expnames=vector()
for(i in 1:nrow(specs)) {
  tempxts=NULL
  if(specs$name[i]=="expense_sensitivity") next(i)
  increase=eval(parse(text=specs$increase[i]))
  if(is.na(increase)) {
    increase=1
  } else {
    increase=rep(log(1+increase)/12,model_length)
    increase=exp(cumsum(increase))
    increase=xts(increase,total_time_index)
    increase=increase[-1:-months_so_far]
  }
  if(!is.na(specs$value_per_mth[i])) {
    tempxts=xts(increase*specs$value_per_mth)
  }
  if(!is.na(specs$value_per_yr[i])) {
    mpay=specs$annual_exp_mth_paid[i]
    idx=op_time_index[month(op_time_index)==mpay]
    tempxts=increase[idx]*specs$value_per_yr[i]
  }
  if(!is.na(specs$value_per_sf[i])) {
    persf=specs$value_per_sf[i]
    sf=filter(specsid,name==specs$sf_id[i])$num
    tempxts=increase*persf*sf
  }
  if(!is.na(specs$pct_of_rev[i])) {
    tempxts=specs$pct_of_rev[i]*total_rent
  }
  Expense=c(Expense,list(tempxts*expense_sensitivity))
  expnames=c(expnames,specs$name[i])
}
Expense=set_names(Expense,expnames)


