library(tidyverse)
library(readxl)
library(purrr)
library(lubridate)
library(xts)
library(timetk)
#library(asrsMethods)

#xts convenience function
mergesum.xts=function(x) {
  x=do.call(cbind,x)
  idx=index(x)
  xsum=rowSums(x,na.rm=TRUE)
  xts(xsum,idx)
}

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
expense_sensitivity=1+filter(specs,name=="expense_sensitivity")$pct
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

# Cap Ex

CapEx=list()
capexnames=vector()
specs=apt_config[["CapEx"]]
specs=mutate(specs,v_per_mth=Value/n_month_constr)
ytg=floor(months_to_go/12)
for(i in 1:nrow(specs)) {
  life_yr=specs$n_year_life[i]
  constr_mth=specs$n_month_constr[i]
  v_per_mth=specs$v_per_mth[i]
  incr=eval(parse(text=specs$Increase[i]))
  nproj=floor(ytg/life_yr)
  if(nproj==0) next(i)
  projdate=CofO_date+years(life_yr*(1:nproj))
  projdateym=projdate
  for (j in 1:(constr_mth-1)) {
    projdateym=c(projdateym,projdate+months(j))
  }
  projdateym=sort(projdateym)
  incr=exp(cumsum(rep(log(1+incr/12),model_length)))
  incr=xts(incr,total_time_index)
  tempxts=incr[projdateym]*v_per_mth
  CapEx=c(CapEx,list(tempxts))
  capexnames=c(capexnames,specs$name[i])
}
CapEx=set_names(CapEx,capexnames)

#working capital
#set cash = max(500,000 or this months revenue)

end_date=Start_date+months(model_length)
proj_interval=interval(Start_date,end_date)
fsdates=ymd(paste(year(Start_date),month(Start_date),1))
num=ceiling(proj_interval/months(1))
fsdates=fsdates+months(1:num)-days(1)
fsdates=fsdates[fsdates %within% proj_interval]
fsdates.x=xts(rep(NA,length(fsdates)),fsdates)
fsdates.zero=xts(rep(0,length(fsdates)),fsdates)
cashbal=mergesum.xts(list(fsdates.zero,total_rent))
ep=endpoints(cashbal,"months")
cashbal=period.apply(cashbal,INDEX=ep,FUN=sum)
cashbalmin=xts(rep(500000,length(fsdates)),fsdates)
cashbal=pmax(cashbal,cashbalmin)
wc_change=diff(cashbal)
wc_change[1]=cashbal[1]
wc=list(wc_change,cashbal)



#create an xts array of unlevered cashflows
cash_obj=merge(mergesum.xts(Revenue),
               -mergesum.xts(predevelopment),
               -mergesum.xts(construction),
               -mergesum.xts(Expense),
               -mergesum.xts(CapEx),
               -wc_change)
cash_obj=cbind(cash_obj,rowSums(cash_obj,na.rm=TRUE))
colnames(cash_obj)=c("Revenue","Predevelopment","Construction","OpEx","CapEx","WorkingCapital","Unlv_CF")


#construction loan

specs=apt_config[["ConstructionLoan"]]
specspl=apt_config[["PermanentLoan"]]
max_loan=filter(specs,name=="max_loan")$value
LTC=filter(specs,name=="LTC")$pct
cl_start=permit_issued
cl_end=CofO_date+months(filter(specspl,name=="CofO_lag")$n_month)
cl_interval=interval(cl_start,cl_end)
cf=cash_obj$Unlv_CF
pdates=ymd(paste(year(cl_start),month(cl_start),1))
num=ceiling(cl_interval/months(1))
pdates=pdates+months(1:num)-days(1)
pdates=pdates[pdates %within% cl_interval]
pdates.x=xts(rep(0,length(pdates)),pdates)
cf=mergesum.xts(list(cf,pdates.x))
cf.d=coredata(cf)
cf.t=index(cf)
int=filter(specscap,name=="Const_loan_rate")$pct_per_year
if(cf.t[1] %within% cl_interval & cf.d[1]<0) {
  draw=-LTC*cf.d[1]
  loanbal=draw
} else {
  loanbal=0
  draw=0
}
cumcost=max(0,-cf.d[1])
cumeq=cumcost-draw[1]
interest=0
accrint=0
for(i in 2:length(cf)) {
  cumcost=cumcost-cf.d[i]
  ndays=as.numeric(cf.t[i]-cf.t[i-1])
  interest[i]=loanbal[i-1]*ndays/365*int
  cumcost=cumcost+interest[i]
  accrint=accrint+interest[i]
  if(!cf.t[i] %within% cl_interval) {
    loanbal[i]=0
    draw[i]=-(loanbal[i-1]+accrint)
    accrint=0
    next(i)
  }
  max_loan_i=min(max_loan,LTC*cumcost)
  max_draw_i=max_loan_i-loanbal[i-1]
  min_draw_i=-(loanbal[i-1]+accrint)  #a negative number, i.e. a payment
  if(as.Date(cf.t[i]) %in% pdates) {
    intpayable=accrint
    } else {
    intpayable=0
  }
  if(cf.d[i] > 0) {
    draw[i]=-cf.d[i]
  } else {
    draw[i]=intpayable-cf.d[i]
  }
  draw[i]=min(draw[i],max_draw_i)
  draw[i]=max(draw[i],min_draw_i)
  print(paste(cf.t[i],"draw =",draw[i],"intpayable=",intpayable,"accrint",accrint))
  if (draw[i]<0) {
    draw_int=max(draw[i],-accrint)
    draw_prin=draw[i]-draw_int
    loanbal[i]=loanbal[i-1]+draw_prin
    accrint=accrint+draw_int
  } else {
    loanbal[i]=loanbal[i-1]+draw[i]
    accrint=accrint-intpayable
  }
  if(as.Date(cf.t[i]) %in% pdates) {
    loanbal[i]=loanbal[i]+accrint
    accrint=0
  }
}
residcf=cf.d-interest+draw
residcf=xts(residcf,cf.t)
interest=xts(interest,cf.t)
draw=xts(draw,cf.t)
loanbal=xts(loanbal,cf.t)
loanbal=loanbal[fsdates]

Construction_loan=list(loanbal,interest,draw,residcf)

#Valuation
#set value through stabilization as greater of cost or current ebitda/caprate
#after stabilization, drop the cost floor


