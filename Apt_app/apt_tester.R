#test apartment analyzer

library(tidyverse)
library(readxl)
library(purrr)
library(lubridate)
library(xts)
library(timetk)
library(asrsMethods)
library(leaflet)

source("apt_functions.r")

ltoapt_config=read_config("modera_decatur.xlsx")
ans=apt_analyzer(apt_config)
#str(ans)
is_list=ans[["islist"]]
is_mat=ltomat.xts(is_list,wtotal=TRUE,tname="Net_Income")
is_df=as.data.frame(is_mat)
is_df=data.frame(Date=as.Date(rownames(is_df)),is_df)
rownames(is_df)=NULL
is_df=data.frame(Year=year((is_df)$Date),is_df)
is_df$Date=NULL
is_df=aggregate(is_df,by=list(is_df$Year),FUN=sum)
is_df$Year=is_df$Group.1
is_df$Group.1=NULL

cflist=ans$cflist
sumcflist=list(mergesum.xts(cflist[1:7]),mergesum.xts(cflist[8:9]),cflist[10:13])
names(sumcflist)=c("Operations","Const_Loan")

apt_config=ans$apt_config
specsid=apt_config$Identification
lati=filter(specsid,name=="Latitude")$num
long=filter(specsid,name=="Longitude")$num
leaflet() %>%
  addTiles() %>%
  addMarkers(
    lat=lati,
    lng=long
  )
