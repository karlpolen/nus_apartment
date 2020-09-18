#test apartment analyzer

library(tidyverse)
library(readxl)
library(purrr)
library(lubridate)
library(xts)
library(timetk)
library(asrsMethods)
library(leaflet)
library(plotly)
source("apt_functions.r")

ltoapt_config=read_config("modera_decatur.xlsx")
ans=apt_analyzer(apt_config)


#str(ans)
analist=ans$analist

islist=ans$islist