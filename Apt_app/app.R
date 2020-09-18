#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
library(tidyverse)
library(readxl)
library(purrr)
library(lubridate)
library(xts)
library(timetk)
library(asrsMethods)
library(knitr)
library(DT)
library(leaflet)
source("apt_functions.r")


# Define UI for application that draws a histogram
ui <- dashboardPage(
    
    dashboardHeader(title = textOutput("projname")),
    dashboardSidebar(
        selectInput(inputId="file",
                    label="Select file",
                    choices=c("modera_decatur.xlsx"),
                    selected="modera_decatur.xlsx")
    ),
    dashboardBody(
        tabBox(
            width=12,
        tabPanel("Map",leafletOutput("map")),    
        tabPanel("Income Statement",DT::DTOutput("is")),
        tabPanel("Balance Sheet",DT::DTOutput("bs")),
        tabPanel("Cash Flow",DT::DTOutput("cf"))
        )
    )
)


# Define server logic required to draw a histogram
server <- function(input, output,session) {
    #  apt_config <- reactiveFileReader(
    #  intervalMillis = 1000,
    #      session = session,
    #      filePath = input$file,
    #      readFunc = read_config
    # )
    ans=reactive({
        apt_config=read_config(input$file)
        apt_analyzer(apt_config)
    })
    # ans=reactive({
    #     apt_analyzer(apt_config)
    # })
    output$projname=renderText({
        apt_config=ans()$apt_config
        specsid=apt_config[["Identification"]]
        filter(specsid,name=="ProjectName")$descr
    })
    output$is=DT::renderDT({
        is_list=ans()$islist
        ltodfyear(is_list,tname="Net_Income",roundthou=TRUE)
    })
    output$bs=DT::renderDT({
        bs_list=ans()$bslist
        ltodfyear(bs_list,total=FALSE,roundthou=TRUE,isbs=TRUE)
    })
    output$cf=DT::renderDT({
        cf_list=ans()$sumcflist
        ltodfyear(cf_list,total=TRUE,tname="Net_CF",roundthou=TRUE)
    })
    output$map=renderLeaflet({
        apt_config=ans()$apt_config
        specsid=apt_config$Identification
        lati=filter(specsid,name=="Latitude")$num
        long=filter(specsid,name=="Longitude")$num
        leaflet() %>%
            addTiles() %>%
            addMarkers(
                lat=lati,
                lng=long
            )
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
