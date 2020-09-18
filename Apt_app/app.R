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
library(plotly)
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
            tabPanel("Financial Analysis",
                     plotlyOutput("plot_irrs"),
                     plotlyOutput("plot_3yr"),
                     plotlyOutput("plot_cumcf")
            ),
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
    output$plot_irrs=renderPlotly({
        analist=ans()$analist
        x=evolution_irr(analist$Investor_CF,analist$Inv_FV)
        y=evolution_irr(analist$Unlev_CF,analist$Unl_FV)
        z=evolution_irr(analist$Lev_CF,analist$Lev_FV)
        irrlist=list(z,x,y)
        names(irrlist)=c("Project_IRR","Investor_IRR","Unlevered_IRR")
        irrdf=ltodf(irrlist)
        irrdf=gather(irrdf,key="type",value="IRR",-Date)
        plot=ggplot(irrdf,aes(x=Date,y=IRR,col=type))+
            geom_line()+
            ggtitle("Inception IRRs")
        ggplotly(plot)
    })
    output$plot_3yr=renderPlotly({
        analist=ans()$analist
        x=rolling_irr(analist$Investor_CF,analist$Inv_FV,36)
        y=rolling_irr(analist$Unlev_CF,analist$Unl_FV,36)
        z=rolling_irr(analist$Lev_CF,analist$Lev_FV,36)
        irrlist=list(z,x,y)
        names(irrlist)=c("Project_IRR","Investor_IRR","Unlevered_IRR")
        irrdf=ltodf(irrlist)
        irrdf=gather(irrdf,key="type",value="IRR",-Date)
        plot=ggplot(irrdf,aes(x=Date,y=IRR,col=type))+
            geom_line()+
            ggtitle("Rolling 3 Yr IRRs")
        ggplotly(plot)
    })
    output$plot_cumcf=renderPlotly({
        analist=ans()$analist
        lcumcf=list(-1*cumsum(analist$Investor_CF),analist$Inv_FV)
        names(lcumcf)=c("Invstr_Cum_CF","Invstr_NAV")
        cumcfdf=ltodf(lcumcf,wtotal=FALSE,donona=FALSE)
        cumcfdfg=gather(cumcfdf,key="type",value="Dollars",-Date)
        cumcfdfg=filter(cumcfdfg,!is.na(Dollars))
        plot=ggplot(cumcfdfg,aes(x=Date,y=Dollars,col=type))+
            geom_line()+
            ggtitle("Investor Cumulative CF and NAV")
        ggplotly(plot)
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
