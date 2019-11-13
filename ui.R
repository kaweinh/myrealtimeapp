library(shiny)
library(DT)

shinyUI(fluidPage(
  h1("Fall 2020 Enrollment Monitor"),
  
  h4("The plots below show the comparision of the number of students who have applied, been admitted, 
  confirmed and registered at UMBC. The red lines show students for Fall 2020 while the blue lines
  shows students for the past years of Fall 2018 and Fall 2019. The sample of students include those
  who came in as new undergraduate freshman. The x-axis indicates the number of days prior to the 
  beginning of the term. The y-axis indicates the number of students who have applied, admitted, 
  confirmed and registered. The first 4 plots below show the most recent 30 days while the 2 plots 
  below show the year in entirety"),
  br(),
  
  fluidRow(
    column(6, plotOutput( outputId = "plot_applied")),
    
    column(6, plotOutput( outputId = "plot_admitted")),
    
    column(6, plotOutput( outputId = "plot_confirmed")),
    
    column(6, plotOutput( outputId = "plot_registered"))
  ),
  
  br(),
  br(),
  
  plotOutput(outputId = "plot_app_adm"),
  plotOutput(outputId = "plot_conf_reg"),
  
  br(),
  h4("The tables below show data for the most recent 30 days starting with most recent and working backwards.
  The first table shows the difference between this year and the average of past years for the amount of 
  students that have applied and the amount of students that have been admitted. The second table shows the 
  same thing but for students that have been confirmed and students that have registered. The green in the 
  difference columns indicates this year 2020 is ahead of past years average for that day while red indicates 
  it is behind past years average for that day."),
  br(),
  
  DT::dataTableOutput(outputId = "app_adm_table"),
  br(),
  DT::dataTableOutput(outputId = "conf_reg_table")
))