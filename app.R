rm(list=ls())
library(tseries)
library(lmtest)
library(sortable)
library(shinydashboard)
library(shiny)
library(tidyverse)
library(olsrr)
library(mfx)
library(stargazer)

path <- '/Users/smile/Downloads/App'
setwd(path)

source('functions.R')
source('ui.R')
source('server.R')
                
options(shiny.maxRequestSize=30*1024^2, shiny.reactlog = TRUE, shin.error = browser) 
# Run the application 
shinyApp(ui = ui, server = server)


