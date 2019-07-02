#rm(list = ls(all=TRUE))
#Weather forecast  Application

library(ggplot2)
library(RCurl)
library(httr)
library(rjson)
library(tibble)
library(tidyverse)
library(shiny)
library(shinythemes)
library(plotly)
library(maps)
library(leaflet)
library(leaflet.extras)
library(dplyr)

#add city names here
cities = c("Sevilla","Bilbao","Vigo","Gijon","Zaragoza","Amsterdam","London","Lisbon","Paris","Barcelona","Madrid","Valencia","Mumbai","Salamanca","Toledo","Getafe","Singapore","Hong Kong","New York","Dubai","Rome","Las Vegas","Milan","Warsaw","Toronto","Miami","Buenos Aires","Rio de Janeiro","Santiago","Quito","Nairobi","Cairo","Istanbul","Berlin","Munich","Moscow","Tokyo","Delhi","Shanghai","Melbourne","Sydney","Vienna","Budapest","Brussels","Nice","Marseille")
City = cities
Measure = c("temperature","speed","pressure","humidity")


#API KEY : openweathermap.org 
api_key = "bffd46a4f5694954742745de6346493e"

#functions for scrapping data 
get_weather_current=function(api_key,city="",country="")
{
  url="http://api.openweathermap.org/data/2.5/weather?"
  
  city=gsub(" ","+",city)
  country=gsub(" ","+",country)
  
  if(city != "")
  {
    url=paste(url,"q=",city,sep="")
    
    if(country!="")
    {
      url=paste(url,",",country,sep="")
    }
  }
  
  url=paste(url,"&APPID=",api_key,sep="")
  d=getURL(url)
  d=fromJSON(d)
  d
}

get_weather_forecast=function(api_key,city="",country="")
{
  url="http://api.openweathermap.org/data/2.5/forecast?"
  
  city=gsub(" ","+",city)
  country=gsub(" ","+",country)
  
  
  if(city != "")
  {
    url=paste(url,"q=",city,sep="")
    
    if(country!="")
    {
      url=paste(url,",",country,sep="")
    }
  }
  
  
  url=paste(url,"&APPID=",api_key,sep="")
  d=getURL(url)
  d=fromJSON(d)
  d
}



#extract live forecast for next 5 days 
forecast = data.frame()

for(j in 1:length(cities)){
  
  bin = data.frame()
  main = data.frame() 
  date_time = data.frame()
  weather = data.frame()
  wind = data.frame()
  
  
  data=get_weather_forecast(api_key,city=cities[j])
  
  
  for (i in 1:length(data$list)) {
    bin = as.data.frame(data$list[[i]]$main)
    main = rbind(main,bin)    
    
    bin = as.data.frame(data$list[[i]]$dt_txt)
    date_time = rbind(date_time,bin)
    
    bin = as.data.frame(data$list[[i]]$weather)
    weather = rbind(weather,bin)
    
    bin = as.data.frame(data$list[[i]]$wind)
    wind = rbind(wind,bin)
  }
  
  temp = data.frame()
  temp = cbind(date_time,main,weather,wind)
  temp$City = data$city$name
  forecast = rbind(forecast,temp)
  
  
}

colnames(forecast)[1] = c("date_time")
forecast$num_forecast = forecast$date_time
levels(forecast$num_forecast) <- 1:length(data$list)  
forecast$num_forecast = as.numeric(forecast$num_forecast)
forecast$num_forecast[forecast$num_forecast<=8] = 1
forecast$num_forecast[forecast$num_forecast>8 & forecast$num_forecast<=16] = 2
forecast$num_forecast[forecast$num_forecast>16 & forecast$num_forecast<=24] = 3
forecast$num_forecast[forecast$num_forecast>24 & forecast$num_forecast<=32] = 4
forecast$num_forecast[forecast$num_forecast>32 & forecast$num_forecast<=40] = 5
forecast$temp = round(forecast$temp - 273.15)
forecast$temp_min = round(forecast$temp_min - 273.15)
forecast$temp_max = round(forecast$temp_max - 273.15)
rm(bin,data,date_time,main,temp,weather,wind)
forecast = forecast[,-c(6,7)]


#GEO MAP + Plot : extracting current weather for multiple cities
current = data.frame()
bin = data.frame()
main = data.frame() 
date_time = data.frame()
weather = data.frame()
wind = data.frame()
visibility = data.frame()
clouds = data.frame()
cordinates = data.frame()
sunrise_set = data.frame()


for(j in 1:length(cities)){
  
  data=get_weather_current(api_key,city=cities[j])
  cordinates = as.data.frame(data$coord)
  main = as.data.frame(data$main)
  main = main[,c("temp","pressure","humidity","temp_min","temp_max")]
  date_time = as.data.frame(data$dt)
  weather = as.data.frame(data$weather)
  weather=weather[,c("main","description")]
  wind = as.data.frame(data$wind)
  wind = wind[,c("speed")]
  
  #visibility = as.data.frame(data$visibility)
  clouds = as.data.frame(data$clouds)
  sunrise_set = as.data.frame(data$sys)
  sunrise_set = sunrise_set[,c("sunrise","sunset")]
  
  temp = data.frame()
  #temp = cbind(date_time,cordinates,main,weather,wind,visibility,clouds,sunrise_set)
  temp = cbind(date_time,cordinates,main,weather,wind,clouds,sunrise_set)
  
  temp$City = data$name
  current = rbind(current,temp) 
  
}
colnames(current)[1] = c("date_time")
current$date_time = as.POSIXct(current$date_time, origin="1970-01-01")
current$sunrise = as.POSIXct(current$sunrise, origin="1970-01-01")
current$sunset = as.POSIXct(current$sunset, origin="1970-01-01")
#colnames(current)[12] = c("Visibility")
colnames(current)[12] = c("Precipitation")
current$temp = round(current$temp - 273.15)
current$temp_min = round(current$temp_min - 273.15)
current$temp_max = round(current$temp_max - 273.15)


rm(bin,data,date_time,main,temp,weather,wind,visibility,clouds,cordinates,sunrise_set)



# Define UI for application that draws a histogram
ui <- fluidPage(
  theme = shinytheme("slate"),
  # Application title
  navbarPage(title="Weather Report & Forecast <><><><> data:openweathermap.org",
             tabPanel(title="Hourly Forecast",icon=icon("fas fa-chart-line"),
                      titlePanel("Forecast"),
                      
                      # Sidebar with a input 
                      sidebarLayout(
                        sidebarPanel(
                          selectInput(inputId = "City1",
                                      label = "Choose your City",
                                      choices = sort(unique(City))),
                          selectInput(inputId = "measure",
                                      label = "Choose your Measure",
                                      choices = unique(Measure)),
                          sliderInput(inputId = "hour",
                                      label = "Days to forecast (Frequency:every 3hours)",
                                      min = 01,max = 5, value = c(1,3))
                          # value is always yyyy-mm-dd, even if the display format is different
                          #dateInput("date", "Date:", value = date, format = "yyyy-mm-dd")
                          
                        ),
                        
                        mainPanel(plotlyOutput(outputId ="Hour",height = 450,width = 'auto'))
                      )
                      
             ),
             tabPanel(title="Maps",icon=icon("far fa-globe"),
                      #titlePanel("View Map"),
                      
                      mainPanel(leafletOutput(outputId = "mymap",height = 800,width = 1400), 
                                absolutePanel(top = 60, left = 20
                                              #checkboxInput("markers", "Weather", FALSE)
                                )
                      )
             )
  )
)

