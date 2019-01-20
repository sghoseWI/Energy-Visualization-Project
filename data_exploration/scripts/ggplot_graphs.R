#Author: Saptarshi Ghose 
#Purpose: Exploratory data visualization on Open PV and Deepsolar project data

rm(list=ls())   

#Set working directory
setwd("/Users/saptarshighose/Documents/Saptarshi/University of Chicago/Data Visualization/Data")

#Install packages

install.packages('tidyverse')
install.packages('ggplot2')
install.packages('haven')
install.packages("rmarkdown")
install.packages('ggmap')
install.packages('ggalt')
install.packages('usmap')

#Load libraries
library(tidyverse)
library(ggplot2)
library(ggmap)
library(ggalt)
library(usmap)
library(rmarkdown)

#Import dta file
deepsolar_data <- read_csv(file = "deepsolar_tract.csv")
openpv_data <- read_csv(file = "/Users/saptarshighose/Downloads/openpv_data_011819.csv")

#Attach the data
attach(data())

#Build Graphs:

#Stacked Area Chart
ggplot(data=openpv_data)
openpv_data %>%
  filter(state %in% c("CA","TX","FL","NY","PA")) %>%
  group_by(state, year) %>%
  summarize(kwsize = sum(size_kw)) %>%
  ggplot(aes(x = year, y = kwsize, fill = state)) +
  geom_area(position = "stack") +
  scale_y_continuous(expand = c(0, 0), labels = scales::comma) +
  scale_x_continuous(expand = c(0, 0), limits = c(2000,2016), breaks = (seq(2000,2016,2)), labels=(seq(2000,2016,2))) +
  labs(title = "California Leads Among Most Populous States in Total Size of Photo Voltaic Installations",
       subtitle = "Total Size of Photo Voltaic Systems (KW) by State",
       caption = "Source: The Open Photo Voltaic Project (National Renewable Energy Lab)",
       x = "Year",
       y = "Total Size of Photo Voltaic Systems (KW)")

#Facet Wrap
deepsolar_data$state = toupper(deepsolar_data$state)  #Capitalize State Abbreviations

deepsolar_data %>%
  ggplot(aes(x = average_household_income)) +
  geom_histogram(color="darkblue", fill="springgreen3") +
  facet_wrap(~state) +
  scale_x_continuous(labels = scales::dollar) +
  scale_y_continuous(expand = c(0, 0)) +
  labs(title = "Average Income Distribution Varies Widely by State",
       subtitle = "Average Household Income by State (Census Tract Level)",
       caption="Source: The Deepsolar Project (Stanford University)",
       x = "Average Household Income",
       y = "Count (US Census Tract Level)") + 
  theme(axis.text.x = element_text(angle = 90),
        strip.text = element_text(face = "plain", 
                                  size = rel(1))) + theme_minimal()

#Scatter Plot
sampled_open_pv_data <- sample_frac(openpv_data, size = 0.08, replace=FALSE)  #Sample of full Open PV dataset

sampled_open_pv_data %>%
  filter(install_type %in% c("Agricultural","Commercial","Education","Government","Nonprofit","Residential")) %>%
  ggplot(aes(x=install_type, y=size_kw, color = install_type, size = cost_per_watt, stroke = 1),  alpha = 0.3)  + 
  geom_point(alpha = 0.3) +
  labs(title = "Commercial and Government Photo Voltaic Systems Vary Most Widely in Size",
       subtitle = "Size and Cost of Photo Voltaic Systems by Type",
       caption = "Source: The Open Photo Voltaic Project (National Renewable Energy Lab)",
       size = "Cost per Watt ($)",
       color = "Type of Photo Voltaic System",
       x = "Type of Photo Voltaic System",
       y = "Total Size of Photo Voltaic System (Kilowatts)") +
  ylim(0, 1500) +
  theme(axis.text.x = element_text(angle = 90),
        strip.text = element_text(face = "plain", 
                                  size = rel(1))) + theme_light()

#Density Plot (Not submitted)
ggplot(deepsolar_data, aes(solar_system_count, fill = state, colour = state)) +
  geom_density(position = "stack") + 
  xlim(0, 30) 
