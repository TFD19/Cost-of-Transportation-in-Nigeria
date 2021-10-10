# Set working directory
setwd("C:/Users/HATYTUDE CONSULTING/Downloads/Practice Datasets")

# View the Items within the directory
dir()

# Load in the necessary libraries
library(dplyr)
library(tidyr)
library(tidyverse)
library(plyr)
library(data.table)
library(purrr)
library(stringr)
library(zoo)
library(xts)
library(lubridate)
library(rebus)
library(broom)
library(ggplot2)
library(gganimate)
library(plotly)
library(gifski)
library(readr)
library(readxl)

# Inspecting the number of sheets in the data file
excel_sheets("TRANSPORT_COST_AUGUST_2021.xlsx")

# Import all the sheets in the data file
  # Create a function to import all the sheets in the excel file
  extr_data <- function(x, tibble = FALSE) {
    sheets <- readxl::excel_sheets(x)
    if(!tibble) x <- lapply(sheets, function(X) readxl::read_excel(x, sheet = X, skip = 1))
    names(x) <- sheets
    x
    print(x)
  }

  # Apply the function on the imported file
  trans_data <- extr_data("TRANSPORT_COST_AUGUST_2021.xlsx")

# Transform the list created into a dataframe
trans_data <- do.call(rbind.data.frame, trans_data)

# Create a column for States
  # Extract the row names representing the states
  new_col <- row.names(trans_data)
  
  # Remove the numbers attached to the states
  new_col <- str_remove(new_col, pattern = c(".1", ".2", ".3", ".4", ".5"))
  
  # Tranform the character list to a datframe
  new_col <- as.data.frame(new_col)

# Bind the newly created column to the previous dataframe
transportation_cost <- cbind(new_col, trans_data)

# Tidy the data by gathering the columns that are spread out, for easy ananlysis
transportation_cost <- gather(transportation_cost, key = "Year", value = "Cost in Naira", 3:70)

# Rename the columns
transportation_cost$`Month on Month (%)` <- transportation_cost$...71
transportation_cost$`Year on Year (%)` <- transportation_cost$...70
transportation_cost$`Transportation` <- transportation_cost$ItemLabels
transportation_cost$State <- transportation_cost$new_col
transportation_cost$new_col <- NULL
transportation_cost$ItemLabels <- NULL
transportation_cost$...70 <- NULL
transportation_cost$...71 <- NULL

# Transform the Year column to a date class
class(transportation_cost$Year)
  # Convert the year variable from character to numeric
  transportation_cost$Year <- as.numeric(transportation_cost$Year)
  # Find the minimum value in the year column
  min(transportation_cost$Year)
  # Subtract the minimum value from the column
  transportation_cost$Year <- transportation_cost$Year - 42370
  # Convert the numeric days to dates by setting to class date with the first date in our data as the origin
  transportation_cost$Year <- as.Date(transportation_cost$Year, origin = "2016-01-31")
  # Transform the year to month and year
  transportation_cost$Year <- zoo::as.yearmon(transportation_cost$Year) 
  transportation_cost$Quarter <- zoo::as.yearqtr(transportation_cost$Year)

# Reduce the decimal points of some of the variables
transportation_cost$`Year on Year (%)` <- round(transportation_cost$`Year on Year (%)`, digits = 2)
transportation_cost$`Month on Month (%)` <- round(transportation_cost$`Month on Month (%)`, digits = 2)
transportation_cost$`Cost in Naira` <- round(transportation_cost$`Cost in Naira`, digits = 0)

# Change the names of some of the transformed columns
transportation_cost$Month <- transportation_cost$Year
transportation_cost$Year <- year(transportation_cost$Month)
transportation_cost <- transportation_cost %>% select(State, Transportation, Month, Quarter, Year, `Cost in Naira`, `Month on Month (%)`, `Year on Year (%)`)

# Change the names of some of all the transportation rows
transportation_cost$Transportation <- str_replace(transportation_cost$Transportation, "Bus journey within  city , per  drop constant  rou", "Intra-city bus transport")
transportation_cost$Transportation <- str_replace(transportation_cost$Transportation, "Bus journey intercity, state route, charg. per per", "Inter-city Bus Transport")
transportation_cost$Transportation <- str_replace(transportation_cost$Transportation, "Air fare charg.for specified routes single journey", "One Way Air Transport")
transportation_cost$Transportation <- str_replace(transportation_cost$Transportation, "Journey by motorcycle (okada) per drop", "Motorcycle Per Drop")
transportation_cost$Transportation <- str_replace(transportation_cost$Transportation, "Water transport : water way passenger  transportat", "Water Transport")

# Add a new column that contains the region each state belongs to
transportation_cost <- transportation_cost %>% mutate(Region = ifelse(State %in% c("ABUJA", "BENUE", "NASSARAWA", "NIGER", "KOGI", "KWARA"), "North Central", 
                                               ifelse(State %in% c("BAUCHI", "BORNO", "ADAMAWA", "TARABA", "YOBE", "GOMBE"), "North East", 
                                                      ifelse(State %in% c("ZAMFARA", "SOKOTO", "KADUNA", "KEBBI", "KATSINA", "KANO", "JIGAWA"), "North West", 
                                                             ifelse(State %in% c("LAGOS", "OYO", "OSUN", "ONDO", "EKITI", "OGUN"), "South West", 
                                                                    ifelse(State %in% c("IMO", "EBONYI", "ANAMBRA", "ENUGU", "ABIA"), "South East", "South South"))))))
  
# Investigate the characteristics of the data with visual aids
# What is the trend of inter city travels in each region
  # For 2021
  transportation_cost %>% filter(Year == 2021 & Transportation == "Inter-city Bus Transport") %>%  
    group_by(Region, Month) %>% summarise_at(vars(`Cost in Naira`), sum) %>% 
    ggplot(aes(x = Month, y = `Cost in Naira`, color = Region)) + 
    geom_line() + labs(title = "Trend of Inter-City Bus Transportation Costs in 2021")
  
  # For 2020
  transportation_cost %>% filter(Year == 2020 & Transportation == "Inter-city Bus Transport") %>%  
    group_by(Region, Month) %>% summarise_at(vars(`Cost in Naira`), sum) %>% 
    ggplot(aes(x = Month, y = `Cost in Naira`, color = Region)) + 
    geom_line() + labs(title = "Trend of Inter-City Bus Transportation Costs in 2020")
    
  # For 2019
    transportation_cost %>% filter(Year == 2019 & Transportation == "Inter-city Bus Transport") %>%  
      group_by(Region, Month) %>% summarise_at(vars(`Cost in Naira`), sum) %>% 
      ggplot(aes(x = Month, y = `Cost in Naira`, color = Region)) + 
      geom_line() + labs(title = "Trend of Inter-City Bus Transportation Costs in 2019")

  # What is the trend of intra city bus transportation costs in each region
    # For 2021  
    transportation_cost %>% filter(Year == 2020 & Transportation == "Intra-city bus transport") %>%  
      group_by(Region, Month) %>% summarise_at(vars(`Cost in Naira`), sum) %>% 
      ggplot(aes(x = Month, y = `Cost in Naira`, color = Region)) + 
      geom_line() + labs(title = "Trend of Intra-City Bus Transportation Costs in 2020")
    
    # For 2020
    transportation_cost %>% filter(Year == 2021 & Transportation == "Intra-city bus transport") %>%  
      group_by(Region, Month) %>% summarise_at(vars(`Cost in Naira`), sum) %>% 
      ggplot(aes(x = Month, y = `Cost in Naira`, color = Region)) + 
      geom_line() + labs(title = "Trend of Intra-City Bus Transportation Costs in 2021")

    # For 2019
    transportation_cost %>% filter(Year == 2019 & Transportation == "Intra-city bus transport") %>%  
      group_by(Region, Month) %>% summarise_at(vars(`Cost in Naira`), sum) %>% 
      ggplot(aes(x = Month, y = `Cost in Naira`, color = Region)) + 
      geom_line() + labs(title = "Trend of Intra-City Bus Transportation Costs in 2019")
    
    # Combination of the three years
    transportation_cost %>% filter(Year %in% c(2019, 2020, 2021) & Transportation == "Inter-city Bus Transport") %>%  
      group_by(Region, Month, Year) %>% summarise_at(vars(`Cost in Naira`), sum) %>% 
      ggplot(aes(x = Month, y = `Cost in Naira`, color = Region)) + 
      geom_line() + facet_wrap(vars(Year), scales = "free") + 
      labs(title = "Trend of Inter-City Bus Transportation Costs for 2019, 2020 and 2021")

# Inspect the year on year growth of transport fares between 2019 and 2020
  # For inter-city bus transportation 
    transportation_cost %>% filter(Year %in% c(2019, 2020, 2021) & Transportation == "Inter-city Bus Transport") %>%  group_by(Region, Quarter) %>% summarise_at(vars(`Cost in Naira`), sum) %>% 
      ggplot(aes(x = Quarter, y = `Cost in Naira`, fill = Region)) + geom_col(position = "dodge") + labs(title = "Growth of Inter-City Bus Transpot Costs within all Regions across 2019 through 2021")
    
  # For air travels
    transportation_cost %>% filter(Year %in% c(2019, 2020, 2021) & Transportation == "One Way Air Transport") %>%  group_by(Region, Quarter) %>% summarise_at(vars(`Cost in Naira`), sum) %>% 
      ggplot(aes(x = Quarter, y = `Cost in Naira`, fill = Region)) + geom_col(position = "dodge") + 
      labs(title = "Growth of One Way Air Transport Cost within all Regions across 2019 through 2021")
    
  # For intra-city transportation
    transportation_cost %>% filter(Year %in% c(2019, 2020, 2021) & Transportation == "Intra-city bus transport") %>%  group_by(Region, Quarter) %>% summarise_at(vars(`Cost in Naira`), sum) %>% 
      ggplot(aes(x = Quarter, y = `Cost in Naira`, fill = Region)) + geom_col(position = "dodge") + 
      labs(title = "Growth of Intra-City Bus Transport Cost within all Regions across 2019 through 2021")
  
    # For Water transportation
    transportation_cost %>% filter(Year %in% c(2019, 2020, 2021) & Transportation == "Water Transport") %>%  group_by(Region, Quarter) %>% summarise_at(vars(`Cost in Naira`), sum) %>% 
      ggplot(aes(x = Quarter, y = `Cost in Naira`, fill = Region)) + geom_col(position = "dodge") + 
      labs(title = "Growth Water Transport Costs within all Regions across 2019 through 2021")
  
  # For Motorcylce Transportation per drop
    transportation_cost %>% filter(Year %in% c(2019, 2020, 2021) & Transportation == "Journey by motorcycle (okada) per drop") %>%  
      group_by(Region, Quarter) %>% summarise_at(vars(`Cost in Naira`), sum) %>% 
      ggplot(aes(x = Quarter, y = `Cost in Naira`, fill = Region)) + 
      geom_col(position = "dodge") + labs(title = "Growth of Motorcycle Transport Costs within all Regions across 2019 through 2021")

# Investigating states with the highest cost of transportation at all levels within each region
transportation_cost %>% filter(Year == 2021) %>% ggplot(aes(x = Region, y = `Cost in Naira`, fill = State)) + 
  geom_col(position = "dodge") + facet_wrap(vars(Transportation), scales = "free") + 
  theme(axis.text.x = element_text(angle = 30, vjust = 1.0, hjust = 1.0)) + 
  labs(title = "Average Cost of each Mode of Transportation in 2021 across all Regions")

# Investigating the rate of year on year growth of of each mode of transportation in all states during Covid-19
transportation_cost %>% filter(Year == 2020) %>% 
  ggplot(aes(x = Region, y = `Year on Year (%)`, fill = State)) + 
  geom_col(position = "dodge") + 
  facet_wrap(vars(Transportation), scales = "free") + 
  theme(axis.text.x = element_text(angle = 30, vjust = 1.0, hjust = 1.0)) + 
  labs(title = "Effects of Covid-19 on the year on year growth of cost of transportation in 2020")

# Investigating the cost of air transport relative to the availability of an airport in each state using the year 2019
transportation_cost %>% filter(Transportation == "One Way Air Transport" & Year == 2019) %>% 
  ggplot(aes(x = State, y = `Cost in Naira`)) + geom_col(fill = "orange") + 
  theme(axis.text.x = element_text(angle = 25, vjust = 0.7, hjust = 0.8)) + 
  scale_y_continuous(n.breaks = 10) + labs(title = "Cost of Air Transportation in each State in 2019")

# Estimating the effect of covid on the growth of cost on all the modes of transports
q <- transportation_cost %>% filter(Year %in% c(2019, 2020, 2021)) %>% 
  group_by(Year, Region) %>% ggplot(aes(x = Transportation, y = `Year on Year (%)`, fill = Region)) + 
  geom_col(position = "dodge") + theme(axis.text.x = element_text(angle = 30, vjust = 0.7, hjust = 0.8)) + 
  facet_wrap(vars(Year), scales = "free")

# Testing for Plotly
ggplotly(q)
args(fct_reorder)
