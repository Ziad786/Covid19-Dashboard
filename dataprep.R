getOption('timeout')
# [1] 60

options(timeout=300)

# Install Packages
install.packages(c("lubridate", "plotly", "RColorBrewer", "tidyverse","DT", "glue", "forcats", "ggpubr", "leaflet", "leafpop"))



# Load packages 
library(tidyverse)
library(data.table)

# Color and maps 
library(plotly)
library(leaflet)
library(leafpop)
library(RColorBrewer)

# Dates
library(lubridate)

# Data

url <- "https://raw.githubusercontent.com/RamiKrispin/coronavirus/master/csv/coronavirus.csv"

df <- fread(url)

# data structure
glimpse(df)

df <- df |>
  mutate(country = trimws(country),
         country = as.factor(country))

# data prep for daily cases

df_daily <- df |> 
  group_by(date, type) |> 
  summarise(total = sum(cases, na.rm = T), .groups = "drop") |> 
  pivot_wider(names_from = type,
              values_from = total)  |> 
  arrange(date) |> 
  ungroup() |> 
  mutate(active = confirmed - death - recovery) |> 
  mutate(confirmed_cum = cumsum(confirmed),
         death_cum = cumsum(death),
         recovery_cum = cumsum(recovery),
         active_cum = cumsum(active))

# covid19 cases by country
df_country <- df |> 
  group_by(country, type) |> 
  summarise(total = sum(cases, na.rm = T), .groups = "drop") |> 
  pivot_wider(names_from = type,
              values_from = total) |> 
  mutate(active = confirmed - death - recovery) |> 
  pivot_longer(cols = -country,
               names_to = "type",
               values_to = "total")

# for global statistics
df_world <- df |> 
  group_by(type) |>
  summarise(total = sum(cases, na.rm = T), .groups = "drop") |>
  pivot_wider(names_from = type,
                values_from = total) |>
  mutate(active = confirmed - death - recovery)







