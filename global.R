library(shiny)
library(shinydashboard)
library(shinythemes)
library(ggplot2)
library(leaflet)
library(lubridate)
library(dplyr)
library(tidyr)
library(ggmap)
library(vegan)
library(stats)
library(scales)
library(shinyjs)
library(DT)

if (!file.exists("sort_brazil.csv")) {
  stop("Data file 'sort_brazil.csv' not found.")
}
brazil <- read.csv("sort_brazil.csv")

if (!file.exists("time_series_brazil.csv")) {
  stop("Data file 'time_series_brazil.csv' not found.")
}
brazil_2 <- read.csv("time_series_brazil.csv")
# Define UI for application

if (!file.exists("timeseries2.csv")) {
  stop("Data file 'timeseries2.csv' not found.")
}
data <- read.csv("timeseries2.csv")
data$MYear <- ym(data$MYear)

na_count <- sum(is.na(data$MYear))
infinite_count <- sum(!is.finite(data$MYear))
cat("NA values in MYear:", na_count, "\n")
cat("Infinite values in MYear:", infinite_count, "\n")

short_data <- gather(brazil, key = "FeedingHabit", value = "Count", 4:12)
short_data$SITES <- factor(short_data$SITES)
long_data <- tidyr::gather(brazil, key = "Species", value = "Count", 13:99)
long_data$SITES <- factor(long_data$SITES)


excludedSpecies <- c("piscivorous", "omnivorous", "nectarivore", "insectivorous", 
                     "herbivore", "granivore", "frugivorous", "detritivore", 
                     "carnivore", "Long", "Lat")

speciesNames <- names(brazil)[!(names(brazil) %in% c("piscivorous", "omnivorous", "nectarivore", 
                                                     "insectivorous", "herbivore", "granivore", 
                                                     "frugivorous", "detritivore", "carnivore", 
                                                     "Long", "Lat", "SITES", "richness"))]
