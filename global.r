
install_packages <- c("shiny","readxl","DT", "tidyverse", "nflscrapR","lubridate","RCurl",
            "plotly","jsonlite", "dplyr")
if (length(setdiff(install_packages, rownames(installed.packages()))) > 0) {
            install.packages(setdiff(install_packages, rownames(installed.packages())))
}



## Import libraries
library(readxl)
library(tidyverse)
library(nflscrapR)
library(lubridate)
library(RCurl)

age <- function(dob, age.day = today(), units = "years", floor = TRUE) {
	
    calc.age = interval(dob, age.day) / duration(num = 1, units = units)
    if (floor) return(as.integer(floor(calc.age)))
    return(calc.age)
}


## Import data
ADPdata <- read_excel("./Data/BDGE Mocks Data.xlsx", sheet="Sheet2") %>% as.data.frame()
PlayerMetadata <- read_excel("./Data/Player Meta Data.xlsx") %>% as.data.frame()

## Extract Draft Metadata
ADP_metadata_DF <- ADPdata[(grep("BDGE", ADPdata[,1])[1]):nrow(ADPdata),colnames(ADPdata) %in% c("Mock", "Date", "Rookie", "LeagueType", "Event")]

## Convert to data frame
ADPdata_DF <- ADPdata

## Overw
ADPdata_DF[grep("BDGE", ADPdata_DF$Mock)[1]:nrow(ADPdata_DF),"OverallPick"] <- 
	ADPdata_DF[grep("BDGE", ADPdata_DF$Mock)[1]:nrow(ADPdata_DF),"Mock"]

## Extract data 
ADPdata_DF <- ADPdata_DF[,grep("OverallPick", colnames(ADPdata)):ncol(ADPdata)]

## Add row names with first column
rownames(ADPdata_DF) <- ADPdata_DF[,1]

## Remove OverallPick column and transpose data
ADPdata_DF <- as.data.frame(t(ADPdata_DF[,!(grepl("OverallPick", colnames(ADPdata_DF)))]))

data(nflteams)
nflteams <- nflteams[!(nflteams$abbr %in% c("STL", "SD", "JAX")),]
nflteams$team <- ifelse(grepl("Raiders", nflteams$team), "Las Vegas Raiders", nflteams$team)
nflteams$abbr <- ifelse(nflteams$abbr == "OAK", "LV", nflteams$abbr)

PlayerMetadata$CurrentAge <- round(age(PlayerMetadata$Date, floor=FALSE),2)

urllogo <- getURL("https://raw.githubusercontent.com/statsbylopez/BlogPosts/master/nfl_teamlogos.csv")
logos <- read.csv(text = urllogo)
logos$team <- as.character(logos$team)
logos$team_code <- as.character(logos$team_code)
logos$url <- as.character(logos$url)
logos$team <- ifelse(grepl("Raiders", logos$team), "Las Vegas Raiders", logos$team)
logos$team_code <- ifelse(logos$team_code == "OAK", "LV", logos$team_code)
logos$team_code <- ifelse(logos$team_code == "JAX", "JAC", logos$team_code)
logos$team_code <- ifelse(logos$team_code == "LA", "LAR", logos$team_code)