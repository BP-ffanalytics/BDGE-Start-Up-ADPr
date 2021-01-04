

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
PlayerMetadata <- read_excel("./Data/Player Meta Data.xlsx") %>% as.data.frame()

## Import data
ADP_metadata_DF <- read.csv('./Data/septBDGEmockmeta.csv', header=TRUE)
ADP_metadata_DF$Date <- mdy(ADP_metadata_DF$Date)

PlayerPos_DF <- read.csv('./Data/septBDGEmockpositions.csv', header=TRUE)

# Remove first column
PlayerPos_DF <- PlayerPos_DF[,!(colnames(PlayerPos_DF) %in% "X")]

# Remove X from Mock ID colnames
colnames(PlayerPos_DF)[!(colnames(PlayerPos_DF) %in% c("playerID","team","position","first_name","last_name","player"))] <- 
	gsub("X", "", colnames(PlayerPos_DF)[!(colnames(PlayerPos_DF) %in% c("playerID","team","position","first_name","last_name","player"))])


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
