library(jsonlite)
library(tidyverse)
library(ggrepel)
library(stringr)
library(docstring)

years <- list("2015", "2016", "2017", "2018", "2019", "2020")

#### talent data

gettalentdata <- function(year) {
  #' Pulls talent data from api.collegefootballdata.com
  #' Output: data frame with 3 columns 
  #'   - year
  #'   - team
  #'   - talent rating
  
  talenturl <- paste0("https://api.collegefootballdata.com/talent?year=", year)
  talent <- jsonlite::fromJSON(talenturl)
  if (length(talent) > 0) {
      return(talent)
  }
}

lapply(years, gettalentdata) %>% bind_rows -> talent
write.csv(talent, "talent.csv")

## Get coaching data

getcoachingdata <- function(year) {
    #' Pulls coaching data from api.collegefootballdata.com
    #' Output: dataframe with 3 columns:
    #'   - school
    #'   - season
    #'   - coach name
    
    coach_url <- paste0("https://api.collegefootballdata.com/coaches?year=", year)   
    coach <- jsonlite::fromJSON(coach_url)
    if (length(coach) > 0) {
        return(coach)
    }
}

lapply(years, getcoachingdata) %>% bind_rows() -> coaches

coaches_data <- data.frame(school = character(),
                           year = numeric(),
                           coach_name = character(),
                           wins = numeric())

for (coach in 1:nrow(coaches)) {
  
    coach_name <- paste(coaches[coach,"first_name"], coaches[coach, "last_name"])
    school <- coaches$seasons[[coach]]$school
    year <- coaches$seasons[[coach]]$year
    wins <- coaches$seasons[[coach]]$wins
    
    coachdata <- data.frame(school, year, coach_name, wins)
    coaches_data <- rbind(coaches_data, coachdata)
}

write.csv(coaches_data, "coaches.csv")

#### Get conference data

conferenceurl <- "https://api.collegefootballdata.com/teams"
conference <- fromJSON(conferenceurl)
conference %>% select(school, conference) %>% na.omit() %>% distinct() -> conferences

#### Get team color data

colorurl <- "https://api.collegefootballdata.com/teams"
fromJSON(colorurl) %>% 
  select(school, color) -> colors

#### Join together to get win-talent data

colnames(coaches_data)
colnames(talent)

left_join(coaches_data, talent, by=c("school", "year")) %>%
    left_join(., colors, by = "school") %>% 
    left_join(., conferences, by = "school") -> wintalentdata

wintalentdata <- wintalentdata[wintalentdata$school != "Idaho", ]
write.csv(wintalentdata, "wintalentdata.csv")

