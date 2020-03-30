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

getcoachingdata("2018") %>% filter(grepl("Arizona State", seasons))

lapply(years, getcoachingdata) %>% bind_rows() -> coaches

coaches_data <- data.frame(school = character(),
                           year = numeric(),
                           coach_name = character(),
                           wins = numeric())

for (row in 1:nrow(coaches)) {
  
    coach_name <- paste(coaches[row,"first_name"], coaches[row, "last_name"])
    school <- coaches$seasons[[row]]$school
    year <- coaches$seasons[[row]]$year
    wins <- coaches$seasons[[row]]$wins
    
    coachdata <- data.frame(school, year, coach_name, wins)
    coaches_data <- rbind(coaches_data, coachdata)
}

get_coach_name <- function(names) {
  
  #' helper function which concatenates coach names for schools with multiple coaches in a season

  if (length(unique(names)) == 1){
    return(names[1])
  } else {
    return(paste(names, collapse = ', '))
  }
}

coaches_data %>% 
  group_by(school, year) %>% 
  summarise(
    coaches = as.character(get_coach_name(coach_name)),
    wins = sum(wins)
  ) -> coaches_data

write.csv(coaches_data, "coaches_data.csv")

# There's some missing data in coaches_data.csv -- will need to fix manually

coaches_data %>% 
  group_by(school) %>%
  summarise(
    count = n()
  ) %>%
  arrange(count) -> missing_rows


coaches %>% filter(grepl("Indiana", seasons))
coaches_data %>% filter(school == "Oregon State")
data.frame(school = "Oregon State",
           year = 2018,
           coach_name = "Jonathan Smith",
           wins = 2) -> row_replace
coaches_data[coaches_data$coach_name == "Gary Andersen" & coaches_data$year == 2019,] <- row_replace

#### Get conference data

conferenceurl <- "https://api.collegefootballdata.com/teams"
conference <- fromJSON(conferenceurl)
conference %>% select(school, conference) %>% na.omit() %>% distinct() -> conferences

#### Get team color data

colorurl <- "https://api.collegefootballdata.com/teams"
fromJSON(colorurl) %>% 
  select(school, color) -> colors

#### Join together to get win-talent data

coaches_data <- read_csv("coaches_data.csv")
colnames(coaches_data)
colnames(talent)

talent[talent$school == "Clemson",]

left_join(coaches_data, talent, by=c("school", "year")) %>%
    left_join(., colors, by = "school") %>% 
    left_join(., conferences, by = "school") -> wintalentdata

wintalentdata <- wintalentdata[wintalentdata$school != "Idaho", ]
write.csv(wintalentdata, "wintalentdata.csv")

