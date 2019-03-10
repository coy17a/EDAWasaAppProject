library(rvest)
library(dplyr)
library(tidyr)
library(stringr)
library(lubridate)
library(ggplot2)


#scraping Results
get_data <- function(x = "olympic") 
{
  years <- c("2018","2017","2016","2015","2014","2013")
  years[3]
  dfs <- vector("list",1) # creat list to append results
  #loop for create a list with the dataframe for all years in vector years
  for (i in 1:length(years)){
    url1 <- c("2018","2017","2016")
    url2 <- c("2015","2014","2013")
    if( x =="olympic"){
      if (years[i] %in% url1){
        url <-paste0("https://www.startlinetiming.com/en/races/",years[i],"/wasa/event/Standard")
      } else {
        url <- paste0("https://www.startlinetiming.com/en/races/",years[i],"/wasa")
      }
    } else {
      url <-paste0("https://www.startlinetiming.com/en/races/",years[i],"/wasa/event/Sprint")
    }
    
    webpage <- read_html(url)
    table_data_html <- html_nodes(webpage,".table-condensed td")
    results <- html_text(table_data_html)
    headers<- html_nodes(webpage,".table-condensed th")
    header_names <- html_text(headers)
    head(header_names,13)
    ## Creating Data Frame
    results2 <- results
    length(results2) <- 13 * ceiling(length(results) / 13)
    mat <- matrix(results2,, 13, byrow = TRUE)
    df <- as.data.frame(mat, stringsAsFactors = FALSE)
    colnames(df) <- header_names
    df <- tbl_df(df) #transform to tibble data frame for use in tidyverse
    dfs[[i]] <- df
  }
  dfs
}

olympic <- get_data("olympic")
sprint <- get_data("sprint")

# correcting typos from the web page
fix_name <- function( x)
   x <- tbl_df(x) %>%
   rename('Place/Division'= "Place/Divsion")

sprint[[6]] <- fix_name(sprint[[6]])
sprint[[5]] <- fix_name(sprint[[5]])
olympic[[6]] <- fix_name(olympic[[6]])
olympic[[5]] <- fix_name(olympic[[5]])
 
cleandata <- function(x,y) {
     x %>%
    mutate(Swim=str_replace(Swim,"\n",""),Division=str_replace_all(Division,"\n|[:space:]",""), Name = str_replace_all(Name,"\n",""),Event = str_replace_all(Event,"\n|[:space:]",""))%>%
    mutate_at(c(3,9,11,13),hms)%>%
    mutate_at(c(10,12),ms)%>%
    mutate_at(c(3,9:13),as.numeric)%>%
    fill(9:13)%>%
    rename(category ="Division",age_group = "Place/Division",genderP ="Place/Gender")%>%
    separate(age_group,into = c("AgePosition","ToalAge"), sep = "/")%>%
    separate(genderP,into = c("GenderPosition","ToaGender"), sep = "/")%>%
    separate(Place,into = c("Position","TotalP"), sep = "/")%>%
    select(-c("Race #","ToalAge","ToaGender","TotalP"))%>%
    mutate_at(c(6:7,2),as.numeric)%>%
    filter(str_detect(category,"(^F|^M)(?=\\d)"))%>%
    mutate(gender = ifelse(str_detect(category,"F"),"F","M"))%>%
    mutate(gender = as.factor(gender),year = y,category = str_replace(category,"[A-Z]",""))%>%
    mutate(category = str_replace(category,"OLY|SPR",""))%>%
    mutate(category = str_replace(category,"70\\+","7074"))%>%
    mutate(Event = ifelse(str_detect(Event,"Sprint"),"Sprint","Olympic"))%>%
    filter(!is.na(Time))
}

ws18 <- cleandata(sprint[[1]],2018)
ws17 <- cleandata(sprint[[2]],2017)
ws16 <- cleandata(sprint[[3]],2016)
ws15 <- cleandata(sprint[[4]],2015)
ws14 <- cleandata(sprint[[5]],2014)
ws13 <- cleandata(sprint[[6]],2013)
#olympic distance
wo18 <- cleandata(olympic[[1]],2018)
wo17 <- cleandata(olympic[[2]],2017)
wo16 <- cleandata(olympic[[3]],2016)
wo15 <- cleandata(olympic[[4]],2015)
wo14 <- cleandata(olympic[[5]],2014)
wo13 <- cleandata(olympic[[6]],2013)


wasaData <-bind_rows(wo18,wo17,wo16,wo15,wo14,wo13,ws18,ws17,ws16,ws15,ws14,ws13)
wasaData <- wasaData %>%
mutate(category = factor(category), year = factor(year),Event=factor(Event))
str(wasaData)
levels(wasaData$Event)
#Save The data Frame for use in the app
write.csv(wasaData, file="wasaData.csv", row.names = FALSE) #avoid to have an extra column with numbers


test1 <- wasaData %>%
  filter(category =="3034",gender=="M",AgePosition== 3,Event=="Olympic")
mean(test1$Time )
