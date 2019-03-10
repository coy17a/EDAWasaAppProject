library(rvest)
library(dplyr)
library(tidyr)
library(stringr)
library(lubridate)
library(ggplot2)


#scraping Results
years <- c("2018","2017","2016","2015","2014","2013")
years[3]
dfs <- vector("list",1)
for (i in 1:length(years)){
url1 <- c("2018","2017","2016")
url2 <- c("2015","2014","2013")
if (years[i] %in% url1){
  url <-paste0("https://www.startlinetiming.com/en/races/",years[i],"/wasa/event/Standard")
} else {
  url <- paste0("https://www.startlinetiming.com/en/races/",years[i],"/wasa")
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
df <- tbl_df(df)
dfs[[i]] <- df
}

dfs[[5]] <- tbl_df(dfs[[5]])%>%
  rename('Place/Division'= "Place/Divsion")
dfs[[6]] <- tbl_df(dfs[[6]])%>%
  rename('Place/Division'= "Place/Divsion")

cleandata <- function(x,y) {
 
     x %>%
    mutate(Swim=str_replace(Swim,"\n",""),Division=str_replace_all(Division,"\n|[:space:]",""), Name = str_replace_all(Name,"\n",""))%>%
    mutate_at(c(3,9,11,13),hms)%>%
    mutate_at(c(10,12),ms)%>%
    mutate_at(c(3,9:13),as.numeric)%>%
    fill(9:13)%>%
    rename(category ="Division",age_group = "Place/Division",genderP ="Place/Gender")%>%
    separate(age_group,into = c("AgePosition","ToalAge"), sep = "/")%>%
    separate(genderP,into = c("GenderPosition","ToaGender"), sep = "/")%>%
    separate(Place,into = c("Position","TotalP"), sep = "/")%>%
    select(-c("Event","Race #","ToalAge","ToaGender","TotalP"))%>%
    mutate_at(c(5:6,1),as.numeric)%>%
    filter(str_detect(category,"(^F|^M)(?=\\d)"))%>%
    mutate(gender = ifelse(str_detect(category,"F"),"F","M"))%>%
    mutate(gender = as.factor(gender),year = y,category = str_replace(category,"[A-Z]",""))%>%
    mutate(category = str_replace(category,"OLY",""))%>%
    mutate(category = str_replace(category,"70\\+","7074"))%>%
    filter(!is.na(Time))
}

w18 <- cleandata(dfs[[1]],2018)
w17 <- cleandata(dfs[[2]],2017)
w16 <- cleandata(dfs[[3]],2016)
w15 <- cleandata(dfs[[4]],2015)
w14 <- cleandata(dfs[[5]],2015)
w13 <- cleandata(dfs[[6]],2013)

wasaData <-bind_rows(w18,w17,w16,w15,w14,w13)
wasaData <- wasaData %>%
mutate(category = factor(category), year = factor(year))
str(wasaData)
levels(wasaData$category)

#str(test)>test1 <-test %>%
 #  filter(category =="3034",gender=="M")%>%

# +   mutate(Time = parse_date_time(seconds_to_period(Time),"HMS"))
 
#  ggplot(test1, aes(x=AgePosition, y = Time,color=year))+
#  +   geom_point()
# str(test)


