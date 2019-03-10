library(readr)
library(dplyr)
library(ggplot2)
library(lubridate)
library(stringr)
library(tidyr)
library(readxl)
library(GGally)
library(ggdark)
library(DT)


wasaData18 <- read_excel("wasaData.xlsx", sheet = "2018")
wasaData17 <- read_excel("wasaData.xlsx", sheet = "2017")
wasaData16 <- read_excel("wasaData.xlsx", sheet = "2016")
wasaData15 <- read_excel("wasaData.xlsx", sheet = "2015")
wasaData13 <- read_excel("wasaData.xlsx", sheet = "2013")

wasaData18 <- rename(wasaData18, Division ="20")
str(wasaData18)
cleandata <- function(x,y) {
  x %>%
    rename(category ="Division",age_group = "Place/Division",genderP ="Place/Gender")%>%
    separate(age_group,into = c("AgePosition","ToalAge"), sep = "/")%>%
    separate(genderP,into = c("GenderPosition","ToaGender"), sep = "/")%>%
    separate(Place,into = c("Position","TotalP"), sep = "/")%>%
    fill(Swim, T1,Bike,T2,Run)%>%
    select(-c("Event","Race #","ToalAge","ToaGender","TotalP"))%>%
    mutate_at(c(5:6,1),as.numeric)%>%
    filter(str_detect(category,"(^F|^M)(?=\\d)"))%>%
    mutate(category=as.factor(category),gender = ifelse(str_detect(category,"F"),"F","M"))%>%
    mutate(gender = as.factor(gender),year = as.factor(y),category = str_replace(category,"[A-Z]",""))%>%
    mutate(category = str_replace(category,"OLY",""))%>%
    mutate_at(c("T1","T2"), as.character)%>%
    separate(T1,into = c("date","T1"),sep = " ")%>%
    separate(T2,into = c("date2","T2"),sep = " ")%>%
    mutate (T2 =str_replace(T2,":00",""), T1=str_replace(T1,":00",""))%>%
    select(-c("date","date2"))%>%
    filter(!is.na(Time))
}


wasaData18c <- cleandata(wasaData18,2018)
wasaData17c <- cleandata(wasaData17,2017)
wasaData16c <- cleandata(wasaData16,2016)
wasaData15c <- cleandata(wasaData15,2015)
wasaData13c <- cleandata(wasaData13,2013)

wasaData1 <- bind_rows(wasaData18c,wasaData17c,wasaData16c,wasaData15c,wasaData13c,wasa)
wasaData1$year <- as.factor(wasaData$year) 

numberAthletes <- wasaData %>%
  group_by(category,year,gender)%>%
  summarize(Athletes = n())

ggplot(numberAthletes, aes (x=category,y=Athletes,fill=year))+
  geom_col()

totalnumberAthletes <- wasaData %>%
  group_by(year,gender)%>%
  summarize(Athletes = n())
ggplot(totalnumberAthletes, aes (x=year,y=Athletes,fill=gender))+
  geom_col()+
  scale_fill_brewer(palette="Set1")+
  dark_theme_gray()

  ggplot(wasaData, aes(x=Time,fill=year))+
  geom_histogram(alpha=0.5)

wasafilter <- wasaData18c %>%
  filter(gender == "M")%>%
  group_by(category)%>%
  summarise(avg_swim = mean(Swim),avg_bike = mean(Bike),avg_run = mean(Run))%>%
  gather(key="sport",value = "averageTime", 2:4)

wasafilter$averageTime<- duration(hour = hour(wasafilter$averageTime), minute = minute(wasafilter$averageTime), 
                          second = second(wasafilter$averageTime))
wasafilter$averageTime <- as.numeric(wasafilter$averageTime)
yLabels <- function(x)
{
  x <- seconds_to_period(x)
  paste(hour(x),minute(x),second(x), sep = ':')
}


wasafilter <- wasaData18c %>%
  filter(gender == "F")%>%
  group_by(category)%>%
  summarise(avg_swim = mean(Swim),avg_bike = mean(Bike),avg_run = mean(Run))%>%
  gather(key="sport",value = "averageTime", 2:4)

  wasafilter <- wasafilter %>%
  mutate(averageTime = duration(hour = hour(wasafilter$averageTime), minute = minute(wasafilter$averageTime),second = second(wasafilter$averageTime)))%>%
  mutate(averageTime = as.numeric(averageTime))


ggplot(wasafilter, aes(x=category, y=averageTime,fill=sport))+
  geom_bar(stat="identity")+
  scale_y_continuous(labels = labelst)+
  coord_flip()
  
##Filter by gender
filter1 <- "F"
wasaData18FilterG <- wasaData18c %>%
  filter(str_detect(category, filter1))

##Filter by Category
filter2 <- "M3034"
wasaData18FilterC <- wasaData18c %>%
  filter(str_detect(category, filter2))
paste(filter1,filter2,sep = "")
## Plot Position
ggplot(wasaData18FilterC, aes(x=AgePosition, y = Time))+
  geom_point()
##Print Table top 5
datatable(data = wasaData18FilterC[,], 
              options = list(pageLength = 5), 
              rownames = FALSE)
#-----------------------------------------------------------------------
#-----------------------------------------------------------------------
#Regresion Model Time 
#

to_seconds <- function(x){
  duration(hour = hour(x), minute = minute(x),second = second(x))
}
clean_lm <- function(x){
  x%>%
  select(-c("Name","year"))%>%
  mutate_at(c(2,6,8,10),to_seconds)%>%
  mutate_at(c(7,9),ms)%>%
  mutate_at(c(2,6:10),as.numeric)%>%
  mutate_at(c("gender","category"),factor)
}
wasaDataLm <- clean_lm(wasaData)
flm1 <- formula(Position ~ Swim+T1+Bike+T2+Run)
flm2 <- formula(AgePosition ~ Swim+T1+Bike+T2+Run)
model_all <- lm(flm1,data=wasaDataLm)
#ggcorr(wasaDataLm,label=TRUE,palette="RdBu",label_color = "black",color="black")
prediction_time <- data.frame(Swim=1349,T1=100,Bike=3790,T2=44,Run=2248,gender ="M",category="2529")
p_all<- round(predict(model_all, prediction_time))

##models by age
  wasaDataAge <- wasaDataLm %>%
  filter(category == "2529", gender == "M")
  m <- lm(flm2,data=wasaDataAge)
  testD <- data.frame(Swim=1449,T1=100,Bike=3790,T2=44,Run=2248)
  pAge <- (round(predict(m,wasaDataAge)))
  plot(pAge,wasaDataAge$AgePosition)

  
  #---------------------
  #backup app
  wasaData18 <- read_excel("wasaData.xlsx", sheet = "2018")
  wasaData17 <- read_excel("wasaData.xlsx", sheet = "2017")
  wasaData16 <- read_excel("wasaData.xlsx", sheet = "2016")
  wasaData15 <- read_excel("wasaData.xlsx", sheet = "2015")
  wasaData14 <- read_excel("wasaData.xlsx", sheet = "2014")
  wasaData13 <- read_excel("wasaData.xlsx", sheet = "2013")
  
  wasaData18 <- rename(wasaData18, Division ="20")
  cleandata <- function(x,y) {
    x %>%
      rename(category ="Division",age_group = "Place/Division",genderP ="Place/Gender")%>%
      separate(age_group,into = c("AgePosition","ToalAge"), sep = "/")%>%
      separate(genderP,into = c("GenderPosition","ToaGender"), sep = "/")%>%
      separate(Place,into = c("Position","TotalP"), sep = "/")%>%
      fill(Swim, T1,Bike,T2,Run)%>%
      select(-c("Event","Race #","ToalAge","ToaGender","TotalP"))%>%
      mutate_at(c(5:6,1),as.numeric)%>%
      filter(str_detect(category,"(^F|^M)(?=\\d)"))%>%
      mutate(category=as.factor(category),gender = ifelse(str_detect(category,"F"),"F","M"))%>%
      mutate(gender = as.factor(gender),year = as.factor(y),category = str_replace(category,"[A-Z]",""))%>%
      mutate(category = str_replace(category,"OLY",""))%>%
      mutate(category = str_replace(category,"70\\+","7074"))%>%
      mutate_at(c("T1","T2"), as.character)%>%
      separate(T1,into = c("date","T1"),sep = " ")%>%
      separate(T2,into = c("date2","T2"),sep = " ")%>%
      mutate (T2 =str_replace(T2,":00",""), T1=str_replace(T1,":00",""))%>%
      select(-c("date","date2"))%>%
      filter(!is.na(Time))
  }
  
  wasaData18c <- cleandata(wasaData18,2018)
  wasaData17c <- cleandata(wasaData17,2017)
  wasaData16c <- cleandata(wasaData16,2016)
  wasaData15c <- cleandata(wasaData15,2015)
  wasaData14c <- cleandata(wasaData14,2014)
  wasaData13c <- cleandata(wasaData13,2013)
  
  wasaData <- bind_rows(wasaData18c,wasaData17c,wasaData16c,wasaData15c,wasaData14c,wasaData13c)
  wasaData$year <- as.factor(wasaData$year) 
  
  clean_lm <- function(x){
    x%>%
      select(-c("Name","year"))%>%
      mutate_at(c(2,6,8,10),to_seconds)%>%
      mutate_at(c(7,9),ms)%>%
      mutate_at(c(2,6:10),as.numeric)%>%
      mutate_at(c("gender","category"),factor)
  }
