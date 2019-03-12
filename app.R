#title: "FinalProjectRS JH"
#author: "Alejandro Coy"
#date: '2019-02-28'
#Wasa Triathlon App

#Load libraries    

library(shiny)
library(readr)
library(ggplot2)
library(shinythemes)
library(gridExtra) 
library(dplyr)
library(lubridate)
library(stringr)
library(tidyr)  
library(ggdark) #graphs
library(leaflet)  #maps
library(DT)#data table

#--------------------------------------------------------
wasaData <- read_csv("wasaData.csv")
wasaData <- wasaData %>%
  mutate_at(c("Event","category","gender","year"),factor) #creating factor variables

# label the axis for formating time 
yLabels <- function(x)
{
  x <- seconds_to_period(x)
  sprintf("%02d:%02d:%02d",hour(x),minute(x),second(x))
}

# cahnge dataframe for using in lineal regresion model
clean_lm <- function(x){
  x%>%
    select(-c("Name","Event"))
}
flm1 <- formula(Position ~ Swim+T1+Bike+T2+Run)
flm2 <- formula(AgePosition ~ Swim+T1+Bike+T2+Run)

#------------------------------------------------------------------------

# Define UI 
ui <- navbarPage( theme = shinytheme("cyborg"), #shinny themse selector
   
   # Application title
   title = "Wasa 2018 Triathlon EDA",
   
   # Tab Home-----------------------------------------------
      tabPanel( "Home",
               h2("Olimpic Wasa Triathlon Analytics"),
              hr(),
              p( "In its promotional campaign, the race is described as: “Gerick Sports Wasa Lake Triathlon features elite prize money and attracts some of the fastest triathletes in the West. Age groupers and elites compete together on a scenic and fast course.” Wasa Lake is located in the beautiful province of British Columbia just west from the Rocky Mountains and 4 hours away from Calgary.
                 In this exploratory data analysis project, we will explore the results for the Olympic race distance (1.5 km swim, 40 km bike ,10 km run) and Sprint distance (750 m swim, 20 km bike ,5 km run). I scrap the data from the official timing company’s websiteand a copy of the file is available in the GitHub folderof the project."
              ),
              p("To check the results you have two option: Overall results and by Categories. Select the Tab you prfer and have Fun!!"),
              br(),
              p("In Addition the app has a predictor that took in consideration the results of the previous six years and predict your overall and age group position. Go to Prediction tab"),
              br(),
              p("Finally you can look for any result in the tab 'Search'. Just type the name"),
              br(),
              h4("Wasa Lake Location"),
              leafletOutput("wasaMap"),
              h3("Participation Information"),
              selectInput(inputId = "distance1",
                          label="Select Event: ",
                          choices = c("Olympic","Sprint"),
                          selected = "Olympic"
                          ),
              checkboxGroupInput(inputId = "selected_yeartotal",
                                 label = "Select year:",
                                 choices = c("2018","2017","2016","2015","2014","2013"),
                                 selected = c("2018","2017","2016","2015","2014","2013"),
                                 inline = TRUE
                                 
              ),
              plotOutput(outputId = "Athletes") #plot output
          ),
   # Tab overall----------------------------------------------
      tabPanel("Overall",
         h1("Overall Results",align="center"),
         flowLayout(selectInput( inputId = "GenderMain",
                      label = "Select Gender:",
                      choices = c("All" ="all", "Female" = "F", "Male" = "M"),
                      selected = "all"
         ),
         checkboxGroupInput(inputId = "selected_year",
                            label = "Select year:",
                            choices = c("2018","2017","2016","2015","2014","2013"),
                            selected = "2018",
                            inline = TRUE
                            
         
      ),
      selectInput(inputId = "distance2",
                  label="Select Event: ",
                  choices = c("Olympic","Sprint"),
                  selected = "Olympic"
      )
      ), htmlOutput(outputId = "titleBarplot"),
         div(plotOutput(outputId = "mainPlot",width = "100%"),align="center",width ="70%"),
         hr(),
         plotOutput("histogram")
         
      ),
   # Tab Categories----------------------------------------------
      tabPanel (title ="Categories",
                flowLayout(selectInput( inputId = "Gender",
                             label = "Select Gender:",
                             choices = c("All" = "all", "Female" = "F", "Male" = "M"),
                             selected = "all"
                ),
                selectInput( inputId = "AgeGroup",
                             label = "Select Age Group:",
                             choices = c("18-24" = "1824", "25-29" = "2529", "30-34" = "3034",
                                         "35-39" = "3539", "40-44" = "4044", "45-49" = "4549",
                                         "50-54" = "5054", "55-59" = "5559","60-64" = "6064","65-69"="6569",
                                         "70-74"="7074"
                             ),
                             selected = "3034"
                ),
                checkboxGroupInput(inputId = "selected_year2",
                                   label = "Select year:",
                                   choices = c("2018","2017","2016","2015","2014","2013"),
                                   selected = "2018",
                                   inline = TRUE
                                   
                ),
                selectInput(inputId = "distance3",
                            label="Select Event: ",
                            choices = c("Olympic","Sprint"),
                            selected = "Olympic"
                )),
             plotOutput("timePlot2"),
             hr(),
             plotOutput("histogram2")
    ),
   # Tab Predictions----------------------------------------------
   tabPanel(title= "Predictions",
              fluidPage(
                sidebarLayout(
                  sidebarPanel(
                    selectInput(inputId = "distance4",
                                label="Select the Event: ",
                                choices = c("Olympic","Sprint"),
                                selected = "Olympic"
                    ),
                    selectInput( inputId = "catA",
                                 label = "Select Age Group:",
                                 choices = c("18-24" = "1824", "25-29" = "2529", "30-34" = "3034",
                                             "35-39" = "3539", "40-44" = "4044", "45-49" = "4549",
                                             "50-54" = "5054", "55-59" = "5559","60-64" = "6064","65-69"="6569",
                                             "70-74"="7074","All"="all"
                                 ),
                                 selected = "all"
                  ),
                  selectInput( inputId = "genderA",
                               label = "Select Gender:",
                               choices = c("All" = "all", "Female" = "F", "Male" = "M"),
                               selected = "all"
                  ),
                  sliderInput( inputId= "Swimmin",
                                label= "Swim Time min:",
                                min = 10,
                                max = 59,
                                value = 30,
                                step = 1
                                
                  ),
                  sliderInput( inputId= "Swimsec",
                                label= "Swim Time sec:",
                                min = 0,
                                max = 59,
                                value = 30,
                                step = 1
                                
                  ),
                  sliderInput( inputId= "T1min",
                                label= "Transition 1 Time:",
                                min = 0,
                                max = 10,
                                value = 2,
                                step = 1
                    ),
                  sliderInput( inputId= "T1sec",
                               label= "Transition Time sec:",
                               min = 0,
                               max = 59,
                               value = 30,
                               step = 1
                      ),
                  sliderInput( inputId= "Bikemin",
                                label= "Bike Time min:",
                                min = 30,
                                max = 200,
                                value = 73,
                                step = 1
                                
                  ),
                  sliderInput( inputId= "Bikesec",
                                label= "Bike Time sec:",
                                min = 0,
                                max = 59,
                                value = 15,
                                step = 1
                                
                  ),
                  sliderInput( inputId= "T2min",
                                label= "Transition 2 Time:",
                                min = 0,
                                max = 10,
                                value = 1,
                                step = 1
                  ),
                  sliderInput( inputId= "T2sec",
                                label= "T2 Time sec:",
                                min = 0,
                                max = 59,
                                value = 20,
                                step = 1
                  ),
                  sliderInput( inputId= "Runmin",
                                label= "Run Time min:",
                                min = 15,
                                max = 200,
                                value = 53,
                                step = 1
                                
                  ),
                  sliderInput( inputId= "Runsec",
                                label= "Run Time sec:",
                                min = 0,
                                max = 59,
                                value = 15,
                                step = 1
                                
                  )
                  
                  ),
                  mainPanel(
                    h5("Please input the time for each sport and transition using the sliders in the left tab. Age group can be selected to predict position specifically for each group"),
                    br(),br(),
                    h4("Splits for Prediction:"),
                    dataTableOutput(outputId = "raceTime"),
                    htmlOutput(outputId = "results"),
                    htmlOutput(outputId = "resultsAge"),
                    br(),br(),
                    dataTableOutput(outputId = "podiumaverage"),
                    htmlOutput(outputId = "podium")
                    )
                )  
              )
            ),
   tabPanel( title = "Search",
             fluidPage(
              dataTableOutput(outputId = "search")  
              )
            )
   
)

# Define server logic r
server <- function(input, output) {
          
#-------------------------------------------------------
#Logic for filtering the data set by Event
#-------------------------------------------------------
            wasaData1 <- reactive({
              wasaData %>%
                filter(Event == input$distance1 )
            })
            wasaData2 <- reactive({
              wasaData %>%
                filter(Event == input$distance2 )
            })
            wasaData3 <- reactive({
              wasaData %>%
                filter(Event == input$distance3 )
            })
            wasaData4 <- reactive({
              wasaData %>%
                filter(Event == input$distance4 )
            })
            
#reactive function for filter data in order to plot "Total Time vs Race Position" - tab caregories
            wasaDataF <-reactive({
            req(input$selected_year)
            year_selection <- c()
            for(i in 1:length(input$selected_year2)) {
              year_selection <- c(year_selection,input$selected_year2[i])
            }
          
            if(input$Gender != "all"){
              if(input$AgeGroup !="all"){
                wasaData3() %>%
                
                filter(year %in% year_selection)%>%
                filter((category==input$AgeGroup & gender==input$Gender))
              }
              else {
                wasaData3() %>% filter(year %in% year_selection)%>%
                filter(gender==input$Gender)
              }
            }else if(input$AgeGroup != "all"){
              wasaData3() %>% filter(year %in% year_selection)%>%
              filter(category==input$AgeGroup)
              } 
             else{
               wasaData3() %>% filter(year %in% year_selection)
            }
              })
# Reactive Function for ploting the average time for each sport and categroy- Tab Overall
          wasafilter <- reactive({
            req(input$selected_year)
            if(input$GenderMain !="all"){
              wasafilter1 <- wasaData2() %>%
                  filter(year == input$selected_year)%>%
                  filter(gender == input$GenderMain)%>%
                  group_by(category)%>%
                  summarise(Swim = mean(Swim),Bike = mean(Bike),Run = mean(Run))%>%
                  gather(key="sport",value = "averageTime", 2:4)%>%
                mutate(sport = factor(sport,levels = c("Run","Bike","Swim")))

            
            } else 
              wasafilter1 <- wasaData2() %>%
                  filter(year == input$selected_year)%>%
                  group_by(category)%>%
                  summarise(Swim = mean(Swim),Bike = mean(Bike),Run = mean(Run))%>%
                  gather(key="sport",value = "averageTime", 2:4)%>%
                  mutate(sport = factor(sport,levels = c("Run","Bike","Swim")))
          })
          
#Reactive function for filter by gender tab -overall
          wasafilterGender <-reactive({
            req(input$GenderMain)
            if(input$GenderMain != "all"){
                filter(wasaData2(),(gender==input$GenderMain))
            }
              else {
                 wasaData2()
            }
          })
        
   #Reactive function for filter by year after Gender filter Histrograms Plots - tab overall 
         wasafilterAll <- reactive ({
           req(input$selected_year)
           year_selection2 <- c()
           for(i in 1:length(input$selected_year)) {
             year_selection2 <- c(year_selection2,input$selected_year[i])
           }
           #if(input$selected_year != 'all'){
             filter(wasafilterGender(),(year %in% year_selection2))
           #}else {
            # wasafilterGender()
           #}
         })
    
         
        #"Total Time vs Race Position -tab overall"
          m1 <- reactive({ggplot(data = wasafilterAll(), aes(x= Position, y = Time, color=year))+
            geom_point()+
            theme_light()+
            scale_y_continuous(labels = yLabels)+
            dark_theme_gray(base_size = 14)
          })
        
          # Reactive Function for ploting the average time for each sport and categroy- Tab Overall
          m2 <- reactive({
          
            ggplot(wasafilter(), aes(x=category, y=averageTime,fill=sport))+
              geom_bar(stat="identity")+
              scale_y_continuous(labels = yLabels)+
              coord_flip()+
              dark_theme_gray(base_size = 14)+
              ylab("Avg Time")+
              xlab("Category")
          })
          #Histogram for Swim  - tab overall 
          h1 <- reactive({
            ggplot(data = wasafilterAll(), aes(x = Swim, fill= year))+
            geom_histogram(alpha=0.5,position = "identity",binwidth = 180)+
              theme(text = element_text(size=15))+
              geom_vline(data=wasafilterAll(), aes(xintercept=mean(Swim),  colour=year),
                         linetype="dashed", size=1)+
              dark_theme_gray(base_size = 14)+
              ylab("Athletes")+
              xlab("Time")+
              ggtitle("Swim")+
              scale_x_continuous(labels = yLabels)
          })
          #Histogram for Bike  - tab overall 
          h2 <- reactive({
            ggplot(data = wasafilterAll(), aes( x = Bike,fill=year))+
              geom_histogram(alpha=0.5,position="identity",binwidth = 180)+
              geom_vline(data=wasafilterAll(), aes(xintercept=mean(Bike),  colour=year),
                         linetype="dashed", size=1)+
              theme(text = element_text(size=15))+
              dark_theme_gray(base_size = 14)+
              ylab("Athletes")+
              xlab("Time")+
              ggtitle("Bike")+
              scale_x_continuous(labels = yLabels)
          })
        #Histogram for Run  - tab overall 
          h3 <- reactive({
            ggplot(data = wasafilterAll(), aes( x = Run,fill= year))+
              geom_histogram(alpha=0.5,position="identity",binwidth = 180)+
              geom_vline(data=wasafilterAll(), aes(xintercept=mean(Run),  colour=year),
                         linetype="dashed", size=1)+
              dark_theme_gray(base_size = 14)+
              ylab("Athletes")+
              xlab("Time")+
              ggtitle("Run")+
              scale_x_continuous(labels = yLabels)
          })
          
          l1 <- reactive({
            ggplot(data = wasaDataF(), aes( y = Swim, x =AgePosition,color=year,linetype=gender))+
              geom_point()+
              geom_line()+
              dark_theme_gray(base_size = 14)+
              ylab("Swim Time")+
              xlab("Age Group Position")+
              ggtitle("Swim Position")+
              scale_y_continuous(labels = yLabels)
             
              
          })
          l2 <- reactive({
            ggplot(data = wasaDataF(), aes( y = Bike, x= AgePosition,color=year,linetype=gender))+
              geom_line()+
              geom_point()+
              dark_theme_gray(base_size = 14)+
              ylab("Bike Time")+
              xlab("Age Group Position")+
              ggtitle("Bike Position")+
              scale_y_continuous(labels = yLabels)
          })
          l3 <- reactive({
            ggplot(data = wasaDataF(), aes( y = Run, x=AgePosition,color =year,linetype=gender))+
              geom_line()+
              geom_point()+
              dark_theme_gray(base_size = 14)+
              ylab("Run Time")+
              xlab("Age Group Position")+
              ggtitle("Run Position")+
              scale_y_continuous(labels = yLabels)
          })
      
        numberAthletes <- reactive({ 
          year_total <- c()
          for(i in 1:length(input$selected_yeartotal)) {
            year_total <- c(year_total,input$selected_yeartotal[i])
          }
          wasaData1() %>%
          group_by(category,year,gender)%>%
          summarize(Athletes = n())%>%
          filter(year %in% year_total)
          })
          
        a1 <- reactive({ggplot(numberAthletes(), aes (x=category,y=Athletes,fill=year))+
          geom_col()+
          dark_theme_gray(base_size = 14)+
            ylab("Number of Athletes")+
            ggtitle("Participants By Category")
        })
          
        totalnumberAthletes <- reactive({
          wasaData1() %>%
          group_by(year,gender)%>%
          summarize(Athletes = n())
          
          })
        
       a2 <- reactive({ ggplot(totalnumberAthletes(), aes (x=year,y=Athletes,fill=gender))+
            geom_col()+
            scale_fill_brewer(palette="Set1")+
            dark_theme_gray(base_size = 14)+
           ggtitle("Total Participant By Year")
       })
       
      output$wasaMap <- renderLeaflet({
        leaflet()%>%
          setView(lng =-115.74, lat =49.789 , zoom =12)%>%
          addTiles()%>%
          addMarkers(-115.740950,49.789852,label="Wasa Triathlon")
      }) 
      output$Athletes <- renderPlot(
        grid.arrange(a1(),a2(),ncol=2)
      )
          
      output$titleBarplot <- renderUI({
        years <- c()
        for(i in 1:length(input$selected_year)) {
          years <- c(years,input$selected_year[i])
        }
        years <- paste(years,sep = "",collapse=",")
        HTML("<h4 style='text-align:right'>", paste0("Average Time per Category Year: ",years),"</h4>")
      })
      output$mainPlot <- renderPlot({
      #plot Position vs Time overall - tab overlla
      grid.arrange(m1(),m2(),ncol =2)
   })
      output$histogram <- renderPlot({
        #Histograms plot - tab overall
      grid.arrange(h1(),h2(),h3(), ncol = 3)
    
      })
      output$timePlot2 <- renderPlot({
        #plot  total time vs race Age group position - tab Categories
        ggplot(data = wasaDataF(), aes(x= AgePosition, y = Time, color=year,linetype=gender))+
          geom_point()+
          geom_line()+
          dark_theme_gray(base_size = 14)+
          ylab("Total Time")+
          xlab("Age Group Position")+
          ggtitle("Total Time vs Race Position")+
          scale_y_continuous(labels = yLabels)
          
      })
      output$histogram2 <- renderPlot({
        #plot position  vs time for each sport - tab categories
        grid.arrange(l1(),l2(),l3(), ncol = 3)
      })
      
      swimT <-reactive({as.numeric(ms(paste(input$Swimmin,":",input$Swimsec)))}) 
      T1T <- reactive({as.numeric(ms(paste(input$T1min,":",input$T1sec)))})
      bikeT <- reactive({as.numeric(ms(paste(input$Bikemin,":",input$Runsec)))})
      T2T <- reactive({as.numeric(ms(paste(input$T2min,":",input$T2sec)))})
      runT <- reactive({as.numeric(ms(paste(input$Runmin,":",input$Runsec)))})
      timeAthlete <-reactive({
        data.frame(Swim=swimT(),T1=T1T(),Bike=bikeT(),T2=T2T(),Run=runT(),Time=swimT()+T1T()+bikeT()+T2T()+runT(),gender=input$genderA,category=input$catA)
      }) 
      timeAthletetoPrint <-reactive({  
        timeAthlete()%>%
        mutate_at(1:6,yLabels)
      })
      wasaDataLm <-reactive({
        clean_lm(wasaData4()) 
        })
      #Formulas for  the two models. Outcome is changed
     
      #Model for all the data 
      model_all <- reactive({
        lm(flm1,data=wasaDataLm())
      })
      #Regresion Model Time 
      #ggcorr(wasaDataLm,label=TRUE,palette="RdBu",label_color = "black",color="black")
      p_all<- reactive({
        round(predict(model_all(), timeAthlete()))
      })
      ##models by age
      wasaDataAge <- reactive({
        if(input$catA !='all' & input$genderA != 'all'){
        wasaDataLm() %>%
        filter(category == input$catA, gender == input$genderA)
        }
          })
      pAge <- reactive({
       m <- lm(flm2,data=wasaDataAge())
      round(predict(m,timeAthlete()))
      })
      podiumtime <- reactive({
        wasaDataAge()%>%
          filter(AgePosition == 3)%>%
          select(-c("Position","gender"))
      })
        mean_podium <- reactive({
          meanPodium <- round(mean(podiumtime()$Time))
          yLabels(meanPodium)
        })
       
        
      podiumtimetoprint <- reactive({
        podiumtime()%>%
        mutate_at(c("Time","Swim","T1","Bike","T2","Run"),yLabels)
      })
     wasaToPrint <- reactive({
       wasaData%>%
         mutate_at(c("Time","Swim","T1","Bike","T2","Run"),yLabels)
     })
     
      output$raceTime <- renderDataTable({
        datatable(timeAthletetoPrint(), 
        options = list(pageLength = 1,dom="t"),
        rownames = FALSE,
        style ="bootstrap"
        )
      })
      output$results <- renderUI({
          HTML("<h4>",paste("Predicted Overall  position is: <strong>",p_all())," </h3>")
        })
      output$resultsAge <- renderUI({
        if(input$catA !='all' & input$genderA != 'all'){
        HTML("<h4>",paste("Predict Age Group position is Age:<strong>",pAge())," </h3>")
        }
         })
      output$podiumaverage <- renderDataTable({
        if(input$catA !='all' & input$genderA != 'all'){
        datatable(podiumtimetoprint(), 
                  options = list(dom="t"),
                  rownames = FALSE
       ) %>% formatStyle(names(podiumtimetoprint()),color="black")
      }
      })
      output$podium <- renderUI({
        if(input$catA !='all' & input$genderA != 'all'){
        HTML("<h4>",paste("Average podium is: <strong>",mean_podium()," </h3>"))
        }
      })
      
      output$search <- renderDataTable({
          datatable(wasaToPrint(), 
                    options = list(pageLength = 10),
                    rownames = FALSE
          ) %>% formatStyle(names(wasaToPrint()),color="black")
     })
      
      
     
}

# Run the application 
shinyApp(ui = ui, server = server)


