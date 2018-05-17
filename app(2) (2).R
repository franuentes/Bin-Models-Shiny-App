
library(magrittr)
library(dplyr)
library(data.table)
library(ggplot2)
library(DT)
library(shiny)
library(lubridate)
library(weathermetrics)

load("./Data/data-2017_updated-with-fitted-May8.RData")


# services <- df$Service %>% unique
buildingNames = df$Name %>% unique %>% sort

## ## Variables for trying code without running app
## df00 <- df
## input <- list(binWidth = 5,
##               startingMonth = "January",
##               endMonth = "October",
##               Building = buildingNames[1],
##               Service = "Chilled water")


bin0 <- 5
t.min <- floor(min(df$Temperature) / bin0) * bin0
t.max <- ceiling(max(df$Temperature) / bin0) * bin0

week_begin_dates <- unique(df$week_begin_date)
week_end_dates <- unique(df$week_end_date)
two_week_begin_dates <- unique(df$two_week_begin_date)
two_week_end_dates <- unique(df$two_week_end_date)

ui <- pageWithSidebar(

    headerPanel(""),

    sidebarPanel(
        selectInput("bin_frequency", "Please select how often to bin observations.",
                  choices=c( "Monthly", "Biweekly", "Weekly"),
                  selected="Monthly"),
        conditionalPanel(condition="input.bin_frequency=='Monthly'",
                         uiOutput("startingMonth"),
                         uiOutput("finalMonth")),
        conditionalPanel(condition="input.bin_frequency=='Biweekly'",
                         uiOutput("startingTwoWeek"),
                         uiOutput("finalTwoWeek")),
        conditionalPanel(condition="input.bin_frequency=='Weekly'",
                         uiOutput("startingWeek"),
                         uiOutput("finalWeek")),
        uiOutput("Service"),
        selectInput("Building", "Please Select Building.",
                    choices = buildingNames,
                    selected = buildingNames[1]),
        selectInput("binmethod", "Please Select Variable to Bin By.",
                    choices = c('Temperature', 'Humidity', 'Heat Index'),
                    selected = 'Temperature'),
        conditionalPanel(condition="input.binmethod == 'Temperature'",
                         uiOutput('temp_binwidth')),
        conditionalPanel(condition="input.binmethod == 'Humidity'",
                         uiOutput("humid_binwidth")),
        conditionalPanel(condition="input.binmethod == 'Heat Index'",
                         uiOutput("hi_binwidth")),
        conditionalPanel(condition="input.binmethod == 'Temperature'",
                         uiOutput('Temp')),
        conditionalPanel(condition="input.binmethod == 'Humidity'",
                         uiOutput("Humid")),
        conditionalPanel(condition="input.binmethod == 'Heat Index'",
                         uiOutput("heat_index")),
        checkboxInput("min_size1", "Please select to show only boxplots above a certain sample size.",
                    value=FALSE),
        conditionalPanel(condition="input.min_size1 == true",
                         sliderInput("min_size2", "Please specify minimum sample size for each boxplot.",
                                     min=0, max=50, step=5, value=0)),
        checkboxInput("fitted", "Please select to show fitted values next to actual values.", value=FALSE),
        checkboxInput("freeY", "Please select for Universal y-axis",
                    value=FALSE),
        actionButton("apply", "Apply Changes")
    ),

    mainPanel(
        plotOutput(outputId="myPlot",height = "auto", width="100%")
    )
)

server <- function(input, output, session){
  npanels <- reactive({
    req(input$Building)
    req(input$Service)
    
    df <- df %>% filter(Name == input$Building,
                        Service == input$Service
    )
    
    if(input$bin_frequency=="Monthly"){
      req(input$startingMonth)
      req(input$endMonth)
      months <- unique(df$Month)
      startMonth <- months[months == match(input$startingMonth, month.name)]
      endMonth <- months[months == match(input$endMonth, month.name)]
      df <- df %>% filter(Month >= startMonth,
                          Month <= endMonth
      )
    }
    
    if(input$bin_frequency=="Biweekly"){
      req(input$startingTwoWeek)
      req(input$endTwoWeek)
      two_week_begin_dates <- unique(df$two_week_begin_date)
      two_week_end_dates <- unique(df$two_week_end_date)
      startTwoWeek <- two_week_begin_dates[two_week_begin_dates==input$startingTwoWeek]
      endTwoWeek <- two_week_end_dates[two_week_end_dates==input$endTwoWeek]
      df <- df %>% filter(two_week_begin_date >= startTwoWeek,
                          two_week_end_date <= endTwoWeek
      )
    }
    
    if(input$bin_frequency=="Weekly"){
      req(input$startingWeek)
      req(input$endWeek)
      week_begin_dates <- unique(df$week_begin_date)
      week_end_dates <- unique(df$week_end_date)
      startWeek <- week_begin_dates[week_begin_dates==input$startingWeek]
      endWeek <- week_end_dates[week_end_dates==input$endWeek]
      df <- df %>% filter(week_begin_date >= startWeek,
                          week_end_date <= endWeek
      )
    }
    
    if(input$binmethod=='Temperature'){
      req(input$Temp)
      req(input$temp_binWidth)
      df <- df[df$Temperature>=min(input$Temp),]
      df <- df[df$Temperature<=max(input$Temp),]
      df$bin_midpoint <- ceiling(df$Temperature/input$temp_binWidth)*input$temp_binWidth-(input$temp_binWidth/2)}
    
    else if(input$binmethod=='Humidity'){
      req(input$Humid)
      req(input$humid_binWidth)
      df <- df[df$Humidity>=min(input$Humid),]
      df <- df[df$Humidity<=max(input$Humid),]
      df$bin_midpoint <- ceiling(df$Humidity/input$humid_binWidth)*input$humid_binWidth-(input$humid_binWidth/2)}
    
    else if (input$binmethod=='Heat Index'){
      req(input$heat_index)
      req(input$hi_binWidth)
      df <- df[df$heat_index>=min(input$heat_index),]
      df <- df[df$heat_index<=max(input$heat_index),]
      df$bin_midpoint <- ceiling(df$heat_index/input$hi_binWidth)*input$hi_binWidth-(input$hi_binWidth/2)}
    
    if(input$bin_frequency=="Monthly"){
      df.months <- list()
      bins <- sort(unique(df$bin_midpoint))
      for (i in startMonth:endMonth) {
        d1 <- data.frame(bin_midpoint = bins)
        d <-
          df %>%
          ## Only this month
          filter(Month == i)
        ## Group by bin
        d <- 
          d %>%
          group_by(bin_midpoint)
        d <- 
          
          d %>%
          ## Count within each bin
          summarize(freq = n())
        d1 <- data.frame(bin_midpoint = bins)
        d <- left_join(d1, d)
        d$freq[is.na(d$freq)] <- 0
        d$Month <- i
        df.months[[i]] <- d
      }
      ## Make a single data frame
      df.months <- bind_rows(df.months)
      df <- left_join(df, df.months, by = c("Month" = "Month", "bin_midpoint" = "bin_midpoint"))
    }
    
    if(input$bin_frequency=="Biweekly"){
      df.two_weeks <- list()
      bins <- sort(unique(df$bin_midpoint))
      for (i in min(df$two_week_period):max(df$two_week_period)) {
        d1 <- data.frame(bin_midpoint = bins)
        d <-
          df %>%
          ## Only this month
          filter(two_week_period == i)
        ## Group by bin
        d <- 
          d %>%
          group_by(bin_midpoint)
        d <- 
          
          d %>%
          ## Count within each bin
          summarize(freq = n())
        d1 <- data.frame(bin_midpoint = bins)
        d <- left_join(d1, d)
        d$freq[is.na(d$freq)] <- 0
        d$two_week_period <- i
        df.two_weeks[[i]] <- d
      }
      ## Make a single data frame
      df.two_weeks <- bind_rows(df.two_weeks)
      df <- left_join(df, df.two_weeks, by = c("two_week_period" = "two_week_period", "bin_midpoint" = "bin_midpoint"))
    }
    
    if(input$bin_frequency=="Weekly"){
      df.weeks <- list()
      bins <- sort(unique(df$bin_midpoint))
      for (i in min(df$week_period):max(df$week_period)) {
        d1 <- data.frame(bin_midpoint = bins)
        d <-
          df %>%
          ## Only this month
          filter(week_period == i)
        ## Group by bin
        d <- 
          d %>%
          group_by(bin_midpoint)
        d <- 
          
          d %>%
          ## Count within each bin
          summarize(freq = n())
        d1 <- data.frame(bin_midpoint = bins)
        d <- left_join(d1, d)
        d$freq[is.na(d$freq)] <- 0
        d$week_period <- i
        df.weeks[[i]] <- d
      }
      ## Make a single data frame
      df.weeks <- bind_rows(df.weeks)
      df <- left_join(df, df.weeks, by = c("week_period" = "week_period", "bin_midpoint" = "bin_midpoint"))
    }
    
    df <- df[df$freq >= input$min_size2,]
    
npanels <- 250*length(unique(df$bin_midpoint))
npanels <- ifelse(length(unique(df$bin_midpoint))==1, npanels*1.5, npanels)
print(npanels)
})
  
  output$startingMonth <- renderUI({
    req(input$Building)
    req(input$Service)
    df <- df %>% filter(Name == input$Building,
                        Service == input$Service)
    months <- unique(df$Month)
    current <- input$startingMonth
    if (isTruthy(input$startingMonth)) {
      req(input$startingMonth)
      current <- input$startingMonth
      }
      else{current <- month.name[min(sort(months))]
      }
    options <- month.name[sort(months)]
    if(isTruthy(input$endMonth)){
      req(input$endMonth)
      options <- month.name[sort(months[months <= months[month.name[months]==input$endMonth]])]
    } else{
      options <- month.name[sort(months)]      
    }
    selectInput("startingMonth", "Please Select Initial Month.",
                choices = options,
                selected = current)
  })
  
  output$finalMonth <- renderUI({
    req(input$startingMonth)
    req(input$Building)
    req(input$Service)
    df <- df %>% filter(Name == input$Building,
                        Service == input$Service)
    months <- unique(df$Month)
    current <- month.name[max(months)]

    ## Check for whether input exists
    if (isTruthy(input$endMonth)) {
      req(input$endMonth)
      if(input$endMonth %in% month.name[months]){
      current <- input$endMonth
      }
    else {
      current <- month.name[max(months)]
    }}
    selectInput("endMonth", "Please Select Final Month.",
                choices = month.name[rev(sort(months[months >= months[month.name[months]==input$startingMonth]]))],
                selected = current)
  })
  
  output$startingTwoWeek <- renderUI({
    req(input$Building)
    req(input$Service)
    df <- df %>% filter(Name == input$Building,
                        Service == input$Service)
    two_week_begin_dates <- unique(df$two_week_begin_date)
    current <- input$startingTwoWeek
    if (isTruthy(input$startingTwoWeek)) {
      req(input$startingTwoWeek)
      current <- input$startingTwoWeek
    }
    else{current <- min(two_week_begin_dates)
    }
    options <- sort(two_week_begin_dates)
    if(isTruthy(input$endTwoWeek)){
      req(input$endTwoWeek)
      options <- sort(two_week_begin_dates[two_week_begin_dates <= input$endTwoWeek])
    } else{
      options <- sort(two_week_begin_dates)     
    }
    selectInput("startingTwoWeek", "Please Select Initial Two Week Period.",
                choices = options,
                selected = current)
  })
  
  output$finalTwoWeek <- renderUI({
    req(input$startingTwoWeek)
    req(input$Building)
    req(input$Service)
    df <- df %>% filter(Name == input$Building,
                        Service == input$Service)
    two_week_end_dates <- unique(df$two_week_end_date)
    current <- max(two_week_end_dates)
    ## Check for whether input exists
    if (isTruthy(input$endTwoWeek)) {
      current <- input$endTwoWeek
    } else {
      current <- max(two_week_end_dates)
    }
    selectInput("endTwoWeek", "Please Select Final Two-Week Period.",
                choices = rev(sort(two_week_end_dates[two_week_end_dates >= input$startingTwoWeek])),
                selected = current)
  })
  
  output$startingWeek <- renderUI({
    req(input$Building)
    req(input$Service)
    df <- df %>% filter(Name == input$Building,
                        Service == input$Service)
    week_begin_dates <- unique(df$week_begin_date)
    current <- input$startingWeek
    if (isTruthy(input$startingWeek)) {
      req(input$startingWeek)
      current <- input$startingWeek
    }
    else{current <- min(week_begin_dates)
    }
    options <- sort(week_begin_dates)
    if(isTruthy(input$endWeek)){
      req(input$endWeek)
      options <- sort(week_begin_dates[week_begin_dates <= input$endWeek])
    } else{
      options <- sort(week_begin_dates)     
    }
    selectInput("startingWeek", "Please Select Initial Week Period.",
                choices = options,
                selected = current)
  })
  
  output$finalWeek <- renderUI({
    req(input$startingWeek)
    req(input$Building)
    req(input$Service)
    df <- df %>% filter(Name == input$Building,
                        Service == input$Service)
    week_end_dates <- unique(df$week_end_date)
    current <- max(week_end_dates)
    ## Check for whether input exists
    if (isTruthy(input$endWeek)) {
      current <- input$endWeek
    } else {
      current <- max(week_end_dates)
    }
    selectInput("endWeek", "Please Select Final Week Period.",
                choices = rev(sort(week_end_dates[week_end_dates >= input$startingWeek])),
                selected = current)
  })
  
  
  
  output$Service <- renderUI({
    req(input$Building)
    
    df <- df %>% filter(Name == input$Building)
    services <- unique(df$Service)
    if(length(services)==1){
      current <- services
      print(paste(current, 1))
    }
    
    else if (isTruthy(input$Service)) {
      req(input$Service)
      current <- input$Service
      print(paste(current, 2))
      
      }
      else {
        current <- input$Service
        print(paste(current, 3))
        
      }
    
    selectInput("Service", "Please Select Service.",
                choices=sort(unique(df$Service[df$Name %in% input$Building])),
                selected = current)
    
  })
  
  output$Temp <- renderUI({
    req(input$Building)
    req(input$Service)
    df <- df %>% filter(Name == input$Building,
                        Service == input$Service)
    
    if(input$bin_frequency=="Monthly"){
      req(input$startingMonth)
      req(input$endMonth)
      months <- unique(df$Month)
      startMonth <- months[months == match(input$startingMonth, month.name)]
      endMonth <- months[months == match(input$endMonth, month.name)]
      df <- df %>% filter(Month >= startMonth,
                          Month <= endMonth
      )
    }
    
    if(input$bin_frequency=="Biweekly"){
      req(input$startingTwoWeek)
      req(input$endTwoWeek)
      two_week_begin_dates <- unique(df$two_week_begin_date)
      two_week_end_dates <- unique(df$two_week_end_date)
      startTwoWeek <- two_week_begin_dates[two_week_begin_dates==input$startingTwoWeek]
      endTwoWeek <- two_week_end_dates[two_week_end_dates==input$endTwoWeek]
      df <- df %>% filter(two_week_begin_date >= startTwoWeek,
                          two_week_end_date <= endTwoWeek
      )
    }
    
    if(input$bin_frequency=="Weekly"){
      req(input$startingWeek)
      req(input$endWeek)
      week_begin_dates <- unique(df$week_begin_date)
      week_end_dates <- unique(df$week_end_date)
      startWeek <- week_begin_dates[week_begin_dates==input$startingWeek]
      endWeek <- week_end_dates[week_end_dates==input$endWeek]
      df <- df %>% filter(week_begin_date >= startWeek,
                          week_end_date <= endWeek
      )
    }
      
    t.min <- floor(min(df$Temperature)/5)*5
    t.max <- ceiling(max(df$Temperature)/5)*5
    sliderInput("Temp", "Please Select Temperature Range (°F).",
                min=t.min, max=t.max, step=5, value=c(t.min, t.max))
    
  })
  
  output$Humid <- renderUI({
    req(input$Building)
    req(input$Service)
    df <- df[df$Name %in% input$Building & df$Service==input$Service,] 
    
    if(input$bin_frequency=="Monthly"){
      req(input$startingMonth)
      req(input$endMonth)
      months <- unique(df$Month)
      startMonth <- months[months == match(input$startingMonth, month.name)]
      endMonth <- months[months == match(input$endMonth, month.name)]
      df <- df %>% filter(Month >= startMonth,
                          Month <= endMonth
      )
    }
    
    if(input$bin_frequency=="Biweekly"){
      req(input$startingTwoWeek)
      req(input$endTwoWeek)
      two_week_begin_dates <- unique(df$two_week_begin_date)
      two_week_end_dates <- unique(df$two_week_end_date)
      startTwoWeek <- two_week_begin_dates[two_week_begin_dates==input$startingTwoWeek]
      endTwoWeek <- two_week_end_dates[two_week_end_dates==input$endTwoWeek]
      df <- df %>% filter(two_week_begin_date >= startTwoWeek,
                          two_week_end_date <= endTwoWeek
      )
    }
    
    if(input$bin_frequency=="Weekly"){
      req(input$startingWeek)
      req(input$endWeek)
      week_begin_dates <- unique(df$week_begin_date)
      week_end_dates <- unique(df$week_end_date)
      startWeek <- week_begin_dates[week_begin_dates==input$startingWeek]
      endWeek <- week_end_dates[week_end_dates==input$endWeek]
      df <- df %>% filter(week_begin_date >= startWeek,
                          week_end_date <= endWeek
      )
    }
    
    h.min <- floor(min(df$Humidity)/5)*5
    h.max <- ceiling(max(df$Humidity)/5)*5
    sliderInput("Humid", "Please Select Humidity Range (%).",
                min=h.min, max=h.max, step=5, value=c(h.min, h.max))
    
  })
  
  output$heat_index <- renderUI({
    req(input$Building)
    req(input$Service)
    df <- df[df$Name %in% input$Building & df$Service==input$Service,] 
    
    if(input$bin_frequency=="Monthly"){
      req(input$startingMonth)
      req(input$endMonth)
      months <- unique(df$Month)
      startMonth <- months[months == match(input$startingMonth, month.name)]
      endMonth <- months[months == match(input$endMonth, month.name)]
      df <- df %>% filter(Month >= startMonth,
                          Month <= endMonth
      )
    }
    
    if(input$bin_frequency=="Biweekly"){
      req(input$startingTwoWeek)
      req(input$endTwoWeek)
      two_week_begin_dates <- unique(df$two_week_begin_date)
      two_week_end_dates <- unique(df$two_week_end_date)
      startTwoWeek <- two_week_begin_dates[two_week_begin_dates==input$startingTwoWeek]
      endTwoWeek <- two_week_end_dates[two_week_end_dates==input$endTwoWeek]
      df <- df %>% filter(two_week_begin_date >= startTwoWeek,
                          two_week_end_date <= endTwoWeek
      )
    }
    
    if(input$bin_frequency=="Weekly"){
      req(input$startingWeek)
      req(input$endWeek)
      week_begin_dates <- unique(df$week_begin_date)
      week_end_dates <- unique(df$week_end_date)
      startWeek <- week_begin_dates[week_begin_dates==input$startingWeek]
      endWeek <- week_end_dates[week_end_dates==input$endWeek]
      df <- df %>% filter(week_begin_date >= startWeek,
                          week_end_date <= endWeek
      )
    }
    
    hi.min <- floor(min(df$heat_index)/5)*5
    hi.max <- ceiling(max(df$heat_index)/5)*5
    sliderInput("heat_index", "Please Select Heat Index Range (°F).",
                min=hi.min, max=hi.max, step=5, value=c(hi.min, hi.max))
    
  })
  
  output$temp_binwidth <- renderUI({
    sliderInput("temp_binWidth", "Please Select Bin Width (°F).",
              min=5, max=25, step=5, value=5)
    })
  
  output$hi_binwidth <- renderUI({
    sliderInput("hi_binWidth", "Please Select Bin Width (°F).",
                min=5, max=25, step=5, value=5)
  })
  
  output$humid_binwidth <- renderUI({
    sliderInput("humid_binWidth", "Please Select Bin Width (%).",
                min=5, max=25, step=5, value=5)
  })
  
  nboxplots <- reactive({
    req(input$Building)
    req(input$Service)
    df <- df %>% filter(Name == input$Building,
                        Service == input$Service)
    
    if(input$bin_frequency=="Monthly"){
      req(input$startingMonth)
      req(input$endMonth)
      months <- unique(df$Month)
      startMonth <- months[months == match(input$startingMonth, month.name)]
      endMonth <- months[months == match(input$endMonth, month.name)]
      df <- df %>% filter(Month >= startMonth,
                          Month <= endMonth
      )
      }
    
    else     if(input$bin_frequency=="Biweekly"){
      req(input$startingTwoWeek)
      req(input$endTwoWeek)
      two_week_begin_dates <- unique(df$two_week_begin_date)
      two_week_end_dates <- unique(df$two_week_end_date)
      startTwoWeek <- two_week_begin_dates[two_week_begin_dates==input$startingTwoWeek]
      endTwoWeek <- two_week_end_dates[two_week_end_dates==input$endTwoWeek]
      df <- df %>% filter(two_week_begin_date >= startTwoWeek,
                          two_week_end_date <= endTwoWeek
      )
    }
    
    else if(input$bin_frequency=="Weekly"){
      req(input$startingWeek)
      req(input$endWeek)
      week_begin_dates <- unique(df$week_begin_date)
      week_end_dates <- unique(df$week_end_date)
      startWeek <- week_begin_dates[week_begin_dates==input$startingWeek]
      endWeek <- week_end_dates[week_end_dates==input$endWeek]
      df <- df %>% filter(week_begin_date >= startWeek,
                          week_end_date <= endWeek
      )
    }
    
    if(input$binmethod=='Temperature'){
      req(input$Temp)
      req(input$temp_binWidth)
      df <- df[df$Temperature>=min(input$Temp),]
      df <- df[df$Temperature<=max(input$Temp),]
      df$bin_midpoint <- ceiling(df$Temperature/input$temp_binWidth)*input$temp_binWidth-(input$temp_binWidth/2)
      }
    
    else if(input$binmethod=='Humidity'){
      req(input$Humid)
      req(input$humid_binWidth)
      df <- df[df$Humidity>=min(input$Humid),]
      df <- df[df$Humidity<=max(input$Humid),]
      df$bin_midpoint <- ceiling(df$Humidity/input$humid_binWidth)*input$humid_binWidth-(input$humid_binWidth/2)}
    
    else if(input$binmethod=='Heat Index'){
      req(input$heat_index)
      req(input$hi_binWidth)
      df <- df[df$heat_index>=min(input$heat_index),]
      df <- df[df$heat_index<=max(input$heat_index),]
      df$bin_midpoint <- ceiling(df$heat_index/input$hi_binWidth)*input$hi_binWidth-(input$hi_binWidth/2)}
    
    
    if(input$bin_frequency=="Monthly"){
      df.months <- list()
      bins <- sort(unique(df$bin_midpoint))
      for (i in startMonth:endMonth) {
        d1 <- data.frame(bin_midpoint = bins)
        d <-
          df %>%
          ## Only this month
          filter(Month == i)
        ## Group by bin
        d <- 
          d %>%
          group_by(bin_midpoint)
        d <- 
          
          d %>%
          ## Count within each bin
          summarize(freq = n())
        d1 <- data.frame(bin_midpoint = bins)
        d <- left_join(d1, d)
        d$freq[is.na(d$freq)] <- 0
        d$Month <- i
        df.months[[i]] <- d
      }
      ## Make a single data frame
      df.months <- bind_rows(df.months)
      df <- left_join(df, df.months, by = c("Month" = "Month", "bin_midpoint" = "bin_midpoint"))
      
      df <- df[df$freq >= input$min_size2,]
      month_dates <- data.frame(Month = startMonth:endMonth,
                                month_begin_date = ymd(sprintf("2017-%s-01", startMonth:endMonth)))
      class(month_dates$month_begin_date) <- class(df$Day)
      df<- merge(x=df, y=month_dates[, c("Month", "month_begin_date")],by.x="Month", by.y="Month")
      df$begin_date <- df$month_begin_date
      ## for(i in startMonth:endMonth){
      ##     month_dates$month_begin_date[[i-unique(df$Month)[1]+1]] <- min(df$Day[df$Month==i])
      ##     month_dates$month_end_date[[i-unique(df$Month)[1]+1]] <- max(df$Day[df$Month==i])
      ## }
      ## class(month_dates$month_end_date) <- class(df$Day)
      df$nsize <- paste(df$begin_date,"N = ", sep="\n")
      df$nsize <- paste(df$nsize, df$freq)
      boxplots <- list()
      for(i in 1:length(unique(df$bin_midpoint))){
        boxplots[[i]] <- length(unique(df$Month[df$bin_midpoint==unique(df$bin_midpoint)[i]]))
      }
      nboxplots<-max(unlist(boxplots))
    }
    
    if(input$bin_frequency=="Biweekly"){
      df.two_weeks <- list()
      bins <- sort(unique(df$bin_midpoint))
      for (i in min(df$two_week_period):max(df$two_week_period)) {
        d1 <- data.frame(bin_midpoint = bins)
        d <-
          df %>%
          ## Only this month
          filter(two_week_period == i)
        ## Group by bin
        d <- 
          d %>%
          group_by(bin_midpoint)
        d <- 
          
          d %>%
          ## Count within each bin
          summarize(freq = n())
        d1 <- data.frame(bin_midpoint = bins)
        d <- left_join(d1, d)
        d$freq[is.na(d$freq)] <- 0
        d$two_week_period <- i
        df.two_weeks[[i]] <- d
      }
      ## Make a single data frame
      df.two_weeks <- bind_rows(df.two_weeks)
      df <- left_join(df, df.two_weeks, by = c("two_week_period" = "two_week_period", "bin_midpoint" = "bin_midpoint"))
      
      df <- df[df$freq >= input$min_size2,]
      df$begin_date <- df$two_week_begin_date 
      df$nsize <- paste(df$begin_date,"N = ", sep="\n")
      df$nsize <- paste(df$nsize, df$freq)
      
      boxplots <- list()
      for(i in 1:length(unique(df$bin_midpoint))){
        boxplots[[i]] <- length(unique(df$two_week_period[df$bin_midpoint==unique(df$bin_midpoint)[i]]))
      }
      nboxplots<-max(unlist(boxplots))      
    }
    
    if(input$bin_frequency=="Weekly"){
      df.weeks <- list()
      bins <- sort(unique(df$bin_midpoint))
      for (i in min(df$week_period):max(df$week_period)) {
        d1 <- data.frame(bin_midpoint = bins)
        d <-
          df %>%
          ## Only this month
          filter(week_period == i)
        ## Group by bin
        d <- 
          d %>%
          group_by(bin_midpoint)
        d <- 
          
          d %>%
          ## Count within each bin
          summarize(freq = n())
        d1 <- data.frame(bin_midpoint = bins)
        d <- left_join(d1, d)
        d$freq[is.na(d$freq)] <- 0
        d$week_period <- i
        df.weeks[[i]] <- d
      }
      ## Make a single data frame
      df.weeks <- bind_rows(df.weeks)
      df <- left_join(df, df.weeks, by = c("week_period" = "week_period", "bin_midpoint" = "bin_midpoint"))
      
      df <- df[df$freq >= input$min_size2,]
      df$begin_date <- df$week_begin_date 
      df$nsize <- paste(df$begin_date,"N = ", sep="\n")
      df$nsize <- paste(df$nsize, df$freq)
      boxplots <- list()
      for(i in 1:length(unique(df$bin_midpoint))){
        boxplots[[i]] <- length(unique(df$week_period[df$bin_midpoint==unique(df$bin_midpoint)[i]]))
      }
      nboxplots<-max(unlist(boxplots))
      }
    nboxplots <- ifelse(nboxplots>18, nboxplots/18*session$clientData$output_myPlot_width, session$clientData$output_myPlot_width)
  })
  
  output$myPlot <- renderPlot({
    req(nboxplots)
    req(npanels)
    req(input$Building)
    req(input$Service)
    
    
    df <- df %>% filter(Name == input$Building,
                        Service == input$Service)
    
    if(input$bin_frequency=="Monthly"){
      req(input$startingMonth)
      req(input$endMonth)
      months <- unique(df$Month)
      startMonth <- months[months == match(input$startingMonth, month.name)]
      endMonth <- months[months == match(input$endMonth, month.name)]
      df <- df %>% filter(Month >= startMonth,
                          Month <= endMonth
      )
      df$rownumber <- 1:nrow(df)}
    
    else if(input$bin_frequency=="Biweekly"){
      req(input$startingTwoWeek)
      req(input$endTwoWeek)
      two_week_begin_dates <- unique(df$two_week_begin_date)
      two_week_end_dates <- unique(df$two_week_end_date)
      startTwoWeek <- two_week_begin_dates[two_week_begin_dates==input$startingTwoWeek]
      endTwoWeek <- two_week_end_dates[two_week_end_dates==input$endTwoWeek]
      df <- df %>% filter(two_week_begin_date >= startTwoWeek,
                          two_week_end_date <= endTwoWeek
      )
      df$rownumber <- 1:nrow(df)}
    
    else if(input$bin_frequency=="Weekly"){
      req(input$startingWeek)
      req(input$endWeek)
      week_begin_dates <- unique(df$week_begin_date)
      week_end_dates <- unique(df$week_end_date)
      startWeek <- week_begin_dates[week_begin_dates==input$startingWeek]
      endWeek <- week_end_dates[week_end_dates==input$endWeek]
      df <- df %>% filter(week_begin_date >= startWeek,
                          week_end_date <= endWeek
      )
      df$rownumber <- 1:nrow(df)}
    

    if(input$binmethod=='Temperature'){
      req(input$Temp)
      req(input$temp_binWidth)
      df <- df[df$Temperature>=min(input$Temp),]
      df <- df[df$Temperature<=max(input$Temp),]
      df$bin_midpoint <- ceiling(df$Temperature/input$temp_binWidth)*input$temp_binWidth-(input$temp_binWidth/2)}
    
    else if(input$binmethod=='Humidity'){
      req(input$Humid)
      req(input$humid_binWidth)
      df <- df[df$Humidity>=min(input$Humid),]
      df <- df[df$Humidity<=max(input$Humid),]
      df$bin_midpoint <- ceiling(df$Humidity/input$humid_binWidth)*input$humid_binWidth-(input$humid_binWidth/2)}
    
    else if(input$binmethod=='Heat Index'){
      req(input$heat_index)
      req(input$hi_binWidth)
      df <- df[df$heat_index>=min(input$heat_index),]
      df <- df[df$heat_index<=max(input$heat_index),]
      df$bin_midpoint <- ceiling(df$heat_index/input$hi_binWidth)*input$hi_binWidth-(input$hi_binWidth/2)}
    
    
    if(input$bin_frequency=="Monthly"){
    df.months <- list()
    bins <- sort(unique(df$bin_midpoint))
    for (i in startMonth:endMonth) {
      d1 <- data.frame(bin_midpoint = bins)
      d <-
        df %>%
        ## Only this month
        filter(Month == i)
      ## Group by bin
      d <- 
        d %>%
        group_by(bin_midpoint)
      d <- 
        
        d %>%
        ## Count within each bin
        summarize(freq = n())
      d1 <- data.frame(bin_midpoint = bins)
      d <- left_join(d1, d)
      d$freq[is.na(d$freq)] <- 0
      d$Month <- i
      df.months[[i]] <- d
    }
    ## Make a single data frame
    df.months <- bind_rows(df.months)
    df <- left_join(df, df.months, by = c("Month" = "Month", "bin_midpoint" = "bin_midpoint"))
    
    df <- df[df$freq >= input$min_size2,]
    month_dates <- data.frame(Month = startMonth:endMonth,
                              month_begin_date = ymd(sprintf("2017-%s-01", startMonth:endMonth)))
    class(month_dates$month_begin_date) <- class(df$Day)
    df<- merge(x=df, y=month_dates[, c("Month", "month_begin_date")],by.x="Month", by.y="Month")
    df$begin_date <- df$month_begin_date
    ## for(i in startMonth:endMonth){
    ##     month_dates$month_begin_date[[i-unique(df$Month)[1]+1]] <- min(df$Day[df$Month==i])
    ##     month_dates$month_end_date[[i-unique(df$Month)[1]+1]] <- max(df$Day[df$Month==i])
    ## }
    ## class(month_dates$month_end_date) <- class(df$Day)
    df$nsize <- paste(df$begin_date,"N = ", sep="\n")
    df$nsize <- paste(df$nsize, df$freq)
    }
    
    if(input$bin_frequency=="Biweekly"){
      df.two_weeks <- list()
      bins <- sort(unique(df$bin_midpoint))
      for (i in min(df$two_week_period) :max(df$two_week_period)) {
        d1 <- data.frame(bin_midpoint = bins)
        d <-
          df %>%
          ## Only this month
          filter(two_week_period == i)
        ## Group by bin
        d <- 
          d %>%
          group_by(bin_midpoint)
        d <- 
          
          d %>%
          ## Count within each bin
          summarize(freq = n())
        d1 <- data.frame(bin_midpoint = bins)
        d <- left_join(d1, d)
        d$freq[is.na(d$freq)] <- 0
        d$two_week_period <- i
        df.two_weeks[[i]] <- d
      }
      ## Make a single data frame
      df.two_weeks <- bind_rows(df.two_weeks)
      df <- left_join(df, df.two_weeks, by = c("two_week_period" = "two_week_period", "bin_midpoint" = "bin_midpoint"))
      
      df <- df[df$freq >= input$min_size2,]
      df$begin_date <- df$two_week_begin_date 
      df$nsize <- paste(df$begin_date,"N = ", sep="\n")
      df$nsize <- paste(df$nsize, df$freq)
    }
    
    if(input$bin_frequency=="Weekly"){
      df.weeks <- list()
      bins <- sort(unique(df$bin_midpoint))
      for (i in min(df$week_period):max(df$week_period)) {
        d1 <- data.frame(bin_midpoint = bins)
        d <-
          df %>%
          ## Only this month
          filter(week_period == i)
        ## Group by bin
        d <- 
          d %>%
          group_by(bin_midpoint)
        d <- 
          
          d %>%
          ## Count within each bin
          summarize(freq = n())
        d1 <- data.frame(bin_midpoint = bins)
        d <- left_join(d1, d)
        d$freq[is.na(d$freq)] <- 0
        d$week_period <- i
        df.weeks[[i]] <- d
      }
      ## Make a single data frame
      df.weeks <- bind_rows(df.weeks)
      df <- left_join(df, df.weeks, by = c("week_period" = "week_period", "bin_midpoint" = "bin_midpoint"))
      
      df <- df[df$freq >= input$min_size2,]
      df$begin_date <- df$week_begin_date 
      df$nsize <- paste(df$begin_date,"N = ", sep="\n")
      df$nsize <- paste(df$nsize, df$freq)
    }
    
    
    if(input$Service=="Chilled Water"){
      df$bin_midpoint <- factor(df$bin_midpoint, levels=rev(sort(unique(df$bin_midpoint))))
      if(input$binmethod=="Temperature"){
        direction <- "Higher Temperatures At Top"
      } else if(input$binmethod=="Humidity"){
        direction <- "Higher Humidities At Top"
      } else if(input$binmethod=="Heat Index"){
        direction <- "Higher Heat Indices At Top"
      }
    }
    
    if(input$Service=="Steam"){
    if(input$binmethod=="Temperature"){
      direction <- "Lower Temperatures At Top"
    } else if(input$binmethod=="Humidity"){
      direction <- "Lower Humidities At Top"
    } else if(input$binmethod=="Heat Index"){
      direction <- "Lower Heat Indices At Top"
    }
    }
    
    title <- paste(input$bin_frequency, "2017 Bin Models By", input$binmethod)
    title <- paste(title, input$Building,  input$Service, direction, sep="\n")
    
    if(input$freeY==FALSE){
      if(input$fitted==TRUE){
        df2 <- df[, c("ObsNorm", "nsize", "bin_midpoint", "Date", "Month"),]

        df2$y <- df2$ObsNorm
        df2$actual_fitted <- "Actual"
        df3 <- df[, c("fitted", "nsize", "bin_midpoint", "Date", "Month"),]
        df3$y <- df3$fitted
        df3$actual_fitted <- "Fitted"
        df4 <- bind_rows(df2[, c("y", "nsize", "actual_fitted", "bin_midpoint", "Date", "Month")], df3[, c("y", "nsize", "actual_fitted", "bin_midpoint", "Date", "Month")])
        
        ggplot(data=df4, aes(x=factor(nsize), y=y, fill=actual_fitted),  environment=environment()) +
          geom_boxplot() +
          facet_wrap(~bin_midpoint, scales="free", ncol = 1) +
          annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf) +
          annotate("segment", x=-Inf, xend=-Inf, y=-Inf, yend=Inf) +
          labs(y="ObsNorm (Every 15 min.)", x="First Day of Period \n Number of Observations in Each Bin by Date", title=title) + 
          theme(legend.position="top",
                legend.text=element_text(size=24),
                legend.title.align=0.5) + 
          scale_fill_discrete(guide = guide_legend(title = "", keywidth = 3, keyheight = 3))  + 
          theme(plot.title = element_text(size=36, hjust=0.5),
                axis.title.x = element_text(size=24),
                axis.title.y = element_text(size=24),
                strip.text = element_text(size=12))
        
        
      }
      else{
        ggplot(data=df, aes(x=factor(nsize), y=ObsNorm), environment=environment()) +
          geom_boxplot(varwidth=TRUE) +
          facet_wrap(~bin_midpoint, scales="free", ncol = 1) +
          annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf) +
          annotate("segment", x=-Inf, xend=-Inf, y=-Inf, yend=Inf) +
          labs(y="ObsNorm (Every 15 min.)", x="First Day of Period \n Number of Observations in Each Bin by Date", title=title)  + 
          theme(plot.title = element_text(size=36, hjust=0.5),
                axis.title.x = element_text(size=24),
                axis.title.y = element_text(size=24),
                strip.text = element_text(size=12))
      }
    }
    else{ 
      if(input$fitted==TRUE){
        df2 <- df[, c("ObsNorm", "nsize", "bin_midpoint", "Date", "Month"),]
        df2$y <- df2$ObsNorm
        df2$actual_fitted <- "Actual"
        df3 <- df[, c("fitted", "nsize", "bin_midpoint", "Date", "Month"),]
        df3$y <- df3$fitted
        df3$actual_fitted <- "Fitted"
        df4 <- bind_rows(df2[, c("y", "nsize", "actual_fitted", "bin_midpoint", "Date", "Month")], df3[, c("y", "nsize", "actual_fitted", "bin_midpoint", "Date", "Month")])
        
        ggplot(data=df4, aes(x=factor(nsize), y=y, fill=actual_fitted),  environment=environment()) +
          geom_boxplot() +
          facet_wrap(~bin_midpoint, scales="free_x", ncol = 1) +
          annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf) +
          annotate("segment", x=-Inf, xend=-Inf, y=-Inf, yend=Inf) +
          labs(y="ObsNorm (Every 15 min.)", x="First Day of Period \n Number of Observations in Each Bin by Date", title=title) + 
          theme(legend.position="top",
                legend.text=element_text(size=24),
                legend.title.align=0.5) + 
          scale_fill_discrete(guide = guide_legend(title = "", keywidth = 3, keyheight = 3))  + 
          theme(plot.title = element_text(size=36, hjust=0.5),
                axis.title.x = element_text(size=24),
                axis.title.y = element_text(size=24),
                strip.text = element_text(size=12))
        
      }
      else{
        ggplot(data=df, aes(x=factor(nsize), y=ObsNorm), environment=environment()) +
          geom_boxplot(varwidth=TRUE) +
          facet_wrap(~bin_midpoint, scales="free_x", ncol = 1) +
          annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf)+
          annotate("segment", x=-Inf, xend=-Inf, y=-Inf, yend=Inf)+
          labs(y="ObsNorm (Every 15 min.)", x="First Day of Period \n Number of Observations in Each Bin by Date", title="Boxplots by Temperature Binned Monthly") + 
          theme(plot.title = element_text(size=36, hjust=0.5),
                axis.title.x = element_text(size=24),
                axis.title.y = element_text(size=24),
                strip.text = element_text(size=12))
        
      }
    }
    
  }, height=npanels, width=nboxplots)
  

    }

shinyApp(ui, server)
