library(shiny)
library(shinydashboard)
library(shinythemes)
library(leaflet)
library(dplyr)
library(ggplot2)
library(TSA)
library(forecast)
library(plotly)
library(coronavirus)
library(rgdal)
library(plyr)

covidc <- read.csv('covidC.csv',header = T)


ui <- dashboardPage(skin = 'black',title = 'Coronavirus',
  dashboardHeader(title = p(icon('hospital'),'COVID 19 DASHBOARD'),titleWidth = 320,
                    dropdownMenu(type = 'notifications',headerText = 'Notification',badgeStatus = 'success',
                                 notificationItem(text = 'The app uses covid 19 historical data',status = 'warning',icon = icon('message'))
                        
                    )),
  dashboardSidebar(
    sidebarMenu(
      menuItem('Global Stats',tabName = 'gstats',icon = icon('globe',class = 'fa-spin'),badgeLabel = 'Big Picture',badgeColor = 'orange'),
      menuItem('Country Stats',tabName = 'cstats',icon = icon('map'),badgeColor = 'green',badgeLabel = 'Get Specific'),
      menuItem('TimeSeries',tabName = 'tm',icon = icon('line-chart'),badgeLabel = 'Forecast',badgeColor = 'red'),
      menuItem('About',tabName = 'About',icon = icon('user'),badgeLabel = 'App Details',badgeColor = 'blue')
    )),
  dashboardBody(
    tabItems(
      tabItem(tabName = 'gstats',
              box(width = 500,height = 900,collapsible = T,background = 'black',
                  fluidRow(
                    column(12,
                           box(width = 400,height = 100,background = 'black',
                               fluidRow(
                                 column(4,infoBoxOutput('vb',width = 12)),
                                 column(4,infoBoxOutput('vb2',width = 12)),
                                 column(4,infoBoxOutput('vb3',width = 12))
                               ),
                               fluidRow(
                                 column(4,box(width = 200,height = 420,status = 'warning',background = 'black',
                                              leafletOutput('map',height = 400))),
                                 column(4,box(width = 200,height = 420,background = 'black',status = 'warning',
                                              plotlyOutput('plt',height = 400))),
                                 column(4,
                                        fluidRow(
                                          column(12,box(width = 200,height = 200,background = 'black',status = 'warning',
                                                        plotlyOutput('line',height = 180)))
                                        ),
                                        fluidRow(
                                          column(12,box(width = 200,height = 200,background = 'black',status = 'warning',
                                                        plotlyOutput('line2',height = 180)))
                                        ))),
                               fluidRow(
                                 column(4,box(width = 200,height = 200,background = 'black',status = 'warning',
                                              plotlyOutput('plt2',height = 180))),
                                 column(4,box(width = 200,height = 200,background = 'black',status = 'warning',
                                              fluidRow(
                                                column(12,dateInput(inputId = 'date',label = 'DATE (Map,Pie,Bar,Infobox)',value = '2020-01-22',min = '2020-01-22',max = '2021-05-27',autoclose = F))
                                              ),
                                              fluidRow(
                                                column(12,selectInput('type','TYPE (Map,Pie)',choices = c('confirmed','recovered','death')))))),
                                 column(4,box(width = 200,height = 200,background = 'black',status = 'warning',
                                              plotlyOutput('line3',height = 180)))
                               ))
                  )))),
      tabItem(tabName = 'cstats',
              box(width = 500,height = 950,background = 'black',collapsible = T,
                  fluidRow(
                    column(12,
                           box(width = 400,height = 800,background = 'black',
                  tabsetPanel(type = 'pills',
                              tabPanel('Dashboard',
                                       tags$br(),
                                       fluidRow(
                                         column(4,infoBoxOutput('inf',width = 12)),
                                         column(4,infoBoxOutput('inf2',width = 12)),
                                         column(4,infoBoxOutput('inf3',width = 12))
                                       ),
                                       fluidRow(
                                         column(8,
                                                box(width = 400,height = 420,background = 'black',status = 'warning',
                                                    leafletOutput('map2',height = 400))),
                                         column(4,
                                                fluidRow(
                                                  column(12,
                                                         box(width = 200,height = 200,background = 'black',status = 'warning',
                                                             plotlyOutput('plt10',height = 180)))
                                                ),
                                                fluidRow(
                                                  column(12,
                                                         box(width = 200,height = 200,background = 'black',status = 'warning',
                                                             plotlyOutput('plt11',height = 180)))
                                                ))
                                       ),
                                       fluidRow(
                                         column(4,
                                                box(width = 200,height = 230,background = 'black',status = 'warning',
                                                    plotlyOutput('plt13',height = 190))),
                                         column(4,
                                                box(width = 200,height = 230,background = 'black',status = 'warning',
                                                    fluidRow(
                                                      column(12,
                                                             fluidRow(column(12,selectInput('country','Country',choices = unique(coronavirus$country)))),
                                                             fluidRow(column(12,dateInput('date2','Date',value = '2020-01-22',min = '2020-01-22',max = '2021-05-27',autoclose = F))),
                                                             fluidRow(column(12,selectInput('slt2','Type',choices = c('confirmed','recovered','death')))))))),
                                         column(4,
                                                box(width = 200,height = 230,background = 'black',status = 'warning',
                                                    plotlyOutput('plt12',height = 190)))
                                       )),
                              tabPanel('Table',
                                       tags$br(),
                                       fluidRow(
                                         column(12,
                                                  box(width = 480,height = 600,status = 'warning',background = 'aqua',
                                                    DT::dataTableOutput('dt11')))
                                       )))))))),
      tabItem(tabName = 'tm',
              box(width = 500,height = 900,background = 'black',collapsible = T,
                  fluidRow(
                    column(12,
                           box(width = 400,height = 800,background = 'black',
                               fluidRow(
                                 column(8,
                                        box(width = 400,height = 420,status = 'warning',background = 'black',
                                            plotlyOutput('plt90',height = 400))),
                                 column(4,
                                        fluidRow(
                                          column(12,
                                                 box(width = 200,height = 200,status = 'warning',background = 'black',
                                                     plotlyOutput('plt30',reportTheme = T,height = 180)))
                                        ),
                                        fluidRow(
                                          column(12,
                                                 box(width = 200,height = 200,status = 'warning',background = 'black',
                                                     plotlyOutput('plt31',height = 180)))
                                        ))
                               ),
                               fluidRow(
                                 column(5,
                                        box(width = 250,height = 230,status = 'warning',background = 'black',
                                            plotOutput('plt0',height = 180))),
                                 column(3,
                                        box(width = 150,height = 230,status = 'warning',background = 'black',
                                            fluidRow(
                                              column(12,selectInput('slt50','Country',choices = covidc$countries))
                                            ),
                                            fluidRow(
                                              column(12,selectInput('slt51','Type',choices = c('confirmed','recovered','death')))
                                            ),
                                            fluidRow(
                                              column(12,numericInput('num','Points To Predict',value = 10,min = 1,max = 2000))
                                            ))),
                                 column(4,
                                        box(width = 200,height = 230,background = 'black',status = 'warning',
                                            plotlyOutput('plt32',height = 180)))
                               )))
                  ))),
      tabItem(tabName = 'About',
              box(width = 500,height = 900,background = 'black',collapsible = T,
                  tags$br(),
                  tags$br(),
                  tags$br(),
                  tags$br(),
                  tags$br(),
                  tags$br(),
                  tags$br(),
                  fluidRow(
                    column(4,offset = 4,
                           tags$audio(src = 'first.mp3',type = 'audio/mp3',controls = 'controls'))
                  )))
    ))
)

server <- shinyServer(function(input,output){
  
  ttss <- reactive({
    
    if (input$slt50 == 'All'){
      
      df <- filter(coronavirus,type == input$slt51)
      attach(df)
      df2 <- tapply(X = cases,INDEX = date,FUN = sum)
      detach(df)
      dft <- as.data.frame(df2)
      
      colnames(dft) <- c('cases')
      
      tss <- ts(data = dft$cases,start = c(2020,01,22),frequency = 12)
      tss
      
    }else{
      
      df <- filter(coronavirus,country == input$slt50 & type == input$slt51)
      tss <- ts(data = df$cases,start = c(2020,01,22),frequency = 12)
      tss
      
    }
    
    tss
    
  })
  
  decom <- reactive({
    
    decom <- decompose(ttss())
    decom
    
  })
  
  output$plt30 <- renderPlotly({
    
    
    tr <- decom()$trend
    tim <- time(decom()$trend)
    
    df <- as.data.frame(cbind(tim,tr))
    colnames(df) <- c('date','value')
    dff <- df
    
    ggplotly(ggplot(dff,aes(x = date,y = value)) + geom_line(color = 'red') + 
               labs(title = paste('Trend 2020-2021'))+ theme(plot.background = element_rect(fill = 'black')) + 
               theme(panel.background = element_rect(fill = 'black')) +
               theme(axis.text.y = element_text(color = 'white')) + 
               theme(axis.text.x = element_text(color = 'black')) + 
               theme(plot.title = element_text(hjust = 0.5,color = 'red',face = 'bold',family = 'times new roman')))
    
  })
  
  output$plt31 <- renderPlotly({
    
    
    tr <- decom()$seasonal
    tim <- time(decom()$seasonal)
    
    df <- as.data.frame(cbind(tim,tr))
    colnames(df) <- c('date','value')
    dff <- df
    
    ggplotly(ggplot(dff,aes(x = date,y = value)) + geom_line(color = 'red') + 
               labs(title = paste('Seaonality 2020-2021'))+ theme(plot.background = element_rect(fill = 'black')) + 
               theme(panel.background = element_rect(fill = 'black')) +
               theme(axis.text.y = element_text(color = 'white')) + 
               theme(axis.text.x = element_text(color = 'black')) + 
               theme(plot.title = element_text(hjust = 0.5,color = 'red',face = 'bold',family = 'times new roman')))
    
  })
  
  output$plt32 <- renderPlotly({
    
    
    tr <- decom()$random
    tim <- time(decom()$random)
    
    df <- as.data.frame(cbind(tim,tr))
    colnames(df) <- c('date','value')
    dff <- df
    
    ggplotly(ggplot(dff,aes(x = date,y = value)) + geom_line(color = 'red') + 
               labs(title = paste('Residual 2020-2021'))+ theme(plot.background = element_rect(fill = 'black')) + 
               theme(panel.background = element_rect(fill = 'black')) +
               theme(axis.text.y = element_text(color = 'white')) + 
               theme(axis.text.x = element_text(color = 'black')) + 
               theme(plot.title = element_text(hjust = 0.5,color = 'red',face = 'bold',family = 'times new roman')))
    
  })
  
  output$plt90 <- renderPlotly({
    
    dfu <- as.data.frame(cbind(time(ttss()),ttss()))
    colnames(dfu) <- c('date','cases')
    
    ggplotly(ggplot(dfu,aes(x = date,y = cases)) + geom_line(col = 'red') + labs(title = 'Observed 2020-2021') + 
               theme(plot.background = element_rect(fill = 'black')) +  
               theme(panel.background = element_rect(fill = 'black')) + 
               theme(axis.text.y = element_text(color = 'black',angle = 90)) + 
               theme(axis.text.x = element_text(color = 'black')) + 
               theme(plot.title = element_text(hjust = 0.5,color = 'red',face = 'bold',family = 'times new roman')))
    
  })
  
  output$plt0 <- renderPlot({
    
    if (is.null(input$num) == T ){
      
      return()
      
    }else{
      
      model <- tbats(ttss())
      fo = forecast(model,h = input$num)
      plot(fo,col = 'red')
       
    }
    
  })
  
  output$dt11 <- DT::renderDataTable(filter(coronavirus,country == input$country),extensions = 'Buttons',options = list(dom = 'Bfrtip',buttons = list('pdf','excel','csv')))
  
  output$map2 <- renderLeaflet({
    
    dtfr <- filter(coronavirus,country == input$country & date == input$date2 & type == input$slt2)
    
    mp <- leaflet(data = dtfr) %>% 
      addTiles() %>%
      setView(lng = unique(dtfr$long),lat = unique(dtfr$lat),zoom = 4) %>%
      addCircleMarkers(lng = unique(dtfr$long),lat = unique(dtfr$lat),weight = 2,fillOpacity = 1,opacity = 0.2,label = paste(unique(dtfr$country),unique(dtfr$cases)))
    
    mp
    
  })
  
  
  output$inf3 <- renderInfoBox({
    
    corona2 <- filter(coronavirus,country == input$country & date == input$date2 & type == 'death')
    
    infoBox(title = corona2$type,value = corona2$cases,icon = icon('cross'),color = 'teal',width = 12,fill = T)
    
  })
  
  output$inf2 <- renderInfoBox({
    
    corona2 <- filter(coronavirus,country == input$country & date == input$date2 & type == 'recovered')
    
    infoBox(title = corona2$type,value = corona2$cases,icon = icon('tree'),color = 'olive',width = 12,fill = T)
    
  })
  
  output$inf <- renderInfoBox({
    
    corona2 <- filter(coronavirus,country == input$country & date == input$date2 & type == 'confirmed')
    
    infoBox(title = corona2$type,value = corona2$cases,icon = icon('hospital'),color = 'aqua',width = 12,fill = T)
    
  })
  
  output$plt13 <- renderPlotly({
    
    corona <- filter(coronavirus,country == input$country & date == input$date2)
    
    ggplotly(ggplot(corona,aes(x = type,y = cases,fill = type)) + geom_col() +
               labs(title = paste('Stats ',input$date2))+ theme(plot.background = element_rect(fill = 'black')) + 
               theme(panel.background = element_rect(fill = 'black')) +
               theme(axis.text.y = element_text(color = 'white')) + 
               theme(axis.text.x = element_text(color = 'black')) + 
               theme(plot.title = element_text(hjust = 0.5,color = 'red',face = 'bold',family = 'times new roman')))
    
  })
  
  output$plt12 <- renderPlotly({
    
    df <- filter(coronavirus,country == input$country & type == 'death')
    
    tss <- ts(data = df$cases,start = c(2020,01,22),end = c(2021,05,27),frequency = 496)
    
    dfu <- as.data.frame(cbind(time(tss),tss))
    colnames(dfu) <- c('date','cases')
    
    ggplotly(ggplot(dfu,aes(x = date,y = cases)) + geom_line(col = 'red') + labs(title = 'Death 2020-2021') + 
               theme(plot.background = element_rect(fill = 'black')) +  
               theme(panel.background = element_rect(fill = 'black')) + 
               theme(axis.text.y = element_text(color = 'black',angle = 90)) + 
               theme(axis.text.x = element_text(color = 'black')) + 
               theme(plot.title = element_text(hjust = 0.5,color = 'red',face = 'bold',family = 'times new roman')))
    
  })
  
  output$plt11 <- renderPlotly({
    
    df <- filter(coronavirus,country == input$country & type == 'recovered')
    
    tss <- ts(data = df$cases,start = c(2020,01,22),end = c(2021,05,27),frequency = 496)
    
    dfu <- as.data.frame(cbind(time(tss),tss))
    colnames(dfu) <- c('date','cases')
    
    ggplotly(ggplot(dfu,aes(x = date,y = cases)) + geom_line(col = 'red') + labs(title = 'Recovered 2020-2021') + 
               theme(plot.background = element_rect(fill = 'black')) +  
               theme(panel.background = element_rect(fill = 'black')) + 
               theme(axis.text.y = element_text(color = 'black',angle = 90)) + 
               theme(axis.text.x = element_text(color = 'black')) + 
               theme(plot.title = element_text(hjust = 0.5,color = 'red',face = 'bold',family = 'times new roman')))
    
  })
  
  output$plt10 <- renderPlotly({
    
    df <- filter(coronavirus,country == input$country & type == 'confirmed')

    tss <- ts(data = df$cases,start = c(2020,01,22),end = c(2021,05,27),frequency = 496)
    
    dfu <- as.data.frame(cbind(time(tss),tss))
    colnames(dfu) <- c('date','cases')
    
    ggplotly(ggplot(dfu,aes(x = date,y = cases)) + geom_line(col = 'red') + labs(title = 'Confirmed 2020-2021') + 
               theme(plot.background = element_rect(fill = 'black')) +  
               theme(panel.background = element_rect(fill = 'black')) + 
               theme(axis.text.y = element_text(color = 'black',angle = 90)) + 
               theme(axis.text.x = element_text(color = 'black')) + 
               theme(plot.title = element_text(hjust = 0.5,color = 'red',face = 'bold',family = 'times new roman')))
    
  })

  
  datafr <- reactive({
    
    corona <- filter(coronavirus,date == input$date)
    attach(corona)
    ad = as.data.frame(tapply(X = cases,INDEX = type,FUN = sum))
    ad[,2] <- row.names(ad)
    colnames(ad) <- c('Cases','Type')
    
    ad
    
  })
  
  output$vb3 <- renderInfoBox({
    
    totalC <- filter(datafr(),Type == 'death')
    
    infoBox(title = totalC$Type,value = totalC$Cases,icon = icon('cross'),color = 'teal',fill = T,width = 12)
    
  })
  
  output$vb2 <- renderInfoBox({
    
    totalC <- filter(datafr(),Type == 'recovered')
    
    infoBox(title = totalC$Type,value = totalC$Cases,icon = icon('tree'),color = 'olive',fill = T,width = 12)
    
  })
  
  output$vb <- renderInfoBox({
    
    totalC <- filter(datafr(),Type == 'confirmed')
    
    infoBox(title = totalC$Type,value = totalC$Cases,icon = icon('hospital'),color = 'aqua',fill = T,width = 12)
    
  })
  
  output$line3 <- renderPlotly({
    
    df <- filter(coronavirus,type == 'death')
    attach(df)
    df2 <- tapply(X = cases,INDEX = date,FUN = sum)
    detach(df)
    dft <- as.data.frame(df2)
    
    colnames(dft) <- c('cases')
    
    tss <- ts(data = dft$cases,start = c(2020,01,22),end = c(2021,05,27),frequency = 496)
    
    dfu <- as.data.frame(cbind(time(tss),tss))
    colnames(dfu) <- c('date','cases')
    
    ggplotly(ggplot(dfu,aes(x = date,y = cases)) + geom_line(col = 'red') + labs(title = 'Deaths 2020-2021') + 
               theme(plot.background = element_rect(fill = 'black')) +  
               theme(panel.background = element_rect(fill = 'black')) + 
               theme(axis.text.y = element_text(color = 'black',angle = 90)) + 
               theme(axis.text.x = element_text(color = 'black')) + 
               theme(plot.title = element_text(hjust = 0.5,color = 'red',face = 'bold',family = 'times new roman')))
    
  })
  
  output$line2 <- renderPlotly({
    
    df <- filter(coronavirus,type == 'recovered')
    attach(df)
    df2 <- tapply(X = cases,INDEX = date,FUN = sum)
    detach(df)
    dft <- as.data.frame(df2)
    
    colnames(dft) <- c('cases')
    
    tss <- ts(data = dft$cases,start = c(2020,01,22),end = c(2021,05,27),frequency = 496)
    
    dfu <- as.data.frame(cbind(time(tss),tss))
    colnames(dfu) <- c('date','cases')
    
    ggplotly(ggplot(dfu,aes(x = date,y = cases)) + geom_line(col = 'red') +
               labs(title = 'Recovered 2020-2021') + 
               theme(plot.background = element_rect(fill = 'black')) +  
               theme(panel.background = element_rect(fill = 'black')) + 
               theme(axis.text.y = element_text(color = 'black',angle = 90)) + 
               theme(axis.text.x = element_text(color = 'black')) + 
               theme(plot.title = element_text(hjust = 0.5,color = 'red',face = 'bold',family = 'times new roman')))
    
  })
  
 output$line <- renderPlotly({
    
    df <- filter(coronavirus,type == 'confirmed')
    attach(df)
    df2 <- tapply(X = cases,INDEX = date,FUN = sum)
    detach(df)
    dft <- as.data.frame(df2)
    
    colnames(dft) <- c('cases')
    
    tss <- ts(data = dft$cases,start = c(2020,01,22),end = c(2021,05,27),frequency = 496)
    
    dfu <- as.data.frame(cbind(time(tss),tss))
    colnames(dfu) <- c('date','cases')
    
    ggplotly(ggplot(dfu,aes(x = date,y = cases)) + geom_line(col = 'red')+ 
               labs(title = 'Confirmed 2020-2021') + 
               theme(plot.background = element_rect(fill = 'black')) +  
               theme(panel.background = element_rect(fill = 'black')) + 
               theme(axis.text.y = element_text(color = 'black',angle = 90)) + 
               theme(axis.text.x = element_text(color = 'black')) +
               theme(plot.title = element_text(hjust = 0.5,color = 'red',face = 'bold',family = 'times new roman')))
    
    
    
  })
  
  output$plt2 <- renderPlotly({
    
    df <- filter(coronavirus,date == input$date & type == input$type)
    arrv <- tail(arrange(df,cases)$cases,5)
    arrl <- tail(arrange(df,cases)$country,5)
    plot_ly(type = 'pie',labels = arrl,values = arrv)  
     
  })
  
  output$map <- renderLeaflet({
    
    shp <- readOGR(dsn = getwd(),layer = 'TM_WORLD_BORDERS_SIMPL-0.3')
    
    cv <- filter(coronavirus,date == input$date & type == input$type)
    
    cv$country <- revalue(x = cv$country,c(US = 'United States'))
    
    dy <- as.data.frame(shp)
    cv <- filter(cv,country %in% dy$NAME)
    
    shp2 <- subset(shp,is.element(shp$NAME,cv$country))
    
    cv <- cv[order(match(cv$country,shp2$NAME)),]
    
    bins = c(0,500,1000,5000,10000,50000,100000,500000,1000000,1500000)
    
    pal <- colorBin(palette = c('yellow','orange','gold','green','purple','maroon','black','blue','red'),domain = cv$cases,bins = bins)
    
    nn <- leaflet(data = shp2) %>% addTiles(group = 'basic') %>%
      setView(lat= 17.86,lng = 34.493,zoom = 4) %>%
      addPolygons(group = 'Chorepleth',color = 'black',fillOpacity = 1,weight = 2,opacity = 0.2,fillColor = pal(cv$cases),highlightOptions = highlightOptions(fillColor = 'black'),label = paste(shp2$NAME)) %>%
      addCircleMarkers(lng = cv$long,lat = cv$lat,group = 'Circles',weight = 2,fillOpacity = 1,opacity = 0.2,clusterOptions = T,label = paste(cv$country,cv$cases),fillColor  = pal(cv$cases)) %>%
      addLayersControl(baseGroups = c('Chorepleth','Circles')) %>%
      addLegend(position = 'bottomleft',pal = pal,values = cv$cases)
    
    nn
    
    
    
    
  })
  
  output$plt <- renderPlotly({
    
    corona <- filter(coronavirus,date == input$date)
    attach(corona)
    ad = as.data.frame(tapply(X = cases,INDEX = type,FUN = sum))
    ad[,2] <- row.names(ad)
    colnames(ad) <- c('Cases','Type')
    
    
    ggplotly(ggplot(ad,aes(x = Type,y = Cases,fill = Type)) + geom_col() +
              labs(title = paste('Stats ',input$date))+ theme(plot.background = element_rect(fill = 'black')) + 
               theme(panel.background = element_rect(fill = 'black')) +
               theme(axis.text.y = element_text(color = 'white')) + 
               theme(axis.text.x = element_text(color = 'black')) + 
               theme(plot.title = element_text(hjust = 0.5,color = 'red',face = 'bold',family = 'times new roman')))
    
  })
  
  
})

shinyApp(ui,server)