#library
library(shiny)
library(tidyverse)
library(ggplot2)
library(leaflet)
library(lubridate)
library(RColorBrewer)
library(leaflet)
library(shinythemes)
library(sf)
library(readr)
library(plotly)
library(shinyWidgets)
library(DT)


#read in data

crash <- read.csv("data/road_crash_data.csv")
cam <- read.csv("data/vic_cam1.csv")
crashmap <- read.csv("data/crash_data.csv")
road_node <- read.csv("data/road_node.csv")
acc_locat <- read.csv("data/ACCIDENT_LOCATION.csv")

#filter speed zone
crash <- crash %>% filter(SPEED_ZONE <=200)

# transform road_node
road_crash <- road_node %>%
  group_by(POSTCODE_NO, Lat, Long)%>%
  summarise(count = n())

# convert chr to date 
crashmap <- crashmap%>%
  mutate(date = as.Date(ACCIDENTDATE),
         year = year(date))


# timwseries plot
p1 <- crash %>%
  mutate(year = year(ACCIDENTDATE))%>%
  filter(ACCIDENTDATE <="2019-12-31")%>%
  group_by(year,Accident.Type.Desc)%>%
  summarise(count = n())%>%
  ggplot(aes(x=year, y=count, color =Accident.Type.Desc )) +
  geom_line(alpha=0.5) 

# color pallet

Acci_col <- c(
  "Collision with a fixed object" = "#F8C471", 
  "Collision with vehicle" = "#f9e79f",
  "Fall from or in moving vehicle" = "#abebc6", 
  "No collision and no object struck" = "#76D7C4",
  "Other accident" = "#aed6f1",
  "Struck Pedestrian" = "#85C1E9", 
  "Struck animal" = "#d2b4de",
  "Vehicle overturned (no collision)" = "#F1948A",
  "collision with some other object" = "#f5b7b1")



# filter all the rows where ROAD_NAME_INT contains "EASTLINK"
allsys_acc <- acc_locat%>%
  filter(grepl("CITYLINK", ROAD_NAME) |
           grepl("GEELONG ROAD", ROAD_NAME) |
           ROAD_NAME == 'MONASH'& ROAD_TYPE == "FREEWAY"|
           grepl("WESTERN RING", ROAD_NAME)|
         grepl("EASTLINK", ROAD_NAME))%>%
  mutate(cam_sys = case_when(grepl("CITYLINK", ROAD_NAME) ~ 'CITYLINK',
                             grepl("GEELONG ROAD", ROAD_NAME) ~ 'GEELONG ROAD', 
                             grepl("MONASH", ROAD_NAME) ~ 'MONASH FREEWAY',
                             grepl("WESTERN RING", ROAD_NAME) ~ 'WESTERN RING',
                             grepl("EASTLINK", ROAD_NAME) ~ 'EASTLINK'
  ))

#join with crash data
allsys_crash <- merge(allsys_acc, crashmap, by ="ACCIDENT_NO")




#ui section
ui <- navbarPage("Crush and Cameras in Vic", id = "dvp",
                 theme = shinytheme("sandstone"),
                 #first home page
                 tabPanel("Home",# tag name
                          titlePanel(h1(tags$b("Visulisation of Crash Data And Road Safe Cameras in Victoria, Australia"),align="center")), # header name
                          fluidRow(tags$br()), #blank line
                          fluidRow(h3("About: ")),
                          fluidRow(p("According to the Australia bureau of statistics", 
                                     tags$span(
                                       class = "inline-link",
                                       tags$a(href = "https://www.abs.gov.au/statistics/industry/tourism-and-transport/motor-vehicle-census-australia/latest-release", "(ABS)")
                                     ), 
                                     "There are 5,157,172 registered vehicles in Victoria in the year 2021. 
                                   The",
                                     tags$span(
                                       class = "inline-link",
                                       tags$a(href = "https://www.abs.gov.au/statistics/industry/tourism-and-transport/transport-census/latest-release", "Australian census data")
                                     ),
                                     "shows, 91.3% of households have at least one vehicle, and 52.7% (6,347,498 people) of the Australian workforce drive to work by car. 
                                   Safety has always been a big issue for travelers as more than 200 lives are lost, and an average of 13,000 people are injured from road accidence every year in Victoria.")),
                          fluidRow(tags$br()),
                          fluidRow(tags$br()),
                          fluidRow(h3("Motivation and Aim:")),
                          fluidRow(p("There is a growing presence of road safety cameras along the route 
                                     as the number of road accidents continues to rise. 
                                     The news often highlights traffic problems specifically on the Monash freeway. 
                                     Statistics indicate a 15% rise in fatalities caused by road accidents in Victoria in 2022 
                                     compared to the previous year.")),
                          fluidRow(tags$br()),
                          fluidRow(p("The purpose of this website is to encourage individuals 
                                     to identify the common risk factors contributing to accidents and understand 
                                     how road safety cameras have contributed to reducing the likelihood of such incidents.")),
                          
                          fluidRow(column(5," "),column(width = 4,  # Adjust the column width as needed
                                                        offset = 4, # Adjust the offset to center the column
                                                        align = "center", actionButton("bottom1", "BEGIN"))),
                          fluidRow(tags$br()),
                          fluidRow(tags$br()),
                          fluidRow(tags$br()),

                          hr(),
                          print("Data Source:	Crash Stats, Metadata, 2020, Vicroad"),
                          fluidRow(tags$br()),
                          print("Data Source:	Find trend, 2022, Camera Save Lives")
                          ),
                 
                 #Second page
                 tabPanel("Crash Type",# tag name
                          titlePanel(h1("Common Accidence type and road condition factor")), # header name
                          fluidRow(" The clickbox on the left hand side show a list of road condition factor, select on the condition you are interested and see the change in the bubble plots and the other graphs."),
                          fluidRow(p(" hint: The size of the bubble is larger when total number of accidents recorded from the selected variable on the given year is high.")),
                          fluidRow(p(" what it tells you when move your mouse on the bubbles and donuts chart?")),
                          fluidRow(tags$br()),
                          fluidRow(sidebarLayout(
                            sidebarPanel(tags$br(),
                              pickerInput("factor1_input",
                                                     "Road Condition", 
                                                     choices=c(unique(crash$Surface.Cond.Desc)), 
                                                     options = list(`actions-box` = TRUE), 
                                                     multiple = T, 
                                                     selected = "Dry"),
                                         

                                         pickerInput("factor2_input",
                                                     "Speed Zone", 
                                                     choices=c(unique(crash$SPEED_ZONE)), 
                                                     options = list(`actions-box` = TRUE), 
                                                     multiple = T, 
                                                     selected = "60"),
                                         

                                         pickerInput("factor3_input",
                                                     "Light Condition", 
                                                     choices=c(unique(crash$Light.Condition.Desc)), 
                                                     options = list(`actions-box` = TRUE), 
                                                     multiple = T, 
                                                     selected = "Day")
                                         
                                        
                            ),
                            
                            mainPanel(plotlyOutput("lineplot")
                              
                            ))),
                          fluidRow(p(" Collsion with vehicle seems like the most common accidenct type, and collison with a fixed object is also common accidents.")),
                          fluidRow(p(" Generally, this happened the must on the road that does not have intersection.")),
                          fluidRow(p(" But how does the graph tell you from your selection?")),
                          fluidRow(tags$br()),
                          fluidRow(tags$br()),
                          fluidRow(column(6,plotlyOutput("donut")),column(6,plotOutput("circbar"))),
                          fluidRow(tags$br()),
                          #fluidRow(
                            #DT::dataTableOutput('x1')
                            #verbatimTextOutput("clickedTable")),

                          fluidRow(column(5," "),column(width = 6,align = "right",
                                                        actionButton("bottom5", "Previous Page"),
                                                        actionButton("bottom2", "Next Page")))),
                 
                 tabPanel("Speed Camera Map",# tag name
                          titlePanel(h1(" Locatoin of camera and crashes in Victoria ")), # header name
                          tabsetPanel(
                            tabPanel("Cameras in Victoria", 
                                               fluidRow(" Select the suburb by postcode in the box below."),
                                               fluidRow(" Full Address can be seem when click on the dots"),
                                               fluidRow(tags$br()),
                                               fluidRow(column(3, 
                                          pickerInput("postcode_input",
                                                      "Search Postcode", 
                                                      choices=c(unique(cam$Postcode)), 
                                                      options = list(`actions-box` = TRUE), 
                                                      multiple = T, 
                                                      selected = 3000)),
                            leafletOutput("cammap")),
                            
                            fluidRow(column(5," "),column(width = 6,align = "right",
                                                          actionButton("bottom6", "Previous Page"),
                                                          actionButton("bottom3", "Next Page")))),
                            
                          tabPanel("Crash data in Victoria",
                                   fluidRow(" Select the time range you are intereted."),
                                   fluidRow(" hint: More detail can be seem when click on the dots"),
                                   fluidRow(" hint: the green playbottom can show you animation of the change in crash data by day."),
                                   fluidRow(column(3, 
                                                   sliderInput('range',"Year:",
                                                               min = min(crashmap$date), 
                                                               max = max(crashmap$date), 
                                                               step = 1, 
                                                               value = range(crashmap$date),
                                                               animate = animationOptions(interval = 500, loop = TRUE))),
                          leafletOutput("crashmap1")),
                 
                          fluidRow(column(5," "),column(width = 6,align = "right",
                                                        actionButton("bottom6", "Previous Page"),
                                                        actionButton("bottom3", "Next Page")))))),
      
                
                 tabPanel("Highway Crashes",# tag name
                          titlePanel(h1("Highway Accident and Camera System")), # header name
                          fluidRow(tags$br()),
                          fluidRow(tags$br()),
                          fluidRow(" The lefthand side plot shows the number of road accident with people who had minor injured. Where as the right scatterplot show number of road accident with people who had Serious injury which requires hospitality. The line in side these plot is a Loess smooth line that shows the trend of injury number on road. "),
                          fluidRow(tags$br()),
                          fluidRow(" When speed camera was inplemented, people starts to manage and reduce their speed while traveling. Thus the number of serious injury has reduce as speed are more managabel. And the trend of serious injury accident has a relatively decreased over the time."),
                          fluidRow(tags$br()),
                          fluidRow(column(3, 
                                          checkboxGroupInput("checkGroup", 
                                                             ("Highway Camera System"), 
                                                             choices = c(unique(allsys_crash$cam_sys)),
                                                             selected = "CITYLINK"))),
                          fluidRow(plotOutput("sysplot")),
                          fluidRow(column(5," "),column(width = 6,align = "right", actionButton("bottom4", "Previous Page"))))
                 )


#server

server <- function(input, output, session){ 
  
  observeEvent( input$bottom1,{
    updateTabsetPanel(session,"dvp", selected = "Crash Type")
  })
  
  observeEvent( input$bottom2,{
    updateTabsetPanel(session,"dvp", selected = "Speed Camera Map")
  })
  
  observeEvent( input$bottom3,{
    updateTabsetPanel(session,"dvp", selected = "Highway Crashes")
  })
  
  observeEvent( input$bottom4,{
    updateTabsetPanel(session,"dvp", selected = "Speed Camera Map")
  })

  observeEvent( input$bottom5,{
    updateTabsetPanel(session,"dvp", selected = "Home")
  })
  
  observeEvent( input$bottom6,{
    updateTabsetPanel(session,"dvp", selected = "Crash Type")
  })
  
  observe({
    print(input$factor1_input)
  })
  
  observe({
    print(input$factor2_input)
  })
  observe({
    print(input$factor3_input)
  })
  
  observe({
    print(input$postcode_input)
  })
  
  output$lineplot <- renderPlotly({
    plot_ly(crash %>%
              mutate(year = year(ACCIDENTDATE)) %>%
              filter(ACCIDENTDATE <= "2019-12-31") %>%
              filter(Surface.Cond.Desc == input$factor1_input,
                     SPEED_ZONE == input$factor2_input,
                     Light.Condition.Desc == input$factor3_input) %>%
              group_by(year, Surface.Cond.Desc) %>%
              summarise(count = n()), 
            x = ~year, y = ~count, type = 'scatter', mode = 'markers',
            color = ~count, size = ~count,
            marker = list(opacity = 0.5, sizemode = 'diameter'),
            hoverinfo = 'text',
            text = ~paste('Year:', year, '<br>Surface condition:', Surface.Cond.Desc, '<br>Number of accidents:', count),
            source = "lineplot") %>%
      layout(
        xaxis = list(title = "Year"),
        yaxis = list(title = "Count"),
        title = "Bubble Plot by Year"
      ) %>%
      event_register("plotly_click")
  })

  observeEvent(event_data("plotly_click", source = "lineplot"), {
    clicked <- event_data("plotly_click", source = "lineplot")
    print(clicked)
    
  ##############################
   #output$table <- renderDataTable(crash$clicked)
  })

  #output$x1 = DT::renderDataTable(crash,server = FALSE)
  
   output$donut <- renderPlotly(
     crash%>% 
       mutate(year = year(ACCIDENTDATE))%>%
       filter(ACCIDENTDATE <="2019-12-31")%>%
       filter(Surface.Cond.Desc == input$factor1_input,
              SPEED_ZONE == input$factor2_input,
              Light.Condition.Desc == input$factor3_input) %>%
       group_by(Accident.Type.Desc)%>%
       summarise(Accident_count = n())%>%
       plot_ly(labels = ~Accident.Type.Desc, values = ~Accident_count, marker = list(colors = Acci_col))%>% 
       add_pie(hole = 0.6)%>% 
       layout(title = "Donut charts by Accident type",  showlegend = T, 
              xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
              yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
  )
   
   output$circbar <- renderPlot({
       # Filter data and perform necessary calculations
       road_des <- crash %>%
         mutate(year = year(ACCIDENTDATE)) %>%
         filter(ACCIDENTDATE <= "2019-12-31") %>%
         filter(Surface.Cond.Desc == input$factor1_input,
                SPEED_ZONE == input$factor2_input,
                Light.Condition.Desc == input$factor3_input) %>%
         group_by(Road.Geometry.Desc, Accident.Type.Desc) %>%
         summarise(Accident_count = n()) %>%
         mutate(log_count = log(as.numeric(Accident_count))) %>%
         filter(
           Road.Geometry.Desc != "Road closure",
           Road.Geometry.Desc != "Private property",
           Road.Geometry.Desc != "Dead end",
           Road.Geometry.Desc != "Unknown",
           Accident_count >= 0
         )
       
       # Set the number of 'empty bar' to add at the end of each group
       empty_bar <- 1
       
       # Split the data by group, add NA rows, and combine back together
       data_split <- split(road_des, road_des$Road.Geometry.Desc)
       
       data_new <- lapply(data_split, function(x) {
         n <- nrow(x)
         to_add <- data.frame(individual = rep(NA, empty_bar), group = x$Road.Geometry.Desc[1], value = rep(NA, empty_bar))
         rbind(x, to_add)
       })
       
       road_des <- do.call(rbind, data_new)
       
       # Add an ID column
       road_des$id <- seq(1, nrow(road_des))
       
       # Get the name and the y position of each label
       label_data <- road_des
       number_of_bar <- nrow(label_data)
       angle <- 90 - 360 * (label_data$id - 0.5) / number_of_bar
       label_data$hjust <- ifelse(angle < -90, 1, 0)
       label_data$angle <- ifelse(angle < -90, angle + 180, angle)
       
       # Prepare a data frame for base lines
       base_data <- road_des %>% 
         group_by(Road.Geometry.Desc) %>% 
         summarize(start = min(id), end = max(id)) %>% 
         rowwise() %>% 
         mutate(title = mean(c(start, end))) %>%
         na.omit()
       
       road_angle <- 90 - 360 * (base_data$title - 0.5) / number_of_bar
       base_data$road_angle <- ifelse(road_angle < -90, road_angle - 90, road_angle - 90)
       base_data$hjust <- ifelse(road_angle < -90, 0.5, 0.5)
       
       # Make the plot
       ggplot(road_des, aes(x = as.factor(id), y = log_count, fill = Accident.Type.Desc)) +
         geom_bar(aes(x = as.factor(id), y = log_count, fill = Accident.Type.Desc), stat = "identity") +
         ylim(-13, 9.5) +
         theme_minimal() +
         scale_fill_manual(values = Acci_col) +
         scale_color_manual(values = Acci_col) +
         theme(
           legend.position = "none",
           axis.text = element_blank(),
           axis.title = element_blank(),
           panel.grid = element_blank(),
           plot.margin = unit(rep(-1, 10), "cm")
         ) +
       coord_polar() + 
         geom_text(data=label_data, aes(x=id, y=log_count, label=Accident_count, hjust=hjust), color="black", fontface="bold",alpha=0.6, size=3.5, angle= label_data$angle, inherit.aes = FALSE )+
         
         # Add base line information
         geom_segment(data=base_data, aes(x = start, y = -0.5, xend = end, yend = -0.5), colour = "black", alpha=0.8, size=0.6 , inherit.aes = FALSE )  +
         geom_text(data=base_data, aes(x = title, y = -2.5, label=Road.Geometry.Desc), hjust=base_data$hjust ,angle=base_data$road_angle, colour = "black", alpha=0.8, size=4.1, fontface="bold", inherit.aes = FALSE)+
         ggtitle(paste("Top 5 road section by Accident type"))
          
   })

  output$cammap <- renderLeaflet(
    
    cam %>% 
      filter(Postcode %in% input$postcode_input)%>%
      leaflet() %>%
      addTiles() %>%
      addProviderTiles("Esri.WorldTopoMap")%>%
      addCircleMarkers(
        lng = ~lon, 
        lat = ~lat, 
        fillOpacity = 0.8,
        popup = ~paste(tags$b("Suburb:"), Suburb,
                       tags$br(), 
                       tags$b("Postcode:"), Postcode,
                       tags$br(), 
                       tags$b("Location:"), Location),
        color = "red"
        
      )
    )
  
  
  output$crashmap1 <- renderLeaflet(
    
    crashmap%>%  
      filter(date >= input$range[1],
             date <= input$range[2]) %>%
      group_by(Long, Lat, date,LGA_NAME)%>%
      summarise(count =n())%>%
      leaflet() %>%
      addTiles() %>%
      addProviderTiles("Esri.WorldTopoMap")%>%
      addCircleMarkers(
        lng = ~Long, 
        lat = ~Lat, 
        radius = ~count+20, 
        fillOpacity = 0.8,
        popup = ~paste(tags$b("LGA name:"), LGA_NAME,
                       tags$br(), 
                       tags$b("Accident Date:"), date,
                       tags$br(), 
                       tags$b("Accident Time:"), count)
        
      )
  )
  
  output$sysplot <- renderPlot(
    {
      Quater_el <-allsys_crash%>%
      filter(ACCIDENTDATE <="2019-12-31")%>%
        filter(cam_sys == input$checkGroup)%>%
      mutate(ACCIDENTDATE = as.Date(ACCIDENTDATE),
             Quarter = floor_date(ACCIDENTDATE,"quarter"),
             SEVERITY = as.character(SEVERITY))%>%
      group_by(Quarter,SEVERITY, cam_sys)%>%
      summarise(count= n())%>%
      filter(SEVERITY >=2)%>%
      mutate(SEVERITY = ifelse(SEVERITY == "3", "Other injury accident","Serious injury accident"))
    
    SEVERITY_labs <- as_labeller(c("Serious injury accident", "Other injury accident"))
        
    ggplot(Quater_el)+
      geom_point(aes(x=Quarter,y=count,
                      gourp=SEVERITY,color=SEVERITY))+
      geom_smooth(aes(x=Quarter,y=count,
                      gourp=SEVERITY,color=SEVERITY))+
      facet_grid(~SEVERITY)+
      ylab("Number of Accident")+
      theme_bw()
    }
  )
  
  }

shinyApp(ui,server)
