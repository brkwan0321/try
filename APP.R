# import packages
library(wordcloud)
library(ggplot2)
library(leaflet)
library(tidyverse)
library(rgdal) 
library(ggthemes)
library(ggmap)  
library(shiny)
library(shinythemes)
library(htmltools)
library(dplyr) 
library(fmsb)
library(janitor) 
library(reshape2)


# Get all the position for different position group
fifa_data <- read.csv("fifa_wrangled.csv")
fifa_data <- fifa_data[,-1] # drop the first useless column 
country_count2 <- read.csv("country_map.csv")
fifa_data <- fifa_data[!(fifa_data$Position == ""),] # drop na
positions <- unique(fifa_data$Position) # get all positions
gk <- "GK"
defs <- positions[str_detect(positions, "B$")] # get positions of group DEF
mids <- positions[str_detect(positions, "M$")]# get positions of group MID
attack <- fifa_data%>%# get positions of group FWD
  filter(!Position %in% mids)%>%
  filter(!Position %in% defs)%>%
  filter(!Position %in% c("GK"))
fwds <- unique(attack$Position)

# map data
map_data <- readOGR("./World_Countries/")  # read world map shapefile

#do some aggregation for plotting
country_count <- aggregate(Overall ~ Nationality, FUN =mean,data = fifa_data)# 
rates <- country_count$Overall[match(map_data$COUNTRY, country_count$Nationality)] 
cpal1 <- colorNumeric("YlOrBr", rates, na.color = "transparent") # define  colors range


# Filter the good player have Overall ranking ≥ 75
over75 <- fifa_data[fifa_data$Overall >= 75,]
count_num <- over75 %>%
  group_by(Nationality)%>%
  summarise( player_count = n())

# define position group for each row
fifa_data <- fifa_data %>% 
  mutate(PositionGroup = ifelse(Position %in% gk, "GK", ifelse(Position %in% defs, "DEF", ifelse(Position %in% mids, "MID", ifelse(Position %in% fwds, "FWD", "Unknown")))))

#select the column exclude the data about goalkeeper
gk_vars <- fifa_data %>% select(contains("GK")) %>% names()
tile_data <- fifa_data %>%
  select_if(is.numeric) %>%
  select(-gk_vars) %>%
  left_join(fifa_data %>% select(ID, Position, PositionGroup), by = "ID") %>%
  filter(Overall >= 75) %>%
  select(-ID,-X,  -Skill.Moves,-International.Reputation, -Jersey.Number, -Special, -Weak.Foot, -Age, -Overall, -lon,-lat, -Potential, -starts_with("Wage"))


tile_data <- tile_data %>% select(-names(tile_data)[1:26])

# do not compare other positions with goalkeeper here, drop it and get the information of the rest data
tile_data <- tile_data %>%
  filter(Position != "GK") %>%
  gather(key = Attribute, value = Value, -Position, -PositionGroup) %>%
  group_by(PositionGroup, Position, Attribute) %>%
  summarise(MedianValue = median(Value, na.rm = T))

##################################
# forward 
tile_fwd <- tile_data%>%
  as.data.frame()%>%
  filter(PositionGroup == "FWD")%>% # select midfield players
  select(Attribute, MedianValue)%>% # get the median value of each skills
  group_by(Attribute)%>%
  summarise(MeanValue = mean(MedianValue,na.rm = T))%>%
  top_n(5)%>% # choose the top 5 needed skills
  t()%>% 
  row_to_names(row_number = 1)%>% # change row names
  as.data.frame()

tile_range <- data.frame(c('81','78'),c('81','78'),c('81','78'),c('81','78'),c('81','78')) # the range of radar chart
colnames(tile_range) <- names(tile_fwd)
tile_fwd.final <- rbind(tile_range,tile_fwd) # combine the data

# change column data type
tile_fwd.final$Acceleration <-  as.numeric(as.character(tile_fwd.final$Acceleration))
tile_fwd.final$Agility <-  as.numeric(as.character(tile_fwd.final$Agility))
tile_fwd.final$BallControl <-  as.numeric(as.character(tile_fwd.final$BallControl))
tile_fwd.final$Dribbling <-  as.numeric(as.character(tile_fwd.final$Dribbling))
tile_fwd.final$Positioning <-  as.numeric(as.character(tile_fwd.final$Positioning))

#########################
# midfield
tile_mid <- tile_data%>%
  as.data.frame()%>%
  filter(PositionGroup == "MID")%>% # select Forward players
  select(Attribute, MedianValue)%>% # get the median value of each skills
  group_by(Attribute)%>%
  summarise(MeanValue = mean(MedianValue,na.rm = T))%>%
  top_n(5)%>% # choose the top 5 needed skills
  t()%>%
  row_to_names(row_number = 1)%>%# change row names
  as.data.frame()

tile_range.mid <- data.frame(c('80','75'),c('80','75'),c('80','75'),c('80','75'),c('80','75'))# the range of radar chart
colnames(tile_range.mid) <- names(tile_mid)
tile_mid.final <- rbind(tile_range.mid,tile_mid)# combine the data

# change column data type
tile_mid.final$Composure <-  as.numeric(as.character(tile_mid.final$Composure))
tile_mid.final$ShortPassing <-  as.numeric(as.character(tile_mid.final$ShortPassing))
tile_mid.final$BallControl <-  as.numeric(as.character(tile_mid.final$BallControl))
tile_mid.final$Dribbling <-  as.numeric(as.character(tile_mid.final$Dribbling))
tile_mid.final$Stamina <-  as.numeric(as.character(tile_mid.final$Stamina))

##############################
# backwards
tile_def <- tile_data%>%
  as.data.frame()%>%
  filter(PositionGroup == "DEF")%>% # select backward players
  select(Attribute, MedianValue)%>% # get the median value of each skills
  group_by(Attribute)%>%
  summarise(MeanValue = mean(MedianValue,na.rm = T))%>%
  top_n(5)%>% # choose the top 5 needed skills
  t()%>%
  row_to_names(row_number = 1)%>%# change row names
  as.data.frame()

tile_range.def <- data.frame(c('78','75'),c('77','75'),c('78','75'),c('78','75'),c('78','75'))# the range of radar chart
colnames(tile_range.def) <- names(tile_def)
tile_def.final <- rbind(tile_range.def,tile_def)# combine the data

# change column data type
tile_def.final$Aggression <-  as.numeric(as.character(tile_def.final$Aggression))
tile_def.final$Interceptions <-  as.numeric(as.character(tile_def.final$Interceptions))
tile_def.final$SlidingTackle <-  as.numeric(as.character(tile_def.final$SlidingTackle))
tile_def.final$Stamina <-  as.numeric(as.character(tile_def.final$Stamina))
tile_def.final$StandingTackle <-  as.numeric(as.character(tile_def.final$StandingTackle))
#########################################
# get the data we need for comparing GDP and soccer level
rates <- country_count$Overall[match(map_data$COUNTRY, country_count$Nationality)] 

country_count3 <- country_count
country_count3$GDP <- country_count2$GDP[match(country_count$Nationality, country_count2$Country)] # join two data frame
count_merge <- country_count3[complete.cases(country_count3),]
count_num2 <- count_num
count_num2$GDP <- country_count2$GDP[match(count_num2$Nationality, country_count2$Country)]
count_merge2 <- count_num2[complete.cases(count_num2),]

#plot the relationship between GDP and soccer level
# 1. number of good player
gdp.count <- ggplot(count_merge2, aes(GDP, player_count)) +
  geom_point(color = "black") +
  geom_smooth(method = "lm")+
  ylab("player count")

# 2. mean player overall ranking
gdp.over <- ggplot(count_merge, aes(GDP, Overall)) +
  geom_point(color = "black") +
  geom_smooth(method = "lm")+
  ylab("Average Rating")


# drop the missing value
country_count2<- country_count2[complete.cases(country_count2),]
###########################################

server <- function(input, output){
  output$mapPlot <- renderLeaflet({ # use leaflet directly in shiny
    country_count_select <- country_count2[(input$GDP[1] <= country_count2$GDP & country_count2$GDP <= input$GDP[2]),] # use the input range to filter the data
    rates.select <- rates[input$rank[1] <= rates & rates <= input$rank[2]]
    # use leaflet to plot the choropleth map
    leaflet(map_data) %>% 
      addTiles() %>% # add tile
      addPolygons( # draw polygons on top of the base map (tile)
        stroke = FALSE,
        smoothFactor = 0.2,
        fillOpacity = 1,
        color = ~cpal1(rates.select) # use the rate of each state to find the correct color
      ) %>%
      addCircleMarkers(data = country_count_select, lat = ~lat, lng = ~lon, color = "#4d004b", radius = ~GDP/5000, # add the circle marker as GDP of the country
                       label = mapply(function(x, y) {
                         HTML(sprintf("<em>%s \n GDP:</em> %s", htmlEscape(x), htmlEscape(y)))}, # define the label of the tooltip
                         country_count2$Country, country_count2$GDP, SIMPLIFY = F),
                       labelOptions = lapply(1:nrow(country_count2), function(x) {
                         labelOptions(direction='auto')}))%>%
      addLegend( pal=cpal1, values=rates.select, opacity=0.7, title = "Mean rating", position = "bottomleft" )
  })
  # call the graph we plot before
  output$gdp_curve <- renderPlot({
    gdp.count
  })
  output$gdp_curve2 <- renderPlot({
    gdp.over
  })
  # heatmap and spider chart
  output$heatplot <- renderPlot({ # plot the data in shiny
    tile_data %>%
      filter(PositionGroup == input$PG) %>% # filter the data according to the input range
      ggplot(aes(x= Attribute, y= Position)) + # define x axis and y axis to create heatmap
      geom_tile(aes(fill = MedianValue), colour = "black") +
      geom_text(aes(label = MedianValue)) +
      scale_fill_gradient(low = "yellow", high = "red") + 
      theme(axis.text.x = element_text(angle = 45, hjust = 1), strip.text = element_text(face = "bold", size = 12), legend.position = "none")
    
  })
  
  output$spider <- renderPlot({
    # plot different radarchart with different select postion group
    if (input$PG == "FWD"){
      radarchart(tile_fwd.final, axistype = 0, 
                 
                 #custom polyon
                 pcol = rgb(0.3,0.2,0.3,0.3), pfcol = rgb(0.2,0.7,0.6,0.2),plwd = 4, plty=1,
                 
                 #custom color
                 cglcol = 'grey', cglty = 1, axislabcol = 'grey',
                 vlcex = 0.8
      )
    }
    if(input$PG == "MID"){
      radarchart(tile_mid.final, axistype = 0,
                 
                 #custom polyon
                 pcol = rgb(0.3,0.2,0.3,0.3), pfcol = rgb(0.2,0.7,0.6,0.2),plwd = 4, plty=1,
                 
                 #custom color
                 cglcol = 'grey', cglty = 1, axislabcol = 'grey',
                 vlcex = 0.8
      )
    }
    if(input$PG == "DEF"){
      radarchart(tile_def.final, axistype = 0,
                 
                 #custom polyon
                 pcol = rgb(0.3,0.2,0.3,0.3), pfcol = rgb(0.2,0.7,0.6,0.2),plwd = 4, plty=1,
                 
                 #custom color
                 cglcol = 'grey', cglty = 1, axislabcol = 'grey',
                 vlcex = 0.8
      )
    }
  
  })
  # create some reactive text 
  PositionGroup1 <- reactiveText(function(){
    paste("The heat map of all Positions in Position Group: ", input$PG)
  })
  best <- reactiveText(function(){
    paste("5 most needed skills in Positon Group ", input$PG)
  })
  output$PositionGroup <- renderText({PositionGroup1()})
  output$best5 <- renderText({best()})
  
  #wordcloud
  output$wordcloud100 <- renderPlot({
    if(is.null(input$checkGroup)){ # do not plot the data without any selected position group
      renderPrint({input$checkGroup})
    }
    else{
    top1001<- fifa_data[input$top_num[1]:input$top_num[2],]
    wc.data1 <- top1001%>%select(Name,Overall,PositionGroup,Preferred.Foot)%>%filter(Preferred.Foot %in% input$checkGroup)
    
    wc11 <- dcast(wc.data1,Name +Preferred.Foot+ Overall ~ PositionGroup, value.var = "Overall") # cast the molten data frame into a data froame
    rownames(wc11) <-wc11[,1] #use name of States as row names
    wc11 <- wc11[,-c(1,2,3)] #remove "States" and "Colleges" column
    wc11[is.na(wc11)] <- 0  #set NA values to zero
    wc11 <- as.matrix(wc11) #convert into matrix
    comparison.cloud(wc11,max.words=Inf,random.order=FALSE, scale = c(.5,.2),  #create the word cloud
                     title.size = 1,  colors=c("purple","orange","red","lightblue"))
    }
  })

  output$piechart <- renderPlot({
    if(is.null(input$checkGroup)){
      renderPrint({input$checkGroup})
    }
    else{
      #prepare the data
      top1001<- fifa_data[input$top_num[1]:input$top_num[2],]
      wc.data1 <- top1001%>%select(Name,Overall,PositionGroup,Preferred.Foot)%>%filter(Preferred.Foot %in% input$checkGroup)

      pie.data1 <- as.data.frame(table(wc.data1[,3]))
      names(pie.data1) <- c('PositionGroup', 'Freq')
      # prepare the data for pie chart
      pie.data1%>%
        mutate(PositionGroup = factor(PositionGroup, levels = c("MID","GK","FWD","DEF")), #opposite order of the data
               cumulative = cumsum(Freq), 
               midpoint = cumulative - Freq / 2,label = paste0(PositionGroup, " ", round(Freq / sum(Freq) * 100, 1), "%")) -> pie.data1 # set the location of the label
      # create the pie chart
      ggplot(pie.data1, aes(x = "", weight = Freq, fill = PositionGroup)) + 
        geom_bar(width = 1, position = "stack") +
        coord_polar("y", start = 0) +
        scale_fill_manual(values=c("lightblue","red","orange","purple"))+
        geom_text(aes(x = 1.3, y = midpoint, label = label)) +
        theme_nothing()
    }
       })

}

#UI Design
ui <- navbarPage(theme = shinytheme("flatly"),collapsible = TRUE, title =  "FIFA 2019", # define a menu bar on the top
                 tabPanel("Amoung The World", # create a tab panel
                          sidebarLayout( # create a side bar
                            sidebarPanel(draggable = TRUE, 
                              # define a slider input
                              sliderInput("GDP",
                                          label = h4("GDP Range "),
                                          min = min(country_count2$GDP), max = max(country_count2$GDP), value = c(min, max)), #define the range of the slider input
                              br(), #blank line
                              sliderInput("rank",
                                          label = h4("Average Ranking"),
                                          min = min(rates[complete.cases(rates)]), max = max(rates[complete.cases(rates)]), value = c(min, max)),
                              strong("Graph Explaintion: "),
                              # put two scatter plot into the ui
                              p("The relationship between GDP and the number of Good player for a country.
                                 The GDP here represents the personal product for this country. "),
                              plotOutput("gdp_curve", height="160px", width="100%"),
                              h6("The relationship between GDP and average ranking for a country."),
                              plotOutput("gdp_curve2", height="160px", width="100%")
                          ),
                          mainPanel( #plot the map in the main panel
                            leafletOutput("mapPlot", width="100%", height=700) # call the mapPlot defined in sever
                          )
                        )
                ),
                
                tabPanel("Skills Analysis",
                         sidebarLayout(
                           sidebarPanel(
                             # define the selections of select bar
                             selectInput(inputId = "PG", 
                                         label = "Position Group",
                                         choices = c("Forward" = "FWD",
                                                     "Midfield" = "MID",
                                                     "Backward" = "DEF"),
                                         selected = "FWD"),
                             strong("Graph Explaintion: "),
                             p("GoalKeeper is not included here because the heatmap on right hand side does not contain Goalkeeper's skills. 
                                Thus we do not compare Goalkeeper with the other positions in here. 
                                For the rest positions, we devide them into three position group, they are Forward(abbr. FWD), Midfield(abbr. MID) and Backward(abbr. DEF)."),
                             br(),
                             h4(textOutput("best5")),
                             hr("The radar chart of top 5 needed skills"),
                             plotOutput("spider")
                           ),
                           mainPanel(
                                      h2(textOutput("PositionGroup"), align = "center"), 
                                     plotOutput("heatplot")
                                     )
                         )
                          ),
                tabPanel("Players Word Cloud",
                         sidebarLayout(
                           sidebarPanel(draggable = TRUE,
                                        sliderInput("top_num",
                                                     label = h4("Please choose the player ranking range: "),
                                                     min = 0, max =500, value = c(min, max)),
                                        checkboxGroupInput("checkGroup", label = h3("Preferred Foot"),  # define the check box
                                                           choices = list("Left", "Right"),
                                                           selected = "Left"),
                                        br(),
                                        strong("Graph Explaintion: "),
                                        p("The word cloud shows all the player names for different groups in different colors 
                                        and the pie chart shows the ratio of different Position group in the condition of selected value in the side bar.
                                        Besides, the color of a position group will have same color between pie chart and word cloud.
                                           We can choose the ranking range of players in the slider Input, for example, if users wants to shows the players from 100th to 500th,
                                           they can drag the button on slider input and change the range. 
                                           Also, we can filter the players with different preffered foot by clicking the check box.")
                           ),
                        mainPanel(
                                     h2("Word cloud of player's name"),
                                     p("For player in different Position Group, they are separated from each other and different position group has different color,
                                        which makes the word cloud shows more directly."),
                                     plotOutput("wordcloud100"),
                                     h2("The ratio of different Position Group"),
                                     plotOutput("piechart"))
                        ))
                
)

# Run the whole page
shinyApp(ui, server)
