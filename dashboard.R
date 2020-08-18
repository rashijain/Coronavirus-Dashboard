# IMPORTING THE REQUIRED LIBRARIES

library(shiny)
library(tidyverse)
library(covid19.analytics)
library(maps)
library(plotly)
library(scales)
library(shinythemes)
library(shadowtext)
library(DT)

#DATA ACQUISITION AND DATA PREPARATION

dat_conf = covid19.data("ts-confirmed")
keycol <- "Date"
valuecol <- "Confirmed"
gathercols <- colnames(dat_conf[,-(1:4)])
dat_conf <- gather_(dat_conf, keycol, valuecol, gathercols)
dat_conf$Date <- as.Date(dat_conf$Date)
dat_conf <- dat_conf   %>%   group_by(Country.Region,Date)   %>%   summarise(Total.Confirmed = sum(Confirmed))

dat_deaths = covid19.data("ts-deaths")
keycol <- "Date"
valuecol <- "Deaths"
gathercols <- colnames(dat_deaths[,-(1:4)])
dat_deaths <- gather_(dat_deaths, keycol, valuecol, gathercols)
dat_deaths$Date <- as.Date(dat_deaths$Date)
dat_deaths <- dat_deaths   %>%   group_by(Country.Region,Date)   %>%   summarise(Total.Deaths = sum(Deaths))

dat_rec = covid19.data("ts-recovered")
keycol <- "Date"
valuecol <- "Recovered"
gathercols <- colnames(dat_rec[,-(1:4)])
dat_rec <- gather_(dat_rec, keycol, valuecol, gathercols)
dat_rec$Date <- as.Date(dat_rec$Date)
dat_rec <- dat_rec   %>%   group_by(Country.Region,Date)   %>%   summarise(Total.Recovered = sum(Recovered))

data_merged <- merge(dat_conf, dat_deaths, by = c("Country.Region","Date"))
data_merged <- merge(data_merged, dat_rec, by = c("Country.Region","Date"))

total_data_merged <- data_merged   %>%   group_by(Date)   %>%   summarise(Total.Confirmed = sum(Total.Confirmed),Total.Deaths = sum(Total.Deaths),Total.Recovered = sum(Total.Recovered))

mapLoc = covid19.data("aggregated")

mapData <- mapLoc[!(mapLoc$Country_Region %in% c("United Kingdom","Netherlands","Denmark","France") & mapLoc$Province_State!=""),]
mapData <- mapData   %>%   filter(!is.na(Lat))   %>%   group_by(Country_Region)   %>%   summarise(lat = mean(Lat), lon = mean(Long_))

USAMapData <- mapLoc[mapLoc$Country_Region == "US",]

dat_USA_All <- USAMapData   %>%   group_by(Province_State)   %>%   summarise(Confirmed = sum(Confirmed), Deaths = sum(Deaths), Recovered = sum(Recovered), Active = sum(Active))
dat_USA_All[is.na(dat_USA_All$Active),"Active"] <- dat_USA_All$Confirmed[is.na(dat_USA_All$Active)] - dat_USA_All$Deaths[is.na(dat_USA_All$Active)]
totalUSA <- dat_USA_All   %>%   summarise(Confirmed = sum(Confirmed), Deaths = sum(Deaths), Recovered = sum(Recovered), Active = sum(Active))
totalUSARow <- gather(totalUSA, "category", "count", Deaths, Recovered, Active)
dataDonutUSA <- totalUSARow %>% 
  mutate(lab.pos = cumsum(count)-.5*count) %>% 
  arrange(desc(count)) %>%
  mutate(category=factor(category,levels=category))
mycols <- c("gold", "green3", "orange")

tableDataUSA <- data.frame(dat_USA_All[dat_USA_All$Province_State!='Recovered', c("Province_State","Confirmed","Deaths","Recovered")])

death_USA <- USAMapData   %>%   group_by(Province_State)   %>%   summarise(Total.Deaths = sum(Deaths))

dat_USA <- USAMapData   %>%   group_by(Province_State)   %>%   summarise(Total.Confirmed = sum(Confirmed))
dat_USA <- dat_USA[order(-dat_USA$Total.Confirmed),]
USAMapData <- USAMapData   %>%   filter(!is.na(Lat))   %>%   group_by(Province_State)   %>%   summarise(lat = mean(Lat), lon = mean(Long_))

dat <- data_merged[data_merged$Date==max(data_merged$Date),]
dat1 <- merge(dat, mapData, by.x = "Country.Region", by.y = "Country_Region")

dat2 <- merge(dat_USA, USAMapData, by.x = "Province_State", by.y = "Province_State")
dat3 <- merge(death_USA, USAMapData, by.x = "Province_State", by.y = "Province_State")

confData <- dat[order(-dat$Total.Confirmed),c("Total.Confirmed","Country.Region")]
deathData <- dat[order(-dat$Total.Deaths),c("Total.Deaths","Country.Region")]
recData <- dat[order(-dat$Total.Recovered),c("Total.Recovered","Country.Region")]

dataTotal <- data.frame(
  category=c("Deaths", "Recovered", "Active"),
  count=c(sum(dat$Total.Deaths), sum(dat$Total.Recovered), sum(dat$Total.Confirmed)-sum(dat$Total.Deaths)-sum(dat$Total.Recovered))
)
dataDonut <- dataTotal %>% 
  mutate(lab.pos = cumsum(count)-.5*count) %>% 
  arrange(desc(count)) %>%
  mutate(category=factor(category,levels=category))
mycols <- c("gold", "green3", "orange")

ndat <- dat[c("Country.Region","Total.Confirmed","Total.Deaths","Total.Recovered")]

confDataUSA <- tableDataUSA[order(-tableDataUSA$Confirmed),c("Confirmed","Province_State")]
deathDataUSA <- tableDataUSA[order(-tableDataUSA$Deaths),c("Deaths","Province_State")]

timeseriesUSA <- read.csv("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us.csv")
timeseriesUSA$date <- as.Date(timeseriesUSA$date)
timeseriesUSA <- timeseriesUSA %>% rename(Date = date, Total.Confirmed = cases, Total.Deaths = deaths)

# UI COMPONENT OF THE DASHBOARD

ui <- fluidPage(theme = shinytheme("superhero"),
                includeCSS("script.css"),
                navbarPage("Coronavirus Dashboard",
                tabPanel("World",
                fluidRow(
                  column(2, style='padding:5px;',
                         wellPanel(p("Total Confirmed:", align = "center"),
                                   h2(strong(textOutput("totalTextConf")), align = "center", style = "color:red")),
                         wellPanel(p("Total Active:", align = "center"),
                                   h2(strong(textOutput("totalTextActive")), align = "center", style = "color:gold")),
                         wellPanel(p("Total Recovered:", align = "center"),
                                   h2(strong(textOutput("totalTextRec")), align = "center", style = "color:limegreen")),
                         wellPanel(p("Total Deaths:", align = "center"),
                                   h2(strong(textOutput("totalTextDeaths")), align = "center", style = "color:orange")),
                         div(style='padding-bottom: 10px;',
                             plotOutput("donutTotal", height = "213px")
                             )
                  ),
                  column(6, style='padding:5px;',
                         tabsetPanel(
                           tabPanel("Confirmed", plotlyOutput("WorldMapPlotConf"),
                                    plotlyOutput("CountriesConf", height = "150px")),
                           tabPanel("Deaths", plotlyOutput("WorldMapPlotDeaths"),
                                    plotlyOutput("CountriesDeaths", height = "150px")),
                           tabPanel("Recovered", plotlyOutput("WorldMapPlotRec"),
                                    plotlyOutput("CountriesRec", height = "150px"))
                         )
                  ),
                  column(4, style='padding:5px;',
                         tabsetPanel(
                           tabPanel("Linear",plotlyOutput("totalPlot", height = "200px")),
                           tabPanel("Logarithmic",plotlyOutput("totalPlotLog", height = "200px"))
                         ),
                         wellPanel(style='margin-top: 10px;',
                         div(style='padding-bottom: 10px;height:225px; overflow-y: scroll;',
                             dataTableOutput("tableAll")
                         )
                         ),
                         wellPanel(p("Countries/Regions:", align = "center"),
                                   h2(strong(textOutput("totalTextCountry")), align = "center"))
                            
                         
                         #           plotlyOutput("USAMapPlot", height = "275px")
                  )
                )),
                tabPanel("USA",
                         fluidRow(
                           column(2, style='padding:5px;',
                                  wellPanel(p("Total Confirmed:", align = "center"),
                                            h2(strong(textOutput("totalTextConfUSA")), align = "center", style = "color:red")),
                                  wellPanel(p("Total Active:", align = "center"),
                                            h2(strong(textOutput("totalTextActiveUSA")), align = "center", style = "color:gold")),
                                  wellPanel(p("Total Recovered:", align = "center"),
                                            h2(strong(textOutput("totalTextRecUSA")), align = "center", style = "color:limegreen")),
                                  wellPanel(p("Total Deaths:", align = "center"),
                                            h2(strong(textOutput("totalTextDeathsUSA")), align = "center", style = "color:orange")),
                                  div(style='padding-bottom: 10px;',
                                      plotOutput("donutTotalUSA", height = "213px")
                                  )
#                                  wellPanel(p("Countries/Regions:", align = "center"),
#                                            h2(strong(textOutput("totalTextCountry")), align = "center")),
                           ),
                           column(6, style='padding:5px;',
                                  tabsetPanel(
                                    tabPanel("Confirmed", plotlyOutput("USAMapPlot"),
                                             plotlyOutput("StatesConf", height = "150px")),
                                    tabPanel("Deaths", plotlyOutput("USAMapPlotDeath"),
                                             plotlyOutput("StatesDeaths", height = "150px"))
                                  )
                           ),
                           column(4, style='padding:5px;',
                                  tabsetPanel(
                                    tabPanel("Linear",plotlyOutput("totalPlotUSA", height = "200px")),
                                    tabPanel("Logarithmic",plotlyOutput("totalPlotLogUSA", height = "200px"))
                                  ),
                                  wellPanel(style='margin-top: 10px;',
                                  div(style='padding-bottom: 10px;height:225px; overflow-y: scroll;',
                                      dataTableOutput("tableAllUSA")
                                  )),
                                  wellPanel(p("Provinces/States:", align = "center"),
                                            h2(strong(textOutput("totalTextState")), align = "center"))
                                  #plotlyOutput("USAMapPlot")
                           )
                         )))
)

# SERVER COMPONENT OF THE DASHBOARD

server <- function(input, output) {
  output$totalPlot <- renderPlotly({
    plt <- ggplot(data=total_data_merged) + theme(axis.title=element_blank(), panel.background = element_rect(fill = "white",colour = "white", size = 0.5, linetype = "solid"), panel.grid.major = element_line(size = 0.5, linetype = 'solid',colour = "#4e5d6c"), panel.grid.minor = element_line(size = 0.25, linetype = 'solid',colour = "#4e5d6c"))
    plt <- plt + geom_line(aes(x=Date, y=Total.Confirmed, group=1),color="red")
    plt <- plt + geom_line(aes(x=Date, y=Total.Deaths, group=1),color="orange")
    plt <- plt + geom_line(aes(x=Date, y=Total.Recovered, group=1),color="green3")
#    plt <- plt + scale_y_continuous(label = unit_format(unit = "M", scale = 1e-6, sep = "", accuracy = 0.1))
    ggplotly(plt,dynamicTicks = TRUE)
  })
  
  output$totalPlotLog <- renderPlotly({
    plt <- ggplot(data=total_data_merged) + theme(axis.title=element_blank(), panel.background = element_rect(fill = "white",colour = "white", size = 0.5, linetype = "solid"), panel.grid.major = element_line(size = 0.5, linetype = 'solid',colour = "#4e5d6c"), panel.grid.minor = element_line(size = 0.25, linetype = 'solid',colour = "#4e5d6c"))
    plt <- plt + geom_line(aes(x=Date, y=Total.Confirmed, group=1),color="red")
    plt <- plt + geom_line(aes(x=Date, y=Total.Deaths, group=1),color="orange")
    plt <- plt + geom_line(aes(x=Date, y=Total.Recovered, group=1),color="green3")
    plt <- plt + scale_y_log10()
    ggplotly(plt,dynamicTicks = TRUE)
  })
  
  output$totalPlotUSA <- renderPlotly({
    plt <- ggplot(data=timeseriesUSA) + theme(axis.title=element_blank(), panel.background = element_rect(fill = "white",colour = "white", size = 0.5, linetype = "solid"), panel.grid.major = element_line(size = 0.5, linetype = 'solid',colour = "#4e5d6c"), panel.grid.minor = element_line(size = 0.25, linetype = 'solid',colour = "#4e5d6c"))
    plt <- plt + geom_line(aes(x=Date, y=Total.Confirmed, group=1),color="red")
    plt <- plt + geom_line(aes(x=Date, y=Total.Deaths, group=1),color="orange")
#    plt <- plt + scale_y_continuous(label = unit_format(unit = "M", scale = 1e-6, sep = "", accuracy = 0.1))
    ggplotly(plt,dynamicTicks = TRUE)
  })
  
  output$totalPlotLogUSA <- renderPlotly({
    plt <- ggplot(data=timeseriesUSA) + theme(axis.title=element_blank(), panel.background = element_rect(fill = "white",colour = "white", size = 0.5, linetype = "solid"), panel.grid.major = element_line(size = 0.5, linetype = 'solid',colour = "#4e5d6c"), panel.grid.minor = element_line(size = 0.25, linetype = 'solid',colour = "#4e5d6c"))
    plt <- plt + geom_line(aes(x=Date, y=Total.Confirmed, group=1),color="red")
    plt <- plt + geom_line(aes(x=Date, y=Total.Deaths, group=1),color="orange")
    plt <- plt + scale_y_log10()
    ggplotly(plt,dynamicTicks = TRUE)
  })
  
  output$WorldMapPlotConf <- renderPlotly({
    mapWorld <- borders("world", colour="gray50", fill="white", size=0.1) # create a layer of borders
    mp <- ggplot() +   mapWorld
    mp <- mp+ geom_point(data=dat1, aes(x=lon, y=lat , size=Total.Confirmed, alpha=0.7, stroke=1, text=Country.Region),color="red") + theme(legend.position = "none", panel.grid = element_blank(), axis.title = element_blank(), axis.text = element_blank(), axis.ticks = element_blank(), panel.background = element_rect(fill = "lightblue"), axis.ticks.length = unit(0, "pt"), plot.margin = margin(0, 12, 0, 0, "pt"))
    ggplotly(mp, tooltip = c("text","size"))
  })
  
  output$WorldMapPlotDeaths <- renderPlotly({
    mapWorld <- borders("world", colour="gray50", fill="white", size=0.1) # create a layer of borders
    mp <- ggplot() +   mapWorld
    mp <- mp+ geom_point(data=dat1, aes(x=lon, y=lat , size=Total.Deaths, alpha=0.7, stroke=1, text=Country.Region),color="orange") + theme(legend.position = "none", panel.grid = element_blank(), axis.title = element_blank(), axis.text = element_blank(), axis.ticks = element_blank(), panel.background = element_rect(fill = "lightblue"), axis.ticks.length = unit(0, "pt"), plot.margin = margin(0, 12, 0, 0, "pt"))
    ggplotly(mp, tooltip = c("text","size"))
  })
  
  output$WorldMapPlotRec <- renderPlotly({
    mapWorld <- borders("world", colour="gray50", fill="white", size=0.1) # create a layer of borders
    mp <- ggplot() +   mapWorld
    mp <- mp+ geom_point(data=dat1, aes(x=lon, y=lat , size=Total.Recovered, alpha=0.7, stroke=1, text=Country.Region),color="green3") + theme(legend.position = "none", panel.grid = element_blank(), axis.title = element_blank(), axis.text = element_blank(), axis.ticks = element_blank(), panel.background = element_rect(fill = "lightblue"), axis.ticks.length = unit(0, "pt"), plot.margin = margin(0, 12, 0, 0, "pt"))
    ggplotly(mp, tooltip = c("text","size"))
  })
  
  output$CountriesConf <- renderPlotly({
    p <- arrange(confData[1:5,], Total.Confirmed) %>% mutate(Country.Region = factor(Country.Region, levels = .$Country.Region)) %>% ggplot(aes(x=Country.Region, y=Total.Confirmed, fill=Country.Region )) + geom_bar(stat = "identity") + coord_flip() + theme(legend.position = "none") + scale_fill_brewer(palette = "Reds")
    p <- p + scale_y_continuous(label = unit_format(unit = "K", scale = 1e-3, sep = "", accuracy = 1)) + theme(axis.title=element_blank(), panel.background = element_rect(fill = "white",colour = "white", size = 0.5, linetype = "solid"), panel.grid.major = element_line(size = 0.5, linetype = 'solid',colour = "#4e5d6c"), panel.grid.minor = element_line(size = 0.25, linetype = 'solid',colour = "#4e5d6c"))
    ggplotly(p, tooltip = c("x","y"),dynamicTicks = TRUE)
  })
  
  output$CountriesDeaths <- renderPlotly({
    p <- arrange(deathData[1:5,], Total.Deaths) %>% mutate(Country.Region = factor(Country.Region, levels = .$Country.Region)) %>% ggplot(aes(x=Country.Region, y=Total.Deaths, fill=Country.Region )) + geom_bar(stat = "identity") + coord_flip() + theme(legend.position = "none")  + scale_fill_brewer(palette = "Oranges")
    p <- p + scale_y_continuous(label = unit_format(unit = "K", scale = 1e-3, sep = "", accuracy = 1)) + theme(axis.title=element_blank(), panel.background = element_rect(fill = "white",colour = "white", size = 0.5, linetype = "solid"), panel.grid.major = element_line(size = 0.5, linetype = 'solid',colour = "#4e5d6c"), panel.grid.minor = element_line(size = 0.25, linetype = 'solid',colour = "#4e5d6c"))
    ggplotly(p, tooltip = c("x","y"),dynamicTicks = TRUE)
  })
  
  output$CountriesRec <- renderPlotly({
    p <- arrange(recData[1:5,], Total.Recovered) %>% mutate(Country.Region = factor(Country.Region, levels = .$Country.Region)) %>% ggplot(aes(x=Country.Region, y=Total.Recovered, fill=Country.Region )) + geom_bar(stat = "identity") + coord_flip() + theme(legend.position = "none") + scale_fill_brewer(palette = "Greens")
    p <- p + scale_y_continuous(label = unit_format(unit = "K", scale = 1e-3, sep = "", accuracy = 1)) + theme(axis.title=element_blank(), panel.background = element_rect(fill = "white",colour = "white", size = 0.5, linetype = "solid"), panel.grid.major = element_line(size = 0.5, linetype = 'solid',colour = "#4e5d6c"), panel.grid.minor = element_line(size = 0.25, linetype = 'solid',colour = "#4e5d6c"))
    ggplotly(p, tooltip = c("x","y"),dynamicTicks = TRUE)
  })
  
  output$StatesConf <- renderPlotly({
    p <- arrange(confDataUSA[1:5,], Confirmed) %>% mutate(Province_State = factor(Province_State, levels = .$Province_State)) %>% ggplot(aes(x=Province_State, y=Confirmed, fill=Province_State )) + geom_bar(stat = "identity") + coord_flip() + theme(legend.position = "none") + scale_fill_brewer(palette = "Reds")
    p <- p + scale_y_continuous(label = unit_format(unit = "K", scale = 1e-3, sep = "", accuracy = 1)) + theme(axis.title=element_blank(), panel.background = element_rect(fill = "white",colour = "white", size = 0.5, linetype = "solid"), panel.grid.major = element_line(size = 0.5, linetype = 'solid',colour = "#4e5d6c"), panel.grid.minor = element_line(size = 0.25, linetype = 'solid',colour = "#4e5d6c"))
    ggplotly(p, tooltip = c("x","y"),dynamicTicks = TRUE)
  })
  
  output$StatesDeaths <- renderPlotly({
    p <- arrange(deathDataUSA[1:5,], Deaths) %>% mutate(Province_State = factor(Province_State, levels = .$Province_State)) %>% ggplot(aes(x=Province_State, y=Deaths, fill=Province_State )) + geom_bar(stat = "identity") + coord_flip() + theme(legend.position = "none")  + scale_fill_brewer(palette = "Oranges")
    p <- p + scale_y_continuous(label = unit_format(unit = "K", scale = 1e-3, sep = "", accuracy = 1)) + theme(axis.title=element_blank(), panel.background = element_rect(fill = "white",colour = "white", size = 0.5, linetype = "solid"), panel.grid.major = element_line(size = 0.5, linetype = 'solid',colour = "#4e5d6c"), panel.grid.minor = element_line(size = 0.25, linetype = 'solid',colour = "#4e5d6c"))
    ggplotly(p, tooltip = c("x","y"),dynamicTicks = TRUE)
  })
  
  output$donutTotal <- renderPlot({
    ggplot(dataDonut, aes(x = 2, y=count, fill = category)) + 
      geom_bar(stat = "identity", color="white") + 
      xlim(1,2.5) + scale_fill_manual(values = mycols) +
      coord_polar(theta = "y", start=0) + 
      geom_text(aes(y = lab.pos, label = category), color = "black") + 
      annotate(geom = 'text', x = 1, y = 0, label = paste0("Confirmed\n", sum(dataTotal$count)), color="red") + 
      theme(legend.position = "none", panel.grid = element_blank(), axis.title = element_blank(), axis.text = element_blank(), axis.ticks = element_blank(), panel.background = element_rect(fill = "white"), axis.ticks.length = unit(0, "pt"), plot.margin = margin(0, 0, 0, 0, "pt"))
  })
  
  output$donutTotalUSA <- renderPlot({
    ggplot(dataDonutUSA, aes(x = 2, y=count, fill = category)) + 
      geom_bar(stat = "identity", color="white") + 
      xlim(1,2.5) + scale_fill_manual(values = mycols) +
      coord_polar(theta = "y", start=0) + 
      geom_text(aes(y = lab.pos, label = category), color = "black") + 
      annotate(geom = 'text', x = 1, y = 0, label = paste0("Confirmed\n", sum(totalUSA$Confirmed)), color="red") + 
      theme(legend.position = "none", panel.grid = element_blank(), axis.title = element_blank(), axis.text = element_blank(), axis.ticks = element_blank(), panel.background = element_rect(fill = "white"), axis.ticks.length = unit(0, "pt"), plot.margin = margin(0, 0, 0, 0, "pt"))
  })
  
  output$USAMapPlot <- renderPlotly({
    mapWorld <- borders("state", colour="gray50", fill="white", size=0.1) # create a layer of borders
    mp <- ggplot() +   mapWorld
    mp <- mp+ geom_point(data=dat2, aes(x=lon, y=lat , size=Total.Confirmed, alpha=0.7, stroke=1, text=Province_State),color="red") + theme(legend.position = "none", panel.grid = element_blank(), axis.title = element_blank(), axis.text = element_blank(), axis.ticks = element_blank(), panel.background = element_rect(fill = "lightblue"), axis.ticks.length = unit(0, "pt"), plot.margin = margin(0, 12, 0, 0, "pt")) 
    mp <- mp + xlim(-130,-65) + ylim(25,50)
    ggplotly(mp, tooltip = c("text","size"))
  })
  
  output$USAMapPlotDeath <- renderPlotly({
    mapWorld <- borders("state", colour="gray50", fill="white", size=0.1) # create a layer of borders
    mp <- ggplot() +   mapWorld
    mp <- mp+ geom_point(data=dat3, aes(x=lon, y=lat , size=Total.Deaths, alpha=0.7, stroke=1, text=Province_State),color="orange") + theme(legend.position = "none", panel.grid = element_blank(), axis.title = element_blank(), axis.text = element_blank(), axis.ticks = element_blank(), panel.background = element_rect(fill = "lightblue"), axis.ticks.length = unit(0, "pt"), plot.margin = margin(0, 12, 0, 0, "pt")) 
    mp <- mp + xlim(-130,-65) + ylim(25,50)
    ggplotly(mp, tooltip = c("text","size"))
  })
  
  output$totalTextConf <- renderText({ format(sum(dat$Total.Confirmed),big.mark=",",scientific=FALSE) })
  
  output$totalTextActive <- renderText({ format(dataTotal[dataTotal$category == "Active","count"],big.mark=",",scientific=FALSE) })
  
  output$totalTextDeaths <- renderText({ format(sum(dat$Total.Deaths),big.mark=",",scientific=FALSE) })
  
  output$totalTextRec <- renderText({ format(sum(dat$Total.Recovered),big.mark=",",scientific=FALSE) })
  
  output$totalTextCountry <- renderText({ format(length(unique(data_merged[,"Country.Region"])) ,big.mark=",",scientific=FALSE) })
  
  output$totalTextConfUSA <- renderText({ format(totalUSA$Confirmed,big.mark=",",scientific=FALSE) })
  
  output$totalTextActiveUSA <- renderText({ format(totalUSA$Active,big.mark=",",scientific=FALSE) })
  
  output$totalTextDeathsUSA <- renderText({ format(totalUSA$Deaths,big.mark=",",scientific=FALSE) })
  
  output$totalTextRecUSA <- renderText({ format(totalUSA$Recovered,big.mark=",",scientific=FALSE) })
  
  output$tableAll <- renderDataTable(ndat %>% rename('Country/Region' = Country.Region, Confirmed = Total.Confirmed, Deaths = Total.Deaths, Recovered = Total.Recovered), rownames = FALSE, options = list(paging = FALSE, searching = FALSE, extensions = "Select", selection = "none", info = FALSE))
  
  output$tableAllUSA <- renderDataTable(tableDataUSA %>% rename('Province/State' = Province_State), rownames = FALSE, options = list(paging = FALSE, searching = FALSE, extensions = "Select", selection = "none", info = FALSE))
  
  output$totalTextState <- renderText({ format(length(unique(tableDataUSA[,"Province_State"])) ,big.mark=",",scientific=FALSE) })
}

shinyApp(ui = ui, server = server)