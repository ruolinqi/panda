library(shiny)
library(shinydashboard) 
library(stringr)
library(ggplot2)
library(dplyr)
library(leaflet)
library(DT)
library(plotly)
library(dplyr)
library(rgdal)
library(tigris)
library(httr)
library(broom)
library(webr)


ui <- dashboardPage(
  dashboardHeader(title = "NYC 2021 Sales"),
  dashboardSidebar(
    sidebarMenu(
      
      menuItem("Property Map", tabName = "tabs", icon = icon("map-o"),startExpanded = TRUE,
        menuSubItem("Residential Units", icon = icon("map-o"), tabName = "tab1"),
        menuSubItem("Commercial Units", icon = icon("map-o"), tabName = "tab2")),
      
      menuItem("Property Sale", tabName = "donuts", icon = icon("chart-pie"), startExpanded = TRUE,
        menuSubItem("Manhattan Distribution", icon = icon("chart-pie"), tabName = "pie1"),
        menuSubItem("Bronx Distribution", icon = icon("chart-pie"), tabName = "pie2"),
        menuSubItem("Brooklyn Distribution", icon = icon("chart-pie"), tabName = "pie3"),
        menuSubItem("Queens Distribution", icon = icon("chart-pie"), tabName = "pie4")
        ),
      
      menuItem("Property Status", tabName = "tabs", icon = icon("map-o"), startExpanded = TRUE,
               menuSubItem("Property Age", icon = icon("map-o"), tabName = "bar"),
               menuSubItem("Property Square Feet", icon = icon("map-o"), tabName = "line")
               )
    )
  ),
  
  dashboardBody(
    tabItems(
      tabItem(tabName='tab1',
              checkboxInput("resid_price", label = "Show Price", value = FALSE),
              leafletOutput("map1", width="100%", height = "680px")),
      tabItem(tabName='tab2',
              checkboxInput("comm_price", label = "Show Price", value = FALSE),
              leafletOutput("map2", width="100%", height = "680px")),
      tabItem(tabName= 'pie1',
              plotOutput("donut1", height = 680)),
      tabItem(tabName= 'pie2',
              plotOutput("donut2", height = 680)),
      tabItem(tabName= 'pie3',
              plotOutput("donut3", height = 680)),
      tabItem(tabName= 'pie4',
              plotOutput("donut4", height = 680)),
      tabItem(tabName= 'bar',
              selectInput(inputId = "borough",label = "Borough",
                          choices = c(1,2,3,4,5)),
              plotOutput("barplot")),
      tabItem(tabName= 'line',
              selectInput(inputId = "Borough",label = "Borough",
                          choices = c(1,2,3,4,5)),
              plotOutput("linechart"))
  )
))


server <- function(input, output, session) {
  
  nyc_sales <- read.csv("nyc_sales.csv")
  
  nyc_sales <- na.omit(nyc_sales)
  
  nyc_tracts <- tracts(state = '36', county = c('061','047','081','005','085'))
  r <- GET('http://data.beta.nyc//dataset/0ff93d2d-90ba-457c-9f7e-39e47bf2ac5f/resource/35dd04fb-81b3-479b-a074-a27a37888ce7/download/d085e2f8d0b54d4590b1e7d1f35594c1pediacitiesnycneighborhoods.geojson')
  nyc_neighborhoods <- readOGR(content(r,'text'), 'OGRGeoJSON', verbose = F)
  nyc_neighborhoods_df <- tidy(nyc_neighborhoods)
  
  
  output$map1 <- renderLeaflet({
    if(input$resid_price == FALSE){
      points <- nyc_sales %>% select(Longitude, Latitude, RESIDENTIAL.UNITS) %>% rename(lng = Longitude, lat = Latitude)
      points_spdf <- points
      coordinates(points_spdf) <- ~lng + lat
      proj4string(points_spdf) <- proj4string(nyc_neighborhoods)
      matches <- over(points_spdf, nyc_neighborhoods)
      points <- cbind(points, matches)
      points_by_neighborhood <- points %>%
        group_by(neighborhood) %>%
        summarize(num_resid=sum(RESIDENTIAL.UNITS))
      map_data <- geo_join(nyc_neighborhoods, points_by_neighborhood, "neighborhood", "neighborhood")
      pal = colorBin("Blues", domain = range(map_data@data[["num_resid"]], na.rm=T))
      leaflet(map_data) %>%
        addProviderTiles("CartoDB.Positron") %>%
        addPolygons(fillColor = ~pal(num_resid), 
                    popup = ~paste(neighborhood, ":", num_resid), 
                    color = "black", 
                    weight = 1,
                    stroke = TRUE,
                    opacity = 1,
                    smoothFactor = 0.5,
                    fillOpacity = 0.9) %>% 
        addLegend(values=~num_resid,pal=pal,title="Residential Units Sold") %>%
        setView(-73.98, 40.75, zoom = 11)
    } else {
      points_price <- nyc_sales %>% select(Longitude, Latitude, RESIDENTIAL.UNITS,TOTAL.UNITS, SALE.PRICE) %>% rename(lng = Longitude, lat = Latitude, price = SALE.PRICE)
      points_price <- points_price %>% filter(price != 0 & RESIDENTIAL.UNITS > 0) 
      points_price_spdf <- points_price
      coordinates(points_price_spdf) <- ~lng + lat
      proj4string(points_price_spdf) <- proj4string(nyc_neighborhoods)
      matches <- over(points_price_spdf, nyc_neighborhoods)
      points_price <- cbind(points_price, matches)
      price_by_neighborhood <- points_price %>%
        group_by(neighborhood) %>%
        summarize(med_price = median(price/TOTAL.UNITS))
      price_data <- geo_join(nyc_neighborhoods, price_by_neighborhood, "neighborhood", "neighborhood")
      qpal = colorBin("Reds", domain = range(price_data@data$med_price, na.rm=T))
      
      leaflet(price_data) %>%
        addProviderTiles("CartoDB.Positron") %>%
        addPolygons(fillColor = ~qpal(med_price), 
                    popup = ~paste(neighborhood,":","$", med_price),
                    color = "black", 
                    weight = 1,
                    stroke = TRUE,
                    opacity = 1,
                    smoothFactor = 0.5,
                    fillOpacity = 1) %>% 
        addLegend(values=~med_price,pal=qpal,title="Price") %>%
        setView(-73.98, 40.75, zoom = 11)
    }
    
    
  })
  output$map2 <- renderLeaflet({
    if(input$comm_price == FALSE){
    points <- nyc_sales %>% select(Longitude, Latitude, COMMERCIAL.UNITS) %>% rename(lng = Longitude, lat = Latitude)
    points_spdf <- points
    coordinates(points_spdf) <- ~lng + lat
    proj4string(points_spdf) <- proj4string(nyc_neighborhoods)
    matches <- over(points_spdf, nyc_neighborhoods)
    points <- cbind(points, matches)
    points_by_neighborhood <- points %>%
      group_by(neighborhood) %>%
      summarize(num_comm=sum(COMMERCIAL.UNITS))
    map_data <- geo_join(nyc_neighborhoods, points_by_neighborhood, "neighborhood", "neighborhood")
    pal = colorBin("Greens", domain = range(map_data@data[["num_comm"]], na.rm=T))
    leaflet(map_data) %>%
      addProviderTiles("CartoDB.Positron") %>%
      addPolygons(fillColor = ~pal(num_comm), 
                  popup = ~paste(neighborhood, ":", num_comm), 
                  color = "black", 
                  weight = 1,
                  stroke = TRUE,
                  opacity = 1,
                  smoothFactor = 0.5,
                  fillOpacity = 0.9) %>% 
      addLegend(values=~num_comm,pal=pal,title="Commercial Units Sold") %>%
      setView(-73.98, 40.75, zoom = 11)
    } else {
      points_price <- nyc_sales %>% select(Longitude, Latitude, COMMERCIAL.UNITS, TOTAL.UNITS,SALE.PRICE) %>% rename(lng = Longitude, lat = Latitude, price = SALE.PRICE)
      points_price <- points_price %>% filter(price != 0 & COMMERCIAL.UNITS > 0) 
      points_price_spdf <- points_price
      coordinates(points_price_spdf) <- ~lng + lat
      proj4string(points_price_spdf) <- proj4string(nyc_neighborhoods)
      matches <- over(points_price_spdf, nyc_neighborhoods)
      points_price <- cbind(points_price, matches)
      points_price <- points_price[-c(1054,2747,1153,3501:3517,3736:3743,3813),]
      price_by_neighborhood <- points_price %>%
        group_by(neighborhood) %>%
        summarize(med_price = median(price/TOTAL.UNITS))
      price_data <- geo_join(nyc_neighborhoods, price_by_neighborhood, "neighborhood", "neighborhood")
      qpal = colorBin("Purples", domain = range(price_data@data$med_price, na.rm=T))
      
      leaflet(price_data) %>%
        addProviderTiles("CartoDB.Positron") %>%
        addPolygons(fillColor = ~qpal(med_price), 
                    popup = ~paste(neighborhood,":","$", med_price),
                    color = "black", 
                    weight = 1,
                    stroke = TRUE,
                    opacity = 1,
                    smoothFactor = 0.5,
                    fillOpacity = 1) %>% 
        addLegend(values=~med_price,pal=qpal,title="Price") %>%
        setView(-73.98, 40.75, zoom = 11)
    }
    
  })
  

  output$donut1 <- renderPlot({
    # mht donutchart
    bor_ngbh <- nyc_sales %>%
      group_by(BOROUGH, NEIGHBORHOOD) %>%
      summarise(cnt = n())
    
    mht_bor_ngbh <- bor_ngbh %>% 
      filter(BOROUGH == 1) %>% 
      mutate(ranking = dense_rank(desc(cnt))) %>% 
      arrange(ranking)
    
    sum(mht_bor_ngbh$cnt[11:nrow(mht_bor_ngbh)])
    
    mht_bor_ngbh[11,"cnt"] <- sum(mht_bor_ngbh$cnt[11:nrow(mht_bor_ngbh)])
    
    mht_bor_ngbh[11, "NEIGHBORHOOD"] <- "OTHERS"
    
    mht_bor_ngbh <- mht_bor_ngbh[1:11,]
    
    mht_bor_ngbh <- mht_bor_ngbh %>% mutate(perc = cnt/sum(cnt),
                                            ymax = cumsum(perc),
                                            ymin = ifelse(is.na(lag(ymax)),0, lag(ymax)))
    
    mht_bor_ngbh <- mht_bor_ngbh %>% select(-c("ranking"))
    
    p1 <- ggplot(mht_bor_ngbh) +
      geom_rect(aes(ymin = ymin, ymax = ymax, fill = NEIGHBORHOOD, xmax = 4, xmin = 2)) +
      coord_polar(theta = "y", start = 5) +
      theme_void() +
      xlim(c(-1,5)) +
      geom_text(aes(x = 5, y = perc, label = paste0(NEIGHBORHOOD, "\n", round(perc*100, 1), "%")), 
                position = position_stack(vjust = 0.5), size = 4)
    
    
    p1
  })
  
  output$donut2 <- renderPlot({
    # brx donutchart
    bor_ngbh <- nyc_sales %>%
      group_by(BOROUGH, NEIGHBORHOOD) %>%
      summarise(cnt = n())
    
    mht_bor_ngbh <- bor_ngbh %>% 
      filter(BOROUGH == 2) %>% 
      mutate(ranking = dense_rank(desc(cnt))) %>% 
      arrange(ranking)
    
    sum(mht_bor_ngbh$cnt[11:nrow(mht_bor_ngbh)])
    
    mht_bor_ngbh[11,"cnt"] <- sum(mht_bor_ngbh$cnt[11:nrow(mht_bor_ngbh)])
    
    mht_bor_ngbh[11, "NEIGHBORHOOD"] <- "OTHERS"
    
    mht_bor_ngbh <- mht_bor_ngbh[1:11,]
    
    mht_bor_ngbh <- mht_bor_ngbh %>% mutate(perc = cnt/sum(cnt),
                                            ymax = cumsum(perc),
                                            ymin = ifelse(is.na(lag(ymax)),0, lag(ymax)))
    
    mht_bor_ngbh <- mht_bor_ngbh %>% select(-c("ranking"))
    
    p1 <- ggplot(mht_bor_ngbh) +
      geom_rect(aes(ymin = ymin, ymax = ymax, fill = NEIGHBORHOOD, xmax = 4, xmin = 2)) +
      coord_polar(theta = "y", start = 5) +
      theme_void() +
      xlim(c(-1,5)) +
      geom_text(aes(x = 5, y = perc, label = paste0(NEIGHBORHOOD, "\n", round(perc*100, 1), "%")), 
                position = position_stack(vjust = 0.5), size = 4)
    
    
    p1
  })
  
  output$donut3 <- renderPlot({
    # brk donutchart
    bor_ngbh <- nyc_sales %>%
      group_by(BOROUGH, NEIGHBORHOOD) %>%
      summarise(cnt = n())
    
    mht_bor_ngbh <- bor_ngbh %>% 
      filter(BOROUGH == 3) %>% 
      mutate(ranking = dense_rank(desc(cnt))) %>% 
      arrange(ranking)
    
    sum(mht_bor_ngbh$cnt[11:nrow(mht_bor_ngbh)])
    
    mht_bor_ngbh[11,"cnt"] <- sum(mht_bor_ngbh$cnt[11:nrow(mht_bor_ngbh)])
    
    mht_bor_ngbh[11, "NEIGHBORHOOD"] <- "OTHERS"
    
    mht_bor_ngbh <- mht_bor_ngbh[1:11,]
    
    mht_bor_ngbh <- mht_bor_ngbh %>% mutate(perc = cnt/sum(cnt),
                                            ymax = cumsum(perc),
                                            ymin = ifelse(is.na(lag(ymax)),0, lag(ymax)))
    
    mht_bor_ngbh <- mht_bor_ngbh %>% select(-c("ranking"))
    
    p1 <- ggplot(mht_bor_ngbh) +
      geom_rect(aes(ymin = ymin, ymax = ymax, fill = NEIGHBORHOOD, xmax = 4, xmin = 2)) +
      coord_polar(theta = "y", start = 5) +
      theme_void() +
      xlim(c(-1,5)) +
      geom_text(aes(x = 5, y = perc, label = paste0(NEIGHBORHOOD, "\n", round(perc*100, 1), "%")), 
                position = position_stack(vjust = 0.5), size = 4)
    
    
    p1
  })
  
  output$donut4 <- renderPlot({
    # qns donutchart
    bor_ngbh <- nyc_sales %>%
      group_by(BOROUGH, NEIGHBORHOOD) %>%
      summarise(cnt = n())
    
    mht_bor_ngbh <- bor_ngbh %>% 
      filter(BOROUGH == 4) %>% 
      mutate(ranking = dense_rank(desc(cnt))) %>% 
      arrange(ranking)
    
    sum(mht_bor_ngbh$cnt[11:nrow(mht_bor_ngbh)])
    
    mht_bor_ngbh[11,"cnt"] <- sum(mht_bor_ngbh$cnt[11:nrow(mht_bor_ngbh)])
    
    mht_bor_ngbh[11, "NEIGHBORHOOD"] <- "OTHERS"
    
    mht_bor_ngbh <- mht_bor_ngbh[1:11,]
    
    mht_bor_ngbh <- mht_bor_ngbh %>% mutate(perc = cnt/sum(cnt),
                                            ymax = cumsum(perc),
                                            ymin = ifelse(is.na(lag(ymax)),0, lag(ymax)))
    
    mht_bor_ngbh <- mht_bor_ngbh %>% select(-c("ranking"))
    
    p1 <- ggplot(mht_bor_ngbh) +
      geom_rect(aes(ymin = ymin, ymax = ymax, fill = NEIGHBORHOOD, xmax = 4, xmin = 2)) +
      coord_polar(theta = "y", start = 5) +
      theme_void() +
      xlim(c(-1,5)) +
      geom_text(aes(x = 5, y = perc, label = paste0(NEIGHBORHOOD, "\n", round(perc*100, 1), "%")), 
                position = position_stack(vjust = 0.5), size = 4)
    
    
    p1
  })
  
  output$barplot <- renderPlot({
    nyc_sales$age <- 2022- nyc_sales$YEAR.BUILT
    mht_sales <-
      nyc_sales %>% filter(BOROUGH == input$borough) %>% 
      group_by(BOROUGH, NEIGHBORHOOD) %>%
      summarise(med_age = median(age))
    
    ggplot(mht_sales, aes(reorder(NEIGHBORHOOD, -med_age, sum), y = med_age)) + 
      theme(axis.text.y=element_blank(), axis.ticks.y = element_blank(),
            panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank()) +
      geom_bar(stat = "identity", fill = "steelblue") +
      theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5)) +
      geom_text(aes(label=round(med_age, 0), vjust=-0.3)) +
      xlab("Neighborhood") +
      ylab("Median Property Age")
    
  })
  
  output$linechart <- renderPlot({
    sqft <- nyc_sales %>% 
      filter(BOROUGH == input$Borough, LAND.SQUARE.FEET != 0) %>% 
      group_by(BOROUGH, NEIGHBORHOOD) %>%
      summarise(med_sqft = median(LAND.SQUARE.FEET))
    
    ggplot(data=sqft, aes(x=NEIGHBORHOOD, y=med_sqft, group = 1)) +
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank()) +
      geom_point() +
      theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5)) +
      geom_line() + 
      ylim(100, 30000) +
      xlab("Neighborhood") +
      ylab("Median Square Feet")
    
    
  })
}

shinyApp(ui = ui, server = server)
