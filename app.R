## SHINY
# https://oliver-f-anderson.shinyapps.io/Vacation_Planner/
# dependencies
library(shiny)
library(tidyverse)
library(colorspace)
library(RColorBrewer)
library(maps)
library(sf)
library(leaflet)
library(htmltools)
library(plotly)
library(ggpubr)
library(thematic)
library(shinythemes)

# reading in datasets
sun = read_csv("Cities by Sunshine Duration.csv")
one_star = read_csv("one-star-michelin-restaurants.csv")
two_stars = read_csv("two-stars-michelin-restaurants.csv")
three_stars = read_csv("three-stars-michelin-restaurants.csv")

# add stars columns
one_star <- one_star %>% 
  mutate(stars = 1)

two_stars <- two_stars %>% 
  mutate(stars = 2)

three_stars <- three_stars %>% 
  mutate(stars = 3)

# merge into one ds
michelin <- rbind(one_star, two_stars, three_stars)
unique(michelin$city)
michelin <- michelin %>% mutate(City = city)
sun_stars <- inner_join(michelin, sun, by = "City")
head(sun_stars)

# final merged dataset
sun_stars <- sun_stars %>% filter(name != "Mews")
sun_stars <- sun_stars[order(sun_stars$city),]
sunshine <- pivot_longer(sun_stars, Jan:Dec, names_to = "month", values_to = "temp")

sunshine$month<-as.factor(sunshine$month)
sunshine$month <- factor(sunshine$month, levels=base::c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))

# get countries and currencies in dataset
countryList <- unique(sun_stars$Country)
countryList <- as.vector(countryList)

currenciesList <- jsonlite::fromJSON("https://api.exchangerate.host/symbols")
currenciesList <- names(tibble::as_tibble(currenciesList$symbols))
currenciesList <- as.vector(currenciesList)

currency <- base::c("THB", "HUF", "USD", "GBP", "HKD", "MOP", "NOK", "KRW", "SGD", "SEK", "TWD", "EUR")
currencies <- tibble(countryList, currency)

currencies <- currencies %>% mutate("Country" = countryList) %>% select(-countryList)

# merge into last dataset for app
sun_stars <- inner_join(sun_stars, currencies, "Country")

# BASE LEAFLET
# getting world map data
mapWorld = map("world", fill = TRUE, plot = FALSE)

# creating popup for michelin star restaurants
mich_popup <- paste0(htmlEscape(sun_stars$name), '<br/>',
                     htmlEscape(sun_stars$city), ', ',
                     htmlEscape(sun_stars$Country),
                     '<br/>Michelin Stars: ', htmlEscape(sun_stars$stars),
                     '<br/>Type of Cuisine: ', htmlEscape(sun_stars$cuisine))

# SHINY UI
ui <- fluidPage(theme = shinytheme("superhero"),
   # title of app
  titlePanel(h1("Sun, Stars, and Currency: Your Guide to Exploring Cities")),
  # title of selection panel
  sidebarLayout(
       sidebarPanel(h4("Plan your perfect getaway: Use the sunshine graph to decide when to visit your selected city"),
                    # inputs for selection panel
                    selectInput("city",
                                label = h5("Choose a vaction spot!"),
                                choices = unique(sunshine$city),
                                selected = "San Diego"),
                    plotlyOutput("myPlot")
       ),
       # title of main panel
       mainPanel(
         h3("Culinary delights: Discover the best Michelin-starred restaurants in your chosen city"),
         leafletOutput("myMap"),
         h4("Travel smart: Get real-time currency conversion rates for your travel destination"),
         selectInput("base",
                     label = h5("Choose your base currency"),
                     choices = unique(currenciesList),
                     selected = "USD"),
         numericInput(
           "cash",
           label = h5("Input amount to convert"),
           value = 100.00,
           min = 0,
         ),
         textOutput("conversion"),
         p(""),
         p("Data from Kaggle.com: The Sunniest Cities in the World by user thedevastator"),
         p("Data from Kaggle.com: Michelin Restaurants by user jackywang529"),
         p("Currency exchange rates from exchangerate.host API")
       )
     )
)

# SHINY SERVER
server <- function(input, output, session) {
  
  # adding zoom function to leaflet: WORKS GREAT
  # reactive dataset
  filtered <- reactive({
    sun_stars[sun_stars$city == input$city,] %>% 
      group_by(city) %>% 
      summarize(latitude = mean(latitude),
                longitude = mean(longitude))
  })
  
  # REACTIVE LEAFLET
  output$myMap <- renderLeaflet({
    # generate leaflet
    leaflet(data = mapWorld) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addPolygons(fillColor = topo.colors(5, alpha = NULL), stroke = FALSE) %>%
      addMarkers(data = sun_stars, popup = ~mich_popup) %>%
      setView(lat=32.9, lng = -117.2001, zoom = 10)
  })
  
  # move and zoom to city in inputSelect
  myMap_proxy <- leafletProxy("myMap")
  
  observe({
    fdata <- filtered()
    myMap_proxy %>%
      clearMarkers() %>%
      addMarkers(data = sun_stars,
                 popup = ~mich_popup) %>%
      flyTo(lng = fdata$longitude, lat = fdata$latitude, zoom = 10)
  })
  
  # REACTIVE PLOTLY
  output$myPlot <- renderPlotly({
    
    # make the filter reactive
    filtered_data <- reactive({
      sunshine %>%
        filter(city == input$city) %>%
        group_by(city, month) %>%
        summarize(temp = mean(temp)) %>% 
        ungroup()
    })
    
    # create the plot
    sun_by_cities <- filtered_data()
    
    gg_sunshine <- sun_by_cities %>%  
      ggplot(aes(x = month, y=temp, text = paste0("Hours of sunshine in\n",input$city, ": ", temp, "hrs in ", month))) +
      geom_line(aes(x = month, y = temp, color = input$city, group = input$city), alpha=0.8, color = "#0454a4")+
      labs(x="Month", y="Hours of sunshine per month", color = "City", caption = "Data from Kaggle.com: uploaded by user thedevastator", title = paste0("Sunshine per month in ", input$city))+
      theme_minimal()+
      theme(
        plot.title = element_text(vjust = 2),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
    
    ggplotly(gg_sunshine, tooltip = 'text')
  })
  
  # filter currency to convert to based upon select input
  filt_curr <- reactive({
    convert <- sun_stars[sun_stars$city == input$city,] %>% 
      select(currency)
    convert<-convert[1,]
    convert <- as.vector(convert)
  })
  
  # query api for current exchange rate
  rate <- reactive({
    conv <- filt_curr()
    rate <- jsonlite::fromJSON(URLencode(paste0('https://api.exchangerate.host/convert?from=',input$base,'&to=',conv$currency)))
  })
  
  # output text
  output$conversion <- renderText({
    rate_conv <- rate()
    conv <- filt_curr()
    paste(input$cash * rate_conv$result, conv$currency)
  })
}

shinyApp(ui, server)



