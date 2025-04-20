library(leaflet)
library(sf)
library(stringr)
library(shiny)
library(shinydashboard)
library(tibble)
library(RColorBrewer)

# List of States

State <- sf::read_sf("https://rstudio.github.io/leaflet/json/us-states.geojson")
State <- State[c(-2, -12, -52), ]
State_zoom <- st_coordinates(State$geometry)
State_zoom <- data.frame(State_zoom)
colnames(State_zoom) = c("X", "Y", "L1", "L2", "L3")
Start <- State$name
lat <- Start[-1:-49]
lng <- Start[-1:-49]

for (i in 1:49) {
  lng[i] <- mean(State_zoom[State_zoom$L3 == i, 1])
  lat[i] <- mean(State_zoom[State_zoom$L3 == i, 2])
}

lat <- as.numeric(lat)
lng <- as.numeric(lng)
State_final <- data.frame(State$name,
                          as.numeric(lng),
                          as.numeric(lat),
                          Pop,
                          Count_daily,
                          Count_yearly)
colnames(State_final) <- c(
  "name",
  "Lng",
  "Lat",
  "Population",
  "Daily Chuck (tons/day)",
  "Yearly Chuck (tons/year)"
)

# Woodchuck population by state

Pop <- c(
  255000,
  0,
  250000,
  0,
  0,
  50000,
  52000,
  78000,
  0,
  370000,
  12000,
  150000,
  13000,
  156000,
  106000,
  303000,
  110000,
  125000,
  310000,
  115000,
  66000,
  135000,
  208000,
  121000,
  13000,
  0,
  0,
  160000,
  112000,
  0,
  131000,
  217000,
  0,
  65000,
  201000,
  0,
  740000,
  210000,
  204000,
  0,
  261000,
  155000,
  0,
  280000,
  206000,
  0,
  560000,
  83000,
  0
)

Pop_k <- as.numeric(Pop) / 1000

State$density <- Pop_k

Pop_lab <- Pop[-1:-length(Pop)]

for (i in 1:length(Pop)) {
  if (isTRUE(Pop[i] == 0))
    Pop_lab[i] <- paste(Pop[i])
  else
    Pop_lab[i] <- paste(Pop[i] / 1000, "K")
}

Daily <- (((Pop * 8.6) * .2) * .1) / 2000
Count_daily <- str_c(prettyNum(Daily, big.mark = ","), "K", sep = " ")

Yearly <- (((Pop * 8.6) * .2) * 365) / 2000
Count_yearly <- str_c(prettyNum(Yearly, big.mark = ","), "K", sep = " ")

measure <- data.frame(State$name, Pop_lab, State$geometry, Count_daily, Count_yearly)
colnames(measure) <- c("State",
                       "density",
                       "geometry",
                       "Daily Chuck (tons/day)",
                       "Annual Chuck (tons/year)")

# Build the app

ui <- dashboardPage(
  
  dashboardHeader(title = "The Chuck Tracker", titleWidth = 400),
  
  dashboardSidebar(width = 250, sidebarMenu(
    selectInput("State", "Select a State", choices = c(" ", State$name)),
    actionButton("reset", "Reset the Map")
  )),
  
  # Show a plot of the generated distribution
  dashboardBody(
    leafletOutput("map", height = 400),
    h3(
      "Welcome to the Chuck Tracker! This site is dedicated to the accurate estimation
       of the age old question of woodchuck wood chucking quantities... or it's a shameless 
       demonstration of my map development and deployment abilites using Leaflet in combination 
       with Shiny Dashboard - whichever perspective you choose, you will become quite educated
       in the IMAGINARY sport of woodchuckery!"
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  observeEvent(input$reset, {
    updateSelectInput(session, "State", "Select a State", choices = c(" ", State$name))
    
    leafletProxy('map') %>%
      clearPopups() %>%
      setView(-96, 37.8, 4, zoom = 4)
    
  })
  
  bins <- c(0, 10, 20, 50, 100, 200, 500, 1000, Inf)
  pal <- colorBin("Purples", domain = State$density, bins = bins)
  
  output$map <- renderLeaflet(
    leaflet(State) %>%
      clearPopups() %>%
      addTiles() %>%
      setView(-96, 37.8, 4, zoom = 4) %>%
      addPolygons(
        fillColor = ~ pal(density),
        weight = 2,
        opacity = 1,
        color = "white",
        dashArray = "3",
        fillOpacity = 0.7,
        label = measure$State,
        popup = paste(
          "Population:",
          measure$density,
          "<br>",
          "Daily Chuck (tons/day):",
          measure$`Daily Chuck (tons/day)`,
          "<br>",
          "Annual Chuck (tons/year):",
          measure$`Annual Chuck (tons/year)`
        )
      )
  )
  
  observeEvent(input$State != " ", {
    leafletProxy('map') %>%
      clearPopups() %>%
      setView(
        lat = as.numeric(State_final[State_final$name == input$State, ][3]),
        lng =  as.numeric(State_final[State_final$name == input$State, ][2]),
        zoom = 7
      ) %>%
      addPopups(
        lat = as.numeric(State_final[State_final$name == input$State, ][3]),
        lng =  as.numeric(State_final[State_final$name == input$State, ][2]),
        measure[measure$State == input$State, ],
        popup = paste(
          "Population:",
          measure[measure$State == input$State, ][2],
          "<br>",
          "Daily Chuck (tons/day):",
          measure[measure$State == input$State, ][4],
          "<br>",
          "Annual Chuck (tons/year):",
          measure[measure$State == input$State, ][5]
        )
      )
  })
}


# Run the application
shinyApp(ui = ui, server = server)
