library(shiny)
library(leaflet)
library(opencage)

# Sample dataset with location IDs and locations
data <- data.frame(
  location_id = c(1, 44563, 44564, 44565, 44566, 44567, 44568),
  location = c("Global", "AFR", "AMR", "SEAR", "EUR", "EMR", "WPR")
)

# OpenCage Geocoder API key
api_key <- "172b62341d3247bfa86f02c22bcc46b0"  # Replace with your own API key

# Function to geocode location names
geocode_location <- function(location) {
  result <- opencage_forward(location, key = api_key)
  if (!is.null(result$results) && !is.null(result$results$geometry)) {
    return(result$results$geometry)
  } else {
    return(NULL)
  }
}

ui <- fluidPage(
  titlePanel("Location Map"),
  leafletOutput("map")
)

server <- function(input, output) {
  output$map <- renderLeaflet({
    leaflet(data) %>%
      addTiles() %>%
      observe({
        for (i in 1:nrow(data)) {
          location <- data[i, "location"]
          coordinates <- geocode_location(location)
          if (!is.null(coordinates)) {
            addMarkers(lng = coordinates$lng, lat = coordinates$lat, label = ~location)
          }
        }
      })
  })
}

shinyApp(ui = ui, server = server)
