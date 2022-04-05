

devtools::install_github("jcheng5/googleCharts")
library(googleCharts)
library(shiny)
library(dplyr)

dat <- readRDS("healthexp.Rds")
data$Region <- as.factor(data$Region)
data<-na.omit(data)

# Use global max/min for axes so the view window stays
# constant as the user moves between years
xlim <- list(
  min = min(data$Fertility.Rate) - 1,
  max = max(data$Fertility.Rate) + 1
)
ylim <- list(
  min = min(data$Life.Expectancy),
  max = max(data$Life.Expectancy) + 3
)

ui <- fluidPage(
  # This line loads the Google Charts JS library
  googleChartsInit(),
  
  # Use the Google webfont "Source Sans Pro"
  tags$link(
    href=paste0("http://fonts.googleapis.com/css?",
                "family=Source+Sans+Pro:300,600,300italic"),
    rel="stylesheet", type="text/css"),
  tags$style(type="text/css",
             "body {font-family: 'Source Sans Pro'}"
  ),
  
  googleBubbleChart("chart",
                    width="100%", height = "475px",
                    # Set the default options for this chart; they can be
                    # overridden in server.R on a per-update basis. See
                    # https://developers.google.com/chart/interactive/docs/gallery/bubblechart
                    # for option documentation.
                    options = list(
                      fontName = "Source Sans Pro",
                      fontSize = 13,
                      # Set axis labels and ranges
                      hAxis = list(
                        title = "Fertility rate, total (births per woman)",
                        viewWindow = xlim
                      ),
                      vAxis = list(
                        title = "Life expectancy (years)",
                        viewWindow = ylim
                      ),
                      # The default padding is a little too spaced out
                      chartArea = list(
                        top = 50, left = 75,
                        height = "75%", width = "75%"
                      ),
                      # Allow pan/zoom
                      explorer = list(),
                      # Set bubble visual props
                      bubble = list(
                        opacity = 0.4, stroke = "none",
                        # Hide bubble label
                        textStyle = list(
                          color = "none"
                        )
                      ),
                      # Set fonts
                      titleTextStyle = list(
                        fontSize = 16
                      ),
                      tooltip = list(
                        textStyle = list(
                          fontSize = 12
                        )
                      )
                    )
  ),
  fluidRow(
    shiny::column(4, offset = 4,
                  sliderInput("year", "Year",
                              min = min(data$Year), max = max(data$Year),
                              value = min(data$Year), animate = TRUE)
    )
  )
)


server <- function(input, output, session) {
  
  # Provide explicit colors for regions, so they don't get recoded when the
  # different series happen to be ordered differently from year to year.
  # http://andrewgelman.com/2014/09/11/mysterious-shiny-things/
  defaultColors <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
  series <- structure(
    lapply(defaultColors, function(color) { list(color=color) }),
    names = levels(data$Region)[2:8]
  )
  
  yearData <- reactive({
    # Filter to the desired year, and put the columns
    # in the order that Google's Bubble Chart expects
    # them (name, x, y, color, size). Also sort by region
    # so that Google Charts orders and colors the regions
    # consistently.
    df <- data %>%
      filter(Year == input$year) %>%
      select(Country, Fertility.Rate, Life.Expectancy,
             Region, Population) %>%
      arrange(Region)
  })
  
  output$chart <- reactive({
    # Return the data and options
    list(
      data = googleDataTable(yearData()),
      options = list(
        title = sprintf(
          "Fertility rate vs. life expectancy, %s",
          input$year),
        series = series
      )
    )
  })
}

shinyApp(ui = ui, server = server)

         