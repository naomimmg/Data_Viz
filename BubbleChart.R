
##dataset downloaded from The World Bank (world development indicators dataset, original source: United Nations Population Division)
##https://databank.worldbank.org/source/world-development-indicators
#x-axis: The number of children that would be born to a woman if she were to live to the end of her childbearing years and bear children in accordance with age-specific fertility rates of the specified year
#y-axis: The number of years a newborn infant would live if prevailing patterns of mortality at the time of its birth were to stay the same throughout its life.

devtools::install_github("jcheng5/googleCharts")
library(googleCharts)
library(shiny)

data <- read.csv("newdat.csv")
data$Region <- as.factor(data$Region)
data<-na.omit(data)

xlim <- list(
  min = min(data$Fertility.Rate) - 1,
  max = max(data$Fertility.Rate) + 1
)
ylim <- list(
  min = min(data$Life.Expectancy),
  max = max(data$Life.Expectancy) + 3
)

ui <- fluidPage(
  googleChartsInit(), #initializing
  #change Google font
  tags$link(
    href=paste0("http://fonts.googleapis.com/css?",
                "family=Source+Sans+Pro:300,600,300italic"),
    rel="stylesheet", type="text/css"),
  tags$style(type="text/css",
             "body {font-family: 'Source Sans Pro'}"
  ),
  #for more stylistic options see https://developers.google.com/chart/interactive/docs/gallery/bubblechart
  googleBubbleChart("chart",
                    width="100%", height = "475px",
                    options = list(
                      fontName = "Source Sans Pro",
                      fontSize = 13,
                      hAxis = list(
                        title = "Fertility rate, total (births per woman)",
                        viewWindow = xlim
                      ),
                      vAxis = list(
                        title = "Life expectancy (years)",
                        viewWindow = ylim
                      ),
                      chartArea = list(
                        top = 50, left = 75,
                        height = "75%", width = "75%"
                      ),
                      explorer = list(),
                      bubble = list(
                        opacity = 0.4, stroke = "none",
                        textStyle = list(
                          color = "none"
                        )
                      ),
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
  defaultColors <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7") #colours for regions
  series <- structure(
    lapply(defaultColors, function(color) { list(color=color) }),
    names = levels(data$Region)[2:8]
  )
  
  yearData <- reactive({
    df <- data %>%
      filter(Year == input$year) %>%
      select(Country, Fertility.Rate, Life.Expectancy,
             Region, Population) %>%
      arrange(Region)
  })
  
  output$chart <- reactive({
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

         