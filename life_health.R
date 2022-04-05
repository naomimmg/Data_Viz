#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
library(SimDesign)
library(shinyjs)

# Define UI for application that draws a histogram
ui <- fluidPage(useShinyjs(),
                fluidRow(
                    column(width=8, align="center",
                           tabsetPanel(id="tabs",
                                       tabPanel(title = '', value = "intro",
                                                fluidRow(

    # Sidebar with a slider input for number of bins 
   


        # Show a plot of the generated distribution
            br(),
            br(),
            br(),
            br(),
            p(id = "greetings", "Welcome!! To start the study, press 'Next'"),
            plotOutput("plot0"),
            plotOutput("plot1"),
            plotOutput("plot2"),
            plotOutput("plot3"),
            plotOutput("plot4"),
        

        sliderInput("cor0", label = p("Estimate the correlation", em("r")),
                    min = -1, max = 1,
                    value = 0, step = 0.05),
        sliderInput("ask", label = p("What is the smallest meaningful association?"),
                    min = -1, max = 1,
                    value = 0, step = 0.05),
        numericInput("perc", "What percent is the TARGET region of the total?", 0),
        sliderInput("tar", label = p("At what percent does the TARGET region become predominant?"),
                    min = 0, max = 100,
                    value = 0, step = 1),
        sliderInput("pie", label = p("At what percent does the TARGET region become predominant?"),
                    min = 0, max = 100,
                    value = 0, step = 1),
        tags$script(HTML("
        $(document).ready(function() {setTimeout(function() {
          supElement = document.getElementById('tar').parentElement;
          $(supElement).find('span.irs-single, span.irs-from, span.irs-to').remove();
        }, 100);})
      ")),
        actionButton("button", "Next")
        
    
))))))

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    counter <- reactiveValues(countervalue=0)
    observeEvent(input$button, {
        counter$countervalue <- counter$countervalue + 1
    })

    df<-reactive( {
        list_cor0 = c(-1, -0.95, -0.9, -0.85, -0.8, -0.75, -0.7, -0.65, -0.6, -0.55, -0.5, -0.45, -0.4, -0.35, -0.3, -0.25, -0.2, -0.15, -0.1, -0.05, 0, 0.05, 0.1, 0.15, 0.2, 0.25, 0.3, 0.35, 0.4, 0.45, 0.5, 0.55, 0.6, 0.65, 0.7, 0.75, 0.8, 0.85, 0.9, 0.95, 1)
        r0 = sample(list_cor0, size = 1)
        
        
        xy0 <- data.frame(SimDesign::rmvnorm( #sample from a multivariate normal
            n=500, #how many points
            mean=c(0,0), #population mean
            sigma=matrix(c(1,r0,r0,1),2,2) #population covariance
        ))
        
        corr0<-cor(xy0$X1, xy0$X2)
        
        list(r0 = r0)
        
    })
    
    output$plot0 <- renderPlot({
        
        ggplot(data=df()$xy0,aes(xy0$X1,xy0$X2)) + geom_point(color="blue") + labs(x="X", y="Y") + theme_classic()
        
    })
    
    output$plot1 <- renderPlot({
        
        xy <- data.frame(rmvnorm( #sample from a multivariate normal
            n=500, #how many points
            mean=c(0,0), #population mean
            sigma=matrix(c(1,input$ask,input$ask,1),2,2) #population covariance
        ))
        
        ggplot(data=xy,aes(X1,X2)) + geom_point(color="blue") + labs(x="X", y="Y") + theme_classic()
        
    })
    
    output$plot2 <- renderPlot({
        
        dviz <- data.frame(
            group = c("TARGET", "proportion 1", "proportion 2"),
            value = c(25, 25, 50)
        )
        ggplot(dviz, aes(x="", y=value, fill=group))+
            geom_bar(width = 0.25, stat = "identity")
        
    })
    
    output$plot3 <- renderPlot({
        
        dviz <- data.frame(
            group = c("TARGET", "proportion 1", "proportion 2"),
            value = c(input$tar, 25, 50)
        )
        ggplot(dviz, aes(x="", y=value, fill=group))+
            geom_bar(width = 0.25, stat = "identity") + theme_void()
        
    })
    
    output$plot4 <- renderPlot({
        
        dviz <- data.frame(
            group = c("TARGET", "proportion 1", "proportion 2"),
            value = c(input$pie, 25, 50)
        )
        ggplot(dviz, aes(x="", y=value, fill=group))+
            geom_bar(width = 0.25, stat = "identity") +coord_polar("y", start=0)+theme_void()
        
    })
    
    observe({
        shinyjs::hide("plot0")
        shinyjs::hide("cor0")
        shinyjs::hide("plot1")
        shinyjs::hide("ask")
        shinyjs::hide("plot2")
        shinyjs::hide("perc")
        shinyjs::hide("plot3")
        shinyjs::hide("tar")
        shinyjs::hide("pie")
        shinyjs::hide("plot4")
        
        if(counter$countervalue==1) {
            shinyjs::show("plot0")
            shinyjs::show("cor0")
            shinyjs::hide("greetings")
        }
        
        if(counter$countervalue==2) {
            shinyjs::show("plot1")
            shinyjs::show("ask")
            shinyjs::hide("greetings")
        }
        
        if(counter$countervalue==3) {
            shinyjs::show("plot2")
            shinyjs::show("perc")
            shinyjs::hide("greetings")
        }
        
        if(counter$countervalue==4) {
            shinyjs::show("plot3")
            shinyjs::show("tar")
            shinyjs::hide("greetings")
        }
        
        if(counter$countervalue==5) {
            shinyjs::show("plot4")
            shinyjs::show("pie")
            shinyjs::hide("greetings")
        }
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
