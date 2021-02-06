#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggthemes)
library(tidyverse)
library(ggplot2)
library(plotly)


theme_proj = theme_minimal(base_family = "Palatino") + 
    theme(plot.title.position = "plot")

robbery <- read.csv("robbery.csv")
assault <- read.csv("assault.csv")
sexualviolence <- read.csv("sexualviolence.csv")

regions = assault$Region

ui <- fluidPage(
    
    # Application title
    titlePanel("Looking at Rate by Region (make a selection)"),
    
    # Sidebar layout with a input and output definitions
    sidebarLayout(
        # Inputs: Select variables to plot
        sidebarPanel(
            
            selectInput(inputId = "crime",
                        label = "Type of Crime: ",
                        choices = c("Assault" = "assault", 
                                    "Robbery" = "robbery", "Sexual Violence" = "sexualviolence"),
                        selected = "assault",
                        multiple = FALSE),
            
            # Select variable for location
            selectInput(inputId = "reg", 
                        label = "Region:",
                        choices = regions, 
                        selected = "Africa",
                        multiple = FALSE),
            
            uiOutput("sub")
        ),
        
        #Output
        
        mainPanel(
            
            # Show plot
            plotlyOutput(outputId = "plot"),
            br(),        # a little bit of visual separation
            
        )
    )
)
# Define server function --------------------------------------------
server <- function(input, output) {
    
    
    
    type <- reactive({
        req(input$crime)
        if(input$crime == "assault"){assault}
        else if (input$crime == "robbery"){robbery}
        else {sexualviolence} 
    })
    
    output$sub <- renderUI({
        req(input$reg)
        this_region <- if(input$crime == "assault"){assault %>% filter(Region == input$reg)}
        else if (input$crime == "robbery"){robbery %>% filter(Region == input$reg)}
        else {sexualviolence %>% filter(Region == input$reg)} 
        
        sub_this_location <- sort(unique(this_region$Subregion))
        
        selectInput(inputId = "sub", label = "Sub Region:", choices = sub_this_location, 
                    selected = "Middle Africa", multiple = FALSE)
    })
    
    # Creates plot object the plotOutput function is expecting
    output$plot <- renderPlotly({
        
        #Creates base plot 
        
        Region_of_Interest = input$reg
        Sub_of_Interest = input$sub
        
        plot_data <- type() %>% 
            filter(Region == Region_of_Interest) %>%
            filter(Subregion == Sub_of_Interest) 
        
        p3 <- ggplot(data = plot_data,aes(x = Year, y = as.numeric(Rate), 
                                          color = Country)) + 
            geom_line(aes(text = Country)) +
            geom_point(aes(text = Country), 
                       size = 3, alpha = 0.8) +
            theme_proj +
            #need to add gridlines because tufte removes them
            theme(panel.grid.major = element_line(color = "gray90"),
                  panel.grid.minor = element_line(color = "gray90"),
                  legend.position = "none") +
            labs(x = "Year", y = "Assault rate (per 100,000 population)",
                 caption = "Source: UNOCD Serious Assault Crime Data")
        
        
        
        ggplotly(p3, tooltip = "text")
    })
    
    
}
# Create the Shiny app object ---------------------------------------
shinyApp(ui, server)
