# 0.0 Libraries ----
library(shiny)
library(shinythemes)
library(tidyverse)
library(ggthemes)
library(thematic)

# 1.0 Pull Data ----
vax_df <- read_csv("data/country_vaccinations.csv") %>% 
    select(country, date, total_vaccinations, people_vaccinated, 
           people_fully_vaccinated, daily_vaccinations, total_vaccinations_per_hundred,
           people_vaccinated_per_hundred, people_fully_vaccinated_per_hundred, 
           daily_vaccinations_per_million)



# 2.0 User Interface ----
ui <- fluidPage( theme = shinytheme("yeti"),
    
    titlePanel("COVID-19 Global Vaccine Progress Comparator",
               windowTitle = "Global Vax Progress"
    ),
    
    # Sidebar 
    sidebarLayout(
        
        sidebarPanel(
        
            tags$h4("Select Inputs:"),
            selectizeInput(inputId = "countries", 
                           label = "Countries (up to 8):", 
                           choices = unique(vax_df$country), 
                           multiple = TRUE,
                           options = list(maxItems = 8, 
                                          placeholder = "Select up to 8 countries")),
            selectInput(inputId = "dv",
                        label = "Dependent Variable:",
                        choices = names(vax_df)[3:10],
                        selected = "total_vaccinations"),
            dateRangeInput(inputId = "daterange", 
                           label = "Adjust Date Range:",
                           start = min(vax_df$date),
                           end = max(vax_df$date),
                           min = min(vax_df$date),
                           max = max(vax_df$date)),
            tags$h6("Caution: data may be incomplete for some countries")
            
        ),
        
        mainPanel(
            
            #tags$h3(":"),
            plotOutput("plot"),
            height = 6
            
        ),
        
        fluid = TRUE
        
    ),
    
)



# 3.0 Server ----
server <- function(input, output) {
    
    output$plot <- renderPlot({
        
        plot_data <- { 
            vax_df %>% 
                select(date, country, input$dv) %>%
                filter(country %in% input$countries) %>%
                filter(between(date, input$daterange[1], input$daterange[2])) %>%
                rename(input$dv, plot_y = input$dv)
        }

        g <- ggplot(data = plot_data, 
                    aes(x = date, 
                        y = plot_y, 
                        color = country))
        
        g + geom_line(size = 1) +
            labs(title = "Vaccine Progress by Country",
                 x = "Date",
                 y = input$dv,
                 color = "Country",
                 caption = "Source: \"Our World in Data\" via Gabriel Preda: https://www.kaggle.com/gpreda/covid-world-vaccination-progress/")
        
    })
    
}

# Auto-theming with {thematic} allows for smooth integration of visuals
ggplot2::theme_set(ggthemes::theme_few())
thematic_shiny()

# Run the application 
shinyApp(ui = ui, server = server)
