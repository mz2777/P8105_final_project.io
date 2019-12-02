library(flexdashboard)
library(shiny)
library(plotly)
library(tidyverse)
library(ggfortify)
library(ggthemes)
library(maps)
library(mapproj)


# get data
fire = read_csv("fire.csv")

fire_tidy = fire %>%
    drop_na(fips_name)

state.abb = append(state.abb, c("DC", "PR"))
state.name = append(state.name, c("District of Columbia", "Puerto Rico"))

# Map the state abbreviations to state names so we can join with the map data
fire_region = fire_tidy %>% 
    mutate(region = map_chr(state, function(x) { tolower(state.name[grep(x, state.abb)]) }))

state_map = map_data('state')

# Define UI for application that draws a histogram
year = fire_region %>% distinct(fire_year) %>% pull()

ui = fluidPage(
    titlePanel("Number of wildfires"),
    
    sidebarLayout(
        sidebarPanel(
            helpText("Create demographic maps for US with 
               fire data from 2009 to 2015."),
            
            
            selectInput("year_choice", label = h3("Choose year"),
                        choices = year,
                        selected = NULL)
            
        ),
        
        mainPanel(
            plotlyOutput("map")
        )
    )
)

server = function(input, output) {
    
    output$map = renderPlotly({  
        
        fire_region %>%
            filter(fire_year == input$year_choice) %>% 
            group_by(region, fire_year) %>%
            summarize(n = n()) %>%
            right_join(state_map, by = 'region') %>%
            ggplot(aes(x = long, y = lat, group = group, fill = n, text = paste("State:", str_to_title(region)))) + 
            geom_polygon() + 
            geom_path(color = 'white') + 
            scale_fill_continuous(low = "orange", 
                                  high = "darkred",
                                  name = 'Number of fires') + 
            theme_map() + 
            coord_map('albers', lat0=30, lat1=40) + 
            ggtitle("US Wildfires, 2009-2015") + 
            theme(plot.title = element_text(hjust = 0.5))
        
    })
}   


shinyApp(ui, server)
