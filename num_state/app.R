library(flexdashboard)
library(shiny)
library(plotly)
library(tidyverse)
library(ggfortify)
library(ggthemes)
library(maps)
library(evaluate)


# get data
fire = read_csv("fire.csv")

fire_tidy = fire %>%
    drop_na(fips_name) %>% 
    filter(fire_year %in% c(2005, 2015))

state.abb = append(state.abb, c("DC", "PR"))
state.name = append(state.name, c("District of Columbia", "Puerto Rico"))

# Map the state abbreviations to state names so we can join with the map data
fire_region = fire_tidy %>% 
    mutate(region = map_chr(state, function(x) { tolower(state.name[grep(x, state.abb)]) }))

county_map = map_data('county')

# Define UI for application that draws a histogram
year = fire_region %>% distinct(fire_year) %>% pull()
region = fire_region %>% distinct(region)

ui = fluidPage(
    titlePanel("Number of wildfire"),
    
    sidebarLayout(
        sidebarPanel(
            helpText("Create demographic maps for each state with 
               fire data from 2005 and 2015."),
            
            
            selectInput("year_choice", label = h3("Choose year"),
                        choices = year,
                        selected = NULL),
            selectInput("region_choice", label = h3("Choose state"),
                        choices = region)
            
        ),
        
        mainPanel(
            plotlyOutput("map")
        )
    )
)

server = function(input, output) {
    
    output$map = renderPlotly({ 
        
        
        fire_region %>%
            filter(fire_year == input$year_choice,
                   region == input$region_choice) %>% 
            group_by(region, fire_year, subregion = tolower(fips_name)) %>%
            summarize(n = n()) %>% 
            right_join(county_map, by = c('region', 'subregion')) %>% 
            ggplot(aes(x = long, y = lat, group = group, fill = n, text = paste("County:", str_to_title(subregion)))) + 
            geom_polygon() + 
            geom_path(color = 'white', size = 0.1) + 
            scale_fill_continuous(low = "orange", 
                                  high = "darkred",
                                  name = 'Number of fires') + 
            theme_map() + 
            coord_map('albers', lat0=30, lat1=40) + 
            ggtitle("US Wildfires, 2005 and 2015") + 
            theme(plot.title = element_text(hjust = 0.5))
        
        
        
    })
    
}

shinyApp(ui, server)
