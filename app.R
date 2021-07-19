library(shiny)
library(shinydashboard)
library(dplyr)
library(shinythemes)
library(ggplot2)
library(readxl)
library(googlesheets4)
library(sp)
library(rgdal)
library(leaflet)
library(RColorBrewer)
library(plotly)
library(sp)


if(grepl('marthclark', getwd())){
    setwd('/Users/marthaclark/Documents/DataLab/public_health/')
}

# read in data
uds <- read.csv('cleaned_udsm.csv')
fqhc <- read.csv('fqhc_data.csv')

# Make sure there's a column in both df's with the exact same name
 uds$state <- NULL
# uds$X <- NULL
# uds$usps <- NULL
# uds$county <- NULL
# fqhc$X <- NULL
# fqhc$city <- fqhc$state <- NULL
# names(uds)[2] = 'city'
# names(uds)[1] = 'zip_code'
# names(uds)[91] = 'county'
# uds$state <- NULL
# uds$X <- NULL
# uds$usps <- NULL
# uds$county <- NULL
# fqhc$X <- NULL
# fqhc$city <- fqhc$state <- NULL
# names(uds)[2] = 'city'
# names(uds)[1] = 'zip_code'
# names(uds)[91] = 'county'

uds$Post_Office_Name = tolower(uds$Post_Office_Name)
fqhc$City = tolower(fqhc$City)

names(uds) <- tolower(names(uds))
names(fqhc) <- tolower(names(fqhc))

uds <- uds %>% filter(state == 'TN')


# Use dplyr::left_join
# uds_fqhc <- dplyr::left_join(uds,fqhc,by= c('hcp_dominant_health_center_2019'= 'health.center.name'))
# uds_fqhc$state.y = NULL

health_data <- read_sheet('https://docs.google.com/spreadsheets/d/1uLIrv4xXrhZseOtRSscWtkuYJkeixcyIpB-i8A6d4ZU/edit?ts=60e89447#gid=658962581', sheet = 2)

shp <- readOGR('county_shape/')
# reproject shp
shp <- spTransform(shp, CRS("+proj=longlat +datum=WGS84"))

# View(shp@data)


# create list of percent variables
per_var <- c('Adult_Smoking', 'Adult_Obesity', 'Flu_Vaccinations', 'Fully_Covid_Vaccinated_2021', 'At_least_1_COVID_vaccine_dose_2021', 'Abortion_Rates_2013', 'Children_under_18', 'People_in_Rural_or_Isolated_settings', 'People_of_Color', 'Disabilities', 'Senior_Citizens', 'Physical Inactivity', 'White_Population_Percentage')

ui <- dashboardPage(
    dashboardHeader (title = "TN Public Health Data"),
    dashboardSidebar(
        sidebarMenu(
            menuItem(
                text="Map",
                tabName="map"),
            menuItem(
                text="Charts",
                tabName="charts"),
            menuItem(
                text = "UDS Mapper",
                tabName = 'uds_mapper'),
            menuItem(
                text = 'FQHC',
                tabName = 'fqhc'),
            menuItem(
                text = 'About',
                tabName = 'about')
            
            
        )
    ),
    dashboardBody(
        tags$head(
            tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
        ),
        tabItems(
            tabItem(
                tabName = 'map',
                
                fluidRow(
                    column(4,
                           selectInput(inputId = 'map_var',
                                       label = 'Choose a variable to plot',
                                       choices = names(health_data[,3:ncol(health_data)]),	
                                       selected = 'Health_Factors_Rankings'),
                           
                           selectInput(inputId = 'map_year',
                                       label = 'Choose a year to plot',
                                       choices = unique(health_data$Year),
                                       selected = '2019')
                    ),
                    
                    column(8,
                           leafletOutput('county_map')
                    )
                )
            ),
            
            tabItem(
                tabName="charts",
                fluidRow(
                    column(4,
                           selectInput(inputId = 'county_name',
                                       label = 'Choose a county',
                                       choices = health_data$County,
                                       multiple = TRUE,
                                       selected = health_data$County[1:10]),
                           selectInput(inputId = 'plot_var',
                                       label = 'Choose a variable to plot',
                                       choices = names(health_data[,3:ncol(health_data)]),	
                                       selected = 'HCP_Total_Patients_2019'),
                           selectInput(inputId = 'plot_year',
                                       label = 'Choose a year to plot',
                                       choices = unique(health_data$Year),
                                       selected = '2019')
                    ),
                    column(8, 
                           plotlyOutput('county_plot'))
                )
            ),
            tabItem(
                tabName = 'uds_mapper',
                fluidRow(
                    column(4,
                           selectInput(inputId = 'compare_to',
                                       label = 'Choose area to compare',
                                       choices = c('county', 'city', 'zip_code'),
                                       selected = 'county'),
                           uiOutput('ui_chosen'),
                           selectInput(inputId = 'uds_var',
                                       label = 'Choose a variable',
                                       choices = names(uds[, 4:ncol(uds)]))
                           
                    ),
                    column(8,
                           plotOutput('uds_plot'))
                )
                
            ),
            tabItem(
                tabName = 'fqhc',
                fluidPage(
                    fluidRow(
                        column(4,
                               
                               ),
                        column(8,
                               )
                    )
                )
            ),
            tabItem(
                tabName = 'about',
                fluidPage(
                    fluidRow(
                        div(img(src='logo_clear.png', align = "center"), style="text-align: center;"),
                        h4('Built in partnership with ',
                           a(href = 'http://datalab.sewanee.edu',
                             target='_blank', 'Sewanee Datalab'),
                           align = 'center'),
                        p('Empowering research and analysis through collaborative data science.', align = 'center'),
                        div(a(actionButton(inputId = "email", label = "clarkml0@sewanee.edu",
                                           icon = icon("envelope", lib = "font-awesome")),
                              href="mailto:clarkml0@sewanee.edu",
                              align = 'center')),
                        style = 'text-align:center;'
                    )
                )
            )
        )
    )
)

# Server
server <- function(input, output) {
    
    output$ui_chosen <- renderUI({
        ct <- input$compare_to
        if(ct == 'county'){
            selectInput(inputId = 'chosen',
                        label = 'Choose county',
                        choices = sort(unique(uds$name)),
                        multiple = TRUE,
                        selected = sort(unique(uds$name))[1:10])
            
        } else if(ct == 'city'){
            selectInput(inputId = 'chosen',
                        label = 'Choose city',
                        choices = unique(uds$post_office_name),
                        multiple = TRUE,
                        selected = sort(unique(uds$post_office_name))[1:10])
        } else {
            selectInput(inputId = 'chosen',
                        label = 'Choose zip code',
                        choices = unique(uds$zcta), 
                        multiple = TRUE,
                        selected = sort(unique(uds$zcta))[1:10])
        }
    })
    
    output$uds_plot <- renderPlot({
        ct <- input$compare_to
        chosen <- input$chosen
        uds_var <- input$uds_var
        
        save(ct, chosen, uds_var, uds, file = '/tmp/martha.RData')
        
        ok <- FALSE
        if(!is.null(chosen)){
            ok <- TRUE
        }
        if(ok){
            if(ct == 'county'){
                pd <- uds %>% filter(name %in% chosen) 
                g <- ggplot(pd, aes_string('county', uds_var)) 
                
            } else if(ct == 'city'){
                pd <- uds %>% filter(city %in% chosen)
                g <- ggplot(pd, aes_string('city', uds_var )) 
            } else {
                pd <- uds %>% filter(zcta %in% chosen)
                g <- ggplot(pd, aes_string('zip_code', uds_var)) 
            }    
            g + geom_bar(stat = 'identity',
                         fill= 'blue') +
                theme(axis.text.x = element_text(angle = 90))
        }
        
        
        
    })
    
    output$county_plot <- renderPlotly({
        cn <- input$county_name
        pv <- input$plot_var
        py <- input$plot_year
        pd <- health_data %>% filter(County %in% cn,
                                     Year == py)
        names(pd)[names(pd) == pv] <- 'value'
        
        if(pv %in% per_var){
            pd$value <- pd$value*100
            plot_text <- paste(
                "County: ", as.character(pd$County),"\n",
                gsub('_', ' ', pv), ' : ', paste0(round(pd$value, digits = 2), ' %'),  "\n",
                sep="") %>%
                lapply(htmltools::HTML)
        } else {
            plot_text <- paste(
                "County: ", as.character(pd$County),"\n",
                gsub('_', ' ', pv), ' : ', paste0(round(pd$value, digits = 2)),  "\n",
                sep="") %>%
                lapply(htmltools::HTML)
        }
        
        
        
        p <- ggplot(data = pd,
                    aes(x = County,
                        y = value, text = plot_text)) +
            geom_bar(stat = 'identity',
                     color = 'Blue',
                     fill = 'cornflowerblue') +
            labs(title = paste('Compare county health')) +
            ggthemes:: theme_pander()
        
        if(pv %in% per_var){
            p <- p + scale_y_continuous(labels = function(x) paste0(x, "%"))  
            
        } 
        p <- p + labs(y=gsub('_', ' ', pv)) + coord_flip() 
        ggplotly(p, tooltip = 'text') %>% layout(hoverlabel = list(bgcolor = 'white')) %>%
            config(displayModeBar = F)
    })
    
    output$county_map <- renderLeaflet({
        mv <- input$map_var
        my <- input$map_year
        pd <- health_data %>% filter(Year == my)
        
        shp@data <- left_join(shp@data, pd, by =c('COUNTY'= 'County'))

        names(shp@data)[names(shp@data) == mv] <- 'value'
        # map text
        map_palette <- colorNumeric(palette = brewer.pal(9, "Blues"), 
                                    domain=shp@data$value,
                                    na.color="black") 
        map_text <- paste(
            "County: ", as.character(shp@data$COUNTY),"<br/>",
            gsub('_', ' ', mv), ' : ', paste0(round(shp@data$value, digits = 2)),  "<br/>",
            sep="") %>%
            lapply(htmltools::HTML)
        
        leaflet(shp) %>%
            addProviderTiles('Esri.WorldShadedRelief') %>%
            addPolygons(
                color = 'black',
                fillColor = ~map_palette(value),
                stroke=TRUE,
                fillOpacity = 0.9,
                weight=1,
                label = map_text,
                highlightOptions = highlightOptions(
                    weight = 1,
                    fillColor = 'white',
                    fillOpacity = 1,
                    color = "white",
                    opacity = 1.0,
                    bringToFront = TRUE,
                    sendToBack = TRUE
                ),
                labelOptions = labelOptions(
                    noHide = FALSE,
                    style = list("font-weight" = "normal", padding = "3px 8px"),
                    textsize = "13px",
                    direction = "auto"
                )
            )  %>%  
            addLegend(pal=map_palette, title = '', 
                      values=~value, 
                      opacity=0.9, 
                      position = "bottomleft", 
                      na.label = "NA" )
        
        
        
    })
    
}

shinyApp(ui, server)