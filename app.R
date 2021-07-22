# Necessary libraries
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
library(DT)
library(RColorBrewer)
library(plotly)
library(sp)
library(Hmisc)
library(stringr)

if(grepl('marthaclark', getwd())){
  setwd('/Users/marthaclark/Documents/DataLab/public_health-main//')
}

# Reading in datasets
uds <- read.csv('cleaned_udsm.csv')
fqhc <- read.csv('fqhc_data.csv')

# Make sure there's a column in both df's with the exact same name

# Changing all column names to lowercase
names(uds) <- tolower(names(uds))
names(fqhc) <- tolower(names(fqhc))

# recode counties in uds mapper so they are the same as the shp file counties
uds$county <- gsub(' County', '', uds$county)
uds <- uds %>% mutate(county = ifelse(county == 'DeKalb', 'Dekalb',
                                      ifelse(county == 'Van Buren', 'Van_Buren', county)))

per_var <- c('Adult_Smoking', 'Adult_Obesity', 'Flu_Vaccinations', 
             'Fully_Covid_Vaccinated_2021', 'At_least_1_COVID_vaccine_dose_2021', 
             'Abortion_Rates_2013', 'Children_under_18', 'People_in_Rural_or_Isolated_settings',
             'People_of_Color', 'Disabilities', 'Senior_Citizens', 'Physical Inactivity',
             'White_Population_Percentage', 'children....18.years.old.', 'adult..18...64.', 
             'older.adults..age.65.and.over.', 'racial.and.or.ethnic.minority', 
             'hispanic.latino.ethnicity', 'black.african.american', 'asian',
             'american.indian.alaska.native', 'native.hawaiian...other.pacific.islander',
             'more.than.one.race', 'best.served.in.another.language', 'medical',
             'dental', 'mental.health', 'substance.abuse', 'vision', 'enabling',
             'hypertension', 'diabetes', 'asthma', 'hiv', 
             "access.to.prenatal.care..first.prenatal.visit.in.1st.trimester.", 
             "low.birth.weight",  "cervical.cancer.screening", 
             "adolescent.weight.screening.and.follow.up", 
             "adult.weight.screening.and.follow.up", 
             "adults.screened.for.tobacco.use.and.receiving.cessation.intervention",
             "colorectal.cancer.screening", "childhood.immunization", 
             "depression.screening","dental.sealants",
             "asthma.treatment..appropriate.treatment.plan.", 
             "statin.therapy.for.the.prevention.and.treatment.of.cardiovascular.disease", 
             "heart.attack.stroke.treatment..aspirin.therapy.for.ischemic.vascular.disease.patients.",
             "blood.pressure.control..hypertensive.patients.with.blood.pressure...140.90.", 
             "uncontrolled.diabetes...9.", "hiv.linkage.to.care", 
             "patients.at.or.below.200..of.poverty", 
             "patients.at.or.below.100..of.poverty","uninsured", "medicaid.chip",
             "medicare", "other.third.party")

# Reading in googlesheet team manually worked on
health_data <- read_sheet('https://docs.google.com/spreadsheets/d/1uLIrv4xXrhZseOtRSscWtkuYJkeixcyIpB-i8A6d4ZU/edit?ts=60e89447#gid=658962581', sheet = 2)


# join health data with uds mapper data
uds$year <- 2019
# uds$source <- 'uds mapper'
# health_data$source <- 'data lab'


health_data <- left_join(health_data, uds, by = c('County'= 'county', 'Year'= 'year'))
# is.list(health_data)
# health_data <- as.list(health_data)
# health_data <- do.call(cbind, health_data)
# health_data <- tibble (health_data)

# for within county choices 
gross_numbers <- c("Total_Population","Health_Outcomes_Rankings","Quality_of_Life","Premature_Deaths","Health_Factors_Rankings","Alcohol_impaired_Driving_Deaths","Driving_Deaths","Uninsured","Number_of_Primary_Care_Physicians","Dentist_Providers","Mental_Health_Providers","Covid_Cases","Covid_Deaths","People_with_HIV","White_Population","Black_African_American_Population","American_Indian_and_Alaskan_Native_Population","Asian_Population","Native_Hawaiian_and_Other_Pacific_Islander_Population","Other_Race","Two_Or_More_Races","FQHCs_ and_LALs","Emergency_Dept","Hospital_Closures","Hospitals","Licensed_Mental_Health_Treatment_Sites","Licensed_Substance_Abuse_Treatment_Sites","Overdose_Deaths","tot_pop","num_fqhc","num_patients","pop_low_income","unserved_low_income","unserved_total_pop","unserved_uninsured","unserved_medicaid_public","unserved_medicaid_private")
# create two vectors of columns, one from health data and one from uds to identify source later
uds_cols <- names(uds)
health_data_cols <- names(health_data)

# create vector of choices for health data
remove_these <- 'County|Year|source|Total_Population|Alcohol_Impaired_Driving_Deaths|Driving_Deaths|Uninsured|Number_of_Primary_Care_Physicians|Dentist_Providers|Mental_Health_Providers|White_Population|White_Population_Percentage|Black_African_American_Population|Black_African_American_Population_percentage|American_Indian_and_Alaskan_Native_Population|American_Indian_and_Alaskan_Native_Population_Percentage|Asian_Population|Asian_Population_percentage|Native_Hawaiian_and_Other_Pacific_Islander_Population|Native_Hawaiian_and_Other_Pacific_Islander_Population_Percentage|Uninsured|People_of_Color|Other_Race|Two_Or_More_Races|Two_Or_More_Races_Percentage|x|first_fqhc|first_share|second_fqhc|second_share|third_fqhc|third_share|fourth_fqhc|fourth_share|fifth_fqhc|fifth_share|Disabilities|Children_under_18|Senior_Citizens'

# create choices
hd_choices <- health_data_cols[!grepl(remove_these, health_data_cols)]

# create label for each choice
hd_labels <- c('Health Outcomes Rankings', 'Quality of Life', 'Premature Deaths', 'Health Factors Rankings', 'Adult Smoking', 'Adult Obesity', 'Medicare Flu Vaccinations', 'Covid Cases', 'Covid Deaths', 'Covid Fully Vaccinated', 'One Covid Vaccine Dose', 'Abortion Rates (2013)', 'HIV', 'People in Rural/Isolated Settings', 'Physical Inactivity', 'Opioid Prescriptions per 100 People', 'FQHCs and LALs Present', 'FQHC:Cherokee or Ocoee', 'Nonprofit Clinics', 'Emergency Departments','Hospital Closures', 'Hospitals', 'Licensed Mental Health Treatment Sites', 'Licensed Substance Abuse Treatment Sites', 'Poverty Percentage', 'TennCare Percentage', 'Overdose Deaths', 'Rural/Urban', 'Total Population', 'Number of FQHCs', 'Number of Patients at HC', 'Low Income Population', 'Low Income HC Not Served', 'Total Population HC Not Served', 'Uninsured HC Not Served', 'Medicaid Public Insurance HC Not Served', 'Medicaid Private Insurance HC Not Served', 'Low Income Penetration at HC', 'Total Population Penetration at HC', 'Uninsured Penetration at HC', 'Medicaid Penetration at HC', 'Uninsured', 'Medicaid Public Insurance', 'Medicaid Private Insurance', 'Poverty Population', 'Minority Population', 'Hispanic Latino Population', 'African American Population', 'Asian Population', 'American Indian/Alaskan Native Population', 'Native Hawaiian Population', 'Other Pacific Islander Population', 'White Population', 'Uninsured Population', 'Medicaid Public Insurance Population', 'Medicaid Private Insurance Population', 'School Age Population', '65 and Up Population', 'Unemployed Population', 'Households with Limited English Population', 'Less than High School Education Population', 'Veteran Population', 'Disability Population', 'Uninsured Below 200 FPL Population')

# use setNames to combine choices and label
hd_choices <- setNames(hd_choices, hd_labels)

# Reading in map of TN shape file
shp <- readOGR('county_shape/')

# Reproject shp to show readable lat and long coordinates
shp <- spTransform(shp, CRS("+proj=longlat +datum=WGS84"))


ui <- dashboardPage(
  # Title of dashboard
  dashboardHeader (title = "TN Public Health Data"),
  dashboardSidebar(
    sidebarMenu(
      # Naming each side tab of dashboard
      menuItem(
        text="Data_Portal",
        tabName="data_portal"),
      menuItem(
        text="FQHC",
        tabName="fqhc"),
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
        
        #Creation of explore tab
        tabName = 'data_portal',
        tabsetPanel(
          tabPanel(title = 'Chart',
                   fluidPage(
                     fluidRow(
                       column(4,
                              selectInput('compare_county',
                                          label = 'Single or multiple counties',
                                          choices = c('Between counties', 'Within counties'),
                                          selected = 'Between counties')
                       ),
                       column(4, 
                              uiOutput('ui_per_gross'))
                     ),
                     
                     uiOutput('ui_county_comparison')
                   )
                   
          ),
          tabPanel(title='Map',
                   fluidPage(
                     fluidRow(
                       column(4,
                              
                              # Allows user to choose a health variable from health_data cols
                              selectInput(inputId = 'map_var',
                                          label = 'Choose a variable to plot',
                                          choices = hd_choices,	
                                          selected = 'Health_Factors_Rankings'),
                              
                              # Allows user to choose a year from health_data Year row       
                              selectInput(inputId = 'map_year',
                                          label = 'Choose a year to plot',
                                          choices = unique(health_data$Year),
                                          selected = '2019')
                       ),
                       column(8,
                              
                              # Places map next to selection inputs
                              leafletOutput('county_map'))
                       
                     )
                   )
                   
          )
          
        )
        
      ),
      
      
      tabItem(
        
        # Creation of fqhc tab
        tabName="fqhc",
        fluidRow(
          column(4,
                 # Allows user to choose a health senter from fqhc data
                 selectInput(inputId = 'health_center_name',
                             label = 'Choose a health center',
                             choices = fqhc$health.center.name,
                             multiple = TRUE,
                             selected = fqhc$health.center.name[1:5])
                 
                 
          ),
          column(4,
                 
                 # Allows user to choose what kind of fqhc variable to view in table
                 selectInput(inputId = 'variable_type',
                             label = 'Choose a type of variable',
                             choices = c('Demographics', 'Patient Characteristics', 
                                         'Services', 'Clinical', 'Cost'),
                             selected = 'Services')    
          )),
        fluidRow(
          column(12, 
                 # Places table
                 DT::dataTableOutput('fqhc_table')
                 
          )
        )
        
        
      ),
      
      tabItem(
        
        #Creation of about tab
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
  
  output$ui_per_gross <- renderUI({
    cc <- input$compare_county
    if(cc != 'Between counties'){
      selectInput('per_gross',
                  label = 'Choose percent or gross variables',
                  choices = c('Percent', 'Gross number'),
                  selected = 'Percent')
    }
    
  })
  output$ui_county_comparison <- renderUI({
    cc <- input$compare_county
    
    message(cc)
    if(cc == 'Between counties'){
      fluidRow(
        column(3,
               
               # Allows user to choose a county from health_data County row 
               # Shows first 10 counties by default
               selectInput(inputId = 'county_name',
                           label = 'Choose a county',
                           choices = health_data$County,
                           multiple = TRUE,
                           selected = health_data$County[1:10])),
        column(3,
               # Allows user to choose a health variable from health_data cols
               selectInput(inputId = 'plot_var',
                           label = 'Choose a variable to plot',
                           choices = hd_choices,	
                           selected = 'HCP_Total_Patients_2019')),
        column(3, 
               # Allows user to choose a year from health_data Year row       
               selectInput(inputId = 'plot_year',
                           label = 'Choose a year to plot',
                           choices = unique(health_data$Year),
                           selected = '2019')),
               
        fluidRow(
          column(12,
                 # Places graph next to selection inputs
                 plotlyOutput('county_plot'))
        )
        
      )
    } else {
      pg <- input$per_gross
      if(pg=='Gross number'){
        var_choices <- gross_numbers
      } else {
        var_choices<- names(health_data)[!names(health_data) %in% gross_numbers]
        var_choices <- var_choices[!var_choices %in% c('County', 'Year', 'FQHC_Cherokee_or_Ocoee', 'Nonprofit_clinics',"Rural_Urban","x","first_fqhc","first_share","second_fqhc","second_share","third_fqhc","third_share","fourth_fqhc","fourth_share","fifth_fqhc","fifth_share")]
      }
      fluidRow(
        column(3,
               
               # Allows user to choose a county from health_data County row 
               # Shows first 10 counties by default
               selectInput(inputId = 'county_name_2',
                           label = 'Choose a county',
                           choices = health_data$County,
                           multiple = TRUE)),
        column(3,
               
               # Allows user to choose a health variable from health_data cols
               selectInput(inputId = 'plot_var_2',
                           label = 'Choose a variable to plot',
                           choices = var_choices,	
                           selected = var_choices[1:4],
                           multiple = TRUE) ),
        column(3, 
               
               # Allows user to choose a year from health_data Year row       
               selectInput(inputId = 'plot_year_2',
                           label = 'Choose a year to plot',
                           choices = unique(health_data$Year),
                           selected = '2019')),
           
           
        fluidRow(
          column(12,
                 # Places graph next to selection inputs
                 plotlyOutput('county_plot_2'))
        )
        
      )
    }
    
  })
  
  output$uds_plot <- renderPlotly({
    
    # Names necessary variables
    ucn<- input$uds_county_name
    uv <- input$uds_var
    
    # Filters data by the selected county
    pd <- uds %>% filter(county == ucn)
    
    # Allows the character variable to be assigned as the y on the graph
    names(pd)[names(pd) == uv] <- 'value'
    
    # Creates graph
    p <- ggplot(data = pd,
                aes(x = county,
                    y = value)) +
      geom_bar(stat = 'identity',
               color = 'Blue',
               fill = 'cornflowerblue') +
      labs(title = paste('Compare county health')) +
      ggthemes:: theme_pander()  
    
    # Replaces underscores with spaces on variables and flips x and y
    p <- p + labs(y=gsub('_', ' ', uv)) + coord_flip() 
    
    
  })
  
  output$uds_map <- renderLeaflet({
    umv <- input$uds_map_var
    
    # Joins the data in shape file to the filtered health_data
    shp@data <- left_join(shp@data, uds, by =c('COUNTY'= 'county'))
    
    names(shp@data)[names(shp@data) == umv] <- 'value'
    
    # Creates color palette for map
    map_palette <- colorNumeric(palette = brewer.pal(9, "Blues"), 
                                domain=shp@data$value,
                                na.color="black") 
    # Labels counties on map and replaces underscores with spaces
    map_text <- paste(
      "County: ", as.character(shp@data$COUNTY),"<br/>",
      gsub('_', ' ', umv), ' : ', paste0(round(shp@data$value, digits = 2)),  "<br/>",
      sep="") %>%
      lapply(htmltools::HTML)
    
    # Creates map
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
  
  
  
  
  
  
  
  
  
  
  output$fqhc_table <-DT::renderDataTable({
    
    # Whole number variables 
    whole_nums <- c("total.patients", "health.center.service.grant.expenditures", 'total.cost', "total.cost.per.patient", "prenatal.patients", "prenatal.patients.who.delivered", 'x', 'health.center.name', 'city', 'state')  
    
    whole_nums_index <- names(fqhc) %in% whole_nums
    save(fqhc, file = 'fqhc.rda')
    fqhc[!whole_nums_index] <- apply(fqhc[!whole_nums_index], 2, function(x) round(x*100, 2))
    
    # When the user chooses the FQHC variable type the appropriate variables will be shown
    vt <- input$variable_type
    
    if(vt== 'Demographics'){
      pd <- fqhc %>% select(`Health Center` = health.center.name, City = city, State = state, `Total Patients` = total.patients, `Children Under 18` = children....18.years.old., `Adults 18-64` = adult..18...64., `Adults 65 and Older` = older.adults..age.65.and.over., `Racial and/or Ethnic Minority` = racial.and.or.ethnic.minority, `Hispanic/Latino Ethnicity` = hispanic.latino.ethnicity, `Black/African American` = black.african.american,Asian = asian,`American Indian/Alaskan Native` = american.indian.alaska.native, `Native Hawaiian/Other Pacific Islander` = native.hawaiian...other.pacific.islander, `More Than One Race` = more.than.one.race, `Best Served in Another Language` = best.served.in.another.language)
      
      
    } else if (vt == 'Patient Characteristics'){
      pd <- fqhc %>% select(`Health Center` = health.center.name,City = city,State = state, 
                            `Patients at 200 or Under Level of Poverty` = patients.at.or.below.200..of.poverty, 
                            `Patients at 100 or Under Level of Poverty` = patients.at.or.below.100..of.poverty, 
                            Uninsured = uninsured, `Medicaid/Chip` = medicaid.chip, 
                            Medicare =medicare)
      
    } else if (vt == 'Services'){
      pd <- fqhc %>% select(`Health Center` = health.center.name, City = city, 
                            State = state, Medical = medical, Dental = dental,
                            `Mental Health` = mental.health, 
                            `Substance Abuse` = substance.abuse, Vision = vision,
                            Enabling = enabling)
      
    } else if (vt == 'Clinical'){
      pd <- fqhc %>% select(`Health Center` = health.center.name, City=city, 
                            State=state, Hypertension=hypertension, Diabetes=diabetes,
                            Asthma=asthma, HIV=hiv, `Prenatal Patients` = prenatal.patients,
                            `Prenatal Patients Who Delivered` = prenatal.patients.who.delivered,
                            `Access to Prenatal Care (1st Prenatal Visit in 1st Trimester)` = access.to.prenatal.care..first.prenatal.visit.in.1st.trimester.,
                            `Low Birth Weight` = low.birth.weight, 
                            `Cervical Cancer Screening` = cervical.cancer.screening, 
                            `Adolescent Weight Screening and Follow Up` = adolescent.weight.screening.and.follow.up, 
                            `Adult Weight Screening and Follow Up` = adult.weight.screening.and.follow.up,
                            `Adults screened for Tobacco Use and Receiving Cessation Intervention` = adults.screened.for.tobacco.use.and.receiving.cessation.intervention,
                            `Colorectal Cancer Screening` = colorectal.cancer.screening, 
                            `Childhood Immunization` = childhood.immunization,
                            `Depression Screening` = depression.screening, 
                            `Dental Sealants` = dental.sealants, 
                            `Asthma Treatment` = asthma.treatment..appropriate.treatment.plan., 
                            `Statin Therapy` = statin.therapy.for.the.prevention.and.treatment.of.cardiovascular.disease,
                            `Heart Attack/Stroke Treatment (Aspirin Therapy for Ischemic Vascular Disease)` = heart.attack.stroke.treatment..aspirin.therapy.for.ischemic.vascular.disease.patients., 
                            `Blood Pressure Control` = blood.pressure.control..hypertensive.patients.with.blood.pressure...140.90.,
                            `Uncontrolled Diabetes` = uncontrolled.diabetes...9., 
                            `HIV Linkage to Care` = hiv.linkage.to.care)
      
    } else{
      pd <- fqhc %>% select(`Health Center` = health.center.name, City=city,
                            State=state, `Health Center Service Grant Expenditures` = health.center.service.grant.expenditures, 
                            `Total Cost` = total.cost, 
                            `Total Cost per Patient` =total.cost.per.patient)
    }
    DT::datatable(pd, options = list(scrollX=TRUE))
    
  })
  output$county_plot_2 <- renderPlotly({
    
    # Naming inputs
    cn <- input$county_name_2
    pv <- input$plot_var_2
    py <- input$plot_year_2
    save(cn, pv, py, file = 'inputs.rda')
    if(cn ==''){
      NULL
    } else {
      library(tidyr)
      # The dataset will filter by the year the user selects
      pd <- health_data %>% filter(County == cn,
                                   Year == py) %>% select(pv, first_fqhc, first_share, second_fqhc, second_share) 
      
      f_name <- pd$first_fqhc
      f_share <- pd$first_share
      s_name <- pd$second_fqhc
      s_share <- pd$second_share
      pd$first_fqhc <- pd$first_share <- pd$second_fqhc <- pd$second_share <- NULL
      pd <- pd %>% gather()
      # save(pd, file = 'plot_2.rda')
      
      # names(pd)[names(pd) == pv] <- 'value'
      # save(cn, pd, pv,py, file = 'temp.rda')
      
      # Turns decimals into percentages if variable is in per_var
      # Adds label of country and replaces underscores on variables with spaces
      
      # pd$value <- as.numeric(pd$value)
      # pd$value <- pd$value*100
      plot_text <- paste(
        'Dominant HC', ' : ', str_to_title(tolower(f_name)), "\n" 
        ,' Share of population served : ', round(f_share*100, 2), ' % ', "\n", 
        'Secondary HC', ' : ', str_to_title(tolower(s_name)), "\n", 
        ' Share of population served : ', round(s_share*100, 2), ' % ',
        sep="") %>%
        lapply(htmltools::HTML)
      # 
      
      # Creation of graph
      p <- ggplot(data = pd,
                  aes(x = gsub('_', ' ', key),
                      y = value,
                      text = plot_text)) +
        geom_bar(stat = 'identity',
                 color = 'Blue',
                 fill = 'cornflowerblue') +
        geom_text(aes(label = round(value,2))) +
        labs(title = paste('Compare county health'),
             x = '',
             y = 'Value') +
        ggthemes:: theme_pander() +
        coord_flip()
      
      # Changes decimals to percentages
      if(pv %in% per_var){
        p <- p + scale_y_continuous(labels = function(x) paste0(x, "%"))  
        
      } 
      # Changes underscores to spaces and flips coordinates
      ggplotly(p, tooltip = 'text') %>% style(textposition = 'right') %>%layout(hoverlabel = list(bgcolor = 'white')) %>%
        config(displayModeBar = F)
    }
    
  })
  
  
  output$county_plot <- renderPlotly({
    
    # Naming inputs
    cn <- input$county_name
    pv <- input$plot_var
    py <- input$plot_year
    
    # The dataset will filter by the year the user selects
    pd <- health_data %>% filter(County %in% cn,
                                 Year == py)
    
    names(pd)[names(pd) == pv] <- 'value'
    # save(cn, pd, pv,py, file = 'temp.rda')
    save(pd, file = 'covid.rda')
    # Turns decimals into percentages if variable is in per_var
    # Adds label of country and replaces underscores on variables with spaces
    
    pd$value <- unlist(pd$value)*100
    plot_text <- paste(
      'Dominant HC', ' : ', str_to_title(tolower(pd$first_fqhc)), "\n" ,' Share of population served : ', round(pd$first_share*100, 2), ' % ', "\n", 'Secondary HC', ' : ', str_to_title(tolower(pd$second_fqhc)), "\n", ' Share of population served : ', round(pd$second_share*100, 2), ' % ',
      sep="") %>%
      lapply(htmltools::HTML)
    
    
    # Creation of graph
    p <- ggplot(data = pd,
                aes(x = County,
                    y = value, text = plot_text)) +
      geom_bar(stat = 'identity',
               color = 'Blue',
               fill = 'cornflowerblue') +
      geom_text(aes(label = round(value,2)))+
      labs(title = paste('Compare county health')) +
      ggthemes:: theme_pander()
    
    # Changes decimals to percentages
    if(pv %in% per_var){
      p <- p + scale_y_continuous(labels = function(x) paste0(x, "%"))  
      
    } 
    # Changes underscores to spaces and flips coordinates
    p <- p + labs(y=gsub('_', ' ', pv)) + coord_flip() 
    ggplotly(p, tooltip = 'text') %>%  style(textposition = 'right') %>% layout(hoverlabel = list(bgcolor = 'white')) %>%
      config(displayModeBar = F)
  })
  
  output$health_center_plot <- renderPlotly({
    
    # Naming inputs
    hcn <- input$health_center_name
    hv <- input$health_var
    
    # Filters graph by chosen health center
    pd <- fqhc %>%
      filter(health.center.name %in% hcn)
    
    
    names(pd)[names(pd) == hv] <- 'value'
    
    plot_text <- paste(
      "Health Center: ", as.character(pd$health.center.name),"\n",
      gsub('.', ' ', hv, fixed = TRUE), ' : ', round(pd$value, digits = 2),  "\n",
      sep="") %>%
      lapply(htmltools::HTML)
    
    # Creates chart
    p <- ggplot(data = pd,
                aes(x = health.center.name,
                    y = value,
                    text = plot_text)) +
      geom_bar(stat = 'identity',
               color = 'Blue',
               fill = 'cornflowerblue') +
      coord_flip() +
      labs(title = paste('Compare health by health center'),
           x = ' ',
           y = hv) +
      ggthemes::theme_pander()
    ggplotly(p, tooltip = 'text') %>% 
      layout(hoverlabel = list(bgcolor = 'white')) %>%
      config(displayModeBar = F)
    
    
    
  })
  
  output$county_map <- renderLeaflet({
    # Names variables used in map
    mv <- input$map_var
    my <- input$map_year
    
    # Year selected is year shown
    pd <- health_data %>% filter(Year == my)
    
    # Joins the data in shape file to the filtered health_data
    shp@data <- left_join(shp@data, pd, by =c('COUNTY'= 'County'))
    
    names(shp@data)[names(shp@data) == mv] <- 'value'
    
    # Creates color palette for map
    map_palette <- colorNumeric(palette = brewer.pal(9, "Blues"), 
                                domain=shp@data$value,
                                na.color="black") 
    # Labels counties on map and replaces underscores with spaces
    map_text <- paste(
      "County : ", shp@data$COUNTY, "<br/>",
      "Dominant HC : ", str_to_title(tolower(shp@data$first_fqhc)),"<br/>",
      'Share of population served :', round(shp@data$first_share*100,2),' % ',  "<br/>",
      'Secondary HC : ', str_to_title(tolower(shp@data$second_fqhc)), "<br/>",
      ' Share of population served : ', round(shp@data$second_share*100,2), ' % ',
      sep="") %>%
      lapply(htmltools::HTML)
    
    # plot_text <- paste(
    #   'Dominant HC', ' : ', str_to_title(tolower(pd$first_fqhc)), "\n" ,' Share of population served : ', round(pd$first_share*100, 2), ' % ', "\n", 'Secondary HC', ' : ', str_to_title(tolower(pd$second_fqhc)), "\n", ' Share of population served : ', round(pd$second_share*100, 2), ' % ',
    #   sep="") %>%
    #   lapply(htmltools::HTML)
    
    # Creates map
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