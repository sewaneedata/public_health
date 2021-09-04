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
library(gsheet)

if(grepl('marthaclark', getwd())){
  setwd('/Users/marthaclark/Documents/DataLab/public_health//')
}

# Get rid of exponential numbers
options(scipen = '1000')

# Reading in datasets
uds <- read.csv('cleaned_udsm.csv')
fqhc <- read.csv('fqhc_data.csv')

# Make sure there's a column in both df's with the exact same name

# Changing all column names to lowercase
names(uds) <- tolower(names(uds))
names(fqhc) <- tolower(names(fqhc))

# Recode counties in uds mapper so they are the same as the shp file counties
uds$county <- gsub(' County', '', uds$county)
uds <- uds %>% mutate(county = ifelse(county == 'DeKalb', 'Dekalb',
                                      ifelse(county == 'Van Buren', 'Van_Buren', county)))

# List all variables that are percentages
per_var <- c('Adult_Smoking %', 'Adult_Obesity %', 'Flu_Vaccinations',
             'Fully_Covid_Vaccinated %', 'At_least_1_COVID_vaccine_dose %',
             'Abortion_Rates_2013', 'Children_under_18 %', 'People_in_Rural_or_Isolated_settings %',
             'People_of_Color', 'Disabilities', 'Senior_Citizens', 'Physical Inactivity %',
             'White_Population(%)', 'children....18.years.old.', 'adult..18...64.',
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
             "patients.at.or.below.100..of.poverty","uninsured",
             "medicaid.chip", "medicare", "other.third.party",
             'first_share', 'second_share', 'third_share', 'fourth_share',
             'fifth_share', 'low_income_pen', 'tot_pop_pen', 'uninsured_pen',
             'medicaid_pen', 'uninsured', 'medicaid_pub_ins', 'medicaid_priv_ins',
             ' pop_poverty', 'pop_hispanic_latino', 'pop_black', 'pop_asian',
             'pop_ai_an', 'pop_nh', 'pop_opi', 'pop_white', 'pop_mixed',
             'pop_medicaid_pub', 'pop_medicaid_priv', 'pop_school_age',
             'pop_65_and_up', 'pop_unemployed', 'pop_houses_limited_english',
             'pop_less_than_high_school', 'pop_vets', 'pop_disability',
             'pop_unins_under_200', "Flu_Vaccinations_Medicare_Enrollees",
             "Fully_Covid_Vaccinated", "At_least_1_COVID_vaccine_dose",
             "People_in_Rural_or_Isolated_settings","Physical Inactivity",
             "American_Indian_and_Alaskan_Native_Population (%)"
             ,"Native_Hawaiian_and_Other_Pacific_Islander_Population(%)",
             "Percent_in_Poverty","Percent_on_Tenncare","pop_poverty",
             'pop_minority', 'pop_uninsured', 'pop_unins_under_200')

# Reading in googlesheet team manually worked on
health_data <- read.csv(text = gsheet2text('https://docs.google.com/spreadsheets/d/1uLIrv4xXrhZseOtRSscWtkuYJkeixcyIpB-i8A6d4ZU/edit?ts=60e89447#gid=658962581', format = 'csv'),
                         stringsAsFactors=TRUE)

# Set UDS data to year available
uds$year <- 2019

# Join health data with UDS data by county and year
health_data <- left_join(health_data, uds, by = c('County'= 'county', 'Year'= 'year'))

# List whole numbers  
gross_numbers <- c("Total_Population","Health_Outcomes_Rankings (/95)","Quality_of_Life","Premature_Deaths (deaths before 75 yrs, per 100,000 people)","Life_Expectancy", "Health_Factors_Rankings (/95)","Alcohol_impaired_Driving_Deaths","Alcohol_impaired_Driving_Deaths_per_1000_people","Driving_Deaths","Driving_Deaths_per_1000_people","Uninsured","Uninsured_per_1000_people","Number_of_Primary_Care_Physicians","Number_of_Primary_Care_Physicians_per_1000_people","Dentist_Providers","Dentist_Providers_per_1000","Mental_Health_Providers","Mental_Health_Providers_per_1000_people","Covid_Cases","Covid_Deaths","People_with_HIV","White_Population","Black_African_American_Population","American_Indian_and_Alaskan_Native_Population","Asian_Population","Native_Hawaiian_and_Other_Pacific_Islander_Population","Other_Race","Two_Or_More_Races","FQHCs_ and_LALs","Emergency_Dept","Hospital_Closures","Hospitals","Licensed_Mental_Health_Treatment_Sites","Licensed_Substance_Abuse_Treatment_Sites","Overdose_Deaths","tot_pop","num_fqhc","num_patients","pop_low_income","unserved_low_income","unserved_total_pop","unserved_uninsured","unserved_medicaid_public","unserved_medicaid_private","Number_of_Opoid_Prescriptions_Per_1000")


# Create two vectors of columns, one from health data and one from uds to identify source later
uds_cols <- names(uds)
health_data_cols <- names(health_data)

# Remove unnecessary variables from health_data
remove_these <- 'County|Year|source|Total_Population|Alcohol_Impaired_Driving_Deaths|Driving_Deaths|Uninsured|Number_of_Primary_Care_Physicians|Dentist_Providers|Mental_Health_Providers|White_Population|`White_Population(%)`|Black_African_American_Population|`Black_African_American_Population (%)`|American_Indian_and_Alaskan_Native_Population|`American_Indian_and_Alaskan_Native_Population(%)`|Asian_Population|`Asian_Population(%)`|Native_Hawaiian_and_Other_Pacific_Islander_Population|Uninsured|People_of_Color|Other_Race|`Other_Race(%)`|Two_Or_More_Races|`Two_Or_More_Races(%)`|x|first_fqhc|first_share|second_fqhc|second_share|third_fqhc|third_share|fourth_fqhc|fourth_share|fifth_fqhc|fifth_share|Disabilities|Children_under_18|Senior_Citizens|FQHC_Cherokee_or_Ocoee|Nonprofit_clinics|Rural_Urban'

# Create vector of choices for health data
hd_choices <- health_data_cols[!grepl(remove_these, health_data_cols)]

# Create nicer labels for each choice
hd_labels <- c('Health Outcomes Rankings', 'Quality of Life', 'Premature Deaths',
               'Health Factors Rankings', 'Adult Smoking', 'Adult Obesity',
               'Medicare Flu Vaccinations', 'Covid Cases', 'Covid Deaths',
               'Covid Fully Vaccinated', 'One Covid Vaccine Dose', 'Abortion Rates (2013)',
               'HIV', 'People in Rural/Isolated Settings', 'Physical Inactivity',
               'Opioid Prescriptions per 1000 People', 'FQHCs and LALs Present',
               'Emergency Departments','Hospital Closures', 'Hospitals',
               'Licensed Mental Health Treatment Sites',
               'Licensed Substance Abuse Treatment Sites', 'Poverty Percentage',
               'TennCare Percentage', 'Overdose Deaths', 'Total Population',
               'Number of FQHCs', 'Number of Patients at HC', 'Low Income Population',
               'Low Income HC Not Served', 'Total Population HC Not Served',
               'Uninsured HC Not Served', 'Medicaid Public Insurance HC Not Served',
               'Medicaid Private Insurance HC Not Served', 'Low Income Penetration at HC',
               'Total Population Penetration at HC', 'Uninsured Penetration at HC',
               'Medicaid Penetration at HC', 'Uninsured', 'Medicaid Public Insurance',
               'Medicaid Private Insurance', 'Poverty Population', 'Minority Population',
               'Hispanic Latino Population', 'African American Population',
               'Asian Population', 'American Indian/Alaskan Native Population',
               'Native Hawaiian Population', 'Other Pacific Islander Population',
               'White Population', 'Uninsured Population',
               'Medicaid Public Insurance Population',
               'Medicaid Private Insurance Population', 'School Age Population',
               '65 and Up Population', 'Unemployed Population',
               'Households with Limited English Population',
               'Less than High School Education Population', 'Veteran Population',
               'Disability Population', 'Uninsured Below 200 FPL Population')

# use setNames to combine choices and labels
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
        text = 'Vaccinations',
        tabName = 'vaccinations'
      ),
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
                              # Choose whether to compare multiple or single counties
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
                              
                              # Allows user to choose a year from health_data Year col      
                              selectInput(inputId = 'map_year',
                                          label = 'Choose a year to plot',
                                          choices = unique(health_data$Year),
                                          selected = '2019')
                       ),
                       column(8,
                              leafletOutput('county_map'))
                     )
                   )
          )
        )
      ),
      
      tabItem(
        # Creation of fqhc tab
        tabName="fqhc",
        tabsetPanel(
          tabPanel(title = 'Table',
                   fluidPage(
                     fluidRow(
                       column(4,
                              # Allows user to choose health centers from fqhc data
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
                              DT::dataTableOutput('fqhc_table')
                              
                       )
                     )
                   )
                 ),
          tabPanel(title='Chart',
                   fluidPage(
                     fluidRow(
                       column(4,
                              # Allows user to choose health centers from fqhc data
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

                       )
                     )
                   )
                 )
               )
             ),
      
      tabItem(
        # Creation of vaccinations tab
        tabName="vaccinations",
        fluidRow(

          
          column(4,
    
          )),
        
        fluidRow(
          column(12,
                 uiOutput('vaccination_plot')
                 
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
  
  # If user wants to compare single county choose percent or gross number to compare
  output$ui_per_gross <- renderUI({
    cc <- input$compare_county
    if(cc != 'Between counties'){
      selectInput('per_gross',
                  label = 'Choose percent or gross variables',
                  choices = c('Percent', 'Gross number'),
                  selected = 'Percent')
    }
  })
  
  # User chooses how many counties to compare
  output$ui_county_comparison <- renderUI({
    cc <- input$compare_county
    
    # If user chooses multiple counties
    if(cc == 'Between counties'){
      fluidRow(
        column(3,
               
               # Allows user to choose counties from health_data County col
               # Shows first 10 counties by default
               selectInput(inputId = 'county_name',
                           label = 'Choose a county',
                           choices = health_data$County,
                           multiple = TRUE,
                           selected = health_data$County[1:10])),
        column(3,
               # Allows user to choose one health variable from health_data cols
               selectInput(inputId = 'plot_var',
                           label = 'Choose a variable to plot',
                           choices = hd_choices,
                           selected = 'HCP_Total_Patients_2019')),
        column(3,
               # Allows user to choose a year from health_data Year col      
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
      # If user chooses single county
    } else {
      pg <- input$per_gross
      # here do null condition
      if(is.null(pg)){
        NULL
      } else {
        # Shows correct options for whole number choice
        if(pg=='Gross number'){
          var_choices <- hd_choices[hd_choices %in%gross_numbers]
        } else {
          # Shows correct options for percentage choice
          var_choices<- hd_choices[!hd_choices %in% gross_numbers]
          var_choices <- var_choices[!var_choices %in% c('County', 'Year',
                                                         'FQHC_Cherokee_or_Ocoee',
                                                         'Nonprofit_clinics',
                                                         "Rural_Urban","x",
                                                         "first_fqhc","first_share",
                                                         "second_fqhc","second_share",
                                                         "third_fqhc","third_share",
                                                         "fourth_fqhc","fourth_share",
                                                         "fifth_fqhc","fifth_share")]
        }

        fluidRow(
          column(3,
                 # Allows user to choose one county from health_data County col
                 selectInput(inputId = 'county_name_2',
                             label = 'Choose a county',
                             choices = health_data$County,
                             selected = TRUE,
                             multiple = FALSE)),
          column(3,
                 # Allows user to choose a health variable from health_data cols
                 selectInput(inputId = 'plot_var_2',
                             label = 'Choose a variable to plot',
                             choices = var_choices,
                             selected = var_choices[1:3],
                             multiple = TRUE) ),
          column(3,
                 # Allows user to choose a year from health_data Year col      
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
    }
  })
  
  output$fqhc_table <-DT::renderDataTable({
    
    # Whole number variables in fqhc data
    whole_nums <- c("total.patients", "health.center.service.grant.expenditures",
                    'total.cost', "total.cost.per.patient", "prenatal.patients",
                    "prenatal.patients.who.delivered", 'x', 'health.center.name',
                    'city', 'state')  
    
    # Has correct decimals multiplied by 100 and rounded to percents
    whole_nums_index <- names(fqhc) %in% whole_nums
    fqhc[!whole_nums_index] <- apply(fqhc[!whole_nums_index], 2, function(x) round(x*100, 2))
    
    # When the user chooses the FQHC variable type the appropriate variables will be shown
    vt <- input$variable_type
    
    if(vt== 'Demographics'){
      pd <- fqhc %>% select(`Health Center` = health.center.name, City = city,
                            State = state, `Total Patients` = total.patients,
                            `Children Under 18` = children....18.years.old.,
                            `Adults 18-64` = adult..18...64.,
                            `Adults 65 and Older` = older.adults..age.65.and.over.,
                            `Racial and/or Ethnic Minority` = racial.and.or.ethnic.minority,
                            `Hispanic/Latino Ethnicity` = hispanic.latino.ethnicity,
                            `Black/African American` = black.african.american,
                            Asian = asian,
                            `American Indian/Alaskan Native` = american.indian.alaska.native,
                            `Native Hawaiian/Other Pacific Islander` = native.hawaiian...other.pacific.islander,
                            `More Than One Race` = more.than.one.race,
                            `Best Served in Another Language` = best.served.in.another.language)
      
      
    } else if (vt == 'Patient Characteristics'){
      pd <- fqhc %>% select(`Health Center` = health.center.name, City = city, State = state,
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
                            `Access to Prenatal Care (1st Prenatal Visit in 1st Trimester)`
                            = access.to.prenatal.care..first.prenatal.visit.in.1st.trimester.,
                            `Low Birth Weight` = low.birth.weight,
                            `Cervical Cancer Screening` = cervical.cancer.screening,
                            `Adolescent Weight Screening and Follow Up` = adolescent.weight.screening.and.follow.up,
                            `Adult Weight Screening and Follow Up` = adult.weight.screening.and.follow.up,
                            `Adults screened for Tobacco Use and Receiving Cessation Intervention`
                            = adults.screened.for.tobacco.use.and.receiving.cessation.intervention,
                            `Colorectal Cancer Screening` = colorectal.cancer.screening,
                            `Childhood Immunization` = childhood.immunization,
                            `Depression Screening` = depression.screening,
                            `Dental Sealants` = dental.sealants,
                            `Asthma Treatment` = asthma.treatment..appropriate.treatment.plan.,
                            `Statin Therapy` =
                              statin.therapy.for.the.prevention.and.treatment.of.cardiovascular.disease,
                            `Heart Attack/Stroke Treatment (Aspirin Therapy for Ischemic Vascular Disease)`
                            = heart.attack.stroke.treatment..aspirin.therapy.for.ischemic.vascular.disease.patients.,
                            `Blood Pressure Control` =
                              blood.pressure.control..hypertensive.patients.with.blood.pressure...140.90.,
                            `Uncontrolled Diabetes` = uncontrolled.diabetes...9.,
                            `HIV Linkage to Care` = hiv.linkage.to.care)
      
    } else{
      pd <- fqhc %>% select(`Health Center` = health.center.name, City=city,
                            State=state, `Health Center Service Grant Expenditures`
                            = health.center.service.grant.expenditures,
                            `Total Cost` = total.cost,
                            `Total Cost per Patient` =total.cost.per.patient)
    }
    # Allows user to scroll through table
    DT::datatable(pd, options = list(scrollX=TRUE))
    
  })
  
  output$county_plot_2 <- renderPlotly({
    
    # Naming inputs
    cn <- input$county_name_2
    pv <- input$plot_var_2
    py <- input$plot_year_2
    
    
    # If no county is selected don't show a graph
    if(is.null(cn)){
      NULL
    } else {
      library(tidyr)
      
      # The dataset will filter by the year the user selects
      pd <- health_data %>% filter(County == cn,
                                   Year == py) %>%
        select(pv, first_fqhc, first_share, second_fqhc, second_share)
      
      save(pd, file = 'temp.rda')
      pd[,pv][grepl('NA', pd[, pv])] <- NA
      # If there is data for year chosen show plot
      if(!all(is.na(pd[,pv]))){
        
        # Names variables shown on hover over
        f_name <- pd$first_fqhc
        f_share <- pd$first_share
        s_name <- pd$second_fqhc
        s_share <- pd$second_share
        
        # Deletes columns from variable choices
        pd$first_fqhc <- pd$first_share <- pd$second_fqhc <- pd$second_share <- NULL
        
        # Unlists variables
        pd <- pd %>% gather()
        
        # Creates hover over text
        plot_text <- paste(
          'Dominant HC', ' : ', str_to_title(tolower(f_name)), "\n"
          ,' Share of FQHC patients served : ', round(f_share*100, 2), ' % ', "\n",
          'Secondary HC', ' : ', str_to_title(tolower(s_name)), "\n",
          ' Share of FQHC patients served : ', round(s_share*100, 2), ' % ',
          sep="") %>%
          lapply(htmltools::HTML)
        
        # Turns decimals into percents
        pd$value <- round( as.numeric(unlist(pd$value)), 2 )
        if(pv %in% per_var){
          for(i in 1:length(pd$value)){
            message("i : ", i)
            message("pd: ", str(pd))
            if( !is.na(pd$value[i]) & pd$value[i] <= 1 ){
              pd$value[i] <-  pd$value[i]*100
            }
          }
        }
        
        # Creation of graph
        p <- ggplot(data = pd,
                    aes(x = gsub('_', ' ', key),
                        y = value,
                        text = plot_text)) +
          geom_bar(stat = 'identity',
                   color = 'Blue',
                   fill = 'cornflowerblue') +
          geom_text(aes(label = value)) +
          labs(title = paste('Compare county health'),
               x = '',
               y = 'Value') +
          ggthemes:: theme_pander() +
          coord_flip()
        
        # Changes decimals to percentages and adds %
        if(pv %in% per_var){
          p <- p + scale_y_continuous(labels = function(x) paste0(x, "%"))  
          
        }
        
        # Formats hover over
        ggplotly(p, tooltip = 'text') %>% style(textposition = 'right') %>%
          layout(hoverlabel = list(bgcolor = 'white')) %>%
          config(displayModeBar = F)
        
        # If there's no data for year chosen show empty plot
      } else{
        empty_plot <- function(title = NULL){
          p <- plotly_empty(type = "scatter", mode = "markers") %>%
            config(
              displayModeBar = FALSE
            ) %>%
            layout(
              title = list(
                text = title,
                yref = "paper",
                y = 0.5
              )
            )
          return(p)
        }
        p <- empty_plot("No data available for the selected inputs")
        ggplotly(p)
      }
      
    }
    
  })
  
  output$county_plot <- renderPlotly({
    
    # Naming inputs
    cn <- input$county_name
    pv <- input$plot_var
    idx <- which( hd_choices == pv )
    y_lab = hd_labels[idx]
    py <- input$plot_year
    
    # The dataset will filter by the year and county the user selects
    pd <- health_data %>%
      filter(County %in% cn,
             Year == py)
    
    # Allows a character variable to be assigned as the y on the graph
    names(pd)[names(pd) == pv] <- 'value'
    
    # If there's data for year chosen show plot
    if(!all(is.na(pd$value))){
      
      # Turns decimals into percentages if variable is in per_var
      if(pv %in% per_var){
        pd$value <- as.numeric(unlist(pd$value))*100
        
      } else {
        pd$value <- as.numeric(unlist(pd$value))
        
      }
      # Creates hover over text
      plot_text <- paste(
        'Dominant HC : ', str_to_title(tolower(pd$first_fqhc)), "\n",
        ' Share of FQHC patients served : ', round(pd$first_share*100, 2), ' % ', "\n",
        'Secondary HC : ', str_to_title(tolower(pd$second_fqhc)), "\n",
        ' Share of FQHC patients served : ', round(pd$second_share*100, 2), ' % ',
        sep="") %>%
        lapply(htmltools::HTML)
      
      # Creation of graph
      p <- ggplot(data = pd,
                  aes(x = County,
                      y = value,
                      text = plot_text)) +
        geom_bar(stat = 'identity',
                 color = 'Blue',
                 fill = 'cornflowerblue') +
        geom_text(aes(label = round(value,2)))+
        labs(title = 'Compare county health') +
        ggthemes:: theme_pander()
      
      # Changes decimals to percentages and adds %
      if(pv %in% per_var){
        p <- p + scale_y_continuous(labels = function(x) paste0(x, "%"))  
      }
      
      # Labels y axis and flips coordinates
      p <- p + labs(y=y_lab) + coord_flip()
      
      # Formats hover over
      ggplotly(p, tooltip = 'text') %>%  
        style(textposition = 'right') %>%
        layout(hoverlabel = list(bgcolor = 'white')) %>%
        config(displayModeBar = F)
      
      # If there's no data for year chosen show empty plot
    } else {
      empty_plot <- function(title = NULL){
        p <- plotly_empty(type = "scatter", mode = "markers") %>%
          config(
            displayModeBar = FALSE
          ) %>%
          layout(
            title = list(
              text = title,
              yref = "paper",
              y = 0.5
            )
          )
        return(p)
      }
      p <- empty_plot("No data available for the selected inputs")
      ggplotly(p)
    }
    
  })
  
  output$county_map <- renderLeaflet({
    
    # Names variables used in map
    mv <- input$map_var
    my <- input$map_year
    
    # Year selected is year shown
    pd <- health_data %>% filter(Year == my)
    
    # Allows character to be y-value
    names(pd)[names(pd)==mv] <- 'value'
    
    # Formats values as number out of lists
    pd$value <- round(as.numeric(unlist(pd$value)), 2)
    
    # If there's data for year chosen show map
    if(!all(is.na(pd$value))){
      
      # Joins the data in shape file to the filtered health_data
      shp@data <- left_join(shp@data, pd, by =c('COUNTY'= 'County'))
      
      # Creates color palette for map
      map_palette <- colorNumeric(palette = brewer.pal(9, "Blues"),
                                  domain=shp@data$value,
                                  na.color="black")
      
      # Turns decimals into percents and adds %
      val <- shp@data$value
      if(mv %in% per_var){
        val <- val * 100
        val <- paste0(val, '%')
      }
      
      # Creates hover over text for map
      map_text <- paste(
        "County : ", shp@data$COUNTY, "<br/>",
        "Value : ", val, "<br/>",
        "Dominant HC : ", str_to_title(tolower(shp@data$first_fqhc)),"<br/>",
        'Share of population served : ', round(shp@data$first_share*100,2),' % ',  "<br/>",
        'Secondary HC : ', str_to_title(tolower(shp@data$second_fqhc)), "<br/>",
        ' Share of population served : ', round(shp@data$second_share*100,2), ' % ',
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
            direction = "bottom"
          )
        )  %>%  
        addLegend(pal=map_palette, title = '',
                  values=~value,
                  opacity=0.9,
                  position = "bottomleft",
                  na.label = "NA" )
      
      # If there's no data for year chosen show empty map
    } else{
      leaflet() %>%
        addProviderTiles('Esri.WorldShadedRelief') %>%  
        addLegend(title = 'No data available',
                  colors = NA,
                  labels = NA)
      
    }
  })
  
  output$vaccination_plot <- renderPlotly({
    
    pd <- health_data %>% 
      filter(Year== 2020 || Year == 2021 ) %>%
      group_by(County) %>%
      summarise(x= mean(Flu_Vaccinations_Medicare_Enrollees, na.rm = TRUE),
                y = mean(as.numeric(unlist(At_least_1_COVID_vaccine_dose)), na.rm = TRUE))

    
    ggplot(data = pd,
           aes(x = x,
               y = y)) +
      geom_point(alpha = 0.5) +
      xlim(0,1) +
      ylim(0,1) +
      geom_smooth(se = FALSE, color = 'red', lty = 2, method = 'lm')
    
    fit <- lm(y~x, data = pd)
    summary(fit)
    
    predictions <- predict(fit, newdata=pd)
    pd$predicted <- predictions
    
    pd$residual <- pd$y-pd$predicted
    
  })
  
}

shinyApp(ui, server)