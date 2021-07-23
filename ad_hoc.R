library(dplyr)
library(ggplot2)
library(readr)


if(grepl('marthaclark', getwd())){
  setwd('/Users/marthaclark/Documents/DataLab/public_health')
}


uds <- read.csv('cleaned_udsm.csv')
uds$year <- 2019
health_data <- read_sheet('https://docs.google.com/spreadsheets/d/1uLIrv4xXrhZseOtRSscWtkuYJkeixcyIpB-i8A6d4ZU/edit?ts=60e89447#gid=658962581', sheet = 2)
health_data <- left_join(health_data, uds, by = c('County'= 'county', 'Year'= 'year'))


pd <- health_data %>% 
  filter(Year != 2020) %>%
  group_by(County) %>%
  summarise(x = mean(Flu_Vaccinations_Medicare_Enrollees, na.rm = TRUE),
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
 