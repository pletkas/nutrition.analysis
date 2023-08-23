#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

library(tidyverse)
library(corrplot)
library(janitor)
library(tidymodels)
library(psych)
library(RColorBrewer)
library(wordcloud)
library(ggfortify)

rawdata <- tibble(read.csv("/Users/sagepletka/Documents/GitHub/nutrition.analysis/food.csv")) %>%
  clean_names()

useful_nutrient_only <- rawdata %>% select(-data_household_weights_1st_household_weight,
                                           -data_household_weights_1st_household_weight_description, - data_household_weights_2nd_household_weight,
                                           -data_household_weights_2nd_household_weight_description, -data_water, 
                                           -data_refuse_percentage, -data_ash, -data_kilocalories, -data_sugar_total,
                                           -data_fat_monosaturated_fat, -data_fat_polysaturated_fat, -data_fat_saturated_fat,
                                           -data_fat_total_lipid, -data_fiber, -data_carbohydrate,
                                           -data_vitamins_vitamin_a_iu, -data_vitamins_vitamin_a_rae,
                                           -data_major_minerals_sodium)

useful_columns <- colnames(useful_nutrient_only)




# Define UI for application that draws a histogram
ui <- fluidPage(
selectInput("nutrient", "Select Nutrient to see top 20 sources",
            useful_columns)
)
server <- function(input, output) {

    })


# Run the application 
shinyApp(ui = ui, server = server)
