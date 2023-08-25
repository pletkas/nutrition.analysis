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

setwd("/Users/sagepletka/Documents/GitHub/nutrition.analysis")

rawdata <- tibble(read.csv("/Users/sagepletka/Documents/GitHub/nutrition.analysis/food.csv")) %>%
  clean_names()



useful_nutrient_only <- rawdata %>%
  select(-data_household_weights_1st_household_weight, 
         -data_household_weights_1st_household_weight_description,
         - data_household_weights_2nd_household_weight,
         -data_household_weights_2nd_household_weight_description, -data_water, 
         -data_refuse_percentage, -data_ash, -data_kilocalories, -data_sugar_total,
         -data_fat_monosaturated_fat, -data_fat_polysaturated_fat, -data_fat_saturated_fat,
         -data_fat_total_lipid, -data_fiber, -data_carbohydrate,
         -data_vitamins_vitamin_a_iu, -data_vitamins_vitamin_a_rae,
         -data_major_minerals_sodium)

useful_columns <- colnames(useful_nutrient_only)

#wordcloud processing
top_nutrients_list <- function(nutrient) {
  list <- rawdata %>% slice_max({{nutrient}}, n = 20) %>% pull(description)
  return(list)
}

densefoods <- map(useful_nutrient_only, top_nutrients_list)

high_nutrient_cat <- function(nutrient) {
  cats <- rawdata %>% slice_max({{nutrient}}, n = 20) %>% pull(category)
  return(cats)
}

cloudvec <- character()
loopvector <- colnames(useful_nutrient_only)
for (i in loopvector) {
  cloudvec <- c(cloudvec, high_nutrient_cat(get(i)))
}  

cloudvec <- cloudvec[61:604]

wordcount <- tibble(cloudvec) %>% drop_na() %>% 
  filter(cloudvec != "No Category") %>% count(cloudvec, sort = TRUE)
wordcount

wordcloud(words = wordcount$cloudvec, freq = wordcount$n, max.words = 50,
                       random.order = FALSE, colors = brewer.pal(8, "Dark2"), 
                       scale=c(3.5,0.25), rot.per = .25)

#correlation matrix
correlation_raw <- cor(useful_nutrient_only[ , 4:30])
correlation_raw[!lower.tri(correlation_raw)] <- NA # remove diagonal and redundant values
correlation_raw 


# Define UI
ui <- fluidPage(
  theme = bslib::bs_theme(bootswatch = "darkly"),
  titlePanel(div("Nutrition Dataset Analysis")),
  textOutput("intro"),
  
  fluidRow(
    column(6, selectInput("nutrient", "Select Nutrient to see top 10 sources",
            choices = names(useful_nutrient_only)[4:30])),
    column(6, textOutput("correlation"))
  ),
  
  fluidRow(
    column(6, plotOutput("dense")),
    column(6, tableOutput("cor_table"))
  ),
  fluidRow(
    column(6, textOutput("cloudlabel"))
  ),
  fluidRow(
    column(6, plotOutput("wordcloud"))
  )
)

#backend server
server <- function(input, output) {
  

  
  output$intro <- renderText("Interactive application exploring a nutrition
                             dataset")
 
  output$correlation <- renderText("Table of Correlations above .5")
  
  output$cloudlabel <- renderText("Wordcloud of food categories")
  
 output$dense <- renderPlot({
   useful_nutrient_only %>% slice_max(.data[[input$nutrient]], n = 10) %>% 
     tibble() %>% 
     ggplot(aes(fct_rev(fct_reorder(description, .data[[input$nutrient]])), .data[[input$nutrient]])) +
     geom_col(aes(fill = description), show.legend = FALSE) + 
     theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size = 7)) +
     xlab("Food Descripton")
  })
 
 output$cor_table <- renderTable(data.frame(correlation_raw) %>%
    rownames_to_column() %>%
    gather(key="variable", value="correlation", -rowname) %>%
    filter(abs(correlation) > 0.5) %>% arrange(desc(correlation) %>%
    tibble()))
 
 output$wordcloud <- renderPlot(width = 500, height = 500, wordcloud(words = wordcount$cloudvec, freq = wordcount$n, max.words = 50,
                                          random.order = FALSE, colors = brewer.pal(8, "Dark2"), 
                                          scale=c(3.5,0.25), rot.per = .25))
   
    }


# Run the application 
shinyApp(ui = ui, server = server)
