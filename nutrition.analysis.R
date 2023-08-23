library(tidyverse)
library(corrplot)
library(janitor)
library(tidymodels)
library(psych)
library(RColorBrewer)
library(wordcloud)
library(ggfortify)

rawdata <- tibble(read.csv("/Users/sagepletka/Documents/data projects/food.nutrition/food.csv")) %>%
  clean_names()
head(rawdata)
#48 col, 7413 row

#all columns
dim(rawdata)
describe(rawdata)
rawdata[0, ]
columns <- colnames(rawdata)
columns

unique(rawdata$category)
unique(rawdata$category)[1:100]
#1183 unique categories

head(rawdata$description)
head(rawdata$category)

rawdata$category[1:100]

useful_nutrient_only <- rawdata %>% select(-data_household_weights_1st_household_weight,
                   -data_household_weights_1st_household_weight_description, - data_household_weights_2nd_household_weight,
                   -data_household_weights_2nd_household_weight_description, -data_water, 
                   -data_refuse_percentage, -data_ash, -data_kilocalories, -data_sugar_total,
                   -data_fat_monosaturated_fat, -data_fat_polysaturated_fat, -data_fat_saturated_fat,
                   -data_fat_total_lipid, -data_fiber, -data_carbohydrate,
                   -data_vitamins_vitamin_a_iu, -data_vitamins_vitamin_a_rae,
                   -data_major_minerals_sodium)

useful_columns <- colnames(useful_nutrient_only)

#correlation matrix
correlation_raw <- cor(useful_nutrient_only[ , 4:30])
correlation_raw[!lower.tri(correlation_raw)] <- NA # remove diagonal and redundant values


high_correlation <- data.frame(correlation_raw) %>%
  rownames_to_column() %>%
  gather(key="variable", value="correlation", -rowname) %>%
  filter(abs(correlation) > 0.5) %>% tibble()


#top 20 choline sources plotted
rawdata %>% slice_max(data_choline, n = 20) %>%
  ggplot(aes(reorder(description, -data_choline), data_choline)) +
  geom_col(aes(fill = description), show.legend = FALSE) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size = 5))


        
#we can create a function that does this for us
top_nutrients_plot <- function(nutrient) {
  graph <- rawdata %>% slice_max({{nutrient}}, n = 20) %>%
    ggplot(aes(reorder(description, -{{nutrient}}), {{nutrient}})) +
    geom_col(aes(fill = description), show.legend = FALSE) +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size = 5)) +
    xlab("Food Descripton")
  return(graph)
}
top_nutrients_plot(data_vitamins_vitamin_k)

#and a function to just create the list
top_nutrients_list <- function(nutrient) {
  list <- rawdata %>% slice_max({{nutrient}}, n = 20) %>% pull(description)
  return(list)
}
top_nutrients_list(data_vitamins_vitamin_e)


#ground sage is highest, but we should check to make sure the amount is relatively 
#small as you probably arent putting a cup of spices on food
rawdata %>% filter(description == "SAGE,GROUND") %>% select(data_household_weights_1st_household_weight, data_household_weights_1st_household_weight_description)


#make a list of graphs 




#i need to try to get an idea of how many foods are in each category
rawdata %>% count(category) %>% summary(n)


#create a list of top food sources for each nutrient
#why is this so fucking goddamn hard
#jk it is one easy ass line of code
densefoods <- map(useful_nutrient_only, top_nutrients_list)



#word cloud of categories with highest nutrients

#function to find category of food item in top 20 oif each nutrient
high_nutrient_cat <- function(nutrient) {
  cats <- rawdata %>% slice_max({{nutrient}}, n = 20) %>% pull(category)
  return(cats)
}


cloudvec <- character()
loopvector <- colnames(useful_nutrient_only)
for (i in loopvector) {
  cloudvec <- c(cloudvec, high_nutrient_cat(get(i)))
}  
#remove first 60 as they correspond to the first three columns which are not nutrients
cloudvec <- cloudvec[61:604]

wordcount <- tibble(cloudvec) %>% drop_na() %>% 
  filter(cloudvec != "No Category") %>% count(cloudvec, sort = TRUE)
wordcount

wordcloud(words = wordcount$cloudvec, freq = wordcount$n, max.words = 50,
          random.order = FALSE, colors = brewer.pal(8, "Dark2"), 
          scale=c(3.5,0.25))

#wordcloud w/out cereals rte
wordcloud(words = wordcount$cloudvec, freq = wordcount$n, max.words = 50,
          random.order = FALSE, colors = brewer.pal(8, "Dark2"))


#now lets do wordcloud with descriptions
descrip <- character()
for (i in loopvector) {
  descrip <- c(descrip, top_nutrients_list(get(i)))
}
descrip <- descrip[61:604]
descrip_wrangle <- descrip %>% paste(collapse = " ") %>%
  str_replace_all(",", " ") %>% str_replace_all("CKD", "") %>% 
  str_replace_all("RTE", "") %>% str_replace_all("PDR", "") %>%
  str_replace_all("VAR", "") %>% str_replace_all("GENERAL", "") %>%
str_split_1(" ")
  

desc_wordcount <- as_tibble(descrip_wrangle) %>% drop_na() %>% count(value, sort = TRUE)
desc_wordcount

wordcloud(words = desc_wordcount$value, freq = desc_wordcount$n, max.words = 50,
          random.order = FALSE, colors = brewer.pal(8, "Dark2"))


#pca 
useful_columns
pca_nutrients <- prcomp(useful_nutrient_only[ , 4:30], scale. = TRUE)
autoplot(pca_nutrients)
autoplot(pca_nutrients)
