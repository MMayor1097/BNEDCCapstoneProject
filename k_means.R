### K-Means Clusterins #### 

data <- readRDS("~/MSBA/BNEDCCapstoneProject/CSV Files/clean_economic_indicators.rds")

install.packages("tidyr")
install.packages("cluster")
install.packages("factoextra")
install.packages("sparcl")

library(tidyr)
library(cluster)
library(factoextra) # clustering algorithms & visualization 
library(sparcl)
library(dplyr)

data <- data %>%
  select(3,4,1,2, 5:18)

data$population <- as.numeric(data$population)


data_2 <- scale(data[,3:18])  
data_3 <- data %>% 
  select(1,2)

data <- cbind.data.frame(data_3, data_2)


###### Initial Clustering ####



set.seed(12345) # Set seed for reproducibility
fit_1 <- kmeans(x = data[3:18], # Set data as explantory variables 
                centers = 4,  # Set number of clusters
                nstart = 25, # Set number of starts
                iter.max = 100 ) # Set maximum number of iterations to use


# Extract clusters
clusters_1 <- fit_1$cluster
# Extract centers
centers_1 <- fit_1$centers

# Take out city cluster
data_no_cities <- data[clusters_1 != 1,]

data_no_cities <- data_no_cities %>%
  select(county_name, county_code, density, population, poverty_percent, household_income,
         household_income_to_state, rural_urban_continum, urban_influence, education_1_percent,
         education_2_percent, education_3_percent, education_4_percent)

saveRDS(data_no_cities, file = "data_no_cities.rds")

summary(as.factor(clusters_1))
####

cat("Cluster 1 county_name:\n")

group_1 <- data$county_name[clusters_1 == 1]
data$county_code[clusters_1 == 1]



cat("Cluster 2 county_name:\n")

group_2<- data$county_name[clusters_1 == 2]
data$county_code[clusters_1 == 2] 




cat("Cluster 3 county_name:\n")

group_3 <-data$county_name[clusters_1 == 3]
data$county_code[clusters_1 == 3] 


cat("Cluster 4 county_name:\n")

group_4 <- data$county_name[clusters_1 == 4]
data$county_code[clusters_1 == 4] 


# visualzing the data #

cluster <- c(1:4) 

center_df <- data.frame(cluster, centers_1) 

center_reshape <- gather(center_df, features, values, density: education_4_percent) 

head(center_reshape)




g_heat_1 <- ggplot(data = center_reshape, aes(x = features, y = cluster, fill = values)) +
  scale_y_continuous(breaks = seq(1, 4, by = 1)) +
  geom_tile() +
  coord_equal() + 
  theme_set(theme_bw(base_size = 22) ) +
  scale_fill_gradient2(low = "blue", # Choose low color
                       mid = "white", # Choose mid color
                       high = "red", # Choose high color
                       midpoint =2, # Choose mid point
                       space = "Lab", 
                       na.value ="grey", # Choose NA value
                       guide = "colourbar", # Set color bar
                       aesthetics = "fill") + # Select aesthetics to apply
  coord_flip()


g_heat_1



group_1 <- as.data.frame(group_1)
group_2 <- as.data.frame(group_2) 
group_3 <- as.data.frame(group_3) 
group_4 <- as.data.frame(group_4)

library(stringr)


group_check <- group_1 %>% 
  filter(str_detect(group_1, "McLean County, Illinois"))

group_check_2 <- group_2 %>% 
  filter(str_detect(group_2, "McLean County, Illinois"))


group_check_3 <- group_3 %>% 
  filter(str_detect(group_3, "McLean County, Illinois"))

group_check_4 <- group_4 %>% 
  filter(str_detect(group_4, "McLean County, Illinois"))


# Our county is in group 3 #### 









