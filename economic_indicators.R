### Key Indicators ###

### Tool Kit ###

library(rvest)
library(magrittr)
library(dplyr)
library(janitor)
library(ggplot2)
library(stringr)
library(httr)
library(rvest) 
library(readr) 
library(plyr)

### Population ###

Key <- "40198e226e787c053829f94a575968a3cd5a67fd"

Pop_LinkGenerator <- glue::glue("https://api.census.gov/data/2019/pep/population?get=COUNTY,DATE_CODE,DATE_DESC,DENSITY,POP,NAME,STATE&for=county:*&key={Key}") 

Pop_LinkGenerator

Pop_Request <- GET(Pop_LinkGenerator) 

Pop_Parsed <- jsonlite:: fromJSON(content(Pop_Request, as = "text"), flatten = TRUE, simplifyDataFrame = TRUE)  

County_Population <- row_to_names(Pop_Parsed, row_number = 1) 

County_Population <- as.data.frame(County_Population) 

County_Population$fips_txt <- paste(County_Population$STATE, County_Population$COUNTY, sep = "")



county_population_2 <- County_Population %>% 
  filter(DATE_CODE == 12)

county_population_2$DENSITY <- as.numeric(county_population_2$DENSITY)




county_population_2 <- county_population_2 %>% 
  select(4,5,6,10)


names(county_population_2)[4] <- paste("county_code")


saveRDS(county_population_2, file = "population.rds")



### Poverty ### 

poverty <- read_csv("poverty.csv") 





saveRDS(poverty, file = "poverty.rds")


### Median House Hold Income ####

house_income <- read_csv("house_income.csv")

names(house_income)[1] <- paste("county_code")

saveRDS(house_income, file = "house_income.rds") 

### Education ### 

education <- read_csv("education.csv")

names(education)[1] <- paste("county_code")


saveRDS(education, file = "education.rds")

education_var_description <- read_csv("education_var_description.csv")

saveRDS(education_var_description, file = "education_var_description.rds")


### Large Data frame ### 



final_data <- join(county_population_2, poverty, type = "inner")

final_data <- join(final_data, house_income, type = "inner")

final_data <- join(final_data, education, type = "inner")


final_data_2 <- final_data %>% 
  select(-5,-6,-7,-8,-11,-12, -13,-14,-15,-18,-19,-20,-21)


final_data_3 <- final_data_2


names(final_data_3)[1] <- paste("density")
names(final_data_3)[3] <- paste("county_name")
names(final_data_3)[4] <- paste("county_code")
names(final_data_3)[2] <- paste("population")
names(final_data_3)[5] <- paste("poverty_total")
names(final_data_3)[6] <- paste("poverty_percent")
names(final_data_3)[7] <- paste("household_income")
names(final_data_3)[8] <- paste("household_income_to_state")
names(final_data_3)[9] <- paste("rural_urban_continum")
names(final_data_3)[10] <- paste("urban_influence")
names(final_data_3)[11] <- paste("education_1")
names(final_data_3)[12] <- paste("education_2")
names(final_data_3)[13] <- paste("education_3")
names(final_data_3)[14] <- paste("education_4")
names(final_data_3)[15] <- paste("education_1_percent")
names(final_data_3)[16] <- paste("education_2_percent")
names(final_data_3)[17] <- paste("education_3_percent")
names(final_data_3)[18] <- paste("education_4_percent") 

final_data_3$household_income_to_state <- final_data_3$household_income_to_state/100

final_data_4 <- final_data_3



final_data_4 <- final_data_4 %>% 
  select(3,4,1,2,5,6,7,8,9,10,11,12,13,14,15,16,17,18)



readr::write_csv(final_data_4, "CleanEconomicData.csv")

saveRDS(final_data_4, file = "clean_economic_indicators.rds")

