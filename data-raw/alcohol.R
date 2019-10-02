## code to prepare `alcohol` dataset goes here

# Libraries
library(tidyverse)
library(here)

# alcohol usage -----------------------------------------------------------

who_1 <- read_csv(here("data-raw", "WHO-alcohol-2000-2009.csv"), 
									skip =1) %>% 
	filter(`Beverage Types` == "All types") %>% 
	gather(year, value, -c(1:3))%>% 
	select(Country, year, value) %>% 
	rename(consumption = value,
				 country_name = Country)%>% 
	mutate(year = as.integer(year))

who_2 <- read_csv(here("data-raw", "WHO-alcohol-1980-2000.csv"), 
									skip =1) %>% 
	filter(`Beverage Types` == "All types") %>% 
	gather(year, value, -c(1:3))%>% 
	select(Country, year, value) %>% 
	rename(consumption = value,
				 country_name = Country) %>% 
	mutate(year = as.integer(year))

who_3 <- read_csv(here("data-raw", "WHO-alcohol-2010-2018.csv"), 
									skip =1) %>% 
	filter(`Beverage Types` == "All types") %>% 
	gather(year, value, -c(1:3))%>% 
	select(Country, year, value) %>% 
	rename(consumption = value,
				 country_name = Country) %>% 
	mutate(year = as.integer(year))

combined_consumption <- bind_rows(who_1, who_2) %>% 
	bind_rows(who_3) %>% 
	mutate(country_name = ifelse(country_name=="United States of America",
															 "United States", country_name)) %>% 
	mutate(country_name = ifelse(grepl("United Kingdom", country_name),
															 "United Kingdom", country_name))

# covariates --------------------------------------------------------------

wb_1 <- read_csv(here("data-raw", "WB-additional-covariates-data.csv")) %>% 
	janitor::clean_names()

wb_1_clean <- wb_1 %>% 
	gather(year, value, -c(country_name:series_name)) %>% 
	mutate(value = as.numeric(value)) %>% 
	mutate(year = str_extract(year, "\\d{4}")) %>% 
	mutate(year = as.integer(year)) %>% 
	mutate(series_name = str_extract(series_name, "\\D+,|\\D+")) %>% 
	mutate(series_name = str_remove_all(series_name, 
																			",|\\(per|\\(World Bank estimate\\)"),
				 series_name = str_trim(series_name, "both"))%>%
	mutate(series_name = tolower(
		gsub(pattern = " ", "_", series_name))) %>% 
	filter(!is.na(value)) %>% 
	pivot_wider(names_from = series_name, values_from = value)

wb_2 <- read_csv(here("data-raw", "WB-gpd-data.csv"),skip = 5)


# combine -----------------------------------------------------------------
# Bring WB and WHO data together
alcohol_data_1 <- combined_consumption %>% 
	left_join(wb_1_clean, by = c("country_name", "year")) %>% 
	right_join(tibble(year = 1990:2016))
# Keep the years in which we have data
alcohol_data_2 <- alcohol_data_1 %>% 
	filter(year >= 1990) %>% 
	select(-literacy_rate,-gini_index) %>% 
	mutate(country_num = as.integer(as.factor(country_name)))

alcohol <- alcohol_data_2
# read out ----------------------------------------------------------------


usethis::use_data(alcohol, overwrite = T)
