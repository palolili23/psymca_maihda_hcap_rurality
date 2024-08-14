library(janitor)
library(here)
library(tidyverse)
# library(ggeffects)
library(lme4)
# library(merTools)
# library(labelled)
# library(sjPlot)
# library(Metrics)
library(rio)
library(WeMix)
library(VIM)

# Import data -------------------------------------------------------------

raw_data <-
  rio::import(here(
    "../PsyMCA2024_Data_Sharing_HCAP",
    "HCAP_harmonized_data_PsyMCA-v4.dta"
  )) %>%
  filter(study %in% c(3, 4, 9))

## Count missingness

raw_data %>% 
  count(is.na(age))

raw_data %>% 
  count(is.na(educattain_resp))

raw_data %>% 
  count(is.na(female))

raw_data %>% 
  count(is.na(rural))

raw_data %>%
  # filter(study == 9) %>% 
  filter(!is.na(fgcp)) %>% 
  dplyr::select(fgcp, fmem, forient, fexf, flang) %>% 
  aggr() %>% 
  summary()

raw_data <- raw_data %>%
  filter(!is.na(age), !is.na(educattain_resp), !is.na(fgcp))

# Clean variables for intersections ---------------------------------------

## Age

raw_data <- raw_data %>% 
  mutate(age_bin = 
           ifelse(age <75, "<75", "75+"))

raw_data %>% count(age_bin)

## Categorical Education

raw_data %>%
  count(educattain_resp)

raw_data <- raw_data %>%
  mutate(education_3cat = 
           case_when(
             educattain_resp == 0 ~ "no/early educ",
             educattain_resp == 1 ~ "primary educ",
             educattain_resp > 1 ~ "sec+ educ"))

# raw_data <- raw_data %>%
#   mutate(education_2cat = 
#            case_when(
#              educattain_resp == 0 ~ "no/early educ",
#              educattain_resp >= 1 ~ "primary,sec+ educ"))

raw_data %>%
  count(educattain_resp, education_3cat)

# raw_data %>%
#   count(educattain_resp, education_2cat)

## female & urban

raw_data <- raw_data %>% 
  mutate(
    rural = ifelse(rural == 1, "rural", "urban"),
    female = ifelse(female == 1, "woman", "men"),
         ) 

raw_data %>% count(rural)  
raw_data %>% count(rural, education_3cat)  

raw_data %>% count(female)  

# Count of intersections --------------------------------------------------

## Frequencies

# raw_data %>% 
#   filter(age <= 75) %>% 
#   count(study) 

# create data strata

raw_data %>% 
  group_by(study) %>% 
  count(rural, female, age_bin,
        education_3cat) %>% arrange(desc(n))

raw_data <- raw_data %>% 
  mutate(strata = fct_cross(rural, female, age_bin,
            education_3cat))

raw_data %>% group_by(study) %>% 
  count(strata) %>% arrange(desc(n))

strata_freq_table <- raw_data %>% 
  group_by(study) %>%
  count(strata) 

strata_freq_table

#Create a second level weight = 1 so that wemix works
raw_data <- raw_data %>% 
  mutate(weightL1 = 1)

# Export data -------------------------------------------------------------

export(strata_freq_table, here("03_output", "strata_freq_table.csv"))
export(raw_data, here("01_data", "clean_data.dta"))
export(raw_data, here("01_data", "clean_data.Rdata"))

