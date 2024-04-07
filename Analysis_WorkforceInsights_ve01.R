# Project Name: Workforce Insights
# Author: Bruno Alves de Carvalho
# Status: ongoing


# Set up ------------------------------------------------------------------

# Set the directory
setwd("/Users/brunoalvesdecarvalho/Desktop/DataWarehouse_20231015_ve01")

# Load packages for data processing
library(tidyverse)
library(memoise)
library(haven)

# Load functions
source("R_Scripts/FunctionRepository_20231016_ve01.R")

# Load color palette
source("R_Scripts/ColorPalette_20240128_ve01.R")

# Load data
aggregated_data_shp <- 
  readRDS("SHP/Data_Aggregated_1999_2022/cached_data_shp.rds")


# Collect data ------------------------------------------------------------

# Select variables necessary for analysis
selected_vars_shp <- 
  rep(
    list(
      c(
        # id variables
        "idpers",
        "idhous$$",
        # time variable
        "year",
        # social variables
        "age$$",
        "generation",
        "sex$$",
        "civsta$$", 
        "edyear$$", # number of years in education
        # economic variables
        "iptotni", # total income
        "iwyni", # working income
        "noga2m$$", # current job: nomenclature of economic activities
        # geographical variables
        "canton$$",
        "com2_$$",
        # political variables
        "p$$p10", # political position, scale 0 (left) to 10 (right)
        # work from home (WFH) variables
        "p$$w80", # work at home, nominal factor of 1 to 5
        "p$$w80a", # work at home frequency, ordinal factor of 1 to 7
        "p$$w80b", # work at home reasons, 8 reasons
        "p$$w77", # number of hours worked per week
        # talent attrition variables
        "p$$w18", # change of employer/job, last 12 months
        "p$$w66", # change of employer, month
        "p$$w23", # change of job, month
        "p$$w21", # change of employer, frequency
        "p$$w600", # change of employer/job, first reason
        "p$$w601", # change of employer/job, second reason
        "type_change",
        "type_reason",
        # talent management variables
        "p$$w229", # interest in tasks, scale 0 to 10 (completely satisfied)
        "p$$w228", # satisfaction with job in general
        "p$$w230", # satisfaction with amount of work
        "p$$w615", # satisfaction with hierarchical superiors
        "p$$w93", # satisfaction with working conditions
        "p$$w94", # satisfaction with work atmosphere
        # total rewards
        "h$$i30", # satisfaction with financial situation of Household
        "p$$i01", # satisfaction with financial situation
        "p$$w92", # satisfaction with income
        # health and well-being variables
        "p$$c18", # frequency of energy and optimism
        "p$$c02", # satisfaction with health status
        "p$$c02", # improvement in health, last 12 months
        "p$$c44" # satisfaction with life in general
      )
    ),
    length(1999:2022)
  )

# Merge data into one single data-set
merged_data_shp <-
  shp_merge_data(aggregated_data_shp, selected_vars_shp)


# Transform Data ----------------------------------------------------------
merged_data_shp$industry_01 <- 
  factor(
    merged_data_shp$`noga2m$$`,
    levels = c(1:17),
    labels = c(
      "Agriculture, hunting, forestry",
      "Fishing and fish farming",
      "Mining and quarrying",
      "Manufacturing",
      "Electricity, gas and water supply",
      "Construction",
      "Wholesale,retail; repair motor vehicles,household goods",
      "Hotels and restaurants",
      "Transport, storage and communication",
      "Financial intermediation; insurance",
      "Real estate; renting; computer; research",
      "Public admin,national defence; compulsory social security",
      "Education",
      "Health and social work",
      "Other community, social and personal service activities",
      "Private households with employed persons",
      "Extra-territorial organizations and bodies"
      )
  )

merged_data_shp$industry_02 <-
  fct_collapse(
    merged_data_shp$industry_01,
    Agriculture = c(
      "Agriculture, hunting, forestry",
      "Fishing and fish farming"
      ),
    Manufacturing = c(
      "Manufacturing"
      ),
    Construction = c(
      "Construction"
      ),
    Retail = c(
      "Wholesale,retail; repair motor vehicles,household goods"
      ),
    Hospitality = c(
      "Hotels and restaurants"
      ),
    Logistics = c(
      "Transport, storage and communication"
      ),
    Finance = c(
      "Financial intermediation; insurance"
      ),
    Professional = c(
      "Real estate; renting; computer; research"
      ),
    Public = c(
      "Public admin,national defence; compulsory social security"
      ),
    Education = c(
      "Education"
      ),
    Health = c(
      "Health and social work"
      ),
    Other = c(
      "Mining and quarrying",
      "Private households with employed persons",
      "Electricity, gas and water supply",
      "Extra-territorial organizations and bodies",
      "Other community, social and personal service activities"
      )
    )

merged_data_shp$wfh_types <-
  factor(
    merged_data_shp$`p$$w80a`,
    levels = c(1:7),
    labels = c(
      "In my job it is not possible to work from home",
      "Never, as I don¿t want to work from home",
      "A few times a year",
      "About once a month",
      "About once a week",
      "Several days a week",
      "I work mainly at home"
      )
    )

merged_data_shp$wfh_types_recoded <-
  fct_collapse(
    merged_data_shp$wfh_types,
    remote = c(
      "I work mainly at home"
      ),
    hybrid = c(
      "About once a week",
      "Several days a week"
      ),
    flexible = c(
      "About once a month",
      "A few times a year"
      ),
    on_site = c(
      "In my job it is not possible to work from home",
      "Never, as I don¿t want to work from home"
      )
    )

merged_data_shp$attrition_month <- 
  ifelse(
    merged_data_shp$`p$$w23` >= 0,
    merged_data_shp$`p$$w23`,
    NA
  )

merged_data_shp$attrition_month_fct <-
  factor(
    merged_data_shp$attrition_month,
    levels = c(1:12),
    labels = c(
      "January",
      "February",
      "March",
      "April",
      "May",
      "June",
      "July",
      "August",
      "September",
      "October",
      "November",
      "December"
      )
    )

merged_data_shp$attrition_month_fct_recoded <-
  fct_collapse(
    merged_data_shp$attrition_month_fct,
    Q1 = c("January", "February", "March"),
    Q2 = c("April", "May", "June"),
    Q3 = c("July", "August", "September"),
    Q4 = c("October", "November", "December")
    )

merged_data_shp$pol_ideology <- 
  ifelse(
    merged_data_shp$`p$$p10` > 5, "right_wing", 
    ifelse(
      merged_data_shp$`p$$p10` < 5 & merged_data_shp$`p$$p10` >= 0, "left_wing", 
      ifelse(
        merged_data_shp$`p$$p10` == 5, "Neutral", NA)
    )
  )

merged_data_shp$age_group_01 <-
  cut(
    merged_data_shp$`age$$`,
    breaks = c(0,18,30,65,125),
    labels = c("minors", "young adults", "adults", "seniors"),
    right = FALSE
  )

merged_data_shp$optimism_scale <- 
  ifelse(
    merged_data_shp$`p$$c18` >= 0,
    merged_data_shp$`p$$c18`,
    NA
    )

merged_data_shp$optimism_group <- 
  ifelse(
    merged_data_shp$`p$$c18` > 5, "optimistic", 
    ifelse(
      merged_data_shp$`p$$c18` < 5 & merged_data_shp$`p$$c18` >= 0, "pessimistic", 
      ifelse(
        merged_data_shp$`p$$c18` == 5, "neither", NA)
    )
  )


# Exploratory Data Analysis -----------------------------------------------

# These 3 plots show that most people report changing employer in Q3, especially in August and September
merged_data_shp %>% 
  filter(type_change == "changed job" & !is.na(attrition_month_fct) & !is.na(type_reason)) %>%
  group_by(type_reason, attrition_month_fct) %>% count() %>% 
  filter(type_reason == "voluntary") %>% 
  ggplot(aes(attrition_month_fct, n)) +
  geom_bar(stat = "identity") 

merged_data_shp %>% 
  filter(type_change == "changed job" & !is.na(attrition_month_fct_recoded) & !is.na(type_reason)) %>% 
  group_by(type_reason, attrition_month_fct_recoded, year) %>% 
  count() %>% 
  group_by(type_reason, year) %>% 
  mutate(prct = n / sum(n)) %>% 
  filter(type_reason == "voluntary") %>% 
  ggplot(aes(year, prct, group = attrition_month_fct_recoded, fill = attrition_month_fct_recoded)) + 
  geom_bar(stat = "identity", position = "stack") +
  scale_y_continuous(
    labels = scales::percent_format(),
    minor_breaks = 0) +
  scale_x_continuous(
    breaks = seq(2004, 2022, 2),
    minor_breaks = 0) +
  scale_fill_manual(values = c(grey_light, grey_medium, red_light, grey_dark)) +
  labs(
    x = NULL, y = NULL,
    title = "It's gettin' hot in here") +
  theme_minimal() + 
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(linewidth = 0.25),
    legend.position = "top",
    legend.title = element_blank(),
    legend.text = element_text(margin = margin(r = 25)),
    legend.box.margin = margin(6, 6, -18, 6)
    ) 

merged_data_shp %>% 
  filter(type_change == "changed job" & !is.na(attrition_month_fct) & !is.na(type_reason)) %>% 
  group_by(type_reason, attrition_month_fct, year) %>% 
  count() %>% group_by(type_reason, year) %>%
  mutate(prct = n / sum(n)) %>% 
  filter(type_reason == "voluntary") %>% 
  ggplot(aes(year, prct, group = attrition_month_fct, color = attrition_month_fct)) + 
  geom_line(linewidth = 0.75, show.legend = F) +
  scale_y_continuous(
    labels = scales::percent_format(),
    breaks = seq(0, 0.3, 0.05),
    limits = c(0,0.3)) +
  scale_x_continuous(breaks = seq(2004, 2022, 2)) +
  scale_color_manual(values = c(rep(grey_medium, 7), red, blue, rep(grey_light, 3))) +
  labs(x = NULL, y = NULL) +
  theme_minimal() +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(linewidth = 0.25)
  )


