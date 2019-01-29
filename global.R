
################################################################################
# LOADING LIBRARIES
library(shinydashboard)
library(tidyverse)
library(DT)
library(googleVis)
################################################################################
# LOADING DATASET
complete.df = read.csv('./data/national.cba.report.1970.2015.txt', 
                   sep = '\t', 
                   stringsAsFactors = FALSE)
################################################################################
complete.df = complete.df %>%
  select(-X) %>%
  mutate(Country = ifelse(Country == 'USA',          'US'            , Country),
         Country = ifelse(Country == 'UK' ,          'United Kingdom', Country),
         Country = ifelse(Country == 'Cote dIvoire', "Cote d'Ivoire" , Country),
         Country = ifelse(Country == 'DR Congo',     'Democratic Republic of the Congo'           , Country),
         Country = ifelse(Country == 'Viet Nam',     'Vietnam'       , Country))
################################################################################
metric = list(
  'Consumption Emission (in GgCO2)'          = 'CBA_GgCO2',
  'Consumption (in GgCO2 / BnUSD GDP)'       = 'CBA_GgCO2PerBnUSDGDP',
  'Consumption (in MtCO2 / Capita)'          = 'CBA_MtCO2perCap',
  
  'Production Emission (in GgCO2)'           = 'PBA_GgCO2',
  'Production (in GgCO2 / BnUSD GDP)'        = 'PBA_GgCO2PerBnUSDGDP',
  'Production (in MtCO2 / Capita)'           = 'PBA_MtCO2perCap',
  
  'Gross Domestic Product (in Millions USD)' = 'GDP_MilUSD',
  'Population'                               = 'Population'
  )
################################################################################

################################################################################


