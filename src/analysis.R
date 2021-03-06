################################################# libraries required #####################################################
library(dplyr)
library(ggplot2)
library(here)
library(skimr)
library(visdat)
library(RColorBrewer)
#################################################### data input ##########################################################
data <- read.csv(here('data', 'train.csv'))
str(data)

################################################  exploratory analysis  ##################################################

# how big is the dataset?
ncol(data)
nrow(data)
head(data)

summary(data) #not much useful in this case
skim(data)
vis_dat(data)

# How many different dwelling types do we have?
data %>%
  select(MSSubClass) %>%
  n_distinct()

# What are the counts?
data %>%
  select(MSSubClass) %>%
  group_by(MSSubClass) %>%
  count(dwelling_count = MSSubClass) %>%
  mutate(dwelling_count = as.factor(dwelling_count)) %>%
    mutate(dwelling_type = recode(dwelling_count, '20' ='1-STORY 1946 & NEWER ALL STYLES',
                                '30' = '1-STORY 1945 & OLDER',
                                '40' = '1-STORY W/FINISHED ATTIC ALL AGES',
                                '45' = '1-1/2 STORY - UNFINISHED ALL AGES',
                                '50' = '1-1/2 STORY FINISHED ALL AGES',
                                '60' =  '2-STORY 1946 & NEWER',
                                '70' =  '2-STORY 1945 & OLDER',
                                '75' =  '2-1/2 STORY ALL AGES',
                                '80' =  'SPLIT OR MULTI-LEVEL',
                                '85' =  'SPLIT FOYER',
                                '90' =  'DUPLEX - ALL STYLES AND AGES',
                                '120' =  '1-STORY PUD (Planned Unit Development) - 1946 & NEWER',
                                '160' =  '2-STORY PUD - 1946 & NEWER',
                                '180' =  'PUD - MULTILEVEL - INCL SPLIT LEV/FOYER',
                                '190' =  '2 FAMILY CONVERSION - ALL STYLES AND AGES' )) %>%
  ggplot(., aes(x = dwelling_type,  y = n))+
  geom_bar(stat = 'identity')+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  labs(title='Count of dwelling types',
       x="dwelling_type", y = "count")
  

data %>%
  select(MSZoning, LotArea, Street) %>%
  mutate(LotArea_log = log10(LotArea)) %>%
  mutate(MSZoning_fact = as.factor(MSZoning)) %>%
  mutate(MSZoning_fact = recode(MSZoning_fact, 
                                'C (all)' = 'Commercial',
                                'FV' = 'Floating Village Residential',
                                'RH' = 'Residential High Density',
                                'RL' = 'Residential Low Density',
                                'RM' = 'Residential Medium Density'
  )) %>%
  ggplot(., aes(x= MSZoning_fact, y = LotArea_log, fill = MSZoning_fact)) + 
  geom_boxplot()+
  scale_fill_brewer(palette="Green") + 
  geom_jitter(shape=16, position=position_jitter(0.2))+
  labs(title = 'Lot size vs zoning',
       y='log(LotArea (square feet)) ',x='Zoning classification')+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Relationship between roof style and roof material
data %>%
  select(RoofStyle, RoofMatl) %>%
  ggplot(., aes(RoofMatl, ..count..)) + 
  geom_bar(aes(fill = RoofStyle), position = "dodge") +
  facet_grid(. ~ RoofStyle) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

data %>%
  select(RoofStyle, RoofMatl) %>%
  group_by(RoofStyle, RoofMatl) %>%
  count() 


data %>%
  select(MSSubClass, MSZoning, LotArea) %>%
  group_by(MSSubClass, MSZoning) %>%
  mutate(mean_lotArea = mean(LotArea)) %>%
  mutate(MSZoning_fact = as.factor(MSZoning)) %>%
  mutate(MSZoning_fact = recode(MSZoning_fact, 
                                'C (all)' = 'Commercial',
                                'FV' = 'Floating Village Residential',
                                'RH' = 'Residential High Density',
                                'RL' = 'Residential Low Density',
                                'RM' = 'Residential Medium Density'
  )) %>%
  mutate(dwelling_fct = as.factor(MSSubClass)) %>%
  mutate(dwelling_type = recode(dwelling_fct, '20' ='1-STORY 1946 & NEWER ALL STYLES',
                                '30' = '1-STORY 1945 & OLDER',
                                '40' = '1-STORY W/FINISHED ATTIC ALL AGES',
                                '45' = '1-1/2 STORY - UNFINISHED ALL AGES',
                                '50' = '1-1/2 STORY FINISHED ALL AGES',
                                '60' =  '2-STORY 1946 & NEWER',
                                '70' =  '2-STORY 1945 & OLDER',
                                '75' =  '2-1/2 STORY ALL AGES',
                                '80' =  'SPLIT OR MULTI-LEVEL',
                                '85' =  'SPLIT FOYER',
                                '90' =  'DUPLEX - ALL STYLES AND AGES',
                                '120' =  '1-STORY PUD (Planned Unit Development) - 1946 & NEWER',
                                '160' =  '2-STORY PUD - 1946 & NEWER',
                                '180' =  'PUD - MULTILEVEL - INCL SPLIT LEV/FOYER',
                                '190' =  '2 FAMILY CONVERSION - ALL STYLES AND AGES' )) %>%
  ggplot(., aes(x = dwelling_type, y =mean_lotArea, color = MSZoning_fact))+
  geom_point() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  labs(title = 'Lot area vs dwelling types',
       y='LotArea (square feet) ',x='Dwelling type')
# + facet_grid(. ~  MSZoning_fact)

data %>%
  select(MSSubClass, MSZoning, LotArea) %>%
  group_by(MSSubClass, MSZoning) %>%
  mutate(mean_lotArea = mean(LotArea)) %>%
  mutate(MSZoning_fact = as.factor(MSZoning)) %>%
  mutate(MSZoning_fact = recode(MSZoning_fact, 
                                'C (all)' = 'Commercial',
                                'FV' = 'Floating Village Residential',
                                'RH' = 'Residential High Density',
                                'RL' = 'Residential Low Density',
                                'RM' = 'Residential Medium Density'
  )) %>%
  mutate(dwelling_fct = as.factor(MSSubClass)) %>%
  mutate(dwelling_type = recode(dwelling_fct, '20' ='1-STORY 1946 & NEWER ALL STYLES',
                                '30' = '1-STORY 1945 & OLDER',
                                '40' = '1-STORY W/FINISHED ATTIC ALL AGES',
                                '45' = '1-1/2 STORY - UNFINISHED ALL AGES',
                                '50' = '1-1/2 STORY FINISHED ALL AGES',
                                '60' =  '2-STORY 1946 & NEWER',
                                '70' =  '2-STORY 1945 & OLDER',
                                '75' =  '2-1/2 STORY ALL AGES',
                                '80' =  'SPLIT OR MULTI-LEVEL',
                                '85' =  'SPLIT FOYER',
                                '90' =  'DUPLEX - ALL STYLES AND AGES',
                                '120' =  '1-STORY PUD (Planned Unit Development) - 1946 & NEWER',
                                '160' =  '2-STORY PUD - 1946 & NEWER',
                                '180' =  'PUD - MULTILEVEL - INCL SPLIT LEV/FOYER',
                                '190' =  '2 FAMILY CONVERSION - ALL STYLES AND AGES' )) %>%
  ggplot(., aes(dwelling_type, color = MSZoning_fact, fill = MSZoning_fact))+
  geom_bar(position ='dodge') +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  labs(title = 'Lot area vs dwelling types',
       y='LotArea (square feet) ',x='Dwelling type')

# density of LotArea
ggplot(data, aes(x=LotArea)) + 
  geom_density(color="darkblue", fill="lightblue")+
  geom_vline(aes(xintercept=mean(LotArea)),
             color="blue", linetype="dashed", size=1)