library(dplyr)
library(tidyr)
library(lubridate)
library(readr)
library(ggplot2)

getwd()

dir(pattern = "*.csv")
filename <- dir(pattern = "*.csv")

daybyday_data <- tibble()

for(i in 1: length(filename)){
  daybyday_data <- read_csv(filename[i]) %>%
    select("Ad name", "Ad set name","Day","Impressions","Results","Cost per result","Amount spent (RUB)","CPM (cost per 1,000 impressions)",
           "Link clicks","CPC (cost per link click)") %>%
    rename(ad_name="Ad name",
           ad_set_name="Ad set name",
           day="Day",
           impressions="Impressions",
           results="Results",
           cost_per_result="Cost per result",
           amount_spent ="Amount spent (RUB)",
           CPM="CPM (cost per 1,000 impressions)",
           link_clicks="Link clicks",
           CPC="CPC (cost per link click)") %>%
    bind_rows(daybyday_data)
}

#impressions for Ads at ad_set level on daily basis
daybyday_time_ad_set_impressions <- daybyday_data %>%
  group_by(day,ad_set_name) %>% 
  summarise(impressions_day_ad_set = sum(impressions,na.rm = TRUE) )

daybyday_time_ad_set_impressions %>% 
  ggplot(aes(x=day,fill=factor(ad_set_name)))+
  geom_col(aes(y=impressions_day_ad_set),alpha=0.8) +
  theme_classic() 

#CPM (cost per 1000) for Ads at ad_set level on daily basis
daybyday_time_CPM <- daybyday_data %>% 
  group_by(day, ad_set_name) %>%
  summarise(CPM_day_ad_set = sum(impressions*CPM/1000,na.rm = TRUE)/sum(impressions,na.rm = TRUE)*1000)


daybyday_time_CPM %>% 
  ggplot(aes(x=day,fill=factor(ad_set_name)))+
  geom_col(aes(y=CPM_day_ad_set),alpha=0.8) +
  theme_classic()


#CPL (cost per results) for Ads at ad_set level on daily basis
daybyday_time_CPL <- daybyday_data %>%
  group_by(day, ad_set_name) %>%
  summarise(CPL_day_ad_set = sum(amount_spent,na.rm = TRUE)/sum(results,na.rm = TRUE))

daybyday_time_CPL %>% 
  ggplot(aes(x=day,fill=factor(ad_set_name)))+
  geom_col(aes(y=CPL_day_ad_set),alpha=0.8) +
  theme_classic()


# CPC (cost per click) for Ads at ad_set level on daily basis
daybyday_time_CPC <- daybyday_data %>%
  group_by(day, ad_set_name) %>%
  summarise(CPC_day_ad_set = sum(link_clicks*CPC,na.rm = TRUE)/sum(link_clicks,na.rm = TRUE))

daybyday_time_CPC %>% 
  ggplot(aes(x=day,fill=factor(ad_set_name)))+
  geom_col(aes(y=CPC_day_ad_set),alpha=0.8) +
  theme_classic()

            
           







