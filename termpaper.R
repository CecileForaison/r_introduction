library(dplyr)
library(ggplot2)
library(tidyr)
library(readr)
library(grid)
library(xlsx)
library(gridExtra)

# We select all of the .csv of the directory file select the important column, rename them and bind all of them in one tibble
filenames <- dir(pattern = "*.csv")
facebook_ads_data <- tibble()
for (i in 1:length(filenames)) {
  facebook_ads_data <- read_csv(filenames[i])%>%
    select("Ad name", "Ad set name", Day, "Ad delivery", Reach, Impressions, Frequency, "Result Type", Results, "Cost per result", "Amount spent (RUB)",
          "CPM (cost per 1,000 impressions)", "Link clicks", "CPC (cost per link click)", "CTR (link click-through rate)", "CPC (all)", "CTR (all)",
          "Clicks (all)", "Ad ID", "Campaign ID") %>%
    rename(ad_name = "Ad name",
           ad_set_name = "Ad set name",
           day = Day,
           ad_delivery = "Ad delivery",
           reach = Reach,
           impressions = Impressions,
           frequency = Frequency,
           result_type = "Result Type",
           results = Results,
           cost_per_result = "Cost per result",
           amount_spent = "Amount spent (RUB)",
           CPM = "CPM (cost per 1,000 impressions)",
           link_clicks = "Link clicks",
           CPC = "CPC (cost per link click)",
           CTR = "CTR (link click-through rate)",
           CPC_all = "CPC (all)",
           CTR_all = "CTR (all)",
           clicks_all = "Clicks (all)",
           ad_ID = "Ad ID",
           campaign_ID = "Campaign ID"
           ) %>%
    bind_rows(facebook_ads_data)
}

# This is used to know how many different ad set we are working with and what their names are
adsetnames <- unique(facebook_ads_data$ad_set_name)
# This function waas created because else if for example there was no leads on one day, 
# the Cost per lead would return "inf". We want it to return zero because it then makes it more clear that there was no leads
divisionby0 <- function(nominator, denominator) {
  if (denominator == 0) {
    return(0)
  }
  else {
    return(nominator/denominator)
  }
}

# This long for loop creates a dataframe for each ad set with the performances detailed for each ads.
# It also creates 3 plots for each ad set to show the evolution of Impresssions, CPM, CPC and CPL 
# At the end it combines the plots and the dataframe in a 1 page PDF
for (i in 1:length(adsetnames)) {
    temp_df_summary <- facebook_ads_data %>%
    filter(ad_set_name == adsetnames[i],
           impressions != 0) %>%
      group_by(ad_name) %>%
      summarise(Impressions = sum(impressions),
                "Amount Spent (RUB)" = sum(amount_spent, na.rm = TRUE),
                CPM = (sum(amount_spent, na.rm = TRUE) / sum(impressions, na.rm = TRUE)) * 1000,
                Clicks = sum(link_clicks, na.rm = TRUE),
                CPC = divisionby0(sum(amount_spent, na.rm = TRUE), sum(link_clicks, na.rm = TRUE)),
                Leads = sum(results, na.rm = TRUE),
                CPL = divisionby0(sum(amount_spent, na.rm = TRUE),sum(results, na.rm = TRUE)))
  # This is used to add a "Total" row to show the general performances of the ad set
    temp_df_summary <- rbind(temp_df_summary, tibble(ad_name = "Total", 
                              Impressions = sum(temp_df_summary$Impressions, na.rm = TRUE),
                              "Amount Spent (RUB)" = sum(temp_df_summary$`Amount Spent (RUB)`, na.rm = TRUE),
                              CPM = mean(temp_df_summary$CPM, na.rm = TRUE),
                              Clicks = sum(temp_df_summary$Clicks, na.rm = TRUE),
                              CPC = sum(`Amount Spent (RUB)`, na.rm = TRUE)/sum(Clicks, na.rm = TRUE),
                              Leads = sum(temp_df_summary$Leads, na.rm = TRUE),
                              CPL = sum(`Amount Spent (RUB)`, na.rm = TRUE)/sum(Leads, na.rm = TRUE))) %>%
      arrange(-Impressions)
   
  # Here is the plots that shows the evolution of impressions over the period
    plot_impressions <- 
      facebook_ads_data %>%
      filter(ad_set_name == adsetnames[i],
             impressions != 0) %>%
      group_by(day) %>%
      summarise(Impressions = sum(impressions, na.rm = TRUE),
                "Amount Spent (RUB)" = sum(amount_spent, na.rm = TRUE),
                CPM = (sum(amount_spent, na.rm = TRUE) / sum(impressions, na.rm = TRUE)) * 1000,
                Clicks = sum(link_clicks, na.rm = TRUE),
                CPC = divisionby0(sum(amount_spent, na.rm = TRUE), sum(link_clicks, na.rm = TRUE)),
                Leads = sum(results, na.rm = TRUE),
                CPL = divisionby0(sum(amount_spent, na.rm = TRUE), sum(results, na.rm = TRUE)))%>%
      ggplot(aes(x = day)) +
      geom_col(aes(y = Impressions), fill = "light blue") +
      xlab("Days")+
      theme_classic() 

    # Here is the plots that shows the evolution of CPM over the period
    plot_CPM <- 
      facebook_ads_data %>%
      filter(ad_set_name == adsetnames[i],
             impressions != 0) %>%
      group_by(day) %>%
      summarise(Impressions = sum(impressions, na.rm = TRUE),
                "Amount Spent (RUB)" = sum(amount_spent, na.rm = TRUE),
                CPM = (sum(amount_spent, na.rm = TRUE) / sum(impressions, na.rm = TRUE)) * 1000,
                Clicks = sum(link_clicks, na.rm = TRUE),
                CPC = divisionby0(sum(amount_spent, na.rm = TRUE), sum(link_clicks, na.rm = TRUE)),
                Leads = sum(results, na.rm = TRUE),
                CPL = divisionby0(sum(amount_spent, na.rm = TRUE), sum(results, na.rm = TRUE)))%>%
      ggplot(aes(x = day)) +
      geom_point(aes(y = CPM), color = "steelblue", size =2) +
      geom_text(mapping = aes(y = CPM,
                             label = round(CPM, digits = 0)),
               position = position_nudge(y = +30))+
      geom_line(aes(y = CPM), color = "steelblue", size = 1, linetype = "dashed") +
      ylab("CPM (RUB)")+ xlab("Days") +
      theme_classic()
  
 # Here is the plots that shows the evolution of CPC and CPL over the period
    plot_CPC_CPL <- 
      facebook_ads_data %>%
      filter(ad_set_name == adsetnames[i],
             impressions != 0) %>%
      group_by(day) %>%
      summarise(Impressions = sum(impressions, na.rm = TRUE),
                "Amount Spent (RUB)" = sum(amount_spent, na.rm = TRUE),
                CPM = (sum(amount_spent, na.rm = TRUE) / sum(impressions, na.rm = TRUE)) * 1000,
                Clicks = sum(link_clicks, na.rm = TRUE),
                CPC = divisionby0(sum(amount_spent, na.rm = TRUE), sum(link_clicks, na.rm = TRUE)),
                Leads = sum(results, na.rm = TRUE),
                CPL = divisionby0(sum(amount_spent, na.rm = TRUE), sum(results, na.rm = TRUE)))%>%
      ggplot(aes(x = day)) +
      geom_point(aes(y = CPC, colour = "CPC"), size = 2) +
      geom_text(mapping = aes(y = CPC,
                              label = round(CPC, digits = 0)),
                position = position_nudge(y = +30))+
      geom_line (aes(y = CPC), color = "blue 4", size = 1, linetype = "dashed") +
      geom_point(aes(y = CPL, colour = "CPL"), size = 2) +
      geom_text(mapping = aes(y = CPL,
                              label = round(CPL, digits = 0)),
                position = position_nudge(y = +30))+
      geom_line (aes(y = CPL), color = "salmon3", size = 1, linetype = "dashed")+
      ylab("RUB") + xlab("Days")+
      scale_colour_manual(values=c("blue 4","salmon3")) +
      labs(colour = "")+
      theme_classic() %+replace%
      theme(legend.position = "bottom",
            legend.direction = "horizontal") 
      
    
    pdf(paste(substring(adsetnames[i], 1, 10), i, ".pdf"), height = 11, width = 10)
    text <- textGrob(paste("REPORT: ", "Ad set: ", adsetnames[i]), gp = gpar(fontsize = 20))
    temp_df_gtable <- tableGrob(temp_df_summary)
    grid.arrange(text, temp_df_gtable,plot_impressions, plot_CPM, plot_CPC_CPL, layout_matrix = rbind(c(1, 1), c(2, 2), c(2, 2), c(3, 3), c(3, 3), c(4, 4), c(4, 4), c(5, 5), c(5, 5)))
    dev.off()
}

# This function is used to show which ads is over a KPI 
least_effective_ads <- function(KPI, maximum) {
  for (i in 1:length(adsetnames)) {
    least_effect <- facebook_ads_data %>%
      filter(ad_set_name == adsetnames[i],
             impressions != 0) %>%
      group_by(ad_name) %>%
      summarise(Impressions = sum(impressions),
                "Amount Spent (RUB)" = sum(amount_spent, na.rm = TRUE),
                CPM = (sum(amount_spent, na.rm = TRUE) / sum(impressions, na.rm = TRUE)) * 1000,
                Clicks = sum(link_clicks, na.rm = TRUE),
                CPC = divisionby0(sum(amount_spent, na.rm = TRUE), sum(link_clicks, na.rm = TRUE)),
                Leads = sum(results, na.rm = TRUE),
                CPL = divisionby0(sum(amount_spent, na.rm = TRUE), sum(results, na.rm = TRUE))) 
    temp_list <- which(least_effect[,KPI] > maximum)
    adtext <- ""
    if (length(temp_list) == 0){
      print(paste("There are no ads with a ", KPI, " over ", maximum, " in ", adsetnames[i]))
    }
    else {
      adtext <- paste("Ad", temp_list, collapse = ", ")
    print(paste("The ads in the", adsetnames[i], "campaign that have a", KPI, "over", maximum, "are:", adtext, sep =" "))
    }
  }
}

least_effective_ads(KPI = "CPL", maximum = 200)


