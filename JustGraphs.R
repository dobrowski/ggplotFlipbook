

### Load libraries -------

library(tidyverse)
library(here)
library(ggthemes)
library(glue)
library(ggrepel)



### Load data -----

susp.green <- read.csv("Emerald Suspension data.csv")%>%
    arrange(reporting_category) %>%
    mutate(definition = fct_reorder(definition,reporting_category))

caaspp <- read.csv("CAASPP_5th_grade.csv")  %>%
    mutate(Students_Tested = as.numeric(Students_Tested),
           Percentage_Standard_Met_and_Above = as.numeric(Percentage_Standard_Met_and_Above),
           Mean_Scale_Score = as.numeric(Mean_Scale_Score),
           Total_CAASPP_Enrollment = as.numeric(Total_CAASPP_Enrollment),
           Percentage_Standard_Exceeded = 
               as.numeric(Percentage_Standard_Exceeded),
           Percentage_Standard_Met = 
               as.numeric(Percentage_Standard_Met),
           Percentage_Standard_Nearly_Met = 
               as.numeric(Percentage_Standard_Nearly_Met)) %>%
    filter(!is.na(Mean_Scale_Score),
           !is.na(Percentage_Standard_Met_and_Above),
           County_Code %in% c(1:6)) %>%
    mutate(County_Code = as.character(County_Code))



### Scatter plot ----

# slow version
caaspp %>%
    ggplot() +
    aes(y = Percentage_Standard_Exceeded) +
    aes(x = Mean_Scale_Score)  +
    geom_point() +
    aes(shape = County_Code ) + 
    aes(color = County_Code ) +
    aes(size = Students_Tested) +
    aes(alpha = Percentage_Standard_Met_and_Above) +
    aes(color = Mean_Scale_Score) +
    theme(legend.position = "none") 

# standard version 

caaspp %>% 
    ggplot( aes(x = Mean_Scale_Score, 
                y = Percentage_Standard_Exceeded,
                shape = County_Code ,
                size = Students_Tested, 
                alpha = Percentage_Standard_Met_and_Above,
                color = Mean_Scale_Score ) ) +
    geom_point() +
    theme(legend.position = "none") 


### Bar chart -------

# slow 


susp.green %>%
    filter(reporting_category == "TA") %>%
    ggplot( ) +
    aes(x = academic_year) +
    aes(y = as.numeric( suspension_rate_total)/100) +
    geom_col(position = "dodge" ) +
    aes(group = school_name) +
    aes(color = school_name) +
    aes(fill = school_name) +
    labs(title = "Percentage Suspended in Emerald High \ncompared to California over time") +
    labs(caption = "Source: https://www.cde.ca.gov/ds/sd/sd/filessd.asp") +
    theme(legend.position = "bottom") 


# standard


susp.green %>%
    filter(reporting_category == "TA") %>%
    ggplot( aes(x = academic_year,
                y = as.numeric( suspension_rate_total)/100,
            group = school_name, 
            color = school_name,
            fill = school_name) ) +
    geom_col(position = "dodge" ) +
    labs(title = "Percentage Suspended in Emerald High \ncompared to California over time",
         caption = "Source: https://www.cde.ca.gov/ds/sd/sd/filessd.asp") +
    theme(legend.position = "bottom") 


### Facet line chart ------


# slow 


susp.green %>%
    ggplot( ) +
    aes(x = academic_year) +
    aes(y = as.numeric( suspension_rate_total)/100) +
    facet_wrap(~definition) +
    aes(group = school_name) +
    geom_line( show.legend = FALSE ) +
    aes(color = school_name) +
    scale_color_manual(
        values =  c("#000000",  "#CC79A7")) +
    scale_y_continuous(
        labels = scales::label_percent(1)  ) +
    labs(title = "Percentage Suspended in Emerald High \ncompared to California over time") +
    labs(caption = "Source: https://www.cde.ca.gov/ds/sd/sd/filessd.asp") +
    labs(x = "Academic Year") + 
    labs(y = "Suspension Rate") +
    theme(axis.text.x = element_text(color=c(1,0,1,0))) + 
    theme_hc()


# standard


susp.green %>%
    ggplot( aes(x = academic_year,
                y = as.numeric( suspension_rate_total)/100,
                group = school_name,
                color = school_name) ) +
    facet_wrap(~definition) +
    geom_line( show.legend = FALSE ) +
    scale_color_manual(
        values =  c("#000000",  "#CC79A7")) +
    scale_y_continuous(
        labels = scales::label_percent(1)  ) +
    labs(title = "Percentage Suspended in Emerald High \ncompared to California over time",
         caption = "Source: https://www.cde.ca.gov/ds/sd/sd/filessd.asp",
         x = "Academic Year",
         y = "Suspension Rate") +
    theme(axis.text.x = element_text(color=c(1,0,1,0))) + 
    theme_hc()


