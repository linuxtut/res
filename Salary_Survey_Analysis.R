
##                        APPENDIX 1 
# 1. Clearing global environment and load required libraries
rm(list=ls())

library(readr)
library(tidyverse)
library(ggthemes)

setwd("C:/Users/Probook/iCloudDrive/ClassBizAnaBigD/BU51037 - Data Visualization for Business/Assesmnt/assesmnt2")
##getwd()

##                        APPENDIX 2
# 2.LoadIing dataset

salarysurvey <- read_csv("salary_survey.csv" , col_names = TRUE) 
tidy_salarysurvey = na.omit(salarysurvey) # removing Missing Data

head(tidy_salarysurvey)

##                          APPENDIX 3
# 3. Exploratory Data Analysis:
# 3.1.  Countries with most respondent ( at least 100)

Respondent <- as.data.frame(tidy_salarysurvey %>%
                              group_by(Country) %>%
                              summarise(total = n()) %>%
                              arrange(desc(total))) 
  
head(Respondent)


# 3.1.a Visualizing Countries with most respondent

fig1 <- Respondent %>% 
  slice_max(n=10, total) %>% 
  ggplot(.,echo= TRUE, aes(x=reorder(Country, -total), y=total, fill = Country))+
  geom_bar(stat='identity',position="dodge")+
  theme_stata()+
  labs(title = "Figure 1. Total Respondents for Top 10 Countries",
       plot.title = element_text(hjust = 0.5))+
  theme(legend.position="none",
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_text(angle = 0),
        plot.title = element_text(hjust = 0.5))

fig1

# 3.2 The most studied undergraduate degree (UndergradMajor) 

UndergradMajor <- as.data.frame(tidy_salarysurvey %>%
                                  group_by(UndergradMajor) %>%
                                  summarise(total = n()) %>%
                                  slice_max(n=1, total))

head(UndergradMajor)

# 3.3 The mean, median and standard deviation of salaries for the whole sample ?

salary_summary = as.data.frame(tidy_salarysurvey %>%
                                 summarise(Mean = mean(Salary), 
                                           Median = median(Salary), 
                                           Std =sd(Salary)))

salary_summary


# 3.4 Creating a table with the ten countries having the highest mean salary,and their corresponding mean job satisfaction.

Country_mean_data <- as.data.frame(tidy_salarysurvey %>%
                                     group_by(Country) %>%
                                     summarise(Average_Salary= round(mean(Salary),2),
                                               Average_JobStat= round(mean(JobSat),1))%>%
                                     slice_max(n=10, Average_Salary))

Country_mean_data


##                          APPENDIX 4
# 4. Outliers
# 4.1  Creating a new dataset with any 3 countries having more than 100 respondents.

dataOutlier <- subset(tidy_salarysurvey,Country %in% 
                        (Respondent %>%filter(total > 100)
                         %>%sample_n(first(3 )))$Country)

head(dataOutlier)

# 4.1b
dataOutlier <- tidy_salarysurvey %>% filter(.,Country %in% c("Australia","United States","Turkey"))


# 4.2 For the years 2018 and 2019, draw a box-plot of salaries for each of the countries.

fig2 <- dataOutlier %>%
    filter(Year  %in% c(2018,2019) & !is.na(Salary)) %>%
    mutate(Country = reorder(Country, Salary, FUN = median)) %>%    # reorder
    ggplot(aes(Country, log(Salary), color= Country )) +
    geom_boxplot() +
    labs(title="Fig 2. Comparison of Salaries",
       subtitle = "Relationship break down by Country class") +
    theme(panel.background = element_rect(fill = "white"),
        legend.position="none", axis.title.x = element_blank())+
    facet_grid(~Year)
fig2


fig2.1 <- dataOutlier %>%
  filter(Year  %in% c(2020,2019) & !is.na(Salary)) %>%
  mutate(Country = reorder(Country, Salary, FUN = median)) %>%    # reorder
  ggplot(aes(Country, log(Salary), color= Country )) +
  geom_boxplot() +
  #theme_stata()+
  labs(title="Fig 2.1. Comparison of Salaries",
       subtitle = "Relationship break down by Country class") +
  theme(panel.background = element_rect(fill = "white"),
        legend.position="none", axis.title.x = element_blank())+
  facet_grid(~Year)
fig2.1


# 4.3  impact of outliers

fig2.2 <- dataOutlier %>%
  filter(Year  %in% c(2020,2019) & !is.na(Salary) & Salary > 999 & Salary < 297828) %>%
  mutate(Country = reorder(Country, Salary, FUN = median)) %>%    # reorder
  ggplot(aes(Country, log(Salary), color= Country )) +
  geom_boxplot() +
  #theme_stata()+
  labs(title="Fig 2.2. Comparison of Salaries With no Outliers",
       subtitle = "Relationship break down by Country class") +
  theme(panel.background = element_rect(fill = "white"),
        legend.position="none", axis.title.x = element_blank())+
  facet_grid(~Year)
fig2.2


##                          APPENDIX 5
# 5. Trends across Countries
# 5.1 Creating a new data set consisting of the mean salary,number of years of professional coding, and job satisfaction for each country and year

trends <- as.data.frame(tidy_salarysurvey %>% group_by(Year, Country) %>% 
                          summarise(salary= round(mean(Salary),2),
                                    YearsCodePro = round(mean(YearsCodePro),0),
                                    JobSat = round(mean(JobSat),1)))
  
  head(trends)

# 5.2 scatter plots depicting the relationship between salary and number of years of coding

  fig3.1 <- trends %>%
    ggplot( mapping = aes(x =YearsCodePro  , y = log(salary))) + 
    geom_point(aes(color= Country), show.legend = FALSE) +
    scale_x_log10()+
    geom_smooth()+
    theme_few()+
    labs( title = "Fig 3.1: Relation between Years of Professional Coding and Log of Average Salary",
          subtitle = "Relationship break down by Country class",
          y= "Log of Salary" , x ="Number of Years" )+
    theme(panel.background = element_rect(fill = "white"), legend.position="bottom")+
    facet_wrap(. ~ Year)
  
  fig3.1
 
# 5.3 scatter plots depicting the relationship between salary and JobSat
  
  fig3.2 <- trends %>%
    ggplot(mapping = aes(x =JobSat  , y = log(salary))) + 
    geom_point(aes(color= Country), show.legend = FALSE) +
    scale_x_log10()+
    scale_y_continuous(limits = c(0, NA))+
    geom_smooth()+
    theme_few()+
    labs(title = "Fig 3.2: Relation between Job Satisfaction and Log of Average Salary and ",
          subtitle = "Relationship break down by Country class",
          y= "Log of Salary", x = "Avg JobSatisfaction Score")+
    theme(panel.background = element_rect(fill = "white"))+
    facet_grid(. ~ Year)
  fig3.2
  
##                          APPENDIX 6
# 6 .Subsample

Subsample <- subset(tidy_salarysurvey,Country 
            %in% (Respondent %>% filter(total %in% (50:59)))$Country)
  
head(Subsample)

# 6.1 Bar-plot for the number of observations for each country and comment.  

fig4.1 <- Subsample %>% 
  group_by(Year,Country) %>%
  summarise(count = n()) %>%
  ggplot(aes(x= Year, y= count, fill= `Year`)) +
  geom_bar(stat='identity', position='dodge') +
  #theme_economist() +
  labs(title = "Figure 4.1: Number of Observations Per Country")+
  theme(panel.background = element_rect(fill = "white"),
        axis.title = element_blank(),
        legend.position = "none")+
  facet_grid(. ~Country)+
  scale_x_continuous(breaks = 2017:2020) 
    
fig4.1  
    
# 6.2 Density plot for the salaries of individuals grouped by countries. Explain the plot

fig4.2 <- Subsample %>% 
  filter( Salary > 100 & Salary < 297828) %>%
  ggplot() +
  aes(fill = Country, x = log(Salary)) +
  geom_density(alpha = 0.2) +
  labs(title = "Figure 4.2: Smooth Density of Salaries")+
  #theme_economist() +
  theme(panel.background = element_rect(fill = "white"))
fig4.2


# 6.3 A bar plot for the median salary for each country for separate years.

 fig4.3 <- Subsample %>% 
   filter( Salary > 100 & Salary < 297828) %>%
   group_by(Country, Year) %>%
   summarise(Median = median(Salary)) %>%
   ggplot(aes(x= Year, y= Median, fill= `Year`)) +
   geom_bar(stat='identity', position='dodge') +
   #theme_few()+
   labs(title = "Figure 4.3: Median Salary Per Country")+
   theme(panel.background = element_rect(fill = "white"),
         axis.title = element_blank(),
         legend.position = "none")+
   facet_grid(. ~Country)+
   scale_x_continuous(breaks = 2016:2020) 
   
 fig4.3 
 
# 6.4 Using an appropriate visual plot, explain whether respondents working in a bigger organisation have higher salaries generally

 fig4.4 <-tidy_salarysurvey %>% 
   group_by(OrgSize) %>%
   summarise(Mean = mean(Salary)) %>%
   ggplot(aes(x= Mean, y= reorder(OrgSize, -Mean))) +
   geom_bar(stat = "identity",position="dodge") +
   geom_text(aes(label= round(Mean,2)), position=position_stack(vjust=0.5), color ="white")+
   #theme_few()+
  labs(title = "Figure 4.4: Relationship between Mean Salary and Organization Size")+
   theme(panel.background = element_rect(fill = "white"),
         axis.title = element_blank(),legend.position = "none") 
 fig4.4