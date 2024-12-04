library(dplyr)
library(tidyverse)
library(readr)
library(lubridate)
library(mosaic)
library(ggplot2)
library(mosaic)

Data_73_21 <- read_csv("Data_73_21.csv")

Dat_Cleaned <- Data_73_21 %>% 
  filter(!is.na(Age)) %>%
  mutate(Time = na_if(Time, "NR")) %>% # Replacing "NR" with NA
  mutate(Time = na_if(Time, "")) %>%
  filter(!is.na(Time)) %>%  # Removing observations with missing age or time data
  mutate(Minutes = (period_to_seconds(hms(Time)))/60) %>% # Adding a column for time in minutes
  mutate(Pace_Num = (period_to_seconds(ms(Pace)))/60)


ggplot(Dat_Cleaned, aes(x=Year, y=n(Year), group=Year)) +
  geom_line()

graph1 <- Data_73_21 %>%
  group_by(Year) %>%
  add_count() %>%
  ungroup()
  
graph1 %>%  ggplot(aes(x=Year, y=n)) +
  geom_point() +
  geom_line() +
  scale_x_continuous(trans="identity", limits=c(1973,2021), breaks=seq(1973,2021,4)) +
  scale_y_continuous(trans="identity", limits=c(100,18000), 
                     breaks=c(100, 2500,5000,7500,10000,12500,15000,17500)) +
  labs(y="Number of Participants", title="Number of Runners Participating Per Year", 
       caption="Note: 2020 was virtual only and 2021 was pushed to September, both due to Covid-19") +
  theme_bw() +
  theme(plot.caption.position = "plot",
        plot.caption = element_text(hjust = 0), 
       axis.text.x=element_text(angle = 45, hjust = 1))

fav_stats(graph1$n)
 

graph2 <- Data_73_21 %>% 
  separate(Name, c("Name", "Gender"), sep = -3) %>% 
  mutate(Gender = case_when(Gender == "(M)" ~ "M",
                            Gender == "(W)" ~ "W")) %>% 
  filter(!is.na(Gender))

graph2 %>% ggplot(aes(x=Year, fill=Gender)) +
  geom_bar(position="fill") +
  scale_x_continuous(trans="identity", limits=c(1973,2021), breaks=seq(1973,2021,8)) +
  scale_fill_manual(values = c("M" = "turquoise", "W" = "salmon")) +
  labs(y="Proportion", title = "Proportion of Men and Women Participating Each Year") +
  geom_abline(aes(intercept=0.5, slope=0), color="black") +
  theme_bw()

graph2.1 <- graph2 %>%
  group_by(Year, Gender) %>% 
  add_count() %>%
  ungroup()

favstats(n ~ Gender, data=graph2.1)

graph2 %>% ggplot(aes(x=Year, groups=Gender)) +
  geom_histogram() +
  facet_wrap(~Gender)

graph2 %>% ggplot(aes(x=Year, fill=Gender)) +
  geom_density(alpha=0.5) +
  scale_fill_manual(values = c("M" = "turquoise", "W" = "salmon"))




