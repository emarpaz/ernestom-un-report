library(tidyverse)
#load in data
gapminder_data <- read_csv("/Users/ernestof/Desktop/un-report/data/gapminder_data.csv")

#using dplyr
# summarizing our data

summarise(gapminder_data, averageLifeExp = mean(lifeExp))

gapminder_data %>% 
  summarise(averageLifeExp = mean(lifeExp))

gapminder_data_summarized <- gapminder_data %>%
  summarise(averageLifeExp = mean(lifeExp))

#filtering our data using filter()
gapminder_data %>% 
  filter(year == 2007) %>%
  summarize(average=mean(lifeExp))

gapminder_data %>%
  filter(year == min(year)) %>%
  summarize(average_gdp=mean(gdpPercap))

#grouping data
gapminder_data %>%
  group_by(year) %>%
  summarise(average=mean(lifeExp))

gapminder_data %>%
  group_by(continent) %>%
  summarise(average = mean(lifeExp), min = min(lifeExp))

#add new column usinf mutate()
gapminder_data %>%
  mutate(gdp = pop * gdpPercap,
         popInMillions = pop/1000000)

# subset columns or change their order wit select()
gapminder_data %>%
  select(pop,year)


gapminder_data %>%
  select(gdpPercap, everything())

#using tidyr to move between long and wide
#pivot_wider() and pivot_longer()

gapminder_data %>%
  select(country, continent,year,lifeExp) %>%
  pivot_wider(names_from = year, values_from = lifeExp)


#combine all the functions we learned and prepare dataset for analysis

gapminder_data_2007 <- read_csv("data/gapminder_data.csv") %>%
  filter(year == 2007 & continent == "Americas") %>%
  select(-year, -continent)



