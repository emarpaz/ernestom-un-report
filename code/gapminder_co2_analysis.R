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

#Data cleaning
read_csv("data/co2-un-data.csv", skip=1) #ignore 1st line of csv (just an example)

read_csv("data/co2-un-data.csv", 
         skip=2,
         col_names = c("region","country","year","series","value","footnotes","source")) #ignore first 2 lines. assign names manually

read_csv("data/co2-un-data.csv", skip=1) %>%
  rename(country = ...2)

read_csv("data/co2-un-data.csv", skip=1) %>%
  rename_all(tolower)


## Practicing select() 

co2_emissions_dirt <- read_csv("data/co2-un-data.csv", 
         skip=2,
         col_names = c("region","country","year","series","value","footnotes","source")) #ignore first 2 lines. assign names manually

co2_emissions_dirt %>% 
  select(country, series, year, value) %>%
  mutate(series = recode(series,
                         "Emissions (thousand metric tons of carbon dioxide)" = "total_emission",
                         "Emissions per capita (metric tons of carbon dioxide)" = "per_capita_emissions")) %>%
  pivot_wider(names_from = series, values_from = value) %>%
  #number of obs per year
  count(year)
  
co2_emissions <- co2_emissions_dirt %>% 
  select(country, series, year, value) %>%
  mutate(series = recode(series,
                         "Emissions (thousand metric tons of carbon dioxide)" = "total_emission",
                         "Emissions per capita (metric tons of carbon dioxide)" = "per_capita_emissions")) %>%
  pivot_wider(names_from = series, values_from = value) %>%
  filter(year == 2005) %>%
  select(-year)


#joining data frames
inner_join(gapminder_data_2007, co2_emissions)

anti_join(gapminder_data_2007, co2_emissions,
          by="country")

co2_emissions <- read_csv("data/co2-un-data.csv", 
                          skip=2,
                          col_names = c("region","country","year","series","value","footnotes","source")) %>%
  select(country,year,series,value) %>%
  mutate(series = recode(series,
                         "Emissions (thousand metric tons of carbon dioxide)" = "total_emission",
                         "Emissions per capita (metric tons of carbon dioxide)" = "per_capita_emissions")) %>%
  pivot_wider(names_from = series, values_from = value) %>%
  filter(year == 2005) %>%
  select(-year) %>%
  mutate(country = recode(country,
                          "Bolivia (Plurin. State of)" = "Bolivia",
                          "United States of America" = "United States",
                          "Venezuela (Boliv. Rep. of)" = "Venezuela"))

#a second anti_jopin
anti_join(gapminder_data_2007, co2_emissions,
          by="country")

gapminder_data_2007 <- read_csv("data/gapminder_data.csv") %>%
  filter(year == 2007 & continent == "Americas") %>%
  select(-year, -continent) %>%
  mutate(country = recode(country,
                          "Puerto Rico" = "United States"))

anti_join(gapminder_data_2007, co2_emissions,
          by="country")

gapminder_data_2007 <- read_csv("data/gapminder_data.csv") %>%
  filter(year == 2007 & continent == "Americas") %>%
  select(-year, -continent) %>%
  mutate(country = recode(country,
                          "Puerto Rico" = "United States")) %>%
  group_by(country) %>%
  summarise(lifeExp = sum(lifeExp * pop)/sum(pop),
            gdpPercap = sum(gdpPercap * pop)/sum(pop),
            pop = sum(pop))

#try inner join again
inner_join(gapminder_data_2007, co2_emissions,by="country")

gapminder_co2 <- inner_join(gapminder_data_2007, co2_emissions,by="country")

gapminder_co2 %>%
  mutate(region = if_else(country == "Canada" | country == "United States" | country == "Mexico",
                          "north",
                          "south"))

write_csv(gapminder_co2, "data/gapminder_co2.csv")





