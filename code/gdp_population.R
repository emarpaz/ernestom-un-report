# load tidyverse packages
library(tidyverse)

# read in data
gapminder_1997 <- read_csv("/Users/ernestof/Desktop/un-report/gapminder_1997.csv")

# learn more about a function
?read_csv

read_csv(file = "gapminder_1997.csv")

# make a plot
ggplot(data = gapminder_1997) +
  aes(x = `gdpPercap`) +
  labs(x = "GDP Per Capita") +
  aes(y = lifeExp) +
  labs(y = "Life Expectancy") +
  labs(title = "Do people
       - in wealthy countries live longer?") +
  geom_point() +
  aes(color = continent) +
  scale_color_brewer(palette = "Set1") +
  aes(size = pop/1000000) +
  labs(size = "Population (in millions)") +
  aes(shape = continent)

# different color palettes
RColorBrewer::display.brewer.all()

# collapse code to make more concise
ggplot(data = gapminder_1997, aes(x = gdpPercap, y = lifeExp, color = continent, size = pop/1000000)) +
  labs(x = "GDP Per Capita", y = "Life Expectancy", size = "Population (in millions)",
       title = "Do people in wealthy countries live longer?") +
  geom_point() +
  scale_color_brewer(palette = "Set1")


#plotting for data exploration

gapminder_data <- read_csv(file = "/Users/ernestof/Desktop/un-report/gapminder_data.csv")

ggplot(data=gapminder_data)+ 
  aes(x=year,y=lifeExp,color=continent) +
  geom_point()

str(gapminder_data)

ggplot(data=gapminder_data)+ 
  aes(x=year,y=lifeExp,color=continent) +
  geom_line()

ggplot(data=gapminder_data)+ 
  aes(x=year,y=lifeExp,color=continent,group=country) +
  geom_line()

ggplot(data=gapminder_1997, aes(x=continent,y=lifeExp)) +
  geom_violin(aes(fill=continent)) +
  geom_jitter(alpha=0.7) 

ggplot(data=gapminder_1997, aes(x=lifeExp)) +
  geom_dotplot(aes(color=continent,group=continent))


# ggplot2 themes

ggplot(data=gapminder_1997, aes(x=lifeExp)) +
  geom_dotplot(aes(color=continent,group=continent)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1),
        axis.text.y = element_text(angle = 45, vjust = 1, hjust=0.5))


#facet

ggplot(data=gapminder_1997, aes(x=gdpPercap, y=lifeExp)) +
  geom_point() +
  facet_wrap(vars(continent))

ggplot(data=gapminder_1997, aes(x=gdpPercap, y=lifeExp)) +
  geom_point() +
  facet_grid(rows=vars(continent))

ggsave("awesome_plot.jpg",
       width = 6,
       height = 4)


#animated plots
install.packages(c("gganimate","gifski"))
library(gganimate)
library(gifski)

ggplot(data=gapminder_data) +
  aes(x = log(gdpPercap), y=lifeExp,size=pop/1000000,color=continent) +
  geom_point()

#Hans flauser (famous guy for animated plot) plot

ostaticHansPlot <- ggplot(data=gapminder_data) +
  aes(x = log(gdpPercap), y=lifeExp,size=pop,color=continent) +
  geom_point(alpha=0.5) +
  scale_color_brewer(palette="Set1") +
  labs(x="GDP Per Capita",y="Life Expectancy",color="Continent",size="Population (in millions)") +
  theme_classic()

animatedHansPlot <- ostaticHansPlot + 
  transition_states(year, transition_length =1, state_length = 1) +
  ggtitle("{closest_state}")

animatedHansPlot

anim_save("hansAnimatedPlot.gif",
          plot=animatedHansPlot,
          renderer = gifski_renderer())  
  



  
  