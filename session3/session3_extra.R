artists <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-09-27/artists.csv')
artists

library(tidytuesdayR)

tt <- tt_load(2022,week = 26)
paygap<- tt$paygap
tt
paygap


afgh <- read_csv("Downloads/2019-03-19-2022-03-28-Afghanistan-Russia-Ukraine-Yemen.csv")

d <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-08-02/frogs.csv")

pop <- read_csv("https://d-learn.ir/wp-content/uploads/2022/07/population_gdp_arable_land.csv")
names(pop)

pop_clean <- pop %>%
  janitor::clean_names()

pop_clean %>%
  filter(iso2c == "IR")
library(stringr)
pop_clean %>%
  filter(str_detect(country , "Iran"))

str(pop_clean)
glimpse(pop_clean)

selected_countries <- c("IR" , "US" , "IN" , "TR" , "UY" , "AE")
pop_clean %>%
  filter(iso2c %in% selected_countries) %>%
  group_by(country) %>%
  summarise(avg = mean(average_precipitation_in_depth_mm_per_year,
                       na.rm = TRUE),
            n = n())
  mutate(country = fct_reorder(country , avg)) %>%
  ggplot(aes(country,avg))+
  geom_col()+
  scale_y_log10()




