
# Visualization for data analysis
# session 2

# Starting dplyr

# what is messy data
# some words on tidytuesday
library(tidyverse)
# packages include: ggplot2, tidyr, doplyr, purrr, tibble, stringr,
# and forcats

## start with dplyr
## filter
?CO2
filter(CO2, Type == "Quebec")

## mutate
data()
CO2
mutate(CO2, index = conc/uptake)

## select
select(CO2 , Plant , Treatment , conc) 

## pipe %>% or |>
CO2 %>%
  pull(conc)

CO2 |>
  pull(conc)

## group by
group_by(CO2, Plant) %>%
  filter(uptake == max(uptake))

group_by(CO2, Plant) %>%
  filter(uptake == min(uptake))
## summarise
group_by(CO2, Plant) %>%
  summarise(
    n= n(),
    mean_uptake = mean(uptake)
  )
## import data


# read file
# https://media.iranopendata.org/IODBU2/2022/08/16/iod-02933-use-media-2022-en.csv
media2 <- read_csv("https://media.iranopendata.org/IODBU2/2022/08/16/iod-02933-use-media-2022-en.csv")
media <- read_csv("Documents/R tutorials/visualisation/data/iod-02933-use-media-2022-en.csv")

media <- media %>%
  janitor::clean_names()

new_names <- c("timestamp", 
               "extend",
               "sources",
               "province",
               "livinglocation",
               "age",
               "gender",
               "edu",
               "d_follow",
               "d_sources",
               "other_shows")

length(media) == length(new_names)

names(media) <- new_names

## take a shot!
glimpse(media)

##factor
media_clean <- media %>%
  mutate(extend = factor(trimws(extend) , levels = c("not at all","very little" , "little",
                                             "a little" , "much" , "very much"),
                         ordered = TRUE))

## to what extend people see the news?

media_clean |>
  #count(extend) |>
  ggplot(aes(extend))+
  geom_bar()+
  labs(
    title = "How people follow news in Iran?",
    y = ""
  )

## how provinces use news
media_clean %>%
  mutate(province = fct_lump(province , 6)) |>
  filter(! province == "Other") |>
  ggplot(aes(province))+
  geom_bar(aes(fill = extend))+
  scale_y_log10()+
  coord_flip()

## which sources are more popular
# count

# no separation
media_clean %>%
  count(sources, sort= TRUE)

## lets clean this...

media_clean |>
  separate_rows(sources, sep = ", ") |>
  ggplot(aes(sources))+
  geom_bar()+
  coord_flip()

## age distribution based on living location
## is there any difference between age mean based on their living location?
media_clean |>
  ggplot(aes(age))+
  geom_histogram(binwidth = 5, color = "white", size = .4)+
  scale_x_continuous(breaks = seq(0, 100, 5))+
  facet_wrap(~livinglocation)+
  theme_light()

media_clean |>
  ggplot(aes(age, color = livinglocation))+
  geom_density()+
  scale_x_continuous(breaks = seq(0, 100, 5))+
  theme_light()

## cleaning
sep_media <- media_clean |>
  separate_rows(sources, sep = ", ") |>
  mutate(sources = str_to_lower(sources)) |>
  mutate(sources = case_when(
    str_starts(string = sources, pattern = "^national")  ~ "IRIB",
    str_detect(sources , "bbc") ~ "BBC",
    str_detect(sources , "telegram") ~ "Telegram", 
    str_starts(sources , "radio") ~ "Radio Farda",
    str_detect(sources , "other") ~ "Other",
    TRUE ~ sources
  ))  |>
  filter(
    !is.na(sources),
    sources != "Other")

## counting sources

sep_media %>%
  count(sources)

sep_media |>  
  count(sources) |>
  filter(n > 10 ,
         !is.na(sources),
         sources != "Other") |>
  mutate(sources = str_to_title(sources)) %>%
ggplot(aes(fct_reorder(sources, n), n))+
  geom_col(color = "red")+
  coord_flip()+
  labs( x= "",
        y= "",
        caption = "plot by ß")+
  theme_light()


## how age affect choosing news sources
sep_media |>
  filter(!sources %in% c("me", "ict")) |>
  ggplot(aes(fct_reorder(sources, age), age))+
  geom_boxplot(color = "navy")+
  geom_jitter(alpha = .1)+
  labs(
    x= "",
    y= "Age"
  )+
  coord_flip()
## how age affecting other shows
media_clean %>%
  separate_rows(other_shows,sep =  ", ") %>%
  separate_rows(other_shows, sep = "or ") %>%
  separate_rows(other_shows, sep = "/ ") %>%
  separate_rows(other_shows, sep = "and ") %>%
  mutate(other_shows = str_to_lower(string = other_shows)) %>%
  mutate(other_shows = trimws(other_shows)) %>%
  add_count(other_shows) %>%
  filter(n > 300) %>%
  group_by(other_shows) %>%
  slice_max(other_shows,n = 3) %>%
  ggplot(aes(other_shows , age))+
  geom_boxplot()+
  coord_flip()

#mutate(sources = str_to_title(sources)) |>
  ggplot(data = sep_media) +
  geom_bar(aes(fct_infreq(sources)))+
  facet_wrap(vars(livinglocation) , scales = "free")+
    coord_flip()

## number of respondents in each province
media_clean |>
  count(livinglocation , sort = TRUE)

media_clean %>%
  ggplot(aes(age))+
  geom_histogram() +
  facet_wrap(vars(livinglocation),nrow = 1)

media_clean %>%
  ggplot(aes(age))+
  geom_density(aes(color = gender))

## what percentage of respondents use sources
View(media_clean)
## lets see top provinces and sources they use
top_provinces <- media_clean %>%
  count(province , sort = TRUE) %>%
  head() %>%
  pull(province)
sep_media |>
  filter(province %in% top_provinces) %>%
  group_by(province, sources) |>
  summarise(
    n = n()
  ) %>%
  slice_max(sources,n = 4, with_ties = FALSE) |>
  ggplot(aes(province , n , fill = sources))+
  geom_col(position = "dodge")+
  coord_flip()

## DECADE sankey

links <- sep_media %>%
  separate_rows(d_sources , sep = ", ") %>%
  #distinct(timestamp, sources, .keep_all = TRUE)%>%
  count( d_sources,sources, sort = TRUE) %>%
  #mutate(d_sources = str_to_lower(d_sources))
  filter(n > 200)

nodes <- data.frame(
  name=c(as.character(links$sources), 
         as.character(links$d_sources)) %>% unique()
)

links$IDsource <- match(links$sources, nodes$name)-1 
links$IDtarget <- match(links$d_sources, nodes$name)-1

library(networkD3)

p <- sankeyNetwork(Links = links, Nodes = nodes,
                   Source = "IDtarget", Target = "IDsource",
                   Value = "n", NodeID = "name", 
                   sinksRight=TRUE)
p

## sources vs. degrees
# how sources are relate to education
sep_media %>%
  group_by(sources) %>%
  summarise(edu = sum(edu == "high school diploma"))

top_media <- sep_media %>%
  count(sources , sort = TRUE) %>%
  head() %>%
  pull(sources)

sep_media %>%
  filter(sources %in% top_media)%>%
  filter(! edu == "other") %>%
  count(sources, edu) %>% 
  mutate(edu = fct_relevel(edu , "associates degree")) %>%
  #filter(!is.na(province),
  #       !is.na(edu),
  #       ! edu == "other") %>%
  #mutate(province = fct_reorder(province , n)) %>%
  ggplot()+
  geom_tile(aes(sources , edu , fill = n))+
  scale_fill_gradient2(low = "blue" , high = "red" , na.value = "black" ,
                       mid = "white" , midpoint = 250)+
  theme(aspect.ratio = 1/5)+
  theme_light()
