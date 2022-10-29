# session 2 and 3
v <-c(1, 2, 3, 4, 5, 4, 3, 1, 4)

mean(v)
median(v)
min(v)
max(v)

quantile(v)

quantile(v)[4] - quantile(v)[2]


#boxt plot
data_boxplot <- data.frame(factor = "blood" , pressure = x )

ggplot(data = data_boxplot , aes(factor , pressure))+
  geom_boxplot()

data = data.frame(
  name=c(rep("A",500), rep("B",500), rep("B",500), rep("C",500), rep('D', 100)  ),
  value=c(rnorm(500, 10, 5), rnorm(500, 13, 1), rnorm(500, 18, 1), rnorm(500, 25, 4), rnorm(100, 12, 1) )
)

data = data.frame(
  name = c(rep("A" , 500), rep("B", 500 ), rep("C", 500)),
  value = c(rnorm(n = 500, mean = 5, sd = 3), rnorm(500 , 9, 6) ,
            rnorm(500,12,3))
)
ggplot(data) +
  geom_boxplot(aes(name , value))

ggplot(diamonds) +
  geom_boxplot(aes(clarity, carat , fill = color))
geom_jitter(aes(clarity , carat) , alpha = .02)

### 

head(data)

CO2

head(CO2)

library(dplyr)

filter(.data = CO2 , Type == "Quebec")
filter(CO2 , uptake > 35)

# pipe

CO2 %>%
  filter(Type == "Mississippi") %>%
  filter(uptake > 30) %>%
  filter(conc > 950)

CO2 |>
  filter(Type == "Quebec")
CO2 %>%
  ggplot(aes(conc , uptake))+
  geom_point()

CO2 %>%
  ggplot(aes(Plant , uptake))+
  geom_boxplot()+
  geom_point(alpha = .5)+
  coord_flip()

data %>%
  ggplot(aes(name , value))+
  geom_boxplot()+
  geom_jitter(alpha = 0.2)


CO2 %>%
  select(Plant , uptake)


CO2 %>% 
  mutate(index = conc / uptake) %>%
  mutate(q = index * 2 ) %>%
  head()

library(tidyverse)
install.packages("tidyverse")
library(dplyr)
library(readr)
# read data
media <- read_csv("Downloads/iod-02933-use-media-2022-en.csv")

View(media)

names(media)

new_names <- c("timestams",
               "follow_news",
               "sources",
               "province",
               "living_location",
               "age",
               "gender",
               "education",
               "follow_news_old",
               "sources_old",
               "other_shows")
new_names

## new names

names(media) <- new_names

names(media)

View(media)

glimpse(media)
## how to find distinct values

media %>%
  distinct(follow_news)

media_clean <- media %>%
  mutate(follow_news = factor(follow_news , 
                              levels = c("not at all" , "very little",
                                         "little", "a little", "much", "very much") , 
                              ordered = TRUE))
glimpse(media_clean)

## to what extend people follow news?

media_clean %>%
  ggplot(aes(follow_news))+
  geom_bar()+
  labs(
    title = "how people follow news",
    subtitle = "based on iran open data",
    x = "",
    y = "count"
    )

### number of people who respod to q
library(forcats)
media_clean %>%
  mutate(province = fct_lump(province , 4)) %>%
  filter(! province == "Other") %>%
  ggplot(aes(province , fill = follow_news))+
  geom_bar()+
  scale_y_log10()+
  coord_flip()

media_clean %>%
  count(follow_news)

media_clean %>%
  count(gender)


media_clean %>%
  count(gender) %>%
  filter(!gender == "other") %>%
  ggplot(aes(x = gender , y = n))+
  geom_col()+
  geom_text(aes(label = n))

media_clean %>%
  filter(! gender == "other") %>%
  ggplot(aes(gender))+
  geom_bar()

media_clean %>%
  count(gender) %>%
  ggplot()+
  geom_col(aes(gender , n))+
  geom_label(aes(x = gender,y = n,label = paste("quantity", n)) , family = "Times" , color = "red")


sep_media <- media_clean %>%
  separate_rows(sources , sep = ", ") %>%
  mutate(sources = str_to_lower(sources)) %>%
  mutate(sources = case_when(
    str_starts(sources , pattern = "national") ~ "IRIB",
    str_detect(sources , "bbc") ~ "BBC",
    str_detect(sources , "telegram") ~ "telegram",
    str_starts(sources , "radio") ~ "Radio Farda",
    str_starts(sources, "news") ~ "website news",
    str_detect(sources , "other") ~ "Other",
    is.na(sources) ~ "not and answered" ,
    TRUE ~ sources
  )) %>%
  filter(!sources %in% c("me" , "ict"))

sep_media %>%
  count(sources) %>%
  head(6)%>%
  mutate(sources = fct_reorder(sources, n)) %>%
  ggplot(aes(sources , n))+
  geom_col()+
  coord_flip()

sep_media 
write_csv(sep_media , "sep_media.csv")
library(readxl)

write_excel_csv()

getwd()
d = c(1,2,3)

d <- c(1, 2, 3)
mean(d)

kimia_mean <- function(x){
  mean(x) * 2
}

kimia_mean(d)

