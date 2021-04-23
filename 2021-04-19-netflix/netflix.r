# load tidyverse - this includes ggplot and stringr
library(tidyverse)

# load the data
netflix_titles <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-04-20/netflix_titles.csv')

# do a couple of filters just for fun (we don't use these beyond this)
movies <- filter(netflix_titles, type == "Movie")
tv <- filter(netflix_titles, type != "Movie")

# example of detecting the presence of text within a longer bit of text
# str_detect returns either a "true" or a "false" depending on the result of
# the search, making it perfect for use in filters
str_detect("Norway, Iceland, United States", "United States")

# Our goal here is to get the number of US films and TV series in the Netflix
# catalogue by year of release date
releaseYears <- netflix_titles %>%
  filter(str_detect(country, "United States")) %>% # detect whether the film is a US film
  group_by(release_year, type) %>%
  summarise(number = n()) %>% # we could just use count() here instead
  ungroup() %>% # we ungroup here because we don't want the release_year grouping for the next bit
  group_by(type) %>% # regroup by type only so that our mutate function will add up all the numbers for a given type
  mutate(total = sum(number)) %>%
  mutate(pct = number/total*100)

library(nsmgplot)

# plot the pct of films in the catalogue by year
releaseYears %>%
  ggplot(aes(x = release_year, y = pct, group = type, color = type)) +
  geom_line() +
  theme_monitor()

########## Directors #############

# Goal here is to find the top five most commonly occurring directors of movies
directors <- netflix_titles %>%
  filter(type == "Movie") %>%
  filter(!is.na(director)) %>% # the ! returns the inverse of the function, keeping everything that ISN'T NA
  group_by(director) %>%
  count() %>%
  ungroup() %>% # ungroup here or else our next line of code will return nonsense
  slice_max(order_by = n, n = 5)

# Bar chart of top five directors
chart <- directors %>%
  ggplot(aes(x = n, y = reorder(director, n))) +
  geom_col(fill = "red", alpha = 0.7) + # alpha is transpareny with 0 being fully transparent and 1 being not transparent
  theme_ns() +
  geom_vline(xintercept = 0) + # add a black line at zero
  theme(panel.grid.major.x = element_line(color = "#dddddd"), # change the gridlines round so they make sense on the bars
        panel.grid.major.y = element_blank())

