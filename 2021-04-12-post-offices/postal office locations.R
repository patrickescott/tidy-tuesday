library(tidyverse)

post_offices <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-04-13/post_offices.csv')

open <- numeric(0)

for (i in (1775:2002)) {
  test <- post_offices %>%
    filter(established <= i) %>%
    filter(discontinued >= i | is.na(discontinued)) %>%
    nrow()

  open <- append(open, test)
}

openByYear <- tibble(year = 1775:2002, open = open)

# chart
usps <- openByYear %>%
  ggplot(aes(year, open/1000), group = 1, color = "#333466") +
  geom_area(alpha = 0.5, fill = "#333466") +
  geom_line() +
  geom_hline(yintercept = 0) +
  theme_minimal() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title = element_blank(),
        plot.title.position = "plot") +
  coord_cartesian(clip = "off", expand = F) +
  labs(title = "You had mail",
       subtitle = "Number of open USPS branches per year, thousands",
       caption = "Data: Cameron Blevins and Richard W. Helbock")

ggsave("2020-04-12-post-offices/usps.png", usps, dpi = 144, width = 600/72, height = 338/72)
