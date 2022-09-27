# This is the only package we'll need to load
library(tidyverse)

# These files are all found on the TidyTuesday GitHub. Download them to your local folder
# https://github.com/rfordatascience/tidytuesday/tree/master/data/2022/2022-09-06
# There are more interesting files - including on colour - that we didn't get round to
inventories <- read_csv("inventories.csv.gz")
inventory_sets <- read_csv("inventory_sets.csv.gz")
sets <- read_csv("sets.csv.gz")


######################################################
############# USING THE PIPE #########################
######################################################

# Getting top 10 largest sets of past decade - using no pipes
sets_last_decade <- filter(sets, year >= 2013)
sets_last_decade_large <- filter(sets_last_decade, num_parts >= 1000)
sets_last_decade_large_sorted <- arrange(sets_last_decade_large, desc(num_parts))
sets_last_decade_large_sorted_top10 <- slice_max(sets_last_decade_large_sorted, num_parts, n = 10)

# Getting top 10 largest sets of past decade - using pipes
# The result is the same as the block above but cuts out
# intermediate steps and is less messy as a result
top10s <- sets %>%
  filter(year >= 2013) %>%
  filter(num_parts >= 1000) %>% # this filter could be merged with the one above if we wanted to
  arrange(desc(num_parts)) %>%
  slice_max(num_parts, n = 10)


######################################################
############# JOINING OUR 3 FILES ####################
######################################################

# Joining all three together. Left join is basically a vlookup but other join
# types - which will be familiar to SQL users - are available
all_df <- inventories %>% # start with inventories then join the other two on
  left_join(inventory_sets, by = "set_num") %>%
  left_join(sets, by = "set_num") 

######################################################
########## ANALYSIS OF LEGO SET SIZES ################
######################################################

# Density plot - for interest
ex_plot <- all_df %>% 
  ggplot(aes(x = num_parts)) +
  geom_density() +
  scale_x_log10()

# Creating some summary stats by year
lego_set_by_year <- sets %>%
  filter(num_parts >= 10) %>% # this was an attempt to filter out things that were not LEGO sets
  group_by(year) %>%
  summarise(number = n(), # this is how to get a count within summarise
            total_parts = sum(num_parts),
            avg_parts = median(num_parts),
            mean_parts = mean(num_parts))

# Quick plot of average parts by year
lego_set_by_year %>%
  ggplot(aes(x = year, y = avg_parts)) +
  geom_line()

######################################################
########## ANALYSIS OF LARGE LEGO SETS ###############
######################################################

# Nesting three functions to convert a year to a decade (numeric for plotting)
as.numeric(paste0(substr(2021, 1, 3), "0"))

lego_large <- sets %>%
  filter(num_parts >= 500) %>% # only keep large sets
  mutate(decade = as.numeric(paste0(substr(year, 1, 3), "0"))) %>%
  group_by(decade) %>%
  count() %>%
  rename(large = n) # renaming this so we can join with a file with identically named columns

# Create this data so we can do % of large sets rather than just total
total_sets <- sets %>%
  mutate(decade = as.numeric(paste0(substr(year, 1, 3), "0"))) %>%
  group_by(decade) %>%
  count()

# Join the two dataframes above and plot % of large sets per decade
lego_large %>%
  left_join(total_sets) %>%
  mutate(pct_large = large/n*100) %>%
  ggplot(aes(x = decade, y = pct_large)) +
  geom_line()



