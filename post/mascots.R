library(tidyverse)
library(sf)

dat <- read_tsv("https://raw.githubusercontent.com/tacookson/data/master/japanese-mascots/yuru-gp.txt")

japan_sf <- rnaturalearth::ne_states(country = 'Japan', returnclass = 'sf')

dat %>% 
  rename(name_en = area) %>% 
  inner_join(japan_sf, by = 'name_en')

## add a score to dat which is inversely proportional to the rank
## (first it best), and also scales with the number of participants
dat <- dat %>% 
  group_by(year) %>%
  mutate(year_n = n()) %>%
  ungroup() %>% 
  mutate(year_n = year_n, 
         score_int = (year_n/rank), 
         score_log = (rank/year_n)
         )

dat_summary <- dat %>%
  group_by(area) %>%
  summarize(times_entered = n(), 
            mean_score_int = mean(score_int), 
            median_score_int  = median(score_int), 
            mean_score_log = mean(score_log), 
            median_score_log = median(score_log)
            ) %>%
  rename(name_en = area)

dat_summary %>% 
  pivot_longer(names_to = 'scoring', 
               values_to = 'values', 
               cols = c(-name_en, -times_entered)
               ) %>%
  ggplot(aes(x = times_entered, y = values)) + 
  geom_point()+ 
  stat_smooth() + 
  ggpubr::stat_cor() + 
  facet_grid(scoring~., scales= 'free_y')

japan_sf %>%
  inner_join(dat_summary, by = 'name_en') %>%
  ggplot(aes(fill = mean_score_log)) + 
  geom_sf() + 
  scale_fill_fermenter(palette = "PuRd", na.value = 'black', direction = -1) 
  geom_sf_label(aes(label = name_en))
