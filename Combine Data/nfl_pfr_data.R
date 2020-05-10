library(tidyverse)
library(dplyr)

# load combine data
## nfl feed
combine_nfl <- readRDS("~/nflfastR/Combine Data/combine_nfl.rds")

## pfr feed
combine_pfr <- readRDS("~/nflfastR/Combine Data/combine_pfr.rds")

# combine nfl & pfr feed into one data frame with priority on the nfl feed
combine <- full_join(combine_nfl, combine_pfr, by = c("nfl_id" = "nfl_id")) %>%
  mutate(year = coalesce(year.x, year.y),
         height = coalesce(height.x, height.y),
         weight = coalesce(weight.x, weight.y),
         hand_size = hand_size,
         arm_length = arm_length,
         `40_y_dash` = coalesce(`40_y_dash.x`, `40_y_dash.y`),
         `20_y_shuttle` = coalesce(`20_y_shuttle.x`, `20_y_shuttle.y`),
         `3_cone` = coalesce(`3_cone.x`, `3_cone.y`),
         vertical_jump = coalesce(vertical_jump.x, vertical_jump.y),
         broad_jump = coalesce(broad_jump.x, broad_jump.y),
         bench_press = coalesce(bench_press.x, bench_press.y)) %>%
  select(nfl_id,
         year,
         height,
         weight,
         hand_size,
         arm_length,
         `20_y_shuttle`,
         `3_cone`,
         `40_y_dash`,
         bench_press,
         broad_jump,
         vertical_jump,
         combine_head_shot_url) %>%
  distinct()

# store data
write_rds(combine, "combine.rds")
