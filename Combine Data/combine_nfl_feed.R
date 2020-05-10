library(httr)
library(purrr)
library(tidyverse)
library(dplyr)
library(purrr)
library(glue)
library(nflfastR)

#### READ.ME ####
#
# Full Data is only available since 2010, for earlier years, the following restrictions apply
# 2007-2010: no information on arm length  & hand size available
# 2007: no information on height & weight available
#
# Data earlier than 2007 is incomplete and this function is not designed to scrape the data.
# Modifications would be required.
#
# This data is supposed to be used together with roster data scraped e.g. via nflfastR.
# Use nfl_id as primary key to join with the roster data. The roster data will also contain the
# GSIS ID which you need to join with pbp data.
#
#################

# define years to be scraped
year <- 2007:2020

# define & apply scrape function
combine_data <- do.call(rbind, lapply(year, function (year) {
  # access nfl rs feed
  combine_raw <-
    httr::GET(url = glue::glue("http://www.nfl.com/feeds-rs/combine/allProspects/{year}")) %>%
    httr::content(as = "text", encoding = "UTF-8") %>%
    # unnest list
    jsonlite::fromJSON(flatten = TRUE) %>%
    data.frame()
  
  # handsize and armlength are only captured since 2010
  # height and weight are only capture since 2008, however, this data can be pulled from the roster data
  if (year >= 2010) {
    # extract measurements & player info
    measurements <- combine_raw %>%
      ## break up fraction columns for arm length & hand size
      separate(handSizeFraction, into = c("hand_fraction", "hand_full")) %>%
      separate(armLengthFraction, into = c("arm_fraction", "arm_full")) %>%
      ## calculate arm length and hand size as decimal
      mutate(hand_size = as.integer(handSizeInches) + as.integer(hand_fraction)/as.integer(hand_full),
             arm_length = as.integer(armLengthInches) + as.integer(arm_fraction)/as.integer(arm_full)) %>%
      ## drop irrelevant columns and order relevant
      select(year,
             nfl_id = nflId,
             #first_name = firstName,
             #last_name = lastName,
             #college,
             #home_town = homeTown,
             #home_state = homeState,
             #high_school = highSchoolName,
             pos = position,
             height,
             weight,
             hand_size,
             arm_length,
             #expert_grade = expertGrade,
             #esb_id = esbId,
             head_shot_url = headShotURL)
  } else if (year >= 2008) {
    # extract measurements & player info
    measurements <- combine_raw %>%
      ## create NA columns as metrics do not exist
      mutate(hand_size = NA,
             arm_length = NA) %>%
      ## drop irrelevant columns and order relevant
      select(year,
             nfl_id = nflId,
             #first_name = firstName,
             #last_name = lastName,
             #college,
             #home_town = homeTown,
             #home_state = homeState,
             #high_school = highSchoolName,
             pos = position,
             height,
             weight,
             hand_size,
             arm_length,
             #expert_grade = expertGrade,
             #esb_id = esbId,
             head_shot_url = headShotURL)
  } else {
    # extract measurements & player info
    measurements <- combine_raw %>%
      ## create NA columns as metrics do not exist
      mutate(hand_size = NA,
             arm_length = NA,
             height = NA,
             weight = NA) %>%
      ## drop irrelevant columns and order relevant
      select(year,
             nfl_id = nflId,
             #first_name = firstName,
             #last_name = lastName,
             #college,
             #home_town = homeTown,
             #home_state = homeState,
             #high_school = highSchoolName,
             pos = position,
             height,
             weight,
             hand_size,
             arm_length,
             #expert_grade = expertGrade,
             #esb_id = esbId,
             head_shot_url = headShotURL)
  }  
  
  # unnest drill data from scraped df
  ## compile list of player who took part in athletic testing
  drill_participants <- do.call(rbind, lapply(1:nrow(combine_raw), function (x) {
    data.frame(nflId = combine_raw$nflId[x], drill_empty = is.null(combine_raw[["workoutResults"]][[x]]))
  })) %>%
    filter(drill_empty == F) %>%
    pull(nflId)
  
  ## extract drill result data from raw df
  drills <- do.call(rbind, lapply(drill_participants, function (x) {
    temp <- combine_raw %>%
      filter(nflId == x)
    temp[["workoutResults"]][[1]] %>%
      mutate(nfl_id = x) %>%
      select(7, 1:6)
  })) %>%
    ### rename columns and dropp irrelevant columns
    select(nfl_id, drill = workoutName, score = result)
  
  # merge measurement & player info with drill results
  combine <- left_join(measurements, drills, by = c("nfl_id" = "nfl_id")) %>%
    spread(drill, score) %>%
    ## select and rename columns for final df
    select(year,
           nfl_id,
           height,
           weight,
           hand_size,
           arm_length,
           `20_y_shuttle` = `20 Yard Shuttle`,
           `3_cone` = `3 Cone Drill`,
           `40_y_dash` = `40 Yard Dash`,
           bench_press = `Bench Press`,
           broad_jump = `Broad Jump`,
           vertical_jump = `Vertical Jump`,
           combine_head_shot_url = head_shot_url)
  
  # report completion
  message(glue("{year} processing completed"))
  
  # call result
  combine
}))

# clean environment
rm(year)

# store data into .rds file
write_rds(combine_data, "combine_nfl.rds")
