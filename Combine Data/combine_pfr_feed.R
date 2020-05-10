library(readr)
library(tidyverse)
library(dplyr)
library(rvest)
library(stringr)
library(purrr)
library(xml2)
library(glue)
library(nflfastR)

# load nfl team data
teams <- teams_colors_logos %>%
  select(team_abbr, team_name, team_id)

# scarpe combine data
combine <- do.call(rbind, lapply(2000:2019, function (y) {
  
  ## Read table from ProFootballReference
  raw_data <- do.call(rbind, lapply(c("QB", "WR", "TE", "RB", "FB", "OL", "OT", "OG", "C", "DE", "DT", "DL", "EDGE",
                                      "ILB", "OLB", "LB", "SS", "FS", "S", "CB", "LS", "K", "P"), function(pos) {
                                        ### URL to be scraped
                                        url <- paste0("https://www.pro-football-reference.com/play-index/nfl-combine-results.cgi?request=1&year_min=",
                                                      y,"&year_max=",y,"&pos%5B%5D=", pos,"&show=p&order_by=year_id")   
                                        ### Read webpage HTML code
                                        webpage <- read_html(url)
                                        
                                        ### scrape table
                                        temp <- html_table(webpage)
                                        
                                        ### store into df
                                        data.frame(temp, stringsAsFactors=FALSE)
                                      })) 
  
  ## transform data
  ## split draft information
  raw_data <- suppressWarnings(separate(raw_data, col = `Drafted..tm.rnd.yr.`, into = c("team", "round", "pick", "year"), sep = "/")) %>%
    mutate(team = str_trim(team, side = "both"),
           round = as.integer(substr(round, 1, 2)),
           pick = as.integer(substr(pick, 1, nchar(pick)-8)),
           year = as.integer(year)) %>%
    ### convert height into inches
    separate(col = Height, into = c("feet", "inches"), sep = "-") %>%
    mutate(height = as.integer(feet)*12+as.integer(inches)) %>%
    ### replace "" with NA
    mutate_all(list(~na_if(.,""))) %>%
    ### drop useless columns
    select("combine_year" =  2,
           "full_name" =  3,
           "pos" =  4,
           "college" =  7,
           "height" =  22,
           "weight" =  11,
           "40_y_dash" =  12,
           "vertical_jump" =  13,
           "bench_press" =  14,
           "broad_jump" =  15,
           "3_cone" =  16,
           "20_y_shuttle" =  17,
           "team" =  18,
           "round" =  19,
           "pick" =  20,
           "draft_year" =  21)
  
  ## store into df
  #data.frame(raw_data, stringsAsFactors=FALSE)
  
  message(glue("{y} scrape completed"))
  
  raw_data <- raw_data
}))
  
# add nfl_ids to combine data
# add team ids
combine_draft <- combine %>%
  left_join(teams, by = c("team" = "team_name")) %>%
  select(1:12, team_id, 14:16)

# load roster data
players <- readRDS(url("https://raw.githubusercontent.com/guga31bb/nflfastR-data/master/roster-data/roster.rds")) %>%
  select("season" = 1, 
         "team_id" = 20, 
         "team" = 21, 
         "nfl_id" = 10,
         "gsis_id" = 12, 
         "full_name" = 2, 
         "first_name" = 3, 
         "last_name" = 5, 
         "pos_group" = 8, 
         "pos" = 9, 
         "height" = 18, 
         "weigth" = 19, 
         "jersey_number" = 17, 
         "college" = 16, 
         "birth" = 13, 
         "home_town" = 14, 
         "head_shot_url" = 27, 
         "profile_url" = 28)

# join to player data  
combine_draft_2 <- combine_draft %>%
  # add positional data
  left_join(data.frame(pos = c("QB", "WR", "TE", "RB", "FB", "OL", "OT", "OG", "C",
                               "DE", "DT", "DL", "EDGE", "EDGE", "ILB", "OLB", "OLB", "LB", "SS", "FS", "S", "CB",
                               "LS", "K", "P"),
                       pos_group = c("QB", "WR", "TE", "RB", "RB", "OL", "OL", "OL", "OL",
                                     "DL", "DL", "DL", "DL", "LB", "LB", "LB", "DL", "LB", "DB", "DB", "DB", "DB",
                                     "SPEC", "SPEC", "SPEC"))) %>%
  # join based on name & team
  left_join(distinct(select(players, team_id, full_name, nfl_id_1 = nfl_id)), by = c("team_id" = "team_id", "full_name" = "full_name")) %>%
  # join based on name & position group
  left_join(distinct(select(players, pos_group, full_name, nfl_id_2 = nfl_id)), by = c("pos_group" = "pos_group", "full_name" = "full_name")) %>%
  # join based on name 
  left_join(distinct(select(players, full_name, nfl_id_3 = nfl_id)), by = c("full_name" = "full_name")) %>%
  mutate(nfl_id = coalesce(nfl_id_1, nfl_id_2, nfl_id_3)) %>%
  select(combine_year,
         nfl_id,
         full_name,
         pos_group,
         pos,
         college,
         height,
         weight,
         `40_y_dash`,
         vertical_jump,
         bench_press,
         broad_jump,
         `3_cone`,
         `20_y_shuttle`,
         draft_year,
         round,
         pick,
         team_id) %>%
  distinct()

# analyze missing nfl
error <- combine_draft_2 %>%
  filter(is.na(nfl_id) == T) %>%
  select(combine_year, full_name, pos_group, pos, team_id) %>%
  ## remove special characters, spaces and uppercase letters
  mutate(full_name_alt = gsub(" ", "", full_name),
         full_name_alt = gsub("\\.", "", full_name_alt),
         full_name_alt = gsub("-", "", full_name_alt),
         full_name_alt = gsub("'", "", full_name_alt),
         full_name_alt = tolower(full_name_alt)) %>%
  ## try match based on name & team 
  left_join(select(mutate(players,
                          ### remove special characters, spaces and uppercase letters
                          full_name_alt = gsub(" ", "", full_name),
                          full_name_alt = gsub("\\.", "", full_name_alt),
                          full_name_alt = gsub("-", "", full_name_alt),
                          full_name_alt = gsub("'", "", full_name_alt),
                          full_name_alt = tolower(full_name_alt)),
                   full_name_alt, pos_group, pos, team_id, nfl_id_1 = nfl_id),
            by = c("team_id" = "team_id", "full_name_alt" = "full_name_alt")) %>%
  ## drop columns added by join
  select(combine_year, full_name, full_name_alt, pos_group = pos_group.x, pos = pos.x, team_id, nfl_id_1) %>%
## try match based on name & pos_group 
left_join(select(mutate(players,
                        ### remove special characters, spaces and uppercase letters
                        full_name_alt = gsub(" ", "", full_name),
                        full_name_alt = gsub("\\.", "", full_name_alt),
                        full_name_alt = gsub("-", "", full_name_alt),
                        full_name_alt = gsub("'", "", full_name_alt),
                        full_name_alt = tolower(full_name_alt)),
                 full_name_alt, pos_group, pos, team_id, nfl_id_2 = nfl_id),
          by = c("pos_group" = "pos_group", "full_name_alt" = "full_name_alt")) %>%
  ## drop columns added by join
  select(combine_year, full_name, full_name_alt, pos_group, pos = pos.x, team_id = team_id.x, nfl_id_1, nfl_id_2) %>%
  mutate(nfl_id = coalesce(nfl_id_1, nfl_id_2)) %>%
  ## drop irrelevant columns
  select(-nfl_id_1, -nfl_id_2) %>%
  distinct()

# distinguish between fix_data and still erroneous data
fix <- error %>%
  filter(is.na(nfl_id) == F)

# apply fix
combine_data <- combine_draft_2 %>%
  left_join(fix, by = c("full_name" = "full_name")) %>%
  mutate(nfl_id = coalesce(nfl_id.x, nfl_id.y)) %>%
  select(1, nfl_id, 3:18) %>%
  distinct()

# convert column types
combine_data <- combine_data %>%
  mutate(height = as.integer(height),
         bench_press = as.double(bench_press),
         broad_jump = as.double(broad_jump))

# drop irrelevant columns & remove duplicates
combine_data <- combine_data %>%
  select(year = combine_year.x,
         nfl_id,
         height,
         weight,
         `40_y_dash`,
         vertical_jump,
         bench_press,
         broad_jump,
         `3_cone`,
         `20_y_shuttle`) %>%
  distinct()
            
# clean environment
rm(combine, combine_draft, combine_draft_2, error, fix, players, teams)

#rm(raw_data)
write_rds(combine_data, "combine_pfr.rds")

