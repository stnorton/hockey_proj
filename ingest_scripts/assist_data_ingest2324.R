############################
## ASSIST XG DATA: INGEST ##
## 2023-2024 DATA         ##
## SEAN NORTON            ##
## 14 NOVEMBER 2023       ##
############################

## SINGLE UPDATE - CONTAINS DATA UP UNTIL 20 DEC 2023

#LIBRARY------------------------------------------------------------
library(hockeyR)
library(sportyR)
library(ggplot2)
library(tidyverse)
library(lubridate)
##GET DATA----------------------------------------------------------
all_df <- load_pbp('2023-24') #load 23-24 season

#now cuts off at 12/29 - will scrape all days since then 
update_vector <- seq(as.Date("2023-12-30"), Sys.Date() - 1, by = "days")

try_scrape_day <- function(day){
  tryCatch(
    scrape_day(day),
    error = function(e) NULL
  )
}

test <- lapply(update_vector, try_scrape_day) #scrape all days since last update

updated_df <- do.call(rbind.data.frame, test) #bind all days together

#bind together
all_df <- rbind(all_df, updated_df)



##CLEAN DATA-------------------------------------------------------
#API changed how it returns player names, fixing so that it is consistent
convert_names <- function(name) {
  # Use gsub to replace the space with a dot
  result <- gsub(" ", ".", name)
  return(result)
}

# ##CLEAN UPDATES
# name_cols <- c('event_player_1_name', 'event_player_2_name', 'event_player_3_name')
# 
# for(i in 1:length(name_cols)){
#   
#   updated_df[, name_cols[i]] <- sapply(updated_df[, name_cols[i]], convert_names)
#   
#   
# }
# 
# 
# #fix inconsistent names
# 
# names_check_df <- bind_rows(
#   updated_df %>%
#     select(event_player_1_name, event_player_1_id) %>%
#     rename(event_name = event_player_1_name, event_id = event_player_1_id),
#   updated_df %>%
#     select(event_player_2_name, event_player_2_id) %>%
#     rename(event_name = event_player_2_name, event_id = event_player_2_id),
#   updated_df %>%
#     select(event_player_3_name, event_player_3_id) %>%
#     rename(event_name = event_player_3_name, event_id = event_player_3_id)
# )
# 
# names_unique_key <- names_check_df %>% #removes any names that are wrong in API data
#   filter(!grepl("\\s", event_name))
# 
# names_unique_key <- distinct(names_unique_key)
# colnames(names_unique_key) <- c('fixed_name', 'id')

# #fix names for players 1 - 3
# updated_df <- left_join( updated_df, names_unique_key, by = c('event_player_1_id' = 'id'))
# updated_df$event_player_1_name <-  updated_df$fixed_name
# updated_df$fixed_name <- NULL
# 
# updated_df <- left_join(updated_df, names_unique_key, by = c('event_player_2_id' = 'id'))
# updated_df$event_player_2_name <-  updated_df$fixed_name
# updated_df$fixed_name <- NULL
# 
# updated_df<- left_join(updated_df, names_unique_key, by = c('event_player_3_id' = 'id'))
# updated_df$event_player_3_name <- updated_df$fixed_name
# updated_df$fixed_name <- NULL

# #rename wrong aho
# wrong_aho <- which(updated_df$event_player_1_name == "Sebastian.Aho"  &
#                      updated_df$event_team == "New York Islanders" |
#                      updated_df$event_player_2_name == "Sebastian.Aho"  &
#                      updated_df$event_team == "New York Islanders" |
#                      updated_df$event_player_3_name == "Sebastian.Aho"  &
#                      updated_df$event_team == "New York Islanders")
# 
# 
# updated_df[wrong_aho, c('event_player_1_name', 'event_player_2_name', 'event_player_3_name')] <-
#   'Sebastian.Aho(NYI)'


name_cols <- c('event_player_1_name', 'event_player_2_name', 'event_player_3_name')

for(i in 1:length(name_cols)){

  all_df[, name_cols[i]] <- sapply(all_df[, name_cols[i]], convert_names)


}


#fix inconsistent names

names_check_df <- bind_rows(
  all_df %>%
    select(event_player_1_name, event_player_1_id) %>%
    rename(event_name = event_player_1_name, event_id = event_player_1_id),
  all_df %>%
    select(event_player_2_name, event_player_2_id) %>%
    rename(event_name = event_player_2_name, event_id = event_player_2_id),
  all_df %>%
    select(event_player_3_name, event_player_3_id) %>%
    rename(event_name = event_player_3_name, event_id = event_player_3_id)
)

names_unique_key <- names_check_df %>% #removes any names that are wrong in API data
  filter(!grepl("\\s", event_name))

names_unique_key <- distinct(names_unique_key)
colnames(names_unique_key) <- c('fixed_name', 'id')

#fix names for players 1 - 3
all_df <- left_join(all_df, names_unique_key, by = c('event_player_1_id' = 'id'))
all_df$event_player_1_name <- all_df$fixed_name
all_df$fixed_name <- NULL

all_df <- left_join(all_df, names_unique_key, by = c('event_player_2_id' = 'id'))
all_df$event_player_2_name <- all_df$fixed_name
all_df$fixed_name <- NULL

all_df <- left_join(all_df, names_unique_key, by = c('event_player_3_id' = 'id'))
all_df$event_player_3_name <- all_df$fixed_name
all_df$fixed_name <- NULL

#rename wrong aho
wrong_aho <- which(all_df$event_player_1_name == "Sebastian.Aho"  &
                     all_df$event_team == "New York Islanders" |
                     all_df$event_player_2_name == "Sebastian.Aho"  &
                     all_df$event_team == "New York Islanders" |
                     all_df$event_player_3_name == "Sebastian.Aho"  &
                     all_df$event_team == "New York Islanders")


all_df[wrong_aho, c('event_player_1_name', 'event_player_2_name', 'event_player_3_name')] <-
  'Sebastian.Aho(NYI)'

#calculate xg
#updated_df <- calculate_xg(updated_df)

##bind back together
#all_df <- rbind(all_df, updated_df)

#calcualte xg
all_df <- calculate_xg(all_df)

#filter to goals
goals_df <- all_df[which(all_df$event_type == "GOAL"), ]

#subset to only needed columns
assist_df <- goals_df[,c('xg', 'event_player_2_name', 'event_player_3_name', 
                         'x_fixed', 'y_fixed', 'event_team')]

#convert to one zone
assist_df$x_fixed <- abs(assist_df$x_fixed)

# # #rename wrong aho
# wrong_aho_p <- which(assist_df$event_player_2_name == "Sebastian.Aho" &
#                        assist_df$event_team == "New York Islanders")
# wrong_aho_s <- which(assist_df$event_player_3_name == "Sebastian.Aho" &
#                        assist_df$event_team == "New York Islanders")
# wrong_aho <- c(wrong_aho_p, wrong_aho_s)
# 
# assist_df[wrong_aho, c('event_player_2_name', 'event_player_3_name')] <-
#   'Sebastian.Aho(NYI)'



##FUNCTIONS-------------------------------------------------------------------

#function that changes raw names from api to pretty first, last format
reorder_name <- function(full_name) {
  split_name <- strsplit(full_name, "\\.")[[1]]  # Split the string by dot
  last_name <- split_name[length(split_name)]  # Get the last element as last name
  
  # Check for cases where there are multiple initials before the last name
  if (length(split_name) > 2) {
    first_names <- paste(split_name[-length(split_name)], collapse = ".")  # Concatenate first names
    reordered_name <- paste(last_name, first_names, sep = ", ")  # Rearrange and concatenate
  } else {
    reordered_name <- paste(last_name, split_name[1], sep = ", ")  # Rearrange and concatenate
  }
  
  return(reordered_name)
}


get_player_stats <- function(pname, assist_df){
  
  #gets statistics by player
  stopifnot(is.character(pname)) #safety check
  
  #get count of assists
  rel_rows_p <- which(assist_df$event_player_2_name == pname) #primary assists
  n_assist_p <- length(rel_rows_p)
  rel_rows_s <- which(assist_df$event_player_3_name == pname) #secondary assists
  n_assist_s <- length(rel_rows_s)
  
  #create output df
  team <-  assist_df[rel_rows_p[1], 'event_team']
  total_pxg <- sum(assist_df[rel_rows_p, 'xg'], na.rm = T)
  avg_pxg  <- ifelse(total_pxg != 0, total_pxg/n_assist_p, 0)
  total_sxg <-  sum(assist_df[rel_rows_s, 'xg'], na.rm = T)
  avg_sxg <- ifelse(total_sxg != 0, total_sxg/n_assist_s, 0)
  total_axg <- total_pxg + total_sxg
  
  out <- cbind(pname, total_axg, total_pxg, total_sxg, avg_pxg, avg_sxg, team)
  
  return(out)
  
}

##plotting function

get_player_plot <- function(pname, assist_df){
  
  require(ggplot2, hockeyR, sportyR, gridExtra)
  stopifnot(is.character(pname))#safety checks
  
  #fetch data
  df <- assist_df[which(assist_df$event_player_2_name == pname|
                          assist_df$event_player_3_name == pname), ]
  
  #create primary vs secondary indicator variable
  df$p_assist <- ifelse(df$event_player_2_name == pname, 1, 0)
  
  #split data
  plot_prim <- df[which(df$p_assist == 1),c('xg', 'x_fixed', 'y_fixed')]
  plot_second <- df[which(df$p_assist == 0),c('xg', 'x_fixed', 'y_fixed')]
  
  p1 <-
    geom_hockey("nhl", display_range = "attacking_zone") +
    geom_hex(data = plot_prim, 
             aes(x = x_fixed, y = y_fixed, weight = xg),
             show.legend = T)+
    scale_fill_gradient(low = "blue", high = "red")+
    labs(fill = 'xG') +
    theme(
      plot.title = element_text(hjust = 0.5),
      plot.subtitle = element_text(hjust = 0.5),
      plot.caption = element_text(hjust = .9))
  
  p2 <- 
    geom_hockey("nhl", display_range = "attacking_zone") +
    geom_hex(data = plot_second, 
             aes(x = x_fixed, y = y_fixed, weight = xg),
             show.legend = T)+
    scale_fill_gradient(low = "blue", high = "red")+
    labs(fill = 'xG') +
    theme(
      plot.title = element_text(hjust = 0.5),
      plot.subtitle = element_text(hjust = 0.5),
      plot.caption = element_text(hjust = .9))
  
  gridExtra::grid.arrange(p1, p2, ncol = 2,
                          top = paste(pname, "Assist xG Generation 22-23"),
                          left = "Primary",
                          right = "Secondary")
  
}




## ALL PLAYERS DF--------------------------------------------------------------
all_players <- unique(c(unique(assist_df$event_player_2_name), 
                        unique(assist_df$event_player_3_name)))

all_players_out <- lapply(all_players, get_player_stats, assist_df)

all_players_df <- do.call(rbind.data.frame, all_players_out)

## CLEAN PLAYER NAMES-----------------------------------------------------------
player_names <- unique(all_players_df$pname)
player_names <- unname(sapply(player_names, reorder_name))
player_names <- data.frame(name = player_names)

## WRITE OUT-----------------------------------------------------------------------
write.csv(all_players_df, file = '../data/all_ind_ax24.csv')
write.csv(assist_df, file = '../data/all_ax_events24.csv')
write.csv(all_df, file = "../data/all_events24.csv")
write.csv(player_names, file = '../data/player_names_clean24.csv')

