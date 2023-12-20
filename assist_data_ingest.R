############################
## ASSIST XG DATA: INGEST ##
## 2022-2023 DATA         ##
## SEAN NORTON            ##
## 14 NOVEMBER 2023       ##
############################

#LIBRARY------------------------------------------------------------
library(hockeyR)
library(sportyR)
library(ggplot2)

##GET DATA----------------------------------------------------------
all_df <- load_pbp('2022-23') #load 22-23 season


##CLEAN DATA-------------------------------------------------------
#rename wrong aho
wrong_aho <- which(all_df$event_player_1_name == "Sebastian.Aho"  &
                     all_df$event_team == "New York Islanders" |
                     all_df$event_player_2_name == "Sebastian.Aho"  &
                     all_df$event_team == "New York Islanders" |
                     all_df$event_player_3_name == "Sebastian.Aho"  &
                     all_df$event_team == "New York Islanders")


all_df[wrong_aho, c('event_player_1_name', 'event_player_2_name', 'event_player_3_name')] <-
  'Sebastian.Aho(NYI)'

#filter to goals
goals_df <- all_df[which(all_df$event_type == "GOAL"), ]

#subset to only needed columns
assist_df <- goals_df[,c('xg', 'event_player_2_name', 'event_player_3_name', 
                         'x_fixed', 'y_fixed', 'event_team')]

#convert to one zone
assist_df$x_fixed <- abs(assist_df$x_fixed)

#rename wrong aho
wrong_aho_p <- which(assist_df$event_player_2_name == "Sebastian.Aho" &
                     assist_df$event_team == "New York Islanders")
wrong_aho_s <- which(assist_df$event_player_3_name == "Sebastian.Aho" &
                       assist_df$event_team == "New York Islanders")
wrong_aho <- c(wrong_aho_p, wrong_aho_s)

assist_df[wrong_aho, c('event_player_2_name', 'event_player_3_name')] <-
  'Sebastian.Aho(NYI)'



##FUNCTIONS-------------------------------------------------------------------

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

## WRITE OUT-----------------------------------------------------------------------
write.csv(all_players_df, file = 'all_ind_ax23.csv')
write.csv(assist_df, file = 'all_ax_events23.csv')
write.csv(all_df, file = "all_events23.csv")
