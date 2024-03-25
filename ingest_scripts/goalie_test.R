############################
## GOALIE DATA            ##
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
library(rstan)
options(mc.cores = 2)
##GET DATA----------------------------------------------------------
# all_df <- load_pbp('2023-24') #load 23-24 season
# #now cuts off at 12/29 - will scrape all days since then 
# update_vector <- seq(as.Date("2023-12-30"), as.Date('2024-02-01'))
# 
# test <- lapply(update_vector, scrape_day) #scrape all days since last update
# 
# updated_df <- do.call(rbind.data.frame, test) #bind all days together
# 
# #bind together
# all_df <- rbind(all_df, updated_df)
all_df <- read.csv('../data/all_events24.csv')


##CLEAN DATA-------------------------------------------------------
#API changed how it returns player names, fixing so that it is consistent
# convert_names <- function(name) {
#   # Use gsub to replace the space with a dot
#   result <- gsub(" ", ".", name)
#   return(result)
# }
# 
# name_cols <- c('event_player_1_name', 'event_player_2_name', 'event_player_3_name', 'event', 
#                'event_goalie_name')
# 
# for(i in 1:length(name_cols)){
#   
#   all_df[, name_cols[i]] <- sapply(all_df[, name_cols[i]], convert_names)
#   
#   
# }
# 
# 
# #fix inconsistent names
# 
# names_check_df <- bind_rows(
#   all_df %>%
#     select(event_player_1_name, event_player_1_id) %>%
#     rename(event_name = event_player_1_name, event_id = event_player_1_id),
#   all_df %>%
#     select(event_player_2_name, event_player_2_id) %>%
#     rename(event_name = event_player_2_name, event_id = event_player_2_id),
#   all_df %>%
#     select(event_player_3_name, event_player_3_id) %>%
#     rename(event_name = event_player_3_name, event_id = event_player_3_id)
# )
# 
# names_unique_key <- names_check_df %>% #removes any names that are wrong in API data
#   filter(!grepl("\\s", event_name))
# 
# names_unique_key <- distinct(names_unique_key)
# colnames(names_unique_key) <- c('fixed_name', 'id')
# 
# #fix names for players 1 - 3 and event_goalie
# all_df <- left_join(all_df, names_unique_key, by = c('event_player_1_id' = 'id'))
# all_df$event_player_1_name <- all_df$fixed_name
# all_df$fixed_name <- NULL
# 
# all_df <- left_join(all_df, names_unique_key, by = c('event_player_2_id' = 'id'))
# all_df$event_player_2_name <- all_df$fixed_name
# all_df$fixed_name <- NULL
# 
# all_df <- left_join(all_df, names_unique_key, by = c('event_player_3_id' = 'id'))
# all_df$event_player_3_name <- all_df$fixed_name
# all_df$fixed_name <- NULL
# 
# all_df <- left_join(all_df, names_unique_key, by = c('event_goalie_id' = 'id'))
# all_df$event_goalie_name <- all_df$fixed_name
# all_df$fixed_name <- NULL
# 
# #rename wrong aho
# wrong_aho <- which(all_df$event_player_1_name == "Sebastian.Aho"  &
#                      all_df$event_team == "New York Islanders" |
#                      all_df$event_player_2_name == "Sebastian.Aho"  &
#                      all_df$event_team == "New York Islanders" |
#                      all_df$event_player_3_name == "Sebastian.Aho"  &
#                      all_df$event_team == "New York Islanders")
# 
# 
# all_df[wrong_aho, c('event_player_1_name', 'event_player_2_name', 'event_player_3_name')] <-
#   'Sebastian.Aho(NYI)'
# 
# #calculate xg
# #updated_df <- calculate_xg(updated_df)
# 
# ##bind back together
# #all_df <- rbind(all_df, updated_df)
# 
# #calcualte xg
# all_df <- calculate_xg(all_df)

#filter to shots and goals
rel_rows <- which(all_df$event_type == "GOAL" | all_df$event_type == "SHOT")
goalie_df <- all_df[rel_rows, ]

#subset to only needed columns
goalie_df <- goalie_df[,c('event_type', 'xg',  'secondary_type', 'event_goalie_id', 'x_fixed', 'y_fixed', 'event_goalie_name')]

#convert to one zone
goalie_df$x_fixed <- abs(goalie_df$x_fixed)

#create goal indicator
goalie_df$goal <- ifelse(goalie_df$event_type == 'GOAL', 1, 0)

##BIN HEXES-----------------------------------------------------------------------------------
library(sp)

#create polygon to sample on
xcoords <- c(0, 100, 100, 0, 0)
ycoords <- c(43, 43, -43, -43, 43)

ozone_poly <- Polygon(cbind(xcoords, ycoords))
ozone_poly <- Polygons(list(ozone_poly), ID = "A")
ozone_poly <- SpatialPolygons(list(ozone_poly))

#sample points as seeds for polygon
hex_points <- spsample(ozone_poly, type = "hexagonal", cellsize = 4)
hex_grid <- HexPoints2SpatialPolygons(hex_points, dx = 4)
plot(hex_grid)

#convert goalie_df to spatial points
goalie_pts <- goalie_df[, c('x_fixed', 'y_fixed')]
goalie_sp <- SpatialPoints(coords = goalie_df[, c('x_fixed', 'y_fixed')])
polygon_id <- over(goalie_sp, hex_grid)

goalie_df$polygon <- polygon_id



##MODEL---------------------------------------------------------------------------------------
#remove nas
goalie_df_comp <- goalie_df[complete.cases(goalie_df), ]

#scale xg
#goalie_df_comp$xg <- scale(goalie_df_comp$xg)

#get numeric IDs for goalies that make more sense
goalie_df_comp$goalie_id_stan <- as.numeric(as.factor(goalie_df_comp$event_goalie_id))

#get polygon id for stan that works
goalie_df_comp$polygon_stan <- as.numeric(as.factor(goalie_df_comp$polygon))


#create stan data
stan_data <- list(
  J = length(unique(goalie_df_comp$goalie_id_stan)), # num goalies
  P = length(unique(goalie_df_comp$polygon)), #number of shot zones
  N = nrow(goalie_df_comp), # num shots
  jj = as.numeric(goalie_df_comp$goalie_id_stan), # goalie id
  pp = as.numeric(goalie_df_comp$polygon_stan), # shot zone id
  y = as.numeric(goalie_df_comp$goal)
)

#fit model
fit <- stan(file = 'goalie_irt.stan', data = stan_data, chains = 2, iter = 5000)

fit_sum <- as.data.frame(summary(fit, pars = 'alpha')$summary)
fit_sum$goalie_id_stan <- seq(1, nrow(fit_sum))

#merge back to goalie_df
goalie_key_df <- distinct(goalie_df[,c('event_goalie_id', 'event_goalie_name')])
model_key_df <- distinct(goalie_df_comp[,c('event_goalie_id', 'goalie_id_stan')])
goalie_key_df <- left_join(goalie_key_df, model_key_df, by = 'event_goalie_id')

#merge in results
goalie_key_df <- left_join(goalie_key_df, fit_sum, by = 'goalie_id_stan')

#remove NA row
goalie_key_df <- goalie_key_df[complete.cases(goalie_key_df), ]

#sort
goalie_key_df <- goalie_key_df %>% arrange(desc(goalie_key_df$mean))

#get top 25 and their ids to extract proper samples
top_25_id <- goalie_key_df[1:25, c('event_goalie_name', 'goalie_id_stan', 'mean')]

#turn into ordered factor
top_25_id$event_goalie_name <- factor(top_25_id$event_goalie_name, levels = top_25_id$event_goalie_name)

#get alpha samples
alpha_samps <- as.data.frame(t(extract(fit, pars = 'alpha')$alpha))
alpha_samps$goalie_id_stan <- seq(1, nrow(alpha_samps))


#merge alpha samps back to key with names
top_25_samps <- left_join(top_25_id, alpha_samps, by = 'goalie_id_stan')

#reorder by mean
top_25_samps <- top_25_samps %>% arrange(desc(top_25_samps$mean))

plot_df <- top_25_samps %>%
  pivot_longer(cols = starts_with("V"), names_to = "Sample", values_to = "Value")

ggplot(plot_df, aes(x = Value, y = event_goalie_name), alpha = 0.5) +
  geom_density_ridges(quantile_lines=TRUE, quantile_fun=mean) +
  theme_ridges() +
  labs(title = "Latent Goalie Ability (IRT)", x = "Ability", y = element_blank())
