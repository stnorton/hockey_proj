###########################
## CLUSTER EMBEDDINGS    ##
## SEAN NORTON           ##
## 29 NOV 2023           ##
###########################

#This script clusters shooters by embedding and plots a shot heatmap for each cluster

##LIBRARY--------------------------------------------------------------------
library(tidyverse)
library(plotly)
library(rgl)
library(cluster)
library(factoextra)
library(hockeyR)
library(sportyR)
set.seed(1017)


##READ IN---------------------------------------------------------------------
embeddings_3d <- read.csv('shooter_embed_3d.csv')
embeddings_full <- read.csv('shooter_embed_full.csv')


#clean up embeddings full from artifacts of pandas/csv
#should automate later
embeddings_full <- embeddings_full[,-c(1)]

#create raw version for clustering
embeddings_raw <- embeddings_full[,c(1:50)]

##CLUSTER-------------------------------------------------------------------------
#k-means clustering
#use elbow method to find optimal number of clusters.
fviz_nbclust(embeddings_raw, kmeans, method = "wss")

#use silhouette method to compare
fviz_nbclust(embeddings_raw, kmeans, method = "silhouette") 

#use gap statistic to cross check
gap_stat <- clusGap(embeddings_raw, FUN = kmeans, nstart = 25,
                    K.max = 10, B = 50)
fviz_gap_stat(gap_stat)

#conflicting results, but will go with three clusters
clusters <- kmeans(embeddings_raw, centers = 3, nstart = 25)

#merge into dataset
embeddings_full$cluster <- clusters$cluster

#get player:cluster key
cluster_key <- embeddings_full[,c('pid','name','cluster')]

##PLOT SHOT HEAT MAPS-------------------------------------------------------------
#read in all events data
all_df <- read.csv('all_events23.csv')

#filter to only shots and goals
shot_df <- all_df[which(all_df$event_type == 'SHOT'|all_df$event_type == 'GOAL'), ]

#get only needed variables
shot_df <- shot_df[,c('xg', 'x_fixed', 'y_fixed', 'event_player_1_name', 'event_type', 'secondary_type')]

#merge in cluster key
shot_df <- merge(shot_df, cluster_key, by.x = 'event_player_1_name', by.y = 'name')

#exploratory analysis
colnames(shot_df)[6] <- 'shot_type'
colnames(shot_df)[1] <- 'name'
shot_df$shot_type <- as.factor(shot_df$shot_type)

cluster_1_stypes <- as.data.frame(table(shot_df$shot_type[which(shot_df$cluster == 1)]))
cluster_2_stypes <- as.data.frame(table(shot_df$shot_type[which(shot_df$cluster == 2)]))
cluster_3_stypes <- as.data.frame(table(shot_df$shot_type[which(shot_df$cluster == 3)]))

pct_calculator <- function(df){
  df$pct <- df$Freq/sum(df$Freq)
  return(df)
}
cluster_1_stypes <- pct_calculator(cluster_1_stypes)
cluster_2_stypes <- pct_calculator(cluster_2_stypes)
cluster_3_stypes <- pct_calculator(cluster_3_stypes)

#plot all 3 stype freqs and display together
plot_stype_pct <- function(df1, df2, df3){
  #require(ggplot2)
  stopifnot(is.data.frame(df1), is.data.frame(df2), is.data.frame(df3))
  
  df1$cluster <- 'Cluster 1'
  df2$cluster <- 'Cluster 2'
  df3$cluster <- 'Cluster 3'
  
  df <- rbind(df1, df2, df3)
  
  ggplot(df, aes(x = Var1, y = pct, fill = cluster))+
    geom_bar(stat = 'identity', position = 'dodge')+
    labs(x = 'Shot Type', y = 'Frequency', title = 'Shot Type Frequency by Cluster')+
    theme(plot.title = element_text(hjust = 0.5), 
          axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
}

plot_stype_pct(cluster_1_stypes, cluster_2_stypes, cluster_3_stypes)
#clustering is not on shot types, very similar distributions

#lets check mean xG by cluster
aggregate(shot_df$xg ~ shot_df$cluster, FUN=mean, na.rm = T)

#and get player means within clusters
player_means <- 
shot_df %>% group_by(name, cluster) %>%
  summarize(mean_xg = mean(xg, na.rm = T))

player_means$cluster <- as.factor(player_means$cluster)


#difference appears real here - cluster 1 takes good shots, cluster 2 poor, cluster 3 middle
#let's see if the difference is in goals - get # and pct of goals by cluster
shot_df$event_type <- as.factor(shot_df$event_type)

cluster_1_goals <- as.data.frame(table(shot_df$event_type[which(shot_df$cluster == 1)]))
cluster_2_goals <- as.data.frame(table(shot_df$event_type[which(shot_df$cluster == 2)]))
cluster_3_goals <- as.data.frame(table(shot_df$event_type[which(shot_df$cluster == 3)]))

cluster_1_goals <- pct_calculator(cluster_1_goals)
cluster_2_goals <- pct_calculator(cluster_2_goals)
cluster_3_goals <- pct_calculator(cluster_3_goals)

#there we go - cluster 2 is elite shooters, cluster 1 is poor shooters, cluster 3 is middle
#difference between xG between cluster 2 and 3 is small though - 2 has good shot selection, good conversion
#cluster 3 has poor shot selection, poor conversion


get_shot_heat_and_goals <- function(name, shots_df){
  
  #require(ggplot, hockeyR, sportyR, gridExtra)
  stopifnot(is.character(name))#safety checks
  
  pname <- reverse_name(name) #change back to API format
  
  #fetch data
  df <- shots_df[which(shots_df$name == pname), ]
  
  shot_heat_xg  <- 
    geom_hockey("nhl", display_range = "attacking_zone") +
    stat_summary_hex(data = df, 
                     aes(x = x_fixed, y = y_fixed, z = xg),
                     show.legend = T)+
    scale_fill_gradient(low = "blue", high = "red")+
    labs(fill = 'xG') 
  
  shot_locations_goals <- 
    geom_hockey("nhl", display_range = "attacking_zone") +
    geom_point(
      data = df,
      aes(x_fixed, y_fixed),
      size = 6,
      shape = ifelse(df$event_type == "GOAL", 19, 1)
    )
  
  gridExtra::grid.arrange(shot_heat_xg, shot_locations_goals, ncol = 2,
                          top = paste(name, "Shot Generation 22-23"),
                          left = "xG",
                          right = "Goals and Shots")
}

##PLOTS BY CLUSTER-----------------------------------------------------------------------------

shot_heat_xg1  <- 
  geom_hockey("nhl", display_range = "attacking_zone") +
  stat_summary_hex(data = shot_df[which(shot_df$cluster == 1), ], 
                   aes(x = x_fixed, y = y_fixed, z = xg),
                   show.legend = T)+
  scale_fill_gradient(low = "blue", high = "red")+
  labs(fill = 'xG') 

shot_locations_goals1 <- 
  geom_hockey("nhl", display_range = "attacking_zone") +
  geom_point(
    data = shot_df[which(shot_df$cluster == 1), ],
    aes(x_fixed, y_fixed),
    size = 2,
    shape = ifelse(shot_df[which(shot_df$cluster == 1), ]$event_type == "GOAL", 19, 1)
  )

shot_heat_xg1 + shot_locations_goals1 + plot_annotation(
  title = "Tier 2 Shot Generation 22-23")

gridExtra::grid.arrange(shot_heat_xg1, shot_locations_goals1, ncol = 2,
                        top =  "Tier 2 Shot Generation 22-23", #tier 2 is cluster 1
                        left = "xG",
                        right = "Goals and Shots")

shot_heat_xg2  <- 
  geom_hockey("nhl", display_range = "attacking_zone") +
  stat_summary_hex(data = shot_df[which(shot_df$cluster == 2), ], 
                   aes(x = x_fixed, y = y_fixed, z = xg),
                   show.legend = T)+
  scale_fill_gradient(low = "blue", high = "red")+
  labs(fill = 'xG') 

shot_locations_goals2 <- 
  geom_hockey("nhl", display_range = "attacking_zone") +
  geom_point(
    data = shot_df[which(shot_df$cluster == 2), ],
    aes(x_fixed, y_fixed),
    size = 2,
    shape = ifelse(shot_df[which(shot_df$cluster == 2), ]$event_type == "GOAL", 19, 1)
  )

shot_heat_xg2 + shot_locations_goals2 + plot_annotation(
  title = "Tier 3 Shot Generation 22-23")

gridExtra::grid.arrange(shot_heat_xg2, shot_locations_goals2, ncol = 2,
                        top =  "Tier 3 Shot Generation 22-23", #tier 3 is cluster 1
                        left = "xG",
                        right = "Goals and Shots")


shot_heat_xg3  <- 
  geom_hockey("nhl", display_range = "attacking_zone") +
  stat_summary_hex(data = shot_df[which(shot_df$cluster == 3), ], 
                   aes(x = x_fixed, y = y_fixed, z = xg),
                   show.legend = T)+
  scale_fill_gradient(low = "blue", high = "red")+
  labs(fill = 'xG') 

shot_locations_goals3 <- 
  geom_hockey("nhl", display_range = "attacking_zone") +
  geom_point(
    data = shot_df[which(shot_df$cluster == 3), ],
    aes(x_fixed, y_fixed),
    size = 2,
    shape = ifelse(shot_df[which(shot_df$cluster == 3), ]$event_type == "GOAL", 19, 1)
  )

gridExtra::grid.arrange(shot_heat_xg3, shot_locations_goals3, ncol = 3,
                        top =  "Tier 1 Shot Generation 22-23", #tier 3 is cluster 1
                        left = "xG",
                        right = "Goals and Shots")
shot_heat_xg3 + shot_locations_goals3 + plot_annotation(
  title = "Tier 1 Shot Generation 22-23")



##GET STATS------------------------------------------------------------------------------------
#read in all 22-23 offensive stats
off_stats <- read.csv('off_stats_23.csv')
#remove index columns and hr player ids
off_stats <- off_stats[,-c(1, 15)]

#rename columns
colnames(off_stats) <- c('name', 'team', 'G', 'A', 'PTS', '+/-', 
                         'points_share', 'EVG', 'PPG', 'SHG', 'GWG', 'shots', 'shoot_pct')

#function for converting hockey_reference names to choice names
convert_name <- function(full_name) {
  names <- strsplit(full_name, " ")[[1]]
  
  if (length(names) == 1) {
    return(names)
  }
  
  last_name <- names[length(names)]
  first_names <- paste(names[-length(names)], collapse = " ")
  
  formatted_name <- paste(last_name, first_names, sep = ", ")
  
  return(formatted_name)
}
off_stats$name <- sapply(off_stats$name, convert_name)

#now convert to API name
off_stats$name <- sapply(off_stats$name, reverse_name)
