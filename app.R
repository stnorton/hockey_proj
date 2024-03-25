###########################
## NHL ASSISTS DASHBOARD ##
## SEAN NORTON           ##
## 29 NOV 2023           ##
###########################

##LIBRARY--------------------------------------------
library(shiny)
library(tidyverse)
library(shinydashboard)
library(reactable)
library(hockeyR)
library(sportyR)
library(readr)
library(patchwork)
library(lsa)
library(proxy)
library(shinyWidgets)

##FUNCTIONS-------------------------------------------
#functions
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

reverse_name <- function(full_name) {
  #changes pretty name form back to api form (from last, first to first.last)
  split_name <- strsplit(full_name, ", ")[[1]]  # Split the string by comma and space
  last_first <- strsplit(split_name[1], "\\.")[[1]]  # Split the last name and first name by dot
  
  # Check for cases where there are multiple initials before the last name
  if (length(last_first) > 1) {
    last_name <- last_first[1]  # Extract last name
    first_names <- paste(last_first[-1], collapse = ".")  # Concatenate first names
    restored_name <- paste(first_names, last_name, sep = "..")  # Rearrange and concatenate
  } else {
    last_name <- last_first[length(last_first)]  # Extract last name
    first_name <- split_name[2]  # Get first name
    restored_name <- paste(first_name, last_name, sep = ".")  # Rearrange and concatenate
  }
  
  return(restored_name)
}

get_player_plot <- function(name, assist_df){
  
  #require(ggplot, hockeyR, sportyR, gridExtra)
  stopifnot(is.character(name))#safety checks
  
  pname <- reverse_name(name) #change back to API format
  
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
    labs(title = 'Primary', fill = 'xG') +
    theme(
      plot.title = element_text(hjust = 0.5),
      plot.subtitle = element_text(hjust = 0.5),
      plot.caption = element_text(hjust = .9),
      plot.margin = unit(c(0.25, -1, 0, -1), 
                         "inches"))
  
  p2 <- 
    geom_hockey("nhl", display_range = "attacking_zone") +
    geom_hex(data = plot_second, 
             aes(x = x_fixed, y = y_fixed, weight = xg),
             show.legend = T)+
    scale_fill_gradient(low = "blue", high = "red")+
    labs(title = 'Secondary', fill = 'xG') +
    theme(
      plot.title = element_text(hjust = 0.5),
      plot.subtitle = element_text(hjust = 0.5),
      plot.caption = element_text(hjust = .9),
      plot.margin = unit(c(0.25, -1, 0, -1), 
                         "inches"))
  
  p1 + p2 + plot_annotation(
    title = paste(name, "Assist xG Production 22-23"))
  
  #gridExtra::grid.arrange(p1, p2, ncol = 2,
                          #top = paste(name, "Assist xG Generation 22-23"),
                          #left = "Primary",
                          #right = "Secondary")
  
}


player_embedding_lookup <- function(embeddings, embeddings_raw,
                                    pname) {
  
  
  name <- reverse_name(pname) # Reverse to format in dataframe
  
  rel_row <- which(embeddings$name == name) # Find player
  rel_embedding <- embeddings_raw[rel_row, ]
  
  if (length(rel_row) == 0) {
    # Handle the case where the player is not found in embeddings
    return(character(0))
  }
  
  # Compute cosine similarity
  similarities <- apply(embeddings_raw, 1, function(x) cosine(rel_embedding, as.vector(x)))
  
  embeddings$sims <- similarities # Merge back in similarities
  embeddings <- embeddings[order(-embeddings$sims), ]
  
  top_names <- embeddings[2:4, 'name']
  top_names <- unname(sapply(top_names, reorder_name))
  
  return(top_names)
  
}

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
    labs(fill = 'xG') +
    labs(title = 'xG Heatmap')
  
  shot_locations_goals <- 
    geom_hockey("nhl", display_range = "attacking_zone") +
    geom_point(
      data = df,
      aes(x_fixed, y_fixed),
      size = 6,
      shape = ifelse(df$event_type == "GOAL", 19, 1)
    ) +
    labs(title = 'Shots and Goals')
  
  shot_heat_xg + shot_locations_goals + plot_annotation(
    title = paste(name, "Shot Profile"))
 
}


##ASSISTS XG-------------------------------------------
# #load data
# ax_player_df <- read_csv('all_ind_ax23.csv')[2:7]
# #convert to numeric
# 
# assist_df <- read_csv('all_ax_events23.csv')
# player_names <- read_csv('player_names_clean.csv')[,2, drop = T]
# player_names <- sort(player_names)
# #get names - not everyone on assists list is in the embeddings dataframe
# player_names_embed <- read.csv('embed_names_clean.csv')[,2, drop = T]
# player_names_embed <- sort(player_names_embed)
# 
# #convert player df to more display friendly format
# ax_player_clean <- ax_player_df
# ax_player_clean$pname <- sapply(ax_player_clean$pname, reorder_name)
# colnames(ax_player_clean) <- c('Name', 'aXg', 'PaXg', 'SaXg', 'Avg. PaXg',
#                                'Avg. SaXg', 'Team')



##SHOOTER SIMILARITY----------------------------------------------------------------
# embeddings_raw <- embeddings_full_rel[,c(1:50)]
# embeddings_raw <- apply(embeddings_raw, 2, as.numeric)
# embeddings_collapsed <-  t(apply(embeddings_raw, 1, function(row) as.numeric(unlist(row))))
# saveRDS(embeddings_collapsed, 'embeddings_collapsed.rds')

#get at least 100 shot attempt players
#shots_df_nattempts <- shots_df %>% group_by(name) %>% summarise(n_attempts = n())
#shots_df_rel <- shots_df_nattempts[which(shots_df_nattempts$n_attempts >= 50), 'name']
#write.csv(shots_df_rel, 'embed_names_clean.csv')


# embeddings_full <- read.csv('shooter_embed_rel.csv')
# embeddings_full$X <- NULL
# # player_names_rel_rev <- sapply(player_names_embed, reverse_name)
# # embeddings_full_rel <- embeddings_full[which(embeddings_full$name %in% player_names_rel_rev), ]
# # write.csv(embeddings_full_rel, 'shooter_embed_rel.csv')
# embeddings_collapsed_raw <- readRDS('embeddings_collapsed.rds')
# shots_df <- read.csv('shots_df_embed23.csv')
# #shots_df <- shots_df[which(shots_df$name %in% player_names_embed), ]
# shots_df$X <- NULL





## APP-------------------------------------------------------------------------------
## app.R ##
ui <- dashboardPage(
  dashboardHeader(title = "NHL Analytics Dashboard"),
  dashboardSidebar(
    collapsed = FALSE,
    sidebarMenu(
      menuItem("Assists xG", tabName = "axg_dashboard", icon = icon("chart-line")),
      menuItem("Shooter Comps", tabName = "embed_dashboard", icon = icon("chart-line"))
    )
  ),

  ## Body content
  dashboardBody(
    tabItems(
      tabItem(tabName = 'axg_dashboard',
              fluidRow(
                box(
                  title = "Information",
                  width = 12,
                  div(
                    style = "text-align: left; font-size: 14px;",
                    "This tab displays assist xG information for both primary and secondary assists.
                    This shows where a player generates chances and the danger of those chances through shots or rebounds (primary) 
                    and second order shots or rebounds (secondary).", br(), br(),
                    "Data and xG calculation come from the hockeyR package."
                  )
                ),
                box(
                  title = "Season Input",
                  width = 12,
                  selectInput(inputId = 'season',
                              label = 'Season',
                              choices = c('2022-23', '2023-24'),
                             )
                ),
                div(
                  style = "text-align: center; font-size: 14px;",
                  helpText("Data from 2023-24 is current as of December 29th")
                ),
              ),
              fluidRow(
                box(
                  title = "Player Input",
                  width = 12,
                  selectInput(inputId = 'name',
                              label = 'Player Name',
                              choices = NULL)
                ),
                fluidRow(
                  box(plotOutput("plot1", height = 250), 
                      width = 12)
                ),
                fluidRow(
                  box(
                    reactableOutput("ax_table"), width = 12)
                )
              )
      ),
      tabItem(tabName = 'embed_dashboard',
              fluidRow(
                box(
                  title = "Information",
                  width = 12,
                  div(
                    style = "text-align: left; font-size: 14px;",
                    "This tab uses embeddings to find the two most similar shooters to any given player.
                    Embeddings were fit using Tensorflow and are based on shot type, shot distance, shot location,
                    shot angle, shot xG, and team strength state.", br(), 
                    "Players with less than 50 shot attempts in 2023-24 may have unreliable embeddings.",br(),br(),
                    "Data and xG calculation come from the hockeyR package."
                  )
                ),
                box(
                  title = "Season Input",
                  width = 12,
                  selectInput(inputId = 'season2',
                              label = 'Season',
                              choices = c('2022-23', '2023-24'))
                )
              ),
              div(
                style = "text-align: center; font-size: 12px;",
                helpText("Data from 2023-24 is current as of December 29th")
              ),
              fluidRow(
                box(
                  title = "Player Input",
                  width = 12,
                  selectInput(inputId = 'name_embed',
                              label = 'Player Name',
                              choices = NULL)
                ),
                fluidRow(
                  box(plotOutput("plot2", height = 250), 
                      width = 12)
                ),
                fluidRow(
                  box(plotOutput("plot3", height = 250), 
                      width = 12)
                ),
                fluidRow(
                  box(plotOutput("plot4", height = 250), 
                      width = 12)
                )
              )
      )
    )
  )
)

server <- function(input, output, session) { 
  
  ax_player_df <- reactive({
    if(input$season == '2022-23') {
      read_csv('./data/all_ind_ax23.csv')[2:7]
    } else {
      read_csv('./data/all_ind_ax24.csv')[2:7]
    }
  })
  
  assist_df <- reactive({
    if(input$season == '2022-23') {
      read_csv('./data/all_ax_events23.csv')
    } else {
      read_csv('./data/all_ax_events24.csv')
    }
  })
  
  player_names <- reactive({
    if(input$season == '2022-23') {
      read_csv('./data/player_names_clean.csv')[,2, drop = T]
    } else {
      read_csv('./data/player_names_clean24.csv')[,2, drop = T]
    }
  })
  
 sorted_player_names <- reactive({
    sort(player_names())
  })
  
  player_names_embed <- reactive({
    if(input$season2 == '2022-23') {
      read.csv('./data/embed_names_clean.csv')[,2, drop = T]
    } else {
      #note had to reorder manually - fix python script
     read.csv('./data/embed_names_clean24.csv')[,2, drop = T]
    }
  })
  
  sorted_player_names_embed <- reactive({
    sort(player_names_embed())
  })
  
  #convert player df to more display friendly format
  
  ax_player_clean <- reactive({
    data <- ax_player_df()
    data$pname <- sapply(data$pname, reorder_name)
    colnames(data) <- c('Name', 'aXg', 'PaXg', 'SaXg', 'Avg. PaXg', 'Avg. SaXg', 'Team')
    data
  })
  
  
  embeddings_full <- reactive({
    if(input$season2 == '2022-23') {
      read.csv('./data/shooter_embed_rel.csv')[,-1]
    } else {
      read.csv('./data/shooter_embed_full24.csv')[,-1]
    }
  })
  
 # embeddings_full$X <- NULL
  
  embeddings_collapsed_raw <- reactive({
    if(input$season2 == '2022-23') {
      readRDS('./data/embeddings_collapsed.rds')
    } else {
      readRDS('./data/embeddings_collapsed24.rds')
    }
  })
  
  shots_df <- reactive({
    if(input$season2 == '2022-23') {
      read.csv('./data/shots_df_embed23.csv')[,-1]
    } else {
      read.csv('./data/shots_df24.csv')[,-1]
    }
  })
  #shots_df$X <- NULL


  observe({
    updateSelectizeInput(session, 'name', choices = sorted_player_names(), server = TRUE)
    updateSelectizeInput(session, 'name_embed', choices = sorted_player_names_embed(), server = TRUE)
  })
  
  
  
  #create plot
  output$plot1 <- renderPlot({
    
    get_player_plot(name = input$name, assist_df = assist_df())
    
  })
  
  #create table
  output$ax_table <- renderReactable({
    reactable(ax_player_clean(),
              filterable = TRUE,
              defaultColDef = colDef(format = colFormat(digits = 3)))
  })
  

  
  #observe event to get player comps
  player_comps <- reactive({
    req(input$name_embed)
    player_embedding_lookup(pname = input$name_embed, embeddings = embeddings_full(), embeddings_raw = embeddings_collapsed_raw())
  })
  
  
  output$plot2 <- 
    renderPlot({
    get_shot_heat_and_goals(name = input$name_embed, shots_df())
    
  })
  
  # output$plot3 <-   
  #   renderPlot({
  #     
  #     get_shot_heat_and_goals(name = player_comps()[1], shots_df())
  #     
  #   })
  # 
  # output$plot4 <- 
  #     renderPlot({
  #       
  #       get_shot_heat_and_goals(name = player_comps()[2], shots_df())
  #       
  #     })
  # 
  # }

  output$plot3 <- renderPlot({
    comps <- player_comps()
    if (length(comps) >= 3) {
      get_shot_heat_and_goals(name = comps[2], shots_df())
    } else {
      # Display a text message when there are not enough comps
      plot(1, type = "n", xaxt = "n", yaxt = "n", xlab = "", ylab = "")
      text(1, 1, "Not enough similar players", cex = 1.5, col = "red")
    }
  })
  
  output$plot4 <- renderPlot({
    comps <- player_comps()
    if (length(comps) >= 3) {
      get_shot_heat_and_goals(name = comps[3], shots_df())
    } else {
      # Display a text message when there are not enough comps
      plot(1, type = "n", xaxt = "n", yaxt = "n", xlab = "", ylab = "")
      text(1, 1, "Not enough similar players", cex = 1.5, col = "red")
    }
  })
}

shinyApp(ui, server)


