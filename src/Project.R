
library(tidyverse) 
library(magrittr)
library(shinydashboard)
library(plotly)
library(scales)

df <- read.csv("fifa19_epl.csv", encoding = "UTF-8")[-1]
head(df)

dim(df)

file_names <- c("2023_2024.csv", "2016_2017.csv", "2017_2018.csv", "2018_2019.csv", "2019_2020.csv", "2020_2021.csv", "2021_2022.csv", "2022_2023.csv")

# Read and combine CSV files row-wise
match_results_df <- bind_rows(lapply(file_names, read.csv))

premierLeague <- c(
  "Arsenal", "Bournemouth", "Brighton & Hove Albion", "Burnley",
  "Cardiff City", "Chelsea", "Crystal Palace", "Everton", "Fulham",
  "Huddersfield Town", "Leicester City", "Liverpool", "Manchester City",
  "Manchester United", "Newcastle United", "Southampton", 
  "Tottenham Hotspur", "Watford", "West Ham United", "Wolverhampton Wanderers"
)

df %<>% mutate(
  League = case_when(
    Club %in% premierLeague ~ "Premier League"
  ),
  Country = case_when(
    League == "Premier League" ~ "UK"
  )
) %>% filter(!is.na(League)) %>% mutate_if(is.factor, as.character)

rm(premierLeague)

head(df)


# Player Value
df$Values <- str_remove_all(df$Value,"€")
df$Values <- str_replace_all(df$Values,"K", "000")
df$Values <- str_remove_all(df$Values,"M")
df$Values <- as.numeric(df$Values)

# Player Wage
df$Wages <- str_remove_all(df$Wage,"€")
df$Wages <- str_replace_all(df$Wages,"K", "000")
df$Wages <- as.numeric(df$Wages)
df <- df  %>% mutate(Values = if_else(df$Values < 1000 , Values * 1000000, Values))

unique(df$Position)
defence <- c("CB", "RB", "LB", "LWB", "RWB", "LCB", "RCB")
midfielder <- c("CM", "CDM","CAM","LM","RM", "LAM", "RAM", "LCM", "RCM", "LDM", "RDM")
df %<>% mutate(Class = if_else(Position %in% "GK", "Goal Keeper",
                               if_else(Position %in% defence, "Defender",
                                       if_else(Position %in% midfielder, "Midfielder", "Forward"))))
rm(defence, midfielder)


# Preparing the world map plot for players in the region
world_map <- map_data("world")
numofplayers <- world_map %>% 
  mutate(region = as.character(region)) %>% 
  left_join((df %>% mutate(Nationality = as.character(Nationality),
                           Nationality = if_else(Nationality %in% "England", 
                                                 "UK", Nationality)) %>%
               count(Nationality, name = "Number of Player") %>%
               rename(region = Nationality) %>%
               mutate(region = as.character(region))), by = "region")

summary_stats <- df %>% 
  filter(League == "Premier League") %>% 
  select(Class, Sprint_Speed, Dribbling, Shot_Power, Finishing, Balance, Short_Passing) %>% 
  group_by(Class) %>% 
  summarise_at(vars(Sprint_Speed:Short_Passing), funs(mean)) %>% 
  gather(Features, values, -Class)


# Function to predict match outcome percentages
predict_match_outcome <- function(home_team, away_team) {
  
  match_results_df <- match_results_df %>%
    mutate_at(vars(FTHG, FTAG), as.numeric)
  
  # Create the dataframe for modeling
  match_results_df_model <- match_results_df %>%
    rowwise() %>%
    mutate(
      team = list(c(HomeTeam, AwayTeam)),
      opponent = list(c(AwayTeam, HomeTeam)),
      goals = list(c(FTHG, FTAG)),
      home = list(c(1, 0))
    ) %>%
    unnest(cols = c(team, opponent, goals, home))
  
  # Train the model
  model <- glm(goals ~ home + team + opponent, 
               family = poisson(link=log),
               data=match_results_df_model)
  
  # Predict the number of goals for the specific match
  home_data <- data.frame(home = 1, team = home_team, opponent = away_team)
  away_data <- data.frame(home = 0, team = away_team, opponent = home_team)
  
  prediction_home <- predict(model, home_data, type = 'response')
  prediction_away <- predict(model, away_data, type = 'response')
  
  k <- 0:7  # Sample value for the number of goals
  
  # Calculate the probability mass function (PMF) using dpois
  # %o% is used to compute the outer product of the two resulting probability vectors. The result is a matrix where each element (i, j) represents the joint probability of the home team scoring i goals and the away team scoring j goals.
  pmf <- dpois(k, prediction_home) %o% dpois(k, prediction_away)
  
  draw <- sum(diag(pmf))
  away <- sum(pmf[upper.tri(pmf)])
  home <- sum(pmf[lower.tri(pmf)])
  
  # Return the percentages
  result <- c(home = home * 100, draw = draw * 100, away = away * 100)
  return(result)
}


# Defining the UI variables

sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("League", tabName = "League", icon = icon("dashboard")),
    menuItem("Players", tabName = "Players", icon = icon("dashboard")),
    menuItem("Prediction", tabName = "Prediction", icon = icon("dashboard"))
  )
)

body <- dashboardBody(
  tabItems(
    # Inside the 'League' tab in the UI
    tabItem(tabName = "League",
            fluidRow(
              box(
                title = "Players in the Region"
                ,status = "primary"
                ,solidHeader = TRUE 
                ,collapsible = TRUE 
                ,plotlyOutput(outputId = "world_map_plot")
              ),
              box(
                title = "Age v/s Wage"
                ,status = "primary"
                ,solidHeader = TRUE 
                ,collapsible = TRUE 
                ,plotlyOutput(outputId = "age_vs_wage_plot")
              )
            ),
            fluidRow(
              box(
                title = "Correlation Analysis for Shot Power and Finishing"
                ,status = "primary"
                ,solidHeader = TRUE 
                ,collapsible = TRUE
                ,selectInput("club_selector", "Select Club:", choices = unique(sort(df$Club)))
                ,plotlyOutput(outputId = "shotpower_vs_finishing_plot")
                ,width=12
              )
            )
    ),
    # Inside the 'Players' tab in the UI
    tabItem(tabName = 'Players',
            fluidRow(
              box(
                title = "Player Comparison",
                status = "primary",
                solidHeader = TRUE,
                collapsible = TRUE,
                selectInput("player_team_selector", "Select Team:", choices = unique(sort(df$Club))),
                uiOutput("player_selector_A_ui"),
                uiOutput("player_selector_B_ui"),
                plotlyOutput(outputId = "players_comparison")
              ),
              box(
                title = "Summary Stats by Position",
                status = "primary",
                solidHeader = TRUE,
                collapsible = TRUE,
                plotlyOutput(outputId = "summary_stats_position")
              ),
              box(
                title = "Best Players from the team",
                status = "primary",
                solidHeader = TRUE,
                collapsible = TRUE,
                selectInput("player_club_selector", "Select Club:", choices = unique(sort(df$Club))),
                plotlyOutput(outputId = "player_potential_comparison"),
                width = 12
              )
            )
    ),
    # Inside the 'Prediction' tab in the UI
    tabItem(tabName = 'Prediction',
            fluidRow(
              box(
                title = "Select your teams!"
                ,status = "primary"
                ,solidHeader = TRUE 
                ,collapsible = TRUE 
                ,selectInput("club_selector_A", "Home Team", choices = unique(sort(match_results_df$HomeTeam)))
                ,selectInput("club_selector_B", "Away Team", choices = unique(sort(match_results_df$HomeTeam)))
                ,plotlyOutput(outputId = "match_prediction")
                ,style = "height: 25vh;"
              ),
              valueBoxOutput("match_prediction_outcome")
            )
    )
  )
)

# Define the UI for the Shiny app
ui = dashboardPage(skin= "blue",
                   dashboardHeader(
                     title = div(
                       HTML('<i class="fa-sharp fa-regular fa-futbol fa-spin"></i>&nbsp;'),
                       "EPL Stats Spotlight",
                       style = "display: flex; align-items: center; font-size: 16px;"
                     )
                     
                   ),
                   sidebar,
                   body
)

server <- function(input, output, session) {
  
  output$world_map_plot <- renderPlotly({
    ggplot(numofplayers, aes(long, lat, group = group))+
      geom_polygon(aes(fill = `Number of Player` ), color = "White", show.legend = FALSE)+
      scale_fill_viridis_c(option = "D")+
      theme_void()+
      labs(fill = "Number of Players",
           title = "Number of Players")
  })
  
  output$age_vs_wage_plot <- renderPlotly({
    gg <- ggplot(df, aes(Age, Wages)) +
      geom_hex() +
      facet_wrap(League ~ ., scales = "free") +
      scale_fill_viridis_c(option = 'D') +
      theme_minimal() +
      labs(title = "Age v/s Wage") +
      scale_y_continuous(labels = scales::label_number_si())
    ggplotly(gg)
  })
  
  
  output$shotpower_vs_finishing_plot <- renderPlotly({
    selected_club <- input$club_selector
    forward_df <- df %>% 
      filter(Club == selected_club, Class == "Forward") %>% 
      select(Name, Preferred_Foot, Finishing, Shot_Power)
    
    # correlation tests to understand the relationship b/w shot_power and finishing
    shapiro.test(forward_df$Finishing); shapiro.test(forward_df$Shot_Power)
    cor.test(forward_df$Shot_Power, forward_df$Finishing, method = "pearson")
    cor.test(forward_df$Shot_Power, forward_df$Finishing, method = "kendall")
    hypo <- cor.test(forward_df$Shot_Power, forward_df$Finishing, method = "spearman")
    
    ggplot(forward_df, aes(Shot_Power, Finishing, color = Preferred_Foot)) +
      geom_text(aes(label = Name)) +
      theme_minimal() +
      theme(legend.position = "bottom") +
      geom_jitter(alpha = 0.3, size = 2.5, width = 0.3, height = 0.3) +
      geom_smooth(aes(group = 1), method = "lm", color = "gray40", lty = 2, se = FALSE, size = 0.6) +
      scale_color_manual(values = c("orangered","steelblue")) +
      labs(
        title = paste(
          "Spearman: rho =", round(hypo$estimate, digits = 2), ", p-value =", ifelse(hypo$p.value < 0.05, "<0.05", round(hypo$p.value, digits = 3)),
          "\n",
          "Pearson: rho =", round(cor.test(forward_df$Shot_Power, forward_df$Finishing, method = "pearson")$estimate, digits = 2), ", p-value =", ifelse(cor.test(forward_df$Shot_Power, forward_df$Finishing, method = "pearson")$p.value < 0.05, "<0.05", round(cor.test(forward_df$Shot_Power, forward_df$Finishing, method = "pearson")$p.value, digits = 3)),
          "\n",
          "Kendall: tau =", round(cor.test(forward_df$Shot_Power, forward_df$Finishing, method = "kendall")$estimate, digits = 2), ", p-value =", ifelse(cor.test(forward_df$Shot_Power, forward_df$Finishing, method = "kendall")$p.value < 0.05, "<0.05", round(cor.test(forward_df$Shot_Power, forward_df$Finishing, method = "kendall")$p.value, digits = 3)),
          "\n",
          "p-value threshold: 0.05"
        )
      ) +
      theme(plot.title = element_text(size = 10, face = "bold"))
  })
  
  
  # Create dynamic UI for player selection based on the selected team
  output$player_selector_A_ui <- renderUI({
    team_selected <- input$player_team_selector
    players_from_team <- unique(sort(df$Name[df$Club == team_selected]))
    
    selectInput("player_selector_A", "Select Player A:", choices = players_from_team)
  })
  
  output$player_selector_B_ui <- renderUI({
    team_selected <- input$player_team_selector
    players_from_team <- unique(sort(df$Name[df$Club == team_selected]))
    
    selectInput("player_selector_B", "Select Player B:", choices = players_from_team)
  })
  
  observe({
    players_from_team_1 <- input$player_selector_A
    players_from_team_2 <- setdiff(unique(sort(df$Name[df$Club == input$player_team_selector])), players_from_team_1)
    updateSelectInput(session, "player_selector_B", choices = players_from_team_2)
  })
  
  output$players_comparison <- renderPlotly({
    player_A <- input$player_selector_A
    player_B <- input$player_selector_B
    
    # Selection of the players
    players_df <- df %>% 
      filter(Name %in% c(player_A, player_B)) %>% 
      # Unite Name & Club variables
      mutate(Name = paste0(Name, ", ", Club)) %>%
      # Selection abilities of the players
      select(Name,Skill_Moves,Crossing,Finishing,Heading_Accuracy,Short_Passing,Volleys,Dribbling,Curve,FK_Accuracy,Long_Passing,Ball_Control,Acceleration,Sprint_Speed,Agility,Reactions,Balance,Shot_Power,Jumping,Stamina,Strength,Long_Shots,Aggression,Interceptions,Positioning,Vision,Penalties,Composure,Marking,Standing_Tackle,Sliding_Tackle) %>% 
      # Transform from Variable to Observation
      gather(Exp,Skill_Moves,Crossing,Finishing,Heading_Accuracy,Short_Passing,Volleys,Dribbling,Curve,FK_Accuracy,Long_Passing,Ball_Control,Acceleration,Sprint_Speed,Agility,Reactions,Balance,Shot_Power,Jumping,Stamina,Strength,Long_Shots,Aggression,Interceptions,Positioning,Vision,Penalties,Composure,Marking,Standing_Tackle,Sliding_Tackle, -Name)
    
    # Plot the comparison
    ggplot(players_df, aes(Exp, Skill_Moves, fill = Name))+
      geom_col(position = "fill")+
      coord_flip()+
      scale_fill_manual(values = c("khaki", "seagreen"))+
      theme_minimal()+
      geom_hline(yintercept = 0.5, color = "white", size = 1, linetype = 2)+
      theme(legend.position = "top", axis.text.x=element_blank())+
      labs(title = paste(player_A, "VS", player_B), 
           fill = NULL,x = NULL, y = NULL)
  })
  
  
  output$summary_stats_position <- renderPlotly({
    ggplot(summary_stats, aes(x = Class, y = values, fill = Features)) +
      geom_bar(stat = "identity", position = "dodge") +
      scale_fill_viridis_d(option = "C") +
      theme_minimal() +
      labs(x = NULL, y = NULL)
  })
  
  
  output$player_potential_comparison <- renderPlotly({
    selected_club <- input$player_club_selector
    club_df <- df %>% 
      filter(Club == selected_club) %>% 
      select(Name, Overall, Potential) %>% 
      arrange(-Overall) %>% 
      head(12) %>% 
      gather(variable, Exp, -Name)
    
    ggplot(club_df, aes(Name, Exp, fill = variable))+
      geom_col(position = "dodge")+
      geom_text(aes(label = Exp),position = position_dodge(width = 0.9), vjust = -0.5)+
      scale_fill_manual(values = c("orangered", "goldenrod1"))+
      theme_minimal()+
      theme(legend.position = "bottom")+
      labs(fill = NULL, x = NULL, title = selected_club)
  })
  
  observe({
    team_A <- input$club_selector_A
    # Get the list of teams excluding the selected team in Team A
    teams_B <- setdiff(unique(sort(match_results_df$HomeTeam)), team_A)
    # Update the choices for Team B
    updateSelectInput(session, "club_selector_B", choices = teams_B)
  })
  
  output$match_prediction_outcome <- renderValueBox({
    
    team_A <- input$club_selector_A
    team_B <- input$club_selector_B
    
    result <- predict_match_outcome(team_A, team_B)
    
    home_progress <- result['home']
    draw_progress <- result['draw']
    away_progress <- result['away']
    
    # Calculate the width of each segment based on the percentage ratio
    home_width <- home_progress
    draw_width <- 100 - (home_progress + away_progress)
    away_width <- away_progress
    
    # Define colors for each segment
    home_color <- "green"
    draw_color <- "orange"
    away_color <- "red"
    
    progress_text <- HTML(paste0(
      "Win percentage of the team<br>",
      team_A, ": ", round(home_progress, 2), "%<br>",
      "Draw: ", round(draw_progress, 2), "%<br>",
      team_B, ": ", round(away_progress, 2), "%"
    ))
    
    progress_html <- tags$div(
      style = "margin-top: 20px; height: 22vh;",
      div(
        class = "progress",
        div(
          class = "progress-bar",
          role = "progressbar",
          "aria-valuenow" = home_progress,
          "aria-valuemin" = "0",
          "aria-valuemax" = "100",
          style = paste0("width: ", home_width, "%; background-color: ", home_color, "; height: 30px;"),
          paste0(team_A)
        ),
        div(
          class = "progress-bar",
          role = "progressbar",
          "aria-valuenow" = draw_progress,
          "aria-valuemin" = "0",
          "aria-valuemax" = "100",
          style = paste0("width: ", draw_width, "%; background-color: ", draw_color, "; height: 30px;"),
          paste0("Draw")
        ),
        div(
          class = "progress-bar",
          role = "progressbar",
          "aria-valuenow" = away_progress,
          "aria-valuemin" = "0",
          "aria-valuemax" = "100",
          style = paste0("width: ", away_width, "%; background-color: ", away_color, "; height: 30px;"),
          paste0(team_B)
        )
      ),
      div(
        style = "margin-top: 10px; text-align: center; font-size: 18px; font-weight: bold; color: white;",
        progress_text
      )
      
    )
    
    valueBox(
      p("Match Prediction Outcome"),
      progress_html,
      color = "blue"
    )
  })
  
}


# Run the Shiny app
shinyApp(ui = ui, server = server)
