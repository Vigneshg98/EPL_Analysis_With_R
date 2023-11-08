
library(tidyverse) 
library(magrittr)
library(shinydashboard)
library(plotly)

df <- read.csv("fifa19_epl.csv", encoding = "UTF-8")[-1]
head(df)

dim(df)


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

df %<>%
  mutate(Height = round((as.numeric(str_sub(Height, start=1,end = 1))*30.48) + (as.numeric(str_sub(Height, start = 3, end = 5))* 2.54)),
         Weight = round(as.numeric(str_sub(Weight, start = 1, end = 3)) / 2.204623))

df %<>% filter(Preferred_Foot %in% c("Left", "Right")) 
df$Preferred_Foot <- as.factor(as.character(df$Preferred_Foot))

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


# Selection of the players
players <- df %>% 
  filter(Name %in% c("E. Hazard", "O. Giroud")) %>% 
  # Unite Name & Club variables
  mutate(Name = paste0(Name, ", ", Club)) %>%
  # Selection abilities of the players
  select(Name,Skill_Moves,Crossing,Finishing,Heading_Accuracy,Short_Passing,Volleys,Dribbling,Curve,FK_Accuracy,Long_Passing,Ball_Control,Acceleration,Sprint_Speed,Agility,Reactions,Balance,Shot_Power,Jumping,Stamina,Strength,Long_Shots,Aggression,Interceptions,Positioning,Vision,Penalties,Composure,Marking,Standing_Tackle,Sliding_Tackle) %>% 
  # Tranform from Variable to Observation
  gather(Exp,Skill_Moves,Crossing,Finishing,Heading_Accuracy,Short_Passing,Volleys,Dribbling,Curve,FK_Accuracy,Long_Passing,Ball_Control,Acceleration,Sprint_Speed,Agility,Reactions,Balance,Shot_Power,Jumping,Stamina,Strength,Long_Shots,Aggression,Interceptions,Positioning,Vision,Penalties,Composure,Marking,Standing_Tackle,Sliding_Tackle, -Name)

summary_stats <- df %>% 
  filter(League == "Premier League") %>% 
  select(Class, Sprint_Speed, Dribbling, Shot_Power, Finishing, Balance, Short_Passing) %>% 
  group_by(Class) %>% 
  summarise_at(vars(Sprint_Speed:Short_Passing), funs(mean)) %>% 
  gather(variables, values, -Class)

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
                title = "ShotPower vs Finishing"
                ,status = "primary"
                ,solidHeader = TRUE 
                ,collapsible = TRUE
                ,selectInput("club_selector", "Select Club:", choices = unique(sort(df$Club)))
                ,plotlyOutput(outputId = "shotpower_vs_finishing_plot")
                ,width=12
              )
            )
    ),
    tabItem(tabName = 'Players',
            fluidRow(
              box(
                title = "Player Comparison"
                ,status = "primary"
                ,solidHeader = TRUE 
                ,collapsible = TRUE 
                ,plotlyOutput(outputId = "players_comparison")
              ),
              box(
                title = "Summary Stats by Position"
                ,status = "primary"
                ,solidHeader = TRUE 
                ,collapsible = TRUE 
                ,plotlyOutput(outputId = "summary_stats_position")
              ),
              box(
                title = "Players Potential Comparison"
                ,status = "primary"
                ,solidHeader = TRUE 
                ,collapsible = TRUE 
                ,selectInput("player_club_selector", "Select Club:", choices = unique(sort(df$Club)))
                ,plotlyOutput(outputId = "player_potential_comparison")
                ,width=12
              )
            )
    ),
    tabItem(tabName = 'Prediction',
            fluidRow(
              box(
                title = "Select your teams!"
                ,status = "primary"
                ,solidHeader = TRUE 
                ,collapsible = TRUE 
                ,selectInput("club_selector_A", "Team A", choices = unique(sort(df$Club)))
                ,selectInput("club_selector_B", "Team B", choices = unique(sort(df$Club)))
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
                   dashboardHeader(title = "EPL Report"),
                   sidebar,
                   body
)

server <- function(input, output, session) {
  output$world_map_plot <- renderPlotly({
    ggplot(numofplayers, aes(long, lat, group = group))+
      geom_polygon(aes(fill = `Number of Player` ), color = "White", show.legend = FALSE)+
      scale_fill_viridis_c(option = "C")+
      theme_void()+
      labs(fill = "Number of Players",
           title = "Number of Players")
  })
  
  output$age_vs_wage_plot <- renderPlotly({
    ggplot(df, aes(Age, Wages))+
      geom_hex()+
      facet_wrap(League~., scales = "free")+
      scale_fill_viridis_c()+
      theme_minimal()+
      labs(title = "Age v/s Wage")
    
  })
  
  output$shotpower_vs_finishing_plot <- renderPlotly({
    selected_club <- input$club_selector
    # Preparing the shot power v/s finishing
    kor <- df %>% 
      filter(Club == selected_club, Class == "Forward") %>% 
      select(Name, Preferred_Foot, Finishing, Shot_Power)
    
    shapiro.test(kor$Finishing); shapiro.test(kor$Shot_Power)
    
    cor.test(kor$Shot_Power, kor$Finishing, method = "pearson")
    cor.test(kor$Shot_Power, kor$Finishing, method = "kendall")
    hypo <- cor.test(kor$Shot_Power, kor$Finishing, method = "spearman")
    
    ggplot(kor, aes(Shot_Power, Finishing, color = Preferred_Foot))+
      geom_text(aes(label = Name))+
      theme_minimal()+
      theme(legend.position = "bottom")+
      geom_jitter(alpha = 0.3, size = 2.5, width = 0.3, height = 0.3)+
      geom_smooth(aes(group = 1), method = "lm", color = "gray40", lty = 2, se = FALSE, size = 0.6)+
      scale_color_manual(values = c("orangered","steelblue"))+
      labs(title = paste("Spearman Correlation Coefficient:", round(hypo$estimate, digits = 2)),
           subtitle = "p-value < 0.05")
  })
  
  output$players_comparison <- renderPlotly({
    ggplot(players, aes(Skill_Moves, Exp, fill = Name))+
      geom_col(position = "fill")+
      coord_flip()+
      scale_fill_manual(values = c("khaki", "seagreen"))+
      theme_minimal()+
      geom_hline(yintercept = 0.5, color = "white", size = 1, linetype = 2)+
      theme(legend.position = "top", axis.text.x=element_blank())+
      labs(title = "Hazard VS Giroud", 
           fill = NULL,x = NULL, y = NULL)
    
  })
  
  output$summary_stats_position <- renderPlotly({
    ggplot(summary_stats, aes(x = Class, y = values, fill = variables)) +
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
      head(10) %>% 
      gather(variable, Exp, -Name)
    
    ggplot(club_df, aes(Name, Exp, fill = variable))+
      geom_col(position = "dodge")+
      geom_text(aes(label = Exp),position = position_dodge(width = 0.9), vjust = -0.5)+
      scale_fill_manual(values = c("orangered", "goldenrod1"))+
      theme_minimal()+
      theme(legend.position = "bottom")+
      labs(fill = NULL, x = NULL, title = selected_club)
  })
  
  
  output$match_prediction_outcome <- renderValueBox({
    valueBox(
      paste0(input$club_selector_A, " v/s ", input$club_selector_B, ""),
      "Match Outcome: ## wins", 
      icon = icon("futbol", lib="font-awesome", class="fas futbol fa-spin"),
      color = "blue"
    )
  })
}


# Run the Shiny app
shinyApp(ui = ui, server = server)



