
library(shiny)
library(httr)
library(jsonlite)
library(dplyr)
library(stringr)



ui <- fluidPage(
  tags$head(
    tags$style(
      HTML("
        /* Custom CSS */
        .custom-title {
          text-align: center;
          margin-bottom: 20px;
        }
        body {
          background-color: #f0f8ff; /* Light blue background */
        }
      ")
    )
  ),
  div(class = "custom-title",
      h1("League of Data Harvesting")
  ),
  sidebarLayout(
    sidebarPanel(
      textInput("name", "Enter name:"),
      textInput("tagline", "Enter tagline:"),
      textInput("key", "Enter API key:"),
      actionButton("submit", "Submit"),
      verbatimTextOutput("counter_text")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Recent Games", tableOutput("recent_games")),
        tabPanel("KDA per Champion", tableOutput("kda_per_champion")),
        tabPanel("Winrate by Role", plotOutput("plays_by_role")),
        tabPanel("Winrate by Day of the Week", plotOutput("winrate_dayweek"))
      )
    )
  )
)

# Define server logic
server <- function(input, output) {
  
  library(shiny)
  library(httr)
  library(jsonlite)
  library(dplyr)
  library(stringr)
  library(lubridate)
  library(ggplot2)
  
  observeEvent(input$submit, {
    req(input$name, input$tagline, input$key)
    
    # Function to fetch data from Riot API
    riot_api_fetching <- function(x) {
      url <- paste0(x, input$key)
      json <- GET(url = url)
      raw <- rawToChar(json$content)
      fromJSON(raw)
    }
    
    
    delimiter <- "?api_key="
    delimiter2 <- "&api_key="
    
    userid <- riot_api_fetching(paste0("https://europe.api.riotgames.com/riot/account/v1/accounts/by-riot-id/", input$name, "/", input$tagline, delimiter))
    
    puuid_user <- userid$puuid
    
    queue <- "420"
    start <- "0"
    count <- "100"
    
    matchestotal_user <- lapply(puuid_user, function(x) {
      
      if (nchar(x) == 0) {
        return(NULL)  # Skip processing for empty elements
      }
      
      
      matches <- riot_api_fetching(paste0("https://europe.api.riotgames.com/lol/match/v5/matches/by-puuid/", puuid_user, "/ids?","/ids?queue=", queue, "&start=", start, "&count=", count, delimiter2))
      
      Sys.sleep(1.3)
      return(matches)
    }) 
    
    
    
    matchestotal_user_unlisted <- unlist(matchestotal_user)
    
    counter <- 0
    
    matchestotal_user_unlisted_combined <- lapply(matchestotal_user_unlisted, function(x) {
      
      counter <<- counter +1
      
      if (nchar(x) == 0) {
        return(NULL)  # Skip processing for empty elements
      }
      
      apibranch <- riot_api_fetching(paste0("https://europe.api.riotgames.com/lol/match/v5/matches/", x, delimiter))
      
      
      if (counter %% 10 == 0) {
        print(paste(counter, "/", length(matchestotal_user_unlisted), "@", format(Sys.time(), "%H:%M:%S")))
      }
      
      if (!is.null(apibranch$info$gameDuration) && apibranch$info$gameDuration != 0) {
        infomatch <- apibranch$info$participants
        parts <- apibranch$metadata$participants
        bind1 <- cbind(infomatch,parts)
        duration <- apibranch$info$gameDuration
        bind2 <- cbind(duration, bind1)
        euwmatch <- apibranch$metadata$matchId 
        bind3 <- cbind(euwmatch,bind2)
        gameCreation <- apibranch$info$gameCreation
        bind4 <- cbind(gameCreation, bind3)
        Sys.sleep(1.4)
        return(bind4)
      } else {
        return(NULL)  # Returning NULL if duration is null or equal to 0
      }
    })
    
    
    matchestotal_user_unlisted_combined_df <- bind_rows(matchestotal_user_unlisted_combined)
    
  
    
    
    matchestotal_user_unlisted_combined_df_byuser <- matchestotal_user_unlisted_combined_df %>% 
      filter(puuid == puuid_user)
    
    recent_games <- matchestotal_user_unlisted_combined_df_byuser  |>  
      slice_head(n = 10)  |>  
      mutate(KDA = round(((kills + assists) / deaths), 2)) |> 
      mutate(KDA = ifelse(is.na((kills + assists) / deaths), "Perfect", round((kills + assists) / deaths,   2))) |> 
      mutate(duration_min = round(duration/60)) |> 
      mutate(cs = totalAllyJungleMinionsKilled + totalMinionsKilled + totalEnemyJungleMinionsKilled) |> 
      mutate(cs_min = cs/(round(duration/60))) |> 
      mutate(game_result = ifelse(win == TRUE &  duration > 5, "Victory", ifelse(duration < 5, "Remake", "Defeat"))) |> 
      select(championName, kills, deaths, assists, cs_min, duration_min, KDA, game_result)
    
    kda_per_champion <- matchestotal_user_unlisted_combined_df_byuser %>%
      group_by(championName) %>%
      summarise(
        kills = round(mean(kills), 2),
        deaths = round(mean(deaths), 2),
        assists = round(mean(assists), 2),
        KDA = ifelse(deaths == 0, NA, round((kills + assists) / deaths, 2)),
        N_of_Games = n(),
        winrate = paste0(round((mean(win) * 100)), "%")
      ) |> 
      arrange(desc(N_of_Games)) |> 
      slice_head(n=5) |> 
      select(championName,KDA, winrate, N_of_Games)
    
    plays_by_role <- matchestotal_user_unlisted_combined_df_byuser  %>%
      group_by(teamPosition) |> 
      summarise(
        kills = round(mean(kills), 2),
        deaths = round(mean(deaths), 2),
        assists = round(mean(assists), 2),
        KDA = ifelse(deaths == 0, NA, round((kills + assists) / deaths, 2)),
        N_of_Games = n(),
        winrate = paste0(round((mean(win) * 100)), "%")
      ) |> 
      arrange(desc(N_of_Games)) |> 
      slice_head(n=5) |> 
      mutate(teamPosition = ifelse(teamPosition == "", "SUPPORT", teamPosition)) %>%
      select(teamPosition,KDA, winrate, N_of_Games)
    
    winrate_dayweek <- matchestotal_user_unlisted_combined_df_byuser %>%
      mutate(Date = as.POSIXct(gameCreation / 1000, origin = "1970-01-01")) %>%
      group_by(day = str_to_title(wday(Date, label = TRUE, abbr = FALSE))) %>%
      summarize(winrate = paste0(round(mean(win) * 100), "%")) %>%
      arrange(match(day, c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")))%>%
      select(day, winrate)
    
    all_roles <- c("TOP", "JUNGLE", "MIDDLE", "BOTTOM", "SUPPORT")
    
    plays_by_role$teamPosition <- factor(plays_by_role$teamPosition, 
                                         levels = all_roles)
    
    rol_ggplot <- ggplot(plays_by_role, aes(x = teamPosition, y = winrate)) +
      geom_bar(stat = "identity", fill = "darkgreen", color = "darkgreen",alpha=0.2) +
      geom_text(aes(label = paste0("[",KDA," (KDA) - ", N_of_Games, " Games]" )), vjust = -0.5, color = "black", size = 4.5) +
      labs(title = "Winrate and Average KDA by role",
           y = "",
           x = "") +
      theme_classic() +
      geom_hline(yintercept = 50, linetype = "dashed", color = "darkgreen") + 
      theme(axis.text.x = element_text(color="black", size=10)) +
      theme(axis.text.y = element_text(color="black", size=10))
    
    
    all_days <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")
    
    
    winrate_dayweek$day <- factor(winrate_dayweek$day, 
                                         levels = all_days)
    
    winrate_dayweek_ggplot <- ggplot(winrate_dayweek, aes(x = day, y = winrate))  +
      geom_bar(stat = "identity", fill = "darkgreen", color = "darkgreen",alpha=0.2) +
      labs(title = "Winrate by day of the week",
           y = "",
           x = "") +
      theme_classic() +
      geom_hline(yintercept = 50, linetype = "dashed", color = "darkgreen") + 
      theme(axis.text.x = element_text(color="black", size=10)) +
      theme(axis.text.y = element_text(color="black", size=10))
    
    output$recent_games <- renderTable(recent_games)
    output$kda_per_champion <- renderTable(kda_per_champion)
    output$plays_by_role <- renderPlot(rol_ggplot)
    output$winrate_dayweek <- renderPlot(winrate_dayweek_ggplot) 
  
  })
}
 
# Run the Shiny app
shinyApp(ui = ui, server = server)




