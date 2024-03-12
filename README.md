# LeagueOfDataHarvesting

The following project replicates the backend functionality of well-known statistical web pages in the League of Legends community such as LeagueOfGraphs and Lolalytics to obtain match history information and statistics of specific players as well as overall statistics of players across  the different ranks in the League of Legends game.

# Files

This GitHub repository contains two files that account for the general project: the RMD and HTML file that explains the interpretability and replication of the project, as well as the results and overall objectives.

# Shiny App

Although in the files there are some functions created to analyze particular players for the commodity of the people (although Lolalytics and LeagueOfGraphs do the same purpose in a better way) we have created a Shiny App that can be accessed [here](https://bbanyulsuc3m.shinyapps.io/OPGG/).

# API Key
As we only have access to a Personal API Key, anyone who wants to use the app or replicate the code  must get their own key to use the app in the [Riot Developer Portal](https://developer.riotgames.com/). 
The data of the match history and general statistics of the user may take some time as Personal API Keys are limited to 100 requests every two minutes. Regarding getting information on thousands of matches in the general code, it may even take hours for that matter the number of matches obtained in the project is small to ensure the replicability is fast and shows what it does.
