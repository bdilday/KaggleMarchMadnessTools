
library(readr)
library(dplyr)
library(stringr)

data_path = "D:/github/KaggleMarchMadnessTools/inst/extdata/"
tourney_seeds <- read_csv(paste0(data_path, 'TourneySeeds.csv')) %>% filter(Season==2017)
tourney_slots <- read_csv(paste0(data_path, 'TourneySlots.csv')) %>% filter(Season==2017)
df_teams <- read_csv(paste0(data_path, 'Teams.csv'))

get_current_results <- function() {
  current_result = read_csv('https://gist.githubusercontent.com/bdilday/ce511b1eedbebe8d908076e4ab7b0954/raw/results2017.csv')

}

key_to_teams <- function(key) {
team1 = as.integer(as.character(str_split(key, '_')[[1]][[2]]))
team2 = as.integer(as.character(str_split(key, '_')[[1]][[3]]))

sprintf("%s vs. %s",
        df_teams[which(df_teams$Team_Id == team1),]$Team_Name,
        df_teams[which(df_teams$Team_Id == team2),]$Team_Name)

}

importances_for_user <- function(file_id) {
  if (!'current_results' %in% ls()) {
    current_results <- get_current_results()
  }
  game_importance <- data.frame()
  for (game_id in current_results$Id) {
    tmp = game_leverage(all_predictions, file_id, game_id)
    game_importance <- rbind.data.frame(game_importance, tmp)
  }
  game_importance
}

game_leverage <- function(all_predictions, file_id, game_id) {
  s = file_id
  matchup = key_to_teams(game_id)
  all_predictions %>%
    filter(Id==game_id) %>%
    mutate(medPred=median(Pred),
           s0=log(1-Pred), s1=log(Pred),
           Pred=round(Pred, 3), medPred=round(medPred,3),
           m0=median(s0), m1=median(s1), diff_win=(s1-m1), diff_loss=(s0-m0)) %>%
    ungroup() %>%
    dplyr::filter(file_id == s) %>% mutate(matchup=matchup) %>%
    select(matchup, Id, file_id, file_name, Pred, medPred, diff_win, diff_loss)
}

get_all_predictions <- function() {
  all_predictions <- data.frame()
  zip_files <- Sys.glob(paste0(data_path, 'predictions/*zip'))
  for (idx in seq_along(zip_files)) {
    zip_file_name <- zip_files[idx]
    csv_file_name <- unzip(zip_file_name, list=TRUE)$Name

    if ( length(csv_file_name) > 1  ) {
      next
    }
    if (grepl('rar|zip$', csv_file_name)) {
      next
    }

    file_id <- str_replace(zip_file_name, '.zip$', '') %>%
      str_replace(paste0(data_path, 'predictions/'), '')
    file_name <- str_replace(csv_file_name, '.csv$', '')
    df1 <-read_csv(zip_file_name)
    names(df1) <- c("Id", "Pred")

    df1 <- df1 %>%
      mutate(Pred=ifelse(Pred>1-1e-15, 1-1e-15, Pred),
             Pred=ifelse(Pred<1e-15, 1e-15, Pred),
             file_id=file_id, file_name=file_name)
    all_predictions <- rbind.data.frame(all_predictions, df1)
  }

  all_predictions
}

get_results <- function() {


  results_df <- data.frame()
  zip_files <- Sys.glob(paste0(data_path, 'predictions/*zip'))
  for (idx in seq_along(zip_files)) {
    zip_file_name <- zip_files[idx]
    csv_file_name <- unzip(zip_file_name, list=TRUE)$Name

    if ( length(csv_file_name) > 1  ) {
      next
    }
    if (grepl('rar|zip$', csv_file_name)) {
      next
    }

    file_id <- str_replace(zip_file_name, '.zip$', '') %>%
      str_replace(paste0(data_path, 'predictions/'), '')
    file_name <- str_replace(csv_file_name, '.csv$', '')
    df1 <-read_csv(zip_file_name)
    names(df1) <- c("Id", "Pred")
    current_score <- current_result %>% merge(df1, by="Id") %>%
      mutate(Pred=ifelse(Pred>1-1e-15, 1-1e-15, Pred),
             Pred=ifelse(Pred<1e-15, 1e-15, Pred),
             game_score=-result*log(Pred) - (1-result)*log(1-Pred),
             file_id=file_id, file_name=file_name)

    results_df <- rbind.data.frame(results_df, current_score)

  }
  results_df

}

top_results <- function(results_df=NULL) {
  if (is.null(results_df)) {
    results_df < get_results()
  }

  results_df %>%
    group_by(file_id) %>%
    summarise(score=mean(game_score, na.rm=-TRUE)) %>%
    arrange(score) %>% head(20) %>% print.data.frame()
}


fuzzy_id <- function(team1, team2) {
  if (team1 > team2) {
    tmp <- team1
    team1 <- team2
    team2 <- tmp
  }

  xx1 = df_teams %>% filter(grepl(team1, Team_Name, ignore.case=TRUE)) %>% mutate(Season=2017)
  xx2 = df_teams %>% filter(grepl(team2, Team_Name, ignore.case=TRUE)) %>% mutate(Season=2017)
  tmp <- merge(xx1, xx2, by='Season', all=TRUE) %>%
    mutate(Id=paste('2017', Team_Id.x, Team_Id.y, sep='_')) %>%
    select(Id, Team_Name.x, Team_Name.y)

  for (idx in 1:nrow(tmp)) {
    v = tmp[idx,]
    s = sprintf("%s,%s,%s,", v$Id, v$Team_Name.x, v$Team_Name.y)
    print(s)
  }
  tmp
}



