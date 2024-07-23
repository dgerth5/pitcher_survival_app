library(readr)
library(tidyverse)
library(baseballr)
library(rstan)
library(ggplot2)
library(reshape2)

fg_data <- read_csv("fangraphs-leaderboards (40).csv")

get_current_players <- function(year){
  df <- map_df(c(1,11,12,13,14), ~baseballr::mlb_stats(
    stat_type = "season",
    sport_id = .x,
    season = year,
    stat_group = "pitching",
    player_pool = "All",
    limit = 2000))
  
  return(unique(df$player_id))
}

get_career_stats <- function(year){
  
  yrs <- seq(year, 2024)
  
  df <- map_df(yrs, ~baseballr::mlb_stats(
    stat_type = "season",
    sport_id = 1,
    season = .x,
    stat_group = "pitching",
    player_pool = "All",
    limit = 2000))
  
  df$innings_pitched <- as.numeric(df$innings_pitched)
  
  career_summary <- df %>%
    group_by(player_id,player_full_name) %>%
    summarise(career_ip = sum(innings_pitched),
              started = sum(games_started) / sum(games_played))
  
}

curr_players <- get_current_players(2024)
career_stats <- get_career_stats(2011)

mod_df <- fg_data %>%
  mutate(curr_player = if_else(MLBAMID %in% curr_players, 1, 0),
         censor = if_else(curr_player == 1, 0, 1),
         fb_usage = coalesce(`FA% (pi)`,0) + coalesce(`FC% (pi)`,0) + coalesce(`SI% (pi)`,0),
         weighted_fb = (coalesce(`FA% (pi)`,0)/fb_usage)*coalesce(`vFA (sc)`,0) + (coalesce(`FC% (pi)`,0)/fb_usage)*coalesce(`vFC (sc)`,0) + (coalesce(`SI% (pi)`,0)/fb_usage)*coalesce(`vSI (sc)`,0)) %>%
  dplyr::select(Season, Name, PlayerId, MLBAMID, Age, IP, ERA, `K%+`, `BB%+`, weighted_fb, curr_player, censor) %>%
  group_by(MLBAMID) %>%
  slice_max(order_by = IP, n = 1) %>%
  rename(k = `K%+`, b = `BB%+`) %>%
  ungroup() %>%
  left_join(career_stats, by = c("MLBAMID"="player_id")) %>%
  mutate(surv_innings = career_ip - IP,
         starter = if_else(started < .3, 0 , 1)) %>%
  filter(surv_innings > 0) # takes out < 10 people

mm <- model.matrix( ~ Age + IP + k + b + starter,
                    data = mod_df)

stan_dat <- list(N = nrow(mm),
                 P = ncol(mm),
                 y = mod_df$surv_innings,
                 censor = mod_df$censor,
                 X = mm)

mod <- stan_model("weibull_model.stan")
fit <- sampling(mod,
                stan_dat,
                iter = 2000,
                chains = 4,
                cores = 6)

post_alpha <- extract(fit)$alpha
post_beta <- extract(fit)$beta

saveRDS(list(post_alpha = post_alpha, post_beta = post_beta), "model_data.RDS")
