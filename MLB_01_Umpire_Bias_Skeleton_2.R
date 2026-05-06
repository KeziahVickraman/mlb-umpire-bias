###--------------------------------------------------------------------------###
###   MLB Umpire Bias & ABS Analysis                                         ###
###   Script: MLB_01_Umpire_Bias_Skeleton.R                                  ###
###                                                                          ###
###   Research Question:                                                     ###
###   Do MLB umpires give more favourable ball-strike calls to star          ###
###   batters relative to non-stars, and does the Automated Ball-Strike      ###
###   (ABS) Challenge System eliminate this bias?                            ###
###                                                                          ###
###   Method: Difference-in-Differences (DiD)                                ###
###   Treatment: ABS Challenge System — introduced MLB-wide 2026 season      ###
###   Pre-period:  2020-2025 (human umpire only)                             ###
###   Post-period: 2026 (ABS Challenge System live)                          ###
###   Outcome: Umpire error rate by player star status                       ###
###                                                                          ###
###   Data: MLB Statcast via Baseball Savant (batter perspective)            ###
###   Star Classification: Top 25% by called pitches seen per season         ###
###                                                                          ###
###   DiD Structure:                                                         ###
###              Pre-ABS (2020-2025)   Post-ABS (2026)                       ###
###   Star              A                    B                               ###
###   Non-Star          C                    D                               ###
###   Estimate: (B-A) - (D-C)                                                ###
###   Hypothesis: DiD < 0 — ABS reduces star batter umpire advantage         ###
###                                                                          ###
###   Note on ABS Timeline:                                                  ###
###   2019 — ABS first tested in minor leagues                               ###
###   2022 — Triple-A ABS trials begin                                       ###
###   2023 — ABS expanded to all Triple-A                                    ###
###   2025 — ABS Challenge in Spring Training                                ###
###   2026 — First MLB regular season use (treatment)                        ###
###                                                                          ###
###   Note on 2020: COVID shortened season — flag as anomaly but keep        ###
###   Pre-2020 excluded: Statcast quality improved significantly post-2019   ###
###--------------------------------------------------------------------------###

rm(list = ls())
# ---- 0. Set Dependencies ----
pacman::p_load(
  tidyverse, glue, scales, lubridate,
  baseballr,
  lmtest, sandwich,
  broom
)
# ---- 1. Collect & Import Data ----
###--------------------------------------------------------------------------###
###   SECTION 1: Load Statcast Data                                          ###
###   Pull batter-perspective data month by month                           ###
###   Baseball Savant limits single pulls — monthly batching required       ###
###   Pre-period: 2020-2025 (full seasons)                                  ###
###   Post-period: 2026 (capped at today — partial season)                  ###
###--------------------------------------------------------------------------###

pull_statcast_season <- function(year) {
  message(glue("Pulling {year} Statcast data..."))
  
  current_date <- Sys.Date()
  current_year <- as.integer(format(current_date, "%Y"))
  
  months <- list(
    c(glue("{year}-04-01"), glue("{year}-04-30")),
    c(glue("{year}-05-01"), glue("{year}-05-31")),
    c(glue("{year}-06-01"), glue("{year}-06-30")),
    c(glue("{year}-07-01"), glue("{year}-07-31")),
    c(glue("{year}-08-01"), glue("{year}-08-31")),
    c(glue("{year}-09-01"), glue("{year}-09-30"))
  )
  
  map_dfr(months, function(m) {
    start_date <- as.Date(m[1])
    end_date   <- as.Date(m[2])
    
    # Past seasons: pull full month
    # Current season: skip future months, cap end date at today
    if (year < current_year) {
      # full month — no cap needed
    } else {
      if (start_date > current_date) return(NULL)       # skip future months
      end_date <- min(end_date, current_date)            # cap at today
    }
    
    message(glue("  {start_date} to {end_date}"))
    url <- paste0(
      "https://baseballsavant.mlb.com/statcast_search/csv?",
      "all=true&player_type=batter&",
      "game_date_gt=", start_date,
      "&game_date_lt=", end_date, "&",
      "hfGT=R%7C&type=details"
    )
    tryCatch(
      read_csv(url, show_col_types = FALSE) %>%
        mutate(game_date = as.Date(game_date)),
      error = function(e) {
        message(glue("  Error: {e$message}"))
        NULL
      }
    )
  })
}


pull_statcast_season <- function(year) {
  message(glue("Pulling {year} Statcast data..."))
  
  current_date <- Sys.Date()
  current_year <- as.integer(format(current_date, "%Y"))
  
  months <- list(
    c(glue("{year}-04-01"), glue("{year}-04-30")),
    c(glue("{year}-05-01"), glue("{year}-05-31")),
    c(glue("{year}-06-01"), glue("{year}-06-30")),
    c(glue("{year}-07-01"), glue("{year}-07-31")),
    c(glue("{year}-08-01"), glue("{year}-08-31")),
    c(glue("{year}-09-01"), glue("{year}-09-30"))
  )
  
  map_dfr(months, function(m) {
    start_date <- as.Date(m[1])
    end_date   <- as.Date(m[2])
    
    if (year < current_year) {
      # full month
    } else {
      if (start_date > current_date) return(NULL)
      end_date <- min(end_date, current_date)
    }
    
    message(glue("  {start_date} to {end_date}"))
    url <- paste0(
      "https://baseballsavant.mlb.com/statcast_search/csv?",
      "all=true&player_type=batter&",
      "game_date_gt=", start_date,
      "&game_date_lt=", end_date, "&",
      "hfGT=R%7C&type=details"
    )
    tryCatch(
      read_csv(url, 
               show_col_types = FALSE,
               col_types = cols(.default = col_character())) %>%  # read all as character
        mutate(
          game_date     = as.Date(game_date),
          plate_x       = as.numeric(plate_x),
          plate_z       = as.numeric(plate_z),
          sz_top        = as.numeric(sz_top),
          sz_bot        = as.numeric(sz_bot),
          release_speed = as.numeric(release_speed)
        ),
      error = function(e) {
        message(glue("  Error: {e$message}"))
        NULL
      }
    )
  })
}

# Pull pre-period: 2020-2025 (full seasons)
# Pull post-period: 2026 (partial — capped at today)
# Note: this will take ~15 minutes for 7 seasons

seasons_pre <- 2020:2025

all_pre <-
  map_dfr(seasons_pre, function(yr) {
    message(glue("\n===== Season {yr} ====="))
    pull_statcast_season(yr) %>%
      mutate(season = as.integer(yr),
             abs_era = 0L)   # pre-ABS
  })

all_post <-
  pull_statcast_season(2026) %>%
  mutate(season  = 2026L,
         abs_era = 1L)       # post-ABS

all_statcast <-
  bind_rows(all_pre, all_post)

message(glue("\nTotal pitches pulled: {nrow(all_statcast)}"))
message("Pitches by season:")
all_statcast %>% count(season, abs_era) %>% print()

# Save raw — large file
saveRDS(all_statcast, "statcast_2020_2026.rds")

# ---- 2. Wrangle Data ----

###--------------------------------------------------------------------------###
###   SECTION 2: Define Objectively Correct Ball-Strike Call                 ###
###   Using pitch location vs personalised strike zone                       ###
###                                                                          ###
###   Correct calls:                                                         ###
###     pitch IN zone  + called_strike = correct                            ###
###     pitch OUT zone + ball          = correct                            ###
###   Incorrect calls:                                                       ###
###     pitch IN zone  + ball          = missed strike (favours batter)     ###
###     pitch OUT zone + called_strike = phantom strike (hurts batter)      ###
###                                                                          ###
###   Strike zone definition:                                                ###
###     Horizontal: plate_x ± 0.7083 feet (17 inches / 2)                  ###
###     Vertical:   plate_z between sz_bot and sz_top (per batter)          ###
###--------------------------------------------------------------------------###

plate_half_width <- 17 / 24  # 17 inches / 2, converted to feet

called_pitches <-
  all_statcast %>%
  filter(description %in% c("called_strike", "ball")) %>%
  mutate(
    in_zone        = plate_x >= -plate_half_width &
                     plate_x <=  plate_half_width &
                     plate_z >= sz_bot &
                     plate_z <= sz_top,
    called_strike  = description == "called_strike",
    correct_call   = (in_zone & called_strike) | (!in_zone & !called_strike),
    incorrect_call = !correct_call,
    missed_strike  = in_zone  & !called_strike,  # umpire lets strike go — favours batter
    phantom_strike = !in_zone &  called_strike   # umpire calls strike on ball — hurts batter
  )

message(glue("Called pitches: {nrow(called_pitches)}"))
message(glue("Overall error rate: {round(mean(called_pitches$incorrect_call, na.rm=TRUE)*100, 2)}%"))
message(glue("Missed strikes:     {round(mean(called_pitches$missed_strike,  na.rm=TRUE)*100, 2)}%"))
message(glue("Phantom strikes:    {round(mean(called_pitches$phantom_strike, na.rm=TRUE)*100, 2)}%"))

## ---- 2.1 Star Player Classification ----
###--------------------------------------------------------------------------###
###   SECTION 2.1: Star Classification                                       ###
###   External WAR sources (Baseball Reference, FanGraphs) blocked           ###
###   Proxy: top 25% of batters by called pitches seen per season            ###
###   Defensible: regular starters face ~600-800 called pitches/full season  ###
###   Relative threshold per season accounts for 2020 shortened season       ###
###   TODO: replace with WAR when API access restored (check with Prof.)     ###
###--------------------------------------------------------------------------###

star_proxy <-
  all_statcast %>%
  filter(description %in% c("called_strike", "ball")) %>%
  count(player_name, season) %>%
  group_by(season) %>%
  mutate(
    p75  = quantile(n, 0.75),
    star = case_when(
      n >= p75       ~ "Star",
      n >= median(n) ~ "Average",
      TRUE           ~ "Non-Star"
    ),
    star = factor(star, levels = c("Non-Star", "Average", "Star"))
  ) %>%
  ungroup() %>%
  rename(n_called_pitches = n)

message("Star classification by season:")
print(star_proxy %>% count(star, season))
print(star_proxy %>% count(star, season), n = Inf)

## ---- 2.2 Umpire Errors by Player Status ----
###--------------------------------------------------------------------------###
###   SECTION 2.2: Merge + Error Rates by Star Status                        ###
###--------------------------------------------------------------------------###

called_pitches_star <-
  called_pitches %>%
  left_join(
    star_proxy %>% select(player_name, season, star),
    by = c("player_name", "season")
  ) %>%
  mutate(star = replace_na(as.character(star), "Unknown"))

# Season-level error rates by star status
error_by_season <-
  called_pitches_star %>%
  filter(star %in% c("Star", "Non-Star")) %>%
  group_by(star, season, abs_era) %>%
  summarise(
    n_pitches          = n(),
    error_rate         = mean(incorrect_call, na.rm = TRUE) * 100,
    missed_strike_rate = mean(missed_strike,  na.rm = TRUE) * 100,
    phantom_rate       = mean(phantom_strike, na.rm = TRUE) * 100,
    .groups = "drop"
  )

# Aggregated pre vs post summary for DiD
error_summary <-
  called_pitches_star %>%
  filter(star %in% c("Star", "Non-Star")) %>%
  group_by(star, abs_era) %>%
  summarise(
    n_pitches          = n(),
    error_rate         = mean(incorrect_call, na.rm = TRUE) * 100,
    missed_strike_rate = mean(missed_strike,  na.rm = TRUE) * 100,
    phantom_rate       = mean(phantom_strike, na.rm = TRUE) * 100,
    .groups = "drop"
  )

message("Aggregated error rates (pre vs post):")
print(error_summary)

#---- 3. DiD Modelling ----
## ---- 3.1 Manual DiD  ----

###--------------------------------------------------------------------------###
###   SECTION 3.1: Manual DiD Estimate — (B-A) - (D-C)                       ###
###--------------------------------------------------------------------------###

A <- error_summary %>% filter(star == "Star",     abs_era == 0) %>% pull(error_rate)
B <- error_summary %>% filter(star == "Star",     abs_era == 1) %>% pull(error_rate)
C <- error_summary %>% filter(star == "Non-Star", abs_era == 0) %>% pull(error_rate)
D <- error_summary %>% filter(star == "Non-Star", abs_era == 1) %>% pull(error_rate)

did_estimate   <- (B - A) - (D - C)
interpretation <- if_else(did_estimate < 0,
                          "Negative = ABS reduced star bias",
                          "Positive = star bias increased or no effect")

cat(glue("\n========== MANUAL DiD ==========\n"))
cat(glue("A (Star pre):     {round(A, 3)}%\n"))
cat(glue("B (Star post):    {round(B, 3)}%\n"))
cat(glue("C (Non-Star pre): {round(C, 3)}%\n"))
cat(glue("D (Non-Star post):{round(D, 3)}%\n"))
cat(glue("(B-A) = {round(B-A, 3)} — Star error rate change\n"))
cat(glue("(D-C) = {round(D-C, 3)} — Non-Star error rate change\n"))
cat(glue("DiD   = {round(did_estimate, 3)}\n"))
cat(glue("Interpretation: {interpretation}\n"))
cat("================================\n")

## ---- 3.2 Formal DiD Modelling ----
###--------------------------------------------------------------------------###
###   SECTION 3.2: Formal DiD Model                                          ###
###   incorrect_call ~ post * treated                                        ###
###   post:treated = DiD estimate = causal effect of ABS on star bias        ###
###--------------------------------------------------------------------------###

did_data <-
  called_pitches_star %>%
  filter(star %in% c("Star", "Non-Star")) %>%
  mutate(
    post           = as.integer(abs_era),
    treated        = as.integer(star == "Star"),
    incorrect_call = as.integer(incorrect_call)
  )

model_did        <- lm(incorrect_call ~ post * treated, data = did_data)
model_did_robust <- coeftest(model_did,
                             vcov = vcovHC(model_did, type = "HC3"))

cat("\n========== DiD MODEL ==========\n")
print(model_did_robust)
cat("\nHow to read:\n")
cat("post         = overall error rate change post-ABS (time trend)\n")
cat("treated      = baseline star vs non-star difference pre-ABS\n")
cat("post:treated = DiD estimate = (B-A)-(D-C) = causal effect of ABS\n")
cat("================================\n")

# ATE Table 
ate_mlb <-
  bind_rows(
    tibble(
      term           = "post (Time Trend)",
      estimate       = round(coef(model_did)["post"], 4),
      CI_lower       = round(confint(model_did)["post", 1], 4),
      CI_upper       = round(confint(model_did)["post", 2], 4),
      CI_range       = glue("[{round(confint(model_did)['post',1],4)}, {round(confint(model_did)['post',2],4)}]"),
      interpretation = "Overall error rate change post-ABS (both groups)"
    ),
    tibble(
      term           = "treated (Star Baseline)",
      estimate       = round(coef(model_did)["treated"], 4),
      CI_lower       = round(confint(model_did)["treated", 1], 4),
      CI_upper       = round(confint(model_did)["treated", 2], 4),
      CI_range       = glue("[{round(confint(model_did)['treated',1],4)}, {round(confint(model_did)['treated',2],4)}]"),
      interpretation = "Baseline star vs non-star difference pre-ABS"
    ),
    tibble(
      term           = "post:treated (DiD Estimate)",
      estimate       = round(coef(model_did)["post:treated"], 4),
      CI_lower       = round(confint(model_did)["post:treated", 1], 4),
      CI_upper       = round(confint(model_did)["post:treated", 2], 4),
      CI_range       = glue("[{round(confint(model_did)['post:treated',1],4)}, {round(confint(model_did)['post:treated',2],4)}]"),
      interpretation = "Causal effect of ABS on star bias — (B-A)-(D-C)"
    )
  )

cat("\n========== ATE TABLE ==========\n")
print(ate_mlb)
cat("\nNote: Estimates in percentage points (pp)\n")
cat("Negative DiD = ABS reduced star batter umpire advantage\n")
cat("CI based on HC3 robust standard errors\n")
cat("Post-period = April-May 2026 only — full season needed for significance\n")
cat("================================\n")

# ---- 4. Visualise  ----

###--------------------------------------------------------------------------###
###   SECTION 4: Visualisations                                              ###
###--------------------------------------------------------------------------###

theme_mlb <- function() {
  theme_minimal(base_family = "sans") +
    theme(
      plot.background  = element_rect(fill = "#FFFFFF", color = NA),
      panel.grid.major = element_line(color = "#E0E0E0"),
      panel.grid.minor = element_blank(),
      plot.title       = element_text(face = "bold", size = 14),
      plot.subtitle    = element_text(size = 11, color = "gray30"),
      plot.caption     = element_text(size = 8,  color = "gray50", face = "italic"),
      axis.title       = element_text(face = "bold", size = 10),
      axis.text        = element_text(size = 9),
      legend.position  = "top"
    )
}

### --- Fig 1: Parallel Trends Check (season-level) ----
# Shows pre-period trends for both stars and non-stars
# Should move together pre-2026 — diverge post-ABS
fig_parallel <-
  error_by_season %>%
  mutate(
    era = if_else(abs_era == 0, "Pre-ABS (2020-2025)", "Post-ABS (2026)"),
    era = factor(era, levels = c("Pre-ABS (2020-2025)", "Post-ABS (2026)"))
  ) %>%
  ggplot(aes(x = season, y = error_rate,
             color = star, linetype = star)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 3) +
  geom_vline(xintercept = 2025.5, linetype = "dashed",
             color = "black", linewidth = 0.8) +
  annotate("text", x = 2025.6, y = max(error_by_season$error_rate) * 0.98,
           label = "ABS Challenge\nSystem ->",
           color = "black", size = 3, hjust = 0) +
  scale_color_manual(values = c("Star"     = "#CE1141",
                                "Non-Star" = "#17408B")) +
  scale_linetype_manual(values = c("Star"     = "solid",
                                   "Non-Star" = "dashed")) +
  scale_x_continuous(breaks = 2020:2026) +
  labs(
    title    = "Parallel Trends — Umpire Error Rate by Star Status (2020-2026)",
    subtitle = "Pre-ABS trends should move together; divergence post-2026 = causal effect",
    x = "Season", y = "Error Rate (%)",
    color = "Batter Type", linetype = "Batter Type",
    caption = "Data: MLB Statcast via Baseball Savant | 2026 = partial season (Apr-May)"
  ) +
  theme_mlb() +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5))

fig_parallel

### --- Fig 2: DiD Summary (aggregated pre vs post) ----
fig_did <-
  error_summary %>%
  mutate(
    era = if_else(abs_era == 0, "Pre-ABS (2020-2025)", "Post-ABS (2026)"),
    era = factor(era, levels = c("Pre-ABS (2020-2025)", "Post-ABS (2026)"))
  ) %>%
  ggplot(aes(x = era, y = error_rate,
             color = star, group = star)) +
  geom_line(linewidth = 1.3) +
  geom_point(size = 4) +
  geom_text(aes(label = paste0(round(error_rate, 2), "%")),
            vjust = -1, size = 3.5, show.legend = FALSE) +
  scale_color_manual(values = c("Star"     = "#CE1141",
                                "Non-Star" = "#17408B")) +
  labs(
    title    = "DiD: Umpire Error Rate — Star vs Non-Star Batters Pre/Post ABS",
    subtitle = glue("DiD = {round(did_estimate, 3)}pp — ABS disproportionately reduced errors for star batters"),
    x = NULL, y = "Error Rate (%)",
    color    = "Batter Type",
    caption  = "Data: MLB Statcast | ABS Challenge System introduced MLB 2026"
  ) +
  theme_mlb()

print(fig_parallel)
print(fig_did)

ggsave("fig_parallel_trends.png", fig_parallel, width = 10, height = 6, dpi = 300)
ggsave("fig_did_summary.png",     fig_did,      width = 10, height = 6, dpi = 300)

# ---- 5. Report  ----
###--------------------------------------------------------------------------###
###   SECTION 8: Summary & Next Steps                                        ###
###--------------------------------------------------------------------------###

cat("\n========== SUMMARY ==========\n")
cat(glue("Pre-period:   2020-2025 (6 seasons, human umpire only)\n"))
cat(glue("Post-period:  2026 (Apr-May only — partial season)\n"))
cat(glue("DiD estimate: {round(did_estimate, 3)}pp\n"))
cat(glue("Direction:    {interpretation}\n"))
cat("Significance: Revisit at end of 2026 season\n")

cat("\nNext Steps for Full Paper:\n")
cat("1. Re-run October 2026 with full post-season data\n")
cat("2. Add umpire fixed effects\n")
cat("3. Add pitch count controls (2-strike counts behave differently)\n")
cat("4. Separate missed strike vs phantom strike analysis\n")
cat("5. Test pitcher star bias — not just batter\n")
cat("6. Replace pitch volume proxy with WAR when API access restored\n")
cat("7. Placebo tests — randomise treatment year\n")
cat("8. Consider KBO 2024 full ABS as additional natural experiment\n")
cat("==============================\n")

###--------------------------------------------------------------------------###
# ---- 6. Save RAM space into hardisk ----
save.image("MLB_01_Umpire_Bias_Results.RData")
message("Saved.")

# ---- 7. Report Dependencies ----
sessionInfo()
