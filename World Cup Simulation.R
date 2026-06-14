# 2026 FIFA World Cup Predictor: Using SOFASCORE Elo Ratings
# ------------------------------------------------------------

library(dplyr)
library(tidyr)
library(purrr)

set.seed(42)

# 1. TEAM ELO RATINGS ----
elo_df <- tibble::tribble(
  ~team, ~elo,
  "Argentina", 1877.27,
  "France", 1870.70,
  "Brazil", 1765.86,
  "England", 1828.02,
  "Spain", 1874.71,
  "Portugal", 1767.85,
  "Netherlands", 1753.57,
  "Germany", 1735.77,
  "Belgium", 1742.24,
  "Croatia", 1714.87,
  "Italy", 1704.73,
  "Morocco", 1755.10,
  "USA", 1671.23,
  "Mexico", 1687.48,
  "Canada", 1559.48,
  "Uruguay", 1673.07,
  "Colombia", 1698.35,
  "Japan", 1661.58,
  "Senegal", 1684.07,
  "Switzerland", 1650.06,
  "Ghana", 1346.88,
  "South Korea", 1591.63,
  "Australia", 1579.34,
  "Ecuador", 1598.52,
  "Wales", 1516.95,
  "Iran", 1412.02,
  "Saudi Arabia", 1423.88,
  "Tunisia", 1476.41,
  "Qatar", 1526.18,
  "Czechia", 1450.31,
  "South Africa", 1505.74,
  "Bosnia and Herzegovina", 1428.38,
  "Haiti", 1387.22,
  "Scotland", 1293.10,
  "Paraguay", 1503.34,
  "Turkiye", 1505.35,
  "Curacao", 1605.73,
  "Ivory Coast", 1294.77,
  "Sweden", 1540.87,
  "Egypt", 1509.79,
  "New Zealand", 1562.37,
  "Cabo Verde", 1275.58,
  "Iraq", 1371.11,
  "Jordan", 1446.28,
  "Austria", 1387.74,
  "Norway", 1597.40,
  "DR Congo", 1557.44,
  "Panama", 1474.43,
  "Uzbekistan", 1539.16,
  "Algeria", 1458.73
)

# 2. GROUP DEFINITIONS (12 groups of 4) ----
groups <- list(
  G1  = c("Mexico","South Korea","Czechia","South Africa"),
  G2  = c("Canada","Bosnia and Herzegovina","Qatar","Switzerland"),
  G3  = c("Brazil","Morocco","Haiti","Scotland"),
  G4  = c("USA","Paraguay","Australia","Turkiye"),
  G5  = c("Germany","Curacao","Ivory Coast","Ecuador"),
  G6  = c("Netherlands","Japan","Sweden","Tunisia"),
  G7  = c("Belgium","Egypt","Iran","New Zealand"),
  G8  = c("Spain","Cabo Verde","Saudi Arabia","Uruguay"),
  G9  = c("France","Senegal","Iraq","Norway"),
  G10 = c("Argentina","Algeria","Austria","Jordan"),
  G11 = c("Portugal","DR Congo","Uzbekistan","Colombia"),
  G12 = c("England","Croatia","Ghana","Panama")
)

# Add missing teams with placeholder rating
all_teams <- unique(unlist(groups))
missing <- setdiff(all_teams, elo_df$team)
if (length(missing) > 0) {
  elo_df <- bind_rows(elo_df, tibble(team = missing, elo = 1450))
}

# 3. ELO -> EXPECTED GOALS (Poisson lambda) ----
elo_to_lambda <- function(elo_a, elo_b, home_adv = 0) {
  diff <- (elo_a + home_adv) - elo_b
  base <- 1.35
  base * exp(diff / 400)
}

# 4. SIMULATE A SINGLE MATCH ----
simulate_match <- function(teamA, teamB, neutral = TRUE) {
  eA <- elo_df$elo[elo_df$team == teamA]
  eB <- elo_df$elo[elo_df$team == teamB]
  
  home_adv <- if (neutral) 0 else 50
  
  lamA <- elo_to_lambda(eA, eB, home_adv)
  lamB <- elo_to_lambda(eB, eA, -home_adv)
  
  goalsA <- rpois(1, lamA)
  goalsB <- rpois(1, lamB)
  
  list(teamA = teamA, teamB = teamB, goalsA = goalsA, goalsB = goalsB)
}

# 5. GROUP STAGE SIMULATION ----
# Returns full standings table with P, W, D, L, GF, GA, GD, Pts
simulate_group <- function(group_teams) {
  combos <- combn(group_teams, 2, simplify = FALSE)
  
  stats <- tibble(
    team = group_teams,
    P = 0L, W = 0L, D = 0L, L = 0L,
    GF = 0L, GA = 0L
  )
  
  for (m in combos) {
    res <- simulate_match(m[1], m[2])
    
    i1 <- which(stats$team == m[1])
    i2 <- which(stats$team == m[2])
    
    stats$P[i1] <- stats$P[i1] + 1L
    stats$P[i2] <- stats$P[i2] + 1L
    
    stats$GF[i1] <- stats$GF[i1] + res$goalsA
    stats$GA[i1] <- stats$GA[i1] + res$goalsB
    stats$GF[i2] <- stats$GF[i2] + res$goalsB
    stats$GA[i2] <- stats$GA[i2] + res$goalsA
    
    if (res$goalsA > res$goalsB) {
      stats$W[i1] <- stats$W[i1] + 1L
      stats$L[i2] <- stats$L[i2] + 1L
    } else if (res$goalsB > res$goalsA) {
      stats$W[i2] <- stats$W[i2] + 1L
      stats$L[i1] <- stats$L[i1] + 1L
    } else {
      stats$D[i1] <- stats$D[i1] + 1L
      stats$D[i2] <- stats$D[i2] + 1L
    }
  }
  
  stats <- stats %>%
    mutate(
      GD = GF - GA,
      Pts = W * 3 + D
    ) %>%
    arrange(desc(Pts), desc(GD), desc(GF)) %>%
    mutate(rank_in_group = row_number())
  
  stats
}

# 6. KNOCKOUT MATCH (with penalty-shootout style tiebreaker) ----
simulate_knockout <- function(teamA, teamB) {
  res <- simulate_match(teamA, teamB, neutral = TRUE)
  if (res$goalsA == res$goalsB) {
    eA <- elo_df$elo[elo_df$team == teamA]
    eB <- elo_df$elo[elo_df$team == teamB]
    p_A <- 1 / (1 + 10^((eB - eA) / 400))
    winner <- if (runif(1) < p_A) teamA else teamB
  } else {
    winner <- if (res$goalsA > res$goalsB) teamA else teamB
  }
  winner
}

round_winners <- function(matchups) {
  map_chr(matchups, ~ simulate_knockout(.x[1], .x[2]))
}

# 7. FULL TOURNAMENT SIMULATION (ONE RUN) ----
# 12 groups -> top 2 (24 teams) + 8 best 3rd-placed teams = 32 -> R32 -> R16 -> QF -> SF -> Final
simulate_tournament <- function() {
  
  group_results <- map(groups, simulate_group)
  
  # Top 2 from each group
  firsts  <- map_chr(group_results, ~ .x$team[.x$rank_in_group == 1])
  seconds <- map_chr(group_results, ~ .x$team[.x$rank_in_group == 2])
  
  # Best 8 third-placed teams (ranked by Pts, GD, GF)
  thirds <- map_dfr(names(group_results), function(gname) {
    g <- group_results[[gname]]
    g %>% filter(rank_in_group == 3) %>% mutate(group = gname)
  }) %>%
    arrange(desc(Pts), desc(GD), desc(GF)) %>%
    slice(1:8)
  
  best_thirds <- thirds$team
  
  # Pool of 32 qualifiers: 12 group winners, 12 runners-up, 8 best thirds
  # Build R32 bracket: pair group winners against runners-up/thirds in a
  # standard cross-bracket fashion (simplified deterministic pairing).
  qualifiers_32 <- c(firsts, seconds, best_thirds)
  
  # Create 16 R32 matchups by pairing sequential teams from a shuffled-but-
  # seeded pool, ensuring no team faces a team from its own group in R32
  # is hard to guarantee generally with 8 floating thirds, so we use a
  # straightforward pairing: winners vs (runners-up/thirds) rotated.
  pot_winners <- firsts                     # 12 teams
  pot_others  <- c(seconds, best_thirds)    # 20 teams
  pot_others  <- sample(pot_others)         # randomize which "other" each winner faces
  
  r32_matchups <- vector("list", 16)
  # First 12 matchups: each group winner vs a randomized "other" team
  for (i in 1:12) {
    r32_matchups[[i]] <- c(pot_winners[i], pot_others[i])
  }
  # Remaining 8 "other" teams play each other for the last 4 matchups... 
  # but 16 matchups total needed for 32 teams -> 12 + remaining 8 others
  # paired = 4 more matchups = 16 total
  remaining_others <- pot_others[13:20]
  for (i in 1:4) {
    r32_matchups[[12 + i]] <- c(remaining_others[2*i - 1], remaining_others[2*i])
  }
  
  r32_winners <- round_winners(r32_matchups)
  
  # Round of 16
  r16_matchups <- split(r32_winners, ceiling(seq_along(r32_winners) / 2))
  r16_winners <- round_winners(r16_matchups)
  
  # Quarterfinals
  qf_matchups <- split(r16_winners, ceiling(seq_along(r16_winners) / 2))
  qf_winners <- round_winners(qf_matchups)
  
  # Semifinals
  sf_matchups <- split(qf_winners, ceiling(seq_along(qf_winners) / 2))
  sf_winners <- round_winners(sf_matchups)
  
  # Final
  champion <- simulate_knockout(sf_winners[1], sf_winners[2])
  runner_up <- setdiff(sf_winners, champion)
  
  list(
    champion = champion,
    finalists = sf_winners,
    semifinalists = qf_winners,
    quarterfinalists = r16_winners,
    round_of_16 = r32_winners
  )
}

###### MONTE CARLO SIMULATION 
n_sims <- 10000

results <- map(1:n_sims, ~ simulate_tournament())

# Champion probabilities (winner of entire tournament)
champions <- map_chr(results, "champion")

champion_probs <- tibble(team = champions) %>%
  count(team, name = "n_titles") %>%
  mutate(win_probability = n_titles / n_sims) %>%
  arrange(desc(win_probability))

# Rank ALL teams by win probability (teams with 0 titles get probability 0)
all_teams_tbl <- elo_df %>% select(team)

final_ranking <- all_teams_tbl %>%
  left_join(champion_probs, by = "team") %>%
  mutate(
    n_titles = replace_na(n_titles, 0L),
    win_probability = replace_na(win_probability, 0)
  ) %>%
  arrange(desc(win_probability), team) %>%
  mutate(rank = row_number()) %>%
  select(rank, team, n_titles, win_probability)

print(final_ranking, n = 50)

# Optional: semifinal appearance probabilities
semis <- map(results, "semifinalists") %>% unlist()
semi_probs <- tibble(team = semis) %>%
  count(team, sort = TRUE) %>%
  mutate(probability = n / n_sims)

print(semi_probs, n = 32)


# 9. BAR CHART OF WIN PROBABILITIES ----
library(ggplot2)

ggplot(final_ranking, aes(x = reorder(team, win_probability), y = win_probability)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(
    title = "2026 World Cup Win Probabilities (Poisson + Elo)\n10000 Monte Carlo Simulations",
    x = NULL,
    y = "Win Probability"
  ) +
  scale_y_continuous(labels = scales::percent) +
  theme_minimal(base_size = 9) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))



# 9. BAR CHART OF WIN PROBABILITIES ----
library(ggplot2)

ggplot(final_ranking, aes(x = reorder(team, win_probability), y = win_probability)) +
  geom_col(fill = "steelblue") +
  geom_text(
    aes(label = scales::percent(win_probability, accuracy = 0.1)),
    hjust = -0.1, size = 2.8
  ) +
  coord_flip(clip = "off") +
  labs(
    title = "2026 World Cup Win Probabilities (Poisson + Elo)\n10000 Monte Carlo Simulations",
    x = NULL,
    y = "Win Probability"
  ) +
  scale_y_continuous(labels = scales::percent, expand = expansion(mult = c(0, 0.15))) +
  theme_minimal(base_size = 9) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))
