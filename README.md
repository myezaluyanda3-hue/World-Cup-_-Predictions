# ⚽ 2026 FIFA World Cup Simulator

Simulates the full 48-team 2026 World Cup using **Elo ratings**, **calibrated Poisson GLMs**, and **Monte Carlo simulation** (10,000+ runs) to produce win probabilities at every stage.

Built in R as part of an Honours Statistics portfolio project.

---

## How it works

1. **Elo ratings** quantify team strength going into the tournament
2. **Poisson GLM** — fit on historical international results — translates Elo differences into expected scorelines
3. **Monte Carlo** runs the full bracket 10,000 times and aggregates probabilities across all rounds

The 48-team format is fully implemented: 12 groups, best-8 third-place selection, and a complete Round of 32 bracket.

---

## Quickstart

```r
install.packages(c("dplyr", "tidyr", "purrr"))

source("R/01_calibrate_model.R")
source("R/02_team_elos.R")
source("R/07_monte_carlo.R")
```

To pull historical match data:

```r
results <- read.csv(
  "https://raw.githubusercontent.com/martj42/international_results/master/results.csv"
)
```

---

## Notes

- Group draw is a placeholder — update `data/groups_2026.csv` after the official draw
- Penalty shootouts are modelled as an Elo-weighted Bernoulli draw
- Increase `n_sims` to 50,000 for more stable deep-round estimates

---

MIT License
