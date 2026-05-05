# MLB Umpire Bias & ABS Analysis

**Author:** Keziah Vickraman  
**Supervisor:**   

---

## Research Question

> *"Do MLB umpires give more favourable ball-strike calls to star batters 
> relative to non-stars, and does the Automated Ball-Strike (ABS) Challenge 
> System eliminate this bias?"*

---

## Overview

This repository contains the R analytical skeleton for a causal analysis 
of umpire bias in Major League Baseball using Difference-in-Differences (DiD).

The ABS Challenge System was introduced at the MLB regular season level for 
the first time in 2026 — creating a clean natural experiment to test whether 
automation reduces star player favouritism in ball-strike calls.

---

## DiD Structure

|  | Pre-ABS (2020–2025) | Post-ABS (2026) |
|---|---|---|
| **Star Batters (Treated)** | A | B |
| **Non-Star Batters (Control)** | C | D |

**Estimate: (B-A) - (D-C)**  
**Hypothesis: DiD < 0 — ABS reduces star batter umpire advantage**

---

## Key Findings (Preliminary — Apr/May 2026 only)

- Star batters error rate: 9.53% (2025) → 9.04% (2026), change = **-0.49pp**
- Non-star batters error rate: 9.60% (2025) → 9.48% (2026), change = **-0.13pp**
- **DiD estimate = -0.362pp** — ABS disproportionately reduced errors for star batters
- Direction consistent with hypothesis — significance pending full 2026 season

> **Note:** Post-period currently April–May 2026 only. Full season data 
> available October 2026 — significance test should be revisited then.

---

## How to Run

```r
# Load raw data (download from Google Drive — see below)
all_statcast <- readRDS("statcast_2020_2026.rds")

# Run full pipeline
source("MLB_01_Umpire_Bias_Skeleton.R")
```
---

## Data

Raw Statcast pitch-level data (2020–2026) is too large for GitHub (116MB).

**Download:** [Google Drive link — rds file link here](https://drive.google.com/file/d/1Py7C9L1Qkmwok-kxLjjNMih443lulJNu/view?usp=sharing)  
**Load with:** `readRDS("statcast_2020_2026.rds")`

| Source | Coverage | Method |
|---|---|---|
| MLB Statcast (Baseball Savant) | 2020–2026 | Direct CSV pull via URL |
| Star classification | Per season | Top 25% by called pitches seen |

---

## Methods

**Correct call definition:**
- Pitch IN zone + called strike = correct
- Pitch OUT zone + ball = correct  
- Pitch IN zone + ball = missed strike (favours batter)
- Pitch OUT zone + called strike = phantom strike (hurts batter)

**Strike zone:** plate_x ± 0.7083 feet (17 inches), plate_z between 
personalised sz_bot and sz_top per batter

**Star classification:** Top 25% of batters by called pitches seen per 
season — proxy for regular starters given WAR API access blocked

---

## ABS Timeline

| Year | Event |
|---|---|
| 2019 | ABS first tested in minor leagues |
| 2022 | Triple-A ABS trials begin |
| 2023 | ABS expanded to all Triple-A |
| 2025 | ABS Challenge in Spring Training |
| **2026** | **First MLB regular season use ← treatment** |

---

## Next Steps for Full Paper

1. Re-run October 2026 with full post-season data
2. Add umpire fixed effects
3. Add pitch count controls (2-strike counts behave differently)
4. Separate missed strike vs phantom strike analysis
5. Test pitcher star bias — not just batter
6. Replace pitch volume proxy with WAR when API access restored
7. Placebo tests — randomise treatment year
8. Consider KBO 2024 full ABS as additional natural experiment

---

## Key References

- Kim & King (2014) — status bias in MLB umpire calls
- Hsu (2024) — home bias in umpire calls, Journal of Sports Economics
- Scientific Reports (2024) — psychophysics of home plate umpire calls
- Scientific Reports (2025) — KBO ABS natural experiment
- MLB Statcast ABS Dashboard — baseballsavant.mlb.com/abs
