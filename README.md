# Office Cutlery Disappearance Monitoring (2018–2019)

## Overview

This repository documents a longitudinal investigation into the disappearance of cutlery from a shared office drawer between October 2018 and March 2019.

Repeated drawer inspections were conducted over a six-month period to monitor the abundance and persistence of forks, knives, and spoons. Particular attention was given to individual spoon identities, allowing a survival-analysis framework to be applied to utensil disappearance events.

Although originally conducted for amusement, the project serves as a complete reproducible example of:

- Data documentation and archiving
- Longitudinal observational study design
- Kaplan-Meier survival analysis
- Cox proportional hazards modelling
- Time-varying covariate effects
- Scientific visualization in R

---

## Repository Structure

| File | Description |
|--------|------------|
| `ustensiles.xlsx` | Original Excel workbook containing all raw observations |
| `ustensiles.R` | Fully documented R datasets (`cuttelry`, `visits`, and `spoons`) |
| `simulate_dataset_from_real_data.R` | Produces survival-analysis datasets from observed spoon histories |
| `analysis_final.R` | Main analysis and figure-generation script |
| `ustensiles_investigation.Rproj` | RStudio project file |
| `ustensile_composition_in_drawer_over_time.jpeg` | Temporal evolution of utensil abundance |
| `km_curves.jpeg` | Kaplan-Meier survival curves |
| `hr_ratios.jpeg` | Hazard-ratio estimates from Cox models |
| `README.md` | Project description |

---

## Datasets

### `cuttelry`

Drawer-level counts recorded during each inspection.

Variables include:

- observation date
- butter knives
- sharp knives
- soup spoons
- dessert spoons
- forks
- field notes

This dataset provides a drawer-level view of cutlery abundance through time.

---

### `visits`

Chronology of all inspection events.

Each visit corresponds to a drawer observation and serves as a bridge between aggregate counts and individual spoon records.

---

### `spoons`

Individual spoon tracking dataset.

Ten spoons (`A`–`J`) were uniquely identified and monitored throughout the study.

Recorded characteristics include:

- spoon type (`soup` or `dessert`)
- weight class (`light`, `normal`, or `heavy`)
- distinctiveness (`alike` or `different`)
- attractiveness score
- presence or absence at each visit

This dataset allows disappearance events to be analyzed at the individual-spoon level.

---

## Analysis Workflow

### 1. Utensil Abundance Through Time

The total number of forks, knives, and spoons remaining in the drawer was plotted across the monitoring period.

Output:

- `ustensile_composition_in_drawer_over_time.jpeg`

---

### 2. Kaplan-Meier Survival Analysis

Individual spoon disappearance histories were converted into time-to-event data.

Kaplan-Meier survival curves were estimated for:

- all spoons combined
- spoon type
- spoon distinctiveness
- spoon weight class

Output:

- `km_curves.jpeg`

---

### 3. Cox Proportional Hazards Modelling

Disappearance risk was modelled as a function of spoon characteristics.

Explanatory variables included:

- weight class
- spoon type
- spoon distinctiveness

Proportional hazard assumptions were assessed using Schoenfeld residuals.

---

### 4. Time-Varying Effects

Evidence of non-proportional hazards for spoon weight prompted the use of an extended Cox model with time-varying coefficients.

Hazard ratios were estimated through time and visualized with confidence intervals.

Output:

- `hr_ratios.jpeg`

---

## Main Findings

Interpretation should remain cautious due to the small sample size (`n = 10` spoons).

Observed patterns include:

- Progressive loss of utensils throughout the monitoring period.
- Substantial heterogeneity in spoon survival times.
- Potential effects of spoon weight on disappearance risk.
- Possible influence of spoon type and distinctiveness on survival outcomes.

While inferential power is limited, the project provides a compact demonstration of survival-analysis methods using an unconventional dataset.

---

## Reproducing the Analysis

### Required Packages

```r
library(tidyverse)
library(lubridate)
library(survival)
library(ggsurvfit)
library(patchwork)
library(cowplot)
```

### Run the Entire Workflow

```r
source("ustensiles.R")
source("simulate_dataset_from_real_data.R")
source("analysis_final.R")
```

Generated figures will be written to the project directory.

---

## Project Origin

This project began as an informal office investigation into the recurring disappearance of communal cutlery.

What started as a light-hearted curiosity eventually became a useful case study for teaching and demonstrating:

- survival analysis
- reproducible research
- data documentation
- exploratory statistical modelling

The author makes no claims regarding the identity, intentions, or organizational affiliation of the individual(s) responsible for the observed disappearances.

---

## Author

**Florent Déry**

Office Cutlery Disappearance Monitoring Project (2018–2019)
