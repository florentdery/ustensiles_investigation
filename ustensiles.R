##############################################################
# Project: Office Cutlery Disappearance Monitoring (2018–2019)
# Author: Florent D.
# Description:
#   This script loads and documents three datasets used to track 
#   the mysterious disappearance of utensils from an office drawer.
#   Observations were made periodically between October 2018 
#   and March 2019. 
#
#   Datasets:
#     1. cuttelry – counts of each utensil type at each visit
#     2. visits – schedule of drawer inspections ("visits")
#     3. spoons – individual spoon tracking by ID and characteristics
#
#   Each dataset was originally imported from Excel sheets, 
#   then converted to R data frames (tibbles) for reproducibility.
##############################################################


#--------------------------------------------------------------
# 1. CUTTELRY DATASET
#--------------------------------------------------------------
# Source: "ustensiles.xlsx", sheet = "cuttelry"
# Description:
#   Tracks the total number of utensils (knives, forks, spoons)
#   found in the drawer at each observation date.
#   Helps detect when specific utensils went missing.
#
# Columns:
#   - day / month / year : Date of observation
#   - butter_k  : Count of butter knives
#   - sharp_k   : Count of sharp knives
#   - soup_s    : Count of soup spoons
#   - dessert_s : Count of dessert spoons
#   - fork      : Count of forks
#   - notes     : Optional comments (e.g. special events)
#   - true_data : Logical indicator ("yes") confirming the data is valid
#
# Notes:
#   Missing utensils between observations may indicate disappearance.
#   The "emmigration dessert spoons" note marks an event of mass loss.

cuttelry <- structure(
  list(
    day = c(1, 11, 18, 23, 25, 3, 8, 21, 8, 14, 18, 24, 31, 14, 1),
    month = c(10, 10, 10, 10, 10, 11, 11, 11, 12, 1, 1, 1, 1, 2, 3), 
    year = c(2018, 2018, 2018, 2018, 2018, 2018, 2018, 2018, 2018,
             2019, 2019, 2019, 2019, 2019, 2019),
    butter_k = c(31, 29, 18, 18, 16, 16, 18, 16, 16, 13, 17, 17, 17, 17, 17),
    sharp_k  = c(4, 4, 4, 3, 3, 3, 3, 3, 3, 4, 3, 4, 3, 4, 4),
    soup_s   = c(8, 7, 6, 7, 7, 7, 7, 5, 5, 3, 3, 2, 2, 2, 3),
    dessert_s = c(2, 2, 2, 2, 3, 3, 3, 3, 1, 1, 3, 3, 3, 3, 2),
    fork = c(10, 11, 8, 8, 8, 8, 9, 7, 7, 5, 6, 6, 6, 5, 6),
    notes = c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 
              "emmigration dessert spoons", NA, NA, NA, NA),
    true_data = rep("yes", 15)
  ),
  class = c("tbl_df", "tbl", "data.frame"),
  row.names = c(NA, -15L)
)


#--------------------------------------------------------------
# 2. VISITS DATASET
#--------------------------------------------------------------
# Source: "ustensiles.xlsx", sheet = "visits"
# Description:
#   Provides the timeline of each visit when the drawer was inspected.
#   Each visit has a name (v1–v15) and a corresponding date.
#
# Columns:
#   - visit_name : Unique visit ID (v1, v2, …)
#   - day / month / year : Date of inspection
#   - true_data : Data validity flag
#
# Notes:
#   Each visit can be linked to the counts in the `cuttelry` dataset
#   or to spoon-specific presence/absence in the `spoons` dataset.

visits <- structure(
  list(
    visit_name = paste0("v", 1:15),
    day = c(1, 4, 11, 18, 23, 25, 3, 11, 21, 8, 14, 18, 31, 14, 1), 
    month = c(10, 10, 10, 10, 10, 10, 11, 11, 11, 12, 1, 1, 1, 2, 3), 
    year = c(2018, 2018, 2018, 2018, 2018, 2018, 2018, 2018, 2018,
             2018, 2019, 2019, 2019, 2019, 2019),
    true_data = rep("yes", 15)
  ),
  class = c("tbl_df", "tbl", "data.frame"),
  row.names = c(NA, -15L)
)


#########################
# 3. SPOONS DATASET
#########################

# Source: Excel sheet "spoons"
# This dataset tracks individual spoons across all visits.
# Each spoon (id A–J) has distinct attributes, allowing 
# recognition of specific spoons during inspections.
#
# Variables:
#  - id: spoon identifier
#  - attractiveness: subjective rating (0–3)
#  - weight: qualitative weight class ("light", "normal", "heavy")
#  - similitude: whether it resembles other spoons ("alike"/"different")
#  - type: "soup" or "dessert" spoon
#  - v1–v15: presence (1) or absence (0) of that spoon during each visit
#  - true_data: confirms this is actual observation data

spoons = structure(
  list(
    id = c("A", "B", "C", "D", "E", "F", "G", "H", "I", "J"),
    attractiveness = c(1, 3, 1, 3, 2, 0, 3, 2, 2, 3),
    weight = c("heavy", "heavy", "light", "normal", "normal",
               "normal", "light", "heavy", "normal", "normal"),
    similitude = c("different", "different", "alike", 
                   "different", "different", "different", 
                   "different", "alike", "alike", "different"),
    type = c("soup", "soup", "soup", "soup", 
             "soup", "dessert", "dessert", "soup",
             "soup", "soup"),
    v1 = c(1,1,1,1,1,1,1,1,1,1),
    v2 = c(1,1,1,1,1,1,1,1,1,1),
    v3 = c(1,1,1,1,1,1,1,1,1,1),
    v4 = c(1,1,1,1,0,1,1,1,1,1),
    v5 = c(1,1,1,1,1,1,1,1,1,1),
    v6 = c(1,1,1,1,1,1,1,1,1,1),
    v7 = c(1,1,1,1,1,1,1,1,1,1),
    v8 = c(1,1,1,1,1,1,1,1,1,1),
    v9 = c(1,0,1,1,1,1,1,1,1,1),
    v10 = c(1,1,1,0,0,0,0,1,0,1),
    v11 = c(1,1,1,0,0,0,0,0,0,0),
    v12 = c(1,1,1,0,0,0,0,0,0,0),
    v13 = c(0,1,1,0,0,1,0,0,0,0),
    v14 = c(1,1,0,0,0,1,0,0,0,0),
    v15 = c(1,0,1,0,0,1,0,0,0,0),
    true_data = rep("yes", 10)
  ),
  class = c("tbl_df", "tbl", "data.frame"),
  row.names = c(NA, -10L)
)
