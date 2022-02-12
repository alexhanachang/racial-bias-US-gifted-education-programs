########## SET UP ########## ########## ########## ########## 
## Load packages ----------------------------------------------------------
library(ggplot2)
library(tidyverse)


## Data import -------------------------------------------------------------
enrollment_raw <- read.csv(file = "data/unprocessed/Enrollment.csv")
gifted_talented_raw <- read.csv(file = "data/unprocessed/Gifted and Talented.csv")


## Identify key -----------------------------------------------------------
intersect(names(enrollment_raw),
          names(gifted_talented_raw))

enrollment_raw %>% 
  count(COMBOKEY) %>% 
  filter(n > 1) # Confirm key

gifted_talented_raw %>% 
  count(COMBOKEY) %>% 
  filter(n > 1) # Confirm key


## filter() -------------------------------------------------------------------------
# Filter out schools that do not have GAT programs
gifted_talented_raw <- gifted_talented_raw %>% 
  filter(SCH_GT_IND == "Yes")


## Rename ------------------------------------------------------------------
# Rename variables in gifted_talented_raw
gifted_talented <- gifted_talented_raw %>% 
  as_tibble() %>% 
  mutate(gifted_talented_raw, 
         state = LEA_STATE, 
         gifted_hisp_m = SCH_GTENR_HI_M, gifted_hisp_f = SCH_GTENR_HI_F, 
         gifted_asian_m = SCH_GTENR_AS_M, gifted_asian_f = SCH_GTENR_AS_F,
         gifted_black_m = SCH_GTENR_BL_M, gifted_black_f = SCH_GTENR_BL_F, 
         gifted_white_m = SCH_GTENR_WH_M, gifted_white_f = SCH_GTENR_WH_F,
         gifted_other_m = SCH_GTENR_AM_M + SCH_GTENR_HP_M + SCH_GTENR_TR_M, 
         gifted_other_f = SCH_GTENR_AM_F + SCH_GTENR_HP_F + SCH_GTENR_TR_F, 
         gifted_bipoc_m = gifted_hisp_m + gifted_asian_m + gifted_black_m + gifted_other_m, 
         gifted_bipoc_f = gifted_hisp_f + gifted_asian_f + gifted_black_f + gifted_other_f, 
         gifted_male = gifted_hisp_m + gifted_asian_m + gifted_black_m + gifted_white_m + gifted_other_m, 
         gifted_female = gifted_hisp_f + gifted_asian_f + gifted_black_f + gifted_white_f + gifted_other_f, 
         gifted_total = gifted_male + gifted_female) %>% # rename and collapse other
  select(COMBOKEY, state, gifted_asian_m, gifted_asian_f,
         gifted_black_m, gifted_black_f, gifted_hisp_m, gifted_hisp_f, 
         gifted_white_m, gifted_white_f, gifted_other_m, gifted_other_f, 
         gifted_bipoc_m, gifted_bipoc_f, 
         gifted_male, gifted_female, gifted_total)

# Rename variables in enrollment
enrollment <- enrollment_raw %>% 
  as_tibble() %>% 
  mutate(enrollment_raw, 
         state = LEA_STATE, 
         hisp_m = SCH_ENR_HI_M, hisp_f = SCH_ENR_HI_F, 
         asian_m = SCH_ENR_AS_M, asian_f = SCH_ENR_AS_F, 
         black_m = SCH_ENR_BL_M, black_f = SCH_ENR_BL_F, 
         white_m = SCH_ENR_WH_M, white_f = SCH_ENR_WH_F, 
         other_m = SCH_ENR_AM_M + SCH_ENR_HP_M + SCH_ENR_TR_M, 
         other_f = SCH_ENR_AM_F + SCH_ENR_HP_F + SCH_ENR_TR_F, 
         bipoc_m = hisp_m + asian_m + black_m + other_m, 
         bipoc_f = hisp_f + asian_f + black_f + other_f, 
         male = hisp_m + asian_m + black_m + white_m + other_m, 
         female = hisp_f + asian_f + black_f + white_f + other_f, 
         total = male + female) %>% # rename and collapse other
  select(COMBOKEY, state, asian_m, asian_f, 
         black_m, black_f, hisp_m, hisp_f, 
         white_m, white_f, other_m, other_f, 
         bipoc_m, bipoc_f, male, female, total)


## filter() ----------------------------------------------------------------
# Filter out all-boys and all-girls schools
enrollment <- enrollment %>% 
  filter(male > 0, 
         female > 0)


########## ANALYSIS 1: SUM BY RACE ########## ########## ########## ########## 
## Rename --------------------------------------------------------------------
# Rename variables in gifted_talented
gifted_talented_sum_by_race <- gifted_talented %>% 
  mutate(gifted_talented, 
         gifted_asian = gifted_asian_m + gifted_asian_f,
         gifted_black = gifted_black_m + gifted_black_f,
         gifted_hisp = gifted_hisp_m + gifted_hisp_f,
         gifted_white = gifted_white_m + gifted_white_f,
         gifted_other = gifted_other_m + gifted_other_f,
         gifted_bipoc = gifted_asian + gifted_black + gifted_hisp + gifted_other) %>% 
  filter(gifted_total > 0) %>% 
  select(COMBOKEY, state, 
         gifted_asian, gifted_black, gifted_hisp, 
         gifted_white, gifted_other, gifted_bipoc, 
         gifted_total)

# Rename variables in enrollment
enrollment_sum_by_race <- enrollment %>% 
  mutate(enrollment, 
         asian = asian_m + asian_f,
         black = black_m + black_f,
         hisp = hisp_m + hisp_f,
         white = white_m + white_f,
         other = other_m + other_f,
         bipoc = asian + black + hisp + other) %>% 
  select(COMBOKEY, 
         asian, black, hisp, white, other, bipoc, 
         total)


## inner_join() -------------------------------------------------------------
gifted_talented_enrollment_by_race <- enrollment_sum_by_race %>% 
  inner_join(gifted_talented_sum_by_race, 
             by = "COMBOKEY")

gifted_talented_enrollment_by_race <- gifted_talented_enrollment_by_race %>% 
  select(COMBOKEY, state, gifted_asian, asian, 
         gifted_black, black, 
         gifted_hisp, hisp, 
         gifted_white, white,
         gifted_other, other, 
         gifted_bipoc, bipoc, 
         gifted_total, total)


## Tidy --------------------------------------------------------------------
gifted_talented_enrollment_by_race <- gifted_talented_enrollment_by_race %>% 
  unite(asian, gifted_asian, asian, sep = "_") %>% 
  unite(black, gifted_black, black, sep = "_") %>% 
  unite(hisp, gifted_hisp, hisp, sep = "_") %>% 
  unite(white, gifted_white, white, sep = "_") %>% 
  unite(other, gifted_other, other, sep = "_") %>% 
  unite(bipoc, gifted_bipoc, bipoc, sep = "_") %>% 
  pivot_longer(c("asian", "black", "hisp", "white", "other", "bipoc"), 
               names_to = "race", values_to = "count") %>% # pivot columns
  separate(count, into = c("gifted_count", "race_count"), sep = "_") %>% # separate race into total 
  # count and GAT count
  mutate(gifted_count = as.numeric(gifted_count), 
         race_count = as.numeric(race_count)) %>% # typeof() = numeric
  filter(total >= 10) %>% # school must have more than 10 students
  select(COMBOKEY, state, race, gifted_count, race_count, 
         gifted_total, total) # select columns

race_levels <- c(
  "asian", "black", "hisp", "white", "other", "bipoc"
) # factor race

race <- factor(gifted_talented_enrollment_by_race$race, levels = race_levels) # factor race


## Export data -------------------------------------------------------------------------
saveRDS(gifted_talented_enrollment_by_race, file = "data/processed/gifted_talented_enrollment_by_race.rds")
write_csv(gifted_talented_enrollment_by_race, file = "data/processed/gifted_talented_enrollment_by_race.csv")


########## ANALYSIS 2: BIPOC REPRESENTATION BY STATE ########## ########## ########## ########## 
## inner_join() -------------------------------------------------------------------------
by_state <- enroll %>% 
  inner_join(GAT, 
             by = "COMBOKEY") %>%    
  mutate(state = state.x, 
         gat_white = gat_white_m + gat_white_f, 
         white = white_m + white_f, 
         gat_bipoc = gat_bipoc_m + gat_bipoc_f, 
         bipoc = bipoc_m + bipoc_f) %>% 
  filter(total >= 10) %>% # rename and collapse variables
  select(state, gat_white, white,
         gat_bipoc, bipoc,
         gat_total, total) # select 


## Calculate ratio -------------------------------------------------------------------------
by_state <- by_state %>% 
  group_by(state) %>% 
  summarize_all(sum) %>% # sum by state
  mutate(bipoc_white_ratio = 
           ((gat_bipoc/bipoc)/(gat_white/white)) * 100) # ratio of white GAT:BIPOC GAT


## Tidy -------------------------------------------------------------------------
by_state <- by_state %>% 
  unite(white, gat_white, white, sep = "_") %>% 
  unite(bipoc, gat_bipoc, bipoc, sep = "_") %>% 
  pivot_longer(c("white", "bipoc"), 
               names_to = "race", values_to = "count") %>% 
  separate(count, into = c("gat_count", "race_count"), sep = "_") %>% # tidy data
  mutate(gat_count = as.numeric(gat_count), 
         race_count = as.numeric(race_count)) %>% 
  select(state, race, gat_count, race_count, bipoc_white_ratio) # organize table


## Export data -------------------------------------------------------------------------
saveRDS(by_state, file = "data/processed/Summation by state_tidy.rds")
write_csv(by_state, file = "data/processed/Summation by state_tidy.csv")


