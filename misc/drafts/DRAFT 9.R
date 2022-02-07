library(tidyverse)
library(ggplot2)


########## DATA IMPORT ########## ########## ########## ########## 
## Data import -------------------------------------------------------------
enroll_raw <- read.csv(file = "data/unprocessed/Enrollment.csv")
gat_raw <- read.csv(file = "data/unprocessed/Gifted and Talented.csv")


## Identify key -----------------------------------------------------------
intersect(names(enroll_raw),
          names(gat_raw))

enroll_raw %>% 
  count(COMBOKEY) %>% 
  filter(n > 1) # Confirm key

gat_raw %>% 
  count(COMBOKEY) %>% 
  filter(n > 1) # Confirm key


## filter() -------------------------------------------------------------------------
# Filter out schools that do not have GAT programs
gat_raw <- gat_raw %>% 
  filter(SCH_GT_IND == "Yes")


## Rename ------------------------------------------------------------------
# Rename variables in GAT
GAT <- gat_raw %>% 
  as_tibble() %>% 
  mutate(gat_raw, 
         state = LEA_STATE, 
         gat_hisp_m = SCH_GTENR_HI_M, gat_hisp_f = SCH_GTENR_HI_F, 
         gat_asian_m = SCH_GTENR_AS_M, gat_asian_f = SCH_GTENR_AS_F,
         gat_black_m = SCH_GTENR_BL_M, gat_black_f = SCH_GTENR_BL_F, 
         gat_white_m = SCH_GTENR_WH_M, gat_white_f = SCH_GTENR_WH_F,
         gat_other_m = SCH_GTENR_AM_M + SCH_GTENR_HP_M + SCH_GTENR_TR_M, 
         gat_other_f = SCH_GTENR_AM_F + SCH_GTENR_HP_F + SCH_GTENR_TR_F, 
         gat_bipoc_m = gat_hisp_m + gat_asian_m + gat_black_m + gat_other_m, 
         gat_bipoc_f = gat_hisp_f + gat_asian_f + gat_black_f + gat_other_f, 
         gat_male = gat_hisp_m + gat_asian_m + gat_black_m + gat_white_m + gat_other_m, 
         gat_female = gat_hisp_f + gat_asian_f + gat_black_f + gat_white_f + gat_other_f, 
         gat_total = gat_male + gat_female) %>% # rename and collapse other
  select(COMBOKEY, state, gat_asian_m, gat_asian_f,
         gat_black_m, gat_black_f, gat_hisp_m, gat_hisp_f, 
         gat_white_m, gat_white_f, gat_other_m, gat_other_f, 
         gat_bipoc_m, gat_bipoc_f, 
         gat_male, gat_female, gat_total)

# Rename variables in enroll
enroll <- enroll_raw %>% 
  as_tibble() %>% 
  mutate(enroll_raw, 
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
enroll <- enroll %>% 
  filter(male > 0, 
         female > 0)



########## ANALYSIS 1: RACE-ETHNICITY ########## ########## ########## ########## 
## Rename --------------------------------------------------------------------
# Rename variables in GAT
GAT_RE <- GAT %>% 
  mutate(GAT, 
         gat_asian = gat_asian_m + gat_asian_f,
         gat_black = gat_black_m + gat_black_f,
         gat_hisp = gat_hisp_m + gat_hisp_f,
         gat_white = gat_white_m + gat_white_f,
         gat_other = gat_other_m + gat_other_f,
         gat_bipoc = gat_asian + gat_black + gat_hisp + gat_other) %>% 
  filter(gat_total > 0) %>% 
  select(COMBOKEY, state, 
         gat_asian, gat_black, gat_hisp, 
         gat_white, gat_other, gat_bipoc, 
         gat_total)

# Rename variables in enroll
enroll_RE <- enroll %>% 
  mutate(enroll, 
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
RE <- enroll_RE %>% 
  inner_join(GAT_RE, 
             by = "COMBOKEY")

RE <- RE %>% 
  select(COMBOKEY, state, gat_asian, asian, 
         gat_black, black, 
         gat_hisp, hisp, 
         gat_white, white,
         gat_other, other, 
         gat_bipoc, bipoc, 
         gat_total, total)


## Tidy --------------------------------------------------------------------
RE <- RE %>% 
  unite(asian, gat_asian, asian, sep = "_") %>% 
  unite(black, gat_black, black, sep = "_") %>% 
  unite(hisp, gat_hisp, hisp, sep = "_") %>% 
  unite(white, gat_white, white, sep = "_") %>% 
  unite(other, gat_other, other, sep = "_") %>% 
  unite(bipoc, gat_bipoc, bipoc, sep = "_") %>% 
  pivot_longer(c("asian", "black", "hisp", "white", "other", "bipoc"), 
               names_to = "race", values_to = "count") %>% # pivot columns
  separate(count, into = c("gat_count", "race_count"), sep = "_") %>% # separate race into total 
  # count and GAT count
  mutate(gat_count = as.numeric(gat_count), 
         race_count = as.numeric(race_count)) %>% # typeof() = numeric
  filter(total >= 10) %>% # school must have more than 10 students
  select(COMBOKEY, state, race, gat_count, race_count, 
         gat_total, total) # select columns

RE_nest <- RE %>% 
  group_by(state, COMBOKEY) %>% 
  nest() # view data by school

race_levels <- c(
  "asian", "black", "hisp", "white", "other", "bipoc"
) # factor race

race <- factor(RE$race, levels = race_levels) # factor race

as_data_frame(RE)
?as_tibble
typeof(RE)

ex6b <- vector("list", ncol(RE))
names(ex6b) <- names(RE)
for (i in names(RE)) {
  ex6b[[i]] <- class(RE[[i]])
}
ex6b

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


ex6b <- vector("list", ncol(by_state))
names(ex6b) <- names(by_state)
for (i in names(by_state)) {
  ex6b[[i]] <- class(by_state[[i]])
}
ex6b

plot_usmap(data = by_state, values = "bipoc_white_ratio", color = "white") + 
  scale_fill_continuous(low = "white", high = "purple4", 
                        name = "Ratio of BIPOC:white students in GAT Programs", label = scales::comma) + 
  theme(legend.position = "bottom")

by_state_barchart <- by_state %>% 
  select(state, bipoc_white_ratio) %>% 
  arrange(-bipoc_white_ratio) %>% 
  unique() 

ggplot(by_state_barchart, aes(state, bipoc_white_ratio)) +
  geom_col(aes(reorder(state, bipoc_white_ratio), fill = bipoc_white_ratio)) + 
  scale_fill_gradient2(low = "white", high = "purple4") +
  coord_flip() 
