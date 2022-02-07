## Load packages ----------------------------------------------------------
library(tidyverse)
library(ggplot2)
library(readr)

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
         gat_male = gat_hisp_m + gat_asian_m + gat_black_m + gat_white_m + gat_other_m, 
         gat_female = gat_hisp_f + gat_asian_f + gat_black_f + gat_white_f + gat_other_f, 
         gat_total = gat_male + gat_female) # rename and collapse other

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
         male = hisp_m + asian_m + black_m + white_m + other_m, 
         female = hisp_f + asian_f + black_f + white_f + other_f, 
         total = male + female) # rename and collapse other


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


## Calculate proportions ---------------------------------------------------
RE_prop <- RE %>% 
  mutate(prop_gat_race = gat_count / race_count, # proportion of racialized students in GAT
         prop_gat_total = gat_total / total) # proportion of school in GAT

RE_prop$prop_gat_race[is.nan(RE_prop$prop_gat_race)] <- 0 # replace NaN values
RE_prop$prop_gat_race

RE_prop <- RE_prop %>% 
  mutate(prop_gat_race = 
           round(prop_gat_race, digits = 2), 
         prop_gat_total = 
           round(prop_gat_total, digits = 2)) # round

RE_prop <- RE_prop %>% 
  filter(prop_gat_race < 1) %>% # proportion of race must be less than 1
  filter(prop_gat_total < 0.5) %>% # define GAT to be schools where less than
                                   # 50% of students are in GAT programs
  filter(prop_gat_total > 0) # there must be at least 1 student in GAT at school


## Visualizations ----------------------------------------------------------
# Asian
RE_prop %>% 
  filter(race == "asian") %>% 
  ggplot(aes(x = prop_gat_total, y = prop_gat_race)) +
  geom_jitter(color = "grey75", size = 0.4) + 
  geom_smooth(color = "firebrick1", se = F) + 
  geom_abline(intercept = 0, slope = 1, size = 1, color = "grey15") +
  scale_x_continuous(name = "Proportion of all students in GAT program", limits = c(0, 0.5)) +
  scale_y_continuous(name = "Proportion of Asian students in GAT program", limits = c(0,1)) + 
  labs(title = "Representation of Asian students in GAT programs")

# Black
RE_prop %>% 
  filter(race == "black") %>% 
  ggplot(aes(x = prop_gat_total, y = prop_gat_race)) +
  geom_jitter(color = "grey75", size = 0.4) + 
  geom_smooth(color = "goldenrod1", se = F) + 
  geom_abline(intercept = 0, slope = 1, size = 1, color = "grey15") +
  scale_x_continuous(name = "Proportion of all students in GAT program", limits = c(0, 0.5)) +
  scale_y_continuous(name = "Proportion of Black students in GAT program", limits = c(0,1)) + 
  labs(title = "Representation of Black students in GAT programs")

# Hisp
RE_prop %>% 
  filter(race == "hisp") %>% 
  ggplot(aes(x = prop_gat_total, y = prop_gat_race)) +
  geom_jitter(color = "grey75", size = 0.4) + 
  geom_smooth(color = "darkolivegreen3", se = F) + 
  geom_abline(intercept = 0, slope = 1, size = 1, color = "grey15") +
  scale_x_continuous(name = "Proportion of all students in GAT program", limits = c(0, 0.5)) +
  scale_y_continuous(name = "Proportion of Hispanic students in GAT program", limits = c(0,1)) + 
  labs(title = "Representation of Hispanic students in GAT programs")

# White
RE_prop %>% 
  filter(race == "hisp") %>% 
  ggplot(aes(x = prop_gat_total, y = prop_gat_race)) +
  geom_jitter(color = "grey75", size = 0.4) + 
  geom_smooth(color = "slateblue1", se = F) + 
  geom_abline(intercept = 0, slope = 1, size = 1, color = "grey15") +
  scale_x_continuous(name = "Proportion of all students in GAT program", limits = c(0, 0.5)) +
  scale_y_continuous(name = "Proportion of white students in GAT program", limits = c(0,1)) + 
  labs(title = "Representation of white students in GAT programs")

# Other
RE_prop %>% 
  filter(race == "other") %>% 
  ggplot(aes(x = prop_gat_total, y = prop_gat_race)) +
  geom_jitter(color = "grey75", size = 0.4) + 
  geom_smooth(color = "cornflowerblue", se = F) + 
  geom_abline(intercept = 0, slope = 1, size = 1, color = "grey15") +
  scale_x_continuous(name = "Proportion of all students in GAT program", limits = c(0, 0.5)) +
  scale_y_continuous(name = "Proportion of other race students in GAT program", limits = c(0,1)) + 
  labs(title = "Representation of other race students in GAT programs")

# Asian, Black, Hisp, white, other
RE_prop %>% 
  filter(race != "bipoc") %>% 
  ggplot(aes(prop_gat_total, prop_gat_race, color = race)) + 
  geom_smooth(se = F) + 
  scale_color_manual(values = c("firebrick1", "goldenrod1", 
                                "darkolivegreen3", "cornflowerblue", "slateblue1")) +
  geom_abline(intercept = 0, slope = 1, size = 1, color = "grey15") +
  scale_x_continuous(name = "Proportion of all students in GAT program", limits = c(0, 0.5)) +
  scale_y_continuous(name = "Proportion of students in GAT program by race", limits = c(0,1)) + 
  labs(title = "Representation of students by race in GAT programs")

# BIPOC
RE_prop %>% 
  filter(race == "bipoc") %>% 
  ggplot(aes(x = prop_gat_total, y = prop_gat_race)) + 
  geom_jitter(color = "grey75", size = 0.4) + 
  geom_smooth(color = "darkorange", se = F) + 
  geom_abline(intercept = 0, slope = 1, size = 1) + 
  scale_x_continuous(name = "Total in GAT / Total at school", limits = c(0, 0.5)) +
  scale_y_continuous(name = "Asians in GAT / Asians at school", limits = c(0,1)) + 
  labs(title = "Representation of BIPOC students in GAT programs")

# BIPOC, white
RE_prop %>% 
  filter(race == c("bipoc", "white")) %>% 
  ggplot(aes(prop_gat_total, prop_gat_race, color = race)) + 
  geom_smooth(se = F) + 
  scale_color_manual(values = c("dark orange", "slateblue1")) +
  geom_abline(intercept = 0, slope = 1, size = 1, color = "grey15") +
  scale_x_continuous(name = "Proportion of all students in GAT program", limits = c(0, 0.5)) +
  scale_y_continuous(name = "Proportion of students in GAT program by race", limits = c(0,1)) + 
  labs(title = "Representation of BIPOC and white students in GAT programs")

# all race geom_smooth()
RE_prop %>% 
  ggplot(aes(prop_gat_total, prop_gat_race, color = race)) + 
  geom_smooth(se = F) + 
  scale_color_manual(values = c("firebrick1", "darkorange", "goldenrod1", 
                                "darkolivegreen3", "cornflowerblue", "slateblue1")) +
  geom_abline(intercept = 0, slope = 1, size = 1, color = "grey15") +
  scale_x_continuous(name = "Proportion of all students in GAT program", limits = c(0, 0.5)) +
  scale_y_continuous(name = "Proportion of students in GAT program by race", limits = c(0,1)) + 
  labs(title = "Representation of all students by race in GAT programs")

# all race facet_wrap()
RE_prop %>% 
  ggplot(aes(x = prop_gat_total, y = prop_gat_race, color = race)) + 
  geom_jitter(color = "grey75", size = 0.4) + 
  geom_smooth() + 
  scale_color_manual(values = c("firebrick1", "darkorange", "goldenrod1", 
                                "darkolivegreen3", "cornflowerblue", "slateblue1")) +
  geom_abline(intercept = 0, slope = 1, size = 1, color = "grey15") +
  facet_wrap(vars(race)) + theme(legend.position = "none") + 
  labs(title = "Representation of all students by race in GAT programs")



########## ANALYSIS 2: AN INTERSECTIONAL APPROACH ########## ########## ########## ########## 
# -------------------------------------------------------------------------

########## ANALYSIS 3: BIPOC REPRESENTATION BY STATE ########## ########## ########## ########## 
by_state <- enroll %>% 
  inner_join(GAT, 
             by = "COMBOKEY") %>% 
  mutate(state = state.x, 
         gat_white = gat_white_m + gat_white_f, 
         white = white_m + white_f, 
         gat_bipoc = gat_bipoc_m + gat_bipoc_f, 
         bipoc = bipoc_m + bipoc_f) %>% 
  select(state, gat_white, white,
         gat_bipoc, bipoc, 
         gat_total, total)

by_state <- by_state %>% 
  group_by(state) %>% 
  summarize_all(sum)

by_state <- by_state %>% 
  unite(white, gat_white, white, sep = "_") %>% 
  unite(bipoc, gat_bipoc, bipoc, sep = "_") %>% 
  pivot_longer(c("white", "bipoc"), 
               names_to = "race", values_to = "count") %>% 
  separate(count, into = c("gat_count", "race_count"), sep = "_") %>% 
  mutate(gat_count = as.numeric(gat_count), 
         race_count = as.numeric(race_count)) %>% 
  filter(total >= 10) %>% 
  select(state, race, gat_count, gat_total, 
         race_count, total)

by_state %>% 
  mutate(gat_prop = gat_count / race_count)

by_stateX <- by_state %>% 
  mutate(gat_prop = gat_count / race_count)

bipoc:white

plot_usmap(data = by_stateX, values = "gat_prop", color = "red") + 
  scale_fill_continuous(
    low = "white", high = "red", name = "Population (2015)", label = scales::comma
  ) + theme(legend.position = "right")









