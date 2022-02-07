## Load packages ----------------------------------------------------------
library(tidyverse)
library(ggplot2)
library(readr)

## Data import -------------------------------------------------------------
enroll_raw <- read.csv(file = "data/unprocessed/Enrollment.csv")
gat_raw <- read.csv(file = "data/unprocessed/Gifted and Talented.csv")


## Prep for join -----------------------------------------------------------
intersect(names(enroll_raw),
          names(gat_raw))

enroll_raw %>% 
  count(COMBOKEY) %>% 
  filter(n > 1) # Confirm key

gat_raw %>% 
  count(COMBOKEY) %>% 
  filter(n > 1) # Confirm key


## enroll_simple -------------------------------------------------------------------------
enroll_simple <- enroll_raw %>% 
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
         total = male + female) %>% # rename and collapse
  select(COMBOKEY, state, hisp_m, hisp_f, 
         asian_m, asian_f, black_m, black_f, 
         white_m, white_f, other_m, other_f, 
         male, female, total) # select

sapply(enroll_simple, class) # vector types

x <- c(3:15) # change columns 3 - 15 to numeric
enroll_simple[,x]

enroll_simple[,x] <- apply(enroll_simple[,x], 2, 
                           function(x) {as.numeric(as.integer(x))})

sapply(enroll_simple, class) # vector types after transformation


## gat_filtered --------------------------------------------------------------
gat_simple <- gat_raw %>% 
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
         gat_total = gat_male + gat_female) %>% # rename and collapse
  select(COMBOKEY, state, SCH_GT_IND, 
         gat_hisp_m, gat_hisp_f, 
         gat_asian_m, gat_asian_f, 
         gat_black_m, gat_black_f, 
         gat_white_m, gat_white_f, 
         gat_other_m, gat_other_f, 
         gat_male, gat_female, gat_total) # select

sapply(gat_simple, class) # vector types

y <- c(4:16) # change columns 4 - 16 to numeric
gat_simple[,y]

gat_simple[,y] <- apply(gat_simple[,y], 2, 
                        function(x) {as.numeric(as.integer(x))}) # change columns 3 - 15 to numeric

sapply(gat_simple, class) # vector types after transformation


## inner_join() ------------------------------------------------------------
join1 <- enroll_simple %>% 
  inner_join(gat_simple, 
             by = "COMBOKEY", 
             suffix = c("", "")) # inner_join()


## enroll_gat --------------------------------------------------------------
enroll_gat <- join1 %>% 
  filter(SCH_GT_IND == "Yes", 
         gat_total > 0) %>% # filter schools with GAT program
  filter(male > 0, 
         female > 0) %>% # filter schools with males AND females
  mutate(prop_gat = gat_total / total) %>% 
  filter(prop_gat < 1) %>% 
  select(COMBOKEY, state, 
         gat_asian_m, asian_m, gat_asian_f,asian_f,
         gat_black_m, black_m, gat_black_f, black_f, 
         gat_hisp_m, hisp_m, gat_hisp_f, hisp_f, 
         gat_white_m, white_m, gat_white_f, white_f, 
         gat_other_m, other_m, gat_other_f, other_f, 
         gat_male, male, gat_female, female, 
         gat_total, total) # select

sapply(enroll_gat, class) # vector types


## Race-Ethnicity (RE) population by school -------------------------------------------
RE_pop <- enroll_gat %>% 
  as_tibble() %>% 
  mutate(enroll_gat, 
         asian = asian_m + asian_f,
         black = black_m + black_f,
         hisp = hisp_m + hisp_f,
         white = white_m + white_f,
         other = other_m + other_f, # enrollment totals by race
         bipoc = asian + black + hisp + other) %>% 
            # BIPOC vs white
  mutate(enroll_gat, 
         gat_asian = gat_asian_m + gat_asian_f,
         gat_black = gat_black_m + gat_black_f,
         gat_hisp = gat_hisp_m + gat_hisp_f,
         gat_white = gat_white_m + gat_white_f,
         gat_other = gat_other_m + gat_other_f, # GAT totals by race
         gat_bipoc = gat_asian + gat_black + gat_hisp + gat_other) %>% 
            # GAT BIPOC vs GAT white
  mutate(enroll_gat, 
         prop_gat_asian = gat_asian / asian, 
         prop_gat_black = gat_black / black, 
         prop_gat_hisp = gat_hisp / hisp, 
         prop_gat_white = gat_white / white, 
         prop_gat_other = gat_other / other, 
         prop_gat_bipoc = gat_bipoc / bipoc) %>% # GAT:population ratios
  na.omit() %>% # omit NaN
  select(state, prop_gat_asian, gat_asian, asian, 
         prop_gat_black, gat_black, black, 
         prop_gat_hisp, gat_hisp, hisp, 
         prop_gat_white, gat_white, white, 
         prop_gat_other, gat_other, other, 
         prop_gat_bipoc, gat_bipoc, bipoc,
         gat_total, total)


RE_pop <- RE_pop %>% 
  filter(prop_gat_asian <= 1, 
         prop_gat_black <= 1, 
         prop_gat_hisp <= 1, 
         prop_gat_white <= 1, 
         prop_gat_other <= 1, 
         prop_gat_bipoc <= 1)

RE_pop2 <- RE_pop %>% 
  filter(prop_gat_asian <= 0.5, 
         prop_gat_black <= 0.5, 
         prop_gat_hisp <= 0.5, 
         prop_gat_white <= 0.5, 
         prop_gat_other <= 0.5, 
         prop_gat_bipoc <= 0.5) %>% 
  mutate(prop_gat_white =
           round(RE_pop2$prop_gat_white, digits = 2))




ggplot(RE_pop2, aes(prop_gat_white, prop_gat_black)) + 
  geom_jitter(aes(prop_gat_white, prop_gat_black), color = "red") + 
  geom_smooth()

ggplot(RE_pop2, aes(prop_gat_white, prop_gat_bipoc)) + 
  geom_jitter(aes(prop_gat_white, prop_gat_bipoc), color = "red") + 
  geom_smooth()

ggplot(RE_pop2, aes(prop_gat_white, prop_gat_asian)) + 
  geom_jitter(aes(prop_gat_white, prop_gat_asian), color = "red") + 
  geom_smooth()

geom_jitter(aes(prop_gat_white, prop_gat_asian), color = "blue")

RE_pop %>% ggplot(data = RE_pop) +
  geom_point(aes(prop_gat_white, prop_gat_black), color = "red") + geom_smooth(aes(prop_gat_white, prop_gat_black), color = "black") +
  geom_point(aes(prop_gat_white, prop_gat_asian), color = "blue") + geom_smooth(aes(prop_gat_white, prop_gat_asian), color = "black")

ggplot(RE_pop, aes(prop_gat_white, prop_gat_black)) + geom_smooth()

# RE population by state --------------------------------------------------

by_stateX <- RE_pop2 %>% 
  group_by(prop_gat_white,state) %>% 
  nest() %>% 
  group_by(state) %>% 
  summarize(sum)


RE_pop3 <- enroll_gat %>% 
  as_tibble() %>% 
  mutate(enroll_gat, 
         asian = asian_m + asian_f,
         black = black_m + black_f,
         hisp = hisp_m + hisp_f,
         white = white_m + white_f,
         other = other_m + other_f, # enrollment totals by race
         bipoc = asian + black + hisp + other) %>% 
  # BIPOC vs white
  mutate(enroll_gat, 
         gat_asian = gat_asian_m + gat_asian_f,
         gat_black = gat_black_m + gat_black_f,
         gat_hisp = gat_hisp_m + gat_hisp_f,
         gat_white = gat_white_m + gat_white_f,
         gat_other = gat_other_m + gat_other_f, # GAT totals by race
         gat_bipoc = gat_asian + gat_black + gat_hisp + gat_other) %>% 
  # GAT BIPOC vs GAT white
  mutate(enroll_gat, 
         prop_gat_white = gat_white / white) %>% # GAT:population ratios
  filter(prop_gat_white <= 0.5) %>% 
  mutate(prop_gat_white =
           round(RE_pop3$prop_gat_white, digits = 2)) %>% 
  select(prop_gat_white, gat_white, white, 
          gat_asian, asian, 
         gat_black, black, 
         gat_hisp, hisp, 
         gat_other, other, 
         gat_bipoc, bipoc,
         gat_total, total)


RE4<-RE_pop3 %>% 
  mutate(prop_gat_yt = as.character(prop_gat_white)) %>% 
  select(prop_gat_yt, gat_white, white, 
         gat_asian, asian, 
         gat_black, black, 
         gat_hisp, hisp, 
         gat_other, other, 
         gat_bipoc, bipoc,
         gat_total, total)

RE6 <- RE4 %>% 
  group_by(prop_gat_yt) %>% 
  summarize_all(sum)


RE7 <- RE6 %>% 
  mutate(RE6, 
         prop_gat_yt = as.numeric(as.character(prop_gat_yt)))

typeof(RE7$prop_gat_yt)
  
RE7 <- RE7 %>% 
  mutate(RE7, 
       prop_gat_asian = gat_asian / asian, 
       prop_gat_black = gat_black / black, 
       prop_gat_hisp = gat_hisp / hisp, 
       prop_gat_other = gat_other / other, 
       prop_gat_bipoc = gat_bipoc / bipoc) %>% # GAT:population ratios
  na.omit() %>% # omit NaN
  select(prop_gat_yt, gat_white, white, 
         prop_gat_asian, gat_asian, asian, 
         prop_gat_black, gat_black, black, 
         prop_gat_hisp, gat_hisp, hisp, 
         prop_gat_other, gat_other, other, 
         prop_gat_bipoc, gat_bipoc, bipoc,
         gat_total, total)


ggplot(RE7, aes(prop_gat_yt, prop_gat_black)) + 
  geom_point(aes(prop_gat_yt, prop_gat_black), color = "red") + 
  geom_smooth() + 
  ylim(0, 0.5)

ggplot(RE7, aes(prop_gat_yt, prop_gat_bipoc)) + 
  geom_jitter(aes(prop_gat_yt, prop_gat_bipoc), color = "red") + 
  geom_smooth()+ 
  ylim(0, 0.5)

ggplot(RE7, aes(prop_gat_yt, prop_gat_asian)) + 
  geom_jitter(aes(prop_gat_yt, prop_gat_asian), color = "red") + 
  geom_smooth()+ 
  ylim(0, 0.5)

ggplot(RE7, aes(prop_gat_yt, prop_gat_asian)) + 
  geom_point(aes(prop_gat_yt, prop_gat_asian), color = "red") + 
  geom_smooth()+ 
  ylim(0, 0.5)

ggplot(RE7, aes(prop_gat_yt, prop_gat_yt)) + 
  geom_point(aes(prop_gat_yt, prop_gat_yt), color = "red") + 
  geom_smooth()+ 
  ylim(0, 0.5)

ggplot(RE7, aes(prop_gat_yt, prop_gat_hisp)) + 
  geom_jitter(aes(prop_gat_yt, prop_gat_hisp), color = "red") + 
  geom_smooth()+ 
  ylim(0, 0.5)

ggplot(RE7, aes(prop_gat_yt, prop_gat_other)) + 
  geom_jitter(aes(prop_gat_yt, prop_gat_other), color = "red") + 
  geom_smooth()+ 
  ylim(0, 0.5)




by_yts <- RE_pop2 %>% 
  mutate(as.character(prop_gat_white)) %>% 
  group_by(prop_gat_white) %>% 
  nest()
  



typeof(RE_pop2$prop_gat_white)
  
  arrange(prop_gat_white, state, everything()) %>% 
  group_by(prop_gat_white)

RE_state <- RE_pop %>% 
  group_by(state) %>% 
  summarise_all(sum) %>% 
  select(state:asian, gat_black, black, 
         gat_hisp, hisp, gat_white, white, 
         gat_other, other, gat_bipoc, bipoc, 
         gat_total, total)

ggplot(RE_state, mapping = aes(x = state, y = asian)) + 
  geom_bar()


write.csv(RE_state, file = "RE_state")



