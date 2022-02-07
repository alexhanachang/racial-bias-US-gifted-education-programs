## Load packages ----------------------------------------------------------
library(tidyverse)
library(readr)
library(nycflights13)

## Data collection ---------------------------------------------------------


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
         indig_m = SCH_ENR_AM_M, indig_f = SCH_ENR_AM_F, 
         asian_m = SCH_ENR_AS_M, asian_f = SCH_ENR_AS_F, 
         nhpi_m = SCH_ENR_HP_M, nhpi_f = SCH_ENR_HP_F, 
         black_m = SCH_ENR_BL_M, black_f = SCH_ENR_BL_F, 
         white_m = SCH_ENR_WH_M, white_f = SCH_ENR_WH_F, 
         multi_m = SCH_ENR_TR_M, multi_f = SCH_ENR_TR_F,
         male = TOT_ENR_M, female = TOT_ENR_F,
         total = TOT_ENR_M + TOT_ENR_F) %>% # rename
  select(COMBOKEY, state, hisp_m, hisp_f, indig_m, indig_f, 
         asian_m, asian_f, nhpi_m, nhpi_f, black_m, 
         black_f, white_m, white_f, multi_m, multi_f, 
         male, female, total) # select
  
sapply(enroll_simple, class)

i <- c(3:19)
enroll_simple[ , i] <- apply(enroll_simple[ , i], 2,            # Specify own function within apply
                    function(x) as.numeric(as.character(x)))




force_vec <- function(x) {
  as.numeric(as.integer((x)))
}

enroll_simple %>% 
  force_vec(hisp_m)

as.numeric(c(enroll_simple$hisp_m))

typeof(enroll_raw)
typeof(enroll_simple$total)
typeof(enroll_simple$hisp_m)
typeof(enroll_simple$female)

## gat_simple --------------------------------------------------------------
gat_filtered <- gat_raw %>% 
  group_by(SCH_GT_IND) %>% 
  filter(SCH_GT_IND == "Yes") # Filter out schools without GAT programs

gat_simple <- gat_filtered %>% 
  mutate(gat_filtered, 
         gat_hisp_m = SCH_GTENR_HI_M, gat_hisp_f = SCH_GTENR_HI_F, 
         gat_indig_m = SCH_GTENR_AM_M, gat_indig_f = SCH_GTENR_AM_F, 
         gat_asian_m = SCH_GTENR_AS_M, gat_asian_f = SCH_GTENR_AS_F,
         gat_nhpi_m = SCH_GTENR_HP_M, gat_nhpi_f = SCH_GTENR_HP_F, 
         gat_black_m = SCH_GTENR_BL_M, gat_black_f = SCH_GTENR_BL_F, 
         gat_white_m = SCH_GTENR_WH_M, gat_white_f = SCH_GTENR_WH_F,
         gat_multi_m = SCH_GTENR_TR_M, gat_multi_f = SCH_GTENR_TR_F, 
         gat_male = TOT_GTENR_M, gat_female = TOT_GTENR_F, 
         gat_total = TOT_GTENR_M + TOT_GTENR_F) %>% # rename
  select(COMBOKEY, gat_hisp_m, gat_hisp_f, gat_indig_m, gat_indig_f, 
         gat_asian_m, gat_asian_f, gat_nhpi_m, gat_nhpi_f, gat_black_m, 
         gat_black_f, gat_white_m, gat_white_f, gat_multi_m, gat_multi_f, 
         gat_male, gat_female, gat_total) # select


sapply(gat_simple, class)

i <- c(3:19)
gat_simple[ , i] <- apply(gat_simple[ , i], 2,            # Specify own function within apply
                             function(x) as.numeric(as.character(x)))





## inner_join() ------------------------------------------------------------
enroll_gat <- enroll_simple %>% 
  inner_join(gat_simple, by = "COMBOKEY")

sapply(enroll_gat, class)


## Race-Ethnicity (RE) variables -------------------------------------------
RE <- enroll_gat %>% 
  mutate(enroll_gat, 
         asian = asian_m + asian_f,
         black = black_m + black_f,
         hisp = hisp_m + hisp_f,
         white = white_m + white_f,
         other = indig_m + indig_f +
           nhpi_m + nhpi_f +
           multi_m + multi_f) %>% # enrollment totals by race
  mutate(enroll_gat, 
         gat_asian = gat_asian_m + gat_asian_f,
         gat_black = gat_black_m + gat_black_f,
         gat_hisp = gat_hisp_m + gat_hisp_f,
         gat_white = gat_white_m + gat_white_f,
         gat_other = gat_indig_m + gat_indig_f +
           gat_nhpi_m + gat_nhpi_f + 
           gat_multi_m + gat_multi_f) %>% # GAT totals by race
  select(state, asian, black, hisp, white, other, gat_asian, gat_black,
         gat_hisp, gat_white, gat_other, gat_total, total)

sapply(RE, class)

RE %>% 
  group_by(state)

RE_summed <- RE %>% 
  group_by(state) %>% 
  summarise_all(sum)

force_fact <- function(x) {
  as.factor(as.character((x)))
}

force_fact(RE$state)

RE$state <- force_fact(RE$state)
typeof(RE$state)

RE %>% 
  aggregate(asian, by=list(Category=state), FUN=sum)



iris %>%                                        # Specify data frame
  group_by(Species) %>%                         # Specify group indicator
  summarise_at(vars(Sepal.Length),              # Specify column
               list(name = sum))  

RE %>% 
  group_by(RE$state) %>% 
  summarize_at(vars(asian, black, hisp, white, other, gat_asian, gat_black,
                    gat_hisp, gat_white, gat_other, gat_total, total),
               list(name = state))



RE5 <- enroll_gat %>% 
  mutate(enroll_gat, 
         asian = asian_m + asian_f,
         black = black_m + black_f,
         hisp = hisp_m + hisp_f,
         white = white_m + white_f,
         other = indig_m + indig_f +
           nhpi_m + nhpi_f +
           multi_m + multi_f) %>% # enrollment totals by race
  mutate(enroll_gat, 
         gat_asian = gat_asian_m + gat_asian_f,
         gat_black = gat_black_m + gat_black_f,
         gat_hisp = gat_hisp_m + gat_hisp_f,
         gat_white = gat_white_m + gat_white_f,
         gat_other = gat_indig_m + gat_indig_f +
           gat_nhpi_m + gat_nhpi_f + 
           gat_multi_m + gat_multi_f) %>% # GAT totals by race
  select(state, asian, black)

RE2<-RE %>% 
  mutate(RE, 
         asian_prop = asian / total,
         black_prop = black / total,
         hisp_prop = hisp / total,
         white_prop = white / total, 
         other_prop = other / total) %>% # enrollment proportions by race
  mutate(RE, 
         gat_asian_prop = gat_asian / gat_total,
         gat_black_prop = gat_black / gat_total,
         gat_hisp_prop = gat_hisp / gat_total,
         gat_white_prop = gat_white / gat_total,
         gat_other_prop = gat_other / gat_total) %>% # GAT proportions by race
  mutate(enroll_gat,
         gat_prop = gat_total / total) %>% # Proportion of school in GAT
  select(COMBOKEY, state, gat_asian, gat_asian_prop, asian, asian_prop,
         gat_black, gat_black_prop, black, black_prop,
         gat_hisp, gat_hisp_prop, hisp, hisp_prop, 
         gat_white, gat_white_prop, white, white_prop, 
         gat_other, gat_other_prop, other, other_prop, 
         gat_total, gat_prop, total) %>% 
  mutate_if(is.numeric, round, digits = 2)



state_vector <- unique(c(RE$state))
state_vector

aggregate(x$Frequency, by=list(Category=x$Category), FUN=sum)

aggregate(RE5$asian, by= list(states = x$state), FUN = sum) %>% 
  aggregate(RE5$black, by= list(states = x$state), FUN = sum)




aggregate(x = iris$Sepal.Length,                # Specify data column
          by = list(s),              # Specify group indicator
          FUN = sum)   




vars



sum_function <- function(x) {
  
}

RE3 <- RE %>% 
  group_by(state) %>% 
  if




# pie chart of gat / population

colSums(RE2 %>% 
  cbind(x = "gat_asian", y = "asian") %>% 
  select(gat_asian, asian))

colSums(y)

RE2 %>% 
  group_by(state)

group_by()
colSum

RE <- cbind(RE)

RE3 <- RE2 %>% 
  mutate(easy_prop = fct_collapse(RE2,
                                  0.5 = c(0.5 <= "0.5" <1)))




RE_prop <- RE %>% 
  mutate(RE,
         gat_bipoc = (gat_asian + gat_black + gat_hisp + gat_other)/gat_total,
         gat_white_prop = (gat_white/gat_total)) %>% 
  select(gat_bipoc, gat_white_prop, total)
RE %>% 
  colSums("asian", na.rm = FALSE, dims = 1)

?sum
x <- cbind(x1 = 3, x2 = c(4:1, 2:5))
rowSums(x); colSums(x)


cbind (x1 = )

