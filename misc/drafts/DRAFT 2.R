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