## Load packages ----------------------------------------------------------
library(ggplot2)
library(tidyverse)

gifted_talented_enrollment_by_race <- readRDS(file = "data/processed/gifted_talented_enrollment_by_race.rds")


gifted_talented_enrollment_by_race <- gifted_talented_enrollment_by_race %>% 
  mutate(prop_gifted_race = gifted_count / race_count, 
         prop_gifted_total = gifted_total / total) 


gifted_talented_enrollment_by_race$prop_gifted_race[is.nan(gifted_talented_enrollment_by_race$prop_gifted_race)] <- 0 

gifted_talented_enrollment_by_race <- gifted_talented_enrollment_by_race %>% 
  filter(prop_gifted_race < 1) %>% 
  filter(prop_gifted_total < 0.5) %>% 
  filter(prop_gifted_total > 0) 


#### (1) Representation of Asian students in GAT programs:  

gifted_talented_enrollment_by_race %>% 
  filter(race == "asian") %>% 
  ggplot(aes(x = prop_gat_total, y = prop_gat_race)) +
  geom_jitter(color = "grey75", size = 0.4) + 
  geom_smooth(color = "firebrick1", se = F) + 
  geom_abline(intercept = 0, slope = 1, size = 1, color = "grey15") +
  scale_x_continuous(name = "Proportion of all students in GAT program", limits = c(0, 0.5)) +
  scale_y_continuous(name = "Proportion of Asian students in GAT program", limits = c(0,1)) + 
  labs(title = "Representation of Asian students in GAT programs")





#### (2) Representation of Black students in GAT programs:  

gifted_talented_enrollment_by_race %>% 
  filter(race == "black") %>% 
  ggplot(aes(x = prop_gat_total, y = prop_gat_race)) +
  geom_jitter(color = "grey75", size = 0.4) + 
  geom_smooth(color = "goldenrod1", se = F) + 
  geom_abline(intercept = 0, slope = 1, size = 1, color = "grey15") +
  scale_x_continuous(name = "Proportion of all students in GAT program", limits = c(0, 0.5)) +
  scale_y_continuous(name = "Proportion of Black students in GAT program", limits = c(0,1)) + 
  labs(title = "Representation of Black students in GAT programs")


  


#### (3) Representation of Hispanic students in GAT programs:  

gifted_talented_enrollment_by_race %>% 
  filter(race == "hisp") %>% 
  ggplot(aes(x = prop_gat_total, y = prop_gat_race)) +
  geom_jitter(color = "grey75", size = 0.4) + 
  geom_smooth(color = "darkolivegreen3", se = F) + 
  geom_abline(intercept = 0, slope = 1, size = 1, color = "grey15") +
  scale_x_continuous(name = "Proportion of all students in GAT program", limits = c(0, 0.5)) +
  scale_y_continuous(name = "Proportion of Hispanic students in GAT program", limits = c(0,1)) + 
  labs(title = "Representation of Hispanic students in GAT programs")





#### (4) Representation of white students in GAT programs:  
gifted_talented_enrollment_by_race %>% 
  filter(race == "white") %>% 
  ggplot(aes(x = prop_gat_total, y = prop_gat_race)) +
  geom_jitter(color = "grey75", size = 0.4) + 
  geom_smooth(color = "slateblue1", se = F) + 
  geom_abline(intercept = 0, slope = 1, size = 1, color = "grey15") +
  scale_x_continuous(name = "Proportion of all students in GAT program", limits = c(0, 0.5)) +
  scale_y_continuous(name = "Proportion of white students in GAT program", limits = c(0,1)) + 
  labs(title = "Representation of white students in GAT programs")


  

#### (5) Representation of other race students in GAT programs:  
gifted_talented_enrollment_by_race %>% 
  filter(race == "other") %>% 
  ggplot(aes(x = prop_gat_total, y = prop_gat_race)) +
  geom_jitter(color = "grey75", size = 0.4) + 
  geom_smooth(color = "cornflowerblue", se = F) + 
  geom_abline(intercept = 0, slope = 1, size = 1, color = "grey15") +
  scale_x_continuous(name = "Proportion of all students in GAT program", limits = c(0, 0.5)) +
  scale_y_continuous(name = "Proportion of other race students in GAT program", limits = c(0,1)) + 
  labs(title = "Representation of other race students in GAT programs")





#### (6) Representation of students by race in GAT programs:  

gifted_talented_enrollment_by_race %>% 
  filter(race != "bipoc") %>% 
  ggplot(aes(prop_gat_total, prop_gat_race, color = race)) + 
  geom_smooth(se = F) + 
  scale_color_manual(values = c("firebrick1", "goldenrod1", 
                                "darkolivegreen3", "cornflowerblue", "slateblue1")) +
  geom_abline(intercept = 0, slope = 1, size = 1, color = "grey15") +
  scale_x_continuous(name = "Proportion of all students in GAT program", limits = c(0, 0.5)) +
  scale_y_continuous(name = "Proportion of students in GAT program by race", limits = c(0,1)) + 
  labs(title = "Representation of students by race in GAT programs")





#### (7) Representation of BIPOC students in GAT programs:  
gifted_talented_enrollment_by_race %>% 
  filter(race == "bipoc") %>% 
  ggplot(aes(x = prop_gat_total, y = prop_gat_race)) + 
  geom_jitter(color = "grey75", size = 0.4) + 
  geom_smooth(color = "darkorange", se = F) + 
  geom_abline(intercept = 0, slope = 1, size = 1) + 
  scale_x_continuous(name = "Total in GAT / Total at school", limits = c(0, 0.5)) +
  scale_y_continuous(name = "Asians in GAT / Asians at school", limits = c(0,1)) + 
  labs(title = "Representation of BIPOC students in GAT programs")


  


#### (8) Representation of BIPOC and white students in GAT programs:  
gifted_talented_enrollment_by_race %>% 
  filter(race == c("bipoc", "white")) %>% 
  ggplot(aes(prop_gat_total, prop_gat_race, color = race)) + 
  geom_smooth(se = F) + 
  scale_color_manual(values = c("dark orange", "slateblue1")) +
  geom_abline(intercept = 0, slope = 1, size = 1, color = "grey15") +
  scale_x_continuous(name = "Proportion of all students in GAT program", limits = c(0, 0.5)) +
  scale_y_continuous(name = "Proportion of students in GAT program by race", limits = c(0,1)) + 
  labs(title = "Representation of BIPOC and white students in GAT programs")



#### (9) Representation of all students in GAT programs:  

gifted_talented_enrollment_by_race %>% 
  ggplot(aes(prop_gat_total, prop_gat_race, color = race)) + 
  geom_smooth(se = F) + 
  scale_color_manual(values = c("firebrick1", "darkorange", "goldenrod1", 
                                "darkolivegreen3", "cornflowerblue", "slateblue1")) +
  geom_abline(intercept = 0, slope = 1, size = 1, color = "grey15") +
  scale_x_continuous(name = "Proportion of all students in GAT program", limits = c(0, 0.5)) +
  scale_y_continuous(name = "Proportion of students in GAT program by race", limits = c(0,1)) + 
  labs(title = "Representation of all students by race in GAT programs")


#### (10) Representation of all students in GAT programs, facetted by race:  

gifted_talented_enrollment_by_race %>% 
  ggplot(aes(x = prop_gat_total, y = prop_gat_race, color = race)) + 
  geom_jitter(color = "grey75", size = 0.4) + 
  geom_smooth() + 
  scale_color_manual(values = c("firebrick1", "darkorange", "goldenrod1", 
                                "darkolivegreen3", "cornflowerblue", "slateblue1")) +
  geom_abline(intercept = 0, slope = 1, size = 1, color = "grey15") +
  facet_wrap(vars(race)) + theme(legend.position = "none") +   
  scale_x_continuous(name = "Proportion of all students in GAT program", limits = c(0, 0.5)) +
  scale_y_continuous(name = "Proportion of students in GAT program by race", limits = c(0,1)) +
  labs(title = "Representation of all students by race in GAT programs")





