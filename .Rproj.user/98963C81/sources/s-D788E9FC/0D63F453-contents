## Load packages ----------------------------------------------------------
library(ggplot2)
library(tidyverse)

> # Analysis II: Ratio of BIPOC to white students in GAT programs by state
  
  In this section, I explore how the representation of BIPOC and white students in GAT programs vary by state. This second analysis is far more brief than Analysis I, but I found the findings interesting, and in turn, included two visualizations in this EDA.

### Relational data: `inner_join()`
To start, I joined the data sets `enroll` and `GAT` using a mutating join, `inner_join()`. Again, I matched pairs of observations in `enroll` and `GAT` by `COMBOKEY`, creating one pooled data set `by_state`.
```{r}
by_state <- enroll %>% 
  inner_join(GAT, 
             by = "COMBOKEY")
```
<br>
  
  
  ### Data transformation
  The next step in this process was to transform this data table. First, I renamed and created the variables `gat_white`, `white`, `gat_bipoc`, and `bipoc` by collapsing sex by race I decided to dichotomize `race` for this analysis because I wanted to investigate how the proportion of students of color in GAT programs compares to the proportion of white students in GAT programs. Thus, I only needed two levels in the factor race: `bipoc` and `white`. Additionally, I filtered this data set to only include schools with a total population that is greater than 10. 

Next, I summed the populations of `gat_white`, `white`, `gat_bipoc`, and `bipoc` by state, leaving me with 50 observations (one observation for each state). Using these four variables, I created `bipoc_white_ratio` which is the ratio of BIPOC to white students in GAT programs disaggregated by state. To calculate this ratio, I found the proportion of BIPOC students represented in GAT programs by dividing `gat_bipoc` by `bipoc`. In the same way, I calculated the proportion of white students represented in GAT programs by dividing `gat_white` by `white`. Finally, I used these two proportions to calculate the ratio of BIPOC to white students in GAT programs by dividing `gat_bipoc/bipoc` by `gat_white/white`; I also multiplied this ratio --- `gat_bipoc/bipoc`:`gat_white/white` --- by `100` for scaling purposes.
```{r}
by_state <- by_state %>% 
  mutate(state = state.x, 
         gat_white = gat_white_m + gat_white_f, 
         white = white_m + white_f, 
         gat_bipoc = gat_bipoc_m + gat_bipoc_f, 
         bipoc = bipoc_m + bipoc_f) %>% 
  filter(total >= 10) %>% 
  select(state, gat_white, white,
         gat_bipoc, bipoc,
         gat_total, total) %>% 
  group_by(state) %>% 
  summarize_all(sum) %>% 
  mutate(bipoc_white_ratio = 
           ((gat_bipoc/bipoc)/(gat_white/white)) * 100)





by_state <- by_state %>% 
  unite(white, gat_white, white, sep = "_") %>% 
  unite(bipoc, gat_bipoc, bipoc, sep = "_") %>% 
  pivot_longer(c("white", "bipoc"), 
               names_to = "race", values_to = "count") %>% 
  separate(count, into = c("gat_count", "race_count"), sep = "_") %>% 
  mutate(gat_count = as.numeric(gat_count), 
         race_count = as.numeric(race_count)) %>% 
  select(state, race, gat_count, race_count, bipoc_white_ratio) 

race_levels_by_state <- c(
  "bipoc", "white"
)

race <- factor(by_state$race, levels = race_levels_by_state)

by_state





saveRDS(by_state, file = "data/processed/Summation by state_tidy.rds")
write_csv(by_state, file = "data/processed/Summation by state_tidy.csv")






library(usmap)

plot_usmap(data = by_state, values = "bipoc_white_ratio", color = "white") + 
  scale_fill_continuous(low = "white", high = "purple4", 
                        name = "BIPOC:white ratio", label = scales::comma) + 
  labs(title = "Ratio of BIPOC:white students in GAT programs by state") +
  theme(legend.position = "bottom")

by_state_barchart <- by_state %>% 
  select(state, bipoc_white_ratio) %>% 
  arrange(-bipoc_white_ratio) %>% 
  unique() 

ggplot(by_state_barchart, aes(state, bipoc_white_ratio)) +
  geom_col(aes(reorder(state, bipoc_white_ratio), fill = bipoc_white_ratio)) + 
  scale_fill_gradient2(low = "white", high = "purple4") + 
  scale_y_continuous(name = "Ratio of BIPOC:white students in GAT Programs") + 
  labs(fill = "BIPOC:white ratio") +
  coord_flip() 