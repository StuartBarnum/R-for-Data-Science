library(tidyverse)

flights %>% ungroup()

#R for Data Science, page 75, expercise 5

#Estiates the relationship between delays in one flight and delays in the next flight 
#departing from the same airport. Crude estimates appear to find at least a weak 
#correlation in the data. Correction is made for the fact that average delays vary 
#with the time
#of day.

fl <- flights %>%
  mutate(dep_hour = sched_dep_time %/% 100) %>%
  group_by(origin, month, dep_hour) %>%
  mutate(ave_delay = mean(arr_delay, na.rm = TRUE)) %>%
  mutate(difference_from_ave = arr_delay - ave_delay) %>%
  arrange(origin, month, day, sched_dep_time) %>%
  mutate(effect_on_next = lead(difference_from_ave) - difference_from_ave) #%>%
  #ungroup() %>%
  #filter(difference_from_ave > 0) %>%  #may induce a regression to the mean "effect"
  #summarize(result = mean(effect_on_next, na.rm = TRUE)) #%>%
  #ungroup() %>%
  #summarize(percentage = mean(result>0, na.rm = TRUE))
  
  filtered <- filter(fl, is.na(effect_on_next))