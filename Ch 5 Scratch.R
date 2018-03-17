smaller <- diamonds %>%
  filter(carat < 3)

ggplot(data = smaller, mapping = aes(x = carat, y = price)) + 
  geom_histogram(mapping = aes(group = cut_width(carat, 0.1)))

library(nycflights13)
fl <- flights

dest <- fl %>% group_by(dest, month) %>%
  summarize(non_arrivals = sum(is.na(arr_delay)) / n(), 
            ave = mean(arr_delay, na.rm = TRUE)) 

ggplot(dest) + geom_tile(aes(x = as.factor(month), y = dest, fill = ave))

cor(dest$non_arrivals, dest$ave, use = "complete.obs")

dia <- diamonds

#conditional distributions with frequency polygons (better to use cut_number
#than to use cut_width, so as to better reflect the numerical distribution of
#dimonds)

ggplot(dia %>% filter(carat < 3)) + 
  geom_freqpoly(aes(x = carat, color = cut_number(price, 5)), 
                bins = 70) +
  guides(color=guide_legend(reverse=TRUE))

ggplot(dia %>% filter(carat < 3)) + 
  geom_boxplot(aes(x = cut_number(carat, 12), y = price)) 

ggplot(dia %>% filter(carat < 3)) + 
  geom_boxplot(aes(x = cut, y = carat,
               color = cut_number(price, 5))) +
  guides(color=guide_legend(reverse=TRUE))

ggplot(dia %>% filter(carat < 3)) + 
  geom_boxplot(aes(x = clarity, y = carat,
                   color = cut_number(price, 5))) +
  guides(color=guide_legend(reverse=TRUE))

ggplot(dia %>% filter(carat < 3)) + 
  geom_boxplot(aes(x = color, y = carat,
                   color = cut_number(price, 5))) +
  guides(color=guide_legend(reverse=TRUE))

ggplot(dia %>% filter(carat < 3)) + 
  geom_tile(aes(x = cut, y = cut_width(carat, .2),
                   fill = price)) 

library(modelr)

mod <- lm(log(price) ~ log(carat), data = smaller)
dia2 <- smaller %>% add_residuals(mod) %>%
  mutate(resid = exp(resid))
ggplot(dia2) + geom_point(aes(x = carat, y = resid))
ggplot(dia2) + geom_boxplot(aes(x = cut_number(carat, 13),
                                y = resid))
#the following was useful (using cut_width for carat):
ggplot(dia2) + geom_boxplot(aes(x = cut_width(carat, .2),
                                y = resid))
