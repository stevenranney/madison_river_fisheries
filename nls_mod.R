

df <- data.frame(
  x = seq(1, 100, by = 1)) %>%
  mutate(
    y = (x^-.3), 
    rand = rnorm(n(), mean = 0, sd = 0.05), 
    mod_dat = y+rand)

df %>% head()

mod <- nls(mod_dat ~ x^a, data = df, start = c(a = -.1), trace = TRUE)

df %>%
  ggplot() +
  aes(x = x, y = mod_dat) +
  geom_point() + 
  stat_smooth(
    method = 'nls', 
    formula = y ~ x^a, 
    aes(colour = 'Negative Exponential'), 
    se = FALSE, 
    method.args = list(start=c(a = 0.1), trace = TRUE))

summary(mod)