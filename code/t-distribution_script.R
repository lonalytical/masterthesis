library(ggplot2)

df <- 8
t_value <- 2.1
alpha <- 0.05

x <- seq(-4, 4, length.out = 1000)
y <- dt(x, df = df)

crit <- qt(1 - alpha/2, df = df)

df_curve <- data.frame(x = x, y = y)

# only the p-value region
df_p <- subset(df_curve, x >= t_value)

ggplot(df_curve, aes(x = x, y = y)) +
  geom_line(linewidth = 0.6) +
  
  geom_area(data = df_p, aes(x = x, y = y), alpha = 0.3) +
  
  geom_vline(xintercept = t_value, linetype = "dashed") +
  
  geom_segment(aes(x = -5, xend = 5, y = -0.01, yend = -0.01),
               linewidth = 0.8) +
  
  labs(x = NULL, y = NULL) +
  coord_cartesian(ylim = c(-0.03, max(y)*1.05)) +
  theme_void()
