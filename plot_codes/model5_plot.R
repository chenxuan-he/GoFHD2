library(ggplot2)
library(tidyr)
library(dplyr)
# change between model5_1 and model5_2
pat <- "model5_1"
pat <- "model5_2"
# Create the data frame
data <- read.csv(paste0(pat,".csv"))

# Reshape the data for plotting
data_long <- data %>%
  pivot_longer(cols = c(ours, rp, an), names_to = "method", values_to = "value")

plot <- ggplot(data_long, aes(x = factor(alpha), y = value, group = method, color = method, shape = method, linetype = method)) +
  geom_point(size = 3) +
  geom_line() +
  geom_hline(yintercept = 0.05, linetype = "dotted", color = "red") +
  facet_grid(. ~ n, labeller = label_bquote(cols = n == .(n))) +
  theme_bw() +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.2)) +
  labs(x = NULL, y = NULL) +
  theme(legend.position = "none")

ggsave(paste0("plots/",pat,".pdf"), plot = plot, units = "cm", width = 16, height = 4)

