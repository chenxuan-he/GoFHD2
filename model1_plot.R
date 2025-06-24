library(ggplot2)
library(tidyr)
library(dplyr)

# Create the data frame
data <- read.csv("model1_1.csv")

# Reshape the data for plotting
data_long <- data %>%
  pivot_longer(cols = c(ours, rp, an), names_to = "method", values_to = "value")

plot <- ggplot(data_long, aes(x = factor(n), y = value, group = method, color = method, shape = method, linetype = method)) +
  geom_point(size = 3) +
  geom_line() +
  geom_hline(yintercept = 0.05, linetype = "dotted", color = "red") +
  facet_grid(. ~ alpha, labeller = label_bquote(cols = xi == .(alpha))) +
  theme_bw() +
  scale_y_continuous(limits = c(0, 0.9), breaks = seq(0, 0.9, by = 0.2)) +
  labs(x = NULL, y = NULL) +
  theme(legend.position = "none")

ggsave("plots/model1_1.pdf", plot = plot, units = "cm", width = 16, height = 4)


# Create the data frame
data <- read.csv("model1_2.csv")

# Reshape the data for plotting
data_long <- data %>%
  pivot_longer(cols = c(ours, rp, an), names_to = "method", values_to = "value")

plot <- ggplot(data_long, aes(x = factor(n), y = value, group = method, color = method, shape = method, linetype = method)) +
  geom_point(size = 3) +
  geom_line() +
  geom_hline(yintercept = 0.05, linetype = "dotted", color = "red") +
  facet_grid(. ~ alpha, labeller = label_bquote(cols = xi == .(alpha))) +
  theme_bw() +
  scale_y_continuous(limits = c(0, 0.9), breaks = seq(0, 0.9, by = 0.2)) +
  labs(x = NULL, y = NULL) +
  theme(legend.position = "none")

ggsave("plots/model1_2.pdf", plot = plot, units = "cm", width = 16, height = 4)
