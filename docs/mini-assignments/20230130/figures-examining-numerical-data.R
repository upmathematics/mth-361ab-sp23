### Scripts for producing png figures for the Examining Numerical Data mini-assignment

library(tidyverse)
library(openintro)
library(kableExtra)
library(janitor)
library(patchwork)

# Unemployment and Poverty
ggplot(county, aes(x = unemployment_rate/100, y = poverty/100)) +
  geom_point(alpha = 0.3, fill = "black", 
             shape = 21, size = 3) +
  geom_smooth(method = "glm", linetype = "dashed", color = "blue", se = FALSE) +
  labs(x = "Unemployment rate",y = "Poverty rate") +
  scale_x_continuous() +
  scale_y_continuous()

ggsave("fig-unemployment-and-poverty.png", width = 6, height = 4)

# Education Level
county %>%
  na.omit(median_edu) %>%
  count(median_edu) %>%
  pivot_wider(names_from = median_edu, values_from = n) %>% 
  adorn_totals(where = c("col")) %>%
  kbl(linesep = "", booktabs = TRUE, caption = "Number of observations per education level") %>%
  kable_styling(bootstrap_options = c("striped", "condensed"), 
                latex_options = c("striped", "hold_position"), full_width = FALSE) %>%
  add_header_above(c("Education level" = 5)) %>%
  column_spec(1:5, width = "5em") %>%
  save_kable(file="tab-education-level-observations.png", zoom=5,density=600)

getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}
county %>%
  na.omit(median_edu) %>%
  group_by(median_edu) %>% 
  summarize(min = min(unemployment_rate),
            q1 = quantile(unemployment_rate, 0.25),
            mode = getmode(unemployment_rate),
            median = median(unemployment_rate),
            mean = mean(unemployment_rate),
            q3 = quantile(unemployment_rate, 0.75),
            max = max(unemployment_rate)) %>%
  kbl(linesep = "", booktabs = TRUE, caption = "A summary statistics table by education level") %>%
  kable_styling(bootstrap_options = c("striped", "condensed"), 
                latex_options = c("striped", "hold_position"), full_width = FALSE) %>%
  add_header_above(c("Education level" = 1, "Summary statistics" = 7)) %>%
  column_spec(1:8, width = "3em") %>%
  save_kable(file="tab-education-level-summary.png",zoom=5,density=600)

p_hist <- county %>%
  na.omit(median_edu) %>%
  ggplot(aes(x = unemployment_rate, fill = median_edu)) + 
  geom_histogram(bins=50, alpha = 0.5) +
  scale_x_continuous() +
  labs(x = "Unemployment rate", y = NULL, fill = "Education level", color = "Education level") + 
  theme(text = element_text(size = 18))

p_box <- county %>%
  na.omit(median_edu)%>%
  ggplot(aes(x = unemployment_rate, y = median_edu, fill=median_edu)) +
  geom_boxplot(color="black") +
  scale_x_continuous() +
  labs(x = "Unemployment rate", y = NULL, fill = "Education level", color = "Education level") + 
  theme(text = element_text(size = 18))

p <- p_hist / p_box + plot_annotation(tag_levels = "A")
ggsave("fig-education-level-distributions.png", width = 8, height = 6)
