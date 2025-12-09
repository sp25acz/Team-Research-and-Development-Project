# 3.1
# install / load required packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load(readr, dplyr, ggplot2, janitor, broom, car, lmtest)

# Reading dataset (use appropriate path for your machine)
# Windows path:
data_path_win <- "N:/Dataset3final.csv"
# Linux/Container fallback:
data_path_linux <- "/mnt/data/Dataset3final.csv"

if (file.exists(data_path_win)) {
  df <- read_csv(data_path_win)
} else if (file.exists(data_path_linux)) {
  df <- read_csv(data_path_linux)
} else {
  stop("Dataset file not found at either N:/Dataset3final.csv or /mnt/data/Dataset3final.csv")
}

# Cleaning column names
df <- df %>% clean_names()

# Quick transform: ensure correct types
df <- df %>%
  mutate(eu_membership = as.factor(european_union_membership),
         women_ent_index = as.numeric(women_entrepreneurship_index),
         ent_index = as.numeric(entrepreneurship_index),
         flfpr = as.numeric(female_labor_force_participation_rate))

# Descriptive summary
summary(df %>% select(country, women_ent_index, ent_index, flfpr, inflation_rate))

# Main scatterplot + regression line colored by EU membership
p1 <- ggplot(df, aes(x = flfpr, y = women_ent_index, color = eu_membership)) +
  geom_point(size = 2, alpha = 0.8) +
  geom_smooth(method = "lm", se = TRUE, aes(fill = eu_membership), show.legend = FALSE) +
  labs(title = "Women Entrepreneurship Index vs Female Labour Force Participation Rate",
       x = "Female Labour Force Participation Rate (%)",
       y = "Women Entrepreneurship Index (score)",
       color = "EU membership") +
  theme_minimal()
p1
# Save plot
ggsave("women_ent_vs_flfpr.png", p1, width = 7, height = 5, dpi = 300)

# Histogram of Women Entrepreneurship Index
p2 <- ggplot(df, aes(x = women_ent_index)) +
  geom_histogram(bins = 12) +
  labs(title = "Distribution of Women Entrepreneurship Index",
       x = "Women Entrepreneurship Index (score)",
       y = "Count") +
  theme_minimal()
p2
ggsave("hist_women_ent_index.png", p2, width = 6, height = 4, dpi = 300)

# Boxplot of Women Entrepreneurship Index by EU membership
p3 <- ggplot(df, aes(x = eu_membership, y = women_ent_index)) +
  geom_boxplot() +
  labs(title = "Women Entrepreneurship Index by EU membership",
       x = "EU membership",
       y = "Women Entrepreneurship Index") +
  theme_minimal()
p3
ggsave("box_women_ent_by_eu.png", p3, width = 6, height = 4, dpi = 300)


# 4.1
# Removing rows with missing key variables
model_df <- df %>% filter(!is.na(women_ent_index), !is.na(flfpr), !is.na(ent_index))

# 1. Pearson correlation
cor_test <- cor.test(model_df$flfpr, model_df$women_ent_index, method = "pearson")
cor_test

# 2. Linear regression controlling for entrepreneurship index and EU membership
model <- lm(women_ent_index ~ flfpr + ent_index + eu_membership, data = model_df)
summary(model)

# 3. Diagnostic tests
# Normality of residuals
shapiro_res <- shapiro.test(residuals(model))
shapiro_res

# Heteroskedasticity test
bptest_res <- bptest(model)  # from lmtest
bptest_res

# If heteroskedastic -> robust SEs (sandwich)
if (!require("sandwich")) install.packages("sandwich")
if (!require("lmtest")) install.packages("lmtest")
coeftest_robust <- lmtest::coeftest(model, vcov = sandwich::vcovHC(model, type = "HC1"))
coeftest_robust

