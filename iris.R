setwd("/Users/takayukitamura/Documents/R_Computing")
library(tidyverse)
library(ggtext)
library(glue)
library(nnet)
library(gt)
library(gtExtras)
library(patchwork)

iris
str(iris)
summary(iris)
plot(iris)

iris <- iris %>% 
  rename_all(tolower) 

iris %>%   
  group_by(species) %>% 
  ggplot(aes(x = sepal.width, y = sepal.length, colour = species)) +
  geom_point() +
  geom_smooth(method = "lm")

iris %>% 
  ggplot(aes(x = sepal.length)) +
  geom_histogram()

iris %>% 
  ggplot(aes(x = sepal.length)) +
  geom_density()

iris %>% 
  group_by(species) %>% 
  mutate(sepal_len_avg = mean(sepal.length)) %>% 
  ggplot(aes(x = sepal.length, fill = species, color = species)) +
  geom_density(alpha = 0.5, show.legend = FALSE) +
  geom_vline(aes(xintercept = sepal_len_avg,
                 colour = species),
             linetype = "dashed",
             show.legend = FALSE) +
  geom_text(
    data = iris %>% 
      group_by(species) %>% 
      summarise(sepal_len_avg = mean(sepal.length), groups = "drop") %>% 
      mutate(label = glue("{species}\nMean: {round(sepal_len_avg, 2)}mm")),
    aes(x = sepal_len_avg, y = c(1.2, 1.0, 0.8),
        label = label,
        fontface = "bold"),
    show.legend = FALSE
    ) +
  labs(title = "Sepal Length by Species of Iris",
       x = "Sepal Length (mm)",
       y = "Density") +
  theme_minimal() +
  theme(
    legend.background = element_blank()
  )

## another version with "text_df"

text_df <- iris %>% 
  group_by(species) %>% 
  summarise(sepal_len_avg = mean(sepal.length), .groups = "drop") %>% 
  mutate(label = glue("{species}\nMean: {round(sepal_len_avg, 2)} mm"),
         y = c(1.2, 1.0, 0.8))

p1 <- iris %>% 
  group_by(species) %>% 
  mutate(sepal_len_avg = mean(sepal.length)) %>% 
  ggplot(aes(x = sepal.length, fill = species, colour = species)) +
  geom_density(alpha = 0.5, show.legend = F) +
  geom_vline(aes(xintercept = sepal_len_avg), linetype = "dashed", linewidth = 0.25, show.legend = F) +
  geom_text(data = text_df,
            aes(x = sepal_len_avg,
                y = y,
                label = label,
                fontface = "bold"),
            show.legend = F)  +
  labs(
    title = "Sepal Length by Species of Iris",
    x = "Sepal Length (mm)",
    y = "Density"
  ) +
  theme_minimal()
  
# t.test
iris %>% 
  filter(species %in% c("setosa", "versicolor")) %>% 
  t.test(sepal.length ~ species, data = .)
    
iris %>% 
  filter(species %in% c("setosa", "virginica")) %>% 
  t.test(sepal.length ~ species, data = .)

iris %>% 
  filter(species %in% c("virginica", "versicolor")) %>% 
  t.test(sepal.length ~ species, data = .)

# Filter to two species
# Choose any 2 out of the 3
a <- "virginica"
b <- "versicolor"

# Filter to 2 species and FORCE level order: a then b
iris_bin <- iris %>%
  filter(species %in% c(a, b)) %>%
  mutate(species = factor(species, levels = c(a, b)))

# Fit logistic regression: P(species == b | sepal.length)
model <- glm(species ~ sepal.length, data = iris_bin, family = binomial)

# Predict for a grid of sepal length values (use data range automatically)
new_data <- tibble(
  sepal.length = seq(min(iris_bin$sepal.length), max(iris_bin$sepal.length), by = 0.05)
) %>%
  mutate(
    prob_b = predict(model, newdata = ., type = "response"),
    prob_a = 1 - prob_b
  )

# Visualize probability curve (choose which one to show)
ggplot(new_data, aes(sepal.length, prob_b)) +
  geom_line(linewidth = 1) +
  scale_y_continuous(limits = c(0, 1)) +
  labs(
    x = "Sepal length (cm)",
    y = glue("Probability of {b}"),
    title = glue("Binary logistic model: {a} vs {b}")
  ) +
  theme_minimal()


# Predict for new values
new_data <- tibble(sepal.length = seq(4, 7, by = 0.5))

new_data %>%
  mutate(probability_b = predict(model, newdata = new_data, type = "response"))


# t.test - 3 Species

model_multi <- multinom(species ~ sepal.length, data = iris)

# predict probabilities
predict(model_multi, type = "probs")

# fit multinomial model
m3 <- multinom(species ~ sepal.length, data = iris, trace = FALSE)

# create grid of sepal.length and predict probabilities
grid <- tibble(
  sepal.length = seq(min(iris$sepal.length), max(iris$sepal.length), by = 0.01)
)

probs <- predict(m3, newdata = grid, type = "probs")

probs_df <- bind_cols(grid, as_tibble(probs))

# prot the 3 probability curves
probs_long <- probs_df %>% 
  pivot_longer(-sepal.length, names_to = "Species", values_to = "prob")

ggplot(probs_long,
       aes(x = sepal.length, y = prob, colour = Species)) +
  geom_line(linewidth = 1) +
  scale_y_continuous(limits = c(0, 1)) +
  labs(
    title = "Predicted Species Probability from Sepal Length",
    x = "Sepal Length (cm)",
    y = "Predicted probability"
  ) +
  theme_minimal()

# add rug marks showing where data points are
p2 <- ggplot(probs_long, aes(sepal.length, prob, colour = Species)) +
  geom_line(linewidth = 1) +
  geom_rug(
    data = iris,
    aes(x = sepal.length, colour = species),
    sides = "b",
    alpha = 0.4,
    inherit.aes = FALSE
  ) +
  scale_y_continuous(limits = c(0, 1)) +
  labs(
    title = "Predicted Species Probability from Sepal Length",
    x = "Sepal Length (cm)",
    y = "Predicted probability"
  ) +
  theme_minimal()

predict(m3, newdata = tibble(sepal.length = 6.8), type = "probs") 

p1 | p2
