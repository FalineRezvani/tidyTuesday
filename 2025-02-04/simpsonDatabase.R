---
title: "tidyTuesday2-4"
output: html_document
date: "2025-02-03"
developer: Faline Rezvani
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

# The [Simpsons Dataset from Kaggle](https://www.kaggle.com/datasets/prashant111/the-simpsons-dataset) was made available by [Prashant Banerjee](https://www.kaggle.com/prashant111) and curated for tidytuesday by [Nicolas Foss, Ed.D., MS with Iowa HHS](https://github.com/nicolasfoss).

```{r}
library(tidytuesdayR)
library(tidyverse)
library(dplyr)
library(glue)
library(cowplot)
```

```{r}
tuesdata <- tt_load('2025-02-04')
```

```{r}
simpsons_locations <- tuesdata$simpsons_locations
simpsons_script_lines <- tuesdata$simpsons_script_lines
simpsons_characters <- tuesdata$simpsons_characters
simpsons_episodes <- tuesdata$simpsons_episodes
```

```{r}
# Grabbing only script lines that have location recorded
# Reduces 'simpsons_script_lines' from 31793 obs. to 31698
loc_script<-simpsons_script_lines |>
  semi_join(simpsons_locations, by = c("location_id"="id"))
```

```{r}
# Now grabbing only script lines that have character recorded
# Reduces 'loc_script' from 31698 obs. to 28184
char_loc_script<-loc_script |>
  semi_join(simpsons_characters, by = c("character_id"="id"))
```

```{r}
# Merging 'char_loc_script' with 'simpsons_locations'
# Telling R 'location_id' in 'char_loc_script' and
# 'id' in 'simpsons_locations' are to be treated equally
# Resulting in the same number of rows as char_loc_script, but
# with the normalized location name column included
df<-merge(char_loc_script, simpsons_locations, by.x = "location_id", by.y = "id")
```

```{r}
# Using subset() to remove columns
df = subset(df, select = -c(raw_location_text,name))
```

```{r}
# Renaming normalized column in 'simpsons_characters'
colnames(simpsons_characters) <-
  c('id','name','normalized_char_name', 'gender')
```

```{r}
# Merging with 'simpsons_characters'
# Telling R 'character_id' in 'df' and
# 'id' in 'simpsons_characters' are to be treated equally
# Resulting in the same number of rows as df, but
# with the normalized character name column included
df<-merge(df, simpsons_characters, by.x = "character_id", by.y = "id")
```

```{r}
# Using subset() to remove columns
df = subset(df, select = -c(raw_character_text))
```

# Now we have the normalized name for locations and characters included in the script dataset

```{r}
# Writing df as .csv file if needed
write.csv(df,"C:/Users/falin/Desktop/RProjectBeginner_2025/simpsonsScript.csv")
```

```{r}
# Who are the top speakers in the 28,184 recorded lines of script?
top_char <- df |>
  count(name, sort = TRUE) |>
  head(20)
```

```{r}
# Plotting top speakers
top_char_plot <- top_char |>
  mutate(name = fct_reorder(name, n)) |>
  ggplot(aes(n, name)) +
  geom_col(fill="#F7EF40", color="black") +
  labs(x = "Total Lines of Dialogue", y = "Character")
```

```{r}
top_char_plot
```

```{r}
ggsave("simpsonsTopChar.png")
```

# Inspecting episode popularity.
# 'simpsons_episodes' dataset has 148 samples (episodes).

```{r}
# Creating new column containing season, episode, and title
# Ordering new column by season
# Technique courtesy of David Robinson Tidy Tuesday R Screencasts
simpsons_episodes <- simpsons_episodes |>
  mutate(episode_title = glue("{season}.{number_in_season} {title}"),
         episode_title = fct_reorder(episode_title, season + .001 * number_in_season))
```

```{r}
simpsons_episodes |>
  count(season, episode_title)
```

```{r}
# Omitting missing values
simpsons_episodes = na.omit(simpsons_episodes)
```

# Visualizing ratings over time

```{r}
episode_views <- simpsons_episodes |>
  ggplot(aes(as.integer(episode_title), us_viewers_in_millions)) +
  geom_point(aes(color = factor(season))) +
  geom_text(aes(label = title), vjust = 1, hjust = 1,
            check_overlap = TRUE,
            size = 2) +
  theme(axis.text.x = element_blank()) +
  expand_limits(x = 0) +
  labs(x = "Episode", y = "U.S. Views in Millions",
       color = "Season")
```

```{r}
episode_views
```

```{r}
# Controlling image size with ggsave
ggsave(filename = "simpsonsViews.png",
  ggplot(simpsons_episodes, aes(as.integer(episode_title), us_viewers_in_millions)) +
  geom_point(aes(color = factor(season))) +
  geom_text(aes(label = title), vjust = 1, hjust = 1,
            check_overlap = TRUE,
            size = 2) +
  theme(axis.text.x = element_blank()) +
  expand_limits(x = 0) +
  labs(x = "Episode", y = "U.S. Views in Millions",
       color = "Season"),
  width = 10, height = 4, dpi = 300, units = "in", device='png')
```

```{r}
# Grabbing lines of script where views were recorded
# 28,184 samples, 29 features
episode_script <- df |>
  inner_join(simpsons_episodes, by = c("episode_id"="id"))
```

```{r}
# Checking out views and titles of top characters
# Using semi_join to grab only top characters
top_char_episode <- episode_script |>
  count(name, title, us_viewers_in_millions) |>
  semi_join(top_char, by = "name") |>
  mutate(name = fct_reorder(name, n)) |>
  ggplot(aes(name, n)) +
  geom_boxplot() +
  coord_flip() +
  labs(x = "Character", y = "Lines per Episode")
```

```{r}
top_char_episode
```

```{r}
# Using cowplot package to fit aal visualizations on one image
plot_grid(top_char_plot, top_char_episode, episode_views, nrow=3, ncol=1)
```

```{r}
ggsave("simpsonsDatabase.png", width = 12, height = 8)
```
