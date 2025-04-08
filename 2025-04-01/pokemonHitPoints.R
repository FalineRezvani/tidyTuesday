---
title: "tidyTuesday04-08-2025"
output: html_document
date: "2025-04-08"
developer: Faline Rezvani
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

## This Pokémon dataset was curated by [Frank Hull](https://github.com/frankiethull).

```{r}
# Loading libraries
library(tidytuesdayR)
library(tidyverse)
library(dplyr)
library(glue)
library(cowplot)
```

```{r}
theme_set(theme_light())
```

```{r}
# Loading data
tuesdata <- tidytuesdayR::tt_load('2025-04-01')
```

```{r}
pokemon_df <- tuesdata$pokemon_df
```

```{r}
# Pokemon with hit points greater than 105
high_hp <- filter(pokemon_df, hp >= 105)
```

```{r}
# Making new column with groups of hit points with base R cut() function
high_hp$hp_group <- cut(high_hp$hp,
                        breaks = c(105, 144, 184, 224, 256), # Defining breaks
                        labels = c("105-144", "145-184", "185-224", "225+"), # Assigning labels
                        right = FALSE) # Whether or not to include endpoint of each interval
```

```{r}
# Showing different groups of hit points with color
# By adding aesthetic
ggsave(filename = "pokemonHitPoints.png",
       ggplot(high_hp, aes(x = height,
                y = weight,
                color = hp_group)) +
       geom_point(size = 3) +
       labs(title = 'Pokemon Hit Points by Height & Weight',
            x = 'Height',
            y = 'Weight',
            color = 'Hit Points',
            caption = "Source: Curated by Frank Hull for the Tidy Tuesday project.") +
        theme(axis.text.x=element_text(size = 14),
              axis.text.y=element_text(size = 14),
              plot.title = element_text(size = 20),
              legend.text = element_text(size = 10),
              legend.title = element_text(size = 12)),
        width = 12, height = 8, dpi = 300, units = "in", device='png')
```




