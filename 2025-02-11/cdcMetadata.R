---
title: "tidyTuesday02-11"
output: html_document
date: "2025-02-19"
developer: Faline Rezvani
---

## These CDC metadata datasets were curated by [Jon Harmon](https://github.com/jonthegeek) from [CDC datasets backed up on archive.org](https://archive.org/download/20250128-cdc-datasets).


```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

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
tuesdata <- tidytuesdayR::tt_load('2025-02-11')
```

```{r}
tuesdata
```

```{r}
# Assigning data to df
cdc_datasets <- tuesdata$cdc_datasets
fpi_codes <- tuesdata$fpi_codes
omb_codes <- tuesdata$omb_codes
```

```{r}
# Counting occurences of each tag
count(cdc_datasets, tags)
```

```{r}
# Counting occurrences of each 
agencies <- count(omb_codes, agency_name, sort = TRUE) |>
  head(20)
```

# 20 federal agencies

```{r}
# Counting occurrences of each tag
count(omb_codes, bureau_name, sort = TRUE)
```

# 359 bureaus within those agencies

# Inspecting bureaus

```{r}
bureau_cdc <- cdc_datasets |>
  filter(bureau_code == "009:20")
```

# 953 CDC datasets

```{r}
bureau_hrsa <- cdc_datasets |>
  filter(bureau_code == "009:15")
```

# 6 HRSA datasets

```{r}
bureau_hhs <- cdc_datasets |>
  filter(bureau_code == "009:00")
```

# 284 Dept. Health & Human Services datasets

```{r}
bureau_cmms <- cdc_datasets |>
  filter(bureau_code == "009:38")
```

# 1 CMMS dataset

```{r}
# Counting occurrences of each program
count(fpi_codes, program_name, sort = TRUE)
```

# 1496 programs within those bureaus

```{r}
# Create mapping from bureau_code in cdc_datasets to bureau name
bureau_names <- c("009:00" = "Dept. Health & Human Services", "009:000" = "Dept. Health & Human Services",
                  "009:032" = "Animal & Plant Health Inspection Service", "009:15" = "Health Resources & Services Administration",
                  "009:20" = "Centers for Disease Control & Prevention", "009:38" = "Centers for Medicare & Medicaid Services")
```

## Inspecting federal programs within bureaus

```{r}
# Merging datasets to include program name
df <- merge(cdc_datasets, fpi_codes, by.x = 'program_code', by.y = 'program_code_pod_format')
```

```{r}
top_programs <- df |>
  count(bureau_code, program_name, sort = TRUE) |>
  mutate(program_name = fct_reorder(program_name, n, sum)) |>
  head(5)
```

```{r}
# Controlling image size with ggsave
ggsave(filename = "cdcDataPrograms.png",
  ggplot(top_programs, aes(n, program_name, fill = bureau_code)) +
  geom_col() +
  coord_flip() +
  scale_fill_discrete(labels = bureau_names) +
  labs(title = 'Datasets by Program within Federal Bureau',
       x = '# of Datasets',
       y = 'Program',
       fill = 'Bureau',
       caption = "Source: Curated by Jon Harmon via archive.org for the Tidy Tuesday project.") +
  theme(axis.text.x=element_text(angle=45, hjust=1)),
  width = 12, height = 4, dpi = 300, units = "in", device='png')
```

## Inspecting categories within programs

```{r}
# Counting occurrences of top categories
top_categories <- count(cdc_datasets, category, sort = TRUE) |>
  head(5) |>
  mutate(category = fct_reorder(category, n)) |>
  ggplot(aes(n, category)) +
  geom_col() +
  labs(title = 'Most Common Categories in Archived Datatsets',
       caption = "Source: Curated by Jon Harmon via archive.org for the Tidy Tuesday project.") +
  theme_set(theme_minimal(base_size = 8))
```

# 731 datasets (58% of all the datasets) are related to the top five categories, 'top_categories'.

```{r}
#top_categories
```

```{r}
top_categories <- df |>
  count(program_name, category, sort = TRUE) |>
  mutate(program_name = fct_lump(program_name, 10),
         category = fct_lump(category, 5)) |>
  filter(category != 'Other') |>
  mutate(category = fct_reorder(category, n, sum))
```
  
```{r}
# Controlling image size with ggsave
ggsave(filename = "cdcDataCategories.png",
  ggplot(top_categories, aes(n, category, fill = program_name)) +
  geom_col() +
  labs(title = 'Datasets by Category within Federal Program',
       x = '# of Datasets',
       y = 'Category',
       fill = 'Program',
       caption = "Source: CDC metadata datasets curated by Jon Harmon via archive.org for the TidyTuesday Data Science project.") +
  theme_set(theme_minimal(base_size = 8)),
  width = 10, height = 4, dpi = 300, units = "in", device='png')
```

## Inspecting tags related to category, 'Public Health Surveillance'.

```{r}
# Isolating category, Public Health Surveillance
public_health <- df |>
  filter(category == 'Public Health Surveillance')
```

```{r}
# New variable 'corpus' will contain tags column
assign('corpus', (public_health$tags))
```

```{r}
library(SnowballC) # Text stemming
library(tm) # Text mining
library(fmsb) # Radar plot
```

```{r}
# Loading the data as a corpus
corpus <- Corpus(VectorSource(corpus))
```

```{r}
# Text pre-processing
# Convert to lowercase
corpus <- tm_map(corpus, content_transformer(tolower))

# Removing numbers
corpus <- tm_map(corpus, removeNumbers)

# Removing punctuation
corpus <- tm_map(corpus, removePunctuation)

# Removing stop words
corpus <- tm_map(corpus, removeWords, stopwords('english'))
```

```{r}
# Removing similar words and acronym.
corpus <- tm_map(corpus, removeWords, c('covid', 'hospitalizations', 'ncirdcorvd'))
```

```{r}
# Building a Term Document Matrix (table of word frequency)
tdm <- TermDocumentMatrix(corpus)
```

```{r}
m <- as.matrix(tdm)

v <- sort(rowSums(m), decreasing = TRUE)

d <- data.frame(word=names(v), frequency=v)
```

# Visualizing top ten tags associated with Public Health Surveillance

```{r}
public_tags <- head(d, 10)
```

```{r}
theme_set(theme_light())
```

```{r}
# Radar plot
public_tags_viz <- ggplot(public_tags, aes(x=word, y=frequency, fill = word)) +
  geom_bar(width = 0.75, stat = "identity", color = "white", linewidth = 1) +
  coord_polar(theta = "x") + xlab("") + ylab("") +
  ggtitle("Tag Frequency of Public Health Surveillance Category") +
  theme(legend.position = "none") + labs(x = NULL, y = NULL) +
  labs(caption = "Source: Curated by Jon Harmon via archive.org for the Tidy Tuesday project.")
```

```{r}
#public_tags_viz
```

```{r}
# Creating dataframe with specifications needed for radarchart() spider plot
data <- as.data.frame(matrix(c(34, 31, 22, 20, 20, 15, 15, 13, 12, 12), nrow=1))
 
colnames(data) <- c("respiratory" , "coronavirus" , "virus" ,
                    "beds" , "hospital", "influenza" ,
                    "rsv", "rates", "icu", "syncytial" )
  
data <- rbind(rep(39,10) , rep(0,10) , data)

radarchart(data, axistype = 1, title = 'Tag Frequency within Public Health Surveillance Datasets',
           cglcol="grey", axislabcol="grey", vlcex = 1, caxislabels = seq(0,40,10))
```

# What programs are associated with these top ten Public Health Surveillance tags?

```{r}
program_top_tags <- df |>
  filter(grepl('virus|hospital|icu|influenza|respiratory|rsv|coronavirus|syncytial|beds|rates', tags))
```

```{r}
program_top_tags <- subset(program_top_tags, program_name != 'Program Management')
```

```{r}
program_top_tags <- count(program_top_tags, program_name, sort = TRUE)
```

```{r}
# Create mapping as aliases to program_names
program_alias <- c("CDC-Wide Activities and Program Support" = "CDC Activities & Support",
                  "Emerging and Zoonotic Infectious Diseases" = "Emerging & Zoonotic Infectious Diseases",
                  "Environmental Health" = "Environmental Health",
                  "Immunization and Respiratory Diseases" = "Immunization & Respiratory Diseases",
                  "Public Health Preparedness and Response" = "Public Health Preparedness & Response")
```

```{r}
# Controlling image size with ggsave
ggsave(filename = "cdcProgramTopTags.png",
  ggplot(program_top_tags, aes(n, program_name, color = n)) +
  geom_point(size = 6) +
#  geom_line(position = 'identity', color = 'black') +
  coord_flip() +
  scale_y_discrete(labels = program_alias) +
  labs(title = 'Programs Attaching Top-10 Tags to Data',
       x = '',
       y = '',
       color = '# of Datasets',) +
  theme(axis.text.x=element_text(size = 14, angle=45, hjust=1),
        axis.text.y=element_blank(),
        plot.title = element_text(size = 20),
        legend.text = element_text(size = 10),
        legend.title = element_text(size = 12)) +
  expand_limits(y = 0),
  width = 10, height = 6, dpi = 300, units = "in", device='png')
```

