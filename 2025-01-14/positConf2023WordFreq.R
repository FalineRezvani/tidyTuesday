title: "tidyTuesday1-14"
output: html_document
date: "2025-01-17"
developer: "Faline Rezvani"
---
```{r setup}
```

```{r}
library(tidytuesdayR)
```

```{r}
tuesdata <- tidytuesdayR::tt_load('2025-01-14')
```

```{r}
conf2024 <- tuesdata$conf2024
```

```{r}
conf2023 <- tuesdata$conf2023
```

```{r conf2023}
summary(conf2023)
```

```{r conf2024}
summary(conf2024)
```
```{r}
#glimpse(conf2023)
```

```{r}
#glimpse(conf2024)
```

```{r}
mean(conf2023$session_length)
```

```{r}
# Count occurences of each speaker
#count(conf2023, speaker_name)
```

```{r}
# Count occurences of each speaker
#count(conf2024, speaker_name)
```

# Tom Mock spoke twice in 2023, and Shannon Pileggi spooke twice in 2024.

```{r}
# Isolating 20 minute sessions
#conf2023 %>%
#  filter(grepl('20', session_length))
```

```{r}
# Isolating sessions about Quarto
#conf2023 %>%
#  filter(grepl('Quarto', session_title))
```

```{r}
#conf2024 %>%
#  filter(grepl('Quarto', talk_title))
```

# There were 14 talks about Quarto in 2023 and 12 in 2024.

```{r}
# New df isolating speakers from LEGO
#lego <- conf2023 %>% filter(grepl('The LEGO Group', speaker_affiliation))
```

```{r}
#View(lego)
```

```{r}
# New variable 'abstract' will contain session_abstract column
assign("abstract", (conf2023$session_abstract))
```

```{r}
#print(abstract)
```

```{r}
# Text stemming
library(SnowballC)
```

```{r}
# Text mining
library(tm)
```

```{r}
# Loading the data as a corpus
document <- Corpus(VectorSource(abstract))
```

```{r}
# Convert to lowercase
document <- tm_map(document, content_transformer(tolower))
```

```{r}
# Removing numbers
document <- tm_map(document, removeNumbers)
```
```{r}
# Removing punctuation
document <- tm_map(document, removePunctuation)
```

```{r}
# Removing stop words
document <-tm_map(document, removeWords, stopwords('english'))
```
```{r}
document <-tm_map(document, removeWords, c("will", "can", "like", "talk", "using", "use", "new", "data"))
```
# Developer chose to remove the word "data" to improve frequency of less-used words.

```{r}
# Building a Term Document Matrix (table of word frequency)
tdm <- TermDocumentMatrix(document)
```

```{r}
m <- as.matrix(tdm)
```

```{r}
v <- sort(rowSums(m), decreasing = TRUE)
```

```{r}
d <- data.frame(word=names(v), frequency=v)
```

```{r}
head(d, 10)
```

```{r}
#glimpse(d)
```

#[Radar plot code origin]<https://www.codementor.io/@jhwatts2010/counting-words-with-r-ds35hzgmj>


```{r}
df <- head(d, 10)
```

```{r}
View(df)
```

```{r}
positConf2023WordFreqPlot <- ggplot2::ggplot(df, aes(x=word, y=frequency, fill=word)) +
  geom_bar(width = 0.75, stat = "identity", color = "white", linewidth = 1) +
  coord_polar(theta = "x") + xlab("") + ylab("") +
  ggtitle("Word Frequency at Posit Conference 2023") +
  theme(legend.position = "none") + labs(x = NULL, y = NULL) +
  labs(caption = "Source: Posit PBC Customer Marketing")
```

```{r}
positConf2023WordFreqPlot
```

```{r}
ggsave(positConf2023WordFreqPlot, 
       filename = "positConf2023WordFreqPlot.pdf",
       device = "pdf",
       height = 6, width = 5, units = "in")
```

## ALT TXT: This chart depicts frequency of words by comparing areas within a circle, similar to a pie chart.  The terms, "python" and "shiny" are the most frequently occuring words.

## Improvements:
- size
- color
- communicating values
- lemmatization
