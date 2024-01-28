##Visualization

library(ggplot2)

# Create a new column with binned "beds" for some of the viusals later on
neighbourhood_data1$beds_binned <- cut(
  neighbourhood_data1$beds,
  breaks = c(0, 2, 5, 10, Inf),
  labels = c("1-2 beds", "3-5 beds", "6-10 beds", "11+ beds"),
  include.lowest = TRUE
)

#1 Histogram of the number of beds
ggplot(neighbourhood_data1, aes(x = beds)) + 
  geom_histogram(binwidth = 1, fill = "blue", color = "white") +
  labs(title = "Distribution of the Number of Beds in Listings", x = "Number of Beds", y = "Count of Listings") +
  theme_minimal()

#2 Scatter plot of beds vs. price
ggplot(neighbourhood_data1, aes(x = beds_binned, y = price)) + 
  geom_point(alpha = 0.5) +
  scale_y_log10() +  # Assuming price is numeric and needs a log transformation due to skewness
  labs(title = "Number of Beds vs. Price", x = "Number of Beds", y = "Log of Price") +
  theme_light()

#3 Violin plot for number of beds by superhost status
ggplot(neighbourhood_data1, aes(x = host_is_superhost, y = beds, fill = host_is_superhost)) +
  geom_violin(trim = FALSE) +
  labs(title = "Number of Beds by Host Superhost Status", x = "Is Superhost", y = "Number of Beds") +
  scale_fill_brewer(palette = "Pastel1") +
  theme_classic()

#4 Point chart for average rating by number of beds
ggplot(neighbourhood_data1, aes(x = beds, y = review_scores_rating)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "loess") +
  labs(title = "Average Ratings by Number of Beds", x = "Number of Beds", y = "Average Rating") +
  theme_classic()

#5 Calculate the average number of reviews per month for each bed category
avg_reviews <- neighbourhood_data1 %>%
  group_by(beds_binned) %>%
  summarise(avg_reviews_per_month = mean(reviews_per_month, na.rm = TRUE))

# Create a bar chart
ggplot(avg_reviews, aes(x = beds_binned, y = avg_reviews_per_month, fill = beds_binned)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Average Monthly Reviews by Number of Beds", x = "Bed Categories", y = "Average Reviews per Month") +
  scale_fill_brewer(palette = "Paired") +
  theme_minimal() +
  theme(legend.position = "none")





##Worldcloud

install.packages("tm")         # for text mining
install.packages("wordcloud")  # for word cloud generation
install.packages("cld2")       # for language detection
install.packages("stopwords")
library(tm)
library(wordcloud)
library(cld2)
library(stopwords)

neighbourhood_data1$language <- detect_language(neighbourhood_data1$neighborhood_overview)
english_overviews <- neighbourhood_data1[neighbourhood_data1$language == "ENGLISH", "neighborhood_overview"]

# Data Preprocessing
italian_stopwords <- stopwords::stopwords("it")

docs <- Corpus(VectorSource(neighbourhood_data1$neighborhood_overview))
docs <- tm_map(docs, content_transformer(tolower))
docs <- tm_map(docs, removePunctuation)
docs <- tm_map(docs, removeNumbers)
docs <- tm_map(docs, removeWords, stopwords("english"))  # remove common stopwords
docs <- tm_map(docs, stripWhitespace)
docs <- tm_map(docs, removeWords, italian_stopwords)  # remove Italian stopwords


# Term Frequency Analysis
dtm <- TermDocumentMatrix(docs)
matrix <- as.matrix(dtm)
word_freqs <- sort(rowSums(matrix), decreasing = TRUE)
word_freqs_df <- data.frame(word = names(word_freqs), freq = word_freqs)

# Creating Word Cloud
set.seed(699)
wordcloud(words = word_freqs_df$word, freq = word_freqs_df$freq, min.freq = 2,  # Adjust min.freq as needed
          max.words = 150,  # Adjust max.words as needed
          random.order = FALSE, 
          rot.per = 0.25,  # Less rotation
          scale = c(4, 0.5),  # Adjust word size scaling
          colors = brewer.pal(8, "Dark2"))