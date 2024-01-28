library(readr)
library(dplyr)
library(tidyr)
library(tidyverse)
rome_listings <- read.csv("/Users/ishika20/Downloads/rome_listings.csv")

neighbourhood_data <- rome_listings %>%
  filter(neighbourhood_cleansed == "XIII Aurelia")

head(neighbourhood_data)

neighbourhood_data1 <- subset(neighbourhood_data, select = -c(id,listing_url,scrape_id,last_scraped, source, 
                                                              name,picture_url, host_url,
                                                              host_about, host_neighbourhood,neighbourhood,neighbourhood_cleansed,
                                                              bathrooms,maximum_maximum_nights, minimum_minimum_nights, maximum_minimum_nights,
                                                              minimum_maximum_nights,calendar_last_scraped,license, first_review, 
                                                              last_review,calculated_host_listings_count_shared_rooms,
                                                              host_verifications, host_picture_url,host_thumbnail_url,
                                                              neighbourhood_group_cleansed,calendar_updated, description))




colnames(neighbourhood_data1)



#Part 1


#host location

table(neighbourhood_data1$host_location)

# Impute missing values with the mode
mode_host_location <- names(sort(table(neighbourhood_data1$host_location), decreasing = TRUE))[1]
neighbourhood_data1$host_location[is.na(neighbourhood_data1$host_location)] <- mode_host_location
table(neighbourhood_data1$host_location)

#host_response_time

table(neighbourhood_data1$host_response_time)

#207 N/A strings

#Maybe N/A here represents a category where response_time is not applicable. So, we keep it. 

table(neighbourhood_data1$host_response_rate)

#Here, response_rate represents a numerical column but there are 207 'N/A' strings. We would have to change it to a numeric value. And taking average of all response rates seems more reasonable here. 

neighbourhood_data1$host_response_rate[neighbourhood_data1$host_response_rate == 'N/A'] <- NA
neighbourhood_data1$host_response_rate <- as.numeric(gsub("[^0-9.]", "", neighbourhood_data1$host_response_rate))
average_response_rate <- mean(neighbourhood_data1$host_response_rate, na.rm = TRUE)
neighbourhood_data1$host_response_rate[is.na(neighbourhood_data1$host_response_rate) | is.na(average_response_rate)] <- average_response_rate

table(neighbourhood_data1$host_acceptance_rate)

#141 N/A strings

neighbourhood_data1$host_acceptance_rate[neighbourhood_data1$host_acceptance_rate == 'N/A'] <- NA
neighbourhood_data1$host_acceptance_rate <- as.numeric(gsub("[^0-9.]", "", neighbourhood_data1$host_acceptance_rate))
average_acceptance_rate <- mean(neighbourhood_data1$host_acceptance_rate, na.rm = TRUE)
neighbourhood_data1$host_acceptance_rate[is.na(neighbourhood_data1$host_acceptance_rate) | is.na(average_acceptance_rate)] <- average_acceptance_rate


#host_is_superhost
table(neighbourhood_data1$host_is_superhost)
#Assuming that the most frequent category is a reasonable representation for the missing values, we impute this with the mode. 
sum(is.na(neighbourhood_data1$host_is_superhost))

mode_host_is_superhost <- names(sort(table(neighbourhood_data1$host_is_superhost), decreasing = TRUE)[1])
neighbourhood_data$host_is_superhost[is.na(neighbourhood_data1$host_is_superhost)] <- mode_host_is_superhost


table(neighbourhood_data1$host_listings_count)
table(neighbourhood_data1$host_total_listings_count)
table(neighbourhood_data1$host_has_profile_pic)
table(neighbourhood_data1$host_identity_verified)

table(neighbourhood_data1$property_type)
#can be consolidated under the label 'All Others' due to the presence of numerous categories with relatively low counts.

table(neighbourhood_data1$room_type)
#This looks meaningful with only 3 categories- Entire home/apt, Hotel room, and Private room

table(neighbourhood_data1$accommodates)

table(neighbourhood_data1$bathrooms_text)
sum(is.na(neighbourhood_data1$bathrooms_text))

neighbourhood_data1 <- neighbourhood_data1[!is.na(neighbourhood_data1$bathrooms_text), ]


#As the missing data is minimal, removing the rows doesn't significantly impact the representativeness of the dataset

#Then we need to re-group it 

table(neighbourhood_data1$bedrooms)

#Removing this column

table(neighbourhood_data1$beds)
sum(is.na(neighbourhood_data1$beds))

neighbourhood_data1 <- neighbourhood_data1[!is.na(neighbourhood_data1$beds), ]

#'More than 7' could be a category here

neighbourhood_data1$beds_grouped <- ifelse(as.numeric(neighbourhood_data1$beds) > 7, "More than 7", as.character(neighbourhood_data1$beds))

table(neighbourhood_data1$price)

#price is good

table(neighbourhood_data1$minimum_nights)

sum(is.na(neighbourhood_data1$minimum_nights))

#Removing outliers 364 and 365

neighbourhood_data1 <- neighbourhood_data1[!(neighbourhood_data1$minimum_nights %in% c(364, 365)), ]

table(neighbourhood_data1$minimum_nights)


table(neighbourhood_data1$maximum_nights)


# Calculate quartiles and IQR
Q1 <- quantile(neighbourhood_data$minimum_nights, 0.25)
Q3 <- quantile(neighbourhood_data$minimum_nights, 0.75)
IQR <- Q3 - Q1

# Define upper and lower bounds for outliers
lower_bound <- Q1 - 1.5 * IQR
upper_bound <- Q3 + 1.5 * IQR

# Identify potential outliers
outliers <- neighbourhood_data$minimum_nights < lower_bound | neighbourhood_data$minimum_nights > upper_bound

# Print values of potential outliers
print(neighbourhood_data$minimum_nights[outliers])



#46
column_data_type <- class(neighbourhood_data1$reviews_per_month)

summary(neighbourhood_data1$reviews_per_month)

neighbourhood_data1$reviews_per_month[is.na(neighbourhood_data1$reviews_per_month)] <- 1.709


#45

class(neighbourhood_data1$calculated_host_listings_count_private_rooms)

summary(neighbourhood_data1$calculated_host_listings_count_private_rooms)


#44

class(neighbourhood_data1$calculated_host_listings_count_entire_homes)

summary(neighbourhood_data1$calculated_host_listings_count_entire_homes)
# do we need the details of listing types counts? 

#43
class(neighbourhood_data1$calculated_host_listings_count)

summary(neighbourhood_data1$calculated_host_listings_count)

#42
class(neighbourhood_data1$calculated_host_listings_count)

summary(neighbourhood_data1$calculated_host_listings_count)

#41
class(neighbourhood_data1$instant_bookable)

summary(neighbourhood_data1$instant_bookable)

neighbourhood_data1$instant_bookable <- ifelse(neighbourhood_data1$instant_bookable == "t", 1, 0)
#we converted t f to 1 and 0

#40
class(neighbourhood_data1$review_scores_value)
summary(neighbourhood_data1$review_scores_value)

#imputed the missing values with the median value

neighbourhood_data1$review_scores_value[is.na(neighbourhood_data1$review_scores_value)] <- 4.750


#39
class(neighbourhood_data1$review_scores_location)

summary(neighbourhood_data1$review_scores_location)

neighbourhood_data1$review_scores_location[is.na(neighbourhood_data1$review_scores_location)] <- 4.730


#38
class(neighbourhood_data1$review_scores_communication)

summary(neighbourhood_data1$review_scores_communication)

neighbourhood_data1$review_scores_communication[is.na(neighbourhood_data1$review_scores_communication)] <- 4.930


#37
class(neighbourhood_data1$review_scores_checkin)

summary(neighbourhood_data1$review_scores_checkin)

neighbourhood_data1$review_scores_checkin[is.na(neighbourhood_data1$review_scores_checkin)] <- 4.910

#36

class(neighbourhood_data1$review_scores_cleanliness)

summary(neighbourhood_data1$review_scores_cleanliness)

neighbourhood_data1$review_scores_cleanliness[is.na(neighbourhood_data1$review_scores_cleanliness)] <- 4.860

#35

class(neighbourhood_data1$review_scores_accuracy)

summary(neighbourhood_data1$review_scores_accuracy)

neighbourhood_data1$review_scores_accuracy[is.na(neighbourhood_data1$review_scores_accuracy)] <- 4.870

#34
class(neighbourhood_data1$review_scores_rating)

summary(neighbourhood_data1$review_scores_rating)

neighbourhood_data1$review_scores_rating[is.na(neighbourhood_data1$review_scores_rating)] <- 4.820

#33
class(neighbourhood_data1$number_of_reviews_l30d)

summary(neighbourhood_data1$number_of_reviews_l30d)

#32

class(neighbourhood_data1$number_of_reviews_ltm)

summary(neighbourhood_data1$number_of_reviews_ltm)

#31
class(neighbourhood_data1$number_of_reviews)

summary(neighbourhood_data1$number_of_reviews)
#maybe look for outliars? 

#30
class(neighbourhood_data1$availability_365)

summary(neighbourhood_data1$availability_365)

#29
class(neighbourhood_data1$availability_90)

summary(neighbourhood_data1$availability_90)

#28
class(neighbourhood_data1$availability_60)

summary(neighbourhood_data1$availability_60)

#27
class(neighbourhood_data1$availability_30)

summary(neighbourhood_data1$availability_30)

#26
class(neighbourhood_data1$has_availability)

summary(neighbourhood_data1$has_availability)

neighbourhood_data1$has_availability <- ifelse(neighbourhood_data1$has_availability == "t", 1, 0)
#we converted t f to 1 and 0

#dropping the $ sign  
neighbourhood_data1$price <- gsub("\\$", "", neighbourhood_data1$price)

class(neighbourhood_data1$price)

summary(neighbourhood_data1$price)
neighbourhood_data1$price <- as.numeric(trimws(as.character(neighbourhood_data1$price)))

na_rows <- neighbourhood_data1[is.na(neighbourhood_data1$price), ]

#removing the 5 N/A values
cleaned_df <- neighbourhood_data1[!is.na(neighbourhood_data1$price), ]


cleaned_df$bedrooms <- NULL

#Amenities 
#only looking at if the amenities have wifi or not

wifi_rows <- cleaned_df[grepl("Wifi", cleaned_df$amenities), ]

cleaned_df$amenities <- grepl("Wifi", cleaned_df$amenities, ignore.case = TRUE)


# clustering analysis
library(ggplot2)
library(dplyr)
numeric_features <- cleaned_df %>% select("accommodates", "price", "minimum_nights","host_response_rate","host_acceptance_rate",
                                          "availability_365","number_of_reviews_ltm","review_scores_value")

# Standardization
scaled_data <- scale(numeric_features)

# Hierarchical Clustering

# Calculating the distance matrix
distance_matrix <- dist(scaled_data)

# Performing hierarchical clustering
hclust_model <- hclust(distance_matrix, method = "ward.D2")

# Cutting the tree to get clusters
num_clusters <- 3
clusters <- cutree(hclust_model, k = num_clusters)

# Interpreting and Evaluate Clusters
cleaned_df$cluster <- clusters

# Visualizations
plot(hclust_model, main = "Hierarchical Clustering Dendrogram", xlab = "Rental Units", sub = NULL)

#Part-iii
library(ggplot2)
library(dplyr)

# Barplot of Average Guest Review Scores by Cluster
cleaned_df %>%
  group_by(cluster) %>%
  summarise(avg_price = mean(price)) %>%
  ggplot(aes(x = as.factor(cluster), y = avg_price, fill = as.factor(cluster))) +
  geom_bar(stat = "identity") +
  labs(title = "Average price by Cluster", x = "Cluster", y = "Average price")

# Boxplot of Number of Beds by Cluster
ggplot(cleaned_df, aes(x = as.factor(cluster), y = beds, fill = as.factor(cluster))) +
  geom_boxplot() +
  labs(title = "Number of Beds by Cluster", x = "Cluster", y = "Number of Beds") +
  scale_fill_discrete(name = "Cluster")

#Violin Plot of Review Scores

ggplot(cleaned_df, aes(x = as.factor(cluster), y = review_scores_value, fill = as.factor(cluster))) +
  geom_violin(trim = FALSE) +
  geom_boxplot(width = 0.1, fill = "white", color = "black") +
  labs(title = "Violin Plot of Review Scores by Cluster", x = "Cluster", y = "Review Scores") +
  scale_fill_discrete(name = "Cluster") +
  facet_wrap(~ as.factor(cluster), scales = "free_y", ncol = 1) +
  theme_minimal()

