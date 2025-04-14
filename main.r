# STREAMING PLATFORM CONTENT ANALYSIS PROJECT
# Objective: Analyze streaming data by genre and popularity

# Step 1: Load required packages
library(dplyr)
library(ggplot2)

# Step 2: Create sample dataset (no external data needed)
set.seed(123) # Ensures reproducible random data

streaming_data <- data.frame(
  Title = paste("Title", 1:50),
  Genre = sample(c("Drama", "Comedy", "Action", "Sci-Fi", "Romance"), 50, replace = TRUE),
  Views = sample(1000:100000, 50, replace = TRUE),
  Rating = round(runif(50, 4.0, 9.8), 1),
  Platform = sample(c("Netflix", "Prime", "Disney+", "Hulu"), 50, replace = TRUE)
)

# Step 3: Basic statistics
genre_stats <- streaming_data %>%
  group_by(Genre) %>%
  summarise(
    Avg_Views = mean(Views),
    Total_Views = sum(Views),
    Most_Popular_Title = Title[which.max(Views)],
    Top_Platform = names(which.max(table(Platform)))
  )

# Print summary
print("Streaming Summary by Genre:")
print(genre_stats)

# Step 4: Visualizations

# Plot 1: Total Views by Genre (Bar Plot)
ggplot(genre_stats, aes(x = Genre, y = Total_Views, fill = Genre)) +
  geom_col() +
  labs(title = "Total Views by Genre", y = "Total Views") +
  theme_minimal()

# Plot 2: View Distribution (Boxplot)
ggplot(streaming_data, aes(x = Genre, y = Views, fill = Genre)) +
  geom_boxplot() +
  labs(title = "View Distribution by Genre") +
  theme_minimal()

# Plot 3: Top 10 Most Viewed Titles (Horizontal Bar Plot)
top_titles <- streaming_data %>%
  arrange(desc(Views)) %>%
  head(10)

ggplot(top_titles, aes(x = reorder(Title, Views), y = Views)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(title = "Top 10 Most Viewed Titles", x = "Title") +
  theme_minimal()

# Optional Step: Identify Hidden Gems
# Titles with high ratings (> 8.5) but relatively low views (< 30000)
hidden_gems <- streaming_data %>%
  filter(Views < 30000 & Rating > 8.5)

print("Hidden Gems (Highly Rated but Low Views):")
print(hidden_gems)

