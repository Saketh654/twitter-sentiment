# Load required libraries
library(dplyr)
library(stringr)
library(tidytext)
library(readr)
library(lubridate)

# Function to preprocess tweets dataset with sampling
preprocess_tweets <- function(input_file, output_file, sample_size = 50000, random_seed = 123) {
  # Read the dataset
  cat("Reading data from", input_file, "...\n")
  tweets_df <- read_csv(input_file)
  
  # Get total number of rows
  total_rows <- nrow(tweets_df)
  cat("Total number of data points:", total_rows, "\n")
  
  # Check if sampling is needed
  if (sample_size < total_rows) {
    cat("Performing random sampling to select", sample_size, "data points...\n")
    
    # Set seed for reproducibility 
    set.seed(random_seed)
    
    # Sample randomly
    sampled_indices <- sample(1:total_rows, size = sample_size, replace = FALSE)
    tweets_df <- tweets_df[sampled_indices, ]
    
    cat("Sampling complete. Working with", nrow(tweets_df), "data points.\n")
  } else {
    cat("Sample size greater than or equal to total data points. Using all data.\n")
  }
  
  # Clean tweets
  cat("Cleaning tweets...\n")
  
  # Function to clean individual tweets
  clean_tweet <- function(tweet) {
    if (is.na(tweet)) {
      return("")
    }
    
    tweet <- iconv(tweet, from = "latin1", to = "UTF-8", sub = " ")
    tweet <- tolower(tweet)  # Convert to lowercase
    tweet <- str_replace_all(tweet, "http\\S+|www\\S+|https\\S+", "")  # Remove URLs
    tweet <- str_replace_all(tweet, "@\\w+", "")  # Remove mentions
    tweet <- str_replace_all(tweet, "#", "")  # Remove hashtag symbol (but keep text)
    tweet <- str_replace_all(tweet, "[[:punct:]]", "")  # Remove punctuation
    tweet <- str_squish(tweet)  # Remove extra whitespace
    
    # Tokenize words, remove stop words, then rejoin
    tweet_words <- unlist(str_split(tweet, " "))
    tweet_words <- tweet_words[!tweet_words %in% stop_words$word]
    tweet <- paste(tweet_words, collapse = " ")
    
    return(tweet)
  }
  
  # Apply cleaning to tweet text
  tweets_df$cleaned_tweet <- sapply(tweets_df$Tweet, clean_tweet)
  
  # Select only the columns we need (including formatted date if available)
  if ("Formatted_Date" %in% colnames(tweets_df)) {
    output_df <- tweets_df %>%
      select(User, Tweet, cleaned_tweet, Formatted_Date)
  } else {
    output_df <- tweets_df %>%
      select(User, Tweet, cleaned_tweet)
  }
  
  # Write to CSV
  cat("Writing processed data to", output_file, "...\n")
  write_csv(output_df, output_file)
  
  # Also save a separate file with just the original sample IDs for reference
  if (sample_size < total_rows) {
    sample_ids_df <- data.frame(row_index = sampled_indices)
    write_csv(sample_ids_df, paste0("sample_indices_", sample_size, ".csv"))
  }
  
  cat("Preprocessing complete!\n")
  
  # Return the processed dataframe
  return(output_df)
}


input_file <- "final_dataset.csv"  # Change to your input file path
output_file <- "preprocessed_tweets.csv"  # Change to your desired output file path
sample_size <- 100000  # Default sample size for sentiment analysis (adjust as needed)

# Run the preprocessing function with sampling
processed_data <- preprocess_tweets(input_file, output_file, sample_size)

# Display sample of processed data
cat("\nSample of processed data:\n")
head(processed_data)