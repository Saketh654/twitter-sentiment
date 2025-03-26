library(shiny)
library(shinydashboard)
library(tidytext)
library(dplyr)
library(stringr)
library(ggplot2)
library(wordcloud)
library(DT)
library(RColorBrewer)
library(plotly)
library(rmarkdown)

options(shiny.maxRequestSize = 100 * 1024^2)

# Make sure dashboardHeader is properly defined
ui <- dashboardPage(
  dashboardHeader(title = "Twitter Sentiment Analysis"),
  dashboardSidebar(
    fileInput("file", "Upload Twitter Dataset (CSV)",
              accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")),
    actionButton("analyze", "Start Sentiment Analysis", 
                 icon = icon("play"), 
                 class = "btn-primary", 
                 style = "color: #fff; background-color: #337ab7; border-color: #2e6da4; width: 85%;"),
    br(), br(),
    selectInput("sentiment_dict", "Sentiment Dictionary:", 
                choices = c("AFINN", "Bing", "NRC"), 
                selected = "AFINN"),
    sliderInput("top_n_words", "Top N Words:", min = 5, max = 50, value = 15),
    checkboxInput("remove_stop_words", "Remove Stop Words", value = TRUE),
    br(),
    # Add download buttons with proper formatting
    downloadButton("downloadReport", "Download Report", 
                   style = "color: #fff; background-color: #5cb85c; border-color: #4cae4c; width: 85%; margin-left:17px;"),
    br(), br(),
    downloadButton("downloadCSV", "Download Analysis CSV",
                   style = "color: #fff; background-color: #f0ad4e; border-color: #eea236; width: 85%; margin-left:17px;")
  ),
  dashboardBody(
    fluidRow(
      infoBoxOutput("totalTweets", width = 4),
      infoBoxOutput("positiveTweets", width = 4),
      infoBoxOutput("negativeTweets", width = 4)
    ),
    fluidRow(
      box(title = "Sentiment Overview", plotlyOutput("sentimentPlot"), width = 6),
      box(title = "Top Words by Sentiment", plotOutput("topWords"), width = 6)
    ),
    fluidRow(
      box(title = "Word Cloud", plotOutput("wordcloud"), width = 6),
      box(title = "Sentiment Timeline", plotlyOutput("timeline"), width = 6)
    ),
    fluidRow(
      box(title = "Sample Tweets by Sentiment", 
          tabsetPanel(
            tabPanel("Positive", DTOutput("positiveSamples")),
            tabPanel("Negative", DTOutput("negativeSamples")),
            tabPanel("Neutral", DTOutput("neutralSamples"))
          ), 
          width = 12)
    )
  )
)

server <- function(input, output, session) {
  
  values <- reactiveValues(
    data = NULL,
    sentiment = NULL,
    analyzed = FALSE,
    user_col = NULL,
    tweet_col = NULL
  )
  
  observeEvent(input$file, {
    req(input$file)
    values$data <- read.csv(input$file$datapath, stringsAsFactors = FALSE)
    
    all_cols <- colnames(values$data)
    
    user_candidates <- c("User", "user", "username", "user_name", "screen_name")
    values$user_col <- intersect(user_candidates, all_cols)[1]
    if(is.na(values$user_col)) {
      values$user_col <- "User"
      values$data$User <- paste0("User_", 1:nrow(values$data))
    }
    
    if(!("cleaned_tweet" %in% all_cols)) {
      tweet_candidates <- c("Tweets", "Tweet", "tweet", "text", "content", "message")
      tweet_col <- intersect(tweet_candidates, all_cols)[1]
      if(!is.na(tweet_col)) {
        values$data$cleaned_tweet <- values$data[[tweet_col]]
      } else {
        stop("No suitable tweet column found. Please ensure your data contains a column with tweet content.")
      }
    }
    
    if(!"date" %in% all_cols) {
      date_candidates <- c("created_at", "date", "timestamp", "time")
      date_col <- intersect(date_candidates, all_cols)[1]
      if(!is.na(date_col)) {
        
        tryCatch({
          values$data$date <- as.Date(values$data[[date_col]])
        }, error = function(e) {
          
          values$data$date <- Sys.Date() - sample(1:30, nrow(values$data), replace = TRUE)
        })
      } else {
        
        values$data$date <- Sys.Date() - sample(1:30, nrow(values$data), replace = TRUE)
      }
    }
    
    values$analyzed <- FALSE
  })
  
  observeEvent(input$analyze, {
    req(values$data)
    withProgress(message = 'Analyzing sentiments...', value = 0, {
      
      incProgress(0.1, detail = "Tokenizing tweets")
      
      if(!"tweet_id" %in% colnames(values$data)) {
        values$data$tweet_id <- 1:nrow(values$data)
      }
      
      tryCatch({
        tweet_words <- values$data %>%
          mutate(row_id = row_number()) %>%
          select(
            user_col = !!values$user_col,
            tweet_id = tweet_id,
            text = cleaned_tweet,
            date = date,
            row_id = row_id
          ) %>%
          unnest_tokens(word, text)
      }, error = function(e) {
        
        tweet_words <- data.frame(
          row_id = rep(1:nrow(values$data), times = sapply(strsplit(values$data$cleaned_tweet, "\\s+"), length)),
          tweet_id = rep(values$data$tweet_id, times = sapply(strsplit(values$data$cleaned_tweet, "\\s+"), length)),
          word = unlist(strsplit(values$data$cleaned_tweet, "\\s+")),
          date = rep(values$data$date, times = sapply(strsplit(values$data$cleaned_tweet, "\\s+"), length))
        )
      })
      
      if(input$remove_stop_words) {
        incProgress(0.2, detail = "Removing stop words")
        data("stop_words")
        tweet_words <- tweet_words %>% 
          anti_join(stop_words, by = "word")
      }
      
      incProgress(0.3, detail = "Calculating sentiment scores")
      if(input$sentiment_dict == "AFINN") {
        sentiment_scores <- tweet_words %>%
          inner_join(get_sentiments("afinn"), by = "word") %>%
          group_by(tweet_id) %>%
          summarize(sentiment_score = sum(value), .groups = "drop") %>%
          mutate(sentiment = case_when(
            sentiment_score > 0 ~ "positive",
            sentiment_score < 0 ~ "negative",
            TRUE ~ "neutral"
          ))
      } else if(input$sentiment_dict == "Bing") {
        sentiment_scores <- tweet_words %>%
          inner_join(get_sentiments("bing"), by = "word") %>%
          count(tweet_id, sentiment) %>%
          tidyr::pivot_wider(names_from = sentiment, values_from = n, values_fill = 0) %>%
          mutate(
            positive = if("positive" %in% names(.)) positive else 0,
            negative = if("negative" %in% names(.)) negative else 0,
            sentiment_score = positive - negative,
            sentiment = case_when(
              sentiment_score > 0 ~ "positive",
              sentiment_score < 0 ~ "negative",
              TRUE ~ "neutral"
            )
          )
      } else { 
        sentiment_scores <- tweet_words %>%
          inner_join(get_sentiments("nrc") %>% 
                       filter(sentiment %in% c("positive", "negative")), by = "word") %>%
          count(tweet_id, sentiment) %>%
          tidyr::pivot_wider(names_from = sentiment, values_from = n, values_fill = 0) %>%
          mutate(
            positive = if("positive" %in% names(.)) positive else 0,
            negative = if("negative" %in% names(.)) negative else 0,
            sentiment_score = positive - negative,
            sentiment = case_when(
              sentiment_score > 0 ~ "positive",
              sentiment_score < 0 ~ "negative",
              TRUE ~ "neutral"
            )
          )
      }
      
      incProgress(0.7, detail = "Finalizing analysis")
      values$sentiment <- values$data %>%
        left_join(sentiment_scores, by = "tweet_id") %>%
        mutate(
          sentiment = ifelse(is.na(sentiment), "neutral", sentiment),
          sentiment_score = ifelse(is.na(sentiment_score), 0, sentiment_score)
        )
      
      values$analyzed <- TRUE
      incProgress(1, detail = "Analysis complete!")
    })
  })
  
  output$totalTweets <- renderInfoBox({
    req(values$data)
    infoBox(
      "Total Tweets", nrow(values$data), icon = icon("twitter"),
      color = "blue"
    )
  })
  
  output$positiveTweets <- renderInfoBox({
    req(values$sentiment, values$analyzed)
    count <- sum(values$sentiment$sentiment == "positive", na.rm = TRUE)
    percentage <- round(count / nrow(values$sentiment) * 100, 1)
    infoBox(
      "Positive Tweets", paste0(count, " (", percentage, "%)"), icon = icon("smile"),
      color = "green"
    )
  })
  
  output$negativeTweets <- renderInfoBox({
    req(values$sentiment, values$analyzed)
    count <- sum(values$sentiment$sentiment == "negative", na.rm = TRUE)
    percentage <- round(count / nrow(values$sentiment) * 100, 1)
    infoBox(
      "Negative Tweets", paste0(count, " (", percentage, "%)"), icon = icon("frown"),
      color = "red"
    )
  })
  
  output$sentimentPlot <- renderPlotly({
    req(values$sentiment, values$analyzed)
    
    sentiment_counts <- values$sentiment %>%
      count(sentiment) %>%
      mutate(percentage = n / sum(n) * 100)
    
    p <- ggplot(sentiment_counts, aes(x = reorder(sentiment, -n), y = n, fill = sentiment)) +
      geom_bar(stat = "identity") +
      geom_text(aes(label = paste0(round(percentage, 1), "%")), vjust = -0.5) +
      scale_fill_manual(values = c("negative" = "#FF6B6B", "neutral" = "#4ECDC4", "positive" = "#59CD90")) +
      labs(x = "Sentiment", y = "Count") +
      theme_minimal() +
      theme(legend.position = "none")
    
    ggplotly(p)
  })
  
  output$topWords <- renderPlot({
    req(values$data, values$analyzed)
    
    tweet_text_df <- data.frame(
      tweet_id = values$data$tweet_id,
      text = values$data$cleaned_tweet,
      stringsAsFactors = FALSE
    )
    
    sentiment_data <- values$sentiment %>% 
      select(tweet_id, sentiment)
    
    tryCatch({
      tweet_word_sentiments <- tweet_text_df %>%
        unnest_tokens(word, text) %>%
        anti_join(stop_words, by = "word") %>%
        left_join(sentiment_data, by = "tweet_id") %>%
        filter(!is.na(sentiment), sentiment != "neutral") %>%
        count(sentiment, word, sort = TRUE) %>%
        group_by(sentiment) %>%
        top_n(input$top_n_words) %>%
        ungroup()
      
      ggplot(tweet_word_sentiments, aes(x = reorder(word, n), y = n, fill = sentiment)) +
        geom_col(show.legend = FALSE) +
        facet_wrap(~sentiment, scales = "free_y") +
        coord_flip() +
        scale_fill_manual(values = c("negative" = "#FF6B6B", "positive" = "#59CD90")) +
        labs(x = NULL, y = "Count") +
        theme_minimal()
    }, error = function(e) {
      
      plot(0, 0, type = "n", axes = FALSE, xlab = "", ylab = "")
      text(0, 0, "Not enough sentiment data to display top words", cex = 1.5)
    })
  })
  
  output$wordcloud <- renderPlot({
    req(values$data, values$analyzed)
    
    tryCatch({
      word_freqs <- values$data %>%
        select(text = cleaned_tweet) %>%
        unnest_tokens(word, text) %>%
        anti_join(stop_words, by = "word") %>%
        count(word, sort = TRUE) %>%
        filter(n > 2)  
      
      if(nrow(word_freqs) > 5) {
        wordcloud(words = word_freqs$word, 
                  freq = word_freqs$n, 
                  max.words = 100,
                  random.order = FALSE,
                  rot.per = 0.35,
                  colors = brewer.pal(8, "Dark2"))
      } else {
        plot(0, 0, type = "n", axes = FALSE, xlab = "", ylab = "")
        text(0, 0, "Not enough data for word cloud", cex = 1.5)
      }
    }, error = function(e) {
      
      plot(0, 0, type = "n", axes = FALSE, xlab = "", ylab = "")
      text(0, 0, "Error generating word cloud", cex = 1.5)
    })
  })
  
  output$timeline <- renderPlotly({
    req(values$sentiment, values$analyzed)
    
    tryCatch({
      daily_sentiment <- values$sentiment %>%
        group_by(date, sentiment) %>%
        summarize(count = n(), .groups = "drop") %>%
        tidyr::complete(date, sentiment, fill = list(count = 0))
      
      p <- ggplot(daily_sentiment, aes(x = date, y = count, color = sentiment, group = sentiment)) +
        geom_line() +
        geom_point() +
        scale_color_manual(values = c("negative" = "#FF6B6B", "neutral" = "#4ECDC4", "positive" = "#59CD90")) +
        labs(x = "Date", y = "Number of Tweets") +
        theme_minimal()
      
      ggplotly(p)
    }, error = function(e) {
      
      plot_ly() %>% 
        add_trace(x = c(1), y = c(1), type = "scatter", mode = "markers") %>%
        layout(title = "Not enough time data to display timeline")
    })
  })
  
  create_sample_table <- function(sentiment_type) {
    req(values$sentiment, values$analyzed)
    
    user_col <- values$user_col
    if(is.null(user_col) || !user_col %in% colnames(values$sentiment)) {
      user_col <- colnames(values$sentiment)[1]  
    }
    
    selected_samples <- values$sentiment %>%
      filter(sentiment == sentiment_type) %>%
      select(
        User = !!user_col, 
        Tweet = cleaned_tweet, 
        Score = sentiment_score
      ) %>%
      head(10)
    
    if(nrow(selected_samples) == 0) {
      return(data.frame(Message = paste0("No ", sentiment_type, " tweets found")))
    }
    
    datatable(selected_samples, options = list(pageLength = 5))
  }
  
  output$positiveSamples <- renderDT({
    create_sample_table("positive")
  })
  
  output$negativeSamples <- renderDT({
    create_sample_table("negative")
  })
  
  output$neutralSamples <- renderDT({
    create_sample_table("neutral")
  })
  
  # Add download handlers for the report and CSV
  output$downloadCSV <- downloadHandler(
    filename = function() {
      # Include the lexicon used in the filename
      paste0("twitter_sentiment_analysis_", input$sentiment_dict, "_", format(Sys.Date(), "%Y%m%d"), ".csv")
    },
    content = function(file) {
      req(values$sentiment, values$analyzed)
      
      # Select the relevant columns for export
      export_data <- values$sentiment %>%
        select(
          tweet_id,
          !!sym(values$user_col),
          cleaned_tweet,
          date,
          sentiment,
          sentiment_score
        )
      
      write.csv(export_data, file, row.names = FALSE)
    }
  )
  
  output$downloadReport <- downloadHandler(
    filename = function() {
      paste0("twitter_sentiment_report_", input$sentiment_dict, "_", format(Sys.Date(), "%Y%m%d"), ".html")
    },
    content = function(file) {
      # Create a temporary Rmd file
      tempReport <- file.path(tempdir(), "report.Rmd")
      
      # Get sentiment summary statistics
      sentiment_counts <- values$sentiment %>%
        count(sentiment) %>%
        mutate(percentage = n / sum(n) * 100)
      
      total_tweets <- nrow(values$sentiment)
      positive_count <- sum(values$sentiment$sentiment == "positive", na.rm = TRUE)
      negative_count <- sum(values$sentiment$sentiment == "negative", na.rm = TRUE)
      neutral_count <- sum(values$sentiment$sentiment == "neutral", na.rm = TRUE)
      
      # Get sample tweets for the report
      positive_samples <- values$sentiment %>%
        filter(sentiment == "positive") %>%
        head(5)
      
      negative_samples <- values$sentiment %>%
        filter(sentiment == "negative") %>%
        head(5)
      
      # Create report content
      report_content <- paste0(
        "---\n",
        "title: \"Twitter Sentiment Analysis Report\"\n",
        "date: \"", format(Sys.Date(), "%B %d, %Y"), "\"\n",
        "output: html_document\n",
        "---\n\n",
        "```{r setup, include=FALSE}\n",
        "knitr::opts_chunk$set(echo = FALSE, warning = FALSE, message = FALSE)\n",
        "library(ggplot2)\n",
        "library(dplyr)\n",
        "```\n\n",
        "## Analysis Overview\n\n",
        "This report summarizes the sentiment analysis of Twitter data using the **", input$sentiment_dict, "** lexicon.\n\n",
        "- **Total Tweets Analyzed:** ", total_tweets, "\n",
        "- **Positive Tweets:** ", positive_count, " (", round(positive_count/total_tweets*100, 1), "%)\n",
        "- **Negative Tweets:** ", negative_count, " (", round(negative_count/total_tweets*100, 1), "%)\n",
        "- **Neutral Tweets:** ", neutral_count, " (", round(neutral_count/total_tweets*100, 1), "%)\n\n",
        "## Sentiment Distribution\n\n",
        "```{r sentiment_plot}\n",
        "sentiment_data <- data.frame(\n",
        "  sentiment = c('positive', 'negative', 'neutral'),\n",
        "  count = c(", positive_count, ", ", negative_count, ", ", neutral_count, "),\n",
        "  percentage = c(", round(positive_count/total_tweets*100, 1), ", ", 
        round(negative_count/total_tweets*100, 1), ", ", 
        round(neutral_count/total_tweets*100, 1), ")\n",
        ")\n\n",
        "ggplot(sentiment_data, aes(x = reorder(sentiment, -count), y = count, fill = sentiment)) +\n",
        "  geom_bar(stat = 'identity') +\n",
        "  geom_text(aes(label = paste0(percentage, '%')), vjust = -0.5) +\n",
        "  scale_fill_manual(values = c('negative' = '#FF6B6B', 'neutral' = '#4ECDC4', 'positive' = '#59CD90')) +\n",
        "  labs(x = 'Sentiment', y = 'Count', title = 'Distribution of Tweet Sentiments') +\n",
        "  theme_minimal() +\n",
        "  theme(legend.position = 'none')\n",
        "```\n\n",
        "## Analysis Details\n\n",
        "- **Sentiment Lexicon:** ", input$sentiment_dict, "\n",
        "- **Stop Words Removed:** ", ifelse(input$remove_stop_words, "Yes", "No"), "\n",
        "- **Date Generated:** ", format(Sys.Date(), "%B %d, %Y"), "\n\n"
      )
      
      # Write report content to a temporary file
      writeLines(report_content, tempReport)
      
      # Render the report
      rmarkdown::render(tempReport, output_file = file,
                        envir = new.env(parent = globalenv()))
    },
    contentType = "text/html"
  )
}

shinyApp(ui = ui, server = server)