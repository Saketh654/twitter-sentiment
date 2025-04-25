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
    selectInput("analysis_mode", "Select Analysis Mode:",
                choices = c("Upload File", "Custom Text"),
                selected = "Upload File"),
    
    # File upload inputs (shown only in Upload File mode)
    conditionalPanel(
      condition = "input.analysis_mode == 'Upload File'",
      fileInput("file", "Upload Twitter Dataset (CSV)", accept = c(".csv")),
      actionButton("analyze", "Start Sentiment Analysis", icon = icon("play"),
                   class = "btn-primary", style = "width: 85%;"),
      br(), br(),
      selectInput("sentiment_dict", "Sentiment Dictionary:",
                  choices = c("AFINN", "Bing", "NRC"), selected = "AFINN"),
      sliderInput("top_n_words", "Top N Words:", min = 5, max = 50, value = 15),
      checkboxInput("remove_stop_words", "Remove Stop Words", value = TRUE),
      downloadButton("downloadReport", "Download Report",
                     style = "color: #fff; background-color: #5cb85c; border-color: #4cae4c; width: 85%; margin-left:17px;"),
      br(), br(),
      downloadButton("downloadCSV", "Download Analysis CSV",
                     style = "color: #fff; background-color: #f0ad4e; border-color: #eea236; width: 85%; margin-left:17px;")
    ),
    
    # Dictionary selector for Custom Text mode
    conditionalPanel(
      condition = "input.analysis_mode == 'Custom Text'",
      selectInput("sentiment_dict_custom", "Sentiment Dictionary:",
                  choices = c("AFINN", "Bing", "NRC"), selected = "AFINN")
    )
  ),
  
  dashboardBody(
    uiOutput("dynamic_ui")  # Will be rendered dynamically in server
  )
)

server <- function(input, output, session) {
  
  values <- reactiveValues(
    data = NULL,
    sentiment = NULL,
    analyzed = FALSE,
    user_col = NULL,
    tweet_col = NULL,
    custom_sentiment_data = NULL,  # Store custom text sentiment data
    emotions_data = NULL  # Store emotions data for NRC
  )
  
  output$dynamic_ui <- renderUI({
    if (input$analysis_mode == "Custom Text") {
      fluidPage(
        fluidRow(
          box(
            title = "Custom Text Sentiment Analysis",
            status = "primary",
            width = 12,
            textInput("user_text", "Enter text:", ""),
            actionButton("analyze_text", "Analyze Text", icon = icon("search")),
            br(), br(),
            verbatimTextOutput("text_sentiment_result")
          )
        ),
        fluidRow(
          box(title = "Sentiment Overview", width = 6, plotOutput("custom_sentiment_plot")),
          box(title = "Top Sentiment Words", width = 6, plotOutput("custom_top_words_plot"))
        ),
        # Add emotions visualization for NRC
        conditionalPanel(
          condition = "input.sentiment_dict_custom == 'NRC'",
          fluidRow(
            box(title = "Emotions Distribution", width = 12, plotOutput("custom_emotions_plot"))
          )
        )
      )
    } else {
      fluidPage(
        fluidRow(
          infoBoxOutput("totalTweets", width = 4),
          infoBoxOutput("positiveTweets", width = 4),
          infoBoxOutput("negativeTweets", width = 4)
        ),
        fluidRow(
          box(title = "Sentiment Overview", plotlyOutput("sentimentPlot"), width = 6),
          box(title = "Top Words by Sentiment", plotOutput("topWords"), width = 6)
        ),
        # Add emotions visualization for NRC
        conditionalPanel(
          condition = "input.sentiment_dict == 'NRC'",
          fluidRow(
            box(title = "Emotions Analysis", plotlyOutput("emotionsPlot"), width = 12)
          )
        ),
        fluidRow(
          box(title = "Word Cloud", plotOutput("wordcloud"), width = 6),
          box(title = "Sentiment Timeline", plotlyOutput("timeline"), width = 6)
        ),
        fluidRow(
          box(title = "Sample Tweets by Sentiment",
              tabsetPanel(
                id = "sampleTabs",
                tabPanel("Positive", DTOutput("positiveSamples")),
                tabPanel("Negative", DTOutput("negativeSamples")),
                tabPanel("Neutral", DTOutput("neutralSamples"))
              ),
              width = 12)
        ),
        # Additional tabset for emotions when NRC is selected
        conditionalPanel(
          condition = "input.sentiment_dict == 'NRC'",
          fluidRow(
            box(title = "Sample Tweets by Emotion",
                tabsetPanel(
                  id = "emotionTabs",
                  tabPanel("Anger", DTOutput("angerSamples")),
                  tabPanel("Fear", DTOutput("fearSamples")),
                  tabPanel("Joy", DTOutput("joySamples")),
                  tabPanel("Sadness", DTOutput("sadnessSamples")),
                  tabPanel("Trust", DTOutput("trustSamples")),
                  tabPanel("Anticipation", DTOutput("anticipationSamples")),
                  tabPanel("Disgust", DTOutput("disgustSamples")),
                  tabPanel("Surprise", DTOutput("surpriseSamples"))
                ),
                width = 12)
          )
        )
      )
    }
  })
  
  # Custom Text Sentiment Analysis Logic
  observeEvent(input$analyze_text, {
    req(input$user_text)
    
    # Dictionary selection for custom text analysis
    selected_dict <- input$sentiment_dict_custom  # Use the custom dictionary selector
    
    words <- tibble(text = input$user_text) %>%
      unnest_tokens(word, text)
    
    if(input$analysis_mode == "Custom Text" && nrow(words) > 0) {
      # Apply stop words removal if needed
      data("stop_words")
      custom_stop_words <- stop_words %>% 
        bind_rows(tibble(word = c("im"), lexicon = "custom"))
      words <- words %>% 
        anti_join(custom_stop_words, by = "word")
      
      # Get sentiment dictionary based on selection
      if (selected_dict == "NRC") {
        # For NRC, we want all emotions, not just positive/negative
        sentiment_dict <- get_sentiments("nrc")
      } else {
        sentiment_dict <- switch(selected_dict,
                                 "AFINN" = get_sentiments("afinn"),
                                 "Bing" = get_sentiments("bing"))
      }
      
      # Join with sentiment dictionary
      sentiment_data <- words %>%
        inner_join(sentiment_dict, by = "word", relationship = "many-to-many")
      
      # Store for later use in plots
      values$custom_sentiment_data <- sentiment_data
      
      # Calculate overall sentiment
      result <- if (selected_dict == "AFINN") {
        score <- sum(sentiment_data$value, na.rm = TRUE)
        if (score > 0) "Positive" else if (score < 0) "Negative" else "Neutral"
      } else if (selected_dict == "Bing") {
        pos <- sum(sentiment_data$sentiment == "positive", na.rm = TRUE)
        neg <- sum(sentiment_data$sentiment == "negative", na.rm = TRUE)
        if (pos > neg) "Positive" else if (neg > pos) "Negative" else "Neutral"
      } else { # NRC
        # For NRC we'll show sentiment AND emotions
        pos <- sum(sentiment_data$sentiment == "positive", na.rm = TRUE)
        neg <- sum(sentiment_data$sentiment == "negative", na.rm = TRUE)
        
        # Get top emotions
        emotions <- sentiment_data %>%
          filter(!sentiment %in% c("positive", "negative")) %>%
          count(sentiment, sort = TRUE)
        
        # Format the result to include emotions
        main_sentiment <- if (pos > neg) "Positive" else if (neg > pos) "Negative" else "Neutral"
        top_emotions <- if(nrow(emotions) > 0) {
          paste("Top emotions:", paste(head(emotions$sentiment, 3), collapse = ", "))
        } else {
          "No specific emotions detected"
        }
        
        paste(main_sentiment, "\n", top_emotions)
      }
      
      output$text_sentiment_result <- renderText({
        result
      })
    }
  })
  
  # Custom sentiment plot
  output$custom_sentiment_plot <- renderPlot({
    req(values$custom_sentiment_data)
    sentiment_data <- values$custom_sentiment_data
    
    if (nrow(sentiment_data) == 0) return(NULL)
    
    # Handle different sentiment dictionaries
    if ("value" %in% colnames(sentiment_data)) {
      # AFINN dictionary
      sentiment_summary <- data.frame(
        sentiment = c("positive", "negative", "neutral"),
        n = c(sum(sentiment_data$value > 0, na.rm = TRUE),
              sum(sentiment_data$value < 0, na.rm = TRUE),
              sum(sentiment_data$value == 0, na.rm = TRUE))
      )
      sentiment_summary <- sentiment_summary[sentiment_summary$n > 0,]
    } else {
      # Bing or NRC dictionary
      if ("sentiment" %in% colnames(sentiment_data)) {
        # For NRC, filter only positive/negative for this plot
        if (input$sentiment_dict_custom == "NRC") {
          sentiment_data <- sentiment_data %>%
            filter(sentiment %in% c("positive", "negative"))
        }
        sentiment_summary <- sentiment_data %>%
          count(sentiment, sort = TRUE)
      } else {
        # Fallback if no sentiment column (shouldn't happen)
        return(NULL)
      }
    }
    
    # Plot with correct column names
    ggplot(sentiment_summary, aes(x = sentiment, y = n, fill = sentiment)) +
      geom_col(show.legend = FALSE) +
      labs(title = "Sentiment Overview", x = "", y = "Count") +
      theme_minimal()
  })
  
  # Custom emotions plot for NRC
  output$custom_emotions_plot <- renderPlot({
    req(values$custom_sentiment_data)
    sentiment_data <- values$custom_sentiment_data
    
    if (nrow(sentiment_data) == 0 || input$sentiment_dict_custom != "NRC") 
      return(NULL)
    
    # Filter for emotions only (exclude positive/negative)
    emotions_data <- sentiment_data %>%
      filter(!sentiment %in% c("positive", "negative")) %>%
      count(sentiment, sort = TRUE)
    
    if (nrow(emotions_data) == 0) {
      # If no emotions, display a message
      plot(0, 0, type = "n", axes = FALSE, xlab = "", ylab = "")
      text(0, 0, "No emotions detected in the text", cex = 1.5)
    } else {
      # Create a colorful plot for emotions
      emotion_colors <- c(
        "anger" = "#E74C3C", "fear" = "#8E44AD", "joy" = "#F1C40F", 
        "sadness" = "#3498DB", "trust" = "#2ECC71", "anticipation" = "#E67E22",
        "disgust" = "#7F8C8D", "surprise" = "#9B59B6"
      )
      
      ggplot(emotions_data, aes(x = reorder(sentiment, n), y = n, fill = sentiment)) +
        geom_col(show.legend = FALSE) +
        scale_fill_manual(values = emotion_colors) +
        coord_flip() +
        labs(title = "Emotions Distribution", x = "", y = "Count") +
        theme_minimal()
    }
  })
  
  # Top words plot
  output$custom_top_words_plot <- renderPlot({
    req(values$custom_sentiment_data)
    sentiment_data <- values$custom_sentiment_data
    
    if (nrow(sentiment_data) == 0) return(NULL)
    
    # Count word frequencies
    top_words <- sentiment_data %>%
      count(word, sort = TRUE) %>%
      top_n(10, n)
    
    # Plot with correct column names
    ggplot(top_words, aes(x = reorder(word, n), y = n, fill = word)) +
      geom_col(show.legend = FALSE) +
      coord_flip() +
      labs(title = "Top Words", x = "Word", y = "Frequency") +
      theme_minimal()
  })
  
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
    
    if (!"date" %in% all_cols) {
      if ("Formatted_Date" %in% all_cols) {
        # Use Formatted_Date as the date column
        values$data$date <- as.Date(values$data$Formatted_Date, format = "%d--%m--%Y")
      } else {
        # Fallback: use random recent dates
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
        custom_stop_words <- stop_words %>% 
          bind_rows(tibble(word = c("im"), lexicon = "custom"))
        tweet_words <- tweet_words %>% 
          anti_join(custom_stop_words, by = "word")
      }
      
      incProgress(0.3, detail = "Calculating sentiment scores")
      if(input$sentiment_dict == "AFINN") {
        sentiment_scores <- tweet_words %>%
          inner_join(get_sentiments("afinn"), by = "word", relationship = "many-to-many") %>%
          group_by(tweet_id) %>%
          summarize(sentiment_score = sum(value), .groups = "drop") %>%
          mutate(sentiment = case_when(
            sentiment_score > 0 ~ "positive",
            sentiment_score < 0 ~ "negative",
            TRUE ~ "neutral"
          ))
      } else if(input$sentiment_dict == "Bing") {
        sentiment_scores <- tweet_words %>%
          inner_join(get_sentiments("bing"), by = "word", relationship = "many-to-many") %>%
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
      } else { # NRC
        # For both sentiment polarity (positive/negative)
        sentiment_scores <- tweet_words %>%
          inner_join(get_sentiments("nrc") %>% 
                       filter(sentiment %in% c("positive", "negative")), 
                     by = "word", relationship = "many-to-many") %>%
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
        
        # For emotions analysis (when NRC is selected)
        incProgress(0.5, detail = "Analyzing emotions")
        emotions_scores <- tweet_words %>%
          inner_join(
            get_sentiments("nrc") %>% 
              filter(!sentiment %in% c("positive", "negative")),
            by = "word", relationship = "many-to-many"
          ) %>%
          count(tweet_id, sentiment) %>%
          tidyr::pivot_wider(names_from = sentiment, values_from = n, values_fill = 0)
        
        # Define emotion columns that we'll look for
        emotion_cols <- c("anger", "fear", "joy", "sadness", "trust", 
                          "anticipation", "disgust", "surprise")
        
        # Store valid emotion columns in reactive values for later use
        values$valid_emotion_cols <- intersect(emotion_cols, colnames(emotions_scores))
        
        # Store emotions data
        if (nrow(emotions_scores) > 0 && length(values$valid_emotion_cols) > 0) {
          values$emotions_data <- values$data %>%
            select(tweet_id, cleaned_tweet, !!sym(values$user_col)) %>%
            left_join(emotions_scores, by = "tweet_id")
          
          # For each row, determine the dominant emotion
          values$emotions_data <- values$emotions_data %>%
            mutate(
              # Create a column for the max value across all emotion columns
              max_emotion_value = pmax(
                ifelse("anger" %in% colnames(.), .data$anger, -Inf),
                ifelse("fear" %in% colnames(.), .data$fear, -Inf),
                ifelse("joy" %in% colnames(.), .data$joy, -Inf),
                ifelse("sadness" %in% colnames(.), .data$sadness, -Inf),
                ifelse("trust" %in% colnames(.), .data$trust, -Inf),
                ifelse("anticipation" %in% colnames(.), .data$anticipation, -Inf),
                ifelse("disgust" %in% colnames(.), .data$disgust, -Inf),
                ifelse("surprise" %in% colnames(.), .data$surprise, -Inf),
                na.rm = TRUE
              )
            ) %>%
            rowwise() %>%
            mutate(
              # Get the name of the emotion with the highest value
              dominant_emotion = case_when(
                max_emotion_value <= 0 ~ NA_character_,
                "anger" %in% colnames(.) && max_emotion_value == anger ~ "anger",
                "fear" %in% colnames(.) && max_emotion_value == fear ~ "fear",
                "joy" %in% colnames(.) && max_emotion_value == joy ~ "joy",
                "sadness" %in% colnames(.) && max_emotion_value == sadness ~ "sadness",
                "trust" %in% colnames(.) && max_emotion_value == trust ~ "trust",
                "anticipation" %in% colnames(.) && max_emotion_value == anticipation ~ "anticipation",
                "disgust" %in% colnames(.) && max_emotion_value == disgust ~ "disgust",
                "surprise" %in% colnames(.) && max_emotion_value == surprise ~ "surprise",
                TRUE ~ NA_character_
              )
            ) %>%
            select(-max_emotion_value) %>%
            ungroup()
        }
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
  
  # New plot for emotions when NRC is selected
  output$emotionsPlot <- renderPlotly({
    req(values$emotions_data, values$analyzed, input$sentiment_dict == "NRC")
    
    # Ensure we have emotions data
    if (is.null(values$emotions_data) || nrow(values$emotions_data) == 0) {
      return(plot_ly() %>% 
               add_trace(x = c(1), y = c(1), type = "scatter", mode = "markers") %>%
               layout(title = "Not enough data to display emotions"))
    }
    
    # Get all emotion columns
    emotion_cols <- c("anger", "fear", "joy", "sadness", "trust", 
                      "anticipation", "disgust", "surprise")
    
    # Get only columns that exist in our data
    valid_emotion_cols <- intersect(emotion_cols, colnames(values$emotions_data))
    
    if (length(valid_emotion_cols) == 0) {
      return(plot_ly() %>% 
               add_trace(x = c(1), y = c(1), type = "scatter", mode = "markers") %>%
               layout(title = "No emotions detected in the dataset"))
    }
    
    # Sum emotions across all tweets
    emotions_summary <- values$emotions_data %>%
      summarize(across(all_of(valid_emotion_cols), sum, na.rm = TRUE)) %>%
      tidyr::pivot_longer(cols = everything(), names_to = "emotion", values_to = "count") %>%
      arrange(desc(count))
    
    # Colors for emotions
    emotion_colors <- c(
      "anger" = "#E74C3C", "fear" = "#8E44AD", "joy" = "#F1C40F", 
      "sadness" = "#3498DB", "trust" = "#2ECC71", "anticipation" = "#E67E22",
      "disgust" = "#7F8C8D", "surprise" = "#9B59B6"
    )
    
    # Create color vector matching our data
    colors <- emotion_colors[emotions_summary$emotion]
    
    p <- ggplot(emotions_summary, aes(x = reorder(emotion, count), y = count, fill = emotion)) +
      geom_bar(stat = "identity") +
      geom_text(aes(label = count), vjust = -0.5) +
      scale_fill_manual(values = colors) +
      labs(title = "Distribution of Emotions in Tweets", x = "Emotion", y = "Count") +
      theme_minimal() +
      theme(legend.position = "none") +
      coord_flip()
    
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
      data("stop_words")
      custom_stop_words <- stop_words %>%
        bind_rows(tibble(word = c("im"), lexicon = "custom"))
      
      tweet_word_sentiments <- tweet_text_df %>%
        unnest_tokens(word, text) %>%
        anti_join(custom_stop_words, by = "word") %>%
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
      data("stop_words")
      custom_stop_words <- stop_words %>%
        bind_rows(tibble(word = c("im"), lexicon = "custom"))
      
      word_freqs <- values$data %>%
        select(text = cleaned_tweet) %>%
        unnest_tokens(word, text) %>%
        anti_join(custom_stop_words, by = "word") %>%
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
        User = !!sym(user_col), 
        Tweet = cleaned_tweet, 
        Score = sentiment_score
      ) %>%
      head(10)
    
    if(nrow(selected_samples) == 0) {
      return(data.frame(Message = paste0("No ", sentiment_type, " tweets found")))
    }
    
    datatable(selected_samples, options = list(pageLength = 5))
  }
  
  # New function to create emotion sample tables
  create_emotion_table <- function(emotion_type) {
    req(values$emotions_data, values$analyzed, input$sentiment_dict == "NRC")
    
    # Check if the emotion column exists
    if(!emotion_type %in% colnames(values$emotions_data)) {
      return(data.frame(Message = paste0("No ", emotion_type, " data found")))
    }
    
    user_col <- values$user_col
    
    # Get tweets with this emotion
    selected_samples <- values$emotions_data %>%
      filter(!!sym(emotion_type) > 0) %>%  # Select tweets that have this emotion
      select(
        User = !!sym(user_col),
        Tweet = cleaned_tweet,
        Score = !!sym(emotion_type)  # Use the emotion score
      ) %>%
      arrange(desc(Score)) %>%
      head(10)
    
    if(nrow(selected_samples) == 0) {
      return(data.frame(Message = paste0("No tweets with ", emotion_type, " emotion found")))
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
  
  # Render emotion sample tables
  output$angerSamples <- renderDT({
    create_emotion_table("anger")
  })
  
  output$fearSamples <- renderDT({
    create_emotion_table("fear")
  })
  
  output$joySamples <- renderDT({
    create_emotion_table("joy")
  })
  
  output$sadnessSamples <- renderDT({
    create_emotion_table("sadness")
  })
  
  output$trustSamples <- renderDT({
    create_emotion_table("trust")
  })
  
  output$anticipationSamples <- renderDT({
    create_emotion_table("anticipation")
  })
  
  output$disgustSamples <- renderDT({
    create_emotion_table("disgust")
  })
  
  output$surpriseSamples <- renderDT({
    create_emotion_table("surprise")
  })
  
  # Add download handlers for the report and CSV
  output$downloadCSV <- downloadHandler(
    filename = function() {
      # Include the lexicon used in the filename
      paste0("twitter_sentiment_analysis_", input$sentiment_dict, "_", format(Sys.Date(), "%Y%m%d"), ".csv")
    },
    content = function(file) {
      req(values$sentiment, values$analyzed)
      
      # Start with basic sentiment data
      export_data <- values$sentiment %>%
        select(
          tweet_id,
          !!sym(values$user_col),
          cleaned_tweet,
          date,
          sentiment,
          sentiment_score
        )
      
      # Add emotion data if NRC lexicon is used
      if(input$sentiment_dict == "NRC" && !is.null(values$emotions_data)) {
        # Get emotion columns
        emotion_cols <- c("anger", "fear", "joy", "sadness", "trust", "anticipation", "disgust", "surprise")
        valid_emotion_cols <- intersect(emotion_cols, colnames(values$emotions_data))
        
        if(length(valid_emotion_cols) > 0) {
          emotions_export <- values$emotions_data %>%
            select(tweet_id, all_of(valid_emotion_cols), dominant_emotion)
          
          # Join with sentiment data
          export_data <- export_data %>%
            left_join(emotions_export, by = "tweet_id")
        }
      }
      
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
      
      # Start building the report content
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
        "```\n\n"
      )
      
      # Add emotions analysis section if NRC lexicon is used
      if(input$sentiment_dict == "NRC" && !is.null(values$emotions_data)) {
        # Get emotion columns
        emotion_cols <- c("anger", "fear", "joy", "sadness", "trust", "anticipation", "disgust", "surprise")
        valid_emotion_cols <- intersect(emotion_cols, colnames(values$emotions_data))
        
        if(length(valid_emotion_cols) > 0) {
          # Add emotions section to the report
          report_content <- paste0(
            report_content,
            "## Emotions Analysis\n\n",
            "The NRC lexicon classifies words into eight basic emotions. The distribution of emotions in the tweets is:\n\n",
            "```{r emotions_plot}\n",
            "# Define emotion colors\n",
            "emotion_colors <- c(\n",
            "  \"anger\" = \"#E74C3C\", \"fear\" = \"#8E44AD\", \"joy\" = \"#F1C40F\",\n", 
            "  \"sadness\" = \"#3498DB\", \"trust\" = \"#2ECC71\", \"anticipation\" = \"#E67E22\",\n",
            "  \"disgust\" = \"#7F8C8D\", \"surprise\" = \"#9B59B6\"\n",
            ")\n\n",
            "# Data for emotions\n",
            "emotions_data <- data.frame(\n",
            "  emotion = c('", paste(valid_emotion_cols, collapse = "', '"), "'),\n",
            "  count = c(", paste(
              sapply(valid_emotion_cols, function(col) {
                sum(values$emotions_data[[col]], na.rm = TRUE)
              }), 
              collapse = ", "
            ), ")\n",
            ")\n\n",
            "# Plot emotions\n",
            "ggplot(emotions_data, aes(x = reorder(emotion, count), y = count, fill = emotion)) +\n",
            "  geom_bar(stat = 'identity') +\n",
            "  geom_text(aes(label = count), vjust = -0.5) +\n",
            "  scale_fill_manual(values = emotion_colors[emotions_data$emotion]) +\n",
            "  labs(x = 'Emotion', y = 'Count', title = 'Distribution of Emotions in Tweets') +\n",
            "  theme_minimal() +\n",
            "  theme(legend.position = 'none') +\n",
            "  coord_flip()\n",
            "```\n\n",
            "### Top Emotions in Tweets\n\n",
            "The most common emotion detected was **", valid_emotion_cols[which.max(sapply(valid_emotion_cols, function(col) sum(values$emotions_data[[col]], na.rm = TRUE)))], "**.\n\n"
          )
          
          # Add information about dominant emotions in tweets
          if("dominant_emotion" %in% colnames(values$emotions_data)) {
            dominant_counts <- values$emotions_data %>%
              filter(!is.na(dominant_emotion)) %>%
              count(dominant_emotion, sort = TRUE)
            
            if(nrow(dominant_counts) > 0) {
              report_content <- paste0(
                report_content,
                "### Dominant Emotions\n\n",
                "Each tweet was classified by its dominant emotion:\n\n",
                "```{r dominant_emotions}\n",
                "dominant_data <- data.frame(\n",
                "  emotion = c('", paste(dominant_counts$dominant_emotion, collapse = "', '"), "'),\n",
                "  count = c(", paste(dominant_counts$n, collapse = ", "), ")\n",
                ")\n\n",
                "ggplot(dominant_data, aes(x = reorder(emotion, count), y = count, fill = emotion)) +\n",
                "  geom_bar(stat = 'identity') +\n",
                "  geom_text(aes(label = count), hjust = -0.2) +\n",
                "  scale_fill_manual(values = emotion_colors[dominant_data$emotion]) +\n",
                "  labs(x = '', y = 'Count', title = 'Number of Tweets by Dominant Emotion') +\n",
                "  theme_minimal() +\n",
                "  theme(legend.position = 'none') +\n",
                "  coord_flip()\n",
                "```\n\n"
              )
            }
          }
        }
      }
      
      # Add analysis details section
      report_content <- paste0(
        report_content,
        "## Analysis Details\n\n",
        "- **Sentiment Lexicon:** ", input$sentiment_dict, "\n"
      )
      
      if(input$sentiment_dict == "NRC") {
        report_content <- paste0(
          report_content,
          "- **NRC Emotions Analyzed:** ", paste(valid_emotion_cols, collapse = ", "), "\n"
        )
      }
      
      report_content <- paste0(
        report_content,
        "- **Stop Words Removed:** ", ifelse(input$remove_stop_words, "Yes", "No"), "\n",
        "- **Date Generated:** ", format(Sys.Date(), "%B %d, %Y"), "\n\n"
      )
      
      # Write report content to a temporary file
      writeLines(report_content, tempReport)
      
      # Render the report
      rmarkdown::render(tempReport, output_file = file,
                        envir = new.env(parent = globalenv()))
    }
  )
}

shinyApp(ui = ui, server = server)