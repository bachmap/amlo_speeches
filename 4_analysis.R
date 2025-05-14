################################
# Clean up
rm(list = ls())

# Read needed packages
library(MASS)
library(tidyverse)
library(lubridate)
library(quanteda)
library(quanteda.textplots)
library(tidytext)
library(sentimentr)
library(tsibble)
library(fable)
library(tokenizers)
library(forecast)
library(scales)

# Load Data
setwd("/Users/paulbachmann/Nextcloud/Amlo_paper/2_Data")
amlo <- read.csv("amlo_videos_with_transcripts_neu.csv")
nrc <- read_csv("lexico_nrc.csv")
media_targets <- read_lines("media_targets.txt")
events <- read_csv("amlo_presidency_events.csv")
approval_ratings <- read_csv("approval_ratings_tracker_AMLO.csv")

#data overview
glimpse(amlo)
glimpse(events)
glimpse(nrc)
media_targets
glimpse(approval_ratings)

#########################
# Prepare data
#########################

# Create date variable
amlo <- amlo %>%
  mutate(upload_date = ymd(upload_date))

# Stopwords
data("stop_words")  # contains only English, so load Spanish manually:
spanish_stopwords <- read_csv("https://raw.githubusercontent.com/stopwords-iso/stopwords-es/master/stopwords-es.txt", col_names = FALSE) %>%
  rename(word = X1)

# Tokenize 
amlo_sentences <- amlo %>%
  dplyr::select(video_id, upload_date, transcript) %>%
  unnest_tokens(sentence, transcript, token = "sentences")

##########################################
# Prepare approval ratings
##########################################

# Approval percentages to numeric
approval_ratings <- approval_ratings %>%
  mutate(
    Approve_num = as.numeric(str_remove(Approve, "%")),
    Disapprove_num = as.numeric(str_remove(Disapprove, "%"))
  )

# Extract dates
approval_ratings <- approval_ratings %>%
  mutate(
    # Extract date information from the first column using regex
    poll_date = str_extract(...1, "\\w+ \\d+, \\d{4}"),
    # Convert to proper date format
    date = mdy(poll_date)
  )

# Add quarter and month variables to match with amlo transcripts
approval_ratings <- approval_ratings %>%
  mutate(
    year = year(date),
    quarter = quarter(date),
    month = month(date),
    quarter_label = paste0(year, "-Q", quarter),
    month_label = paste0(year, "-", sprintf("%02d", month))
  )

# Aggregate by quarter (average of approval ratings per quarter)
quarterly_approval <- approval_ratings %>%
  group_by(year, quarter, quarter_label) %>%
  summarise(
    mean_approval = mean(Approve_num, na.rm = TRUE),
    mean_disapproval = mean(Disapprove_num, na.rm = TRUE),
    n_polls = n()
  ) %>%
  arrange(year, quarter)

# Aggregate by month (average of approval ratings per month)
monthly_approval <- approval_ratings %>%
  group_by(year, month, month_label) %>%
  summarise(
    mean_approval = mean(Approve_num, na.rm = TRUE),
    mean_disapproval = mean(Disapprove_num, na.rm = TRUE),
    n_polls = n()
  ) %>%
  arrange(year, month)

# Create time series format for quarterly data
quarterly_approval_ts <- quarterly_approval %>%
  mutate(
    quarter_date = yq(quarter_label)
  ) %>%
  rename(period = quarter_date)

# Create time series format for monthly data
monthly_approval_ts <- monthly_approval %>%
  mutate(
    month_date = ym(paste0(year, "-", sprintf("%02d", month)))
  ) %>%
  rename(period = month_date)

###############################
# Process media mentions and sentiment
###############################

media_targets <- tolower(media_targets)

# Identify media mentions and attacks
amlo_sentences <- amlo_sentences %>%
  mutate(is_media_mention = str_detect(tolower(sentence), str_c(media_targets, collapse = "|")))

# Get positive and negative sentiment words
nrc_pos <- nrc %>% filter(sentimiento == "positivo") %>% pull(palabra)
nrc_neg <- nrc %>% filter(sentimiento == "negativo") %>% pull(palabra)

# Sentiment coding for media-related sentences
amlo_sentences <- amlo_sentences %>%
  mutate(
    is_attack = is_media_mention & str_detect(tolower(sentence), str_c(nrc_neg, collapse = "|")),
    sentiment = case_when(
      is_media_mention & str_detect(tolower(sentence), str_c(nrc_neg, collapse = "|")) ~ "negative",
      is_media_mention & str_detect(tolower(sentence), str_c(nrc_pos, collapse = "|")) ~ "positive",
      is_media_mention ~ "neutral",
      TRUE ~ NA_character_
    ))

# Add month and quarter variables
amlo_sentences <- amlo_sentences %>%
  mutate(
    quarter = floor_date(upload_date, unit = "quarter"),
    month = floor_date(upload_date, unit = "month")
  )

# Process events data
events <- events %>% 
  mutate(
    event_type = case_when(
      Category == "Institutional" ~ "institutional",
      Category == "Non-institutional" ~ "non_institutional",
      TRUE ~ "other"
    ),
    quarter = floor_date(Date, unit = "quarter"),
    month = floor_date(Date, unit = "month")
  )

###############################
# QUARTERLY ANALYSIS
###############################

# Create quarterly attacks dataset
quarterly_attacks <- amlo_sentences %>%
  filter(sentiment == "negative") %>%
  count(quarter) %>%
  rename(verbal_attacks = n) %>%
  complete(quarter = seq(min(quarter), max(quarter), by = "3 months"), 
           fill = list(verbal_attacks = 0))

# Create quarterly intervention dummies
quarterly_interventions <- events %>%
  group_by(quarter) %>%
  summarise(
    non_institutional = max(as.integer(event_type == "non_institutional")),
    institutional = max(as.integer(event_type == "institutional"))
  )

# Join with attacks data
quarterly_attacks <- quarterly_attacks %>%
  left_join(quarterly_interventions, by = "quarter") %>%
  replace_na(list(non_institutional = 0, institutional = 0))

# Add approval ratings
quarterly_attacks <- quarterly_attacks %>%
  left_join(quarterly_approval_ts, by = c("quarter" = "period"))

# Create time series
quarterly_ts <- ts(quarterly_attacks$verbal_attacks, 
                   frequency = 4, 
                   start = c(year(min(quarterly_attacks$quarter)), 
                             quarter(min(quarterly_attacks$quarter))))

# Interventions as regressors
quarterly_xreg <- as.matrix(quarterly_attacks %>% 
                              dplyr::select(non_institutional, institutional))

# ARIMA with exogenous variables
quarterly_bt_model <- auto.arima(quarterly_ts, xreg = quarterly_xreg)
summary(quarterly_bt_model)

# Poisson model
quarterly_poisson <- glm(
  verbal_attacks ~ non_institutional + institutional,
  data = quarterly_attacks,
  family = poisson()
)
summary(quarterly_poisson)

# Negative Binomial model
quarterly_nb <- glm.nb(
  verbal_attacks ~ non_institutional + institutional,
  data = quarterly_attacks
)
summary(quarterly_nb)

# Models with control
quarterly_xreg_with_approval <- as.matrix(quarterly_attacks %>% 
                                            dplyr::select(non_institutional, institutional, mean_approval))

quarterly_bt_with_approval <- auto.arima(quarterly_ts, xreg = quarterly_xreg_with_approval)
summary(quarterly_bt_with_approval)

quarterly_poisson_with_approval <- glm(
  verbal_attacks ~ non_institutional + institutional + mean_approval,
  data = quarterly_attacks,
  family = poisson()
)
summary(quarterly_poisson_with_approval)

quarterly_nb_with_approval <- glm.nb(
  verbal_attacks ~ non_institutional + institutional + mean_approval,
  data = quarterly_attacks
)
summary(quarterly_nb_with_approval)

# Quarterly plot
quarterly_plot <- ggplot(quarterly_attacks, aes(x = quarter, y = verbal_attacks)) +
  geom_line(color = "darkred", size = 1) +
  geom_point(color = "darkred") +
  geom_vline(
    data = filter(quarterly_attacks, non_institutional == 1),
    aes(xintercept = as.numeric(quarter)),
    color = "blue", linetype = "dashed"
  ) +
  geom_vline(
    data = filter(quarterly_attacks, institutional == 1),
    aes(xintercept = as.numeric(quarter)),
    color = "green", linetype = "dotted"
  ) +
  labs(
    title = "",
    x = "Quarter",
    y = "Number of negative media attacks"
  ) +
  theme_minimal()

ggsave("/Users/paulbachmann/Nextcloud/Amlo_paper/3_Output/quarterly_plot.png", quarterly_plot, width = 10, height = 10, dpi = 500)

###############################
# MONTHLY ANALYSIS
###############################

# Create monthly attacks dataset
monthly_attacks <- amlo_sentences %>%
  filter(sentiment == "negative") %>%
  count(month) %>%
  rename(verbal_attacks = n) %>%
  complete(month = seq(min(month), max(month), by = "1 month"), 
           fill = list(verbal_attacks = 0))

# Create monthly intervention dummies
monthly_interventions <- events %>%
  group_by(month) %>%
  summarise(
    non_institutional = max(as.integer(event_type == "non_institutional")),
    institutional = max(as.integer(event_type == "institutional"))
  )

# Join with attacks data
monthly_attacks <- monthly_attacks %>%
  left_join(monthly_interventions, by = "month") %>%
  replace_na(list(non_institutional = 0, institutional = 0))

# Add approval ratings to monthly data (using the closest available)
monthly_attacks <- monthly_attacks %>%
  left_join(monthly_approval_ts, by = c("month" = "period"))

# Create monthly time series
monthly_ts <- ts(monthly_attacks$verbal_attacks, 
                 frequency = 12, 
                 start = c(year(min(monthly_attacks$month)), 
                           month(min(monthly_attacks$month))))

# Interventions as regressors
monthly_xreg <- as.matrix(monthly_attacks %>% 
                            dplyr::select(non_institutional, institutional))

# ARIMA with exogenous variables
monthly_bt_model <- auto.arima(monthly_ts, xreg = monthly_xreg)
summary(monthly_bt_model)

# Poisson model
monthly_poisson <- glm(
  verbal_attacks ~ non_institutional + institutional,
  data = monthly_attacks,
  family = poisson()
)
summary(monthly_poisson)

# Negative Binomial model
monthly_nb <- glm.nb(
  verbal_attacks ~ non_institutional + institutional,
  data = monthly_attacks
)
summary(monthly_nb)

# Models with control
monthly_xreg_with_approval <- as.matrix(monthly_attacks %>%
                                          filter(!is.na(mean_approval)) %>%
                                          dplyr::select(non_institutional, institutional, mean_approval))

monthly_ts_filtered <- ts(
  monthly_attacks %>% 
    filter(!is.na(mean_approval)) %>% 
    pull(verbal_attacks),
  frequency = 12,
  start = c(year(min(monthly_attacks$month[!is.na(monthly_attacks$mean_approval)])),
            month(min(monthly_attacks$month[!is.na(monthly_attacks$mean_approval)])))
)

monthly_bt_with_approval <- auto.arima(monthly_ts_filtered, xreg = monthly_xreg_with_approval)
summary(monthly_bt_with_approval)

monthly_poisson_with_approval <- glm(
  verbal_attacks ~ non_institutional + institutional + mean_approval,
  data = monthly_attacks %>% filter(!is.na(mean_approval)),
  family = poisson()
)
summary(monthly_poisson_with_approval)

monthly_nb_with_approval <- glm.nb(
  verbal_attacks ~ non_institutional + institutional + mean_approval,
  data = monthly_attacks %>% filter(!is.na(mean_approval))
)
summary(monthly_nb_with_approval)

# Monthly plot
month_plot <- ggplot(monthly_attacks, aes(x = month, y = verbal_attacks)) +
  geom_line(color = "darkred", size = 1) +
  geom_point(color = "darkred") +
  geom_vline(
    data = filter(monthly_attacks, non_institutional == 1),
    aes(xintercept = as.numeric(month)),
    color = "blue", linetype = "dashed"
  ) +
  geom_vline(
    data = filter(monthly_attacks, institutional == 1),
    aes(xintercept = as.numeric(month)),
    color = "green", linetype = "dotted"
  ) +
  labs(
    title = "",
    x = "Month",
    y = "Number of negative media attacks"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

ggsave("/Users/paulbachmann/Nextcloud/Amlo_paper/3_Output/month_plot.png", month_plot, width = 10, height = 10, dpi = 500)

###############################
# COMPARISON OF MODELS
###############################

# Create a table to compare quarterly and monthly models
model_comparison <- data.frame(
  Frequency = c("Quarterly", "Quarterly", "Monthly", "Monthly"),
  Model = c("ARIMA", "Negative Binomial", "ARIMA", "Negative Binomial"),
  AIC = c(
    AIC(quarterly_bt_model),
    AIC(quarterly_nb),
    AIC(monthly_bt_model),
    AIC(monthly_nb)
  ),
  BIC = c(
    BIC(quarterly_bt_model),
    BIC(quarterly_nb),
    BIC(monthly_bt_model),
    BIC(monthly_nb)
  )
)

print(model_comparison)


###############################
#Reach (Views) and Reactions (Likes)
###############################

# Count the number of negative media attacks per video
attack_counts <- amlo_sentences %>%
  filter(sentiment == "negative") %>%
  count(video_id, name = "negative_attacks")

# Merge attack data with video metadata
video_level <- amlo %>%
  left_join(attack_counts, by = "video_id") %>%
  mutate(
    negative_attacks = replace_na(negative_attacks, 0)
  )

cor_matrix <- video_level %>%
  select(view_count, like_count, negative_attacks) %>%
  cor(use = "complete.obs")

print(cor_matrix)

####################################
# Plot: Negative media attacks vs Views
####################################

media_attac_view <- ggplot(video_level, aes(x = negative_attacks, y = view_count)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", color = "darkblue") +
  scale_y_continuous(labels = comma) +
  labs(
    title = "Media Attacks and YouTube Views",
    x = "Number of Negative Media Attacks",
    y = "Video Views"
  ) +
  theme_minimal()
ggsave("/Users/paulbachmann/Nextcloud/Amlo_paper/3_Output/media_attac_view .png", media_attac_view, width = 10, height = 10, dpi = 500)

####################################
# Poisson regression models for views and likes
####################################

# Model 1: Views
views_poisson <- glm(
  view_count ~ negative_attacks + duration,
  data = video_level,
  family = poisson()
)
summary(views_poisson)

# Model 2: Likes
likes_poisson <- glm(
  like_count ~ negative_attacks + duration,
  data = video_level,
  family = poisson()
)
summary(likes_poisson)

####################################
# Log-transformed OLS model
####################################

video_level <- video_level %>%
  mutate(
    log_views = log1p(view_count),
    log_likes = log1p(like_count)
  )

views_lm <- lm(log_views ~ negative_attacks + duration, data = video_level)
summary(views_lm)

likes_lm <- lm(log_likes ~ negative_attacks + duration, data = video_level)
summary(likes_lm)


####################################
# Engagement Rate (Likes per View)
####################################

video_level <- video_level %>%
  mutate(engagement_rate = like_count / view_count)

video_level_plot <- ggplot(video_level, aes(x = negative_attacks, y = engagement_rate)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", color = "darkgreen") +
  labs(
    title = "Negative Media Attacks and Engagement Rate",
    x = "Number of Negative Media Attacks",
    y = "Engagement Rate (Likes / Views)"
  ) +
  theme_minimal()
ggsave("/Users/paulbachmann/Nextcloud/Amlo_paper/3_Output/video_level_plot.png", video_level_plot, width = 10, height = 10, dpi = 500)


# Model for engagement rate
engagement_lm <- lm(engagement_rate ~ negative_attacks + duration, data = video_level)
summary(engagement_lm)

####################################
#  Binary indicator for attack presence
####################################
video_level <- video_level %>%
  mutate(
    log_views = log1p(view_count)
  )

video_level <- video_level %>%
  mutate(has_attack = as.integer(negative_attacks > 0))

views_dummy_lm <- lm(log_views ~ has_attack + duration, data = video_level)
summary(views_dummy_lm)


####################################
# Monthly Aggregation and Join with Approval Ratings
####################################

monthly_summary <- video_level %>%
  mutate(month = floor_date(upload_date, "month")) %>%
  group_by(month) %>%
  summarise(
    total_attacks = sum(negative_attacks),
    avg_views = mean(view_count, na.rm = TRUE),
    avg_likes = mean(like_count, na.rm = TRUE),
    n_videos = n()
  ) %>%
  left_join(monthly_approval_ts, by = c("month" = "period"))

# Plot: Attacks, Views and Approval combined
Attacks_Views_Approval  <- ggplot(monthly_summary, aes(x = month)) +
  geom_col(aes(y = total_attacks * 1000), fill = "red", alpha = 0.3) +
  geom_line(aes(y = avg_views), color = "blue", size = 1) +
  geom_line(aes(y = mean_approval * 10000), color = "green", linetype = "dashed", size = 1) +
  scale_y_continuous(
    name = "Avg Views / Total Attacks (scaled)",
    sec.axis = sec_axis(~ . / 10000, name = "Approval Rating (%)")
  ) +
  labs(
    title = "Media Attacks, Views and Approval Rating per Month",
    x = "Month"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("/Users/paulbachmann/Nextcloud/Amlo_paper/3_Output/Attacks_Views_Approval.png", Attacks_Views_Approval, width = 10, height = 10, dpi = 500)


###############################
# Descriptive Plots
###############################

# Mañaneras numbers per month
mananeras_per_month <- amlo %>%
  mutate(month = floor_date(upload_date, unit = "month")) %>%
  count(month, name = "number_of_mananeras")

# month barchart
monthly_plot <- ggplot(mananeras_per_month, aes(x = month, y = number_of_mananeras)) +
  geom_col(fill = "darkred", alpha = 0.8) +
  geom_text(aes(label = number_of_mananeras), vjust = -0.5, size = 3) +
  labs(
    title = "",
    x = "Month",
    y = "Numbers of Mañaneras"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

#save plot
ggsave(
  "/Users/paulbachmann/Nextcloud/Amlo_paper/3_Output/monthly_time_plot.png",
  monthly_plot,
  width = 10,
  height = 6,
  dpi = 500
)

###############################
# Wordcloud 
###############################

# Tokenisierung, Stopwörter entfernen
amlo_tokens <- amlo %>%
  unnest_tokens(word, transcript) %>%
  anti_join(spanish_stopwords, by = "word") %>%
  filter(!str_detect(word, "^[0-9]+$"), nchar(word) > 1)

# Top 200 häufigste Wörter
word_counts <- amlo_tokens %>%
  count(word, sort = TRUE) %>%
  slice_head(n = 200)

# create quanteda Wordcloud 
word_corpus <- corpus(paste(word_counts$word, collapse = " "))
tokens_wc <- tokens(word_corpus)
dfm_wc <- dfm(tokens_wc)

# save as png 
png("/Users/paulbachmann/Nextcloud/Amlo_paper/3_Output/wordcloud_plot.png", width = 1000, height = 1000, res = 150)
textplot_wordcloud(
  dfm_wc,
  min_count = 1,
  max_words = 200,
  random_order = FALSE,
  rot_per = 0.1,
  color = RColorBrewer::brewer.pal(8, "Dark2")
)
dev.off()