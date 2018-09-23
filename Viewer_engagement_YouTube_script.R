##--------------------------------------------------------------------------------------------------##
##           VIEWER ENGAGEMENT ON YOUTUBE: MINING DATA ON THE FLORIDA HIGH SCHOOL SHOOTING          ##
##--------------------------------------------------------------------------------------------------##


## R version 3.4.3 (2017-11-30)

## Authors: Lisa Hehnke (dataplanes.org | @DataPlanes) & Josef Holnburger (holnburger.com | @holnburger)


#-------#
# Setup #
#-------#

# Install and load packages using pacman
if (!require("pacman")) install.packages("pacman")
library(pacman)

p_load(lubridate, reshape2, RMySQL, rvest, tidyverse, tidytext, urltools, wordcloud, xml2)


#-------------------------#
# Connect to SQL database #
#-------------------------#

con <- dbConnect(MySQL(), user = "[INSERT USER HERE]", password = "[INSERT PASSWORD HERE]", dbname = "[INSERT NAME HERE]", host = "[INSERT HOST HERE]")
sqlsetutf8 <- dbGetQuery(con, "SET NAMES utf8mb4")


#---------------#
# Download data #
#---------------#

query <- dbSendQuery(con, "SELECT * FROM Videos")
videos <- dbFetch(query, n = -1)

query <- dbSendQuery(con, "SELECT * FROM Comments")
comments <- dbFetch(query, n = -1)


#--------------------------#
# Theme for visualizations #
#--------------------------#

viz_theme <- theme(
  strip.background = element_rect(colour = "transparent", fill = "grey90"),
  axis.line = element_line(colour = "black"),
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  panel.border = element_blank(),
  panel.background = element_blank(),
  legend.key = element_rect(fill = "white"),
  strip.text = element_text(size = rel(1), face = "bold"),
  plot.caption = element_text(colour = "grey50"),
  text = element_text(family = "Avenir"))


#----------------#
# Data wrangling #
#----------------#

# Search terms for shooting-related videos
shootingTerms <- "florida|high school|shooting|nra"

# Clean video data
videos <- videos %>%
  select(videoId, publishedAt, channelId, channelTitle, title, description, viewCount, likeCount, dislikeCount, commentCount, liveBroadcastContent) %>%
  mutate(datePublished = as.Date(publishedAt)) %>%
  mutate(freqLikes = likeCount/(likeCount+dislikeCount)) %>%
  mutate(freqDislikes = dislikeCount/(likeCount+dislikeCount)) %>%
  arrange(desc(viewCount)) %>%
  mutate(aboutShooting = case_when(
    str_detect(tolower(description), shootingTerms) == TRUE | 
      str_detect(tolower(title), shootingTerms) == TRUE ~ "TRUE",
    str_detect(tolower(description), shootingTerms) == FALSE |
      str_detect(title, shootingTerms) == FALSE ~ "FALSE"))

# Change channel title
videos$channelTitle[videos$channelTitle == "The Alex Jones Channel"] <- "Alex Jones" 

# Add function
"%notin%" <- Negate("%in%")


#-------#
# Stats #
#-------#

# Video summary
videos_summary <- videos %>%
  group_by(channelTitle, aboutShooting) %>%
  summarise(Videos = n(), meanViews = mean(viewCount), meanFreqLikes = mean(freqLikes), meanFreqDislikes = mean(freqDislikes))

# Get total number of comments by authorChannelId
users <- comments %>%
  group_by(authorChannelId) %>%
  summarise(comments = n()) %>%
  arrange(desc(comments))

# Get number of comments on shooting-related videos by authorChannelId
users_shooting <- comments %>%
  left_join(videos %>% select(videoId, aboutShooting), by = "videoId") %>%
  filter(aboutShooting == TRUE) %>%
  group_by(authorChannelId) %>%
  summarise(comments = n()) %>%
  arrange(desc(comments))

# List of shooting-related videos
videos_shooting <- videos %>%
  filter(aboutShooting == TRUE)


#-------#
# Plots #
#-------#

# Number of shooting-related videos over time by channel
videos %>%
  filter(aboutShooting == TRUE) %>%
  ggplot(aes(x = datePublished, colour = channelTitle)) + geom_freqpoly(bins = "14") +
  viz_theme

# Number of shooting-related videos compared to the rest of the videos
videos_summary %>%
  ggplot(aes(channelTitle, Videos)) +   
  geom_bar(aes(fill = aboutShooting), position = "dodge", stat = "identity") + 
  theme(axis.title.x = element_blank()) +
  ggtitle("Number of shooting-related vs. other videos", subtitle = "") + labs(fill = "Shooting-related") +  
  scale_fill_discrete(labels = c("No", "Yes")) + 
  theme(text = element_text(size = 20)) + 
  viz_theme

ggsave("plot_videos.png", width = 12, height = 12, units = "in", dpi = 100)

# Views of shooting-related videos
p <- videos %>%
  filter(aboutShooting == TRUE) %>%
  ggplot(aes(channelTitle, viewCount, fill = channelTitle)) + geom_boxplot() +
  theme(axis.title.x = element_blank(), legend.position = "NONE") +
  ggtitle("Views of shooting-related videos by channel", subtitle = "") + ylab("Views") +
  viz_theme

require(scales)
p + scale_y_continuous(labels = comma)

# Views of shooting-related vs. other videos by channel
p_cat <- videos %>% 
  ggplot(aes(channelTitle, viewCount, fill = channelTitle)) + geom_boxplot() +
  theme(axis.title.x = element_blank(), legend.position = "NONE") +
  ggtitle("Number of video views by channel", subtitle = "") +
  facet_wrap(~aboutShooting, labeller = as_labeller(c("FALSE" = "Not shooting-related", "TRUE" = "Shooting-related"))) + ylab("Views") +
  theme(text = element_text(size = 20)) + 
  viz_theme

require(scales)
p_cat + scale_y_continuous(labels = comma)

ggsave("plot_views.png", width = 14, height = 10, units = "in", dpi = 100)

# Overall percentage of likes by channel
videos %>% 
  ggplot(aes(channelTitle, freqLikes, fill = channelTitle)) + geom_boxplot() +
  theme(axis.title.x = element_blank(), legend.position = "NONE") +
  ggtitle("Percentage of likes by channel", subtitle = "") + ylab("% Likes") +
  viz_theme

# Percentage of likes on shooting-related vs. other videos by channel
videos %>% 
  ggplot(aes(channelTitle, freqLikes, fill = channelTitle)) + geom_boxplot() +
  theme(axis.title.x = element_blank(), legend.position = "NONE") +
  ggtitle("Percentage of likes by channel", subtitle = "") +
  facet_wrap(~aboutShooting, labeller = as_labeller(c("FALSE" = "Not shooting-related", "TRUE" = "Shooting-related"))) + ylab("% Likes") +
  theme(text = element_text(size = 20)) + 
  viz_theme

ggsave("plot_likes.png", width = 14, height = 10, units = "in", dpi = 100)


#-----------------#
# Text processing #
#-----------------#

# Encode HTML characters and remove them
## Thanks to Jeroen (https://stackoverflow.com/questions/5060076/convert-html-character-entity-encoding-in-r).
unescape_html <- function(str){
  xml2::xml_text(xml2::read_html(paste0("<x>", str, "</x>")))
}
comments$text_clean <- lapply(comments$text, unescape_html)

# Remove URLs
url_pattern <- "http[s]?://(?:[a-zA-Z]|[0-9]|[$-_@.&+]|[!*\\(\\),]|(?:%[0-9a-fA-F][0-9a-fA-F]))+"
www_pattern <- "www\\S+\\s*"

comments %<>%
  mutate(text_clean = stringr::str_replace_all(text_clean, url_pattern, "")) %>%
  mutate(text_clean = stringr::str_replace_all(text_clean, www_pattern, "")) 

# Remove emojis
comments$text_clean <- iconv(comments$text_clean, to = "UTF-8-MAC", sub = "byte")

# Convert to lowercase
comments$text_clean <- tolower(comments$text_clean)

# Remove punctuation and numbers
comments$text_clean <- gsub("[^[:alpha:][:blank:]]", "", comments$text_clean)

# Merge data
comments_shooting <- comments %>%
  left_join(videos %>% select(videoId, channelTitle, aboutShooting), by = "videoId") %>%
  filter(aboutShooting == TRUE)

# Sample comments
set.seed(42)
comments_sample <- comments_shooting %>%
  group_by(channelTitle) %>%
  sample_n(1000)

# Unnest and tokenize text and remove stop words
comments_sample_tidy <- comments_sample %>%
  unnest_tokens(word, text_clean) %>%
  anti_join(stop_words)


#--------------------#
# Comments over time #
#--------------------#

# Count comments by date
comments_ts <- comments_shooting %>% 
  group_by(channelTitle) %>%
  dplyr::count(date = as.Date(publishedAt))

# Plot timeline
ggplot(comments_ts, aes(date, n)) + 
  geom_line(col = "red", size = 1) +
  facet_wrap(~channelTitle, scales = "free_y", ncol = 3) +
  labs(x = "Date", y = "Count", title = "Number of YouTube comments over time", subtitle = " ") +
  scale_x_date(date_breaks = "1 day", date_labels = "%b %d") + 
  theme(text = element_text(size = 20)) + 
  viz_theme + ylim(0, 100000) + theme(axis.text.x = element_text(angle = 65, vjust = 0.5))

ggsave("plot_timeline.png", width = 12, height = 8, units = "in", dpi = 100)


#-----------------#
# Views over time #
#-----------------#

# Views per date (without CNN)
views_ts <- videos %>% 
  group_by(channelTitle) %>%
  filter(channelTitle != "CNN") %>%
  filter(aboutShooting == TRUE) %>%
  dplyr::count(date = as.Date(publishedAt))

# Plot timeline
ggplot(views_ts, aes(date, n)) + 
  geom_line(col = "red", size = 1) +
  facet_wrap(~channelTitle, scales = "free_y", ncol = 2) +
  labs(x = "Date", y = "Count", title = "Views of YouTube videos over time", subtitle = " ") +
  scale_x_date(date_breaks = "1 day", date_labels = "%b %d") + 
  theme(text = element_text(size = 20)) + 
  viz_theme + theme(axis.text.x = element_text(angle = 65, vjust = 0.5))


#------------------#
# Word frequencies #
#------------------#

comments_tidy <- comments_shooting %>%
  unnest_tokens(word, text_clean) %>%
  anti_join(stop_words)

comments_wordfreq <- comments_tidy %>%
  dplyr::count(word, sort = TRUE)

# Plot words
comments_tidy %>%
  group_by(channelTitle) %>%
  dplyr::count(word, sort = TRUE) %>%
  top_n(6, n) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) +
  geom_col() +
  facet_wrap(~channelTitle, scales = "free_y", ncol = 3) +
  theme(text = element_text(size = 25)) + 
  xlab("") + ylab("") + ggtitle("Most common words in YouTube comments", subtitle = " ") + theme(axis.text.x = element_text(angle = 65, vjust = 0.5)) + 
  coord_flip() + viz_theme 

ggsave("plot_words.png", width = 12, height = 8, units = "in", dpi = 100)

# Wordcloud
comments_tidy %>%
  count(word) %>%
  with(wordcloud(word, n, max.words = 100, random.order = FALSE))


#------------------#
# Comparison cloud #
#------------------#

comments_tidy %>%
  filter(channelTitle == "Alex Jones") %>%
  count(word) %>%
  with(wordcloud(word, n, max.words = 100, random.order = FALSE))

comments_sample_tidy %>%
  count(word, channelTitle, sort = TRUE) %>%
  acast(word ~ channelTitle, value.var = "n", fill = 0) %>%
  comparison.cloud(max.words = 200, random.order = FALSE, title.size = 1.4)


#--------------------#
# Sentiment analysis #
#--------------------#

# Remove trump
senti_rm <- c("trump")

# Calculate and plot total sentiment scores (nrc)
comments_sample_tidy %>%
  filter(word %notin% senti_rm) %>%
  group_by(channelTitle) %>%
  inner_join(get_sentiments("nrc")) %>%
  count(word, sentiment) %>%
  ggplot(aes(sentiment, n)) +
  geom_bar(aes(fill = sentiment), stat = "identity") +
  facet_wrap(~channelTitle, scales = "free_y", ncol = 2) +
  theme(text = element_text(size = 30), axis.text.x = element_text(angle = 65, vjust = 0.5)) +
  xlab("") + ylab("") + ggtitle("Total sentiment scores in YouTube comments (nrc)", subtitle = " ") +
  ylim(0, 2500) + theme(legend.position = "none") + viz_theme 

ggsave("plot_sentiments.png", width = 12, height = 8, units = "in", dpi = 100)


#----------------------#
# Sentiments over time #
#----------------------#

comments_tidy$publishedAt <- ymd_hms(comments_tidy$publishedAt)

# Calculate and plot sentiment scores (nrc) over time
comments_sent_ts <- comments_tidy %>%
  filter(word %notin% senti_rm) %>%
  group_by(channelTitle) %>%
  inner_join(get_sentiments("nrc")) %>%
  dplyr::count(Date = as.Date(publishedAt), sentiment)

ggplot(comments_sent_ts, aes(Date, n, group = sentiment)) +
  geom_line(size = 1, alpha = 0.7, aes(color = sentiment)) +
  facet_wrap(~channelTitle, scales = "free_y", ncol = 2) +
  theme(text = element_text(size = 30), axis.text.x = element_text(angle = 65, vjust = 0.5)) +
  scale_x_date(date_breaks = "1 day", date_labels = "%b %d") + 
  xlab("") + ylab("") + ggtitle("Total sentiment scores in YouTube comments", subtitle = " ") +
  ylim(0, 30000) + viz_theme 

ggsave("plot_sentiments_timeline.png", width = 12, height = 8, units = "in", dpi = 100)


#-------------------------#
# Positive/negative words #
#-------------------------#

# Calculate positive and negative sentiments (bing)  
bing_counts <- comments_tidy %>%
  filter(word %notin% senti_rm) %>%
  group_by(channelTitle) %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup()

# Calculate top word contributors 
bing_counts_plot <- bing_counts %>%
  group_by(channelTitle, sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) 

# Plot most common positive and negative words
ggplot(bing_counts_plot, aes(word, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  xlab("") + ylab("") + 
  theme(text = element_text(size = 30)) + 
  ggtitle("Most common +/- words in YouTube comments", subtitle = " ") +
  coord_flip() + viz_theme

ggsave("plot_pos_neg_words.png", width = 12, height = 8, units = "in", dpi = 100)


#---------------------#
# Most liked comments #
#---------------------#

# Get most active commenters
most_active <- comments_shooting %>%
  group_by(authorChannelId) %>%
  tally()  %>%
  arrange(-n, authorChannelId)

# Get most active commenters: same text
most_active_text <- comments_shooting %>%
  group_by(authorChannelId) %>%
  dplyr::count(text_clean, sort = TRUE)


#---------------------#
# Most liked comments #
#---------------------#

# Get most liked comments
most_liked <- comments_shooting %>%
  group_by(channelTitle) %>%
  top_n(100, likeCount) %>%
  arrange(channelTitle, -likeCount)

# Tokenize text and remove stop words
most_liked_tidy <- most_liked %>%
  unnest_tokens(word, text_clean) %>%
  anti_join(stop_words) %>%
  filter(word %notin% words_rm) 


#--------------------------------#
# Scrape profanity list from web #
#--------------------------------#

# Create URL for each letter
letters <- paste(letters)
urls <- paste0("https://www.noswearing.com/dictionary/", letters) 

# Function for scraping tables
get_words <- function(url) {
  url %>%
    read_html() %>%
    html_nodes("td") %>% 
    html_nodes("a") %>% 
    html_attr("name") %>%
    na.omit()
} 

# Get tables and convert to data frame
profanity <- unlist(lapply(urls, get_words))

# Clean words
profanity <- gsub("\\\\'.*", "", profanity)


#----------------#
# Comment length #
#----------------#

## Idea adapted from: https://www.curiousgnu.com/youtube-comments-text-analysis

# Count words per comment
comments_sample$number_words <- vapply(strsplit(comments_sample$text, "\\W+"), length, integer(1))


#-----------------#
# Comment quality #
#-----------------#

# Count number of profane words
comments_sample$number_prof <- str_count(comments_sample$text, paste0(c("\\b("), paste(profanity, collapse = "|"), c(")\\b")))

# Get profanities to words rate
comments_sample$prof_rate <- (comments_sample$number_prof / comments_sample$number_words) * 100

# View profanity rate by category
ggplot(comments_sample, aes(prof_rate, colour = channel)) + 
  geom_freqpoly(bins = 100) + 
  labs(x = "Rate", y = "Count", title = "Profanity rate for YouTube comments", subtitle = "Profanities to words rate",
       colour = "Channel") +
  theme(text = element_text(size = 20)) + 
  viz_theme