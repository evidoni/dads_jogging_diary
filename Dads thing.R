# Install necessary packages if not already installed
required_packages <- c("tm", "wordcloud", "tidytext", "dplyr", "lubridate", "tools", "stringr", "tidyverse")
new_packages <- required_packages[!(required_packages %in% installed.packages()[,"Package"])]
if(length(new_packages)) install.packages(new_packages)

# Load necessary libraries
library(tm)
library(wordcloud)
library(tidytext)
library(dplyr)
library(lubridate)
library(tools)
library(stringr)
library(tidyverse)
library(ggplot2)
library(ggwordcloud)
library(grid)
library(gridExtra)
library(png)
library(gtable)
library(patchwork)


########## Main script #####
#file_path <- "C:/Users/vidon/Downloads/24 May 26,2024 RUN DIARY copy 3.txt"
file_path <- "~/Downloads/24 May 26,2024 RUN DIARY copy 3.txt"
data <- readLines(file_path)
data <- data[nzchar(data)]
df <- data.frame(stringsAsFactors = FALSE)


### Get the date  ####
for (line in 1:length(data)) {
  print(line)
  date_part <- gsub(" ", "\\\\", substr(data[line], 1, 10))
  
  
  # Split date parts
  date_parts <- strsplit(date_part, "\\\\")[[1]]
  date_parts <- gsub("[a-zA-Z[:punct:]]", "", date_parts)
  date_parts <- date_parts[nzchar(date_parts)]
  
  # Ensure zero-padding
  month <- sprintf("%02d", as.numeric(date_parts[1]))
  day <- sprintf("%02d", as.numeric(date_parts[2]))
  year <- as.numeric(date_parts[3])
  
  # Determine century
  if(year > 1000){
    year <- year
  } else if (year > 70) {
    year <- paste0("19", sprintf("%02d", year))
  } else {
    year <- paste0("20", sprintf("%02d", year))
  }
  
  
  # Combine into a date
  date <- as.Date(paste(year, month, day, sep = "-"))
  
  # Extract note
  note <- substr(data[line], 6, nchar(data[line]))
  note <-   trimws(gsub('[^a-zA-Z\\s]|', "", note, perl = TRUE), 'both')
  note <- toTitleCase(note)
  
  df <- rbind(df, data.frame(date = date, text = note, original = data[line]))
}

df$year <- year(df$date)


# Custom function to convert seconds to mm:ss format
seconds_to_mmss <- function(seconds) {
  minutes <- floor(seconds / 60)
  seconds <- round(seconds %% 60)
  sprintf("%02d:%02d", minutes, seconds)
}

#### Try to get the time. #####

# Function to split based on a variable pattern
split_based_on_pattern <- function(string, pattern) {
  str_split(string, pattern, simplify = TRUE)
}

df2 <- df %>%
  mutate(two_yr =  as.character(str_sub(year, 3,4))) %>%
  rowwise() %>%
  mutate(splits = list(split_based_on_pattern(original, two_yr))) %>%
  mutate(a = splits[1], b = splits[2]) %>%
  ungroup() %>%
  select(-splits, -two_yr) %>%
  mutate(a = map_chr(a, ~ ifelse(length(.) > 0, ., NA_character_)),
         b = map_chr(b, ~ ifelse(length(.) > 0, ., NA_character_))) %>%
  select(-a) 

# Extract the first two sets of numbers from column 'b'
df3 <- df2 %>%
  mutate(c = gsub("[a-zA-Z]",'', b)) %>%
  mutate(d = trimws(gsub('[[:punct:]]', ' ', c), 'both')) %>%
  separate(., col = d, into = c('a1','a2','a3'), sep = '\\s') %>%
  mutate(a1 = str_sub(a1, 1,4)) #%>%
#mutate_at(vars('a1','a2','a3'), ~ as.numeric(.)) 


for (i in 1:nrow(df3)){
  #print(i)
  
  if (is.na(df3$a1[i]) | df3$a1[i] == '') {
    #print('do nothing')
  } else  if (as.numeric(df3$a1[i]) >= 7000){
    df3$a1[i] <- str_sub(df3$a1[i],1,1)
    df3$a2[i] <- str_sub(df3$a1[i],2,3)
  } else if( as.numeric(df3$a1[i]) >2700  & as.numeric(df3$a1[i]) < 7000){
    df3$a1[i] <- NA
    df3$a2[i] <- NA
  } else if ( as.numeric(df3$a1[i]) >= 1000 & as.numeric(df3$a1[i]) <= 2700){
    df3$a1[i] <- str_sub(df3$a1[i],1,2)
    df3$a2[i] <- str_sub(df3$a1[i],2,4)
  } else if ( as.numeric(df3$a1[i]) >= 100 & as.numeric(df3$a1[i]) <= 1000){
    df3$a1[i] <- str_sub(df3$a1[i],1,1)
    df3$a2[i] <- str_sub(df3$a1[i],2,3)
  } else {
    #print('do nothing 2')
  }
  
  
  if (is.na(df3$a2[i]) | df3$a2[i] == '') {
    df3$a2[i] <- 0
  } else  if (as.numeric(df3$a2[i]) >= 100){
    df3$a2[i] <- str_sub(df3$a2[i],1,2)
  } else if( as.numeric(df3$a2[i]) >59  ){
    df3$a2[i] <- str_sub(df3$a2[i],1,1)
  } else {
    # print('do nothing 2')
  }
  
}

df4 <- df3 %>%
  mutate_at(vars('a1','a2','a3'), ~ as.numeric(.)) %>%
  mutate(a2 = ifelse(a2 > 59, 0, a2)) %>%
  mutate(a1 = ifelse(a1 > 30, NA, a1)) %>%
  mutate(a2 = ifelse(is.na(a1), NA, a2)) %>%
  mutate(time_s = a1*60+a2) %>%
  mutate(year = format(date, "%Y"), 
         month = format(date, "%m")) %>%
  select(date, text, year, month, time_s ) %>%
  filter(year <= 2024 & year >= 1990)


df_duration <- df4 %>%
  filter(! is.na(time_s)) %>%
  filter(time_s > 420) %>%
  group_by(year, month) %>%
  summarise(mean_seconds = mean(time_s, na.rm = TRUE)) %>%
  mutate(minutes = mean_seconds %/% 60,
         rem_seconds = round(mean_seconds %% 60,0)) %>%
  mutate(time = paste0(minutes,':', str_pad(rem_seconds, width = 2, side = 'left', pad = 0))) %>%
  mutate(time = gsub('60','59', time)) %>%
  mutate(date = paste(year,month, ifelse(month == '01', '01',
                                         ifelse(month == '12', '31','15')),sep='-')) %>%
  mutate(date = as.Date(date, format = '%Y-%m-%d'))


df_duration <- bind_rows(data.frame(year = '1990', month = '01',
                                    mean_seconds = NA, minutes = NA,
                                    rem_seconds = NA, 
                                    time = NA, date =  NA), df_duration)
rm(df, df2, df3)


#### Process words #####
# Create a dataframe to store words, year, frequency, and valence
word_data <- data.frame(stringsAsFactors = FALSE)

# Preprocess text and assign valence

sentiments <- get_sentiments("bing")


word_freq <- df4 %>%
  mutate(text = tolower(text)) %>%
  mutate(text = removePunctuation(text)) %>%
  mutate(text = removeWords(text, stopwords("en"))) %>%
  select(date, year, month, time_s, text) %>%
  separate(text, into = as.vector(outer(letters, c(1:4), paste, sep = "")), sep = "\\s") %>%
  pivot_longer(., cols = -c('date', 'year', 'month', 'time_s')) %>%
  rename(., word = 'value') %>%
  filter(!grepl('^min$|^thur$|^mon$|^tu$|^tue$|^wed$|^th$|^thu$|^fri$|^s$|^sa$|^sat$|^su$|^abt$|^mx$|^m$|^avg$|^av$',word)) %>%
  filter(!is.na(word) & word != '') %>%
  as_tibble(.) %>%
  mutate(word = ifelse(grepl('^il$|^ill$|^illi$|^illin$|^illiniois',word),'illini',word)) %>%
  mutate(word = ifelse(grepl('^uof$|ui',word),"UofI",word)) %>%
  mutate(word = ifelse(grepl('^mu$|^marq$',word),'marquette',word)) %>%
  mutate(word = ifelse(grepl('^ac$',word),'AC',word)) %>%
  filter(!grepl('^av$|^avg$|^error$|^st$|^sta$',word)) %>%
  mutate(word = ifelse(grepl('^havent$',word),"haven't",word)) %>%
  group_by(word, year) %>%
  summarise(freq = n()) %>%
  left_join(., sentiments, by = "word") %>%
  mutate(sentiment = ifelse(is.na(sentiment),'neutral', sentiment))  %>%
  filter(freq > 1) %>%
  filter(nchar(word) > 1) %>%
  filter(!grepl('^av$|^avg$|^error$|^st$|^sta$',word)) %>%
  mutate(word = ifelse(grepl('^havent$',word),"haven't",word)) %>%
  rowwise() %>%
  mutate(word = str_to_title(word)) %>%
  ungroup() %>%
  mutate(word = ifelse(grepl('^Ac$',word),'AC',word)) %>%
  mutate(word = ifelse(grepl('^im$',word),"I'm",word)) %>%
  mutate(word = ifelse(grepl('^Uofi$',word),"UofI",word)) %>%
  mutate(year = as.numeric(year)) %>%
  mutate(color = ifelse(sentiment == "positive", "#FF5F05", 
                        ifelse(sentiment == "neutral","#9BA7AA","#13294B"))) %>%
  mutate(rotation = sample(c(0, 45, 90, 270), n(), replace = TRUE))


# Calculate the maximum frequency to use consistent scaling
max_freq <- max(word_freq$freq, na.rm = TRUE)
peak_words <- word_freq %>%
  group_by(year) %>%
  summarise(count = n()) %>%
  slice_max(count) %>%
  pull(count)


yr_seq <- seq(min(word_freq$year), max(word_freq$year))

for( i in 1:length(yr_seq)){
  
  #i = 10
  tmp_yr <- yr_seq[i]
  tmp <- word_freq %>%
    filter(year == tmp_yr)

  x <- ggplot(tmp, aes(label = word, size = sqrt(freq), color = sentiment, angle = rotation)) +
    geom_text_wordcloud_area(rm_outside = FALSE) +
    scale_size_area(max_size = 5, limits = c(1, sqrt(max_freq))) +
    scale_color_manual(values = c("positive" = "#FF5F05", "neutral" = "#9BA7AA", "negative" = "#13294B")) +
    theme_minimal() +
    theme(plot.title = element_text(size = 20, hjust = 0.5), # Customizing title size and alignment
          axis.text.x = element_text(size = 10),
          axis.text.y = element_text(size = 10),
          panel.grid.major = element_line(color = "lightgrey"),
          panel.grid.minor = element_line(color = "lightgrey")) +
    ggtitle(tmp_yr) 
  
  
  assign(paste0('plot_',tmp_yr), x)
  
  rm(tmp, tmp_yr, x)
}

i = 1
#common_x_limits <- range(df_duration$date, na.rm = TRUE)
common_y_limits <- range(df_duration$mean_seconds, na.rm = TRUE)

for( i in 1:6 ){
  
  tmp_yr <- yr_seq[seq(i*6-5,i*6)]
  tmp <- df_duration %>%
    filter(year %in% tmp_yr)
  
  # Define the x-axis limits to cover half a month before the start and after the end of the range
  x_min <- min(tmp$date) - months(0)
  x_max <- max(tmp$date) + months(0)
  
  duration_plot <- ggplot(tmp, aes(x = date, y = mean_seconds)) +
    geom_line() +
    scale_y_continuous(labels = scales::trans_format("identity", seconds_to_mmss), 
                       limits = common_y_limits, 
                       breaks = pretty(common_y_limits, 2)) +
    scale_x_date(limits = c(x_min, x_max), expand = c(0,0)) +
    labs( x = "", y = "Run Duration") +
    theme_minimal() +
    theme(#axis.text.x = element_text(size = 10),
          axis.text.x=element_blank(),
          axis.text.y = element_text(size = 10),
          panel.grid.major = element_line(color = "lightgrey"),
          panel.grid.minor = element_line(color = "lightgrey"))
  assign(paste0('dur_',tmp_yr[1]), duration_plot)
  rm(tmp, tmp_yr, duration_plot, x_min, x_max)
}


# Path to your map image
map_path <- "~/Downloads/map.png"  # Adjust the path accordingly

# Load the map image
map_image <- readPNG(map_path)


# Create a rasterGrob
map_grob <- rasterGrob(map_image, width = unit(3, "in"), height = unit(4, "in"))

# Create the textGrob
text_grob <- textGrob(
  "Diary of a Jogger",
  gp = gpar(fontface = c("bold"), fontsize = 24, col = "black"),  # Make the text bold, larger font size, and white color for visibility
  hjust = 0.5,  # Center horizontally
  vjust = 0.5  # Center vertically
)

# Combine the map_grob and text_grob
combined_grob <- grobTree(map_grob, text_grob)
combined_grob <- ggplotify::as.ggplot(combined_grob)

# row1 <- list(plot_1990, plot_1991, plot_1992, plot_1993, plot_1994, plot_1995)
# row3 <- list(plot_1996, plot_1997, plot_1998, plot_1999, plot_2000, plot_2001)
# row5 <- list(plot_2002, plot_2003, plot_2004, plot_2005, plot_2006, plot_2007)
# row7 <- list(plot_2008, plot_2009, plot_2010, plot_2011, plot_2012, plot_2013)
# row9 <- list(plot_2014, plot_2015, plot_2016, plot_2017, plot_2018, plot_2019)
# 
# # Add the combined grob to the plot list (span across rows 11 and 12)
# row11 <- list(plot_2020, plot_2021, plot_2022, plot_2023, plot_2024, combined_grob)
# 
# row2 <- list(dur_1990)
# row4 <- list(dur_1996)
# row6 <- list(dur_2002)
# row8 <- list(dur_2008)
# row10 <- list(dur_2014)
# row12 <- list(dur_2020)
# 
# plot_lists <- list(row1, row2, row3, row4, row5, row6, row7, row8, row9, row10, row11, row12)
# 
# # Flatten the list of lists for grid.arrange, ensuring single plots span the entire width
# all_plots <- list()
# for (i in seq_along(plot_lists)) {
#   all_plots <- c(all_plots, plot_lists[[i]])
# }
# 
# # Adjust layout matrix to span combined_grob across rows 11 and 12
# layout <- rbind(c(seq(1, 6)),
#                 c(rep(7, 6)),
#                 c(seq(8, 13)),
#                 c(rep(14, 6)),
#                 c(seq(15, 20)),
#                 c(rep(21, 6)),
#                 c(seq(22, 27)),
#                 c(rep(28, 6)),
#                 c(seq(29, 34)),
#                 c(rep(35, 6)),
#                 c(seq(36, 40), 41),
#                 c(rep(42, 5), 41))
# 
# heights <- c(rep(c(3, 1), 6))


# Combine the plots using patchwork
layout <- (
  (plot_1990 | plot_1991 | plot_1992 | plot_1993 | plot_1994 | plot_1995) / 
    (dur_1990) /
    (plot_1996 | plot_1997 | plot_1998 | plot_1999 | plot_2000 | plot_2001) / 
    (dur_1996) /
    (plot_2002 | plot_2003 | plot_2004 | plot_2005 | plot_2006 | plot_2007) / 
    (dur_2002) /
    (plot_2008 | plot_2009 | plot_2010 | plot_2011 | plot_2012 | plot_2013) / 
    (dur_2008) /
    (plot_2014 | plot_2015 | plot_2016 | plot_2017 | plot_2018 | plot_2019) / 
    (dur_2014) /
    (plot_2020 | plot_2021 | plot_2022 | plot_2023 | plot_2024 /dur_2020) | combined_grob) +
  plot_layout(ncol = 6, heights = c(rep(c(3, 1), 5), 4))

# Create the layout
pdf(file.path('~/Downloads', "large_layout.pdf"), width = 24, height = 25)
#grid.arrange(grobs = all_plots, ncol = 6, nrow = 12,
#             layout_matrix = layout,
#             heights = unit(heights, "in"))
print(layout)
dev.off()

