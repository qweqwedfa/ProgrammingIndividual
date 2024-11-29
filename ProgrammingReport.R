raw_df <- read.csv('/Users/yanhanjun/Desktop/ProgrammingReport/delhivery_data.csv')
#head(raw_df)
library(dplyr)

# drop rows with null and duplicates
raw_df <- na.omit(raw_df)
raw_df <- distinct(raw_df)
head(raw_df)

# convert datetime
library(lubridate)

time_columns <- c('trip_creation_time', 'od_start_time', 'od_end_time', 'cutoff_timestamp')
for (col in time_columns) {
  raw_df[[col]] <- parse_date_time(raw_df[[col]], orders = "ymd HMS") 
}
summary(raw_df)
# observe the category of every columns
sapply(raw_df, class)

colSums(is.na(raw_df))
# split the train and testlabels
train_df <- raw_df[raw_df$data == "training", ]
test_df <- raw_df[raw_df$data == "test", ]

# the counts of being Source and Des
Source_dict <- table(train_df$source_center)
Des_dict <- table(train_df$destination_center)
head(Source_dict)
merged_dict <- list()

# merge the source and des
for (center in names(Source_dict)) {
  merged_dict[[center]] <- c(Source_dict[center], 0)  
}
head(merged_dict)
# following the processing in Python
for (center in names(Des_dict)) {
  if (center %in% names(merged_dict)) {
    merged_dict[[center]] <- c(merged_dict[[center]][1], Des_dict[center])
  } else {
    merged_dict[[center]] <- c(0, Des_dict[center])
  }
}
head(merged_dict)
# Convert the merged list into a data frame
Source_and_des <- as.data.frame(do.call(rbind, merged_dict))

# Name the columns
colnames(Source_and_des) <- c('NumAsSource', 'NumAsDes')
head(Source_and_des)
# Add the 'CenterID' column (which is the row names)
Source_and_des$CenterID <- rownames(Source_and_des)
head(Source_and_des)
# Reorder columns to match the desired output
Source_and_des <- Source_and_des[, c('CenterID', 'NumAsSource', 'NumAsDes')]
head(Source_and_des)

# staring improvment on visualization
library(ggplot2)

top_source <- Source_and_des %>%
  arrange(desc(NumAsSource)) %>%
  head(20)

top_des <- Source_and_des %>%
  arrange(desc(NumAsDes)) %>%
  head(20)

library(gridExtra)
print(top_source)
print(top_des)
# Barplot for NumAsSource
plot1 <- ggplot(top_source, aes(x = reorder(CenterID, -NumAsSource), y = NumAsSource, fill = CenterID)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  theme_minimal() +
  labs(title = "Top 20 Centers by NumAsSource", x = "CenterID", y = "NumAsSource") +
  theme(legend.position = "none")
plot1
# Barplot for NumAsDes
plot2 <- ggplot(top_des, aes(x = reorder(CenterID, -NumAsDes), y = NumAsDes, fill = CenterID)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  theme_minimal() +
  labs(title = "Top 20 Centers by NumAsDes", x = "CenterID", y = "NumAsDes") +
  theme(legend.position = "none")
plot2
# Combine the two plots into one figure
grid.arrange(plot1, plot2, ncol = 2)

head(top_des)

