#this script is calculating the time between the rhythmic calls

wd <- "C:/Users/egrout/Dropbox/coaticalls/Galaxy_labels/completed_labels/"
plot_dir <- "C:/Users/egrout/Dropbox/coaticalls/results/"

setwd <- wd

#LIBRARIES
library(stringr)
library(hms)
library(tidyr)
library(dplyr)
library(ggplot2)
library(RColorBrewer)
library(ggthemes)
library(ggplot2)
library(gridExtra)

# get a list of all the CSV files in the folder
files <- list.files(wd, pattern = "*.csv")

#create data frame with 0 rows and 3 columns
all_data <- data.frame(matrix(ncol = 8, nrow = 0))
#provide column names
colnames(all_data) <- c("label","Start","Duration","Time","Format","Type","Description","file_name")

# loop through each CSV file and add it to the dataframe
for (i in 1:length(files)) {
  # read in the CSV data as a tibble
  # using header = TRUE assumes the first row of each CSV file is a header with column names
  file_data <- read.csv(paste0(wd, files[i]), header = T, sep="\t")
  
  # add a column with the row names (i.e. the name of the CSV file)
  file_data$file_name <- files[i]
  #only keeping necessary info of the file name
  file_data$file_name <- str_sub(file_data$file_name,end=10)
  colnames(file_data)[colnames(file_data) == "Name"] <- "label"
  
  # add the data to the all_data dataframe
  all_data <- rbind(all_data, file_data)
}

#remove rows which contain the date 
all_data <- all_data[!grepl(":", all_data$label),]


#remove unnecessary rows
all_data <- all_data[, -c(4:6)]

#add ID column
all_data$id <- str_sub(all_data$file_name,end=4)


#remove rows where number of characters in string is 8 or less because can't convert these to time format (and not in the last hour for comparison)
all_data <- all_data[(which(nchar(all_data$Start) > 9)),]

#then need to convert the start time to a time format
all_data$time <- as_hms(all_data$Start)

#need remove rows which are below the 2 hours, only keeping last hour
all_data <- all_data[(which(all_data$time > as.hms("02:00:00.000"))),]

#---------------------------------------------------------------------
#now cleaning labels and removing labels which are not calls
unique(all_data$label)
#remove labels
all_data_cleaned <- all_data

#removing labels not interested in:
all_data_cleaned <- all_data_cleaned[!grepl("unk", all_data_cleaned$label),]
all_data_cleaned <- all_data_cleaned[!grepl("scratch", all_data_cleaned$label),]
all_data_cleaned <- all_data_cleaned[!grepl("shake", all_data_cleaned$label),]
all_data_cleaned <- all_data_cleaned[!grepl("firework", all_data_cleaned$label),]
all_data_cleaned <- all_data_cleaned[!grepl("wood", all_data_cleaned$label),]
all_data_cleaned <- all_data_cleaned[!grepl("mov", all_data_cleaned$label),]
all_data_cleaned <- all_data_cleaned[!grepl("monkey", all_data_cleaned$label),]
all_data_cleaned <- all_data_cleaned[!grepl("sniff", all_data_cleaned$label),]
all_data_cleaned <- all_data_cleaned[!grepl("walk", all_data_cleaned$label),]
all_data_cleaned <- all_data_cleaned[!grepl("dig", all_data_cleaned$label),]
all_data_cleaned <- all_data_cleaned[!grepl("train", all_data_cleaned$label),]
all_data_cleaned <- all_data_cleaned[!grepl("walk", all_data_cleaned$label),]
all_data_cleaned <- all_data_cleaned[!grepl("drink", all_data_cleaned$label),]
all_data_cleaned <- all_data_cleaned[!grepl("breath", all_data_cleaned$label),]
all_data_cleaned <- all_data_cleaned[!grepl("hale", all_data_cleaned$label),]
all_data_cleaned <- all_data_cleaned[!grepl("run", all_data_cleaned$label),]
all_data_cleaned <- all_data_cleaned[!grepl("dolphin", all_data_cleaned$label),]
all_data_cleaned <- all_data_cleaned[!grepl("synch", all_data_cleaned$label),]
all_data_cleaned <- all_data_cleaned[!grepl("bird", all_data_cleaned$label),]
all_data_cleaned <- all_data_cleaned[!grepl("manakin", all_data_cleaned$label),]
all_data_cleaned <- all_data_cleaned[!grepl("pant", all_data_cleaned$label),]
all_data_cleaned <- all_data_cleaned[!grepl("vibrate", all_data_cleaned$label),]
all_data_cleaned <- all_data_cleaned[!grepl("chittering", all_data_cleaned$label),]
all_data_cleaned <- all_data_cleaned[!grepl("forag", all_data_cleaned$label),]
all_data_cleaned <- all_data_cleaned[!grepl("fart", all_data_cleaned$label),]
all_data_cleaned <- all_data_cleaned[!grepl("buzz", all_data_cleaned$label),]
all_data_cleaned <- all_data_cleaned[!grepl("howler", all_data_cleaned$label),]


#remove nf calls
all_data_cleaned <- all_data_cleaned[!grepl("nf", all_data_cleaned$label),]


#cleaning call labels
all_data_cleaned[all_data_cleaned == "chirp "] <- "chirp"
all_data_cleaned[all_data_cleaned == "grnt"] <- "grunt"
all_data_cleaned[all_data_cleaned == "gunt"] <- "grunt"
all_data_cleaned[all_data_cleaned == "spueal"] <- "squeal"
all_data_cleaned[all_data_cleaned == "nf chitter "] <- "nf chitter"
all_data_cleaned[all_data_cleaned == "nf chitter x"] <- "nf chitter"
all_data_cleaned[all_data_cleaned == "chitter x "] <- "chitter"
all_data_cleaned[all_data_cleaned == "chitter x"] <- "chitter"
all_data_cleaned[all_data_cleaned == "bob"] <- "bop"
all_data_cleaned[all_data_cleaned == "chirpgr x"] <- "chirp grunt"
all_data_cleaned[all_data_cleaned == "chirpgr "] <- "chirp grunt"
all_data_cleaned[all_data_cleaned == "chirpgr"] <- "chirp grunt"
all_data_cleaned[all_data_cleaned == "low peep"] <- "peep"
all_data_cleaned[all_data_cleaned == "chirp click "] <- "chirp click"
all_data_cleaned[all_data_cleaned == "chirpr"] <- "chirp grunt"
all_data_cleaned[all_data_cleaned == "squeal chitters"] <- "squeal chittering"
all_data_cleaned[all_data_cleaned == "squeal chitter"] <- "squeal chittering"


unique(all_data_cleaned$label)
#look at counts for each call type 
table(all_data_cleaned$label)

cleaned <- all_data_cleaned

#remove calls which are not described in the repertoire paper where the sample size is also super small

cleaned <- cleaned[!grepl("chop chop", cleaned$label),]
cleaned <- cleaned[!grepl("peep", cleaned$label),]
cleaned <- cleaned[!grepl("purr", cleaned$label),]
cleaned <- cleaned[!grepl("quack", cleaned$label),]
cleaned <- cleaned[!grepl("snarl", cleaned$label),]
cleaned <- cleaned[!grepl("squeal chittering", cleaned$label),]
#cleaned[cleaned == "chirpgr"] <- "chirp grunt"


#want to look at each individuals to get average number of calls for each call type
#first need to split dataframe into list of dataframes for each ind
#data_list <- split(cleaned, f = cleaned$id)  



cleaned_diff_time <- cleaned %>%
  arrange(file_name, label, Start) %>%
  group_by(file_name, label) %>%
  mutate(Start = as_hms(Start),  # Convert "Start" to hms format
         diff_time_s = as.numeric(round(difftime(Start, lag(Start, default = first(Start)), units = "secs"), 4))) %>%
  ungroup()


# Create a violin plot for all calls
ggplot(cleaned_diff_time, aes(y = label, x = diff_time_s)) +
  geom_violin() +
  labs(
    title = "Distribution of diff_time for Call Types (diff_time < 60 seconds)",
    y = "Call Type",
    x = "Time Difference (seconds)"
  )


# Filter the data for rows where diff_time is less than 60 seconds
filtered_data <- cleaned_diff_time[cleaned_diff_time$diff_time_s < 1, ]

#removing calls not interested in
excluded_call_types <- c("growl", "snort", "chuckle", "click", "grunt", "chirp grunt", "chirp click", "chirp", "bop", "hum")

filtered_data <- filtered_data %>%
  filter(!label %in% excluded_call_types)

# Create a violin plot for filtered calls

png(height = 800, width = 800, units = 'px', filename = paste0(plot_dir, "intercall_timediff.png"))
ggplot(filtered_data, aes(y = label, x = diff_time_s)) +
  geom_violin() +
  labs(
    #title = "Distribution of diff_time for Call Types (diff_time < 1 seconds)",
    y = "Call Type",
    x = "Time Difference (seconds)") + 
  theme_classic()

dev.off()































