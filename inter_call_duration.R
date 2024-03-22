#this script is calculating the time between the rhythmic calls

wd <- "C:/Users/egrout/Dropbox/coaticalls/Galaxy_labels/completed_labels/labels_cleaned_25.02.24/"
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
  file_data <- read.csv(paste0(wd, files[i]), header = T)
  
  # add a column with the row names (i.e. the name of the CSV file)
  file_data$file_name <- files[i]
  #only keeping necessary info of the file name
  file_data$file <- str_sub(file_data$file_name,end=11)
  file_data$date <- str_sub(file_data$file_name, start = 13, end = 20)
  colnames(file_data)[colnames(file_data) == "Name"] <- "label"
  
  # add the data to the all_data dataframe
  all_data <- rbind(all_data, file_data)
}


#remove rows which contain the date 
all_data <- all_data[!grepl(":", all_data$label),]

#add ID column
all_data$id <- str_sub(all_data$file_name, end=5)

#make date colum in POSIXct
all_data$date <- as.Date(sub("(..)$", "20\\1", all_data$date), "%d.%m.%Y")

#make time column
all_data$Start <- all_data$Start
table(str_length(all_data$Start))

#remove rows which contain the date 
all_data <- all_data[!grepl(":", all_data$label), ]

#remove unnecessary rows
all_data <- all_data[, -c(4:6)]



#because the length of the time column is different due to some times less than an hour, need to split the data to get the times and then rbind them
t <- filter(all_data, nchar(Start) == 8)
t$time <- paste0("6:", t$Start)
t$time <- as_hms(t$time)

s <- filter(all_data, nchar(Start) == 9)
s$time <- paste0("6:", s$Start)
s$time <- as_hms(s$time)

v <- filter(all_data, nchar(Start) == 11)
v$time <- as_hms(as_hms(v$Start) + as_hms('6:00:00.000'))

all_data_hms <- rbind(t,s,v)

#add as.POSIXct
all_data_hms$datetime <- as.POSIXct(paste(all_data_hms$date, all_data_hms$time), format = "%Y-%m-%d %H:%M:%OS")


#---------------------------------------------------------------------
#now cleaning labels and removing labels which are not calls
unique(all_data$label)
#remove labels
all_data_cleaned <- all_data_hms

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
all_data_cleaned <- all_data_cleaned[!grepl("forag", all_data_cleaned$label),]
all_data_cleaned <- all_data_cleaned[!grepl("fart", all_data_cleaned$label),]
all_data_cleaned <- all_data_cleaned[!grepl("buzz", all_data_cleaned$label),]
all_data_cleaned <- all_data_cleaned[!grepl("howler", all_data_cleaned$label),]
all_data_cleaned <- all_data_cleaned[!grepl("fission", all_data_cleaned$label),]
all_data_cleaned <- all_data_cleaned[!grepl("fusion", all_data_cleaned$label),]
all_data_cleaned <- all_data_cleaned[!grepl("start", all_data_cleaned$label),]
all_data_cleaned <- all_data_cleaned[!grepl("stop", all_data_cleaned$label),]
all_data_cleaned <- all_data_cleaned[!grepl("insect", all_data_cleaned$label),]
all_data_cleaned <- all_data_cleaned[!grepl("rain", all_data_cleaned$label),]
all_data_cleaned <- all_data_cleaned[!grepl("eating", all_data_cleaned$label),]
all_data_cleaned <- all_data_cleaned[!grepl("collar", all_data_cleaned$label),]


#remove nf calls
all_data_cleaned <- all_data_cleaned[!grepl("nf", all_data_cleaned$label),]

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
cleaned <- cleaned[!grepl("cackle", cleaned$label),]
cleaned <- cleaned[!grepl("cackling", cleaned$label),]
cleaned <- cleaned[!grepl("quack", cleaned$label),]
cleaned <- cleaned[!grepl("whistle", cleaned$label),]
cleaned <- cleaned[!grepl("snort", cleaned$label),]
cleaned <- cleaned[!grepl("squeak", cleaned$label),]
cleaned <- cleaned[!grepl("chuckle", cleaned$label),]
cleaned <- cleaned[!grepl("low squeal", cleaned$label),]


#look at counts for each call type 
table(cleaned$label)


#want to look at each individuals to get average number of calls for each call type

cleaned_diff_time <- cleaned %>%
  arrange(file_name, label, time) %>%
  group_by(file, label) %>%
  mutate(Start = as_hms(time),  # Convert "Start" to hms format
         diff_time_s = as.numeric(round(difftime(time, lag(time, default = first(time)), units = "secs"), 4))) %>%
  ungroup()


# Create a violin plot for all calls
ggplot(cleaned_diff_time, aes(y = label, x = diff_time_s)) +
  geom_violin() +
  labs(
    title = "Distribution of diff_time for Call Types (diff_time < 60 seconds)",
    y = "Call Type",
    x = "Time Difference (seconds)"
  )


# Filter the data for rows where diff_time is less than 1 second
filtered_data <- cleaned_diff_time[cleaned_diff_time$diff_time_s < 1, ]

#removing calls not interested in
excluded_call_types <- c("growl", "snort", "chuckle", "click", "grunt", "chirp grunt", "chirp click", "chirp", "bop", "hum", "snore", "squeal chittering")

filtered_data$label[filtered_data$label == "dc"] <- "dolphin call"

#remove points where time difference is 0 because it's impossible to call 2 calls at the same time, so likely a nf caller

filtered_data <- filtered_data[!filtered_data$diff_time_s == 0, ]


filtered_data <- filtered_data %>%
  filter(!label %in% excluded_call_types)

# Create a violin plot for filtered calls

png(height = 600, width = 1000, units = 'px', filename = paste0(plot_dir, "intercall_timediff.png"))
ggplot(filtered_data, aes(y = label, x = diff_time_s)) +
  geom_violin(fill = "dodgerblue1") +
  labs(
    #title = "Distribution of diff_time for Call Types (diff_time < 1 seconds)",
    y = "Call Type",
    x = "Time Difference (seconds)") + geom_jitter(height = 0.15, width = 0, size = 0.01, alpha = 0.1, color = "black")+
  theme(legend.title=element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.text=element_text(size=20), axis.title = element_text(size = 20), legend.text = element_text(size = 20))

dev.off()































