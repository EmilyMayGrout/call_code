#this script is to look at the calls used and call rates before and after events

#read in labels 

wd <- "C:/Users/egrout/Dropbox/coaticalls/Galaxy_labels/completed_labels/labels_25.02.24/"
plot_dir <- "C:/Users/egrout/Dropbox/coaticalls/results/"

setwd <- wd

#LIBRARIES
library(stringr)
library(hms)
library(tidyr)
library(tidyverse)
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
i = 67

for (i in 1:length(files)) {
  # read in the CSV data as a tibble
  # using header = TRUE assumes the first row of each CSV file is a header with column names
  file_data <- read.csv(paste0(wd, files[i]), header = T, sep="\t")
  
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

#remove file_name column 
all_data <- all_data[,-7]

#add ID column
all_data$id <- str_sub(all_data$file, end=5)

#make date colum in POSIXct
all_data$date <- as.Date(sub("(..)$", "20\\1", all_data$date), "%d.%m.%Y")

#make time column
all_data$Start <- all_data$Start
table(str_length(all_data$Start))

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

write.csv(all_data_hms, "C:/Users/egrout/Dropbox/coaticalls/processed/all_data_hms.csv")
 
###TODO Make the code below automated for all fission events

#in case want to split by id for plotting
#f1 <- all_data_hms %>% group_split(id)

#this forloop will go through each individual, through each fission and fusion event and summarise the call rates for the most common call types before and after each event. This will be stored in a seperate dataframe BUT NEED TO THINK ABOUT HOW WE WANT TO STORE THIS INFO FOR FURTHER ANALYSIS

#for now, to solve the time synch (though this needs correcting properly later) I'm adding 1 minute to all calls

all_data_hms$datetime_new <- ifelse(all_data_hms$label %in% c('fission', 'fusion'), 
                                 all_data_hms$datetime,
                                 all_data_hms$datetime + lubridate::seconds(60))

all_data_hms$datetime_new <- as.POSIXct(all_data_hms$datetime_new)

df_odd <- all_data_hms %>% 
  mutate(ff_label = ifelse(label %in% c("fission", "fusion"), label, NA),
         ff_label = ifelse(!is.na(ff_label), str_c(ff_label, as.character(datetime_new), sep = "_"), NA),
         # removing incorrect fusion label (maybe go back later to correct, then remove below line)
         ff_label = ifelse(ff_label == "fusion_2021-12-27 07:27:00", NA, ff_label)) 
# %>% 
#   group_by(id) %>% 
#   mutate(ff_label = ifelse())
  

# Identify the rows with fission fusion labels (non-NA in ff_label) and create intervals
event_intervals <- df_odd %>%
  filter(!is.na(ff_label)) %>%
  mutate(
    before_interval = interval(start = datetime_new - minutes(10), end = datetime_new),
    after_interval = interval(start = datetime_new, end = datetime_new + minutes(10))
  )

# Initialize the ba_label column with NAs
df_odd$ba_label <- NA_character_

# Loop through event_intervals and classify each datetime in df
for(i in 1:nrow(event_intervals)) {
  df_odd <- df_odd %>% 
    group_by(id) %>% 
    mutate(ba_label = ifelse(datetime_new %within% event_intervals$before_interval[i], 
                  "before", 
                  ba_label),
           ba_label = ifelse(datetime_new %within% event_intervals$after_interval[i], 
                  "after", 
                  ba_label)) %>% 
    ungroup()
}

# Corrected function to update ff_label based on ba_label
df_odd_new <- df_odd %>% 
  mutate(updated_ff_label = NA_character_) # Initialize the column to store updated labels

last_seen_label <- NA  # Variable to keep track of the last seen non-NA ff_label
for (i in seq_along(df_odd_new$ff_label)) {
  if (!is.na(df_odd_new$ff_label[i])) {
    last_seen_label <- df_odd_new$ff_label[i]  # Update last seen label when a non-NA ff_label is found
  }
  
  if (!is.na(df_odd_new$ba_label[i]) && !is.na(last_seen_label)) {
    # Prepend "b_" or "a_" to the last seen label based on ba_label
    df_odd_new$updated_ff_label[i] <- paste0(df_odd_new$ba_label[i], "_", last_seen_label)
  }
}

# chack if this worked
test <- df_odd_new %>% 
  filter(!is.na(ba_label))

# only making NAs for first before of first fission other wise it worked
# need to change for loop to count the first fission
sum(is.na(test$updated_ff_label))

# plot
df_odd_new %>% 
  filter(!is.na(updated_ff_label),
         label %in% c("chirp", "chirpgr", "chitter")) %>%
  group_by(id, label, ba_label, updated_ff_label ) %>% 
  summarise(freq = n()) %>% 
  ggplot(aes(x = label, y = freq, color = ba_label, fill = ba_label),
            ) +
  geom_boxplot(outlier.shape = 3) +
  geom_jitter(alpha = 0.2) +
  facet_wrap(~id) +
  theme_bw() 






#create a data frame to hold output data: columns are fission time, ind id, chirp grunt rate befre, chirpgr rate after, chitter rate before, chitter after, etc.
#df_out <- data.frame()
#df_out <- data.frame(time = NA, ind = NA, fis_or_fus = , etc. ) #fake first row might be necessary
i = 1
j = 2
n = 5
for (i in unique(all_data_hms$id)){
  #curr_ind <- i #your current individual is i
  
  #extract each individual
  ind_i <- all_data_hms[all_data_hms$id == unique(all_data_hms$id)[i],]
  
  #for loop through each day
  for (j in 1:unique(ind_i$date)){
    day_j <- ind_i[ind_i$date == unique(ind_i$date)[j],]
    
    for (n in 1:nrow(day_j)){
      #go through each row of dataframe, if the label is "fission", then extract all labels 15 mins before and after that row number to get a table of call counts
      row_n <- day_j[n,]
      label <- row_n$label
      if(label %in% c("fission", 'fusion')){ #if row_n is either a fission or a fusion
        
        #if the row is a fission, get the time of that fission, then get the time of calls 15 mins before and 15 mins after
        fis_time <- day_j$datetime_new[n]
        first_time <- fis_time - lubridate::seconds(900) #15 mins
        last_time <-  fis_time + lubridate::seconds(900)
        
        labels_bef <- day_j[day_j$datetime_new >= first_time & day_j$datetime_new <= fis_time,]
        labels_aft <-day_j[day_j$datetime_new >= fis_time & day_j$datetime_new <= last_time,]
        
        #get the number of each type of call before
        #get the time elapsed (fis_time - first_time)
        #divide to get the call rate for each type of call
        #same for after
        
        #create a row of your data frame above and append it using rbind
        # row <- data.frame(time = fis_time, ind = i, fis_or_fus = row_n, chirp_bef = ,etc.)
        # df_out <- rbind(df_out, row) #bind the row to the current data frame
        
        
        data.frame(table(labels_bef$label))
        
      }
      
        
        else{NULL} #not needed probably
  }
 }
}
    
    
    
    
    

#look at one fission event to see call rates of all individuals involved

#fission event at 06:55 on the 28.12.21

fission_1_281221 <- all_data_hms[all_data_hms$date == "2021-12-28",]

#filter to 15 mins before and after fission event
fission_1_281221$event <- "out"
fission_1_281221$event[fission_1_281221$time > as_hms('6:40:00.000') & fission_1_281221$time < as_hms('6:55:00.000')] <- "1_bef"
fission_1_281221$event[fission_1_281221$time > as_hms('6:55:00.000') & fission_1_281221$time < as_hms('7:10:00.000')] <- "2_aft"

#filter to around event
f1 <- fission_1_281221[fission_1_281221$event == c("1_bef", "2_aft"),]
#filter to the main call types
f1 <- f1[f1$label == c("chirp", "chirpgr", "chitter"),]

ggplot(data = f1, aes(x = label, fill = event, group = event))+
  geom_bar(position = "dodge")+
  facet_wrap(~id)

fission_2_281221 <- all_data_hms[all_data_hms$date == "2021-12-28",]
fission_2_281221$event <- "out"
fission_2_281221$event[fission_2_281221$time > as_hms('7:47:00.000') & fission_2_281221$time < as_hms('8:02:00.000')] <- "1_bef"
fission_2_281221$event[fission_2_281221$time > as_hms('8:02:00.000') & fission_2_281221$time < as_hms('8:17:00.000')] <- "2_aft"

#filter to around event
f1 <- fission_2_281221[fission_2_281221$event == c("1_bef", "2_aft"),]
#filter to the main call types
f1 <- f1[f1$label == c("chirp", "chirpgr", "chitter"),]

ggplot(data = f1, aes(x = label, fill = event, group = event))+
  geom_bar(position = "dodge")+
  facet_wrap(~id)








