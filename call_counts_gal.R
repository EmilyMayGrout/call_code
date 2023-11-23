#this script is reading in the call labels in the completed labels folder on OwnCloud and calculating call counts for each individual

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
data_list <- split(cleaned, f = cleaned$id)  

#make data table to store call counts
call_counts <- data.frame(matrix(ncol = length(unique(cleaned$id)), nrow = length(unique(cleaned$label))))
colnames(call_counts) <- unique(cleaned$id)
row.names(call_counts) <- unique(cleaned$label)

#make data table to store call rates (divided by observation time)
call_rates <- data.frame(matrix(ncol = length(unique(cleaned$id)), nrow = length(unique(cleaned$label))))
colnames(call_rates) <- unique(cleaned$id)
row.names(call_rates) <- unique(cleaned$label)





#going through each individual
for (i in 1:length(data_list)){
  
  ind <- data_list[[i]]
  name <- unique(ind$id)
  
  #going through each call type to get call rate per hour
  for (j in 1:length(unique(ind$label))){
    
    
    call_type <- unique(ind$label)[j]
    call_count <- length(which(ind$label == call_type))
    hours_labelled <- length(unique(ind$file_name))
    call_rate <- call_count/hours_labelled
    call_rate <- round(call_rate, 2)
    #get reference location in table
    call_counts[call_type, name] <- call_count
    call_rates[call_type, name] <- call_rate
    
  }
}
#----------------------------------------------------------
#now to make some plots!

#I want a plot where I have calls on the x axis and call count/rate on the Y with error bars for the variation between individuals 

#first need to rearrange data for plotting (to long format)

#adding column with the call_type
call_counts_long <- call_counts
call_counts_long$call_type <- unique(cleaned$label)
#pivot the data frame into a long format
call_counts_long <- call_counts_long %>% pivot_longer(cols = unique(cleaned$id), names_to='id',values_to='count')

#replace NA's with 0
call_counts_long[is.na(call_counts_long)] <- 0

#summarize mean and sd for each category
df_summary_count <- call_counts_long %>%
  group_by(call_type) %>%
  summarize(mean=mean(count),
            sd=sd(count))

#view summary data
df_summary_count

png(height = 700, width = 1000, units = 'px', filename = paste0(plot_dir, "summary_count.png"))
ggplot(df_summary_count) +
  geom_bar(aes(x=call_type, y=mean), stat='identity', fill='steelblue2') +
  geom_errorbar(aes(x=call_type, ymin=pmax(mean-sd,0), ymax=mean+sd), width=0.3, color='orange3') + theme_classic() +labs(x="Call type",y="Total call count")+ theme(text=element_text(size=30))+ theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))
dev.off()

# Also for call RATES --------------------------------------------
#adding column with the call_type
call_rates_long <- call_rates
call_rates_long$call_type <- unique(cleaned$label)
#pivot the data frame into a long format
call_rates_long <- call_rates_long %>% pivot_longer(cols = unique(cleaned$id), names_to='id',values_to='rate')

#replace NA's with 0
call_rates_long[is.na(call_rates_long)] <- 0

#summarize mean and sd for each category
df_summary_rate <- call_rates_long %>%
  group_by(call_type) %>%
  summarize(mean=mean(rate),
            sd=sd(rate))

#view summary data
df_summary_rate

png(height = 700, width = 1000, units = 'px', filename = paste0(plot_dir, "summary_rates.png"))
ggplot(df_summary_rate) +
  geom_bar(aes(x=call_type, y=mean), stat='identity', fill='steelblue') +
  geom_errorbar(aes(x=call_type, ymin=pmax(mean-sd,0), ymax=mean+sd), width=0.3, color='orange4') + theme_classic() +labs(x="Call type",y="Call rate (per hour)")+ theme(text=element_text(size=30))+ theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))
dev.off()

#I also want a plot for the call counts based on age/sex rank - do adults call more than subadults ect.

#first look at variation for each ind and order based on age/sex class
#ggplot(df_summary_rate) + geom_bar(aes(x=call_type, y=mean), stat='identity')

#categorize age/sex class in new column for call counts and call rates 
#read in coati ID dataframe
coati_ids <- read.csv("C:/Users/egrout/Dropbox/coatithon/rawdata/2022/galaxy/metadata/coati_id.csv", header = F)

#make a column for age_sex class
coati_ids$age_sex <- paste(coati_ids$V3, coati_ids$V4, sep=" ")

#remove to columns needed
ids <- coati_ids[,c(1,2,5)]

#rename column for merge function
colnames(ids)[colnames(ids) == "V1"] <- "name"
colnames(ids)[colnames(ids) == "V2"] <- "id"


#make empty column in cleaned dataframe
cleaned$age_sex <- NA
cleaned$name <- NA

#merge the ids dataframe to the cleaned dataframe to add the age_sex column 
newtable <- merge(cleaned,ids, by  = "id", all.x = T, all.y = T)
newtable <- newtable[, c(1:6, 9,10)]

#now want to plot the distributions of call counts and call rates for each age_sex class....


Spectral_edit <- c("#9E0142", "#D53E4F", "#F46D43", "#FDAE61", "#FEE08B", "#FFFFBF", "#E6F598", "#ABDDA4", "#66C2A5", "#3288BD", "#5E4FA2")

Paired_edit <- c("#A6CEE3", "#1F78B4", "#A2CD5A", "#33A02C", "#FB9A99", "#E31A1C", "#FDBF6F", "#FF7F00", "#CDB5CD", "#8B4789", "#00868B")
Paired_5 <- c("#CDB5CD", "#A2CD5A","#E31A1C", "#FF7F00",  "#00868B")



#filter to only chirp, chirpgrunt, chitter and squeal

newtable_filt <- newtable
unique(newtable$label)
newtable_filt <- newtable[newtable$label %in% c("chirp grunt", "chirp", "chitter", "squeal", "dc", "bark"), ]

# Number of calls in each class for each ind:
#ggplot(newtable_filt, aes(label)) + geom_bar(aes(fill = name.y), position="dodge")  #+ facet_wrap(vars(id), ncol = 3)

# Number of calls in each class for each age/sex class:
png(height = 1200, width = 2000, units = 'px', filename = paste0(plot_dir, "summary_counts_agesex.png"))

ggplot(newtable_filt, aes(label)) + geom_bar(aes(fill = name.y), position="dodge") + facet_wrap(vars(age_sex.y), ncol = 3) +theme_classic() + scale_fill_manual(values=Paired_edit) + labs(x="Call type",y="Call count") + labs(fill="name") + theme(text=element_text(size=40), axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))
dev.off()


png(height = 1200, width = 2000, units = 'px', filename = paste0(plot_dir, "summary_counts_agesex_odd.png"))
ggplot(newtable_filt, aes(label, width=.25)) + 
  geom_bar(aes(fill = name.y, color = name.y),
           alpha = 0.5, position="dodge", size = 1.5) + 
  facet_wrap(vars(age_sex.y), ncol = 3) +
  #theme_classic() + 
  theme_clean() +
  scale_fill_manual(values=Paired_edit, aesthetics = c("color", "fill")) + 
  labs(x="Call type",y="Call count", fill = "name", color = "name") +
  theme(panel.grid.major.x = element_line(color = "gray", 
                                          linetype = "dotted"),
        panel.grid.major.y = element_blank(),
        axis.text=element_text(size=40),
        strip.text = element_text(size = 40),
        legend.title = element_text(size = 40),
        legend.text = element_text(size = 40),
        legend.position = "right",
        legend.background = element_rect(color = NA),
        axis.title=element_text(size = 40),
        axis.text.x = element_text(angle = 45, 
                                   vjust = 1, 
                                   hjust=1),)
dev.off()





#put the call counts in one graph with different colours for age/sex class
png(height = 700, width = 1200, units = 'px', filename = paste0(plot_dir, "summary_counts_agesex_combo.png"))

ggplot(newtable_filt, aes(label)) + geom_bar(aes(fill = age_sex.y), position="dodge")  +theme_classic()+ scale_fill_manual(values=Paired_5)  +labs(x="Call type",y="Call count")+labs(fill="name")+ theme(text=element_text(size=30), axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))

dev.off()

#other ways of changing the colours
#+ scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9", "red", "red","#999999", "#E69F00", "#56B4E9", "#999999", "#E69F00", "#56B4E9","#999999", "#E69F00"))
#+ scale_fill_hue(l=40, c=35)

#these plots are just call counts which doesn't take into account differences in call rate 


# --------ADD call RATES for age sex class--------------------------------


#add age_sex class to dataframe
call_rates_class <- merge(call_rates_long, ids, by  = "id")

#filter to calls interested in plotting
call_rates_class_filt <- call_rates_class[call_rates_class$call_type %in% c("chirp grunt", "chirp","chirp click", "chitter", "squeal", "dc", "bark"), ]

png(height = 1000, width = 1400, units = 'px', filename = paste0(plot_dir, "summary_rate_agesex.png"))

ggplot(call_rates_class_filt, aes(x = call_type, y = rate))+ geom_col(aes(fill = name), position="dodge")+ facet_wrap(vars(age_sex))+theme_classic()+ scale_fill_manual(values=Paired_edit)  +labs(x="Call type",y="Call rate (per hour)")+labs(fill="name")+ theme(text=element_text(size=30), axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))

dev.off()

#want to change order of this plot so it's in the long format and the x axis is call rate and y axis is call type

ggplot(call_rates_class_filt, aes(x = rate, y = name))+ 
  geom_point(aes(color = call_type)) +
  scale_x_continuous(limits = c(0,250))+# panel spacing
facet_grid(vars(age_sex), scales = "free", space = "free") +
  theme_bw() +
  theme(panel.grid = element_blank())

#save call rates for each ind
save(call_rates_class_filt, file = "C:/Users/egrout/Dropbox/coaticalls/Galaxy_labels/coati_call_rates.Rda")
#save as a csv file
write.csv(call_rates_class_filt, "C:/Users/egrout/Dropbox/coaticalls/Galaxy_labels/coati_call_rates.csv", row.names=FALSE, quote=FALSE) 


#--------------------------------------------------------------------------------------------

png(height = 700, width = 1100, units = 'px', filename = paste0(plot_dir, "summary_rate_agesex_combo.png"))

ggplot(call_rates_class_filt, aes(x = call_type, y = rate))+ geom_col(aes(fill = age_sex), position="dodge")+theme_classic()+ scale_fill_manual(values=Paired_5)  + labs(x="Call type",y="Call rate (per hour)")+labs(fill="name")+ theme(text=element_text(size=30), axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))

dev.off()


#Zipfs law - plotting call duration and total call count for all inds

all_count <- all_data_cleaned[,c(1,3,5)]

#turn duration into numeric by removing the "0:" to put it into seconds
all_count$Duration <- gsub("0:", "", all_count$Duration)
all_count$Duration <- as.numeric(all_count$Duration)

#getting count for each call type
z <- all_count %>% count(label)

#getting the mean duration for each call type
t <- all_count %>% 
  group_by(label) %>%
  summarize(mean=mean(Duration),
            sd=sd(Duration))

zt <- merge(x = z, y = t, by = "label", all = TRUE)
colnames(zt)<- c("label","call_count","mean_duration", "sd")

plot(zt$call_count, zt$mean_duration, xlab = "call count", ylab = "mean call duration")

# The errorbars overlapped, so use position_dodge to move them horizontally
pd <- position_dodge(0.1) # move them .05 to the left and right

#logged
png(height = 800, width = 1000, units = 'px', filename = paste0(plot_dir, "zipfs_law_log.png"))
ggplot(zt, aes(x=log(call_count), y=mean_duration)) + 
  geom_errorbar(aes(ymin=mean_duration-sd, ymax=mean_duration+sd), colour="black", width=.1, position=pd)+
  labs(x="Logged Call Count",y="Call Duration (s)")+
  geom_point(position=pd, size=8)+
  theme_classic(base_size = 40)
dev.off()

#raw
png(height = 800, width = 1000, units = 'px', filename = paste0(plot_dir, "zipfs_law.png"))
ggplot(zt, aes(x=call_count, y=mean_duration)) + 
  geom_errorbar(aes(ymin=mean_duration-sd, ymax=mean_duration+sd), colour="black", width=.1, position=pd)+
  labs(x="Call Count",y="Call Duration (s)")+
  geom_point(position=pd, size=8)+
  theme_classic(base_size = 40)
dev.off()

#rank the call counts to see if a pattern emerges
order.scores <- order(zt$call_count)
zt <- zt[order.scores,]
zt$rank <- rank(zt$call_count)


png(height = 800, width = 1000, units = 'px', filename = paste0(plot_dir, "zipfs_law_rank.png"))
ggplot(zt, aes(x=rank, y=mean_duration)) + 
  geom_errorbar(aes(ymin=mean_duration-sd, ymax=mean_duration+sd), colour="black", width=.1, position=pd)+
  geom_point(position=pd, size=8)+
  labs(x="Ranked Call Count",y="Call Duration (s)")+
  theme_classic(base_size = 40)
dev.off()


#look at zipfs law per individual

#need to split all_data_cleaned to make a list for each individual (so can do a forloop over the list to get the mean duration and call counts)


#getting the mean duration for each call type for each individual
all_inds <- data.frame(matrix(ncol = 4, nrow = 0))

for (i in 1:length(unique(all_count$id))){
  
  #get the individual to calculate the duration and call counts
  ind <- unique(all_count$id)[i]
  #subset to the dataframe for that individuals data
  id_count <- subset(all_count, id == ind)
  
  #get the mean duration and sd for each call type
  dur_perind <- id_count %>% 
  group_by(label) %>%
  summarize(mean=mean(Duration),
            sd=sd(Duration))
  #get the call counts for each call type
  count_percall <- id_count %>% count(label)
  
  #merge the dataframes and save into the list
  ind_merge <- merge(x = count_percall, y = dur_perind, by = "label", all = TRUE)
  ind_merge$id <- ind
         
  all_inds <- rbind(all_inds, ind_merge)
  
}


pd <- position_dodge(0.1) # move them .05 to the left and right

#go through each individual to plot their call count against mean call duration

for (i in 1:length(unique(all_inds$id))) {
  df <- subset(all_inds, id == unique(all_inds$id)[i])
  j <- df$id[1]
  
  plot <- ggplot(df, aes(x = log(n), y = mean)) + 
    geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd), colour = "black", width = 0.1) +
    geom_point(size = 3) +
    #scale_y_continuous(limits = c(0, 1)) +
    #scale_x_continuous(limits = c(0, 1500)) +
    labs(x = "Call Count", y = "Call Duration (s)") +
    theme_classic()
  
  filename <- file.path(plot_dir, "zipf/log", paste0("zipfs_law_", j, ".png"))
  
  ggsave(filename, plot, height = 4, width = 5, units = "in")
}


#put the plots together to visualise easily:

# Create an empty list to store the individual plots
plot_list <- list()

# Example code within your loop
for (i in 1:length(unique(all_inds$id))) {
  df <- subset(all_inds, id == unique(all_inds$id)[i])
  j <- df$id[1]
  
  plot <- ggplot(df, aes(x = log(n), y = mean)) + 
    geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd), colour = "black", width = 0.1) +
    geom_point(size = 3) +
    labs(x = "Call Count", y = "Call Duration (s)") +
    theme_classic()
  
  # Store the individual plot in the list
  plot_list[[i]] <- plot
}

# Arrange the individual plots into a grid
combined_plot <- do.call(grid.arrange, c(plot_list, ncol = 3)) # Adjust ncol as needed

# Save the combined plot as an image
filename <- file.path(plot_dir, "zipf/log", "combined_plot.png")
ggsave(filename, combined_plot, height = 8, width = 10, units = "in")





#make plot for the number of files used:

file_count <- data.frame(files)
file_count$id <- str_extract(file_count$files, ".+?(?=_)")
colnames(coati_ids)[colnames(coati_ids) == "V2"] <- "id"

merged_data <- merge(coati_ids, file_count, by = "id", all = T)

png(height = 1000, width = 1500, units = 'px', filename = paste0(plot_dir, "number_of_files_labelled.png"))
par(mar = c(9, 4, 4, 2) + 2)
barplot(table(merged_data$V1), xlab = "", ylab = "Number of files labelled", col = "aquamarine4", cex.lab = 3, cex.axis = 3, cex.names = 3, las = 2)
dev.off()











