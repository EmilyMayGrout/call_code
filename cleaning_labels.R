#this script is for cleaning the labels and saving them back into the same file structure but in a different folder

#read in labels 
wd <- "C:/Users/egrout/Dropbox/coaticalls/Galaxy_labels/completed_labels/labels_25.02.24/"

setwd <- wd

#read in coati IDs 
load('C:/Users/egrout/Dropbox/coatithon/processed/2022/galaxy/galaxy_coati_ids.RData')

# get a list of all the CSV files in the folder
files <- list.files(wd, pattern = "*.csv")

#create data frame with 0 rows and 3 columns
all_data <- data.frame(matrix(ncol = 8, nrow = 0))
#provide column names
colnames(all_data) <- c("label","Start","Duration","Time","Format","Type","Description","file_name")

# loop through each CSV file and add it to the dataframe
#i = 67

#directory cleaned labels are saved into
output_dir <- "C:/Users/egrout/Dropbox/coaticalls/Galaxy_labels/completed_labels/labels_cleaned_25.02.24/"

#create data frame with 0 rows and 3 columns, so can check if all labels cleaned here have been corrected
#all_data <- data.frame(matrix(ncol = 8, nrow = 0))


for (i in 1:length(files)) {
  # read in the CSV data as a tibble
  # using header = TRUE assumes the first row of each CSV file is a header with column names
  file_data <- read.csv(paste0(wd, files[i]), header = T, sep="\t")
  
  
  #cleaning call labels
  file_data$Name[file_data$Name == "chirp "] <- "chirp"
  file_data$Name[file_data$Name == "chrip"] <- "chirp"
  file_data$Name[file_data$Name == "chirp x"] <- "chirp"
  file_data$Name[file_data$Name == "grnt"] <- "grunt"
  file_data$Name[file_data$Name == "gunt"] <- "grunt"
  file_data$Name[file_data$Name == "spueal"] <- "squeal"
  file_data$Name[file_data$Name == "squeal "] <- "squeal"
  file_data$Name[file_data$Name == "nf chitter "] <- "nf chitter"
  file_data$Name[file_data$Name == "nf chitter x"] <- "nf chitter"
  file_data$Name[file_data$Name == "nf chirtter"] <- "nf chitter"
  file_data$Name[file_data$Name == "chitter x "] <- "chitter"
  file_data$Name[file_data$Name == "chitter x"] <- "chitter"
  file_data$Name[file_data$Name == "chitter "] <- "chitter"
  file_data$Name[file_data$Name == "dc x"] <- "dc"
  file_data$Name[file_data$Name == "bob"] <- "bop"
  file_data$Name[file_data$Name == "chirpgr x"] <- "chirp grunt"
  file_data$Name[file_data$Name == "chirpgr "] <- "chirp grunt"
  file_data$Name[file_data$Name == "chirpgr"] <- "chirp grunt"
  file_data$Name[file_data$Name == "low peep"] <- "peep"
  file_data$Name[file_data$Name == "chirp click "] <- "chirp click"
  file_data$Name[file_data$Name == "chirp cick"] <- "chirp click"
  file_data$Name[file_data$Name == "chirpr"] <- "chirp grunt"
  file_data$Name[file_data$Name == "chirgpr"] <- "chirp grunt"
  file_data$Name[file_data$Name == "unk chirpgr"] <- "chirp grunt"
  file_data$Name[file_data$Name == "click grnut"] <- "click grunt"
  file_data$Name[file_data$Name == "squeal chitters"] <- "squeal chittering"
  file_data$Name[file_data$Name == "squeal chitter"] <- "squeal chittering"
  file_data$Name[file_data$Name == "squeal chitter x"] <- "squeal chittering"
  file_data$Name[file_data$Name == "squeal chitter x"] <- "squeal chittering"
  file_data$Name[file_data$Name == "nf chitter squeal"] <- "nf squeal chitter"
  file_data$Name[file_data$Name == "chitter squeal"] <- "squeal chittering"
  file_data$Name[file_data$Name == "scratch "] <- "scratch"
  file_data$Name[file_data$Name == " scratch"] <- "scratch"
  file_data$Name[file_data$Name == "scatch"] <- "scratch"
  file_data$Name[file_data$Name == "head shake "] <- "head shake"
  file_data$Name[file_data$Name == "heah shake"] <- "head shake"
  file_data$Name[file_data$Name == "bird "] <- "bird"
  file_data$Name[file_data$Name == "bird  "] <- "bird"
  
  #can add the data to the all_data dataframe to check we have all the spelling errors fixed
  #all_data <- rbind(all_data, file_data)
  
  # add file back into folder
  
  # Define the output file path
  output_file <- file.path(output_dir, files[i])
  
  # Write the cleaned data back to CSV --  DOUBLE CHECK IF FILES ALREADY IN THIS DIRECTORY 
  #write.csv(file_data, file = output_file, row.names = FALSE)
  
}





#to check the call labels are correct
table(all_data$Name)
#unique(all_data$Name)



