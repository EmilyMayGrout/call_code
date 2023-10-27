# counting number of calls for DCASE dataset

Gus_FL3 <- read.csv("F:/PhD/DCASE/6467_2_FL3_cut.csv", sep = "\t")
Gus_FL4 <- read.csv("F:/PhD/DCASE/6467_2_FL4_cut.csv", sep = "\t")

table(Gus_FL3$Name) #could do dc (385) or click (29) for this file
table(Gus_FL4$Name) #could do bark (168) or chirp (157) for this file

together <- rbind(Gus_FL3, Gus_FL4)
#get the number of unique call types
length(unique(together$Name))





