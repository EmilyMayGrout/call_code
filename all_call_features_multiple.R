#this script is getting the acoustic features for multiple examples of the same call type to get the sandard error ect.

library(seewave)
library(tuneR)
library(ggplot2)
library(DT)
library(plyr)

#colour palette for spectrogram
jet.colors <- colorRampPalette(c("white","white", "seashell","seashell1", "plum1", "deeppink4", "darkblue", "black"))

#list of features to keep
keep <- c("mode", "median", "sh", "Q25", "Q75", "skewness", "kurtosis", "cent", "sfm")

# mean	- mean frequency (see mean)
# sd	- standard deviation of the mean (see sd)
# sem	- standard error of the mean
# median	- median frequency (see median)
# mode	- mode frequency, i.e. the dominant frequency
# Q25	- first quartile (see quantile)
# Q75	- third quartile (see quantile)
# IQR	- interquartile range (see IQR)
# cent	- centroid
# skewness	- skewness, a measure of asymmetry
# kurtosis	- kurtosis, a measure of peakedness
# sfm	- spectral flatness measure (see sfm)
# sh	- spectral entropy (see sh)
# prec	- frequency precision of the spectrum

#directory
dir <- "F:/PhD/All things coati/Edic mini calls/each call type/growl/"

filenames <- list.files(dir, pattern=".wav", full.names=TRUE)
all_names = basename(filenames)
lwf <- lapply(filenames, readWave)


#make empty dataframe to add the features too
prop <- data.frame(matrix(ncol = (length(keep) + 6), nrow = 0))

#this forloop is reading in each call (cut into single wave file from adobe audition) and extracting acoustic features, and putting it into the prop dataframe


for (i in 1:length(lwf)) {
  
  #spectro(lwf[[i]],f=22050,ovlp=85,zp=16, palette=jet.colors, osc=TRUE, collevels=seq(-29,0), grid = F)
  f = lwf[[i]]@samp.rate
  dat <- meanspec(lwf[[i]],f=f,plot=F)
  
  dat2 <- as.data.frame(dat)
  
  #excluding rows containing 1.0000
  dat2 <- subset(dat2, y != 1)
  #exclude rows where x is 0
  dat2 <- subset(dat2, x != 0)
  #excluding values greater than 4kHz
  dat2 <- subset(dat2, x < 4)
  
  #dominant frequency is the frequency of max amplitude, which is x for the max of y in chirp_dat2
  dom_freq <- dat2[dat2$y == max(dat2$y),1]
  
  prop1 <- as.data.frame(specprop(dat,f= lwf[[i]]@samp.rate))
  
  #only keeping these measures:
  #mode: dominant frequency
  #median: median frequency
  #sh: Shannon entropy - The Shannon spectral entropy of a noisy signal will tend towards 1 whereas the Shannon spectral entropy of a pure tone signal will tend towards 0
  prop1 <- prop1[, keep] 
  prop1$dom_freq <- dom_freq*1000
  
  #Acoustic complexity index
  prop1$ACI <- ACI(lwf[[i]], lwf[[i]]@samp.rate, channel = 1, wl = 512, ovlp = 0,  wn = "hamming", flim = NULL, nbwindows = 1)
  #Entropy
  prop1$entropy <- H(lwf[[i]], lwf[[i]]@samp.rate, channel = 1, wl = 512, envt="hil", msmooth = NULL, ksmooth = NULL)
  prop1$duration <- duration(lwf[[i]], lwf[[i]]@samp.rate, channel=1)
  prop1$name <- "growl"
  prop1$file <- all_names[i]
  prop <- rbind(prop, prop1)
  dev.off()
}


#chirp

dir <- "F:/PhD/All things coati/Edic mini calls/each call type/chirp/"
filenames <- list.files(dir, pattern=".wav", full.names=TRUE)
all_names = basename(filenames)
lwf <- lapply(filenames, readWave)

#i = 3
for (i in 1:length(lwf)) {
  
  #spectro(lwf[[i]],f=lwf[[i]]@samp.rate,ovlp=85,zp=16, palette=jet.colors, osc=TRUE, collevels=seq(-29,0), grid = F)
  
  f = lwf[[i]]@samp.rate
  
  #incase we wanted to resample but I don't think this is necessary 
  #if (f > 100000){
  #  lwf[[i]] <- resamp(lwf[[i]], f = lwf[[i]]@samp.rate, g = 50000, output = "Wave")
  #}
  
  #dev.off()
  dat <- meanspec(lwf[[i]],f= lwf[[i]]@samp.rate , plot= F)
  dat2 <- as.data.frame(dat)
  
  #excluding rows containing 1.0000
  dat2 <- subset(dat2, y != 1)
  #exclude rows where x is 0
  dat2 <- subset(dat2, x != 0)
  #excluding values less than 4kHz
  dat2 <- subset(dat2, x > 4)
  
  
  #dominant frequency is the frequency of max amplitude, which is x for the max of y in chirp_dat2
  dom_freq <- dat2[dat2$y == max(dat2$y),1]
  
  prop2 <- as.data.frame(specprop(dat,f= lwf[[i]]@samp.rate))
  prop2 <- prop2[, keep] 
  prop2$dom_freq <- dom_freq*1000
  
  #Acoustic complexity index
  prop2$ACI <- ACI(lwf[[i]], lwf[[i]]@samp.rate, channel = 1, wl = 512, ovlp = 0,  wn = "hamming", flim = NULL, nbwindows = 1)
  #Entropy
  prop2$entropy <- H(lwf[[i]], lwf[[i]]@samp.rate, channel = 1, wl = 512, envt="hil", msmooth = NULL, ksmooth = NULL)
  prop2$duration <- duration(lwf[[i]], lwf[[i]]@samp.rate, channel=1)
  prop2$name <- "chirp"
  prop2$file <- all_names[i]
  
  prop <- rbind(prop, prop2)

}


#chitter

dir <- "F:/PhD/All things coati/Edic mini calls/each call type/chitter/"
filenames <- list.files(dir, pattern=".wav", full.names=TRUE)
all_names = basename(filenames)
lwf <- lapply(filenames, readWave)

i = 7
for (i in 1:length(lwf)) {
  
  #spectro(lwf[[i]],f=lwf[[i]]@samp.rate,ovlp=85,zp=16, palette=jet.colors, osc=TRUE, collevels=seq(-29,0), grid = F)
  
  f = lwf[[i]]@samp.rate
  
  dat <- meanspec(lwf[[i]],f= lwf[[i]]@samp.rate , plot= F)
  dat2 <- as.data.frame(dat)
  
  #excluding rows where y is 1.0000
  dat2 <- subset(dat2, y != 1)
  #exclude rows where x is 0
  dat2 <- subset(dat2, x != 0)
  
  #excluding values less than 4kHz
  #dat2 <- subset(dat2, x > 4)
  
  #dominant frequency is the frequency of max amplitude, which is x for the max of y in chirp_dat2
  dom_freq <- dat2[dat2$y == max(dat2$y), 1]
  
  prop3 <- as.data.frame(specprop(dat,f=lwf[[i]]@samp.rate))
  prop3 <- prop3[, keep] 
  prop3$dom_freq <- dom_freq*1000
  
  #Acoustic complexity index
  prop3$ACI <- ACI(lwf[[i]], channel = 1, wl = 512, ovlp = 0,  wn = "hamming", flim = NULL, nbwindows = 1)
  
  #Entropy
  prop3$entropy <- H(lwf[[i]], lwf[[i]]@samp.rate, channel = 1, wl = 512, envt="hil", msmooth = NULL, ksmooth = NULL)
  prop3$duration <- duration(lwf[[i]], lwf[[i]]@samp.rate, channel=1)
  prop3$name <- "chitter"
  prop3$file <- all_names[i]
  
  prop <- rbind(prop, prop3)
  
}


#this works, now need to think about what other features should be extracted

#visualising spectral properties between call types
boxplot(dom_freq ~ name, prop)   
boxplot(sfm ~ name, prop)   


stand_dev <- data.frame(matrix(ncol = (length(keep) + 6), nrow = 3))
colnames(stand_dev) <- colnames(prop)

stand_dev$name[1:3] <- c("growl", "chirp", "chitter")

stand_dev$mode[1] <- sd(prop$mode[prop$name == "growl"])









