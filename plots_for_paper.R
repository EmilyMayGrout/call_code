#this script is to make a table with each calls properties for the repertoire paper

library(seewave)
library(tuneR)
library(ggplot2)
library(DT)
library(plyr)

#colour palette:
jet.colors <- colorRampPalette(c("white","white", "plum1", "deeppink4", "blue", "black"))

#GROWL
growl.wave <- readWave('F:/PhD/All things coati/Edic mini calls/each call type/growl cut.wav')

f=22050
cutgrowl <- cutw(growl.wave,f=22050,from=0.03, to = 0.55 ,plot=T)
spectro(growl.wave,f=22050,ovlp=85,zp=16, palette=jet.colors, osc=TRUE, collevels=seq(-29,0), grid = F)

growl_dat <- meanspec(cutw(growl.wave,f=22050,from=0.03, to = 0.55 ,plot=T),f=22050,plot=T)

prop1 <- as.data.frame(specprop(growl_dat,f=22050))
prop1$call_type <- "growl"
prop1 <- prop1[, c("mode", "median", "sh")] 

#only keeping these measures:
#mode: dominant frequency
#median: median frequency
#sh: Shannon entropy - The Shannon spectral entropy of a noisy signal will tend towards 1 whereas the Shannon spectral entropy of a pure tone signal will tend towards 0

#Acoustic complexity index
prop1$ACI <- ACI(growl.wave, f, channel = 1, wl = 512, ovlp = 0,  wn = "hamming", flim = NULL, nbwindows = 1)
#Entropy
prop1$entropy <- H(growl.wave, f, channel = 1, wl = 512, envt="hil", msmooth = NULL, ksmooth = NULL)
prop1$duration <- duration(growl.wave, f, channel=1)
prop1$name <- "growl"












#plot amplitude envelope
env <- env(growl.wave, f, channel = 1, envt = "hil", 
    msmooth = NULL, ksmooth = NULL, ssmooth = NULL,
    asmooth = NULL,
    fftw = FALSE, norm = FALSE,
    plot = T, alab= "Amplitude (dB)", yaxt= "s")










