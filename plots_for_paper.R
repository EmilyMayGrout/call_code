#this script is to make a table with each calls properties for the repertoire paper

#spectrograms were saved with the export button to the plot_dir/spectrograms 600x600 size

library(seewave)
library(tuneR)
library(ggplot2)
library(DT)
library(plyr)

plot_dir <- "C:/Users/egrout/Dropbox/coaticalls/results/spectrograms/"

#colour palette:
jet.colors <- colorRampPalette(c("white","white", "plum1", "deeppink4", "blue", "black"))
jet.colors2 <- colorRampPalette(c("plum1","deeppink4", "blue2"))
jet.colors3 <- colorRampPalette(c("white","plum1","deeppink4", "blue2"))
jet.colors4 <- colorRampPalette(c("white","plum2","deeppink4", "blue2"))

#load in each example call type to describe in paper
#AGGRESIVE
growl <- readWave('F:/PhD/All things coati/Edic mini calls/each call type/growl cut.wav')
squeal <- readWave('F:/PhD/All things coati/Edic mini calls/each call type/squeal_down.wav')
squeal_grunt <- readWave('F:/PhD/All things coati/Edic mini calls/each call type/squeal grunt.wav')
chittering <- readWave('F:/PhD/All things coati/Edic mini calls/each call type/chittering.wav')
long_chitter <- readWave('F:/PhD/All things coati/Edic mini calls/each call type/long chitter.wav')

#CONTACT
chirp_grunt <- readWave('F:/PhD/All things coati/Edic mini calls/each call type/chirpgrunt_cut.wav')
chirp_click_grunt <- readWave('F:/PhD/All things coati/Edic mini calls/each call type/chirpclickgrunt2.wav')
chirp_grunt_snort <- readWave('F:/PhD/All things coati/Edic mini calls/each call type/chirpgruntsnort.wav')#from file 3 6887 edic mini
grunt <- readWave('F:/PhD/All things coati/Edic mini calls/each call type/grunt.wav') #from 9480_1_FL8
clicks <- readWave('F:/PhD/All things coati/Edic mini calls/each call type/clicks.wav') ##from Chris Hass Ramsey Canyon Coatis video
chirp2 <-readWave('F:/PhD/All things coati/Edic mini calls/each call type/chirp2.wav') #from 9480_1_FL3


#AGGRESSIVE CALL SEQUENCES
agg_seq4 <- readWave('F:/PhD/All things coati/Edic mini calls/each call type/aggsequence4.wav')
agg_seq5 <- readWave('F:/PhD/All things coati/Edic mini calls/each call type/aggsequence5.wav')
agg_seq6 <- readWave('F:/PhD/All things coati/Edic mini calls/each call type/aggsequence6.wav')

#ALARM
chop_chop <- readWave('F:/PhD/All things coati/Edic mini calls/each call type/chop chop.wav') 
barking <- readWave('F:/PhD/All things coati/Edic mini calls/each call type/barking.wav') #from Chris Hass Ramsey Canyon Coatis video
bark <- readWave('F:/PhD/All things coati/Edic mini calls/each call type/barkcomplex.wav') #from Chris Hass Ramsey Canyon Coatis video

#MATING
dolphin_call <- readWave('F:/PhD/All things coati/Edic mini calls/each call type/dolphin call2.wav') #call comes from Galaxy 9463_2_FL1

#SHORT CALLS - UNK FUNCTION
bop <- readWave('F:/PhD/All things coati/Edic mini calls/each call type/bop.wav') #call comes from 9471_1_FL4 

#MECHANICAL SOUNDS - UNK FUNCTION
snorts <- readWave('F:/PhD/All things coati/Edic mini calls/each call type/snorts.wav') ##from file 3 6887 edic mini

#SLEEP SOUNDS
snore_hum <- readWave('F:/PhD/All things coati/Edic mini calls/each call type/snore.wav') #from 9480_1_FL8
hum <- readWave('F:/PhD/All things coati/Edic mini calls/each call type/sleephum.wav') #from 9480_1_FL8

#BABY COATI SOUNDS
baby_coati <- readWave('F:/PhD/All things coati/Edic mini calls/each call type/babycoati.wav') #from Lydia's whatsapp recordings
baby_chitters <- readWave('F:/PhD/All things coati/Edic mini calls/each call type/babychitters.wav') #from Chris recordings
baby_mews <- readWave('F:/PhD/All things coati/Edic mini calls/each call type/babymews.wav') #from Chris recordings
baby_purr <- readWave('F:/PhD/All things coati/Edic mini calls/each call type/babypurr.wav') #from Chris recordings
baby_whines <- readWave('F:/PhD/All things coati/Edic mini calls/each call type/babywhines.wav') #from Chris recordings
baby_whistle <- readWave('F:/PhD/All things coati/Edic mini calls/each call type/babywhistle.wav') #from Chris recordings


#plot amplitude envelope
# env <- env(growl, f, channel = 1, envt = "hil", 
#            msmooth = NULL, ksmooth = NULL, ssmooth = NULL,
#            asmooth = NULL,
#            fftw = FALSE, norm = FALSE,
#            plot = T, alab= "Amplitude (dB)", yaxt= "s")


#------------GROWL---------------------------------
spectro(growl,f=22050,ovlp=85,zp=16, palette=jet.colors, osc=TRUE, collevels=seq(-29,0), grid = F, heights = 1.6, main = "growl", cexlab = 2)
cutgrowl <- cutw(growl,f=22050,from=0.020, to = 0.54 ,plot=T)
spectro(cutgrowl,f=22050,ovlp=85,zp=16, palette=jet.colors, osc=TRUE, collevels=seq(-29,0), grid = F, flog = F,heights = 1.6, main = "growl")
growl_dat <- meanspec(cutgrowl,f=22050,plot=T)

prop1 <- as.data.frame(specprop(growl_dat,f=22050))
prop1$call_type <- "growl"
#only keeping these measures:
#mode: dominant frequency
#median: median frequency
#sh: Shannon entropy - The Shannon spectral entropy of a noisy signal will tend towards 1 whereas the Shannon spectral entropy of a pure tone signal will tend towards 0
prop1 <- prop1[, c("mode", "median", "sh")] 

#Acoustic complexity index
prop1$ACI <- ACI(growl, 22050, channel = 1, wl = 512, ovlp = 0,  wn = "hamming", flim = NULL, nbwindows = 1)
#Entropy
prop1$entropy <- H(growl, 22050, channel = 1, wl = 512, envt="hil", msmooth = NULL, ksmooth = NULL)
prop1$duration <- duration(growl, 22050, channel=1)
prop1$name <- "growl"

growl_all <- prop1

#----------SQUEAL--------------
spectro(squeal,f=22050,ovlp=85,zp=16, palette=jet.colors, osc=TRUE, collevels=seq(-29,0), grid = F, heights = 1.7, main = "squeal")
cutsqueal <- cutw(squeal,f=22050,from=0.055, to = 0.20 ,plot=T)
spectro(cutsqueal,f=22050,ovlp=85,zp=16, palette=jet.colors, osc=TRUE, collevels=seq(-29,0), grid = F)
squeal_dat <- meanspec(cutsqueal,f=22050,plot=T)

prop2 <- as.data.frame(specprop(squeal_dat,f=22050))
prop2$call_type <- "squeal"
prop2 <- prop2[, c("mode", "median", "sh")] 

#Acoustic complexity index
prop2$ACI <- ACI(cutsqueal, 22050, channel = 1, wl = 512, ovlp = 0,  wn = "hamming", flim = NULL, nbwindows = 1)
#Entropy
prop2$entropy <- H(cutsqueal, 22050, channel = 1, wl = 512, envt="hil", msmooth = NULL, ksmooth = NULL)
prop2$duration <- duration(cutsqueal, 22050, channel=1)
prop2$name <- "squeal"

squeal_all <- prop2


#----------SQUEAL GRUNT--------------
spectro(squeal_grunt,f=22050,ovlp=85,zp=16, palette=jet.colors, osc=TRUE, collevels=seq(-50,0), grid = F, heights = 2.1, main = "squeal grunt")
cutsqueal_grunt1 <- cutw(squeal_grunt,f=22050,from=0.55, to = 0.7 ,plot=T)
spectro(cutsqueal_grunt1,f=22050,ovlp=85,zp=16, palette=jet.colors, osc=TRUE, collevels=seq(-50,0), grid = F)

cutsqueal_grunt <- cutw(squeal_grunt,f=22050,from=0.6, to = 0.7 ,plot=T)
spectro(cutsqueal_grunt,f=22050,ovlp=85,zp=16, palette=jet.colors, osc=TRUE, collevels=seq(-50,0), grid = F)
squeal_grunt_dat <- meanspec(cutsqueal_grunt,f=22050,plot=T)

prop3 <- as.data.frame(specprop(squeal_grunt_dat,f=22050))
prop3$call_type <- "squeal_grunt"
prop3 <- prop3[, c("mode", "median", "sh")] 

#Acoustic complexity index
prop3$ACI <- ACI(cutsqueal_grunt, 22050, channel = 1, wl = 512, ovlp = 0,  wn = "hamming", flim = NULL, nbwindows = 1)
#Entropy
prop3$entropy <- H(cutsqueal_grunt, 22050, channel = 1, wl = 512, envt="hil", msmooth = NULL, ksmooth = NULL)
prop3$duration <- duration(cutsqueal_grunt, 22050, channel=1)
prop3$name <- "squeal_grunt"

squeal_grunt_all <- prop3

#----------CHITTER--------------
spectro(chittering,f=22050,ovlp=85,zp=16, palette=jet.colors, osc=TRUE, collevels=seq(-29,0), grid = F, heights = 2.5, main = "chittering")
cutchitter <- cutw(chittering,f=22050,from=0.23, to = 0.328 ,plot=T)
spectro(cutchitter,f=22050,ovlp=85,zp=16, palette=jet.colors, osc=TRUE, collevels=seq(-29,0), grid = F, heights = 1.9, main = "chitter")
chitter_dat <- meanspec(cutchitter,f=22050,plot=T)

prop4 <- as.data.frame(specprop(chitter_dat,f=22050))
prop4$call_type <- "chitter"
prop4 <- prop4[, c("mode", "median", "sh")] 

#Acoustic complexity index
prop4$ACI <- ACI(cutchitter, 22050, channel = 1, wl = 512, ovlp = 0,  wn = "hamming", flim = NULL, nbwindows = 1)
#Entropy
prop4$entropy <- H(cutchitter, 22050, channel = 1, wl = 512, envt="hil", msmooth = NULL, ksmooth = NULL)
prop4$duration <- duration(cutchitter, 22050, channel=1)
prop4$name <- "chitter"

chitter_all <- prop4

#----------LONG_CHITTER--------------
spectro(long_chitter,f=22050,ovlp=85,zp=16, palette=jet.colors, osc=TRUE, collevels=seq(-29,0), grid = F, heights = 1.8)
cutlong_chitter <- cutw(long_chitter,f=22050,from=0.17, to = 0.31 ,plot=T)
spectro(cutlong_chitter,f=22050,ovlp=85,zp=16, palette=jet.colors, osc=TRUE, collevels=seq(-29,0), grid = F)
long_chitter_dat <- meanspec(cutlong_chitter,f=22050,plot=T)

prop5 <- as.data.frame(specprop(long_chitter_dat,f=22050))
prop5$call_type <- "long_chitter"
prop5 <- prop5[, c("mode", "median", "sh")] 

#Acoustic complexity index
prop5$ACI <- ACI(cutlong_chitter, 22050, channel = 1, wl = 512, ovlp = 0,  wn = "hamming", flim = NULL, nbwindows = 1)
#Entropy
prop5$entropy <- H(cutlong_chitter, 22050, channel = 1, wl = 512, envt="hil", msmooth = NULL, ksmooth = NULL)
prop5$duration <- duration(cutlong_chitter, 22050, channel=1)
prop5$name <- "long_chitter"

long_chitter_all <- prop5

#----------CHIRP_GRUNT--------------
spectro(chirp_grunt,f=22050,ovlp=85,zp=16, palette=jet.colors, osc=TRUE, collevels=seq(-29,0), grid = F, heights = 1.8, main = "chirp grunt")
cutchirp_grunt <- cutw(chirp_grunt,f=22050,from=0.09, to = 0.29 ,plot=T)
spectro(cutchirp_grunt,f=22050,ovlp=85,zp=16, palette=jet.colors, osc=TRUE, collevels=seq(-29,0), grid = F, main = "chirp grunt")
chirp_grunt_dat <- meanspec(cutchirp_grunt,f=22050,plot=T)

prop6 <- as.data.frame(specprop(chirp_grunt_dat,f=22050))
prop6$call_type <- "chirp_grunt"
prop6 <- prop6[, c("mode", "median", "sh")] 

#Acoustic complexity index
prop6$ACI <- ACI(cutchirp_grunt, 22050, channel = 1, wl = 512, ovlp = 0,  wn = "hamming", flim = NULL, nbwindows = 1)
#Entropy
prop6$entropy <- H(cutchirp_grunt, 22050, channel = 1, wl = 512, envt="hil", msmooth = NULL, ksmooth = NULL)
prop6$duration <- duration(cutchirp_grunt, 22050, channel=1)
prop6$name <- "chirp_grunt"

chirp_grunt_all <- prop6

#----------chirp_click_grunt--------------
#png(height = 600, width = 800, units = 'px', filename = paste0(plot_dir, "chirpclickgrunt.png"))

spectro(chirp_click_grunt,f=24000,ovlp=85,zp=16, palette=jet.colors, osc=TRUE, collevels=seq(-48,0), grid = F, heights = c(3,1.2), cexlab = 2, cexaxis = 1.5, scalecexlab = 1.5, oma = rep(1,4), widths = c(6,1), flab = "", alab = "")
#title(xlab="Time (s)", line=2.2, cex.lab=2)
title(ylab="Frequency (kHz)", line=2.2, cex.lab=2.3)

#dev.off()

cutchirp_click_grunt <- cutw(chirp_click_grunt,f=24000,from=0.131, to = 0.35 ,plot=T)

spectro(cutchirp_click_grunt, f=24000, ovlp=85, zp=16, palette=jet.colors, osc=TRUE, collevels=seq(-47,0), grid = F, heights = 1.8, main = "chirp click grunt")
chirp_click_grunt_dat <- meanspec(cutchirp_click_grunt,f=24000,plot=T)

prop7 <- as.data.frame(specprop(chirp_click_grunt_dat, f=24000))
prop7$call_type <- "chirp_click_grunt"
prop7 <- prop7[, c("mode", "median", "sh")] 

#Acoustic complexity index
prop7$ACI <- ACI(cutchirp_click_grunt, 24000, channel = 1, wl = 512, ovlp = 0,  wn = "hamming", flim = NULL, nbwindows = 1)
#Entropy
prop7$entropy <- H(cutchirp_click_grunt, 24000, channel = 1, wl = 512, envt="hil", msmooth = NULL, ksmooth = NULL)
prop7$duration <- duration(cutchirp_click_grunt, 24000, channel=1)
prop7$name <- "chirp_click_grunt"

chirp_click_grunt_all <- prop7

#----------chirp_grunt_snort--------------
spectro(chirp_grunt_snort,f=22050,ovlp=85,zp=16, palette=jet.colors, osc=TRUE, collevels=seq(-50,0), grid = F, heights = 1.8, main = "chirp click grunt snort")
cutchirp_grunt_snort <- cutw(chirp_grunt_snort,f=22050,from=0.25, to = 0.53 ,plot=T)
spectro(cutchirp_grunt_snort,f=22050,ovlp=85,zp=16, palette=jet.colors, osc=TRUE, collevels=seq(-50,0), grid = F, heights = 1.8, main = "chirp click grunt snort")
chirp_grunt_snort_dat <- meanspec(cutchirp_grunt_snort,f=22050,plot=T)

prop8 <- as.data.frame(specprop(chirp_grunt_snort_dat,f=22050))
prop8$call_type <- "chirp_grunt_snort"
prop8 <- prop8[, c("mode", "median", "sh")] 

#Acoustic complexity index
prop8$ACI <- ACI(cutchirp_grunt_snort, 22050, channel = 1, wl = 512, ovlp = 0,  wn = "hamming", flim = NULL, nbwindows = 1)
#Entropy
prop8$entropy <- H(cutchirp_grunt_snort, 22050, channel = 1, wl = 512, envt="hil", msmooth = NULL, ksmooth = NULL)
prop8$duration <- duration(cutchirp_grunt_snort, 22050, channel=1)
prop8$name <- "chirp_grunt_snort"

chirp_grunt_snort_all <- prop8

#----------grunt--------------
spectro(grunt,f=24000,ovlp=85,zp=16, palette=jet.colors, osc=TRUE, collevels=seq(-40,0), grid = F, flog = T, heights = 1.8)
cutgrunt <- cutw(grunt,f=24000,from=0.14, to = 0.32 ,plot=T)
spectro(cutgrunt,f=24000,ovlp=85,zp=16, palette=jet.colors, osc=TRUE, collevels=seq(-40,0), grid = F, flog = T)
grunt_dat <- meanspec(cutgrunt,f=24000,plot=T)

prop9 <- as.data.frame(specprop(grunt_dat,f=24000))
prop9$call_type <- "grunt"
prop9 <- prop9[, c("mode", "median", "sh")] 

#Acoustic complexity index
prop9$ACI <- ACI(cutgrunt, 24000, channel = 1, wl = 512, ovlp = 0,  wn = "hamming", flim = NULL, nbwindows = 1)
#Entropy
prop9$entropy <- H(cutgrunt, 24000, channel = 1, wl = 512, envt="hil", msmooth = NULL, ksmooth = NULL)
prop9$duration <- duration(cutgrunt, 24000, channel=1)
prop9$name <- "grunt"

grunt_all <- prop9

#----------click--------------
spectro(clicks,f=32000,ovlp=85,zp=16, palette=jet.colors3, osc=TRUE, collevels=seq(-35,0), grid = F, heights = 2.3, main = "clicks")
cutclick <- cutw(clicks,f=32000,from=0.455, to = 0.52, plot=T)
spectro(cutclick,f=32000,ovlp=85,zp=16, palette=jet.colors, osc=TRUE, collevels=seq(-40,0), grid = F)
click_dat <- meanspec(cutclick,f=32000, plot=T)

prop10 <- as.data.frame(specprop(click_dat,f=32000))
prop10$call_type <- "click"
prop10 <- prop10[, c("mode", "median", "sh")] 

#Acoustic complexity index
prop10$ACI <- ACI(cutclick, f=32000, channel = 1, wl = 512, ovlp = 0,  wn = "hamming", flim = NULL, nbwindows = 1)
#Entropy
prop10$entropy <- H(cutclick, f=32000, channel = 1, wl = 512, envt="hil", msmooth = NULL, ksmooth = NULL)
prop10$duration <- duration(cutclick, f=32000, channel=1)
prop10$name <- "click"

click_all <- prop10

#----------chirp--------------
spectro(chirp2,f=24000,ovlp=85,zp=16, palette=jet.colors, osc=TRUE, collevels=seq(-50,0), grid = F, heights = 1.8)
cutchirp <- cutw(chirp2,f=24000,from=0.275, to = 0.407 ,plot=T)
cutchirp1 <- cutw(chirp2,f=24000,from=0.22, to = 0.46 ,plot=T)
spectro(cutchirp1,f=24000,ovlp=85,zp=16, palette=jet.colors, osc=TRUE, collevels=seq(-50,0), grid = F, heights = 1.8, main = "chirp")

spectro(cutchirp,f=24000,ovlp=85,zp=16, palette=jet.colors, osc=TRUE, collevels=seq(-50,0), grid = F)
chirp_dat <- meanspec(cutchirp,f=24000,plot=T)
flim=c(4,6)

prop11 <- as.data.frame(specprop(chirp_dat,f=24000, flim=c(5,11)))
prop11$call_type <- "chirp"
prop11 <- prop11[, c("mode", "median", "sh")] 

#Acoustic complexity index
prop11$ACI <- ACI(cutchirp, 24000, channel = 1, wl = 512, ovlp = 0,  wn = "hamming", flim = NULL, nbwindows = 1)
#Entropy
prop11$entropy <- H(cutchirp, 24000, channel = 1, wl = 512, envt="hil", msmooth = NULL, ksmooth = NULL)
prop11$duration <- duration(cutchirp, 24000, channel=1)
prop11$name <- "chirp"

chirp_all <- prop11

#----------chop--------------
spectro(chop_chop,f=22050,ovlp=85,zp=16, palette=jet.colors, osc=TRUE, collevels=seq(-49,0), grid = F, heights = 2.3, main = "chop-chop")
cutchop <- cutw(chop_chop,f=22050,from=1.35, to = 1.47 ,plot=T)
spectro(cutchop,f=22050,ovlp=85,zp=16, palette=jet.colors2, osc=TRUE, collevels=seq(-29,0), grid = F)
chop_dat <- meanspec(cutchop,f=22050,plot=T)

prop12 <- as.data.frame(specprop(chop_dat,f=22050))
prop12 <- prop12[, c("mode", "median", "sh")] 

#Acoustic complexity index
prop12$ACI <- ACI(cutchop, 22050, channel = 1, wl = 512, ovlp = 0,  wn = "hamming", flim = NULL, nbwindows = 1)
#Entropy
prop12$entropy <- H(cutchop, 22050, channel = 1, wl = 512, envt="hil", msmooth = NULL, ksmooth = NULL)
prop12$duration <- duration(cutchop, 22050, channel=1)
prop12$name <- "chop"

chop_all <- prop12

#----------bark--------------
#Chris recordings sample rate: 32000
spectro(bark, f = 48000,ovlp=85,zp=16, palette=jet.colors, osc=TRUE, collevels=seq(-55,0), grid = F, heights = 1.8, main = "bark")
cutbark <- cutw(bark,f = 48000,from=0.078, to = 0.29 ,plot=T)
spectro(cutbark,f=48000,ovlp=85,zp=16, palette=jet.colors, osc=TRUE, collevels=seq(-55,0), grid = F)
bark_dat <- meanspec(cutbark,f = 48000,plot=T)

prop13 <- as.data.frame(specprop(bark_dat,f = 48000))
prop13 <- prop13[, c("mode", "median", "sh")] 

#Acoustic complexity index
prop13$ACI <- ACI(cutbark, f = 48000, channel = 1, wl = 512, ovlp = 0,  wn = "hamming", flim = NULL, nbwindows = 1)
#Entropy
prop13$entropy <- H(cutbark, f = 48000, channel = 1, wl = 512, envt="hil", msmooth = NULL, ksmooth = NULL)
prop13$duration <- duration(cutbark, f = 48000, channel=1)
prop13$name <- "bark"

bark_all <- prop13

#----------dolphin_call--------------
spectro(dolphin_call,f=24000,ovlp=85,zp=16, palette=jet.colors3, osc=TRUE, collevels=seq(-54,0), grid = F, heights = 2.7, main = "dolphin call")
cutdolphin_call <- cutw(dolphin_call,f=24000,from=0.70, to = 0.754 ,plot=T)
spectro(cutdolphin_call,f=24000,ovlp=85,zp=16, palette=jet.colors2, osc=TRUE, collevels=seq(-29,0), grid = F)
dolphin_call_dat <- meanspec(cutdolphin_call,f=24000,plot=T)

prop14 <- as.data.frame(specprop(dolphin_call_dat,f=24000))
prop14$call_type <- "dolphin_call"
prop14 <- prop14[, c("mode", "median", "sh")] 

#Acoustic complexity index
prop14$ACI <- ACI(cutdolphin_call, 24000, channel = 1, wl = 512, ovlp = 0,  wn = "hamming", flim = NULL, nbwindows = 1)
#Entropy
prop14$entropy <- H(cutdolphin_call, 24000, channel = 1, wl = 512, envt="hil", msmooth = NULL, ksmooth = NULL)
prop14$duration <- duration(cutdolphin_call, 24000, channel=1)
prop14$name <- "dolphin_call"

dolphin_call_all <- prop14

#----------bop--------------
spectro(bop,f=24000,ovlp=85,zp=16, palette=jet.colors, osc=TRUE, collevels=seq(-35,0), grid = F, heights = 2.1, main = "bop")
cutbop <- cutw(bop,f=24000,from=0.16, to = 0.225 ,plot=T)
spectro(cutbop,f=24000,ovlp=85,zp=16, palette=jet.colors, osc=TRUE, collevels=seq(-35,0), grid = F)
bop_dat <- meanspec(cutbop,f=24000,plot=T)

prop15 <- as.data.frame(specprop(bop_dat,f=24000))
prop15$call_type <- "bop"
prop15 <- prop15[, c("mode", "median", "sh")] 

#Acoustic complexity index
prop15$ACI <- ACI(cutbop, 24000, channel = 1, wl = 512, ovlp = 0,  wn = "hamming", flim = NULL, nbwindows = 1)
#Entropy
prop15$entropy <- H(cutbop, 24000, channel = 1, wl = 512, envt="hil", msmooth = NULL, ksmooth = NULL)
prop15$duration <- duration(cutbop, 24000, channel=1)
prop15$name <- "bop"

bop_all <- prop15

#----------snort--------------
spectro(snorts,f=22050,ovlp=85,zp=16, palette=jet.colors, osc=TRUE, collevels=seq(-40,0), grid = F, heights = 2.5, main = "snorting")
cutsnort <- cutw(snorts,f=22050,from=0.76, to = 0.93 ,plot=T)
spectro(cutsnort,f=22050,ovlp=85,zp=16, palette=jet.colors, osc=TRUE, collevels=seq(-35,0), grid = F)
snort_dat <- meanspec(cutsnort,f=22050,plot=T)

prop16 <- as.data.frame(specprop(snort_dat,f=22050))
prop16$call_type <- "snort"
prop16 <- prop16[, c("mode", "median", "sh")] 

#Acoustic complexity index
prop16$ACI <- ACI(cutsnort, 22050, channel = 1, wl = 512, ovlp = 0,  wn = "hamming", flim = NULL, nbwindows = 1)
#Entropy
prop16$entropy <- H(cutsnort, 22050, channel = 1, wl = 512, envt="hil", msmooth = NULL, ksmooth = NULL)
prop16$duration <- duration(cutsnort, 22050, channel=1)
prop16$name <- "snort"

snort_all <- prop16

#----------snore_hum--------------
spectro(snore_hum,f=24000,ovlp=85,zp=16, palette=jet.colors3, osc=TRUE, collevels=seq(-42,0), grid = F, heights = 1.8, flog = F)
cutsnore_hum <- cutw(snore_hum,f=24000,from=0.48, to = 1.26 ,plot=T)
spectro(cutsnore_hum,f=24000,ovlp=85,zp=16, palette=jet.colors4, osc=TRUE, collevels=seq(-45,0), grid = F)
snore_hum_dat <- meanspec(cutsnore_hum,f=24000,plot=T)

prop17 <- as.data.frame(specprop(snore_hum_dat,f=24000, flim=c(0.2,1)))
prop17$call_type <- "snore_hum"
prop17 <- prop17[, c("mode", "median", "sh")] 

#Acoustic complexity index
prop17$ACI <- ACI(cutsnore_hum, 24000, channel = 1, wl = 512, ovlp = 0,  wn = "hamming", flim = NULL, nbwindows = 1)
#Entropy
prop17$entropy <- H(cutsnore_hum, 24000, channel = 1, wl = 512, envt="hil", msmooth = NULL, ksmooth = NULL)
prop17$duration <- duration(cutsnore_hum, 24000, channel=1)
prop17$name <- "snore_hum"

snore_hum_all <- prop17

#----------hum--------------
spectro(hum,f=24000,ovlp=85,zp=16, palette=jet.colors3, osc=TRUE, collevels=seq(-45,0), grid = F, heights = 1.8, flog = F, main = "hum")
cuthum <- cutw(hum,f=24000,from=0.45, to = 1.4 ,plot=T)
spectro(cuthum,f=24000,ovlp=85,zp=16, palette=jet.colors3, osc=TRUE, collevels=seq(-45,0), grid = F,heights = 1.8, main = "hum")
hum_dat <- meanspec(cuthum,f=24000,plot=T)

prop18 <- as.data.frame(specprop(hum_dat,f=24000, flim=c(0.2,1)))
prop18$call_type <- "hum"
prop18 <- prop18[, c("mode", "median", "sh")] 

#Acoustic complexity index
prop18$ACI <- ACI(cuthum, 24000, channel = 1, wl = 512, ovlp = 0,  wn = "hamming", flim = NULL, nbwindows = 1)
#Entropy
prop18$entropy <- H(cuthum, 24000, channel = 1, wl = 512, envt="hil", msmooth = NULL, ksmooth = NULL)
prop18$duration <- duration(cuthum, 24000, channel=1)
prop18$name <- "hum"

hum_all <- prop18

#----------cub_chitter--------------
spectro(baby_coati,f=44100,ovlp=85,zp=16, palette=jet.colors4, osc=TRUE, collevels=seq(-40,0), grid = F, heights = 1.8, flog = F)
cutcub_chitter <- cutw(baby_coati,f=44100,from=0.43, to = 0.49 ,plot=T)
spectro(cutcub_chitter,f=44100,ovlp=85,zp=16, palette=jet.colors4, osc=TRUE, collevels=seq(-40,0), grid = F)
cub_chitter_dat <- meanspec(cutcub_chitter,f=44100,plot=T)

prop19 <- as.data.frame(specprop(cub_chitter_dat,f=44100))
prop19$call_type <- "cub_chitter"
prop19 <- prop19[, c("mode", "median", "sh")] 

#Acoustic complexity index
prop19$ACI <- ACI(cutcub_chitter, 44100, channel = 1, wl = 512, ovlp = 0,  wn = "hamming", flim = NULL, nbwindows = 1)
#Entropy
prop19$entropy <- H(cutcub_chitter, 44100, channel = 1, wl = 512, envt="hil", msmooth = NULL, ksmooth = NULL)
prop19$duration <- duration(cutcub_chitter, 44100, channel=1)
prop19$name <- "cub_chitter"

cub_chitter_all <- prop19

#----------cub_chitter2--------------
spectro(baby_chitters,f=44100,ovlp=85,zp=16, palette=jet.colors3, osc=TRUE, collevels=seq(-55,0), grid = F, heights = 1.8, flog = F)
cutcub_chitter <- cutw(baby_chitters,f=44100,from=0.02, to = 0.105 ,plot=T)
spectro(cutcub_chitter,f=44100,ovlp=85,zp=16, palette=jet.colors3, osc=TRUE, collevels=seq(-45,0), grid = F)
cub_chitter_dat <- meanspec(cutcub_chitter,f=44100,plot=T)

prop20 <- as.data.frame(specprop(cub_chitter_dat,f=44100))
prop20$call_type <- "cub_chitter"
prop20 <- prop20[, c("mode", "median", "sh")] 

#Acoustic complexity index
prop20$ACI <- ACI(cutcub_chitter, 44100, channel = 1, wl = 512, ovlp = 0,  wn = "hamming", flim = NULL, nbwindows = 1)
#Entropy
prop20$entropy <- H(cutcub_chitter, 44100, channel = 1, wl = 512, envt="hil", msmooth = NULL, ksmooth = NULL)
prop20$duration <- duration(cutcub_chitter, 44100, channel=1)
prop20$name <- "cub_chitter2"

cub_chitter2_all <- prop20

#----------cub_mews--------------
spectro(baby_mews,f=44100,ovlp=85,zp=16, palette=jet.colors3, osc=TRUE, collevels=seq(-45,0), grid = F, heights = 1.8, flog = F)
cutcub_mew <- cutw(baby_mews,f=44100,from=0.06, to = 0.18 ,plot=T)
spectro(cutcub_mew,f=44100,ovlp=85,zp=16, palette=jet.colors3, osc=TRUE, collevels=seq(-45,0), grid = F)
cub_mew_dat <- meanspec(cutcub_mew,f=44100,plot=T)

prop21 <- as.data.frame(specprop(cub_mew_dat,f=44100))
prop21$call_type <- "cub_mew"
prop21 <- prop21[, c("mode", "median", "sh")] 

#Acoustic complexity index
prop21$ACI <- ACI(cutcub_mew, 44100, channel = 1, wl = 512, ovlp = 0,  wn = "hamming", flim = NULL, nbwindows = 1)
#Entropy
prop21$entropy <- H(cutcub_mew, 44100, channel = 1, wl = 512, envt="hil", msmooth = NULL, ksmooth = NULL)
prop21$duration <- duration(cutcub_mew, 44100, channel=1)
prop21$name <- "cub_mew"

cub_mew_all <- prop21

#----------cub_purr--------------
spectro(baby_purr,f=44100,ovlp=85,zp=16, palette=jet.colors3, osc=TRUE, collevels=seq(-35,0), grid = F, heights = 1.8, flog = F)
cutcub_purr <- cutw(baby_purr,f=44100,from=0.06, to = 0.2 ,plot=T)
spectro(cutcub_purr,f=44100,ovlp=85,zp=16, palette=jet.colors3, osc=TRUE, collevels=seq(-35,0), grid = F)
cub_purr_dat <- meanspec(cutcub_purr,f=44100,plot=T)

prop22 <- as.data.frame(specprop(cub_purr_dat,f=44100))
prop22$call_type <- "cub_purr"
prop22 <- prop22[, c("mode", "median", "sh")] 

#Acoustic complexity index
prop22$ACI <- ACI(cutcub_purr, 44100, channel = 1, wl = 512, ovlp = 0,  wn = "hamming", flim = NULL, nbwindows = 1)
#Entropy
prop22$entropy <- H(cutcub_purr, 44100, channel = 1, wl = 512, envt="hil", msmooth = NULL, ksmooth = NULL)
prop22$duration <- duration(cutcub_purr, 44100, channel=1)
prop22$name <- "cub_purr"

cub_purr_all <- prop22

#----------cub_whines--------------
spectro(baby_whines,f=44100,ovlp=85,zp=16, palette=jet.colors3, osc=TRUE, collevels=seq(-35,0), grid = F, heights = 1.8, flog = F)
cutcub_whine <- cutw(baby_whines,f=44100,from=0.03, to = 0.65 ,plot=T)
spectro(cutcub_whine,f=44100,ovlp=85,zp=16, palette=jet.colors3, osc=TRUE, collevels=seq(-30,0), grid = F)
cub_whine_dat <- meanspec(cutcub_whine,f=44100,plot=T)

prop23 <- as.data.frame(specprop(cub_whine_dat,f=44100))
prop23$call_type <- "cub_whine"
prop23 <- prop23[, c("mode", "median", "sh")] 

#Acoustic complexity index
prop23$ACI <- ACI(cutcub_whine, 44100, channel = 1, wl = 512, ovlp = 0,  wn = "hamming", flim = NULL, nbwindows = 1)
#Entropy
prop23$entropy <- H(cutcub_whine, 44100, channel = 1, wl = 512, envt="hil", msmooth = NULL, ksmooth = NULL)
prop23$duration <- duration(cutcub_whine, 44100, channel=1)
prop23$name <- "cub_whine"

cub_whine_all <- prop23

#----------cub_whistle--------------
spectro(baby_whistle,f=44100,ovlp=85,zp=16, palette=jet.colors3, osc=TRUE, collevels=seq(-35,0), grid = F, heights = 1.8, flog = F)
cutcub_whistle <- cutw(baby_whistle,f=44100,from=0.1, to = 0.445 ,plot=T)
spectro(cutcub_whistle,f=44100,ovlp=85,zp=16, palette=jet.colors3, osc=TRUE, collevels=seq(-35,0), grid = F)
cub_whistle_dat <- meanspec(cutcub_whistle,f=44100,plot=T)

prop24 <- as.data.frame(specprop(cub_whistle_dat,f=44100))
prop24$call_type <- "cub_whistle"
prop24 <- prop24[, c("mode", "median", "sh")] 

#Acoustic complexity index
prop24$ACI <- ACI(cutcub_whistle, 44100, channel = 1, wl = 512, ovlp = 0,  wn = "hamming", flim = NULL, nbwindows = 1)
#Entropy
prop24$entropy <- H(cutcub_whistle, 44100, channel = 1, wl = 512, envt="hil", msmooth = NULL, ksmooth = NULL)
prop24$duration <- duration(cutcub_whistle, 44100, channel=1)
prop24$name <- "cub_whistle"

cub_whistle_all <- prop24

#combine properties of all calls
all_dat <- rbind(prop1, prop2, prop3, prop4, prop5, prop6, prop7, prop8, prop9, prop10, prop11, prop12, prop13, prop14, prop15, prop16, prop17, prop18, prop19, prop20, prop21, prop22, prop23)

all_dat2 <- rbind(growl_all, squeal_all ,squeal_grunt_all, chitter_all, long_chitter_all, chirp_grunt_all, chirp_click_grunt_all, chirp_grunt_snort_all, grunt_all, click_all, chirp_all, chop_all, bark_all, dolphin_call_all, bop_all, snort_all, snore_hum_all, hum_all, cub_chitter2_all, cub_mew_all, cub_purr_all, cub_whine_all, cub_whistle_all)

filt <- rbind(chirp_all,click_all,grunt_all,chitter_all,squeal_all,growl_all,chop_all, bark_all, dolphin_call_all, bop_all, snort_all, hum_all, cub_chitter2_all, cub_mew_all, cub_purr_all, cub_whine_all, cub_whistle_all)

filt2 <- filt[,c(7, 1:6)]
#colnames(filt2)[colnames(filt2) == "mode"] <- "Dominant Frequency (Hz)"

write.table(filt2,  file = "C:/Users/egrout/Dropbox/coaticalls/results/call_descriptions2.csv", quote = FALSE, sep ="\t" ,row.names = TRUE, col.names = TRUE)



#more calls to make spectrograms
cackle <- readWave('F:/PhD/All things coati/Edic mini calls/each call type/cackle2.wav')
spectro(cackle,f=96000,ovlp=85,zp=16, palette=jet.colors, osc=TRUE, collevels=seq(-59,0), grid = F, heights = 2.3, main = "cackle")
cutcackle <- cutw(cackle,f=96000,from=0.1, to = 0.545 ,plot=T)
spectro(cutcackle,f=96000,ovlp=85,zp=16, palette=jet.colors, osc=TRUE, collevels=seq(-59,0), grid = F, heights = 2.3, main = "cackling")

chuckle <- readWave('F:/PhD/All things coati/Edic mini calls/each call type/chuckles.wav')
spectro(chuckle,f=96000,ovlp=85,zp=16, palette=jet.colors, osc=TRUE, collevels=seq(-69,0), grid = F, heights = 2.3, main = "chuckling")
cutchuckle <- cutw(chuckle,f=96000,from=0.1, to = 0.6 ,plot=T)
spectro(cutchuckle,f=96000,ovlp=85,zp=16, palette=jet.colors, osc=TRUE, collevels=seq(-65,0), grid = F, heights = 2.3, main = "chuckling")

excite <- readWave('F:/PhD/All things coati/Edic mini calls/each call type/excitementchirps.wav')
spectro(excite,f=96000,ovlp=85,zp=16, palette=jet.colors, osc=TRUE, collevels=seq(-69,0), grid = F, heights = 2.3, main = "chuckling")
cutexcite <- cutw(excite,f=96000,from=0.1, to = 0.3 ,plot=T)
spectro(cutexcite,f=96000,ovlp=85,zp=16, palette=jet.colors, osc=TRUE, collevels=seq(-65,0), grid = F, heights = 2.3, main = "chirp squeal")


chirp_ultra <- readWave('F:/PhD/All things coati/Edic mini calls/each call type/chirpultra3.wav')

spectro(chirp_ultra,f=250000,ovlp=85,zp=16, palette=jet.colors, osc=TRUE, collevels=seq(-60,0), grid = F, heights = 2.3, main = "chirp", fastdisp=TRUE)
cutchirp_ultra <- cutw(chirp_ultra,f=250000,from=0.01, to = 0.13 ,plot=T)
spectro(cutchirp_ultra,f=250000,ovlp=85,zp=16, palette=jet.colors, osc=TRUE, collevels=seq(-60,0), grid = F, heights = 2.3, main = "chirp", fastdisp=F, flog = F, flim = c(0, 40))


chitter_ultra <- readWave('F:/PhD/All things coati/Edic mini calls/each call type/chitter_ultra.wav')

spectro(chitter_ultra,f=250000,ovlp=85,zp=16, palette=jet.colors, osc=TRUE, collevels=seq(-80,0), grid = F, heights = 2.3, main = "chitter", flim = c(0, 60))




# spectrograms for call sequences (without property info) for: AGG calls, chop chop, barking, dc
# call sequences in captivity are a combination of the different calls described here
#when mode was 0, a dominant frequency was not found









