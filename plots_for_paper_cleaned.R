#this code is for making all plot the same for the paper (by reading in all wave files into a list then running the spectro function over each call type in a forloop)

library(seewave)
library(tuneR)
library(ggplot2)
library(DT)
library(plyr)

plot_dir <- "C:/Users/egrout/Dropbox/coaticalls/results/spectrograms/"

jet.colors <- colorRampPalette(c("white","white", "plum1", "deeppink4", "blue", "black"))


Growl <- readWave('F:/PhD/All things coati/Edic mini calls/each call type/all/growl cut.wav')
Squeal <- readWave('F:/PhD/All things coati/Edic mini calls/each call type/all/squeal_down1.wav')
Squeal_grunt <- readWave('F:/PhD/All things coati/Edic mini calls/each call type/all/squeal grunt.wav')
Chittering1 <- readWave('F:/PhD/All things coati/Edic mini calls/each call type/all/chittering.wav')
Chittering <- readWave('F:/PhD/All things coati/Edic mini calls/each call type/all/chittering_ramseycanyon.wav')
Long_chitter <- readWave('F:/PhD/All things coati/Edic mini calls/each call type/all/long chitter.wav')
Squawk <- readWave('F:/PhD/All things coati/Edic mini calls/each call type/all/squawk5_ASDM_121217.wav')

#CONTACT
Chirp_grunt <- readWave('F:/PhD/All things coati/Edic mini calls/each call type/all/chirpgrunt_cut.wav')
Chirp_click_grunt <- readWave('F:/PhD/All things coati/Edic mini calls/each call type/all/chirpclickgrunt2.wav')
Chirp_grunt_snort <- readWave('F:/PhD/All things coati/Edic mini calls/each call type/all/chirpgruntsnort.wav')#from file 3 6887 edic mini
Grunt <- readWave('F:/PhD/All things coati/Edic mini calls/each call type/all/grunt.wav') #from 9480_1_FL8
Clicks <- readWave('F:/PhD/All things coati/Edic mini calls/each call type/all/clicks.wav') ##from Chris Hass Ramsey Canyon Coatis video
Chirp2 <-readWave('F:/PhD/All things coati/Edic mini calls/each call type/all/chirp2.wav') #from 9480_1_FL3

Squeak1 <- readWave('F:/PhD/All things coati/Edic mini calls/each call type/all/squeak1_ramseycanyon.wav')
Squeak2 <- readWave('F:/PhD/All things coati/Edic mini calls/each call type/all/squeak2_ramseycanyon.wav')
Squeak3 <- readWave('F:/PhD/All things coati/Edic mini calls/each call type/all/squeak3_ramseycanyon.wav')
Squeak4 <- readWave('F:/PhD/All things coati/Edic mini calls/each call type/all/squeak4_ramseycanyon.wav')
Squeak5 <- readWave('F:/PhD/All things coati/Edic mini calls/each call type/all/squeak5_ramseycanyon.wav')
Squeak <- readWave('F:/PhD/All things coati/Edic mini calls/each call type/all/squeak6_ramseycanyon.wav')

#AGGRESSIVE CALL SEQUENCES
agg_seq4 <- readWave('F:/PhD/All things coati/Edic mini calls/each call type/all/aggsequence4.wav')
agg_seq5 <- readWave('F:/PhD/All things coati/Edic mini calls/each call type/all/aggsequence5.wav')
agg_seq6 <- readWave('F:/PhD/All things coati/Edic mini calls/each call type/all/aggsequence6.wav')

#ALARM
Chop_chop <- readWave('F:/PhD/All things coati/Edic mini calls/each call type/all/chop chop.wav') 
Barking1 <- readWave('F:/PhD/All things coati/Edic mini calls/each call type/all/barking.wav') #from Chris Hass Ramsey Canyon Coatis video
Barking <- readWave('F:/PhD/All things coati/Edic mini calls/each call type/all/barking5.wav') #from Chris Hass Ramsey Canyon Coatis video
Barking2 <- readWave('F:/PhD/All things coati/Edic mini calls/each call type/all/barks_ramseycanyon.wav') #from Chris Hass Ramsey Canyon Coatis video
Chirp_bark <- readWave('F:/PhD/All things coati/Edic mini calls/each call type/all/barkcomplex.wav') #from Chris Hass Ramsey Canyon Coatis video
Bark <- readWave('F:/PhD/All things coati/Edic mini calls/each call type/all/bark_ramseycanyon.wav') #from Chris Hass Ramsey Canyon Coatis video
Roar <- readWave('F:/PhD/All things coati/Edic mini calls/each call type/all/roar20_LuceroTrap.wav')

#LOST CALL
Lost_call <- readWave('F:/PhD/All things coati/Edic mini calls/each call type/all/lostcall_benvid1383.wav')


#MATING
Dolphin_call1 <- readWave('F:/PhD/All things coati/Edic mini calls/each call type/all/dolphin call2.wav') #call comes from Galaxy 9463_2_FL1
Dolphin_call <- readWave('F:/PhD/All things coati/Edic mini calls/each call type/all/dolphin call2_amplified.wav') #call comes from Galaxy 9463_2_FL1

#SHORT CALLS - UNK FUNCTION
Bop <- readWave('F:/PhD/All things coati/Edic mini calls/each call type/all/bop.wav') #call comes from 9471_1_FL4 
Vibrate <-  readWave('F:/PhD/All things coati/Edic mini calls/each call type/all/vibrate30_6895_6.wav')

#MECHANICAL SOUNDS - UNK FUNCTION
Snorting <- readWave('F:/PhD/All things coati/Edic mini calls/each call type/all/snorts.wav') ##from file 3 6887 edic mini
Snort <- readWave('F:/PhD/All things coati/Edic mini calls/each call type/all/snort_Lucerotrap.wav')


#SLEEP SOUNDS
Snore_hum <- readWave('F:/PhD/All things coati/Edic mini calls/each call type/all/snore.wav') #from 9480_1_FL8
Hum <- readWave('F:/PhD/All things coati/Edic mini calls/each call type/all/sleephum.wav') #from 9480_1_FL8

#BABY COATI SOUNDS
Baby_chitters <- readWave('F:/PhD/All things coati/Edic mini calls/each call type/all/babycoati.wav') #from Lydia's whatsapp recordings
Baby_chitters1 <- readWave('F:/PhD/All things coati/Edic mini calls/each call type/all/babychitters.wav') #from Chris recordings
Baby_mews <- readWave('F:/PhD/All things coati/Edic mini calls/each call type/all/babymews.wav') #from Chris recordings
Baby_purr <- readWave('F:/PhD/All things coati/Edic mini calls/each call type/all/babypurr.wav') #from Chris recordings
Baby_whines <- readWave('F:/PhD/All things coati/Edic mini calls/each call type/all/babywhines.wav') #from Chris recordings
Baby_whistle <- readWave('F:/PhD/All things coati/Edic mini calls/each call type/all/babywhistle.wav') #from Chris recordings

#more calls to make spectrograms
Cackle <- readWave('F:/PhD/All things coati/Edic mini calls/each call type/all/cackle2.wav')
Cut_cackle <- cutw(cackle,f=96000,from=0.1, to = 0.545 ,plot=T)
Chuckle <- readWave('F:/PhD/All things coati/Edic mini calls/each call type/all/chuckles.wav')
Cut_Chuckle <- cutw(chuckle,f=96000,from=0.1, to = 0.6 ,plot=T)
Excite <- readWave('F:/PhD/All things coati/Edic mini calls/each call type/all/excitementchirps.wav')
Cut_Excite <- cutw(excite,f=96000,from=0.1, to = 0.3 ,plot=T)
Chirp <- readWave('F:/PhD/All things coati/Edic mini calls/each call type/all/chirpultra3_clipped.wav')
Chitter_Ultra <- readWave('F:/PhD/All things coati/Edic mini calls/each call type/all/chitter_ultra.wav')


#only have wave files wanted in the list in the global environemt
waves <- Filter(function(x) is(x, "Wave"), mget(ls()))
waves <- within(waves, rm("Excite", "Chuckle"))

for (i in 1:length(waves)){

  call = waves[[i]]
  f = call@samp.rate
  name = names(waves[i])
  
  name <- gsub("_", "-", name)
  #if wanting all call names to be in lowercase
  #tolower(name)
  
png(height = 1000, width = 1200, units = 'px', filename = paste0(plot_dir, name, ".png"))
#png(height = 1000, width = 1200, units = 'px', filename = paste0(plot_dir,"logged_scale/", name, ".png"))
  
par(mgp=c(3,2.5,0))
spectro(call, f=f, ovlp=85, zp=16, flog = F, palette=jet.colors, osc=T, collevels=seq(-50,0), grid = F, cexlab = 4, cexaxis = 4, scalecexlab = 3, width = c(9,1), oma = rep(6,4), 
        tlab = " ",
        flab = " ",
        alab = " ")

#adding axis labels because default settings cut them off when I want to zoom the spectrogram to be more readable
mtext(name, side = 1, line = -55, cex = 4)
mtext("Time (s)", side = 3, line = -74, cex = 4, las = 1)
mtext("Frequency (kHz)", side = 2, line = 6, cex = 4, las = 3)
mtext("Amplitude", side = 2, line = 6, cex = 3, las = 3, at = -1.5) #-1.5 when flog = F and -0.4 when flog = T

dev.off()

}

