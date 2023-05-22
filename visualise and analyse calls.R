library(seewave)
library(tuneR)
library(ggplot2)

#single call from edic mini

chirp.wav.file <- 'F:/PhD/All things coati/Edic mini calls/squeal_down.wav'
coati <-readWave(chirp.wav.file)
f=22050
par(mfrow=c(1,1), mar = c(3,5,1,3))
#par(mfrow=c(1,1), mar = c(1,1,1,1)) #order: bottom, left, top, and right
#plot the wave form
plot(coati)
f=22050

#upload single calls from edic mini recordings
squeal_down <- readWave('F:/PhD/All things coati/Edic mini calls/each call type/squeal_down.wav')
squeal_grunt <- readWave('F:/PhD/All things coati/Edic mini calls/each call type/squeal grunt.wav')
chirp_grunt <- readWave('F:/PhD/All things coati/Edic mini calls/each call type/chirpgrunt_cut.wav')
chittering <- readWave('F:/PhD/All things coati/Edic mini calls/each call type/chittering.wav')
long_chitter <- readWave('F:/PhD/All things coati/Edic mini calls/each call type/long chitter.wav')
growl <- readWave('F:/PhD/All things coati/Edic mini calls/each call type/growl cut.wav')

par(mfrow=c(2,3), mar = c(3.5,5,1,1)) #order: bottom, left, top, and right
#plot the wave form
plot(squeal_down, xlab = "", ylab = "", main = "squeal down", cex.main = 1, ylim = c(-3000000,3000000))
plot(squeal_grunt,xlab = "", ylab = "", main = "squeal grunt", cex.main = 1)
plot(chirp_grunt, xlab = "", ylab = "", main = "chirp grunt", cex.main = 1)
plot(chittering, xlab = "", ylab = "", main = "chittering", cex.main = 1, las = 1, cex.lab = 2)
#mtext("Amplitude/dB", side=2, line= 4, cex=0.8)
#mtext("time", side=1, line= 2, cex=0.8)
plot(long_chitter, xlab = "", ylab = "", main = "long chitter", cex.main = 1)
plot(growl, xlab = "", ylab = "", main = "growl", cex.main = 1, tck = -0.05)
dev.off()


#make quick spectrogram, flog so frequency is logarithmic not linear - can change to false
windows()
spectro(coati, f=22050, palette = rainbow, flog = F)
#2D spectrogram
spectro(coati, f=22050, ovlp=50, zp=16, collevels=seq(-40,0, 0.5), cont = F)

#you can cut the plot to the desired call

cutplot<-cutw(coati,f=22050,from=0.03, to = 0.23 ,plot=T)

spectro(cutplot,f=22050,ovlp=85,zp=16,osc=TRUE,
        cont=TRUE,contlevels=seq(-30,0,20),colcont="red",
        lwd=1.5,lty=2,palette=reverse.terrain.colors)

# black and white spectrogram ---- my favourite 
spectro(cutplot,f=22050,ovlp=85,zp=16,
        palette=reverse.gray.colors.1,osc=TRUE, collevels=seq(-29,0), grid = F)

# colour modifications
spectro(cutplot,f=22050,ovlp=85,zp=16,
        palette=reverse.cm.colors, osc=TRUE, colwave="orchid3", collevels=seq(-25, 0), grid = F) 

spectro(cutplot,f=22050,ovlp=85,zp=16,osc=TRUE,palette=reverse.heat.colors,
        colbg="black",colgrid="white", colwave="white",colaxis="white",collab="white", grid = F, listen = T)

#changing the colours
jet.colors <- colorRampPalette(c("white","white", "seashell","seashell1", "plum1", "deeppink4", "darkblue", "black"))

spectro(cutplot, f=22050, ovlp=75, palette=jet.colors, collevels=seq(-45,0,0.5))

#heat map spectrogram
v <- ggspectro(coati, ovlp=99, na.value="transparent", low="white", high="black") + theme_bw()
v + geom_tile(aes(fill = amplitude)) + scale_fill_gradient(low="white", high="grey30")
#this spectrogram doesn't show the call clearly and I can't manipulate the background colour to make the call look sharper


#can go on seewave website to play around with spectrogram visualisations
#want to get out call parameters


#Any colour palette can be used!
#In particular, it is possible to use other palettes coming with seewave: 
#temp.colors, reverse.gray.colors.1, reverse.gray.colors.2, reverse.heat.colors, reverse.terrain.colors, 
#reverse.topo.colors, reverse.cm.colors corresponding to the reverse of heat.colors, terrain.colors, topo.colors, cm.colors.

#---------------------analysis--------------------

#for plotting dominant frequency, need to cut the frequencies below 2kHz which is background noise
spectro(cutplot, f=22050, ovlp=80, zp=8, palette=reverse.gray.colors.1, scale=T, grid = F)
par(new=TRUE)
dfreq(cutplot, f=22050, ovlp=80, threshold= -15, type="p", col="red", lwd=2,  yaxt="n",  xaxt="n", ylab =" ", xlab = " ")


#cut frequencies below 2kHz
title(main="1.5 kHz")
c <- fir(cutplot, f=22050, from=2000, to=22050, bandpass=T, rescale = F )
spectro(c,f=22050,scale=FALSE)

#now plot dominant frequency again

par(mfrow=c(1,1))
spectro(c, f=22050, ovlp=80, zp=8, palette=reverse.gray.colors.1, scale=T, grid = F)
par(new=TRUE)
dfreq(c, f=22050, ovlp=70, threshold= 50, type="p", col="red", lwd=2,  yaxt="n",  xaxt="n", ylab =" ", xlab = " ")

#fundemental frequency
spectro(cutplot, f=f, ovlp=75, scale=T)
res <- autoc(cutplot,f=f, threshold=10, fmin=2000, fmax=10000, plot=FALSE)
points(res, pch=20)
legend(0.5,3.6, "Fundamental frequency", pch=20, col="black", bty=0, cex=0.7)

#this isn't particularly useful for this call type

#frequency peak detection
spec <- meanspec(cutplot, f=22050, plot=T)
specdB <- meanspec(cutplot, f=22050, dB="max0", plot=T)
# all peaks
fpeaks(spec)
# 10 highest peaks
fp10 <- fpeaks(spec, nmax=10)
# highest peak (ie dominant frequency)
fp1 <- fpeaks(spec, nmax=1, digits = T)

par(mfrow=c(2,1), mar = c(3,5,1,3))
spec(cutplot, f=f)
fpeaks(spec, nmax=1, digits = T)
dev.off()

#looking at signal entropy
#The entropy of a noisy signal will tend towards 1 whereas the entropy of a pure tone signal will tend towards 0.
H(cutplot,f=22050)
# changing the spectral parameter (wl)
H(cutplot,f=22050,wl=1024)
# changing the temporal parameter (msmooth)
H(cutplot,f=22050,msmooth=c(20,0))

#roughness of a curve
spec <- meanspec(cutplot, f=f, plot=T)[,2]
roughness(spec) 

#get table of spectral properties
a<-meanspec(cutplot,f=22050,plot=T)
properties <- as.data.frame(specprop(a,f=22050))

# to get a single measure of the list
specprop(a,f=22050)$mode
# to get the results structured
specprop(a,f=22050,str=TRUE)
# to limit the analysis between 2 and 10 kHz
specprop(a,f=22050,flim=c(2,10),str=TRUE)

par(mfrow=c(1,1), mar = c(3,5,1,3))
# plots
specprop(a,f=22050,plot=1)
specprop(a,f=22050,plot=2)

wf(cutplot,f=22050)
# changing the display parameters
jet.colors <- colorRampPalette(c("blue", "green"))
wf(cutplot,f=22050, hoff=0, voff=2, col=jet.colors, border = NA)
# matrix input instead of a time wave and transparent lines display
m <- numeric()
for(i in seq(-pi,pi,len=40)) {m <- cbind(m,10*(sin(seq(0,2*pi,len=100)+i)))}
wf(x=m, lines=TRUE, col="#0000FF50",xlab="Time", ylab="Amplitude",
   main="waterfall display")
##not sure what this is good for....



