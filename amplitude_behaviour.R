# this script is using the audio data to infer the behaviour of the individual - resting vs foraging/moving

#question: is there a strong relationship with animal activity from ACC data and amplitude in audio data? If we don't have ACC data, can we infer behavioural states with audio and vice versa?

library(seewave) #https://rdocumentation.org/packages/seewave/versions/2.2.0
library(tuneR)
library(ggplot2)
library(DT)
library(plyr)
library(dplyr)
library(zoo)

#first loading in one wave file from the juvenile
test <- readWave("C:/Users/egrout/Desktop/Galaxy group/sorokas on coatis - raw/9460/1 - 25.12.21/FL1.WAV")

#resample to 1000Hz
test <- resamp(test, f = test@samp.rate, g = 1000, output = "Wave")

#if wanting to cut the wave
#testcut <- cutw(test,f=test@samp.rate,from=1045, to = 1050 ,plot=T, fastdisp = TRUE)

#get amplitude of wave
env <- env(test, 1000, channel = 1, envt = "hil", fastdisp=TRUE)

#make into dataframe to add time, so can get the rolling mean of the amplitude
env <- as.data.frame(env)
env$time <- 1:nrow(env)

#calculate rolling average of env for n seconds window
env <- env %>%
  mutate(env_mean = rollmean(V1, k=120, fill=NA, align='right'))

#because this takes too long with the raw data, taking every nth row and making new dataframe
df.new <-  env[seq(1, nrow(env), 60), ] #This creates an index from row 1 to nrow (number of rows of the table) every 5 rows. You can play with the starting point and the 5 to extract other sequences.


plot(df.new$V1, col = "blue", type = "l")
points(df.new$env_mean, col = "red", type = "l")


# #k means clustering the amplitude
# clust <- kmeans(env$V1, centers = 4, iter.max = 10, nstart = 5)
# env$clust <- clust$cluster
# 
# #replace Na's with 0's otherwise kmeans can't cluster
# env[is.na(env)] <- 0
# 
# clust_mean <- kmeans(env$env_mean, centers = 4, iter.max = 10, nstart = 5)
# env$clust_mean <- clust_mean$cluster


#do clustering with df.new for visualising
#k means clustering the amplitude
clust_df <- kmeans(df.new$V1, centers = 4, iter.max = 10, nstart = 5)
df.new$clust <- clust_df$cluster


#plot results of clusters
ggplot() +
  geom_point(data = df.new, aes(x = time, y = V1, color = clust))


#could do onset and offset times of loud vs quiet and compare to ACC







