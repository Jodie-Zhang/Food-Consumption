rm(list=ls())
install.packages("NLP")
library(NLP)
library(tm)


##### Create DTM #########
docs<-Corpus(DirSource("fpost"))
dtm <- DocumentTermMatrix(docs, control=list(tolower=T, removePunctuation=T, removeNumbers=T, 
                                             stripWhitespace=T, stopwords=c(stopwords("english"))))
dtm = removeSparseTerms(dtm,0.5)
idx <- rowSums(as.matrix(dtm))>0
newdocs <- docs[idx]
dtm = dtm[idx,]
dim(dtm)

###################################################
########### Explore cauliflower rice ############## 
########### freuency distribution #################
###################################################
####Cauliflower rice forms a trend in 2016

### Type in the words you want to measure
words<-c("cauliflower","rice","gluten","granules") ## the words related to cauliflower rice
### Transform freq into matrix ###
freq <- matrix(data=NA, nrow=dim(dtm)[1], ncol=length(words),dimnames = list(Docs(dtm),words))


#### Create a data frame of the picked words' frequency ###
for (i in 1:dim(dtm)[1]){
  freq[i, ] <- inspect(dtm[i,words])
}
freq <- as.data.frame(freq)

### Manipulate the data frame (order, date) ###
year <- 5  #### The number of year
nmonths <- year*12  #### The number of month in total

rownames(freq) <- substr(rownames(freq), start = 7, stop = nchar(rownames(freq))-4) 
for (i in 1:year) {
  rownames(freq)[i*12-10] <- paste(c(rownames(freq)[i*12],1),collapse = "")
  rownames(freq)[i*12-9] <- paste(c(rownames(freq)[i*12],2),collapse = "")
  rownames(freq)[i*12-8] <- paste(c(rownames(freq)[i*12],3),collapse = "")
}                   #### write a loop to substitute the month of "10","11","12" to be "91", "92", "93"
freq <- freq[ order(row.names(freq)), ]  #order the topic following the rowname order

freq$yearmonth <- seq(as.Date("2011/1/1"), by = "month", length.out = nmonths) ### add a column to denote the monthyear

### Transform wide data frame into long data frame ###
library(tidyr)
freq_long <- gather(freq, term, frequency, cauliflower:granules, factor_key=TRUE)

##### Time Series Data Visulization with ggplot #####
library(ggplot2)
### Plot the trend of word frequency-"cauliflower","rice","gluten","granules"
p1 <- ggplot(freq_long, aes(x = yearmonth, y = frequency)) + 
  geom_line(aes(color = term), size = 1) +
  scale_color_manual(values = c("#00AFBB", "#E7B800","#0072B2", "#CC79A7")) +
  theme_minimal()
p1

### Plot the trend of word frequency-"cauliflower","rice"
p2 <- ggplot(freq_long[freq_long$term==c("cauliflower","rice"), ], aes(x = yearmonth, y = frequency)) + 
  geom_line(aes(color = term), size = 1) +
  scale_color_manual(values = c("#00AFBB", "#E7B800")) +
  theme_minimal()
p2


###################################################
########### Explore vegetable noodle###############
########## frequency distribution #################
###################################################
####vegetable noodle forms a trend in 2015

### Type in the words you want to measure
words1<-c("veggie","vegetable","noodle","spiralizer") ## the words related to cauliflower rice
### Transform freq into matrix ###
freq1 <- matrix(data=NA, nrow=dim(dtm)[1], ncol=length(words1),dimnames = list(Docs(dtm),words1))


#### Create a data frame of the picked words' frequency ###
for (j in 1:dim(dtm)[1]){
  freq1[j, ] <- inspect(dtm[j,words1])
}
freq1 <- as.data.frame(freq1)

### Manipulate the data frame (order, date) ###
year <- 5  #### The number of year
nmonths <- year*12  #### The number of month in total

rownames(freq1) <- substr(rownames(freq1), start = 7, stop = nchar(rownames(freq))-4) 
for (i in 1:year) {
  rownames(freq1)[i*12-10] <- paste(c(rownames(freq1)[i*12],1),collapse = "")
  rownames(freq1)[i*12-9] <- paste(c(rownames(freq1)[i*12],2),collapse = "")
  rownames(freq1)[i*12-8] <- paste(c(rownames(freq1)[i*12],3),collapse = "")
}                   #### write a loop to substitute the month of "10","11","12" to be "91", "92", "93"
freq1 <- freq1[ order(row.names(freq1)), ]  #order the topic following the rowname order

freq1$yearmonth <- seq(as.Date("2011/1/1"), by = "month", length.out = nmonths) ### add a column to denote the monthyear

### Transform wide data frame into long data frame ###
library(tidyr)
freq1_long <- gather(freq1, term, frequency, veggie:spiralizer, factor_key=TRUE)

##### Time Series Data Visulization with ggplot #####
library(ggplot2)
install.packages("wesanderson")
library("wesanderson")
### Plot the trend of word frequency-"veggie","vegetable","noodle","spiralizer"
p3 <- ggplot(freq1_long, aes(x = yearmonth, y = frequency)) + 
  geom_line(aes(color = term), size = 1) +
  scale_color_manual(values = c( "#00AFBB", "#E7B800","#0072B2", "#CC79A7")) +
  theme_minimal()
p3

### Plot the trend of word frequency-"veggie","noodle"
p4 <- ggplot(freq1_long[freq1_long$term==c("veggie","spiralizer"), ], aes(x = yearmonth, y = frequency)) + 
  geom_line(aes(color = term), size = 1) +
  scale_color_manual(values = c("#0072B2", "#CC79A7")) +
  theme_minimal()
p4

#####################################################
############### Explore term frequency ##############
############### Proportion Distribution #############
#####################################################
#Find proportion of terms in all the documents in the year they became popular

#prepare a blank matrix for the next step
caulrice <- matrix(data=NA, nrow=dim(dtm)[1], ncol=2)
#create a data frame containing frequency proportion per document (monthyear) of cauliflower and rice
for (i in 1:dim(dtm)[1]){
  caulrice[i,1] <- colSums(inspect(dtm[i,"cauliflower"]))/rowSums(inspect(dtm[i, ]))
  caulrice[i,2] <- colSums(inspect(dtm[i,"rice"]))/rowSums(inspect(dtm[i, ]))
}

caulrice <- as.data.frame(caulrice)  
colnames(caulrice) <- c("cauliflower","rice")
caulrice$yearmonth <- freq$yearmonth

####Transform wide data frame into long data frame ###
caulrice_long<- gather(caulrice, term, freq_prob, cauliflower:rice, factor_key=TRUE)

##plot the frequency probability of caulrice
p5 <- ggplot(caulrice_long, aes(x = yearmonth, y = freq_prob)) + 
  geom_line(aes(color = term), size = 1) +
  scale_color_manual(values=c("#E69F00", "#56B4E9"))+
  theme_minimal()
p5

######## Repeat the process for veggie and spiralizer
vegspr <- matrix(data=NA, nrow=dim(dtm)[1], ncol=2)

for (i in 1:dim(dtm)[1]){
  vegspr[i,1] <- colSums(inspect(dtm[i,"veggie"]))/rowSums(inspect(dtm[i, ]))
  vegspr[i,2] <- colSums(inspect(dtm[i,"spiralizer"]))/rowSums(inspect(dtm[i, ]))
}

vegspr <- as.data.frame(vegspr)  
colnames(vegspr) <- c("veggie","spiralizer")
vegspr$yearmonth <- freq$yearmonth

vegspr_long<- gather(vegspr, term, freq_prob, veggie:spiralizer, factor_key=TRUE)


p6 <- ggplot(vegspr_long, aes(x = yearmonth, y = freq_prob)) + 
  geom_line(aes(color = term), size = 1) +
  scale_color_manual(values=c("#E69F00", "#56B4E9"))+
  theme_minimal()
p6


#############################################
######### Plot Moving Average Trend #########
#############################################

### Cauliflower
# create a dataframe containing moving average of cauliflower frequency by 4 months
caulfreq <- freq$cauliflower
caulmov <- filter(caulfreq, rep(1/4,4))
caulmov <- as.numeric(caulmov <- filter(caulfreq, rep(1/4,4)))
caulmov <- as.data.frame(caulmov[2:58])
names(caulmov) <- "cauliflowermov"
caul123 <- as.data.frame(caulfreq[1:3])
names(caul123) <- "cauliflowermov"
caulmov <- rbind(caul123,caulmov)
caulmov$yearmonth <- freq$yearmonth

### combine caulmov with actual caul frequency
caulfreq <- freq[ ,c("cauliflower","yearmonth")]
caul_wide <- merge(caulmov, caulfreq, bu="yearmonth")
## covert wide dataframe into long dataframe for plotting
caul_long<- gather(caul_wide, type, frequency, cauliflowermov:cauliflower, factor_key=TRUE)

## Plot cauliflower moving average and actual frequency
p7 <- ggplot(caul_long, aes(x = yearmonth, y = frequency)) + 
  geom_line(aes(color = type), size = 1) +
  scale_color_manual(values = c("#E69F00", "#56B4E9")) +
  theme_minimal()+ylim(0, 1000)
p7


### Repeat the process for vegetable noodles
# create a dataframe containing moving average of cauliflower frequency by 4 months
spifreq <- freq1$spiralizer
spimov <- filter(spifreq, rep(1/4,4))
spimov <- as.numeric(spimov <- filter(spifreq, rep(1/4,4)))
spimov <- as.data.frame(spimov[2:58])
colnames(spimov) <- "spiralizermov"
spi123 <- as.data.frame(spifreq[1:3])
names(spi123) <- "spiralizermov"
spimov <- rbind(spi123,spimov)
spimov$yearmonth <- freq1$yearmonth

### combine caulmov with actual caul frequency
spifreq<- freq1[ ,c("spiralizer","yearmonth")]
spi_wide <- merge(spimov, spifreq, bu="yearmonth")
## covert wide dataframe into long dataframe for plotting
spi_long<- gather(spi_wide, type, frequency, spiralizermov:spiralizer, factor_key=TRUE)

## Plot cauliflower moving average and actual frequency
p8 <- ggplot(spi_long, aes(x = yearmonth, y = frequency)) + 
  geom_line(aes(color = type), size = 1) +
  scale_color_manual(values = c("#E69F00", "#56B4E9")) +
  theme_minimal()+ylim(0, 1000)
p8
