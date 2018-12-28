
##the project to extract the tweets from twitter and perform emoji analysis on them 
library(twitteR)
library(reshape)

#keys to access the twitter api 

api_key <- 'lkB1oRR23DtnJP8UGAUP8qebW'
api_secret <- '9fOhDAv29JKLhOlNGKRes7Vbjr9QyQXtUC3ic2Yh96R36utmPo'
access_token <- '751247338014859264-vJJr27JzkHDHxiDw9MNIkbY0Mv8SCtI'
access_token_secret <- 'rPFKVpGAXvALfLyTzJSywWGJ2Y23VUNlctz1aQ6UaVt0r'

#connecting to twitter account and api
setup_twitter_oauth(api_key, api_secret, access_token, access_token_secret)

#getting tweets from twitter 
set.seed(20170202)

#topic of intrest 
ht <- '#nobannowall'

#search the twitter fro the certain hastag #nobannowall
tweets.raw <- searchTwitter(ht, n = 1000, lang = 'en', since = '2018-11-25', until = '2018-12-25')

#create a data frame with the tweets 
df <- twListToDF(strip_retweets(tweets.raw, strip_manual = TRUE, strip_mt = TRUE)); df$hashtag <- ht; df$created <- as.POSIXlt(df$created); df$text <- iconv(df$text, 'latin1', 'ASCII', 'byte'); df$url <- paste0('https://twitter.com/', df$screenName, '/status/', df$id); df <- rename(df, c(retweetCount = 'retweets'))
df.a <- subset(df, select = c(text, created, url, latitude, longitude, retweets, hashtag));


nrow(df.a)
head(df.a)

#write the data gather in the csv file format 
write.csv(df.a, paste0('tweets.cleaned_', format(min(df.a$created), '%m%d'), '-', format(max(df.a$created), '%m%d'), '_', ht, '_', Sys.Date(), '_', format(Sys.time(), '%H-%M-%S'), '_n', nrow(df.a), '.csv'), row.names = FALSE);

library(plyr)
library(ggplot2)
library(splitstackshape)
library(stringr)

#fnames <- c('tweets.cleaned_1218-1224_#nobannowall_2018-12-25_14-01-23_n608.csv')
#df <- do.call(rbind.fill, lapply(fnames, read.csv));

#vantage point 
df<-df.a

#getting the username from the url 
df$username <- substr(substr(df$url, 21, nchar(as.character(df$url))), 1, nchar(substr(df$url, 21, nchar(as.character(df$url))))-26);
tweets.full <- df
tweets.full$X <- NULL
tweets.full$z <- 1 

#### sanity checking
tweets.full$created <- as.POSIXlt(tweets.full$created)
min(tweets.full$created)
max(tweets.full$created)
median(tweets.full$created)
nrow(tweets.full)
length(unique(tweets.full$username))

##remove the duplicate data from the dataset.
#can use either url or username 
## to dedupe dataset by url
tweets.dupes <- tweets.full[duplicated(tweets.full$url), ]
nrow(tweets.full)
nrow(tweets.dupes)

tweets <- tweets.full[!duplicated(tweets.full$url), ]
tweets <- arrange(tweets, url)
row.names(tweets) <- NULL
tweets$tweetid <- as.numeric(row.names(tweets))
nrow(tweets);

tweets.final <- tweets;

## dedupe dataset by username
#tweets.dupes <- tweets.full[duplicated(tweets.full$username), ]
#nrow(tweets.full)
#nrow(tweets.dupes); 
#test <- subset(tweets, url %in% tweets.dupes$url)
#test <- test[with(test, order(url)), ];

#tweets <- tweets.full[!duplicated(tweets.full$username), ] 
#tweets <- arrange(tweets, url)
#row.names(tweets) <- NULL
#tweets$tweetid <- as.numeric(row.names(tweets))
#nrow(tweets);

#### READ IN EMOJI DICTIONARIES
emdict.la <- read.csv('emoticon_conversion_noGraphic.csv', header = F)
View(emdict.la)

#removing the row  
emdict.la <- emdict.la[-1, ]

row.names(emdict.la) <- NULL; 

#setting the names of the data frame 
names(emdict.la) <- c('unicode', 'bytes', 'name')

#assigning the id as the row number 
emdict.la$emojiid <- row.names(emdict.la);

emdict.jpb <- read.csv('emDict.csv', header = F)
emdict.jpb <- emdict.jpb[-1, ]
row.names(emdict.jpb) <- NULL
names(emdict.jpb) <- c('name', 'bytes', 'rencoding')
emdict.jpb$name <- tolower(emdict.jpb$name)
emdict.jpb$bytes <- NULL;

## merge dictionaries
emojis <- merge(emdict.la, emdict.jpb, by = 'name')
class(emojis$emojiid)

emojis$emojiid <- as.numeric(emojis$emojiid) 
emojis <- arrange(emojis, emojiid);


###### FIND TOP EMOJIS FOR A GIVEN SUBSET OF THE DATA
tweets <- tweets.final
df.s <- matrix(NA, nrow = nrow(tweets), ncol = ncol(emojis))
system.time(df.s <- sapply(emojis$rencoding, regexpr, tweets$text, ignore.case = T, useBytes = T));

rownames(df.s) <- 1:nrow(df.s)
colnames(df.s) <- 1:ncol(df.s)
df.t <- data.frame(df.s)
df.t$tweetid <- tweets$tweetid;

# merge in hashtag data from original tweets dataset
df.a <- subset(tweets, select = c(tweetid, hashtag)) 
df.u <- merge(df.t, df.a, by = 'tweetid')
df.u$z <- 1
df.u <- arrange(df.u, tweetid); 

tweets.emojis.matrix <- df.u;

## create emoji count dataset
df <- subset(tweets.emojis.matrix)[, c(2:843)]
count <- colSums(df > -1);
emojis.m <- cbind(count, emojis)
emojis.m <- arrange(emojis.m, desc(count))
emojis.count <- subset(emojis.m, count >= 1)
emojis.count$dens <- round(1000 * (emojis.count$count / nrow(tweets)), 1)
emojis.count$dens.sm <- (emojis.count$count + 1) / (nrow(tweets) + 1);

emojis.count$rank <- as.numeric(row.names(emojis.count));
emojis.count.p <- subset(emojis.count, select = c(name, dens, count, rank));

# print summary stats
subset(emojis.count.p, rank <= 10);

num.tweets <- nrow(tweets)

df.t <- rowSums(tweets.emojis.matrix[, c(2:843)] > -1)

num.tweets.with.emojis <- length(df.t[df.t > 0])

num.emojis <- sum(emojis.count$count)

min(tweets$created)

max(tweets$created)

median(tweets$created);
num.tweets
num.tweets.with.emojis
round(100 * (num.tweets.with.emojis / num.tweets), 1)
num.emojis
nrow(emojis.count);

##### MAKE BAR CHART OF TOP EMOJIS IN NEW DATASET
df.plot <- subset(emojis.count.p, rank <= 10)

xlab <- 'Rank' 
ylab <- 'Overall Frequency (per 1,000 Tweets)'
df.plot <- arrange(df.plot, name)

imgs <- lapply(paste0(df.plot$name, '.png'), png::readPNG); g <- lapply(imgs, grid::rasterGrob);
k <- 0.20 * (10/nrow(df.plot)) * max(df.plot$dens); df.plot$xsize <- k; df.plot$ysize <- k
df.plot <- arrange(df.plot, name);

#resume from here 
g1 <- ggplot(data = df.plot, aes(x = rank, y = dens)) +
  geom_bar(stat = 'identity', fill = 'dodgerblue4') +
  xlab(xlab) + ylab(ylab) +
  mapply(function(x, y, i) {
    annotation_custom(g[[i]], xmin = x-0.5*df.plot$xsize[i], xmax = x+0.5*df.plot$xsize[i], 
                      ymin = y-0.5*df.plot$ysize[i], ymax = y+0.5*df.plot$ysize[i])},
    df.plot$rank, df.plot$dens, seq_len(nrow(df.plot))) +
  scale_x_continuous(expand = c(0, 0), breaks = seq(1, nrow(df.plot), 1), labels = seq(1, nrow(df.plot), 1)) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 1.10 * max(df.plot$dens))) +
  theme(panel.grid.minor.y = element_blank(),
        axis.title.x = element_text(size = 10), axis.title.y = element_text(size = 14), 
        axis.text.x  = element_text(size = 8, colour = 'black'), axis.text.y  = element_text(size = 8, colour = 'black'));

g1
png(paste0('emoji_barchart_', as.Date(min(tweets$created)), '_', as.Date(max(tweets$created)), '_', Sys.Date(), '_', format(Sys.time(), '%H-%M-%S'), '_n', nrow(tweets), '.png'), 
    width = 6600, height = 4000, units = 'px', res = 1000);
g1; dev.off()

