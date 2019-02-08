library('dplyr')

data <- read.csv('liweb.csv') #load data
origdata <- data #cloning to keep without modifying
summary(data) 


data[,21:27] <- replace(data[,21:27],data[,21:27]=="" | data[,21:27]=="null",NA) #replacing blanks and nulls with NA

secdata <- data[,21:27] #getting just sec columns
secdata$id <- 1:nrow(secdata) # creating an id columns for melting to work
library('reshape')
secdata <- melt(secdata, id="id") # melting all 7 sec columns into 1 column
secfreq <- table(secdata$value) # getting count of each item in sec columns
secfreq <- secfreq[order(secfreq,decreasing = TRUE)] #sorting

par(mar=c(5.1, 4.1, 4.1, 2.1)+4,mgp=c(7,1,0)) # setting margins
barplot(secfreq[1:20],main="Top 20 Sec Items",las=2,ylab="Occurrence") # getting bar plot of top sec items
title(xlab="Label/Section")

data$seccount <- apply(data[,21:27],1, function(x) length(which(!is.na(x)))) #getting number of sec columns with values for each row


#Marcus Code
data$timestampest <- as.POSIXct(data$timestampest, format = "%Y-%m-%d %H:%M:%S")



tse <- data %>%
  group_by(uid) %>%
  summarize(recency = difftime(Sys.time(),max(timestampest, na.rm = TRUE),units="days"))

new1 <- merge(data, tse, all.x = TRUE, by="uid")



tof <- data %>%
  group_by(uid) %>%
  summarize(tof = difftime(Sys.time(),min(timestampest, na.rm = TRUE),units="days"))

new2 <- merge(new1, tof, all.x = TRUE, by="uid")


uid <- as.data.frame(table(data$uid))

colnames(uid) <- c("uid","freq")
new3 <- merge(new2, uid, all.x = TRUE, by="uid")

new3$perday <- new3$freq / 92

library('lattice')
# Professor Malthouse Code
summary.kmeans = function(fit)
{
  p = ncol(fit$centers)
  k = nrow(fit$centers)
  n = sum(fit$size)
  sse = sum(fit$withinss)
  xbar = t(fit$centers)%*%fit$size/n
  ssb = sum(fit$size*(fit$centers - rep(1,k) %*% t(xbar))^2)
  print(data.frame(
    n=c(fit$size, n),
    Pct=(round(c(fit$size, n)/n,2)),
    round(rbind(fit$centers, t(xbar)), 2),
    RMSE = round(sqrt(c(fit$withinss/(p*(fit$size-1)), rss/(p*(n-k)))), 4)
  ))
  cat("SSE = ", sse, "; SSB = ", ssb, "\n")
  cat("R-Squared = ", ssb/(ssb+sse), "\n")
  cat("Pseudo F = ", (ssb/(k-1))/(sse/(n-k)), "\n\n");
  invisible(list(sse=sse, ssb=ssb, Rsqr=ssb/(ssb+sse), F=(ssb/(k-1))/(sse/(n-k))))
}
plot.kmeans = function(fit,boxplot=F)
{
  require(lattice)
  p = ncol(fit$centers)
  k = nrow(fit$centers)
  plotdat = data.frame(
    mu=as.vector(fit$centers),
    clus=factor(rep(1:k, p)),
    var=factor( 0:(p*k-1) %/% k, labels=colnames(fit$centers))
  )
  print(dotplot(var~mu|clus, data=plotdat,
                panel=function(...){
                  panel.dotplot(...)
                  panel.abline(v=0, lwd=.1)
                },
                layout=c(k,1),
                xlab="Cluster Mean"
  ))
  invisible(plotdat)
}
source(kmeans.R)

#kmeans clustering
set.seed(12345)
fit = kmeans(new3[1:1000,c("freq","seccount")],3,100,100)
plot(fit)