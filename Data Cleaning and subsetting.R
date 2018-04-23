#install.packages("ggplot2")
#install.packages("zoo")
#install.packages("gridExtra")
library(zoo)
library(ggplot2)
library(gridExtra)
webforum = read.csv("webforum.csv")
clean.webforum = webforum[!(webforum$WC <= 10),] #If the word count of a post is less or equal to 10, ommit it

# omit posts that sended by anonymous
clean.webforum = clean.webforum[!(clean.webforum$AuthorID == -1),]

# omit the year which has less post number than average
numpost.year = as.data.frame(as.table(by(clean.webforum, format(as.Date(clean.webforum$Date,"%Y-%m-%d"), "%Y"), function(df) nrow(df))))
colnames(numpost.year) = c("year", "post")
choose.year = numpost.year[numpost.year$post > mean(numpost.year$post),]$year
remove(numpost.year)
clean.webforum = clean.webforum[format(as.Date(clean.webforum$Date,"%Y-%m-%d"), "%Y") %in% as.character(choose.year),]
remove(choose.year)

# omit threads where they have less posts than average
numpost.thread = as.data.frame(as.table(by(clean.webforum, clean.webforum$ThreadID, function(df) nrow(df))))
colnames(numpost.thread) = c("ThreadID", "Post")
choose.thread = as.data.frame(numpost.thread[numpost.thread$Post > mean(numpost.thread$Post),]$ThreadID)
colnames(choose.thread) = c("ThreadID")
remove(numpost.thread)
clean.webforum = clean.webforum[clean.webforum$ThreadID %in% choose.thread$ThreadID,]
remove(choose.thread)

# omit threads where they have less number of authors than 10
numauthor.thread = as.data.frame(as.table(by(clean.webforum, clean.webforum$ThreadID, function(df) length(unique(df$AuthorID)))))
colnames(numauthor.thread) = c("ThreadID", "Author")
choose.thread = as.data.frame(numauthor.thread[numauthor.thread$Author >= 10,]$ThreadID)
colnames(choose.thread) = c("ThreadID")
remove(numauthor.thread)
clean.webforum = clean.webforum[clean.webforum$ThreadID %in% choose.thread$ThreadID,]
remove(choose.thread)






