# Find the number of posts for specific threadID and authorID
num.thread.author = as.data.frame(as.table(by(clean.webforum, list(clean.webforum$ThreadID, clean.webforum$AuthorID), function(df) nrow(df))))
num.thread.author = num.thread.author[complete.cases(num.thread.author),]
colnames(num.thread.author) = c("ThreadID", "AuthorID", "Post")

# Sort authors in every thread by the number of posts, and choose top ten authors for representing each thread
maxPost.authorID.thread = by(num.thread.author, num.thread.author$ThreadID, function(df) df[order(-df$Post),][1:10,])
maxPost.authorID.thread = do.call("rbind", maxPost.authorID.thread)
remove(num.thread.author)
maxPost.authorID.thread$ThreadID = as.integer(as.character(maxPost.authorID.thread$ThreadID))
maxPost.authorID.thread$AuthorID = as.integer(as.character(maxPost.authorID.thread$AuthorID))

# extract posts for specific author and thread
extract.webforum = clean.webforum
extract.webforum$TF = NA
for (row in seq(nrow(extract.webforum))){
  value.thread = extract.webforum$ThreadID[row]
  if (extract.webforum$AuthorID[row] %in% (maxPost.authorID.thread[maxPost.authorID.thread$ThreadID == value.thread,]$AuthorID)){
    extract.webforum$TF[row] = TRUE
  }
  else{
    extract.webforum$TF[row] = FALSE
  }
}
extract.webforum = extract.webforum[extract.webforum$TF == TRUE,]
remove(row)
remove(value.thread)

# Do the t-test for analytic
analytic.webforum = extract.webforum[, c(1,2,3,7)]
# Between top 5 authors of top 10 and bottom 5 authors of top 10
analytic.thread.ttest = as.data.frame(as.table(by(analytic.webforum, analytic.webforum$ThreadID, function(df) 
  t.test(df[df$AuthorID %in% maxPost.authorID.thread[maxPost.authorID.thread$ThreadID %in% df$ThreadID[1],]$AuthorID[1:5],]$Analytic, 
         df[df$AuthorID %in% maxPost.authorID.thread[maxPost.authorID.thread$ThreadID %in% df$ThreadID[1],]$AuthorID[6:10],]$Analytic)$p.value)))
colnames(analytic.thread.ttest) = c("ThreadID", "PV")
analytic.similar = nrow(analytic.thread.ttest[analytic.thread.ttest$PV>=0.05,])
analytic.notsimilar = nrow(analytic.thread.ttest[analytic.thread.ttest$PV<0.05,])
remove(analytic.webforum)
remove(analytic.thread.ttest)

#Do the t test for clout
clout.webforum = extract.webforum[, c(1,2,3,8)]
clout.thread.ttest = as.data.frame(as.table(by(clout.webforum, clout.webforum$ThreadID, function(df) t.test(df[df$AuthorID %in% maxPost.authorID.thread[maxPost.authorID.thread$ThreadID %in% df$ThreadID[1],]$AuthorID[1:5],]$Clout, df[df$AuthorID %in% maxPost.authorID.thread[maxPost.authorID.thread$ThreadID %in% df$ThreadID[1],]$AuthorID[6:10],]$Clout)$p.value)))
colnames(clout.thread.ttest) = c("ThreadID", "PV")
clout.similar = nrow(clout.thread.ttest[clout.thread.ttest$PV>=0.05,])
clout.notsimilar = nrow(clout.thread.ttest[clout.thread.ttest$PV<0.05,])
remove(clout.webforum)
remove(clout.thread.ttest)

# Do the t test for authentic
authentic.webforum = extract.webforum[, c(1,2,3,9)]
authentic.thread.ttest = as.data.frame(as.table(by(authentic.webforum, authentic.webforum$ThreadID, function(df) t.test(df[df$AuthorID %in% maxPost.authorID.thread[maxPost.authorID.thread$ThreadID %in% df$ThreadID[1],]$AuthorID[1:5],]$Authentic, df[df$AuthorID %in% maxPost.authorID.thread[maxPost.authorID.thread$ThreadID %in% df$ThreadID[1],]$AuthorID[6:10],]$Authentic)$p.value)))
colnames(authentic.thread.ttest) = c("ThreadID", "PV")
authentic.similar = nrow(authentic.thread.ttest[authentic.thread.ttest$PV>=0.05,])
authentic.notsimilar = nrow(authentic.thread.ttest[authentic.thread.ttest$PV<0.05,])
remove(authentic.webforum)
remove(authentic.thread.ttest)

# Do the t test for tone
tone.webforum = extract.webforum[, c(1,2,3,10)]
tone.thread.ttest = as.data.frame(as.table(by(tone.webforum, tone.webforum$ThreadID, function(df) t.test(df[df$AuthorID %in% maxPost.authorID.thread[maxPost.authorID.thread$ThreadID %in% df$ThreadID[1],]$AuthorID[1:5],]$Tone, df[df$AuthorID %in% maxPost.authorID.thread[maxPost.authorID.thread$ThreadID %in% df$ThreadID[1],]$AuthorID[6:10],]$Tone)$p.value)))
colnames(tone.thread.ttest) = c("ThreadID", "PV")
tone.similar = nrow(tone.thread.ttest[tone.thread.ttest$PV>=0.05,])
tone.notsimilar = nrow(tone.thread.ttest[tone.thread.ttest$PV<0.05,])
remove(tone.webforum)
remove(tone.thread.ttest)

remove(maxPost.authorID.thread)





