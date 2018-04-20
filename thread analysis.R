anonymous.webforum = extract.webforum[!(extract.webforum$AuthorID == -1),]
num.post.thread = as.data.frame(as.table(by(anonymous.webforum, anonymous.webforum$ThreadID, function(df) length(df$PostID))))
colnames(num.post.thread) = c("ThreadID", "Post")
summary(num.post.thread$Post)
choose.thread = as.data.frame(num.post.thread[num.post.thread$Post > mean(num.post.thread$Post),]$ThreadID)
colnames(choose.thread) = c("threadID")

thread.webforum = anonymous.webforum[anonymous.webforum$ThreadID %in% choose.thread$threadID,]

num.author.thread = as.data.frame(as.table(by(thread.webforum, thread.webforum$ThreadID, function(df) length(unique(df$AuthorID)))))
colnames(num.author.thread) = c("ThreadID", "Author")
choose.author = as.data.frame(num.author.thread[num.author.thread$Author >= 10,]$ThreadID)
colnames(choose.author) = c("threadID")

thread.webforum = thread.webforum[thread.webforum$ThreadID %in% choose.author$threadID,]

num.thread.author = as.data.frame(as.table(by(thread.webforum, list(thread.webforum$ThreadID, thread.webforum$AuthorID), function(df) length(df$PostID))))
# Calculate the number of post for each thread of each author
#num.thread.author = as.data.frame(as.table(with(thread.webforum, tapply(thread.webforum$PostID, list(ThreadID, AuthorID), length))))
num.thread.author = num.thread.author[complete.cases(num.thread.author),]
colnames(num.thread.author) = c("ThreadID", "AuthorID", "Post")

maxPost.authorID.thread = by(num.thread.author, num.thread.author$ThreadID, function(df) df[order(-df$Post),][1:10,])
maxPost.authorID.thread = do.call("rbind", maxPost.authorID.thread)
maxPost.authorID.thread$ThreadID = as.integer(as.character(maxPost.authorID.thread$ThreadID))
maxPost.authorID.thread$AuthorID = as.integer(as.character(maxPost.authorID.thread$AuthorID))
# Find the top 5 authors for each thread



thread.webforum$TF = NA
for (row in seq(nrow(thread.webforum))){
  value.thread = thread.webforum$ThreadID[row]
  if (thread.webforum$AuthorID[row] %in% (maxPost.authorID.thread[maxPost.authorID.thread$ThreadID == value.thread,]$AuthorID)){
    thread.webforum$TF[row] = TRUE
  }
  else{
    thread.webforum$TF[row] = FALSE
  }
}
# To get their posts and limit within 30 posts

out = thread.webforum[thread.webforum$TF == TRUE,]



# Do the t test by split posts of thread half
analytic.webforum = out[, c(1,2,3,7)]
analytic.thread.ttest = as.data.frame(as.table(by(analytic.webforum, analytic.webforum$ThreadID, function(df) t.test(df[df$AuthorID %in% maxPost.authorID.thread[maxPost.authorID.thread$ThreadID %in% df$ThreadID[1],]$AuthorID[1:5],]$Analytic, df[df$AuthorID %in% maxPost.authorID.thread[maxPost.authorID.thread$ThreadID %in% df$ThreadID[1],]$AuthorID[6:10],]$Analytic)$p.value)))
# Do the t test by split posts of thread half
colnames(analytic.thread.ttest) = c("ThreadID", "PV")
num.similar.analytic = nrow(analytic.thread.ttest[analytic.thread.ttest$PV>=0.05,])
num.notsimilar.analytic = nrow(analytic.thread.ttest[analytic.thread.ttest$PV<0.05,])
# Comparing

#Do the t test for analytic

clout.webforum = out[, c(1,2,3,8)]
clout.thread.ttest = as.data.frame(as.table(by(clout.webforum, clout.webforum$ThreadID, function(df) t.test(df[df$AuthorID %in% maxPost.authorID.thread[maxPost.authorID.thread$ThreadID %in% df$ThreadID[1],]$AuthorID[1:5],]$Clout, df[df$AuthorID %in% maxPost.authorID.thread[maxPost.authorID.thread$ThreadID %in% df$ThreadID[1],]$AuthorID[6:10],]$Clout)$p.value)))
colnames(clout.thread.ttest) = c("ThreadID", "PV")
num.similar.clout = nrow(clout.thread.ttest[clout.thread.ttest$PV>=0.05,])
num.notsimilar.clout = nrow(clout.thread.ttest[clout.thread.ttest$PV<0.05,])

authentic.webforum = out[, c(1,2,3,9)]
authentic.thread.ttest = as.data.frame(as.table(by(authentic.webforum, authentic.webforum$ThreadID, function(df) t.test(df[df$AuthorID %in% maxPost.authorID.thread[maxPost.authorID.thread$ThreadID %in% df$ThreadID[1],]$AuthorID[1:5],]$Authentic, df[df$AuthorID %in% maxPost.authorID.thread[maxPost.authorID.thread$ThreadID %in% df$ThreadID[1],]$AuthorID[6:10],]$Authentic)$p.value)))
colnames(authentic.thread.ttest) = c("ThreadID", "PV")
num.similar.authentic = nrow(authentic.thread.ttest[authentic.thread.ttest$PV>=0.05,])
num.notsimilar.authentic = nrow(authentic.thread.ttest[authentic.thread.ttest$PV<0.05,])

tone.webforum = out[, c(1,2,3,10)]
tone.thread.ttest = as.data.frame(as.table(by(tone.webforum, tone.webforum$ThreadID, function(df) t.test(df[df$AuthorID %in% maxPost.authorID.thread[maxPost.authorID.thread$ThreadID %in% df$ThreadID[1],]$AuthorID[1:5],]$Tone, df[df$AuthorID %in% maxPost.authorID.thread[maxPost.authorID.thread$ThreadID %in% df$ThreadID[1],]$AuthorID[6:10],]$Tone)$p.value)))
colnames(tone.thread.ttest) = c("ThreadID", "PV")
num.similar.tone = nrow(tone.thread.ttest[tone.thread.ttest$PV>=0.05,])
num.notsimilar.tone = nrow(tone.thread.ttest[tone.thread.ttest$PV<0.05,])





