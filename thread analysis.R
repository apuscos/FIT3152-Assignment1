anonymous.webforum = extract.webforum[!(extract.webforum$AuthorID == -1),]
num.post.thread = as.data.frame(as.table(by(anonymous.webforum, anonymous.webforum$ThreadID, function(df) length(df$PostID))))
colnames(num.post.thread) = c("ThreadID", "Post")
summary(num.post.thread$Post)
choose.thread = as.data.frame(num.post.thread[num.post.thread$Post > mean(num.post.thread$Post),]$ThreadID)
colnames(choose.thread) = c("threadID")

thread.webforum = anonymous.webforum[anonymous.webforum$ThreadID %in% choose.thread$threadID,]

num.thread.author = as.data.frame(as.table(by(thread.webforum, list(thread.webforum$ThreadID, thread.webforum$AuthorID), function(df) length(df$PostID))))
#num.thread.author = as.data.frame(as.table(with(thread.webforum, tapply(thread.webforum$PostID, list(ThreadID, AuthorID), length))))
num.thread.author = num.thread.author[complete.cases(num.thread.author),]
colnames(num.thread.author) = c("ThreadID", "AuthorID", "Post")


maxPost.authorID.thread = by(num.thread.author, num.thread.author$ThreadID, function(df) df[which.max(df$Post),])
maxPost.authorID.thread = do.call("rbind", maxPost.authorID.thread)
maxPost.authorID.thread$ThreadID = as.character(maxPost.authorID.thread$ThreadID)
maxPost.authorID.thread$AuthorID = as.integer(as.character(maxPost.authorID.thread$AuthorID))

hashtable = new.env()
for (r in seq(nrow(maxPost.authorID.thread))){
  hashtable[[ maxPost.authorID.thread[r,1] ]] = maxPost.authorID.thread[r,2]
}

threadPost.hashtable = new.env()
for (r in seq(nrow(maxPost.authorID.thread))){
  threadPost.hashtable[[ maxPost.authorID.thread[r,1] ]] = maxPost.authorID.thread[r,3]
}

out = thread.webforum[0,]
for (row in seq(nrow(thread.webforum))){
  if(threadPost.hashtable[[as.character(thread.webforum[row,2])]] < 10){
    out = rbind(out, thread.webforum[row,])
  }
  else{
    if(thread.webforum[row, 3] == hashtable[[as.character(thread.webforum[row,2])]]){
      out = rbind(out, thread.webforum[row,])
    }
  }
}

topten.analytic.webforum = out[, c(1,2,3,7)]
topten.analytic.webforum = by(topten.analytic.webforum, topten.analytic.webforum$ThreadID, function(df) df[order(-df$Analytic),][1:10,])
topten.analytic.webforum = do.call("rbind", topten.analytic.webforum)

analytic.thread.ttest = as.data.frame(as.table(by(topten.analytic.webforum, topten.analytic.webforum$ThreadID, function(df) t.test(df$Analytic[1:5], y = df$Analytic[6:10])$p.value)))
colnames(analytic.thread.ttest) = c("ThreadID", "PV")
summary(analytic.thread.ttest$PV)
num.similar.analytic = nrow(analytic.thread.ttest[analytic.thread.ttest$PV>0.05,])
num.notsimilar.analytic = nrow(analytic.thread.ttest[analytic.thread.ttest$PV<=0.05,])

topten.clout.webforum = out[, c(1,2,3,8)]
topten.clout.webforum = by(topten.clout.webforum, topten.clout.webforum$ThreadID, function(df) df[order(-df$Clout),][1:10,])
topten.clout.webforum = do.call("rbind", topten.clout.webforum)

clout.thread.ttest = as.data.frame(as.table(by(topten.clout.webforum, topten.clout.webforum$ThreadID, function(df) t.test(df$Clout[1:5], y = df$Clout[6:10])$p.value)))
colnames(clout.thread.ttest) = c("ThreadID", "PV")
summary(clout.thread.ttest$PV)
num.similar.clout = nrow(clout.thread.ttest[clout.thread.ttest$PV>0.05,])
num.notsimilar.clout = nrow(clout.thread.ttest[clout.thread.ttest$PV<=0.05,])

topten.authentic.webforum = out[, c(1,2,3,9)]
topten.authentic.webforum = by(topten.authentic.webforum, topten.authentic.webforum$ThreadID, function(df) df[order(-df$Authentic),][1:10,])
topten.authentic.webforum = do.call("rbind", topten.authentic.webforum)

authentic.thread.ttest = as.data.frame(as.table(by(topten.authentic.webforum, topten.authentic.webforum$ThreadID, function(df) t.test(df$Authentic[1:5], y = df$Authentic[6:10])$p.value)))
colnames(authentic.thread.ttest) = c("ThreadID", "PV")
summary(authentic.thread.ttest$PV)
num.similar.authentic = nrow(authentic.thread.ttest[authentic.thread.ttest$PV>0.05,])
num.notsimilar.authentic = nrow(authentic.thread.ttest[authentic.thread.ttest$PV<=0.05,])

topten.tone.webforum = out[, c(1,2,3,10)]
topten.tone.webforum = by(topten.tone.webforum, topten.tone.webforum$ThreadID, function(df) df[order(-df$Tone),][1:10,])
topten.tone.webforum = do.call("rbind", topten.tone.webforum)


tone.ttest = function(col){
  if(length(unique(col)) == 1){
    return(1)
  }
  else{
    return(t.test(col[1:5], y = col[6:10])$p.value)
  }
}
tone.thread.ttest = as.data.frame(as.table(by(topten.tone.webforum, topten.tone.webforum$ThreadID, function(df) tone.ttest(df$Tone))))
colnames(tone.thread.ttest) = c("ThreadID", "PV")
summary(tone.thread.ttest$PV)
num.similar.tone = nrow(tone.thread.ttest[tone.thread.ttest$PV>0.05,])
num.notsimilar.tone = nrow(tone.thread.ttest[tone.thread.ttest$PV<=0.05,])