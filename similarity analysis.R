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

# Do the t-test for social
social.webforum = extract.webforum[, c(1,2,3,23)]
# Between top 5 authors of top 10 and bottom 5 authors of top 10
social.thread.ttest = as.data.frame(as.table(by(social.webforum, social.webforum$ThreadID, function(df) 
  t.test(df[df$AuthorID %in% maxPost.authorID.thread[maxPost.authorID.thread$ThreadID %in% df$ThreadID[1],]$AuthorID[1:5],]$social, 
         df[df$AuthorID %in% maxPost.authorID.thread[maxPost.authorID.thread$ThreadID %in% df$ThreadID[1],]$AuthorID[6:10],]$social)$p.value)))
colnames(social.thread.ttest) = c("ThreadID", "PV")
g1 = ggplot(social.thread.ttest, aes(x = ThreadID, y = PV, group = 1)) + geom_point() + 
  theme(axis.text.x=element_blank()) + geom_hline(yintercept = 0.05) + 
  scale_y_continuous(breaks = c(0.00, 0.05, 0.25, 0.50, 0.75, 1.00)) + labs(x = "social")
social.similar = nrow(social.thread.ttest[social.thread.ttest$PV>=0.05,])
social.notsimilar = nrow(social.thread.ttest[social.thread.ttest$PV<0.05,])
remove(social.webforum)
remove(social.thread.ttest)

#Do the t test for clout
clout.webforum = extract.webforum[, c(1,2,3,8)]
clout.thread.ttest = as.data.frame(as.table(by(clout.webforum, clout.webforum$ThreadID, function(df) 
  t.test(df[df$AuthorID %in% maxPost.authorID.thread[maxPost.authorID.thread$ThreadID %in% df$ThreadID[1],]$AuthorID[1:5],]$Clout, 
         df[df$AuthorID %in% maxPost.authorID.thread[maxPost.authorID.thread$ThreadID %in% df$ThreadID[1],]$AuthorID[6:10],]$Clout)$p.value)))
colnames(clout.thread.ttest) = c("ThreadID", "PV")
g2 = ggplot(clout.thread.ttest, aes(x = ThreadID, y = PV, group = 1)) + geom_point() + 
  theme(axis.text.x=element_blank()) + geom_hline(yintercept = 0.05) + 
  scale_y_continuous(breaks = c(0.00, 0.05, 0.25, 0.50, 0.75, 1.00)) + labs(x = "clout")
clout.similar = nrow(clout.thread.ttest[clout.thread.ttest$PV>=0.05,])
clout.notsimilar = nrow(clout.thread.ttest[clout.thread.ttest$PV<0.05,])
remove(clout.webforum)
remove(clout.thread.ttest)

# Do the t test for ppron
ppron.webforum = extract.webforum[, c(1,2,3,11)]
ppron.thread.ttest = as.data.frame(as.table(by(ppron.webforum, ppron.webforum$ThreadID, function(df) 
  t.test(df[df$AuthorID %in% maxPost.authorID.thread[maxPost.authorID.thread$ThreadID %in% df$ThreadID[1],]$AuthorID[1:5],]$ppron, 
         df[df$AuthorID %in% maxPost.authorID.thread[maxPost.authorID.thread$ThreadID %in% df$ThreadID[1],]$AuthorID[6:10],]$ppron)$p.value)))
colnames(ppron.thread.ttest) = c("ThreadID", "PV")
g3 = ggplot(ppron.thread.ttest, aes(x = ThreadID, y = PV, group = 1)) + geom_point() + 
  theme(axis.text.x=element_blank()) + geom_hline(yintercept = 0.05) + 
  scale_y_continuous(breaks = c(0.00, 0.05, 0.25, 0.50, 0.75, 1.00)) + labs(x = "ppron")
ppron.similar = nrow(ppron.thread.ttest[ppron.thread.ttest$PV>=0.05,])
ppron.notsimilar = nrow(ppron.thread.ttest[ppron.thread.ttest$PV<0.05,])
remove(ppron.webforum)
remove(ppron.thread.ttest)

# Do the t test for negemo
negemo.webforum = extract.webforum[, c(1,2,3,20)]
negemo.thread.ttest = as.data.frame(as.table(by(negemo.webforum, negemo.webforum$ThreadID, function(df) 
  t.test(df[df$AuthorID %in% maxPost.authorID.thread[maxPost.authorID.thread$ThreadID %in% df$ThreadID[1],]$AuthorID[1:5],]$negemo, 
         df[df$AuthorID %in% maxPost.authorID.thread[maxPost.authorID.thread$ThreadID %in% df$ThreadID[1],]$AuthorID[6:10],]$negemo)$p.value)))
colnames(negemo.thread.ttest) = c("ThreadID", "PV")
g4 = ggplot(negemo.thread.ttest, aes(x = ThreadID, y = PV, group = 1)) + geom_point() + 
  theme(axis.text.x=element_blank()) + geom_hline(yintercept = 0.05) + 
  scale_y_continuous(breaks = c(0.00, 0.05, 0.25, 0.50, 0.75, 1.00)) + labs(x = "negemo")
  
negemo.similar = nrow(negemo.thread.ttest[negemo.thread.ttest$PV>=0.05,])
negemo.notsimilar = nrow(negemo.thread.ttest[negemo.thread.ttest$PV<0.05,])
remove(negemo.webforum)
remove(negemo.thread.ttest)

remove(maxPost.authorID.thread)

#Print out the result
print(paste("The number of threads that have similar social: ", as.character(social.similar)))
print(paste("The number of threads that have different social: ", as.character(social.notsimilar)))
print(paste("The number of threads that have similar clout: ", as.character(clout.similar)))
print(paste("The number of threads that have different clout: ", as.character(clout.notsimilar)))
print(paste("The number of threads that have similar ppron: ", as.character(ppron.similar)))
print(paste("The number of threads that have different ppron: ", as.character(ppron.notsimilar)))
print(paste("The number of threads that have similar negemo: ", as.character(negemo.similar)))
print(paste("The number of threads that have different negemo: ", as.character(negemo.notsimilar)))


pdf("PV.pdf")
grid.arrange(g1, g2, g3, g4)
dev.off()



remove(social.similar, social.notsimilar, clout.similar, clout.notsimilar, ppron.similar, ppron.notsimilar, negemo.similar, negemo.notsimilar)
remove(extract.webforum)
remove(g1,g2,g3,g4)

