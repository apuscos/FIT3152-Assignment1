thread.webforum = clean.webforum

thread.webforum = by(thread.webforum, thread.webforum$ThreadID, function(df) df[order(df$Date),])
thread.webforum = do.call("rbind", thread.webforum)

numMonth.thread = as.data.frame(as.table(by(thread.webforum, thread.webforum$ThreadID, function(df) length(unique(as.yearmon(df$Date))))))
colnames(numMonth.thread) = c("threadID", "Month")

choose.thread = numMonth.thread[numMonth.thread$Month>=4,]$threadID

thread.webforum = thread.webforum[thread.webforum$ThreadID %in% choose.thread,]

remove(choose.thread, numMonth.thread)


thread.webforum$Date = as.yearmon(thread.webforum$Date)

ttest = function(df){
  numberMonth = length(unique(as.yearmon(df$Date)))
  numberDateG1 = floor(numberMonth/2)
  numberDateG2 = numberMonth = numberDateG1
  DateG1 = unique(as.yearmon(df$Date))[1:numberDateG1]
  DateG2 = unique(as.yearmon(df$Date))[numberDateG1 +1: numberMonth]
  DataCloutG1 = df[as.yearmon(df$Date) %in% as.yearmon(DateG1),]$Clout
  DataCloutG2 = df[as.yearmon(df$Date) %in% as.yearmon(DateG2),]$Clout
  resultClout = t.test(DataCloutG1, DataCloutG2)$p.value
  
  DataPpronG1 = df[as.yearmon(df$Date) %in% as.yearmon(DateG1),]$ppron
  DataPpronG2 = df[as.yearmon(df$Date) %in% as.yearmon(DateG2),]$ppron
  resultPpron = t.test(DataPpronG1, DataPpronG2)$p.value
  
  DataNegemoG1 = df[as.yearmon(df$Date) %in% as.yearmon(DateG1),]$negemo
  DataNegemoG2 = df[as.yearmon(df$Date) %in% as.yearmon(DateG2),]$negemo
  resultNegemo = t.test(DataNegemoG1, DataNegemoG2)$p.value
  
  DataSocialG1 = df[as.yearmon(df$Date) %in% as.yearmon(DateG1),]$social
  DataSocialG2 = df[as.yearmon(df$Date) %in% as.yearmon(DateG2),]$social
  resultSocial = t.test(DataSocialG1, DataSocialG2)$p.value
  
  result = cbind(resultClout, resultPpron, resultNegemo, resultSocial)
  return(result)
}

ttest = by(thread.webforum, thread.webforum$ThreadID, function(df)  ttest(df))
ttest = do.call("rbind", ttest)
ttest= as.data.frame(ttest)

#Print out the result
print(paste("The number of threads social does not change by time: ", as.character(nrow(ttest[ttest$resultSocial>=0.05,]))))
print(paste("The number of threads that social does change by time: ", as.character(nrow(ttest[ttest$resultSocial<0.05,]))))
print(paste("The number of threads that clout does not change by time: ", as.character(nrow(ttest[ttest$resultClout>=0.05,]))))
print(paste("The number of threads that clout does change by time: ", as.character(nrow(ttest[ttest$resultClout<0.05,]))))
print(paste("The number of threads that ppron does not change by time: ", as.character(nrow(ttest[ttest$resultPpron>=0.05,]))))
print(paste("The number of threads that ppron does change by time: ", as.character(nrow(ttest[ttest$resultPpron<0.05,]))))
print(paste("The number of threads that negemo does not change by time: ", as.character(nrow(ttest[ttest$resultNegemo>=0.05,]))))
print(paste("The number of threads that negemo does not change by time: ", as.character(nrow(ttest[ttest$resultNegemo<0.05,]))))



ttest$ThreadID = unique(thread.webforum$ThreadID)

g1 = ggplot(ttest, aes(x = ThreadID, y = resultSocial, group = 1)) + geom_point() + 
  theme(axis.text.x=element_blank()) + geom_hline(yintercept = 0.05) + 
  scale_y_continuous(breaks = c(0.00, 0.05, 0.25, 0.50, 0.75, 1.00)) + labs(x = "social", y = "PV")

g2 = ggplot(ttest, aes(x = ThreadID, y = resultClout, group = 1)) + geom_point() + 
  theme(axis.text.x=element_blank()) + geom_hline(yintercept = 0.05) + 
  scale_y_continuous(breaks = c(0.00, 0.05, 0.25, 0.50, 0.75, 1.00)) + labs(x = "clout", y = "PV")

g3 = ggplot(ttest, aes(x = ThreadID, y = resultPpron, group = 1)) + geom_point() + 
  theme(axis.text.x=element_blank()) + geom_hline(yintercept = 0.05) + 
  scale_y_continuous(breaks = c(0.00, 0.05, 0.25, 0.50, 0.75, 1.00)) + labs(x = "ppron", y = "PV")

g4 = ggplot(ttest, aes(x = ThreadID, y = resultNegemo, group = 1)) + geom_point() + 
  theme(axis.text.x=element_blank()) + geom_hline(yintercept = 0.05) + 
  scale_y_continuous(breaks = c(0.00, 0.05, 0.25, 0.50, 0.75, 1.00)) + labs(x = "negemo", y = "PV")

pdf("timePV.pdf")
grid.arrange(g1,g2,g3,g4)
dev.off()

remove(g1,g2,g3,g4)
remove(ttest)


