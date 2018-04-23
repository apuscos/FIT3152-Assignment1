time.webforum = clean.webforum

# Format date from year-mon-day to year-mon
time.webforum$Date = as.yearmon(time.webforum$Date, "%Y-%m-%d")

# Calculate the mean value of ppron for each month
month.mean.ppron = as.data.frame(as.table(by(time.webforum, time.webforum$Date, function(df) mean(df$ppron))))
colnames(month.mean.ppron) = c("Date", "ppron")
month.mean.ppron = month.mean.ppron[order(month.mean.ppron$Date),]
timeplot.ppron = ggplot(data=month.mean.ppron, aes(x=Date, y=ppron, group = 1)) + geom_line(color = "#00AFBB", size = 2) + 
  theme(axis.text.x = element_text(angle = 270, hjust = 1)) + geom_smooth(method = "lm",aes(group=1))
remove(month.mean.ppron)
# Do the t test for first 2.5years and next 2.5years
ppron.group1 = time.webforum[time.webforum$Date <= as.yearmon("2007-06", "%Y-%m"), c(1,2,3,4,11)]
ppron.group2 = time.webforum[time.webforum$Date >= as.yearmon("2007-07", "%Y-%m"), c(1,2,3,4,11)]
ttest.ppron = t.test(ppron.group1$ppron, ppron.group2$ppron)
remove(ppron.group1, ppron.group2)

# Calculate the mean value of clout for each month
month.mean.clout = as.data.frame(as.table(by(time.webforum, time.webforum$Date, function(df) mean(df$Clout))))
colnames(month.mean.clout) = c("Date", "clout")
timeplot.clout = ggplot(data=month.mean.clout, aes(x=Date, y=clout, group = 1)) + geom_line(color = "#00AFBB", size = 2) + 
  theme(axis.text.x = element_text(angle = 270, hjust = 1)) + geom_smooth(method = "lm",aes(group=1))
remove(month.mean.clout)
# Do the t test for first 2.5years and next 2.5 years
clout.group1 = time.webforum[as.yearmon(time.webforum$Date) <= as.yearmon("2007-06", "%Y-%m"), c(1,2,3,4,8)]
clout.group2 = time.webforum[as.yearmon(time.webforum$Date) >= as.yearmon("2007-07", "%Y-%m"), c(1,2,3,4,8)]
ttest.clout = t.test(clout.group1$Clout, clout.group2$Clout)
remove(clout.group1, clout.group2)

# Calculate the mean value of negemo for each month
month.mean.negemo = as.data.frame(as.table(by(time.webforum, time.webforum$Date, function(df) mean(df$negemo))))
colnames(month.mean.negemo) = c("Date", "negemo")
timeplot.negemo = ggplot(data=month.mean.negemo, aes(x=Date, y=negemo, group = 1)) + geom_line(color = "#00AFBB", size = 2) + 
  theme(axis.text.x = element_text(angle = 270, hjust = 1)) + geom_smooth(method = "lm",aes(group=1))
remove(month.mean.negemo)
# Do the t test for first 2.5years and next 2.5 years
negemo.group1 = time.webforum[as.yearmon(time.webforum$Date) <= as.yearmon("2007-06", "%Y-%m"), c(1,2,3,4,20)]
negemo.group2 = time.webforum[as.yearmon(time.webforum$Date) >= as.yearmon("2007-07", "%Y-%m"), c(1,2,3,4,20)]
ttest.negemo = t.test(negemo.group1$negemo, negemo.group2$negemo)
remove(negemo.group1, negemo.group2)

# Calculate the mean value of social for each month
month.mean.social = as.data.frame(as.table(by(time.webforum, time.webforum$Date, function(df) mean(df$social))))
colnames(month.mean.social) = c("Date", "social")
timeplot.social = ggplot(data=month.mean.social, aes(x=Date, y=social, group = 1)) + geom_line(color = "#00AFBB", size = 2) + 
  theme(axis.text.x = element_text(angle = 270, hjust = 1)) + geom_smooth(method = "lm",aes(group=1))
remove(month.mean.social)
# Do the t test for first 2.5years and next 2.5 years
social.group1 = time.webforum[as.yearmon(time.webforum$Date) <= as.yearmon("2007-06", "%Y-%m"), c(1,2,3,4,23)]
social.group2 = time.webforum[as.yearmon(time.webforum$Date) >= as.yearmon("2007-07", "%Y-%m"), c(1,2,3,4,23)]
ttest.social = t.test(social.group1$social, social.group2$social)
remove(social.group1, social.group2)

pdf("timeClout.pdf")
grid.arrange(timeplot.clout)
dev.off()

pdf("timePpron.pdf")
grid.arrange(timeplot.ppron)
dev.off()

pdf("timeSocial.pdf")
grid.arrange(timeplot.social)
dev.off()

pdf("timeNegemo.pdf")
grid.arrange(timeplot.negemo)
dev.off()

remove(timeplot.negemo, timeplot.clout, timeplot.ppron, timeplot.social)

remove(time.webforum)

print(ttest.ppron)

print(ttest.clout)

print(ttest.social)

print(ttest.negemo)

remove(ttest.negemo, ttest.clout, ttest.ppron, ttest.social)
