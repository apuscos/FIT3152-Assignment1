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

# Calculate the mean value of number for each month
month.mean.number = as.data.frame(as.table(by(time.webforum, time.webforum$Date, function(df) mean(df$number))))
colnames(month.mean.number) = c("Date", "number")
timeplot.number = ggplot(data=month.mean.number, aes(x=Date, y=number, group = 1)) + geom_line(color = "#00AFBB", size = 2) + 
  theme(axis.text.x = element_text(angle = 270, hjust = 1)) + geom_smooth(method = "lm",aes(group=1))
remove(month.mean.number)
# Do the t test for first 2.5years and next 2.5 years
number.group1 = time.webforum[as.yearmon(time.webforum$Date) <= as.yearmon("2007-06", "%Y-%m"), c(1,2,3,4,17)]
number.group2 = time.webforum[as.yearmon(time.webforum$Date) >= as.yearmon("2007-07", "%Y-%m"), c(1,2,3,4,17)]
ttest.number = t.test(number.group1$number, number.group2$number)
remove(number.group1, number.group2)

# Calculate the mean value of affect for each month
month.mean.affect = as.data.frame(as.table(by(time.webforum, time.webforum$Date, function(df) mean(df$affect))))
colnames(month.mean.affect) = c("Date", "affect")
timeplot.affect = ggplot(data=month.mean.affect, aes(x=Date, y=affect, group = 1)) + geom_line(color = "#00AFBB", size = 2) + 
  theme(axis.text.x = element_text(angle = 270, hjust = 1)) + geom_smooth(method = "lm",aes(group=1))
remove(month.mean.affect)
# Do the t test for first 2.5years and next 2.5 years
affect.group1 = time.webforum[as.yearmon(time.webforum$Date) <= as.yearmon("2007-06", "%Y-%m"), c(1,2,3,4,18)]
affect.group2 = time.webforum[as.yearmon(time.webforum$Date) >= as.yearmon("2007-07", "%Y-%m"), c(1,2,3,4,18)]
ttest.affect = t.test(affect.group1$affect, affect.group2$affect)
remove(affect.group1, affect.group2)

# Calculate the mean value of affect for each month
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

pdf("timeAffect.pdf")
grid.arrange(timeplot.affect)
dev.off()

pdf("timePpron.pdf")
grid.arrange(timeplot.ppron)
dev.off()

pdf("timeSocial.pdf")
grid.arrange(timeplot.social)
dev.off()

remove(timeplot.affect, timeplot.number, timeplot.ppron, timeplot.social)

remove(time.webforum)

print(ttest.ppron)

print(ttest.affect)

print(ttest.social)

remove(ttest.affect, ttest.number, ttest.ppron, ttest.social)
