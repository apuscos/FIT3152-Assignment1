time.webforum = thread.webforum

time.webforum$Date = format(as.Date(time.webforum$Date, "%Y-%m-%d"), "%Y-%m")
time.webforum$Date = factor(time.webforum$Date, levels = unique(time.webforum$Date))
#time.webforum$Date = as.numeric(as.character(time.webforum$Date))

year.total.ppron = as.data.frame(as.table(by(time.webforum, time.webforum$Date, function(df) mean(df$ppron))))
colnames(year.total.ppron) = c("Date", "ppron")
year.total.ppron$Date = as.yearmon(year.total.ppron$Date, "%Y-%m")
year.total.ppron = year.total.ppron[order(year.total.ppron$Date),]

year.total.ppron$Date = as.Date(year.total.ppron$Date)
year.total.ppron$Date = format(as.Date(year.total.ppron$Date, "%Y-%m-%d"), "%Y-%m")
ggplot(data=year.total.ppron, aes(x=Date, y=ppron, group = 1)) + geom_line(color = "#00AFBB", size = 2) + theme(axis.text.x = element_text(angle = 270, hjust = 1))

time.webforum$Date = as.yearmon(time.webforum$Date, "%Y-%m")
ppron.group1 = time.webforum[as.yearmon(time.webforum$Date) <= as.yearmon("2007-06", "%Y-%m"), c(1,2,3,4,11)]
ppron.group2 = time.webforum[as.yearmon(time.webforum$Date) >= as.yearmon("2007-07", "%Y-%m"), c(1,2,3,4,11)]
t.test(ppron.group1$ppron, ppron.group2$ppron)
time.webforum$Date = as.Date(time.webforum$Date)
time.webforum$Date = format(as.Date(time.webforum$Date, "%Y-%m-%d"), "%Y-%m")



month.mean.number = as.data.frame(as.table(by(time.webforum, time.webforum$Date, function(df) mean(df$number))))
colnames(month.mean.number) = c("Date", "Number")
ggplot(data=month.mean.number, aes(x=Date, y=Number, group = 1)) + geom_line(color = "#00AFBB", size = 2) + theme(axis.text.x = element_text(angle = 270, hjust = 1))
number.group1 = time.webforum[as.yearmon(time.webforum$Date) <= as.yearmon("2007-06", "%Y-%m"), c(1,2,3,4,17)]
number.group2 = time.webforum[as.yearmon(time.webforum$Date) >= as.yearmon("2007-07", "%Y-%m"), c(1,2,3,4,17)]
t.test(number.group1$number, number.group2$number)

month.mean.affect = as.data.frame(as.table(by(time.webforum, time.webforum$Date, function(df) mean(df$affect))))
colnames(month.mean.affect) = c("Date", "affect")
ggplot(data=month.mean.affect, aes(x=Date, y=affect, group = 1)) + geom_line(color = "#00AFBB", size = 2) + theme(axis.text.x = element_text(angle = 270, hjust = 1))
affect.group1 = time.webforum[as.yearmon(time.webforum$Date) <= as.yearmon("2007-06", "%Y-%m"), c(1,2,3,4,18)]
affect.group2 = time.webforum[as.yearmon(time.webforum$Date) >= as.yearmon("2007-07", "%Y-%m"), c(1,2,3,4,18)]
t.test(affect.group1$affect, affect.group2$affect)

month.mean.affect = as.data.frame(as.table(by(time.webforum, time.webforum$Date, function(df) mean(df$affect))))
colnames(month.mean.affect) = c("Date", "affect")
ggplot(data=month.mean.affect, aes(x=Date, y=affect, group = 1)) + geom_line(color = "#00AFBB", size = 2) + theme(axis.text.x = element_text(angle = 270, hjust = 1))
affect.group1 = time.webforum[as.yearmon(time.webforum$Date) <= as.yearmon("2007-06", "%Y-%m"), c(1,2,3,4,18)]
affect.group2 = time.webforum[as.yearmon(time.webforum$Date) >= as.yearmon("2007-07", "%Y-%m"), c(1,2,3,4,18)]
t.test(affect.group1$affect, affect.group2$affect)

mean.weforum = time.webforum[,11:32]
colMeans(mean.weforum)

month.mean.social = as.data.frame(as.table(by(time.webforum, time.webforum$Date, function(df) mean(df$social))))
colnames(month.mean.social) = c("Date", "social")
ggplot(data=month.mean.social, aes(x=Date, y=social, group = 1)) + geom_line(color = "#00AFBB", size = 2) + theme(axis.text.x = element_text(angle = 270, hjust = 1))
social.group1 = time.webforum[as.yearmon(time.webforum$Date) <= as.yearmon("2007-06", "%Y-%m"), c(1,2,3,4,23)]
social.group2 = time.webforum[as.yearmon(time.webforum$Date) >= as.yearmon("2007-07", "%Y-%m"), c(1,2,3,4,23)]
t.test(social.group1$social, social.group2$social)




