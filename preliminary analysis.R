attach(clean.webforum)
# Format Year
num.post.year = as.data.frame(as.table(by(clean.webforum, format(as.Date(Date,"%Y-%m-%d"), "%Y"), function(df) length(df$PostID))))
detach(clean.webforum)
colnames(num.post.year) = c("Year", "Post")
attach(num.post.year)
ggplot(num.post.year, aes(x=Year, y=Post)) + geom_col()
num.post.year
summary(Post)
choose.year = num.post.year[Post > mean(Post),]$Year
choose.year
detach(num.post.year)

extract.webforum = clean.webforum[format(as.Date(clean.webforum$Date,"%Y-%m-%d"), "%Y") >= 2005,]
extract.webforum = extract.webforum[format(as.Date(extract.webforum$Date,"%Y-%m-%d"), "%Y") <= 2009,]

