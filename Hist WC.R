webforum = read.csv("webforum.csv")
attach(webforum)
hist(WC, main = "Word Count", xlab = "Word Count", xlim = c(0, max(WC)), ylim = c(0, 10000), breaks = 100)
detach(webforum)