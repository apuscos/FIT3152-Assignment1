
library(ggplot2)
webforum = read.csv("webforum.csv")
clean.webforum = webforum[!(webforum$WC <= 10),]