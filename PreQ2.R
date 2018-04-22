mean.weforum = clean.webforum[,c(11, 17, 18, 21, 22, 23, 24, 25, 26, 27, 28, 29, 31, 32)]
mean.attribute = as.data.frame(as.table(colMeans(mean.weforum)))
colnames(mean.attribute) = c("attribute", "mean")
print(mean.attribute)
remove(mean.weforum)
attribute.mean.bar = ggplot(mean.attribute, aes(x = mean.attribute$attribute, y = mean.attribute$mean)) + geom_col() + 
  theme(axis.text.x = element_text(angle = 270, hjust = 1)) + labs(x = "attribute", y = "mean")


pdf("meanAttribute.pdf")
grid.arrange(attribute.mean.bar)
dev.off()

remove(mean.attribute)
remove(attribute.mean.bar)