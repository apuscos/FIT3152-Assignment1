correlation.webforum = clean.webforum[, 7:32]
correlation = as.data.frame(as.table(cor(correlation.webforum)))
correlation = correlation[!(correlation$Var1 == correlation$Var2),]

correlation$Freq = abs(correlation$Freq)
coe = as.data.frame(as.table(by(correlation, correlation$Var2, function(df) sum(df$Freq))))
coe = coe[order(-coe$Freq),]

print(coe[1:4,])


graph = ggplot(coe, aes(x = coe$correlation.Var2, y = coe$Freq, group = 1)) + geom_col() +
  theme(axis.text.x = element_text(angle = 270, hjust = 1)) + labs(x = "Attributes", y = "sum(cor)")
pdf("cor.pdf")
grid.arrange(graph)
dev.off()

remove(coe, correlation, correlation.webforum, graph)