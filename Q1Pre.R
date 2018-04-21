# Plot the histogram for Analytic, Clout, Authentic and Tone

analytic.hist = ggplot(clean.webforum, aes(x = clean.webforum$Analytic)) + geom_histogram(binwidth = 3) + labs(x = "Analytic")
clout.hist = ggplot(clean.webforum, aes(x = clean.webforum$Clout)) + geom_histogram(binwidth = 3) + labs(x = "Clout")
authentic.hist = ggplot(clean.webforum, aes(x = clean.webforum$Authentic)) + geom_histogram(binwidth = 3) + labs(x = "Authentic")
tone.hist = ggplot(clean.webforum, aes(x = clean.webforum$Tone)) + geom_histogram(binwidth = 3) + labs(x = "Tone")

pdf("histAttribute.pdf")
grid.arrange(analytic.hist, clout.hist, authentic.hist, tone.hist)
dev.off()

remove(analytic.hist, clout.hist, authentic.hist, tone.hist)

print(paste("The standard deviation for Analytic: ", as.character(sd(clean.webforum$Analytic))))
print(paste("The standard deviation for Clout: ", as.character(sd(clean.webforum$Clout))))
print(paste("The standard deviation for Authentic: ", as.character(sd(clean.webforum$Authentic))))
print(paste("The standard deviation for Tone: ", as.character(sd(clean.webforum$Tone))))