univariable.analysis = function(variable){
  string = deparse(substitute(variable))
  attach(clean.webforum)
  jpeg(filename = paste(string, " Histogram.jpeg", sep = ""))
  file.remove(paste(string, " univariable.txt", sep = ""))
  output = capture.output(summary(variable), sd(variable))    # Make the output to table
  cat(output, file=paste(string, " univariable.txt", sep = ""), sep="n")
  #value = log(variable)
  #hist(value, prob = TRUE, xlim = c(0, 6), main = "", xlab = paste("Log ", string, sep = ""))
  #x= seq(min(value),max(value), by = 0.01)
  #curve(dsnorm(x, mean = mean(value), sd = sd(value), xi = s), col = "red", lwd = 2, add = TRUE)
  hist(variable, prob = TRUE, main = "", xlab = string)
  dev.off()
  detach(clean.webforum)
}
univariable.analysis(Analytic)
univariable.analysis(Clout)
univariable.analysis(Authentic)
univariable.analysis(Tone)

attach(clean.webforum)
thread.id = unique(ThreadID)
length(t.id)

author.id = unique(AuthorID)
length(a.id)

date = unique(Date)
length(date)

detach(clean.webforum)

