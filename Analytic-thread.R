calc.cv = function(col){
  sd = sd(col)
  avg = mean(col)
  cv = sd/avg
  return(cv)
}

attach(clean.webforum)
thread.analytic.cv = as.data.frame(as.table(by(clean.webforum[,7], clean.webforum[,2], function(df) calc.cv(df))))
colnames(thread.analytic.cv) = c("ThreadID", "Analytic.cv") 
thread.clout.cv = as.data.frame(as.table(by(clean.webforum[,8], clean.webforum[,2], function(df) calc.cv(df))))
colnames(thread.clout.cv) = c("ThreadID", "Clout.cv") 
thread.authentic.cv = as.data.frame(as.table(by(clean.webforum[,9], clean.webforum[,2], function(df) calc.cv(df))))
colnames(thread.authentic.cv) = c("ThreadID", "Authentic.cv") 
thread.tone.cv = as.data.frame(as.table(by(clean.webforum[,10], clean.webforum[,2], function(df) calc.cv(df))))
colnames(thread.tone.cv) = c("ThreadID", "Tone.cv") 


total = merge(thread.analytic.cv, thread.clout.cv,by = "ThreadID")
total = merge(total, thread.authentic.cv, by = "ThreadID")
total = merge(total, thread.tone.cv, by = "ThreadID")
detach(clean.webforum)


#par(mfrow = c(2,2))
#hist(thread.analytic.sd$Analytic)


#hist(thread.clout.sd$Clout)
#hist(thread.authentic.sd$Authentic)
#hist(thread.tone.sd$Tone)
par(mfrow = c(2,2))
attach(total)
hist(Analytic.cv)
summary(Analytic.cv)
hist(Clout.cv)
summary(Clout.cv)
hist(Authentic.cv)
summary(Authentic.cv)
hist(Tone.cv)
summary(Tone.cv)
detach(total)