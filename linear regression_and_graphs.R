####
# Linear Regression 
###
REGRA <- lm(data$dep ~ data$gender + data$motheredu + data$fatheredu + data$back + data$breadw)
REGRB <- lm(data$dep ~ data$gender + data$self + data$motheredu + data$fatheredu + data$back + data$breadw)
summary(REGRA) #summary for linear regressions
summary(REGRB)
# Plot a Boxplot
boxplot(dep~self, data, xlab="Self efficacy", ylab="department", 
        main="Self efficacy and STEM")
# Histograms for Self efficacy and Gender
library(scales)
bl <- adjustcolor("blue", alpha.f = 0.2)
bl
hist(data$self[data$gender==0], xlab="Self efficacy",ylab = "Sample",ylim=range(0,20),
     main="Self efficacy in male and female", col=c("pink","pink","pink","pink","pink",
                                                    "pink"),add=FALSE, border="grey")#male
hist(data$self[data$gender==1],xlab="Self efficacy",ylab = "Sample", ylim = range(0,20),
     col=c("#0000FF80","#0000FF80","#0000FF80","#0000FF80","#0000FF80",
           "#0000FF80"),add=TRUE,border="grey")#female
legend("topright" ,c("Male", "Female"),lty=c(1, 1),col=c("pink","blue"), bty = "n")
#Scatterplot Matrix
pairs(data$dep~ data$breadwinner + data$fatheredulevel+data$motheredulevel+
        data$back,main="Scatterplot Matrix")
###
# Perform the Bootstrapping for the sample, because even the constant was not statistically significant.
library("MASS")
model <- lm(data$dep ~ data$gender + selfeff, data=data, maxit=200)
summary(model)
#plot(model)
plot(model)
df %>%
  ggplot(aes(x=data$gender,y=data$dep)) +
  geom_point(alpha=0.5) +
  labs(x= "Gender", y="STEM level")

install.packages("Macro") #instal package 'process R' by going on the site 'http://www.processmacro.org/download.html'
#after that your computer will be able to read 'processR' as a package
install.packages(processR)
library(processR)
##after this run the process.something lines below and be sure that on the 
#environment under the section 'Functions' is activated 'process'
##
#WITH CONTROL VARIABLES- categorical
process(data=data, 
        y = "dep", 
        x="gender", 
        m="self", 
        model= 4, effsize=1, stand=1,modelbt=1,total=1,boot=10000, 
        cov=c("father_low", "father_high", "mother_low", "mother_high", 
              "mother_bread", "both_bread", "familyback"))
#TABLE PART
##
table(data$gender,data$dep)
table(data$gender,data$back)
#test for normal distribution of observations
shapiro.test(data$gender[data$dep=="0"])
shapiro.test(data$gender[data$dep=="1"])

##
##GRAPH PART
##
# Histograms
hist(data$gender[data$dep=="0"])
hist(data$gender[data$dep=="1"])
hist(data$gender[data$dep=="2"])
table(data$dep, data$gender)
#color trasparency col=rgb(0, 1, 0,0.5))
hist(data$gender[data$dep=="0"], xlab = "Gender", ylab = "People",xaxt='n',border="grey",
     main = "Slightly STEM",col = c("pink","pink", "pink","pink", "blue" ))
hist(data$gender[data$dep=="1"],xlab = "Gender", ylab = "People", xaxt='n',border="grey",
     ylim=range(0,25),main = "Moderately STEM",  add=FALSE, col = c("pink","pink", "pink","pink", "blue"))
hist(data$gender[data$dep=="2"],xlab = "Gender", ylab = "People",xaxt='n', border="grey",
     ylim=range(0,25),main = "Highly STEM",xaxt='n', add=FALSE, 
     col = c("pink","pink", "pink","pink", "blue" ))
legend("top" ,c("Male", "Female"),lty=c(1, 1),col=c("pink","blue"), bty = "n")
##
#color things
col2rgb("transparent")
c1 <- rgb(255,0,0,max = 255, alpha = 200, names = "lt.blue")
c2 <- rgb(0,255,0, max = 255, alpha = 100, names = "lt.pink")
c3 <- rgb(255,255,255, max= 255, alpha = 1, names= "transparent")
plot(h1, col=c1,xlab = "Gender", ylab = "People freq", 
     main = "STEM Level" )
plot(h2, add=FALSE,col=c2)
plot(h3, add=FALSE, col=c3)
hist(data$dep[data$gender==0])
hist(data$gender[data$back == "0"], xlab ="Gender", ylab="frequency",
     main = "Family Background")
hist(data$gender[familyback == "3"], xlab ="Gender", ylab="frequency",
     main = "Family Background")
curve (dnorm(x, mean=mean(data$breadwinner), 
             sd=sd(data$breadwinner)), add=FALSE, col="red")
curve (dnorm(x, mean=mean(data$self), 
             sd=sd(data$self)), add=FALSE, col="red")
hist(data$dep)
hist(data$gender)