
library(tidyverse)
library(dplyr)
library(ggplot2)

site_1_flat_rocks <- c(3, 3, 4, 5, 2, 3, 2, 3, 4, 5)
site_2_complex_rocks <- c(3, 5, 2, 1, 7, 8, 7, 4, 11, 9)

mean(site_1_flat_rocks)
mean(site_2_complex_rocks)

sd(site_1_flat_rocks)
sd(site_2_complex_rocks)

se.site1 <- sd(site_1_flat_rocks)/sqrt(length((site_1_flat_rocks)))
se <- sd(site_2_complex_rocks)/sqrt(length((site_2_complex_rocks)))

intertidal.data <- bind_cols(site_1_flat_rocks, site_2_complex_rocks) %>% 
  rename(Site_1 = ...1, Site_2 = ...2) 
intertidal.data <- as.data.frame(intertidal.data)

#Two-sample Student's t-test
two.sample.t.test<-t.test(intertidal.data$Site_1, intertidal.data$Site_2, var.equal=TRUE, na.rm=TRUE)
two.sample.t.test

#Welch's t-test: 
welch.t.test<-t.test(intertidal.data$Site_1, intertidal.data$Site_2, na.rm=TRUE)
welch.t.test


#calculate standard error 
se.site1 <- sd(site_1_flat_rocks)/sqrt(length((site_1_flat_rocks))) #calculating se 
se.site2 <- sd(site_2_complex_rocks)/sqrt(length((site_2_complex_rocks)))
se.site1
se.site2

#create data set of summary statistics
data <- data.frame(
  name=c("Site 1: Flat Topography","Site 2: Complex Topography") , #site names
  mean=c(3.4, 5.7), #mean
  se=c(0.3399346, 1.022524)) #standard error

#plot summary statistics 
ggplot(data) +
  geom_bar( aes(x=name, y=mean, fill=name), stat="identity", alpha=0.85) +
  geom_errorbar( aes(x=name, ymin=mean-se, ymax=mean+se), width=0.5, colour="black",
                 alpha=0.95, size=1)+
  theme(legend.position = "right", legend.title=element_text(size=20),
        legend.text=element_text(size=14))+
  theme_minimal()+ 
  labs(y = "Sea Urchin Density (per 1^m2 quadrats) ", x="Site 1 versus Site 2", title = "Difference in Urchin Densities Between \n Complex and Flat Intertidal Topography", fill="Site")

#load library 
library(car) 

#load in data 
Coastbuckwheat<- read_csv("/Users/robertdellinger/Documents/Biometry/Data/CoastBuckwheat.csv")

#checking for normality 
qqp(Coastbuckwheat$Density, "norm") 

#formal test for normality 
shapiro.test(Coastbuckwheat$Density) #If P>0.05, then the data is normal

#calculating statistics 
mean(Coastbuckwheat$Density)
wheatmean<-mean(Coastbuckwheat$Density)
wheatmean
sdwheatmean<-sd(Coastbuckwheat$Density, na.rm=TRUE)
sdwheatmean

#t test 
t.test<-t.test(Coastbuckwheat$quadrat, Coastbuckwheat$Density, mu= 4, paired =  TRUE)#worked
t.test

#confidence interval 
alpha <- 0.05 #95% confidence for alpha
nwheatmean<- length(Coastbuckwheat$Density) 
standard_error <- sd(Coastbuckwheat$Density)/sqrt(nwheatmean) #calculating se 
degrees_freedom <- nwheatmean - 1
t_score_top <- qt(p=alpha/2, df=degrees_freedom) #qt() command calculates the t-score
margin_error <- t_score_top * standard_error #finding margin error
#confidence interval is the mean +/- margin of error
lower_bound <- wheatmean - margin_error
upper_bound <- wheatmean + margin_error
confidence_interval<-(c(lower_bound,upper_bound)) 

#plotting values using ggplot
wheatgraph <- ggplot(Coastbuckwheat, aes(x=1, y=wheatmean), fill=factor(wheatmean)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(),
        axis.line = element_line(colour = "black"), axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) +
  geom_bar(stat = "identity", color="black", fill="darkcyan", position = "dodge", size=0.6) +
  labs(x="Mean Density", y= "Count", title= "Mean Buckwheat Density") +
  geom_errorbar(aes(ymax=upper_bound, ymin=lower_bound), position = position_dodge(0.9), width=0.1) +
  geom_hline(yintercept = 4) 
print(wheatgraph)


# Question 3 

# Use an appropriate t-test to test the hypothesis 
#that the sports drink improves athletic performance. 

library(tidyr)
library(dplyr)

runtime.data <- read.csv("/Users/robertdellinger/Documents/Biometry/Data/RunTimes.csv")


paired.test <- t.test(runtime.data$Water, runtime.data$Sportsdrink, data=runtime.data$Student, paired=TRUE, na.rm=TRUE)
paired.test


runtime.data.clean <- runtime.data %>% 
  data.frame(Treatment = as.factor(rep(c("Water", "Sportsdrink"), each = 20)),
             Time = c(runtime.data$Water, runtime.data$Sportsdrink)) %>% 
  select(Student, Treatment, Time)
str(runtime.data.clean)
attach(runtime.data.clean)


crying.data <- read.csv("/Users/robertdellinger/Documents/Biometry/Data/crying_babies.csv")

#using a scatter plot to part data 
plot(cryduration~IQ, data=crying.data)

#with three different tests of correlation: Pearson’s r, Spearman's rho, and Kendall's tau
pearsons.test <- cor.test(crying.data$cryduration, crying.data$IQ, method="pearson", na.rm=TRUE, exact=FALSE)
spearmans.test <- cor.test(crying.data$cryduration, crying.data$IQ, method="spearman", na.rm=TRUE, exact=FALSE)
kendalls.test <- cor.test(crying.data$cryduration, crying.data$IQ, method="kendall", na.rm=TRUE, exact=FALSE)

pearsons.test
spearmans.test
kendalls.test
 
butterflyballot.data <- read.csv("/Users/robertdellinger/Documents/Biometry/Data/butterflyballot.csv")


plot(Bush~Buchanan, data=butterflyballot.data)

palm.beach.removed <- butterflyballot.data[-67, ]

palm.beach.removed.test <- cor.test(palm.beach.removed$Bush, palm.beach.removed$Buchanan, method="pearson", na.rm=TRUE)
palm.beach.removed.test 


butterflyballot.data.test <- cor.test(butterflyballot.data$Bush, butterflyballot.data$Buchanan, method="pearson", na.rm=TRUE)
butterflyballot.data.test

#cleaning data 
mydata <- read.csv("/Users/robertdellinger/Documents/Biometry/Data/streams.csv")
mtdata.clean <- mydata %>% na.omit()

#linear model 
model1<-lm(Biomass~NumberSpp, data=mydata, na.action="na.omit")
model1res<-resid(model1)

#testing for normality 
qqp(model1res, "norm")

#fitted model 
fitted(model1)
plot(model1res~fitted(model1)) #fitted vs residuals 
plot(Biomass~NumberSpp, data=mydata)
abline(model1)

#summmary statistics 
summary(model1)

#plotting using ggplot 
ggplot(mydata, aes(x=NumberSpp, y=Biomass))+
  theme(panel.grid.major=element_blank(), panel.grid.minor=element_blank()) +
  geom_point(shape=1) +
  guides(fill="none") +
  ylab("Biomass (mg)") +
  xlab("Number of Species (m^2)") +
  labs(title ="Species Richness of Invertebrates
       as a Function of Biomass")+
  geom_smooth(method = "lm", formula = y~x)




plot(cooks.distance(model1))
cooks.distance(model1)


############8##############
rm(list=ls())
#linear model 
mydata <- read.csv("/Users/robertdellinger/Documents/Biometry/Data/algae.csv")
model1<-lm(Height~Surface_area, data=mydata, na.action="na.omit")
resid(model1)
model1res<-resid(model1)
qqp(model1res, "norm")
plot(model1res~fitted(model1))
plot(Height~Surface_area, data=mydata)
abline(model1)
summary(model1)

ggplot(mydata, aes(x=Surface_area, y=Height, fill="red"))+
  theme(panel.grid.major=element_blank(), panel.grid.minor=element_blank()) +
  geom_point(shape=1) +
  guides(fill="none") +
  ylab("Height (mg)") +
  xlab("Surface area (cm^2)") +
  labs(title ="Surface Area of Algal Surface Area
       as a Function of Height")+
  geom_smooth(method = "lm", formula = y~x, se=TRUE) #confidence interval



plot(cooks.distance(model1))
cooks.distance(model1)
