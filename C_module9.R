## Module 9: Version Control
## Meghan Owings and Bri LaCarubba
## Last modified: Oct 22 Sun

## Question 1

##Load dataset

(library(ggplot2): data(msleep):?msleep)


## how many diet types (vore) categories are there? 
head(msleep)
summary(msleep)
unique(msleep$vore)


#There are 4 types (carni, omni, herbi, insecti) but there are NA's in the data

#visually investigate whether daily sleep totals vary with diet type
#make boxplot comparing daily sleep totals across vore categories 
#briefly describe in 1-2 sentences the major patterns in the plot


msleep2<-msleep[!is.na(msleep$vore),]
unique(msleep2$vore)

boxplot(msleep2$sleep_total ~ msleep2$vore, xlab= "Diet Type", ylab= "Daily Sleep Totals (hr)")

## Insectivores sleep have the highest average daily sleep total (but also a lot of variation). 
## Carnivores, herbivores, and omnivores sleep (on average) for the same amount of time. 

##Question 2
## Use plot() to show the relationship between the natural log of body size
## and the length of the sleep cycle 

msleep3<-msleep[!is.na(msleep$sleep_cycle),]
msleep3$logbodywt<-log(msleep3$bodywt)
summary(msleep3)

plot(msleep3$logbodywt, msleep3$sleep_cycle, xlab= "Log Body Weight (kg)", ylab = "Sleep Cycle Length (hr)", pch = 16, col= "blue")

unique(msleep3$conservation)
msleep4<-msleep3[!is.na(msleep3$conservation),]
summary(msleep4)
unique(msleep4$conservation)

ggplot(data = msleep4, aes(x=logbodywt, y=sleep_cycle))+
  geom_point()+
  facet_wrap(~ conservation)+
  stat_smooth(method="lm", se=F)

## Yes, the species within each conservation group show a linear trend where 
## they increase the hours of daily sleep as they increase in body weight. This may not 
## be justified considering we transformed the data and there was limited data. Looking at conservation
## en and nt there is only one data point so we can't say whether this data are linear. Vu there are only
## two points so we can't say whether this data is really linear.

## Question 3: 
## how does the ratio of brain weight to body weight vary by diet type?
## write a function that returns a dataframe with a row for each diet type (vore) category
## and three columns named "vore" "brain_body_mean" and "brain_body_se" 


brain_body_ratio<- function(x,y,z){
  
  names<-as.data.frame(sort(unique(x)))
  brain<-tapply(y, x, mean, na.rm=T)
  body<-tapply(z, x, mean, na.rm=T)
  
  m<-brain/body
  
  brainsd<-tapply(y, x, sd, na.rm=T)
  bodysd<-tapply(z, x, sd, na.rm=T)
  brainse<-brainsd/sqrt(length(na.omit(brainsd)))
  bodyse<-bodysd/sqrt(length(na.omit(bodysd)))
  se<-brainse/bodyse
  
  s1<-cbind(names,m,se)
  colnames(s1)<-c("vore","brain_body_mean","brain_body_se")
  rownames(s1)<-NULL
  return(s1)
}

test1 <- brain_body_ratio(msleep4$vore, msleep4$brainwt, msleep4$bodywt)
head(test1)

msleep<-msleep[!is.na(msleep$vore),]
msleep<-msleep[!is.na(msleep$brainwt),]
ratio<-brain_body_ratio(msleep$vore, msleep$brainwt, msleep$bodywt)
ratio

plot(ratio$brain_body_mean~ratio$vore)

##omnivores have the greatest brain weight to body weight ratio with herbivores
##being the lowest (but very similar to insectivores)

