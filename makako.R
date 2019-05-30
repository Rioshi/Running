#Data Reading#
curv <-read.delim("clipboard",header = TRUE) #Means dataset

##Modelling temporal variables
fit1<-lm(ALT~log(DDS),data=subset(curv,TRAT=="T1"))
fit2<-lm(ALT~log(DDS),data=subset(curv,TRAT=="T2"))
fit3<-lm(ALT~log(DDS),data=subset(curv,TRAT=="T3"))
fit4<-lm(ALT~log(DDS),data=subset(curv,TRAT=="T4"))
fit5<-lm(ALT~log(DDS),data=subset(curv,TRAT=="T5"))


x=seq(from=1,to=100,by=0.5)
y=predict(fit,newdata=list(DDS=x),interval="confidence")
matlines(x,y,lwd=2)


##Plotting temporal variables
library(ggplot2)
g1 <- ggplot(curv, aes(x=DDS, y=ALT, na.rm = TRUE,color=TRAT)) + 
  geom_point(shape=20, na.rm=TRUE) +
  geom_smooth(method=lm,formula = y~log(x), na.rm=TRUE,se=FALSE)+
  theme_light() +
  theme(axis.text=element_text(size=10),
        axis.title.x = element_text(face="bold"),
        axis.title.y = element_text(face="bold"))+
  xlab("Días despues de la siembra (DDS)")+
  ylab("Altura (cm)")
