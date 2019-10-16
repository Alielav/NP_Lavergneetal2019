rm(list=ls())

# 1- Upload isotope-derived and predicted ci/ca data + environmental data

REG_all_5yrs<-as.data.frame(read.table("/Users/alienorlavergne/Desktop/My_papers/Lavergneetal_2019_NP/TRdata_5yrs.txt",header=T))
head(REG_all_5yrs)

REG_all_5yrs$PFT <- factor(REG_all_5yrs$PFT,
                         levels = c('ENF','DBF'),ordered = FALSE)

nb_sites<-length(unique(REG_all_5yrs$Site))


# 2-  Observed versus predicted chi: spatial patterns

## Model performance
library(performance)

model<-lm(cicaobs ~ cicasimu,data=REG_all_5yrs)
modelenf<-lm(REG_all_5yrs$cicaobs[REG_all_5yrs$PFT=="ENF"] ~ REG_all_5yrs$cicasimu[REG_all_5yrs$PFT=="ENF"])
modeldbf<-lm(REG_all_5yrs$cicaobs[REG_all_5yrs$PFT=="DBF"] ~ REG_all_5yrs$cicasimu[REG_all_5yrs$PFT=="DBF"])
compare_performance(model,modelenf,modeldbf)


## Fig. 2

{
library(ggplot2)
   
  onetoone<-cbind(seq(from=0.4,to=0.9,by=0.05),seq(from=0.4,to=0.9,by=0.05))
  colnames(onetoone)<-c("x1","x2")
  onetoone<-as.data.frame(onetoone)
  
  med<-as.data.frame(cbind(median(REG_all_5yrs$cicaobs,na.rm=T),median(REG_all_5yrs$cicasimu,na.rm=T)))
  sdev<-as.data.frame(cbind((median(REG_all_5yrs$cicaobs,na.rm=T)-sd(REG_all_5yrs$cicaobs,na.rm=T)),(median(REG_all_5yrs$cicasimu,na.rm=T)-sd(REG_all_5yrs$cicasimu,na.rm=T))))
  sdev<-rbind(sdev,as.data.frame(cbind((median(REG_all_5yrs$cicaobs,na.rm=T)+sd(REG_all_5yrs$cicaobs,na.rm=T)),(median(REG_all_5yrs$cicasimu,na.rm=T)+sd(REG_all_5yrs$cicasimu,na.rm=T)))))
  sdev<-cbind(sdev,rep(median(REG_all_5yrs$cicasimu,na.rm=T),2),rep(median(REG_all_5yrs$cicaobs,na.rm=T),2))
  colnames(sdev)<-c("V1obs","V1simu","V2obs","V2simu")
  
  ## ENF
  med_ENF<-as.data.frame(cbind(median(REG_all_5yrs$cicaobs[REG_all_5yrs$PFT == "ENF"],na.rm=T),median(REG_all_5yrs$cicasimu[REG_all_5yrs$PFT == "ENF"],na.rm=T)))
  sdev_ENF<-as.data.frame(cbind((median(REG_all_5yrs$cicaobs[REG_all_5yrs$PFT == "ENF"],na.rm=T)-sd(REG_all_5yrs$cicaobs[REG_all_5yrs$PFT == "ENF"],na.rm=T)),(median(REG_all_5yrs$cicasimu[REG_all_5yrs$PFT == "ENF"],na.rm=T)-sd(REG_all_5yrs$cicasimu[REG_all_5yrs$PFT == "ENF"],na.rm=T))))
  sdev_ENF<-rbind(sdev_ENF,as.data.frame(cbind((median(REG_all_5yrs$cicaobs[REG_all_5yrs$PFT == "ENF"],na.rm=T)+sd(REG_all_5yrs$cicaobs[REG_all_5yrs$PFT == "ENF"],na.rm=T)),(median(REG_all_5yrs$cicasimu[REG_all_5yrs$PFT == "ENF"],na.rm=T)+sd(REG_all_5yrs$cicasimu[REG_all_5yrs$PFT == "ENF"],na.rm=T)))))
  sdev_ENF<-cbind(sdev_ENF,rep(median(REG_all_5yrs$cicasimu[REG_all_5yrs$PFT == "ENF"],na.rm=T),2),rep(median(REG_all_5yrs$cicaobs[REG_all_5yrs$PFT == "ENF"],na.rm=T),2))
  colnames(sdev_ENF)<-c("V1obs","V1simu","V2obs","V2simu")
  
  ## DBF
  med_DBF<-as.data.frame(cbind(median(REG_all_5yrs$cicaobs[REG_all_5yrs$PFT == "DBF"],na.rm=T),median(REG_all_5yrs$cicasimu[REG_all_5yrs$PFT == "DBF"],na.rm=T)))
  sdev_DBF<-as.data.frame(cbind((median(REG_all_5yrs$cicaobs[REG_all_5yrs$PFT == "DBF"],na.rm=T)-sd(REG_all_5yrs$cicaobs[REG_all_5yrs$PFT == "DBF"],na.rm=T)),(median(REG_all_5yrs$cicasimu[REG_all_5yrs$PFT == "DBF"],na.rm=T)-sd(REG_all_5yrs$cicasimu[REG_all_5yrs$PFT == "DBF"],na.rm=T))))
  sdev_DBF<-rbind(sdev_DBF,as.data.frame(cbind((median(REG_all_5yrs$cicaobs[REG_all_5yrs$PFT == "DBF"],na.rm=T)+sd(REG_all_5yrs$cicaobs[REG_all_5yrs$PFT == "DBF"],na.rm=T)),(median(REG_all_5yrs$cicasimu[REG_all_5yrs$PFT == "DBF"],na.rm=T)+sd(REG_all_5yrs$cicasimu[REG_all_5yrs$PFT == "DBF"],na.rm=T)))))
  sdev_DBF<-cbind(sdev_DBF,rep(median(REG_all_5yrs$cicasimu[REG_all_5yrs$PFT == "DBF"],na.rm=T),2),rep(median(REG_all_5yrs$cicaobs[REG_all_5yrs$PFT == "DBF"],na.rm=T),2))
  colnames(sdev_DBF)<-c("V1obs","V1simu","V2obs","V2simu")
  
  
  plot1<-ggplot(REG_all_5yrs,aes(x = cicasimu, y=cicaobs,color=PFT)) + 
    xlim(0.4,0.9) +
    ylim(0.4,0.9) +
    theme_classic() + 
    annotate(geom = "text", x = 0.5, y = 0.86, label = "ENF",size=4,colour="coral2") +
    annotate(geom = "text", x = 0.5, y = 0.82, label = expression(italic("R")^{2}*""["adj"]*" = 0.38"),size=4,colour="coral2") +
    annotate(geom = "text", x = 0.5, y = 0.78, label = paste0("RMSE = ",round(model_performance(modelenf)[5],3)),size=4,colour="coral2") +
    annotate(geom = "text", x = 0.8, y = 0.50, label = "DBF",size=4,colour="darkturquoise") +
    annotate(geom = "text", x = 0.8, y = 0.46, label = expression(italic("R")^{2}*""["adj"]*" = 0.21"),size=4,colour="darkturquoise") +
    annotate(geom = "text", x = 0.8, y = 0.42, label = paste0("RMSE = ",round(model_performance(modeldbf)[5],3)),size=4,colour="darkturquoise") +
    
    annotate(geom = "text", x = 0.45, y = 0.9, label ="(a)",size=5,colour=1) +
    scale_color_discrete(name="") +
    geom_point(alpha=0.2,shape=16,size=1.5,show.legend = FALSE) + 
    geom_smooth(method='lm', se = TRUE,size=1,show.legend = FALSE, formula = y ~ x) +  
    geom_line(data = onetoone,aes(x=x2,y=x1),linetype="dashed",color="gray",size=1) +
    
    geom_point(size=2,data=med_ENF,aes(x=V2,y=V1),color="coral2") +
    geom_line(data = sdev_ENF,aes(x=V2obs,y=V1obs),color="coral2",size=1) +
    geom_line(data = sdev_ENF,aes(x=V1simu,y=V2simu),color="coral2",size=1) +
    
    geom_point(size=2,data=med_DBF,aes(x=V2,y=V1),color="darkturquoise") +
    geom_line(data = sdev_DBF,aes(x=V2obs,y=V1obs),color="darkturquoise",size=1) +
    geom_line(data = sdev_DBF,aes(x=V1simu,y=V2simu),color="darkturquoise",size=1) +
    labs(x=expression("Predicted "*chi),y= expression("Isotope-derived "*chi)) +
    theme(
      axis.ticks.length = unit(.15, "cm"),
      #axis.ticks.y = element_blank(),
      axis.text.y = element_text(size = 14),
      axis.text.x = element_text(size = 14),
      axis.title.x = element_text(size = 14),
      axis.title.y = element_text(size = 14),
      #plot.title = element_text(size = 14),
      legend.text = element_text(size = 14),
      legend.background = element_rect(fill=NA),
      legend.title = element_text(size = 14), 
      legend.position = c(0.86, 0.15))
  plot1
  
  
  plot2<-ggplot(REG_all_5yrs,aes(x = cicasimu, y=cicaobs,color=z_km)) + 
    xlim(0.4,0.9) +
    ylim(0.4,0.9) +
    theme_classic() + 
    annotate(geom = "text", x = 0.5, y = 0.84, label = expression(italic("R")^{2}*""["adj"]*" = 0.39"),size=4,colour=1) +  
    
    annotate(geom = "text", x = 0.5, y = 0.80, label = paste0("RMSE = ",round(model_performance(model)[5],3)),size=4,colour=1) +  
    
    annotate(geom = "text", x = 0.45, y = 0.9, label ="(b)",size=5,colour=1) +
    
    geom_point(alpha=0.3,shape=16,size=1.5,show.legend = TRUE) + 
    
    geom_line(data = onetoone,aes(x=x2,y=x1),linetype="dashed",color="gray",size=1) +
    geom_smooth(method='lm', se = TRUE,size=1,show.legend = FALSE, formula = y ~ x,color="black") +  
    geom_point(size=2,data=med,aes(x=V2,y=V1),color="black") +
    #  geom_vline(data=sdev,aes(x=V1,y=V2),linetype="dashed",color=2) +
    geom_line(data = sdev,aes(x=V2obs,y=V1obs),color="black",size=1) +
    geom_line(data = sdev,aes(x=V1simu,y=V2simu),color="black",size=1) +
    scale_colour_gradientn(colours=rainbow(4)) +
    # scale_y_discrete(position="right",limits=c(0.35,0.85)) +
    labs(x=expression("Predicted "*chi),y= " ") +
    theme(
      axis.ticks.length = unit(.15, "cm"),
      #axis.ticks.y = element_blank(),
      axis.text.y = element_text(size = 14),
      axis.text.x = element_text(size = 14),
      axis.title.x = element_text(size = 14),
      axis.title.y = element_text(size = 14),
      #plot.title = element_text(size = 14),
      legend.text = element_text(size = 14),
      legend.background = element_rect(fill=NA),
      legend.title = element_text(size = 14), 
      legend.position = c(0.86, 0.25))
  plot2
  
  
  library(gridExtra)
  combo_plot <-grid.arrange(plot1,plot2,ncol = 2,nrow=1)
}


# 3 - Observed versus predicted chi: temporal patterns

  ## Calculation of Theil-Sen slopes
  library(trend)  
  
  {
    
## 3.1 Calculation chi trends 
  
 
  # 3.1.a Isotope-derived chi
  
  slope_TR_OBS<-matrix(ncol=7,nrow=nb_sites)
  
  for (z in 1:nb_sites){
    
    a<-(REG_all_5yrs$cicaobs[REG_all_5yrs$Site==z & !(is.na(REG_all_5yrs$cicaobs))]-mean(REG_all_5yrs$cicaobs[REG_all_5yrs$Site==z & !(is.na(REG_all_5yrs$cicaobs))]))/mean(REG_all_5yrs$cicaobs[REG_all_5yrs$Site==z & !(is.na(REG_all_5yrs$cicaobs))])*100
    
    slope_TR_OBS[z,1]<-sens.slope(a)$estimates
    slope_TR_OBS[z,2]<-sens.slope(a)$p.value
    slope_TR_OBS[z,3]<-sens.slope(a)$conf.int[1]
    slope_TR_OBS[z,4]<-sens.slope(a)$conf.int[2]    
    slope_TR_OBS[z,5]<-unique(REG_all_5yrs$z_km[REG_all_5yrs$Site==z])
    slope_TR_OBS[z,6]<-z
    slope_TR_OBS[z,7]<-as.character(unique(REG_all_5yrs$PFT[REG_all_5yrs$Site==z]))

  }
  
  colnames(slope_TR_OBS)<-c("estimatesOBS","pvalueOBS","conf1OBS","conf2OBS","z_km","Site","PFT")

  slope_TR_OBS<-as.data.frame((slope_TR_OBS))  
  
  for (i in 1:6){
    slope_TR_OBS[,i]<-as.numeric(as.character(slope_TR_OBS[,i]))
  }
  
  
  # 3.1.a Predicted chi
  
  slope_TR_SIMU<-matrix(ncol=4,nrow=nb_sites)
  
  for (z in 1:nb_sites){
    
#    a<-(REG_all_5yrs$cicaobs[REG_all_5yrs$Site==z]-mean(REG_all_5yrs$cicaobs[REG_all_5yrs$Site==z],na.rm=T))/mean(REG_all_5yrs$cicaobs[REG_all_5yrs$Site==z],na.rm=T)*100
    
    b<-(REG_all_5yrs$cicasimu[REG_all_5yrs$Site==z & !(is.na(REG_all_5yrs$cicaobs))]-mean(REG_all_5yrs$cicasimu[REG_all_5yrs$Site==z & !(is.na(REG_all_5yrs$cicaobs))]))/mean(REG_all_5yrs$cicasimu[REG_all_5yrs$Site==z & !(is.na(REG_all_5yrs$cicaobs))])*100
    
    slope_TR_SIMU[z,1]<-sens.slope(b)$estimates
    slope_TR_SIMU[z,2]<-sens.slope(b)$p.value
    slope_TR_SIMU[z,3]<-sens.slope(b)$conf.int[1]
    slope_TR_SIMU[z,4]<-sens.slope(b)$conf.int[2]
  }
  
  colnames(slope_TR_SIMU)<-c("estimatesSIMU","pvalueSIMU","conf1SIMU","conf2SIMU")
  
  slope_TR_SIMU<-as.data.frame((slope_TR_SIMU))  
  
  for (i in 1:4){
    slope_TR_SIMU[,i]<-as.numeric(as.character(slope_TR_SIMU[,i]))
  }
  
#slope_TR_SIMU$estimatesSIMU
#write.table(slope_TR_SIMU,"slope_TR_SIMU4.txt")  
  
  
## 3.2 Calculation climate trends
    
  # 3.2.a Growing-season temperature (Tg, degC)
  
  slope_clim_T<-matrix(ncol=4,nrow=nb_sites)
  
  for (z in 1:nb_sites){

    b<-(REG_all_5yrs$Tg_degC[REG_all_5yrs$Site==z & !(is.na(REG_all_5yrs$cicaobs))]-mean(REG_all_5yrs$Tg_degC[REG_all_5yrs$Site==z & !(is.na(REG_all_5yrs$cicaobs))]))/mean(REG_all_5yrs$Tg_degC[REG_all_5yrs$Site==z & !(is.na(REG_all_5yrs$cicaobs))])*100
    
    slope_clim_T[z,1]<-sens.slope(b)$estimates
    slope_clim_T[z,2]<-sens.slope(b)$p.value
    slope_clim_T[z,3]<-sens.slope(b)$conf.int[1]
    slope_clim_T[z,4]<-sens.slope(b)$conf.int[2]
    
  }
  
  colnames(slope_clim_T)<-c("estimatesTg","pvalueTg","conf1Tg","conf2Tg")
  
  slope_TR_clim_T<-as.data.frame((slope_clim_T))  
  
  for (i in 1:4){
    slope_clim_T[,i]<-as.numeric(as.character(slope_clim_T[,i]))
  }
  
  
  # 3.2.b Growing-season vapour pressure deficit (Dg, kPa)
  
  slope_clim_D<-matrix(ncol=4,nrow=nb_sites)
  
  for (z in 1:nb_sites){

    b<-(REG_all_5yrs$Dg_kPa[REG_all_5yrs$Site==z & !(is.na(REG_all_5yrs$cicaobs))]-mean(REG_all_5yrs$Dg_kPa[REG_all_5yrs$Site==z & !(is.na(REG_all_5yrs$cicaobs))]))/mean(REG_all_5yrs$Dg_kPa[REG_all_5yrs$Site==z & !(is.na(REG_all_5yrs$cicaobs))])*100
    
    slope_clim_D[z,1]<-sens.slope(b)$estimates
    slope_clim_D[z,2]<-sens.slope(b)$p.value
    slope_clim_D[z,3]<-sens.slope(b)$conf.int[1]
    slope_clim_D[z,4]<-sens.slope(b)$conf.int[2]
  }
  
  colnames(slope_clim_D)<-c("estimatesDg","pvalueDg","conf1Dg","conf2Dg")
  
  slope_TR_clim_D<-as.data.frame((slope_clim_D))  
  
  for (i in 1:4){
    slope_clim_D[,i]<-as.numeric(as.character(slope_clim_D[,i]))
  }
  }

  
## 3.3 Further calculations  
  
  {
    
  # 3.3.a Calculation median trends with bootstrapping
  library(boot)
  samplemedian <- function(x, d) {
    return(median(x[d]))
  }
  
  ## Isotope-derived chi trends
  boot(slope_TR_OBS$estimates, samplemedian, R=1000)
  boot(slope_TR_OBS$pvalue, samplemedian, R=1000)
  min(slope_TR_OBS$estimates);max(slope_TR_OBS$estimates)
  IQR(slope_TR_OBS$estimates,na.rm=T)
  sd.obs<-sd(slope_TR_OBS$estimates,na.rm=T);sd.obs
  
  ## Predicted chi trends
  boot(slope_TR_SIMU$estimates, samplemedian, R=1000)
  boot(slope_TR_SIMU$pvalue, samplemedian, R=1000)
  min(slope_TR_SIMU$estimates);max(slope_TR_SIMU$estimates)
  IQR(slope_TR_SIMU$estimates,na.rm=T)
  sd.simu<-sd(slope_TR_SIMU$estimates,na.rm=T);sd.simu
  
  slope_TR_all<-cbind(slope_TR_OBS,slope_TR_SIMU,slope_clim_T,slope_clim_D)
 # slope_TR_all$groupz <- factor(slope_TR_all$groupz,levels = c('0-1','1-2','2-4'),ordered = TRUE)
  slope_TR_all$PFT <- factor(slope_TR_all$PFT,
                         levels = c('ENF','DBF'),ordered = TRUE)
  

  # 3.3.b Groups of trends for T, D, isotope-derived and predicted chi
  
  groupT<-matrix(ncol=1,nrow=nb_sites)
  for (z in (1:nb_sites)){
    if (slope_TR_all$Site[z] %in% subset(slope_TR_all,slope_TR_all$pvalueTg<=0.05 & slope_TR_all$estimatesTg>0)$Site){
      groupT[z]<-"increase"
    }
    if (slope_TR_all$Site[z] %in% subset(slope_TR_all,slope_TR_all$pvalueTg<=0.05 & slope_TR_all$estimatesTg<0)$Site){
      groupT[z]<-"decrease"
    }
    if (slope_TR_all$Site[z] %in% subset(slope_TR_all,slope_TR_all$pvalueTg>0.05)$Site) {
      groupT[z]<-"ns"
    }
  }
  
  groupD<-matrix(ncol=1,nrow=nb_sites)
  for (z in (1:nb_sites)){
    if (slope_TR_all$Site[z] %in% subset(slope_TR_all,slope_TR_all$pvalueDg<=0.05 & slope_TR_all$estimatesDg>0)$Site){
      groupD[z]<-"increase"
    }
    if (slope_TR_all$Site[z] %in% subset(slope_TR_all,slope_TR_all$pvalueDg<=0.05 & slope_TR_all$estimatesDg<0)$Site){
      groupD[z]<-"decrease"
    }
    if (slope_TR_all$Site[z] %in% subset(slope_TR_all,slope_TR_all$pvalueDg>0.05)$Site) {
      groupD[z]<-"ns"
    }
  }
  
  groupobs<-matrix(ncol=1,nrow=nb_sites)
  for (z in (1:nb_sites)){
    if (slope_TR_all$Site[z] %in% subset(slope_TR_all,slope_TR_all$pvalueOBS<=0.05 & slope_TR_all$estimatesOBS>0)$Site){
      groupobs[z]<-"increase"
    }
    if (slope_TR_all$Site[z] %in% subset(slope_TR_all,slope_TR_all$pvalueOBS<=0.05 & slope_TR_all$estimatesOBS<0)$Site){
      groupobs[z]<-"decrease"
    }
    if (slope_TR_all$Site[z] %in% subset(slope_TR_all,slope_TR_all$pvalueOBS>0.05)$Site) {
      groupobs[z]<-"ns"
    }
  }
  
  groupsimu<-matrix(ncol=1,nrow=nb_sites)
  for (z in (1:nb_sites)){
    if (slope_TR_all$Site[z] %in% subset(slope_TR_all,slope_TR_all$pvalueSIMU<=0.05 & slope_TR_all$estimatesSIMU>0)$Site){
      groupsimu[z]<-"increase"
    }
    if (slope_TR_all$Site[z] %in% subset(slope_TR_all,slope_TR_all$pvalueSIMU<=0.05 & slope_TR_all$estimatesSIMU<0)$Site){
      groupsimu[z]<-"decrease"
    }
    if (slope_TR_all$Site[z] %in% subset(slope_TR_all,slope_TR_all$pvalueSIMU>0.05)$Site) {
      groupsimu[z]<-"ns"
    }
  }


## Final matrix with all calculations formatted for ggplot use

colnm_slope_TR_all<-colnames(slope_TR_all)  
colnames(slope_TR_all)<-NA
slope_TR<-cbind(rbind(slope_TR_all[,1:4],slope_TR_all[,8:11]),c(rep("ISO",nb_sites),rep("PRED",nb_sites)),rep(slope_TR_all[,5],2),rep(slope_TR_all[,6],2),rep(slope_TR_all[,7],2),rep(slope_TR_all[,12],2),rep(slope_TR_all[,16],2),c(groupobs,groupsimu),rep(groupT,2),rep(groupD,2))
 
colnames(slope_TR)<-c("estimates","pvalue","conf1","conf2","type","z_km","Site","PFT","estimatesTg","estimatesDg","groupchi","groupT ", "groupD")

  slope_TR$groupT <- factor(slope_TR$groupT,
                            levels = c('increase','ns','decrease'),ordered = TRUE)
  
  slope_TR$groupD <- factor(slope_TR$groupD,
                            levels = c('increase','ns','decrease'),ordered = TRUE)
  slope_TR$groupchi <- factor(slope_TR$groupchi,
                              levels = c('increase','ns','decrease'),ordered = TRUE)
  
  colnames(slope_TR_all)<-colnm_slope_TR_all
  
  }
  

## 3.4 Fig. 3     
  
  {
  
  ## Trend in partial pressure of CO2: 2.05 % per 5-yrs
  sens.slope((REG_all_5yrs$ca_Pa[REG_all_5yrs$Site==1]-mean(REG_all_5yrs$ca_Pa[REG_all_5yrs$Site==1]))/mean(REG_all_5yrs$ca_Pa[REG_all_5yrs$Site==1])*100)
  
  
  ## PFT: no significant difference between DBF and ENF sites
  t.test(subset(slope_TR,slope_TR$type=="ISO" & slope_TR$PFT=="DBF")[,1],subset(slope_TR,slope_TR$type=="ISO" & slope_TR$PFT=="ENF")[,1]) # p > 0.05
  
  
  library(ggsignif)
  
  # 3.4.a Boxplots
  
  ## Isotope-derived versus predicted chi trends
  ## Student's t test + variance test
  
  t.test(subset(slope_TR,slope_TR$type=="ISO")[,1],subset(slope_TR,slope_TR$type=="PRED")[,1]) # p > 0.05
  var.test(subset(slope_TR,slope_TR$type=="ISO")[,1],subset(slope_TR,slope_TR$type=="PRED")[,1]) # p < 0.001
  
  plot3<-ggplot(slope_TR,aes(x = type, y = estimates,colour=type)) + 
    theme_bw()+
    geom_jitter(alpha = 0.3, color = "gray") +
    geom_boxplot(alpha = 0.3,show.legend = FALSE,varwidth = FALSE,notch=FALSE,notchwidth=0.5,size=0.75) +
    scale_color_discrete(name="") +
    geom_signif(stat="identity",data=data.frame(x=c(1), xend=c(2), y=c(1.25), annotation=c(paste0("p  = ", round(t.test(subset(slope_TR,slope_TR$type=="ISO")[,1],subset(slope_TR,slope_TR$type=="PRED")[,1])$p.value,3)))),aes(x=x,xend=xend, y=y, yend=y, annotation=annotation),linetype=1, color=c("black"),tip_length = 0.02) +
    labs(x=" ",y= expression(chi*" trends [% 5yr"^{-1}*"]")) +
    annotate(geom = "text", x = 0.65, y = 1.85, label ="(a)",size=5,colour=1) +
    scale_y_continuous(position="left") +
    theme(
      axis.ticks.length = unit(.15, "cm"),
      axis.text.y = element_text(size = 14),
      axis.text.x = element_text(size = 14),
      axis.title.x = element_text(size = 14),
      axis.title.y = element_text(size = 14),
      legend.text = element_text(size = 10),
      legend.background = element_rect(fill=NA),
      legend.title = element_text(size = 11), 
      legend.position = "top")
  plot3

  
  ## T trend
  ## Student's t test
  
  t.test(subset(slope_TR,slope_TR$type=="ISO" & slope_TR$groupT=="ns")[,1],subset(slope_TR,slope_TR$type=="ISO" &  slope_TR$groupT=="increase")[,1]) # p > 0.05

  t.test(subset(slope_TR,slope_TR$type=="PRED" & slope_TR$groupT=="ns")[,1],subset(slope_TR,slope_TR$type=="PRED" &  slope_TR$groupT=="increase")[,1]) # p < 0.05

  
  plot4<-ggplot(slope_TR,aes(x = type, y = estimates, colour=groupT)) + 
    theme_bw()+
    geom_boxplot(alpha = 0.3,show.legend = TRUE,varwidth = FALSE,notch=FALSE,notchwidth=0.5,size=0.75) +
    scale_color_discrete(name="") +
    labs(x=" ",y= " ") +
    geom_signif(stat="identity",data=data.frame(x=c(1.8), xend=c(2.2), y=c(0.5), annotation=c(paste0("p  = ", round(t.test(subset(slope_TR,slope_TR$type=="PRED" & slope_TR$groupT=="ns")[,1],subset(slope_TR,slope_TR$type=="PRED" &  slope_TR$groupT=="increase")[,1])$p.value,3)))),aes(x=x,xend=xend, y=y, yend=y, annotation=annotation),linetype=1, color=c("black"),tip_length = 0.02) +
    geom_signif(stat="identity",data=data.frame(x=c(0.8), xend=c(1.2), y=c(1.25), annotation=c(paste0("p  = ", round(t.test(subset(slope_TR,slope_TR$type=="ISO" & slope_TR$groupT=="ns")[,1],subset(slope_TR,slope_TR$type=="ISO" &  slope_TR$groupT=="increase")[,1])$p.value,3)))),aes(x=x,xend=xend, y=y, yend=y, annotation=annotation),linetype=1, color=c("black"),tip_length = 0.02) +
    annotate(geom = "text", x = 1.10, y = 1.8, label =expression("(c)     "*italic("T")["g"]*" trends"),size=5,colour=1) +
    theme(
      axis.ticks.length = unit(.15, "cm"),
      axis.text.y = element_text(size = 14),
      axis.text.x = element_text(size = 14),
      axis.title.x = element_text(size = 14),
      axis.title.y = element_text(size = 14),
      legend.text = element_text(size = 10),
      legend.background = element_rect(fill=NA),
      legend.title = element_text(size = 11), 
      legend.position = c(0.8, 0.9))
  plot4
  
  
  ## D trend
  ## Student's t test
  
  t.test(subset(slope_TR,slope_TR$type=="ISO" & slope_TR$groupD=="ns")[,1],subset(slope_TR,slope_TR$type=="ISO" &  slope_TR$groupD=="increase")[,1]) # p > 0.05
  
  t.test(subset(slope_TR,slope_TR$type=="PRED" & slope_TR$groupD=="ns")[,1],subset(slope_TR,slope_TR$type=="PRED" &  slope_TR$groupD=="increase")[,1]) # p < 0.05
  
  
  plot5<-ggplot(slope_TR,aes(x = type, y = estimates, colour=groupD)) + 
    theme_bw()+
    geom_boxplot(alpha = 0.3,show.legend = FALSE,varwidth = FALSE,notch=FALSE,notchwidth=0.5,size=0.75) +
    scale_color_discrete(name="") +
    geom_signif(stat="identity",data=data.frame(x=c(1.8), xend=c(2.2), y=c(0.5), annotation=c(paste0("p  < 0.001 "))),aes(x=x,xend=xend, y=y, yend=y, annotation=annotation),linetype=1, color=c("black"),tip_length = 0.02) +
    geom_signif(stat="identity",data=data.frame(x=c(0.8), xend=c(1.2), y=c(1.25), annotation=c(paste0("p  = ", round(t.test(subset(slope_TR,slope_TR$type=="ISO" & slope_TR$groupD=="ns")[,1],subset(slope_TR,slope_TR$type=="ISO" &  slope_TR$groupD=="increase")[,1])$p.value,3)))),aes(x=x,xend=xend, y=y, yend=y, annotation=annotation),linetype=1, color=c("black"),tip_length = 0.02) +
    labs(x=" ",y= " ") +
    annotate(geom = "text", x = 1.15, y = 1.8, label =expression("(d)     "*italic("D")["g"]*" trends"),size=5,colour=1) +
    scale_y_continuous(position="left") +
    theme(
      axis.ticks.length = unit(.15, "cm"),
      axis.text.y = element_text(size = 14),
      axis.text.x = element_text(size = 14),
      axis.title.x = element_text(size = 14),
      axis.title.y = element_text(size = 14),
      legend.text = element_text(size = 10),
      legend.background = element_rect(fill=NA),
      legend.title = element_text(size = 11), 
      legend.position = c(0.8, 0.9))
  plot5

  
  # 3.4.b Predicted versus isotope-derived chi  

  Trendcomp<-cbind(slope_TR_OBS$estimatesOBS,slope_TR_SIMU$estimatesSIMU,as.character(slope_TR_OBS$PFT))
  colnames(Trendcomp)<-c("Obs_slope","Simu_slope","PFT")
  Trendcomp<-as.data.frame(Trendcomp)
  Trendcomp$Obs_slope<-as.numeric(as.character(Trendcomp$Obs_slope))
  Trendcomp$Simu_slope<-as.numeric(as.character(Trendcomp$Simu_slope))  

  # Ordinary Least Square (OLS) model
  mod1<-lm(Obs_slope ~ Simu_slope, data=Trendcomp)
  summary(mod1)
  model_performance(mod1)
  
  # Robust linear model (RLM)
  library(MASS)
  mod2<-rlm(Obs_slope ~ Simu_slope, data=Trendcomp)
  summary(mod2)
  dd<-data.frame(summary(mod2)$coefficients)
  dd$p.value =  2*pt(abs(dd$t.value), summary(mod2)$df[2], lower.tail=FALSE) 
  dd

  new<-data.frame(Simu_slope=seq(-2,2,0.01))
  ols_fit<-cbind(seq(-2,2,0.01),predict(lm(Obs_slope ~ Simu_slope,data=Trendcomp),newdata =new,interval="confidence",level=0.95))
  colnames(ols_fit)<-c("V1","V2","lwr","upr")
  ols_fit<-as.data.frame(ols_fit)
  ols_fit$V1<-as.numeric(as.character(ols_fit$V1))
  ols_fit$V2<-as.numeric(as.character(ols_fit$V2))
  
  rlm_fit<-cbind(seq(-2,2,0.01),predict(rlm(Obs_slope ~ Simu_slope,data=Trendcomp),newdata =new,interval="confidence",level=0.95))
  colnames(rlm_fit)<-c("V1","V2","lwr","upr")
  rlm_fit<-as.data.frame(rlm_fit)
  rlm_fit$V1<-as.numeric(as.character(rlm_fit$V1))
  rlm_fit$V2<-as.numeric(as.character(rlm_fit$V2))
  

  onetoone<-cbind(seq(from=-2,to=2,by=0.05),seq(from=-2,to=2,by=0.05))
  colnames(onetoone)<-c("x1","x2")
  onetoone<-as.data.frame(onetoone)

  plot6<-ggplot(slope_TR_all,aes(x = estimatesSIMU, y=estimatesOBS)) + 
    xlim(-1.0,1.2) +
    ylim(-2.5,3.5) +
    theme_classic() + 
    geom_vline(xintercept = 0,linetype="dotted",color="gray",size=0.75) +
    geom_hline(yintercept = 0,linetype="dotted",color="gray",size=0.75) +
    geom_line(data = onetoone,aes(x=x2,y=x1),linetype="dashed",color="gray",size=1) +
    annotate(geom = "text", x = 0.8, y = -0.7, label = expression(italic("R")^{2}*""["adj"]*" = 0.08"),size=3.65,colour="black") +  
    annotate(geom = "text", x = 0.8, y = -1.2, label = paste0("RMSE = ",round(model_performance(mod1)[5],3)),size=3.65,colour="black") +  
    annotate(geom = "text", x = 0.8, y = -1.8, label = paste0("OLS: p = ",round(summary(lm(Obs_slope ~ Simu_slope,data=Trendcomp))$coef[8],3)),size=3.65,colour="blue") +  
    annotate(geom = "text", x = 0.8, y = -2.3, label = paste0("RLM: p = ",round(dd[4]$p.value[2],3)),size=3.65,colour="red") +  
    geom_linerange(data=slope_TR_all,aes(x= estimatesSIMU, ymin = conf1OBS, ymax = conf2OBS),colour="gray",size=0.25,alpha=0.6) +
    geom_errorbarh(data=slope_TR_all,aes(y= estimatesOBS, xmin = conf1SIMU, xmax = conf2SIMU),colour="gray",size=0.25,alpha=0.6) +
    geom_point(data=slope_TR_all,aes(x = estimatesSIMU, y=estimatesOBS,colour=z_km),size=0.6,show.legend = FALSE) +
    annotate(geom = "text", x = -0.8, y = 3.5, label ="(b)",size=5,colour=1) +   
    geom_ribbon(data=merge(slope_TR_all,ols_fit),aes(x=V1,ymin=lwr,ymax=upr),fill="blue",alpha=0.1) +
    geom_ribbon(data=merge(slope_TR_all,rlm_fit),aes(x=V1,ymin=lwr,ymax=upr),fill="red",alpha=0.1) +
    scale_colour_gradientn(colours=rainbow(5)) +
    geom_line(data = ols_fit,aes(x=V1,y=V2),color="blue",size=0.75) +
    geom_line(data = rlm_fit,aes(x=V1,y=V2),color="red",size=0.75) +
    labs(x=expression(chi["pred"]*" trends [% 5yr"^{-1}*"]"),y= expression(chi["iso"]*" trends [% 5yr"^{-1}*"]"),col="Elevation") +
    theme(
      axis.ticks.length = unit(.15, "cm"),
      axis.text.y = element_text(size = 12),
      axis.text.x = element_text(size = 12),
      axis.title.x = element_text(size = 12),
      axis.title.y = element_text(size = 12),
      legend.text = element_text(size = 10),
      legend.background = element_rect(fill=NA),
      legend.title = element_text(size = 12), 
      legend.position = c(0.85, 0.25))
  plot6
  
  
  combo_plot <-grid.arrange(plot3,plot6,plot4,plot5,ncol = 2,nrow=2) 
 
  } 


# 4 - Partial residual plots of logit-transformed of observations and predictions against T, VPD and elevation, soil moisture indices

  # 4.0 - Calculation of logit chi
  for (i in 1: length(REG_all_5yrs[,1])){
    if (i==1){
      REG_all_5yrs2<-cbind(REG_all_5yrs[i,],log(REG_all_5yrs$Dg_kPa[i]),log(REG_all_5yrs$cicaobs[i]/(1-REG_all_5yrs$cicaobs[i])),log(REG_all_5yrs$cicasimu[i]/(1-REG_all_5yrs$cicasimu[i])))
    }else{
      REG_all_5yrs2<-rbind(REG_all_5yrs2,cbind(REG_all_5yrs[i,],log(REG_all_5yrs$Dg_kPa[i]),log(REG_all_5yrs$cicaobs[i]/(1-REG_all_5yrs$cicaobs[i])),log(REG_all_5yrs$cicasimu[i]/(1-REG_all_5yrs$cicasimu[i]))))
    }
  }
  
  colnames(REG_all_5yrs2)<-c(colnames(REG_all_5yrs),"LnD","logobs","logsimu")
  
  
  # 4.1 - Comparisons of linear models
  
  ## 4.1.a - Linear regression model with fixed effects
  
  test<-subset(REG_all_5yrs2,!is.na(REG_all_5yrs2$logobs))
  mod_obs<-lm(logobs ~ Tg_degC + LnD + z_km + ca_Pa, data=test);summary(mod_obs)
  
  
  test2<-test; test2$Tg_degC <-test$Tg-25
  mod_obs1<-lm(logobs ~ Tg_degC + LnD + z_km + ca_Pa, data=test2);summary(mod_obs1)
  mod_obs2<-lm(logobs ~ Tg_degC + LnD + z_km, data=test2);summary(mod_obs2)
  
  anova(mod_obs1,mod_obs2) ## p = 0.011
  
  mod_simu<-lm(logsimu ~ Tg_degC + LnD + z_km + ca_Pa, data=test);summary(mod_simu)
  
  
  ## Commonality analysis 
  library(yhat)
  commonalityCoefficients(test2,"logobs",list("Tg_degC", "LnD","z_km","ca_Pa"))
  commonalityCoefficients(test2,"logsimu",list("Tg_degC", "LnD","z_km","ca_Pa"))

  
  ## 4.1.b - Linear regression model with fixed and random effects
  library(lme4)
  
  mod_lme <- lmer(logobs ~ Tg_degC + LnD + z_km + ca_Pa + (1 |Site) , REML = T, data = test2);summary(mod_lme)
  dd<-data.frame(summary(mod_lme)$coefficients)
  dd$p.value =  2*pt(abs(dd$t.value), summary(mod_obs)$df[2], lower.tail=FALSE) 
  dd
  
  mod_lme1 <- lmer(logobs ~ Tg_degC + LnD + z_km + ca_Pa + (Tg_degC |Site) + (LnD |Site) , REML = T, data = test2);summary(mod_lme1)
  dd1<-data.frame(summary(mod_lme1)$coefficients)
  dd1$p.value =  2*pt(abs(dd1$t.value), summary(mod_obs)$df[2], lower.tail=FALSE) 
  dd1
  
  anova(mod_lme,mod_lme1)
  compare_performance(mod_obs,mod_lme,mod_lme1)

  # 4.2 - Fig. 4
 
  { 
  library(effects)
  closest <- function(x, x0) apply(outer(x, x0, FUN=function(x, x0) abs(x - x0)), 1, which.min)
  
  library(tidyverse)
  library(dplyr)
  
  effT = effect("Tg_degC", mod_obs, partial.residuals=T);plot(effT)
  effD = effect("LnD", mod_obs, partial.residuals=T);plot(effD)
  effz = effect("z_km", mod_obs, partial.residuals=T);plot(effz)
  effca = effect("ca_Pa", mod_obs, partial.residuals=T);plot(effca)

  
  # 4.2.a - logit chi against Tg
  xT.fit <- unlist(effT$x.all)
  trans <- I
  xT <- data.frame(lower = effT$lower, upper = effT$upper, fit = effT$fit, Tg_degC = effT$x$Tg_degC)
  xyT <- data.frame(x = xT.fit, y = xT$fit[closest(trans(xT.fit), xT$Tg_degC)] + effT$residuals,Site=test$Site)

  for (i in 1:length(unique(xyT$Site))){
    if(i==1){
      g<-length(xyT$Site[xyT$Site==unique(xyT$Site)[i]])
    }else{
      g<-c(g,length(xyT$Site[xyT$Site==unique(xyT$Site)[i]]))  
    }
  }
  
  for (i in 1:length(unique(xyT$Site))){
    if(i==1){
      gg<-rep(i,g[i])
    }else{
      gg<-c(gg,rep(i,g[i]))  
    }
  }
  
  prep5 <- xyT %>%
    mutate(group5 = gg) %>%
    group_by(group5) %>%
    mutate(
      slope = round(sens.slope(y)$estimates, 2),
      significance = round(sens.slope(y)$p.value, 4)
    ) %>%
    filter(significance < .05)   # only keep those with a pvalue < .05 
  
  plot7<-ggplot(xT,aes(x = Tg_degC, y=fit)) + 
    ylim(-0.25,2.25) +
    theme_classic() + 
    geom_point(data = xyT, aes(x = x, y = y,colour=Site), alpha=0.2, size = 0.75,show.legend = FALSE) +
    geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2) +
    geom_smooth(data = prep5,aes(x = x, y = y,group=group5,colour=Site),method='lm', se = FALSE, alpha = 0.2,size=0.75,show.legend = FALSE, formula = y ~ x,linetype = "solid") +  
    geom_line(size = 1,col = "black") +
    annotate(geom = "text", x = 9, y = 2.2, label ="(a)",size=5,colour=1) +
    labs(x=expression(italic("T")["g"]*" (°C)"),y= expression("logit "*chi["iso"])) +
    theme(
      axis.ticks.length = unit(.15, "cm"),
      axis.text.y = element_text(size = 14),
      axis.text.x = element_text(size = 14),
      axis.title.x = element_text(size = 14),
      axis.title.y = element_text(size = 14),
      legend.text = element_text(size = 14),
      legend.background = element_rect(fill=NA),
      legend.title = element_text(size = 14), 
      legend.position = c(0.9, 0.15))
  plot7
  
  
  ## 4.2.b - logit chi against Dg
  xD.fit <- unlist(effD$x.all)
  trans <- I
  xD <- data.frame(lower = effD$lower, upper = effD$upper, fit = effD$fit, D = effD$x$LnD)
  xyD <- data.frame(x = xD.fit, y = xD$fit[closest(trans(xD.fit), xD$D)] + effD$residuals,Site=test$Site)
  
  prep5 <- xyD %>%
    mutate(group5 = gg) %>%
    group_by(group5) %>%
    mutate(
      slope = round(sens.slope(y)$estimates, 2),
      significance = round(sens.slope(y)$p.value, 4)
    ) %>%
    filter(significance < .05)   # only keep those with a pvalue < .05 
  
  plot8<-ggplot(xD,aes(x = D, y=fit)) + 
    ylim(-0.25,2.25) +
    theme_classic() + 
    geom_point(data = xyD, aes(x = x, y = y,colour=Site), alpha=0.2, size = 0.75,show.legend = FALSE) +
    geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2) +
    geom_smooth(data = prep5,aes(x = x, y = y,group=group5,colour=Site),method='lm', se = FALSE,size=0.75,show.legend = FALSE, formula = y ~ x,linetype = "solid", alpha = 0.2) +  
    geom_line(size = 1,col = "black") +
    annotate(geom = "text", x = -1.15, y = 2.2, label ="(b)",size=5,colour=1) +
    labs(x=expression("ln "*italic("D")["g"]*" (kPa)"),y= " ") +
    theme(
      axis.ticks.length = unit(.15, "cm"),
      axis.text.y = element_text(size = 14),
      axis.text.x = element_text(size = 14),
      axis.title.x = element_text(size = 14),
      axis.title.y = element_text(size = 14),
      legend.text = element_text(size = 14),
      legend.background = element_rect(fill=NA),
      legend.title = element_text(size = 14), 
      legend.position = c(0.9, 0.15))
  plot8
  
  
  ## 4.2.C - logit chi against elevation (z) 
  xz.fit <- unlist(effz$x.all)
  trans <- I
  xz <- data.frame(lower = effz$lower, upper = effz$upper, fit = effz$fit, z_km = effz$x$z_km)
  xyz <- data.frame(x = xz.fit, y = xz$fit[closest(trans(xz.fit), xz$z_km )] + effz$residuals,Site=test$Site)
  
  
  plot9<-ggplot(xz,aes(x = z_km, y=fit)) + 
    # xlim(-1.7,1) +
    ylim(-0.25,2.25) +
    theme_classic() + 
    #  geom_smooth(data = xyz,aes(x = x, y = y,colour=Site),method='lm', se = FALSE,size=1,show.legend = FALSE, formula = y ~ x) +  
    geom_point(data = xyz, aes(x = x, y = y,colour=Site), alpha=0.2, size = 0.75,show.legend = FALSE) +
    geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2) +
    geom_line(size = 1,col = "black") +
    annotate(geom = "text", x = 0.36, y = 2.2, label ="(d)",size=5) +
    
    labs(x=expression(italic("z")*" (km)"),y= " ") +
    # scale_y_discrete(position="right",limits=c(-0.5,1.6)) +
    theme(
      axis.ticks.length = unit(.15, "cm"),
      #axis.ticks.y = element_blank(),
      axis.text.y = element_text(size = 14),
      axis.text.x = element_text(size = 14),
      axis.title.x = element_text(size = 14),
      axis.title.y = element_text(size = 14),
      #plot.title = element_text(size = 14),
      legend.text = element_text(size = 14),
      legend.background = element_rect(fill=NA),
      legend.title = element_text(size = 14), 
      legend.position = c(0.9, 0.15))
  plot9
  
  
  ##   ## 4.2.C - logit chi against ca 
  xca.fit <- unlist(effca$x.all)
  trans <- I
  xca <- data.frame(lower = effca$lower, upper = effca$upper, fit = effca$fit, ca_Pa = effca$x$ca_Pa)
  xyca <- data.frame(x = xca.fit, y = xca$fit[closest(trans(xca.fit), xca$ca_Pa)] + effca$residuals,Site=test$Site)
  
  prep5 <- xyca %>%
    mutate(group5 = gg) %>%
    group_by(group5) %>%
    
    mutate(
      slope = round(sens.slope(y)$estimates, 2),
      significance = round(sens.slope(y)$p.value, 4)
    ) %>%
    filter(significance < .05)   # only keep those with a pvalue < .05 
  
  plot10<-ggplot(xca,aes(x = ca_Pa, y=fit)) + 
    #   xlim(-1.4,1) +
    ylim(-0.25,2.25) +
    theme_classic() + 
    geom_point(data = xyca, aes(x = x, y = y,colour=Site), alpha=0.2, size = 0.75,show.legend = FALSE) +
    geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2) +
    geom_smooth(data = prep5,aes(x = x, y = y,group=group5,colour=Site),method='lm', se = FALSE,size=0.75,show.legend = FALSE, formula = y ~ x,linetype = "solid", alpha = 0.2) +  
    geom_line(size = 1,col = "black") +
    annotate(geom = "text", x = 21.5, y = 2.2, label ="(c)",size=5,colour=1) +
    labs(x=expression(italic("c")["a"]*" (Pa)"),y= " ") +
    theme(
      axis.ticks.length = unit(.15, "cm"),
      axis.text.y = element_text(size = 14),
      axis.text.x = element_text(size = 14),
      axis.title.x = element_text(size = 14),
      axis.title.y = element_text(size = 14),
      legend.text = element_text(size = 14),
      legend.background = element_rect(fill=NA),
      legend.title = element_text(size = 14), 
      legend.position = c(0.9, 0.15))
  plot10

  combo_plot <-grid.arrange(plot7,plot8,plot10,plot9,ncol = 4)

} 


# 5 -  Model bias against environmental drivers

  # 5.0 - Calculation of model bias as (prediction-observation)/observation*100
  
  for (i in 1:length(REG_all_5yrs2[,1])){
    if (i==1){
      REG_all_5yrs3<-cbind(REG_all_5yrs2[i,],((REG_all_5yrs2$cicasimu[i]-REG_all_5yrs2$cicaobs[i])/REG_all_5yrs2$cicaobs[i])*100)
    }else{
      REG_all_5yrs3<-rbind(REG_all_5yrs3,cbind(REG_all_5yrs2[i,],((REG_all_5yrs2$cicasimu[i]-REG_all_5yrs2$cicaobs[i])/REG_all_5yrs2$cicaobs[i])*100))
    }
  }
  
  colnames(REG_all_5yrs3)<-c(colnames(REG_all_5yrs2),"bias")
  
  
  # 5.1 - Model bias against environmental predictors
   # 5.1.a - summary linear model
  test<-subset(REG_all_5yrs3,!is.na(REG_all_5yrs3$bias))
  
  mod_obs<-lm(bias ~ Tg_degC + LnD + z_km + ca_Pa, data=test)
  summary(mod_obs) ## only significant with elevation
  
   # 5.1.b - Fig. 5
  
  {
  effT = effect("Tg_degC", mod_obs, partial.residuals=T);plot(effT)
  effD = effect("LnD", mod_obs, partial.residuals=T);plot(effD)
  effz = effect("z_km", mod_obs, partial.residuals=T);plot(effz)
  effca = effect("ca_Pa", mod_obs, partial.residuals=T);plot(effca)
  
  closest <- function(x, x0) apply(outer(x, x0, FUN=function(x, x0) abs(x - x0)), 1, which.min)
  
  ## Temperature 
  xT.fit <- unlist(effT$x.all)
  trans <- I
  xT <- data.frame(lower = effT$lower, upper = effT$upper, fit = effT$fit, Tg_degC = effT$x$Tg_degC)
  xyT <- data.frame(x = xT.fit, y = xT$fit[closest(trans(xT.fit), xT$Tg_degC)] + effT$residuals,Site=test$Site)
  
  prep5 <- xyT %>%
    mutate(group5 = gg) %>%
    group_by(group5) %>%
    mutate(
      slope = round(sens.slope(y)$estimates, 2),
      significance = round(sens.slope(y)$p.value, 4)
    ) %>%
    filter(significance < .05)   # only keep those with a pvalue < .05 
  
  
  plot11<-ggplot(xT,aes(x = Tg_degC, y=fit)) + 
    ylim(-22,50) +
    theme_classic() +
    geom_hline(yintercept=0, color="gray60") +
    geom_point(data = xyT, aes(x = x, y = y,colour=Site), alpha=0.3, size = 0.75,show.legend = FALSE) +
    geom_smooth(data = prep5,aes(x = x, y = y,group=group5,colour=Site),method='lm', se = FALSE,size=0.75,show.legend = FALSE, formula = y ~ x,linetype = "solid") +  
    labs(x=expression(italic("T")["g"]*" (°C)"),y= "Model bias (%)") +
    annotate(geom = "text", x = 9, y = 49, label ="(a)",size=5,colour=1) +
    theme(
      axis.ticks.length = unit(.15, "cm"),
      axis.text.y = element_text(size = 14),
      axis.text.x = element_text(size = 14),
      axis.title.x = element_text(size = 14),
      axis.title.y = element_text(size = 14),
      legend.text = element_text(size = 14),
      legend.background = element_rect(fill=NA),
      legend.title = element_text(size = 14), 
      legend.position = c(0.9, 0.15))
  plot11
  
  
  ## LnD
  xD.fit <- unlist(effD$x.all)
  trans <- I
  xD <- data.frame(lower = effD$lower, upper = effD$upper, fit = effD$fit, D = effD$x$LnD)
  xyD <- data.frame(x = xD.fit, y = xD$fit[closest(trans(xD.fit), xD$D)] + effD$residuals,Site=test$Site)
  
  prep5 <- xyD %>%
    mutate(group5 = gg) %>%
    group_by(group5) %>%
    mutate(
      slope = round(sens.slope(y)$estimates, 2),
      significance = round(sens.slope(y)$p.value, 4)
    ) %>%
    filter(significance < .05)   # only keep those with a pvalue < .05 
  
  
  plot12<-ggplot(xD,aes(x = D, y=fit)) + 
    xlim(-1.6,1) +
    ylim(-22,50) +
    theme_classic() + 
    geom_hline(yintercept=0, color="gray60") +
    geom_point(data = xyD, aes(x = x, y = y,colour=Site), alpha=0.3, size = 0.75,show.legend = FALSE) +
    geom_smooth(data = prep5,aes(x = x, y = y,group=group5,colour=Site),method='lm', se = FALSE,size=0.75,show.legend = FALSE, formula = y ~ x,linetype = "solid") +  
    labs(x=expression("ln "*italic("D")["g"]*" (kPa)"),y= " ") +
    annotate(geom = "text", x = -1.3, y = 49, label ="(b)",size=5,colour=1) +
    theme(
      axis.ticks.length = unit(.15, "cm"),
      axis.text.y = element_text(size = 14),
      axis.text.x = element_text(size = 14),
      axis.title.x = element_text(size = 14),
      axis.title.y = element_text(size = 14),
      legend.text = element_text(size = 14),
      legend.background = element_rect(fill=NA),
      legend.title = element_text(size = 14), 
      legend.position = c(0.9, 0.15))
  plot12
  
  
  ## Elevation 
  xz.fit <- unlist(effz$x.all)
  trans <- I
  xz <- data.frame(lower = effz$lower, upper = effz$upper, fit = effz$fit, z_km = effz$x$z_km)
  xyz <- data.frame(x = xz.fit, y = xz$fit[closest(trans(xz.fit), xz$z_km)] + effz$residuals,Site=test$Site)
  
  plot13<-ggplot(xz,aes(x = z_km, y=fit)) + 
    ylim(-22,50) +
    theme_classic() + 
    geom_hline(yintercept=0, color="gray60") +
    geom_point(data = xyz, aes(x = x, y = y,colour=Site), alpha=0.3, size = 0.75,show.legend = FALSE) +
    geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2) +
    geom_line(size = 1,col = "black") + 
    labs(x=expression(italic("z")*" (km)"),y= " ") +
    annotate(geom = "text", x = 0.4, y = 49, label ="(d)",size=5,colour=1) +
    theme(
      axis.ticks.length = unit(.15, "cm"),
      axis.text.y = element_text(size = 14),
      axis.text.x = element_text(size = 14),
      axis.title.x = element_text(size = 14),
      axis.title.y = element_text(size = 14),
      legend.text = element_text(size = 14),
      legend.background = element_rect(fill=NA),
      legend.title = element_text(size = 14), 
      legend.position = c(0.9, 0.15))
  plot13
  
  
  ## ca
  xca.fit <- unlist(effca$x.all)
  trans <- I
  xca <- data.frame(lower = effca$lower, upper = effca$upper, fit = effca$fit, ca_Pa = effca$x$ca_Pa)
  xyca <- data.frame(x = xca.fit, y = xca$fit[closest(trans(xca.fit), xca$ca_Pa)] + effca$residuals,Site=test$Site)
  
  prep5 <- xyca %>%
    mutate(group5 = gg) %>%
    group_by(group5) %>%
    mutate(
      slope = round(sens.slope(y)$estimates, 2),
      significance = round(sens.slope(y)$p.value, 4)
    ) %>%
    filter(significance < .05)   # only keep those with a pvalue < .05 
  
  
  plot14<-ggplot(xca,aes(x = ca_Pa, y=fit)) + 
    ylim(-22,50) +
    theme_classic() + 
    geom_hline(yintercept=0, color="gray60") +
    geom_point(data = xyca, aes(x = x, y = y,colour=Site), alpha=0.2, size = 0.75,show.legend = FALSE) +
    geom_smooth(data = prep5,aes(x = x, y = y,group=group5,colour=Site),method='lm', se = FALSE,size=0.75,show.legend = FALSE, formula = y ~ x,linetype = "solid", alpha = 0.2) +  
    annotate(geom = "text", x = 22, y = 49, label ="(c)",size=5,colour=1) +
    labs(x=expression(italic("c")["a"]*" (Pa)"),y= " ") +
    theme(
      axis.ticks.length = unit(.15, "cm"),
      axis.text.y = element_text(size = 14),
      axis.text.x = element_text(size = 14),
      axis.title.x = element_text(size = 14),
      axis.title.y = element_text(size = 14),
      legend.text = element_text(size = 14),
      legend.background = element_rect(fill=NA),
      legend.title = element_text(size = 14), 
      legend.position = c(0.9, 0.15))
  plot14
  
  combo_plot <-grid.arrange(plot11,plot12,plot14,plot13,ncol = 4)
  }

  
  # 5.2 - Model bias against soil moisture indices 
  # Linear model of bias against environmental predictors and one indice of soil water availability for common 1978-2014 period

  {
   # 5.2.a - SPEI: Standardized Precipitation-Evapotranspiration Index from 0.5° gridded monthly SPEIbase dataset (Beguería et al., 2010)

  test<-subset(REG_all_5yrs3,(!is.na(REG_all_5yrs3$bias)) & REG_all_5yrs3$Year >=1978)
  mod_obs<-lm(bias ~ Tg_degC + LnD + z_km + ca_Pa + SPEI, data=test)
  summary(mod_obs)
  
  test2<-test;test2$Tg_degC<-test2$Tg_degC-25
    mod_obs1<-lm(logobs ~ Tg_degC + LnD + z_km + ca_Pa + SPEI, data=test2)
  summary(mod_obs1);model_performance(mod_obs1)

  
  effa = effect("SPEI", mod_obs, partial.residuals=T);plot(effa)
  xa.fit <- unlist(effa$x.all)
  trans <- I
  xa <- data.frame(lower = effa$lower, upper = effa$upper, fit = effa$fit, SPEI = effa$x$SPEI)
  xya <- data.frame(x = xa.fit, y = xa$fit[closest(trans(xa.fit), xa$SPEI)] + effa$residuals,Site=test$Site)
  
  for (i in 1:length(unique(xya$Site))){
    if(i==1){
      h<-length(xya$Site[xya$Site==unique(xya$Site)[i]])
    }else{
      h<-c(h,length(xya$Site[xya$Site==unique(xya$Site)[i]]))  
    }
  }
  
  for (i in 1:length(unique(xya$Site))){
    if(i==1){
      hh<-rep(i,h[i])
    }else{
      hh<-c(hh,rep(i,h[i]))  
    }
  }
  
  prep5 <- xya %>%
    mutate(group5 = hh) %>%
    group_by(group5) %>%
    mutate(
      slope = round(sens.slope(y)$estimates, 2),
      significance = round(sens.slope(y)$p.value, 4)
    ) %>%
    filter(significance < .05)   # only keep those with a pvalue < .05 
  
  
  plot15<-ggplot(xa,aes(x = SPEI, y=fit)) + 
    ylim(-22,38) +
    theme_classic() + 
    geom_hline(yintercept=0, color="gray60") +
    geom_point(data = xya, aes(x = x, y = y,colour=Site), alpha=0.3, size = 0.75,show.legend = FALSE) +
    geom_smooth(data = prep5,aes(x = x, y = y,group=group5,colour=Site),method='lm', se = FALSE,size=0.75,show.legend = FALSE, formula = y ~ x,linetype = "solid") +  
    annotate(geom = "text", x = -0.5, y = 37, label ="(a)",size=5,colour=1) +
    labs(x=expression("SPEI"),y= expression("Model bias (%)")) +
    theme(
      axis.ticks.length = unit(.15, "cm"),
      axis.text.y = element_text(size = 14),
      axis.text.x = element_text(size = 14),
      axis.title.x = element_text(size = 14),
      axis.title.y = element_text(size = 14),
      legend.text = element_text(size = 14),
      legend.background = element_rect(fill=NA),
      legend.title = element_text(size = 14), 
      legend.position = c(0.9, 0.15))
  plot15
  
  
   # 5.2.b - alpha: Priestley-Taylor coefficient from SPLASH model
  
  test<-subset(REG_all_5yrs3,(!is.na(REG_all_5yrs3$bias)) & REG_all_5yrs3$Year >=1978)
  
  mod_obs<-lm(bias ~ Tg_degC + LnD + z_km + ca_Pa + alpha, data=test)
  summary(mod_obs) ## p <0.05 for alpha
  
  test2<-test;test2$Tg_degC<-test2$Tg_degC-25
  mod_obs1<-lm(logobs ~ Tg_degC + LnD + z_km + ca_Pa + alpha, data=test2)
  summary(mod_obs1);model_performance(mod_obs1)
  
  
  effc = effect("alpha", mod_obs, partial.residuals=T);plot(effc)
  
  xc.fit <- unlist(effc$x.all)
  trans <- I
  xc <- data.frame(lower = effc$lower, upper = effc$upper, fit = effc$fit, alpha = effc$x$alpha)
  xyc <- data.frame(x = xc.fit, y = xc$fit[closest(trans(xc.fit), xc$alpha)] + effc$residuals,Site=test$Site)
  
  prep5 <- xyc %>%
    mutate(group5 = hh) %>%
    group_by(group5) %>%
    mutate(
      slope = round(sens.slope(y)$estimates, 2),
      significance = round(sens.slope(y)$p.value, 4)
    ) %>%
    filter(significance < .05)   # only keep those with a pvalue < .05 
  
  
  plot16<-ggplot(xc,aes(x = alpha, y=fit)) + 
    ylim(-22,38) +
    theme_classic() + 
    geom_hline(yintercept=0, color="gray60") +
    geom_point(data = xyc, aes(x = x, y = y,colour=Site), alpha=0.3, size = 0.75,show.legend = FALSE) +
    geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2) +
    geom_smooth(data = prep5,aes(x = x, y = y,group=group5,colour=Site),method='lm', se = FALSE,size=0.75,show.legend = FALSE, formula = y ~ x,linetype = "solid") +  
    geom_line(size = 1,col = "black") + 
    annotate(geom = "text", x = 0.5, y = 37, label ="(c)",size=5,colour=1) +
    labs(x=expression(alpha),y= expression(" ")) +
    theme(
      axis.ticks.length = unit(.15, "cm"),
      axis.text.y = element_text(size = 14),
      axis.text.x = element_text(size = 14),
      axis.title.x = element_text(size = 14),
      axis.title.y = element_text(size = 14),
      legend.text = element_text(size = 14),
      legend.background = element_rect(fill=NA),
      legend.title = element_text(size = 14), 
      legend.position = c(0.9, 0.15))
  plot16
  
  
   # 5.2.c - teta: surface soil moisture content from the 0.25° resolution product of the European Space Agency Climate Change Initiative (Dorigo et al., 2017)
  
  test<-subset(REG_all_5yrs3,(!is.na(REG_all_5yrs3$bias)) & REG_all_5yrs3$Year >=1978)
  
  test2<-subset(test,!is.na(test$teta))
  
  mod_obs<-lm(bias ~ Tg_degC + LnD + z_km + ca_Pa + teta, data=test2)
  summary(mod_obs) ## p < 0.05
  
  test2<-test;test2$Tg_degC<-test2$Tg_degC-25
  mod_obs1<-lm(logobs ~ Tg_degC + LnD + z_km + ca_Pa + teta, data=test2)
  summary(mod_obs1);model_performance(mod_obs1)
  
  
  effb = effect("teta", mod_obs, partial.residuals=T);plot(effb)
  
  xb.fit <- unlist(effb$x.all)
  trans <- I
  xb <- data.frame(lower = effb$lower, upper = effb$upper, fit = effb$fit, teta = effb$x$teta)
  xyb <- data.frame(x = xb.fit, y = xb$fit[closest(trans(xb.fit), xb$teta)] + effb$residuals,Site=test2$Site)
  
  for (i in 1:length(unique(xyb$Site))){
    if(i==1){
      h<-length(xyb$Site[xyb$Site==unique(xyb$Site)[i]])
    }else{
      h<-c(h,length(xyb$Site[xyb$Site==unique(xyb$Site)[i]]))  
    }
  }
  
  for (i in 1:length(unique(xyb$Site))){
    if(i==1){
      hh<-rep(i,h[i])
    }else{
      hh<-c(hh,rep(i,h[i]))  
    }
  }
  
  
  prep5 <- xyb %>%
    mutate(group5 = hh) %>%
    group_by(group5) %>%
    
    mutate(
      slope = round(sens.slope(y)$estimates, 2),
      significance = round(sens.slope(y)$p.value, 4)
    ) %>%
    filter(significance < .05)   # only keep those with a pvalue < .05 
  
  
  plot19<-ggplot(xb,aes(x = teta, y=fit)) + 
    ylim(-22,38) +
    theme_classic() + 
    geom_hline(yintercept=0, color="gray60") +
    geom_point(data = xyb, aes(x = x, y = y,colour=Site), alpha=0.3, size = 0.75,show.legend = FALSE) +
    geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2) +
    geom_smooth(data = prep5,aes(x = x, y = y,group=group5,colour=Site),method='lm', se = FALSE,size=0.75,show.legend = FALSE, formula = y ~ x,linetype = "solid") +  
    geom_line(size = 1,col = "black") + 
    annotate(geom = "text", x = 0.15, y = 37, label ="(b)",size=5,colour=1) +
    labs(x=expression(theta*" (m"^{3}*" m"^{-3}*")"),y= " ") +
    theme(
      axis.ticks.length = unit(.15, "cm"),
      axis.text.y = element_text(size = 14),
      axis.text.x = element_text(size = 14),
      axis.title.x = element_text(size = 14),
      axis.title.y = element_text(size = 14),
      legend.text = element_text(size = 14),
      legend.background = element_rect(fill=NA),
      legend.title = element_text(size = 14), 
      legend.position = c(0.9, 0.15))
  plot19
  }
  
    # 5.2.d - Fig. 7
  combo_plot <-grid.arrange(plot17,plot19,plot18,ncol = 3,nrow=1)



