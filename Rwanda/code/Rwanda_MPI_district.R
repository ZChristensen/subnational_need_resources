list.of.packages <- c("data.table","readr","foreign")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only=T)

if(Sys.info()[["user"]]=="Alex"){
  prefix = "C:"
}else{
  prefix = "E:"
}

wd = paste0(prefix,"/git/mpi_recalc/pov")
setwd(wd)


weighted.percentile <- function(x,w,prob,na.rm=TRUE){
  df <- data.frame(x,w)
  if(na.rm){
    df <- df[which(complete.cases(df)),]
  }
  #Sort 
  df <- df[order(df$x),]
  sumw <- sum(df$w)
  df$cumsumw <- cumsum(df$w)
  #For each percentile
  cutList <- c()
  cutNames <-c()
  for(i in 1:length(prob)){
    p <- prob[i]
    pStr <- paste0(round(p*100,digits=2),"%")
    sumwp <- sumw*p
    df$above.prob <- df$cumsumw>=sumwp
    thisCut <- df$x[which(df$above.prob==TRUE)[1]]
    cutList <- c(cutList,thisCut)
    cutNames <- c(cutNames,pStr)
  }
  names(cutList) <- cutNames
  return(cutList)
}


rawMPI=read.dta("rwa_dhs14-15_pov.dta",convert.factors = T)

rawMPI$sample.weights=rawMPI$hv005/100000
rawMPI$wealth=rawMPI$hv271/100000
rawMPI$MPI.A=((rawMPI$d_cm+rawMPI$d_nutr)*(1/6))+((rawMPI$d_satt+rawMPI$d_educ)*(1/6))+((rawMPI$d_elct+rawMPI$d_ckfl+rawMPI$d_sani+rawMPI$d_hsg+rawMPI$d_asst+rawMPI$d_wtr)*(1/18))
rawMPI$MPI.H=(rawMPI$MPI.A>(1/3))*1
cuts=c(.8611,.5150,.2)

povperc <- weighted.percentile(rawMPI$wealth,rawMPI$sample.weights,prob=cuts)
rawMPI$p20 <- (rawMPI$wealth < povperc[1])
rawMPI$ext <- (rawMPI$wealth < povperc[2])
rawMPI$np20<- (rawMPI$wealth < povperc[3])

district=data.table(rawMPI)[,.(
           Child.mortality=weighted.mean(d_cm, sample.weights,na.rm=T)
           ,Nutrition=weighted.mean(d_nutr, sample.weights,na.rm=T)
           ,School.attendance=weighted.mean(d_satt, sample.weights,na.rm=T)
           ,Years.of.schooling=weighted.mean(d_educ, sample.weights,na.rm=T)
           ,Electricity=weighted.mean(d_elct, sample.weights,na.rm=T)
           ,Drinking.water=weighted.mean(d_wtr, sample.weights,na.rm=T)
           ,Sanitation=weighted.mean(d_sani, sample.weights,na.rm=T)
           ,Housing=weighted.mean(d_hsg, sample.weights,na.rm=T)
           ,Assets=weighted.mean(d_asst, sample.weights,na.rm=T)
           ,Cooking...fuel=weighted.mean(d_ckfl, sample.weights,na.rm=T)
           ,MPI.A=weighted.mean(MPI.A,sample.weights,na.rm=T)
           ,MPI.H=weighted.mean(MPI.H,sample.weights,na.rm=T)
           ,p20=weighted.mean(p20,sample.weights,na.rm=T)
           ,ext=weighted.mean(ext,sample.weights,na.rm=T)
           ,np20=weighted.mean(np20,sample.weights,na.rm=T)
           ,weights=sum(sample.weights)
           ),by=c("shdistrict")]
district$MPI=district$MPI.A*district$MPI.H

weights=sum(rawMPI$sample.weight)
district$share.of.pop=district$weights/weights
fwrite(district,"E:/git/subnational_need_resources/Rwanda/input/Rwanda_MPI_district.csv")
