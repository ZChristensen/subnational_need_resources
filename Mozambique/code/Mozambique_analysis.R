list.of.packages <- c("data.table","readr","reshape2","readxl","ggplot2")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only=T)

if(Sys.info()[["user"]]=="Alex"){
  prefix = "C:"
}else{
  prefix = "E:"
}

wd = paste0(prefix,"/git/mpi_recalc")
setwd(wd)

z.score=function(x){(x-mean(x))/sd(x)}
r2=function(x,y){round(summary(lm(x ~ y))$r.squared,4)}
outlier = function(x){
  qnt = quantile(x, probs=c(0.25, 0.75), na.rm=T)
  H = 1.5 * IQR(x, na.rm=T)
  return(
    (x < (qnt[1] - H)) | (x > (qnt[2] + H))
  )
}

MPIsubnational=read_excel("MPI_downloads/Table-5-Subnational-MPI-2018-1.xlsx", sheet="5.5 Raw Headcounts Region",range=cell_rows(9:1138))
MPIsubnational=MPIsubnational[,c(
  "ISO Num code"                                        
  ,"ISO Code"                                            
  ,"Country"                                             
  ,"World region"                                        
  ,"Survey Name"                                         
  ,"Survey year"                                         
  ,"Subnational \r\nregion"                              
  ,"MPI of the country"                                  
  ,"MPI of the region"                                   
  ,"Nutrition"                                           
  ,"Child mortality"                                     
  ,"Years of schooling"                                  
  ,"School attendance"                                   
  ,"Cooking \r\nfuel"                                    
  ,"Sanitation"                                          
  ,"Drinking water"                                      
  ,"Electricity"                                         
  ,"Housing"                                             
  ,"Assets"                                              
  ,"Pop survey year"                                     
  ,"Population 2015"                                     
  ,"Population 2016"                                     
  ,"Population share by region"                          
  ,"Population size by \r\nregion"                       
  ,"Total number of indicators included \r\n(out of ten)"
  ,"Indicator (s) missing"
)]
MPIsubnational=MPIsubnational[which(MPIsubnational$Country=="Afghanistan"),]
names(MPIsubnational)=make.names(names(MPIsubnational))
MPIsubnational$region=MPIsubnational$Subnational...region

funding=read.csv("funding/AFG_GovExp_ODA_2017.csv")
funding=funding[1:34,]
setnames(funding,"Province","region")
funding$region=as.character(funding$region)
funding$region[which(funding$region=="Uruzgan")]="Urozgan"
dat=merge(funding,MPIsubnational,by=c("region"))

Province.Names=read.csv("funding/AFG_province_names.csv")
AD_pop=read.csv("funding/AidData_AFG_pop.csv")
#AidData numbers taken from the GPWv4 UN Adjusted 2015 numbers which are taken from CIESIN
AD_pop=AD_pop[,c("gpw_v4_count.2015.sum","Name")]
names(AD_pop)=c("AidData.2015.pop","aidData.list")
AD_pop=merge(AD_pop,Province.Names,by=c("aidData.list"))
AD_pop=AD_pop[c("AidData.2015.pop","MPI.list")]
names(AD_pop)=c("AidData.2015.pop","region")
dat=merge(dat,AD_pop,by=c("region"))


# dat$MPI.Z2=z.score(dat$`MPI of the region`)
# dat$Child.mort.Z=z.score(dat$`Child mortality`)
# dat$health.pc.Z=z.score(dat$Health.spending..per.capita.US..)
# dat$health.ODA.Z=z.score(dat$Health.ODA..off.budget.disbursements..per.capita.US..)
dat$MPI.pop.2016=dat$Population.size.by...region*1000
dat$Afg.CSO.pop.2017.2018=dat$X2017.18.population

pop.long=melt(dat[,c("region","MPI.pop.2016","AidData.2015.pop","Afg.CSO.pop.2017.2018")],id.vars=c("region")) 
ggplot(pop.long, aes(x=region,y=(value/1000000)))+
  geom_point(aes(color=factor(variable)))+
  labs(title="Afghanistan population estimates",x="",y="Population (millions)")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  theme(legend.title=element_blank())
ggsave("graphics/population_estimates.png")
#MPI pops
dat$Gov.Health.Tot.PC.MPI=dat$Health.spending..total.US../dat$MPI.pop.2016
dat$OFF.H.PC.MPI=dat$Health.ODA..off.budget.disbursements./dat$MPI.pop.2016
dat$ON.H.PC.MPI=dat$Health.ODA..on.budget.commitments./dat$MPI.pop.2016
dat$OFF.Gov.Health.Tot.PC.MPI=dat$Gov.Health.Tot.PC.MPI+dat$OFF.H.PC.MPI
dat$ON.OFF.Gov.Health.Tot.PC.MPI=dat$OFF.Gov.Health.Tot.PC.MPI+dat$ON.H.PC.MPI

dat$Gov.Edu.Tot.PC.MPI=dat$General.education.spending..total.US../dat$MPI.pop.2016
dat$OFF.Edu.PC.MPI=dat$Education.ODA..off.budget.disbursements./dat$MPI.pop.2016
dat$ON.Edu.PC.MPI=dat$Education.ODA..on.budget.commitments./dat$MPI.pop.2016
dat$OFF.Gov.Edu.Tot.PC.MPI=dat$Gov.Edu.Tot.PC.MPI+dat$OFF.Edu.PC.MPI
dat$ON.OFF.Gov.Edu.Tot.PC.MPI=dat$OFF.Gov.Edu.Tot.PC.MPI+dat$ON.Edu.PC.MPI
dat$ON.OFF.G.Edu.and.HealthPC.MPI=dat$ON.OFF.Gov.Edu.Tot.PC.MPI+dat$ON.OFF.Gov.Health.Tot.PC.MPI
dat$OFF.G.Edu.and.HealthPC.MPI=dat$OFF.Gov.Edu.Tot.PC.MPI+dat$OFF.Gov.Health.Tot.PC.MPI

#AidData pops
dat$Gov.Health.Tot.PC.AidData=dat$Health.spending..total.US../dat$AidData.2015.pop
dat$OFF.H.PC.AidData=dat$Health.ODA..off.budget.disbursements./dat$AidData.2015.pop
dat$ON.H.PC.AidData=dat$Health.ODA..on.budget.commitments./dat$AidData.2015.pop
dat$OFF.Gov.Health.Tot.PC.AidData=dat$Gov.Health.Tot.PC.AidData+dat$OFF.H.PC.AidData
dat$ON.OFF.Gov.Health.Tot.PC.AidData=dat$OFF.Gov.Health.Tot.PC.AidData+dat$ON.H.PC.AidData

dat$Gov.Edu.Tot.PC.AidData=dat$General.education.spending..total.US../dat$AidData.2015.pop
dat$OFF.Edu.PC.AidData=dat$Education.ODA..off.budget.disbursements./dat$AidData.2015.pop
dat$ON.Edu.PC.AidData=dat$Education.ODA..on.budget.commitments./dat$AidData.2015.pop
dat$OFF.Gov.Edu.Tot.PC.AidData=dat$Gov.Edu.Tot.PC.AidData+dat$OFF.Edu.PC.AidData
dat$ON.OFF.Gov.Edu.Tot.PC.AidData=dat$OFF.Gov.Edu.Tot.PC.AidData+dat$ON.Edu.PC.AidData
dat$ON.OFF.G.Edu.and.HealthPC.AidData=dat$ON.OFF.Gov.Edu.Tot.PC.AidData+dat$ON.OFF.Gov.Health.Tot.PC.AidData
dat$OFF.G.Edu.and.HealthPC.AidData=dat$OFF.Gov.Edu.Tot.PC.AidData+dat$OFF.Gov.Health.Tot.PC.AidData

#CSO pops
dat$Gov.Health.Tot.PC.CSO=dat$Health.spending..total.US../dat$Afg.CSO.pop.2017.2018
dat$OFF.H.PC.CSO=dat$Health.ODA..off.budget.disbursements./dat$Afg.CSO.pop.2017.2018
dat$ON.H.PC.CSO=dat$Health.ODA..on.budget.commitments./dat$Afg.CSO.pop.2017.2018
dat$OFF.Gov.Health.Tot.PC.CSO=dat$Gov.Health.Tot.PC.CSO+dat$OFF.H.PC.CSO
dat$ON.OFF.Gov.Health.Tot.PC.CSO=dat$OFF.Gov.Health.Tot.PC.CSO+dat$ON.H.PC.CSO

dat$Gov.Edu.Tot.PC.CSO=dat$General.education.spending..total.US../dat$Afg.CSO.pop.2017.2018
dat$OFF.Edu.PC.CSO=dat$Education.ODA..off.budget.disbursements./dat$Afg.CSO.pop.2017.2018
dat$ON.Edu.PC.CSO=dat$Education.ODA..on.budget.commitments./dat$Afg.CSO.pop.2017.2018
dat$OFF.Gov.Edu.Tot.PC.CSO=dat$Gov.Edu.Tot.PC.CSO+dat$OFF.Edu.PC.CSO
dat$ON.OFF.Gov.Edu.Tot.PC.CSO=dat$OFF.Gov.Edu.Tot.PC.CSO+dat$ON.Edu.PC.CSO
dat$ON.OFF.G.Edu.and.HealthPC.CSO=dat$ON.OFF.Gov.Edu.Tot.PC.CSO+dat$ON.OFF.Gov.Health.Tot.PC.CSO
dat$OFF.G.Edu.and.HealthPC.CSO=dat$OFF.Gov.Edu.Tot.PC.CSO+dat$OFF.Gov.Health.Tot.PC.CSO

dat.long=melt(dat, id.vars=c("region","MPI.of.the.region","Child.mortality","School.attendance"))
vars.health=c("ON.OFF.Gov.Health.Tot.PC.AidData","ON.OFF.Gov.Health.Tot.PC.CSO","ON.OFF.Gov.Health.Tot.PC.MPI")
vars.edu=c("ON.OFF.Gov.Edu.Tot.PC.AidData","ON.OFF.Gov.Edu.Tot.PC.CSO","ON.OFF.Gov.Edu.Tot.PC.MPI")
vars.humcap=c("OFF.G.Edu.and.HealthPC.MPI","OFF.G.Edu.and.HealthPC.MPI","OFF.G.Edu.and.HealthPC.MPI")
vars.frame=list(vars.health,vars.edu,vars.humcap)

nams=c("Health","Education","Human.Capital")



nam="Health"
vari=c("ON.OFF.Gov.Health.Tot.PC.AidData", "ON.OFF.Gov.Health.Tot.PC.CSO" ,"ON.OFF.Gov.Health.Tot.PC.MPI")

dat2=dat[,c("region","Child.mortality","ON.OFF.Gov.Health.Tot.PC.AidData", "ON.OFF.Gov.Health.Tot.PC.CSO" ,"ON.OFF.Gov.Health.Tot.PC.MPI" )]
dat2$ymed.Health=apply(dat[,vari],1,median)
dat2$ymin.Health=apply(dat[,vari],1,min)
dat2$ymax.Health=apply(dat[,vari],1,max)


filenam=paste0("graphics/",nam,"_mortality.png")

r2s=c(r2(dat2$Child.mortality,dat2$ymin.Health),r2(dat2$Child.mortality,dat2$ymed.Health),r2(dat2$Child.mortality,dat2$ymax.Health))

ggplot()+
  geom_pointrange(data=dat2,mapping=aes(x=(Child.mortality/100),ymin=ymin.Health,ymax=ymax.Health,y=ymed.Health))+
  geom_text(check_overlap=T)+
  geom_text(x=.75*(max(dat2$Child.mortality/100)),y=.75*(max(dat2$ymax.Health)),label=paste("R^2: ",r2s), parse=T)+
  theme_classic()+
  scale_x_continuous(labels=scales::percent)+
  scale_y_continuous(labels=scales::dollar)+
  labs(title=paste(nam," and mortality with three population estimates"),y=paste(nam,"spending per capita"),x="Mortality")
ggsave(filenam)

nams=c("Health","Education","Human.Capital")


nam="Education"
vari=c("ON.OFF.Gov.Edu.Tot.PC.AidData","ON.OFF.Gov.Edu.Tot.PC.CSO","ON.OFF.Gov.Edu.Tot.PC.MPI")

dat2=dat[,c("region","School.attendance","ON.OFF.Gov.Edu.Tot.PC.AidData","ON.OFF.Gov.Edu.Tot.PC.CSO","ON.OFF.Gov.Edu.Tot.PC.MPI" )]
dat2$ymed.Education=apply(dat[,vari],1,median)
dat2$ymin.Education=apply(dat[,vari],1,min)
dat2$ymax.Education=apply(dat[,vari],1,max)


filenam=paste0("graphics/",nam,"_absenteeism.png")

ggplot()+
  geom_pointrange(data=dat2,mapping=aes(x=(School.attendance/100),ymin=ymin.Education,ymax=ymax.Education,y=ymed.Education))+
  geom_text(check_overlap=T)+
  scale_x_continuous(labels=scales::percent)+
  scale_y_continuous(labels=scales::dollar)+
  theme_classic()+
  labs(title=paste(nam,"and school absenteeism with three population esitmates"),y=paste(nam,"spending per capita"),x="Households where at least one child was out of school")
ggsave(filenam)  


nam="Human Capital"

dat2=dat[,c("region","MPI.of.the.region",vari)]
dat2$ymed.Human.Capital=apply(dat[,vari],1,median)
dat2$ymin.Human.Capital=apply(dat[,vari],1,min)
dat2$ymax.Human.Capital=apply(dat[,vari],1,max)


filenam=paste0("graphics/",nam,"_MPI.png")

ggplot()+
  geom_pointrange(data=dat2,mapping=aes(x=(MPI.of.the.region),ymin=ymin.Human.Capital,ymax=ymax.Human.Capital,y=ymed.Human.Capital))+
  geom_text(check_overlap=T)+
  scale_x_continuous(labels=scales::percent)+
  scale_y_continuous(labels=scales::dollar)+
  theme_classic()+
  labs(title=paste(nam,"and MPI with three populations"),y=paste(nam,"spending per capita"),x="Multidimensional Poverty Index")
ggsave(filenam)  

# dat$low.health.spend="other countries"
# dat$low.health.spend[which(dat$total.health.spend<(mean(dat$total.health.spend)-sd(dat$total.health.spend)))]="high spend"
# #No regions are below one sd from the mean total health spending
# dat$high.child.mort="other countries"
# dat$high.child.mort[which(dat$`Child mortality`>(mean(dat$`Child mortality`)+sd(dat$`Child mortality`)))]="high mortality"
# #Kandahar and Nooristan have a child mortality that is more than 1 sd above the mean
# dat$low.educ.spend="other countries"
# dat$low.educ.spend[which(dat$total.educ.spend<(mean(dat$total.educ.spend)-sd(dat$total.educ.spend)))]=1
# #Kabul Khost are the provinces with low total education spending pc
# dat$low.school.attend="other countries"
# dat$low.school.attend[which(dat$`School attendance`>(mean(dat$`School attendance`)+sd(dat$`School attendance`)))]="low attendance"
# #Balkh    Faryab   Kabul    Kapisa   Panjsher are the provinces that are more than 1 SD below the mean 
# dat$high.MPI="other countries"
# dat$high.MPI[which(dat$`MPI of the region`>(mean(dat$`MPI of the region`)+sd(dat$`MPI of the region`)))]="high MPI"


dat$need.vs.resources=NA
dat$need.vs.resources[which(dat$OFF.G.Edu.and.HealthPC<=median(dat$OFF.G.Edu.and.HealthPC)&dat$`MPI of the region`<=median(dat$`MPI of the region`))]="low poverty low resources"
dat$need.vs.resources[which(dat$OFF.G.Edu.and.HealthPC>median(dat$OFF.G.Edu.and.HealthPC)&dat$`MPI of the region`<=median(dat$`MPI of the region`))]="low poverty high resources"
dat$need.vs.resources[which(dat$OFF.G.Edu.and.HealthPC>median(dat$OFF.G.Edu.and.HealthPC)&dat$`MPI of the region`>median(dat$`MPI of the region`))]="high poverty high resources"
dat$need.vs.resources[which(dat$OFF.G.Edu.and.HealthPC<=median(dat$OFF.G.Edu.and.HealthPC)&dat$`MPI of the region`>median(dat$`MPI of the region`))]="high poverty low resources"



dat.long=melt(dat,id.vars=c("region","need.vs.resources","Child mortality","School attendance","MPI of the region"))




# dat$on.need.vs.resources=NA
# dat$on.need.vs.resources[which(dat$ON.OFF.G.Edu.and.HealthPC<=median(dat$ON.OFF.G.Edu.and.HealthPC)&dat$`MPI of the region`<=median(dat$`MPI of the region`))]="low poverty low resources"
# dat$on.need.vs.resources[which(dat$ON.OFF.G.Edu.and.HealthPC>median(dat$ON.OFF.G.Edu.and.HealthPC)&dat$`MPI of the region`<=median(dat$`MPI of the region`))]="low poverty high resources"
# dat$on.need.vs.resources[which(dat$ON.OFF.G.Edu.and.HealthPC>median(dat$ON.OFF.G.Edu.and.HealthPC)&dat$`MPI of the region`>median(dat$`MPI of the region`))]="high poverty high resources"
# dat$on.need.vs.resources[which(dat$ON.OFF.G.Edu.and.HealthPC<=median(dat$ON.OFF.G.Edu.and.HealthPC)&dat$`MPI of the region`>median(dat$`MPI of the region`))]="high poverty low resources"
# adding on budGov.Edu.Tot aid shifts Herat from "High Poverty High Resources" to "High Poverty Low Resources" and shifts Paktya from "low poverty low resource" to "low poverty high resource"
aid.vars= c(
  "Gov.Health.Tot.PC"                                                             
  , "OFF.H.PC"                                                           
  , "ON.H.PC"                                                            
  , "OFF.Gov.Health.Tot.PC"                                                         
  , "ON.OFF.Gov.Health.Tot.PC"                                                      
  , "Gov.Edu.Tot.PC"                                                             
  , "OFF.Edu.PC"                                                           
  , "ON.Edu.PC"                                                            
  , "OFF.Gov.Edu.Tot.PC"                                                         
  , "ON.OFF.Gov.Edu.Tot.PC"                                                      
  , "ON.OFF.Edu.and.HealthPC"                                                       
  , "OFF.G.Edu.and.HealthPC"                                                        
  , "total.health.spend"                                                 
  ,"total.educ.spend"                                                   
  , "total.health.and.educ.spend"                                       
  , "ON.OFF.G.Edu.and.HealthPC")
h.aid.vars= c(
  "Gov.Health.Tot.PC"                                                             
  , "OFF.H.PC"                                                           
  , "ON.H.PC"                                                            
  , "OFF.Gov.Health.Tot.PC"                                                         
  , "ON.OFF.Gov.Health.Tot.PC"                                                      
  , "ON.OFF.Edu.and.HealthPC"                                                       
  , "OFF.G.Edu.and.HealthPC"                                                        
  , "total.health.spend"                                                 
  , "total.health.and.educ.spend"                                       
  , "ON.OFF.G.Edu.and.HealthPC")

h.aid.long=subset(dat.long, variable %in% h.aid.vars)
h.aid.long$value=as.numeric(h.aid.long$value)
h.aid.long$Child.mortality=h.aid.long$Child.mortality/100
h.aid.long=data.table(h.aid.long)[
  ,r2:=r2(.SD$value,.SD$Child.mortality)
  ,by=.(variable)
  ]
h.aid.long=data.table(h.aid.long)[
  ,value.outlier:=outlier(.SD$value)
  ,by=.(variable)
  ]
h.aid.long$cm.outlier=outlier(h.aid.long$Child.mortality)
h.aid.long=h.aid.long[which(!h.aid.long$value.outlier | !h.aid.long$cm.outlier),]
h.aid.long=h.aid.long[order(h.aid.long$r2),]
h.aid.long$variable=as.character(h.aid.long$variable)
h.aid.long$variable=factor(h.aid.long$variable,levels=unique(h.aid.long$variable))

ggplot(h.aid.long,aes(x=value,y=Child.mortality))+
  facet_wrap(~variable)+
  geom_point(aes(color=factor(need.vs.resources)))+
  geom_smooth(method="lm")+
  geom_text(x=150,y=.3,aes(label=paste("R^2: ",r2)))+
  scale_y_continuous(labels=scales::percent)+
  scale_x_continuous(labels=scales::dollar)
ggsave("graphics/different_aid_mortality.png")

##education
ed.vars= c(
  "Gov.Edu.Tot.PC"                                                             
  , "OFF.Edu.PC"                                                           
  , "ON.Edu.PC"                                                            
  , "OFF.Gov.Edu.Tot.PC"                                                         
  , "ON.OFF.Gov.Edu.Tot.PC"                                                      
  , "ON.OFF.Edu.and.HealthPC"                                                       
  , "OFF.G.Edu.and.HealthPC"                                                        
  ,"total.educ.spend"                                                   
  , "total.health.and.educ.spend"                                       
  , "ON.OFF.G.Edu.and.HealthPC")


e.aid.long=subset(dat.long, variable %in% ed.vars)
e.aid.long$value=as.numeric(e.aid.long$value)
e.aid.long=data.table(e.aid.long)[
  ,r2:=r2(.SD$value,.SD$School.attendance)
  ,by=.(variable)
  ]
e.aid.long=data.table(e.aid.long)[
  ,value.outlier:=outlier(.SD$value)
  ,by=.(variable)
  ]
e.aid.long$ed.outlier=outlier(e.aid.long$School.attendance)
e.aid.long=e.aid.long[which(!e.aid.long$value.outlier | !e.aid.long$ed.outlier),]
e.aid.long=e.aid.long[order(e.aid.long$r2),]
e.aid.long$variable=as.character(e.aid.long$variable)
e.aid.long$variable=factor(e.aid.long$variable,levels=unique(e.aid.long$variable))
e.aid.long$School.attendance=e.aid.long$School.attendance/100

ggplot(e.aid.long,aes(x=value,y=School.attendance))+
  facet_wrap(~variable)+
  geom_point(aes(color=factor(need.vs.resources)))+
  geom_smooth(method="lm")+
  geom_text(x=150,y=.7,aes(label=paste("R^2: ",r2)))+
  scale_y_continuous(labels=scales::percent)+
  scale_x_continuous(labels=scales::dollar)
ggsave("graphics/different_aid_education.png")


#MPI
aid.long=subset(dat.long, variable %in% aid.vars)
aid.long$value=as.numeric(aid.long$value)
aid.long=data.table(aid.long)[
  ,r2:=r2(.SD$value,.SD$MPI.of.the.region)
  ,by=.(variable)
  ]
aid.long=data.table(aid.long)[
  ,value.outlier:=outlier(.SD$value)
  ,by=.(variable)
  ]
aid.long$outlier=outlier(aid.long$MPI.of.the.region)
aid.long=aid.long[which(!aid.long$value.outlier | !aid.long$outlier),]
aid.long=aid.long[order(aid.long$r2),]
aid.long$variable=as.character(aid.long$variable)
aid.long$variable=factor(aid.long$variable,levels=unique(aid.long$variable))


ggplot(aid.long,aes(x=value,y=MPI.of.the.region))+
  facet_wrap(~variable)+
  geom_point(aes(color=factor(need.vs.resources)))+
  geom_smooth(method="lm")+
  geom_text(x=150,y=.3,aes(label=paste("R^2: ",r2)))+
  theme_classic()+
  scale_x_continuous(labels=scales::dollar)
ggsave("graphics/different_aid_MPI.png")

##End facet wraps



ggplot(dat, aes(x=Health.spending..per.capita.US..,y=Health.ODA..off.budGov.Edu.Tot.disbursements..per.capita.US..,label=region))+
  labs(title="Government health spending and ODA",x="Government health spending per capita",y="Off budGov.Edu.Tot health ODA per capita")+
  geom_point(color="grey")+
  geom_text(check_overlap=T,  size=2.5)+
  geom_text(x=20,y=15,label=paste("R^2: ",r2(dat$Health.spending..per.capita.US..,dat$Health.ODA..off.budget.disbursements..per.capita.US..)), parse=T)+
  theme_classic()+
  theme(legend.title=element_blank())+
  scale_y_continuous(labels=scales::dollar)+
  scale_x_continuous(labels=scales::dollar)

ggplot(dat, aes(x=Gov.Health.Tot.PC,y=OFF.H.PC,label=region))+
  labs(title="Government health spending and ODA MPI pop",x="Government health spending per capita",y="Off budGov.Edu.Tot health ODA per capita")+
  geom_point(color="grey")+
  geom_text(check_overlap=T,  size=2.5)+
  geom_text(x=20,y=15,label=paste("R^2: ",r2(dat$Gov.Health.Tot,dat$OFF.H.PC)), parse=T)+
  theme_classic()+
  theme(legend.title=element_blank())+
  scale_y_continuous(labels=scales::dollar)+
  scale_x_continuous(labels=scales::dollar)


ggsave("graphics/health_spend_corr_ODA.png")
ggplot(dat, aes(x=Health.spending..per.capita.US..,y=(`Child mortality`/100),label=region))+
  geom_text(x=17,y=.35,label=paste("R^2: ",r2(dat$Health.spending..per.capita.US..,dat$`Child mortality`)), parse=T)+
  labs(title="Government health spending and child mortality",x="Government health spending per capita",y="Households with recent child mortality")+
  geom_point(aes(color=factor(high.child.mort)))+
  geom_text(check_overlap=T,  size=2.5)+
  theme_classic()+
  theme(legend.title=element_blank())+
  scale_y_continuous(labels=scales::percent)+
  scale_x_continuous(labels=scales::dollar)
ggsave("graphics/gov_health_child_mortality.png")
ggplot(dat, aes(x=Health.ODA..off.budGov.Edu.Tot.disbursements..per.capita.US..,y=(`Child mortality`/100),label=region))+
  geom_text(x=12,y=.35,label=paste("R^2: ",r2(dat$Health.ODA..off.budget.disbursements..per.capita.US..,dat$`Child mortality`)), parse=T)+
  labs(title="Health ODA and child mortality",x="Health ODA per capita",y="Households with recent child mortality")+
  geom_point(aes(color=factor(high.child.mort)))+
  geom_text(check_overlap=T,  size=2.5)+
  theme_classic()+
  theme(legend.title=element_blank())+
  scale_y_continuous(labels=scales::percent)+
  scale_x_continuous(labels=scales::dollar)
ggsave("graphics/gov_health_child_mortality.png")
ggplot(dat, aes(x=total.health.spend,y=(`Child mortality`/100),label=region))+
  geom_text(x=20,y=.35,label=paste("R^2: ",r2(dat$total.health.spend,dat$`Child mortality`)), parse=T)+
  labs(title="Total health spending and child mortality",x="Total health spending per capita",y="Households with recent child mortality")+
  geom_point(aes(color=factor(high.child.mort)))+
  geom_text(check_overlap=T,  size=2.5)+
  theme_classic()+
  theme(legend.title=element_blank())+
  scale_y_continuous(labels=scales::percent)+
  scale_x_continuous(labels=scales::dollar)
ggsave("graphics/total_health_child_mortality.png")


ggplot(dat, aes(x=General.education.spending..per.capita.US..,y=Education.ODA..off.budGov.Edu.Tot.disbursements..per.capita.US..,label=region))+
  labs(title="Government education spending and ODA",x="Government education spending per capita",y="Off budGov.Edu.Tot education ODA per capita")+
  geom_point(color="grey")+
  geom_text(check_overlap=T,  size=2.5)+
  geom_text(x=120,y=7.5,label=paste("R^2: ",r2(dat$General.education.spending..per.capita.US..,dat$Education.ODA..off.budGov.Edu.Tot.disbursements..per.capita.US..)), parse=T)+
  theme_classic()+
  theme(legend.title=element_blank())+
  scale_y_continuous(labels=scales::dollar)+
  scale_x_continuous(labels=scales::dollar)
ggsave("graphics/educ_spend_corr_ODA.png")
ggplot(dat, aes(x=General.education.spending..per.capita.US..,y=(`School attendance`/100),label=region))+
  geom_text(x=120,y=.75,label=paste("R^2: ",r2(dat$General.education.spending..per.capita.US..,dat$`School attendance`)), parse=T)+
  labs(title="Government education budGov.Edu.Tot & school absentee",x="Total education spending per capita",y="School-aged children not in school")+
  geom_point(aes(color=factor(low.school.attend)))+
  geom_text(check_overlap=T,  size=2.5)+
  theme_classic()+
  theme(legend.title=element_blank())+
  scale_y_continuous(labels=scales::percent)+
  scale_x_continuous(labels=scales::dollar)
ggsave("graphics/gov_educ_spend_attend.png")
ggplot(dat, aes(x=Education.ODA..off.budGov.Edu.Tot.disbursements..per.capita.US..,y=(`School attendance`/100),label=region))+
  geom_point(aes(color=factor(low.school.attend)))+
  geom_text(check_overlap=T,  size=2.5)+
  geom_text(x=7,y=.75,label=paste("R^2: ",r2(dat$Education.ODA..off.budGov.Edu.Tot.disbursements..per.capita.US..,dat$`School attendance`)), parse=T)+
  labs(title="Education ODA and school absentees",x="Education ODA per capita",y="School-aged children not in school")+
  theme_classic()+
  theme(legend.title=element_blank())+
  scale_y_continuous(labels=scales::percent)+
  scale_x_continuous(labels=scales::dollar)
ggsave("graphics/educ_oda_attend.png")

ggplot(dat, aes(x=total.educ.spend,y=(`School attendance`/100),label=region))+
  geom_point(aes(color=factor(low.school.attend)))+
  geom_text(check_overlap=T,  size=2.5)+
  geom_text(x=120,y=.75,label=paste("R^2: ",r2(dat$total.educ.spend,dat$`School attendance`)), parse=T)+
  labs(title="Total education spending and school absentees",x="Total education spending per capita",y="School-aged children not in school")+
  theme_classic()+
  theme(legend.title=element_blank())+
  scale_y_continuous(labels=scales::percent)+
  scale_x_continuous(labels=scales::dollar)
ggsave("graphics/educ_spend_attend.png")
ggplot(dat, aes(x=total.health.and.educ.spend,y=`MPI of the region`,label=region))+
  geom_point(aes(color=high.MPI))+
  geom_text(check_overlap=T,  size=2.5)+
  geom_text(x=150,y=.5,label=paste("R^2: ",r2(dat$total.health.and.educ.spend,dat$`MPI of the region`)), parse=T)+
  labs(title="Total health and education spending and MPI",x="Total health and education spending per capita",y="Multidimensional poverty index")+
  theme_classic()+
  theme(legend.title=element_blank())+
  scale_x_continuous(labels=scales::dollar)
ggsave("graphics/health_educ_MPI.png")