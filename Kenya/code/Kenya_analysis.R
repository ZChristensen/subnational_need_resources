list.of.packages <- c("data.table","readr","reshape2","ggplot2","readxl","openxlsx")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only=T)

if(Sys.info()[["user"]]=="Alex"){
  prefix = "C:"
}else{
  prefix = "E:"
}

wd = paste0(prefix,"/git/subnational_need_resources/Kenya")
setwd(wd)

# z.score=function(x){(x-mean(x))/sd(x)}
r2=function(x,y){round(summary(lm(x ~ y))$r.squared,4)}
# outlier = function(x){
#   qnt = quantile(x, probs=c(0.25, 0.75), na.rm=T)
#   H = 1.5 * IQR(x, na.rm=T)
#   return(
#     (x < (qnt[1] - H)) | (x > (qnt[2] + H))
#   )
# }

# MPIsubnational=read_xlsx("E:/git/MPI_recalc/MPI_downloads/Table-5-Subnational-MPI-2018-1.xlsx", sheet="5.5 Raw Headcounts Region",range=cell_rows(9:1138))
# MPIsubnational=MPIsubnational[,c(
#   "ISO Num code"
#   ,"ISO Code"
#   ,"Country"
#   ,"World County"
#   ,"Survey Name"
#   ,"Survey year"
#   ,"Subnational \r\nCounty"
#   ,"MPI of the country"
#   ,"MPI of the County"
#   ,"Nutrition"
#   ,"Child mortality"
#   ,"Years of schooling"
#   ,"School attendance"
#   ,"Cooking \r\nfuel"
#   ,"Sanitation"
#   ,"Drinking water"
#   ,"Electricity"
#   ,"Housing"
#   ,"Assets"
#   ,"Pop survey year"
#   ,"Population 2015"
#   ,"Population 2016"
#   ,"Population share by County"
#   ,"Population size by \r\nCounty"
#   ,"Total number of indicators included \r\n(out of ten)"
#   ,"Indicator (s) missing"
# )]
# MPIsubnational=MPIsubnational[which(MPIsubnational$Country=="Kenya"),]
# names(MPIsubnational)=make.names(names(MPIsubnational))
# MPIsubnational$County=MPIsubnational$Subnational...Region
# MPIpop=unique(MPIsubnational$Population.2016)
# MPIsubnational2=read.csv("input/Kenya_MPI_county.csv")
# MPIsubnational2$Population.2016=NA
# MPIsubnational2$Population.2016=MPIpop
# MPIsubnational2$Population.size.by...County=MPIsubnational2$share.of.pop*MPIsubnational2$Population.2016
# fwrite(MPIsubnational2,"input/Kenya_MPI_county.csv")
MPIsubnational=read.csv("input/Kenya_MPI_county.csv")

funding=read.csv("E:/git/subnational_need_resources/Kenya/input/Kenya_Health_budget_per_capita_by_county.csv")
setnames(MPIsubnational,"shregion","County")
MPIsubnational$County=toupper(MPIsubnational$County)
funding$County=toupper(funding$County)
funding$County=as.character(funding$County)
funding$County[which(funding$County=="ELGEYO MARAKWET")]="ELGEYO MARAK"
funding$County[which(funding$County=="HOMABAY")]="HOMA BAY"
funding$County[which(funding$County=="THARAKA NITHI")]="THARAKA"
funding$County[which(funding$County=="TRANS NZOIA")]="TRANS-NZOIA"
dat=merge(funding,MPIsubnational,by=c("County"))

# Province.Names=read.csv("funding/AFG_province_names.csv")
# AD_pop=read.csv("funding/AidData_AFG_pop.csv")
# #AidData numbers taken from the GPWv4 UN Adjusted 2015 numbers which are taken from CIESIN
# AD_pop=AD_pop[,c("gpw_v4_count.2015.sum","Name")]
# names(AD_pop)=c("AidData.2015.pop","aidData.list")
# AD_pop=merge(AD_pop,Province.Names,by=c("aidData.list"))
# AD_pop=AD_pop[c("AidData.2015.pop","MPI.list")]
# names(AD_pop)=c("AidData.2015.pop","County")
# dat=merge(dat,AD_pop,by=c("County"))


# dat$MPI.Z2=z.score(dat$`MPI of the County`)
# dat$Child.mort.Z=z.score(dat$`Child mortality`)
# dat$health.pc.Z=z.score(dat$Health.spending..per.capita.US..)
# dat$health.ODA.Z=z.score(dat$Health.ODA..off.budget.disbursements..per.capita.US..)
dat$MPI.pop.2016=dat$Population.size.by...County*1000
dat$Ken.Gov.pop.2017.2018=dat$`Population.2017/18`

pop.long=melt(dat[,c("County","MPI.pop.2016","Ken.Gov.pop.2017.2018")],id.vars=c("County")) 
ggplot(pop.long, aes(x=County,y=(value/1000000)))+
  geom_point(aes(color=factor(variable)))+
  labs(title="Kenya population estimates",x="",y="Population (millions)")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  theme(legend.title=element_blank())
ggsave("graphics/population_estimates.png")



# dat$low.health.spend="other countries"
# dat$low.health.spend[which(dat$total.health.spend<(mean(dat$total.health.spend)-sd(dat$total.health.spend)))]="high spend"
# #No Countys are below one sd from the mean total health spending
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
# dat$high.MPI[which(dat$`MPI of the County`>(mean(dat$`MPI of the County`)+sd(dat$`MPI of the County`)))]="high MPI"

dat$health.per.cap.ave=(dat$Health..Budget.per.capita..US...2015.17+dat$Health..Expenditure.per.capita..US...2017.19)/3

dat$need.vs.resources=NA
dat$need.vs.resources[which(dat$health.per.cap.ave<=median(dat$health.per.cap.ave)&dat$MPI<=median(dat$MPI))]="low poverty low resources"
dat$need.vs.resources[which(dat$health.per.cap.ave>median(dat$health.per.cap.ave)&dat$MPI<=median(dat$MPI))]="low poverty high resources"
dat$need.vs.resources[which(dat$health.per.cap.ave>median(dat$health.per.cap.ave)&dat$MPI>median(dat$MPI))]="high poverty high resources"
dat$need.vs.resources[which(dat$health.per.cap.ave<=median(dat$health.per.cap.ave)&dat$MPI>median(dat$MPI))]="high poverty low resources"



ggplot(dat, aes(y=health.per.cap.ave,x=MPI,label=County))+
  geom_point(color="grey")+
  geom_text(check_overlap=T,  size=2.5)+
  geom_text(y=.9*(max(dat$health.per.cap.ave)),x=.9*(max(dat$MPI)),label=paste("R^2: ",r2(dat$health.per.cap.ave,dat$MPI)), parse=T)+
  labs(title="Average government health spending and MPI",y="Average annual health spending per capita",x="Multidimensional poverty index")+
  theme_classic()+
  theme(legend.title=element_blank())+
  scale_y_continuous(labels=scales::dollar)
ggsave("graphics/health_MPI.png")

ggplot(dat, aes(y=health.per.cap.ave,x=Child.mortality,label=County))+
  geom_point(color="grey")+
  geom_text(check_overlap=T,  size=2.5)+
  geom_text(y=.9*(max(dat$health.per.cap.ave)),x=.9*(max(dat$Child.mortality)),label=paste("R^2: ",r2(dat$health.per.cap.ave,dat$Child.mortality)), parse=T)+
  labs(title="Average government health spending and child mortality",y="Average annual health spending per capita",x="Households having experienced a child mortality in last 5 years")+
  theme_classic()+
  theme(legend.title=element_blank())+
  scale_y_continuous(labels=scales::dollar)+
  scale_x_continuous(labels=scales::percent)
ggsave("graphics/health_Child_mortality.png")

ggplot(dat, aes(y=health.per.cap.ave,x=Nutrition,label=County))+
  geom_point(color="grey")+
  geom_text(check_overlap=T,  size=2.5)+
  geom_text(y=.9*(max(dat$health.per.cap.ave)),x=.9*(max(dat$Nutrition)),label=paste("R^2: ",r2(dat$health.per.cap.ave,dat$Nutrition)), parse=T)+
  labs(title="Average government health spending and malnutrition",y="Average annual health spending per capita",x="Households with person experiencing malnutrition")+
  theme_classic()+
  theme(legend.title=element_blank())+
  scale_y_continuous(labels=scales::dollar)+
  scale_x_continuous(labels=scales::percent)
ggsave("graphics/health_nutrition.png")


ggplot(dat, aes(y=health.per.cap.ave,x=np20,label=County))+
  geom_point(color="grey")+
  geom_text(check_overlap=T,  size=2.5)+
  geom_text(y=.9*(max(dat$health.per.cap.ave)),x=.75,label=paste("R^2: ",r2(dat$health.per.cap.ave,dat$np20)), parse=T)+
  labs(title="Average government health spending and share of households in national P20",y="Average annual health spending per capita",x="Households in the national P20 (bottom wealth quintile)")+
  theme_classic()+
  theme(legend.title=element_blank())+
  scale_y_continuous(labels=scales::dollar)+
  scale_x_continuous(labels=scales::percent)
ggsave("graphics/health_np20.png")


dat.long=melt(dat,id.vars=c("County","need.vs.resources","Child mortality","School attendance","MPI of the County","Nutrition"))





# dat$on.need.vs.resources=NA
# dat$on.need.vs.resources[which(dat$ON.OFF.G.Edu.and.HealthPC<=median(dat$ON.OFF.G.Edu.and.HealthPC)&dat$`MPI of the County`<=median(dat$`MPI of the County`))]="low poverty low resources"
# dat$on.need.vs.resources[which(dat$ON.OFF.G.Edu.and.HealthPC>median(dat$ON.OFF.G.Edu.and.HealthPC)&dat$`MPI of the County`<=median(dat$`MPI of the County`))]="low poverty high resources"
# dat$on.need.vs.resources[which(dat$ON.OFF.G.Edu.and.HealthPC>median(dat$ON.OFF.G.Edu.and.HealthPC)&dat$`MPI of the County`>median(dat$`MPI of the County`))]="high poverty high resources"
# dat$on.need.vs.resources[which(dat$ON.OFF.G.Edu.and.HealthPC<=median(dat$ON.OFF.G.Edu.and.HealthPC)&dat$`MPI of the County`>median(dat$`MPI of the County`))]="high poverty low resources"
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
  ,r2:=r2(.SD$value,.SD$MPI.of.the.County)
  ,by=.(variable)
  ]
aid.long=data.table(aid.long)[
  ,value.outlier:=outlier(.SD$value)
  ,by=.(variable)
  ]
aid.long$outlier=outlier(aid.long$MPI.of.the.County)
aid.long=aid.long[which(!aid.long$value.outlier | !aid.long$outlier),]
aid.long=aid.long[order(aid.long$r2),]
aid.long$variable=as.character(aid.long$variable)
aid.long$variable=factor(aid.long$variable,levels=unique(aid.long$variable))


ggplot(aid.long,aes(x=value,y=MPI.of.the.County))+
  facet_wrap(~variable)+
  geom_point(aes(color=factor(need.vs.resources)))+
  geom_smooth(method="lm")+
  geom_text(x=150,y=.3,aes(label=paste("R^2: ",r2)))+
  theme_classic()+
  scale_x_continuous(labels=scales::dollar)
ggsave("graphics/different_aid_MPI.png")

##End facet wraps



ggplot(dat, aes(x=Health.spending..per.capita.US..,y=Health.ODA..off.budGov.Edu.Tot.disbursements..per.capita.US..,label=County))+
  labs(title="Government health spending and ODA",x="Government health spending per capita",y="Off budGov.Edu.Tot health ODA per capita")+
  geom_point(color="grey")+
  geom_text(check_overlap=T,  size=2.5)+
  geom_text(x=20,y=15,label=paste("R^2: ",r2(dat$Health.spending..per.capita.US..,dat$Health.ODA..off.budget.disbursements..per.capita.US..)), parse=T)+
  theme_classic()+
  theme(legend.title=element_blank())+
  scale_y_continuous(labels=scales::dollar)+
  scale_x_continuous(labels=scales::dollar)

ggplot(dat, aes(x=Gov.Health.Tot.PC,y=OFF.H.PC,label=County))+
  labs(title="Government health spending and ODA MPI pop",x="Government health spending per capita",y="Off budGov.Edu.Tot health ODA per capita")+
  geom_point(color="grey")+
  geom_text(check_overlap=T,  size=2.5)+
  geom_text(x=20,y=15,label=paste("R^2: ",r2(dat$Gov.Health.Tot,dat$OFF.H.PC)), parse=T)+
  theme_classic()+
  theme(legend.title=element_blank())+
  scale_y_continuous(labels=scales::dollar)+
  scale_x_continuous(labels=scales::dollar)


ggsave("graphics/health_spend_corr_ODA.png")
ggplot(dat, aes(x=Health.spending..per.capita.US..,y=(`Child mortality`/100),label=County))+
  geom_text(x=17,y=.35,label=paste("R^2: ",r2(dat$Health.spending..per.capita.US..,dat$`Child mortality`)), parse=T)+
  labs(title="Government health spending and child mortality",x="Government health spending per capita",y="Households with recent child mortality")+
  geom_point(aes(color=factor(high.child.mort)))+
  geom_text(check_overlap=T,  size=2.5)+
  theme_classic()+
  theme(legend.title=element_blank())+
  scale_y_continuous(labels=scales::percent)+
  scale_x_continuous(labels=scales::dollar)
ggsave("graphics/gov_health_child_mortality.png")
ggplot(dat, aes(x=Health.ODA..off.budGov.Edu.Tot.disbursements..per.capita.US..,y=(`Child mortality`/100),label=County))+
  geom_text(x=12,y=.35,label=paste("R^2: ",r2(dat$Health.ODA..off.budget.disbursements..per.capita.US..,dat$`Child mortality`)), parse=T)+
  labs(title="Health ODA and child mortality",x="Health ODA per capita",y="Households with recent child mortality")+
  geom_point(aes(color=factor(high.child.mort)))+
  geom_text(check_overlap=T,  size=2.5)+
  theme_classic()+
  theme(legend.title=element_blank())+
  scale_y_continuous(labels=scales::percent)+
  scale_x_continuous(labels=scales::dollar)
ggsave("graphics/gov_health_child_mortality.png")
ggplot(dat, aes(x=total.health.spend,y=(`Child mortality`/100),label=County))+
  geom_text(x=20,y=.35,label=paste("R^2: ",r2(dat$total.health.spend,dat$`Child mortality`)), parse=T)+
  labs(title="Total health spending and child mortality",x="Total health spending per capita",y="Households with recent child mortality")+
  geom_point(aes(color=factor(high.child.mort)))+
  geom_text(check_overlap=T,  size=2.5)+
  theme_classic()+
  theme(legend.title=element_blank())+
  scale_y_continuous(labels=scales::percent)+
  scale_x_continuous(labels=scales::dollar)
ggsave("graphics/total_health_child_mortality.png")


ggplot(dat, aes(x=General.education.spending..per.capita.US..,y=Education.ODA..off.budGov.Edu.Tot.disbursements..per.capita.US..,label=County))+
  labs(title="Government education spending and ODA",x="Government education spending per capita",y="Off budGov.Edu.Tot education ODA per capita")+
  geom_point(color="grey")+
  geom_text(check_overlap=T,  size=2.5)+
  geom_text(x=120,y=7.5,label=paste("R^2: ",r2(dat$General.education.spending..per.capita.US..,dat$Education.ODA..off.budGov.Edu.Tot.disbursements..per.capita.US..)), parse=T)+
  theme_classic()+
  theme(legend.title=element_blank())+
  scale_y_continuous(labels=scales::dollar)+
  scale_x_continuous(labels=scales::dollar)
ggsave("graphics/educ_spend_corr_ODA.png")
ggplot(dat, aes(x=General.education.spending..per.capita.US..,y=(`School attendance`/100),label=County))+
  geom_text(x=120,y=.75,label=paste("R^2: ",r2(dat$General.education.spending..per.capita.US..,dat$`School attendance`)), parse=T)+
  labs(title="Government education budGov.Edu.Tot & school absentee",x="Total education spending per capita",y="School-aged children not in school")+
  geom_point(aes(color=factor(low.school.attend)))+
  geom_text(check_overlap=T,  size=2.5)+
  theme_classic()+
  theme(legend.title=element_blank())+
  scale_y_continuous(labels=scales::percent)+
  scale_x_continuous(labels=scales::dollar)
ggsave("graphics/gov_educ_spend_attend.png")
ggplot(dat, aes(x=Education.ODA..off.budGov.Edu.Tot.disbursements..per.capita.US..,y=(`School attendance`/100),label=County))+
  geom_point(aes(color=factor(low.school.attend)))+
  geom_text(check_overlap=T,  size=2.5)+
  geom_text(x=7,y=.75,label=paste("R^2: ",r2(dat$Education.ODA..off.budGov.Edu.Tot.disbursements..per.capita.US..,dat$`School attendance`)), parse=T)+
  labs(title="Education ODA and school absentees",x="Education ODA per capita",y="School-aged children not in school")+
  theme_classic()+
  theme(legend.title=element_blank())+
  scale_y_continuous(labels=scales::percent)+
  scale_x_continuous(labels=scales::dollar)
ggsave("graphics/educ_oda_attend.png")

ggplot(dat, aes(x=total.educ.spend,y=(`School attendance`/100),label=County))+
  geom_point(aes(color=factor(low.school.attend)))+
  geom_text(check_overlap=T,  size=2.5)+
  geom_text(x=120,y=.75,label=paste("R^2: ",r2(dat$total.educ.spend,dat$`School attendance`)), parse=T)+
  labs(title="Total education spending and school absentees",x="Total education spending per capita",y="School-aged children not in school")+
  theme_classic()+
  theme(legend.title=element_blank())+
  scale_y_continuous(labels=scales::percent)+
  scale_x_continuous(labels=scales::dollar)
ggsave("graphics/educ_spend_attend.png")
ggplot(dat, aes(x=total.health.and.educ.spend,y=MPI,label=County))+
  geom_point(aes(color=high.MPI))+
  geom_text(check_overlap=T,  size=2.5)+
  geom_text(x=150,y=.5,label=paste("R^2: ",r2(dat$total.health.and.educ.spend,dat$MPI)), parse=T)+
  labs(title="Total health and education spending and MPI",x="Total health and education spending per capita",y="Multidimensional poverty index")+
  theme_classic()+
  theme(legend.title=element_blank())+
  scale_x_continuous(labels=scales::dollar)
ggsave("graphics/health_educ_MPI.png")

