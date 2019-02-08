list.of.packages <- c("data.table","readr","reshape2","readxl","ggplot2")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only=T)



wd = "C:/Users/Zach/Box/Subnational paper - Q1 2019/Uganda"
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


rawMPI=read.csv("input/UG_mpi_by_district.csv")
rawMPI$sample.weights=rawMPI$weight/100000

rawMPI$MPI.A=((rawMPI$d_cm+rawMPI$d_nutr)*(1/6))+((rawMPI$d_satt+rawMPI$d_educ)*(1/6))+((rawMPI$d_elct+rawMPI$d_ckfl+rawMPI$d_sani+rawMPI$d_hsg+rawMPI$d_asst+rawMPI$d_wtr)*(1/18))
rawMPI$MPI.H=(rawMPI$MPI.A>(1/3))*1




MPIsubnational=data.table(rawMPI)[,.(
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
),by=c("adm1")]
MPIsubnational$MPI.of.the.region=MPIsubnational$MPI.A*MPIsubnational$MPI.H
totalweights=sum(MPIsubnational$weights)
MPIsubnational$MPI.pop.2016=(MPIsubnational$weights/totalweights)*41487960


funding=read.xlsx("input/Ugandan Government spend on education and health spending by district.xlsx",sheet="District level")
funding$SubRegion=funding$Old.District
setnames(MPIsubnational,"adm1","region")
setnames(funding,"SubRegion","region")
funding=funding[,3:9]
funding$region=toupper(funding$region)
funding$region=gsub(" DISTRICT","",funding$region)
funding$region[which(funding$region=="SEMBABULE")]="SSEMBABULE"
dat=merge(funding,MPIsubnational,by=c("region"))
names(dat)=make.names(names(dat))

dat$health.per.cap.ave=dat$Health.budget.2018.19..US../dat$Population
dat$educ.per.cap.ave=dat$Education.budget.2018.19..US../dat$Population
dat$enrollment=dat$Enrolment..pre.primary..primary.and.secondary. 

dat$need.vs.resources=NA
dat$need.vs.resources[which(dat$health.per.cap.ave<=median(dat$health.per.cap.ave)&dat$MPI<=median(dat$MPI))]="low poverty low resources"
dat$need.vs.resources[which(dat$health.per.cap.ave>median(dat$health.per.cap.ave)&dat$MPI<=median(dat$MPI))]="low poverty high resources"
dat$need.vs.resources[which(dat$health.per.cap.ave>median(dat$health.per.cap.ave)&dat$MPI>median(dat$MPI))]="high poverty high resources"
dat$need.vs.resources[which(dat$health.per.cap.ave<=median(dat$health.per.cap.ave)&dat$MPI>median(dat$MPI))]="high poverty low resources"


dat$Country="Uganda"

dat2=dat[c("region","educ.per.cap.ave","School.attendance","health.per.cap.ave","Child.mortality","MPI.of.the.region","need.vs.resources","MPI.pop.2016","Country","enrollment")]
fwrite(dat2,"graphics/output.csv")


ggplot(dat, aes(y=health.per.cap.ave,x=MPI.of.the.region,label=region))+
  geom_point(color="grey")+
  geom_text(check_overlap=T,  size=2.5)+
  geom_text(y=.9*(max(dat$health.per.cap.ave)),x=.9*(max(dat$MPI.of.the.region)),label=paste("R^2: ",r2(dat$health.per.cap.ave,dat$MPI.of.the.region)), parse=T)+
  labs(title="health government spending and MPI",y="Government health spending\nper capita",x="Multidimensional poverty index")+
  theme_classic()+
  theme(legend.title=element_blank())+
  scale_y_continuous(labels=scales::dollar)
ggsave("graphics/health_edu_MPI.png")


ggplot(dat, aes(y=health.per.cap.ave,x=Child.mortality,label=region))+
  geom_point(color="grey")+
  geom_text(check_overlap=T,  size=2.5)+
  geom_text(y=.9*(max(dat$health.per.cap.ave)),x=.9*(max(dat$Child.mortality)),label=paste("R^2: ",r2(dat$health.per.cap.ave,dat$Child.mortality)), parse=T)+
  labs(title="Government health spending and child mortality",y="Health government spending\nper capita",x="Households having experienced a child mortality in last 5 years")+
  theme_classic()+
  theme(legend.title=element_blank())+
  scale_y_continuous(labels=scales::dollar)+
  scale_x_continuous(labels=scales::percent)
ggsave("graphics/health_gov_Child_mortality.png")


# ggplot(dat, aes(y=educ.per.cap.ave,x=School.attendance,label=region))+
#   geom_point(color="grey")+
#   geom_text(check_overlap=T,  size=2.5)+
#   geom_text(y=.9*(max(dat$educ.per.cap.ave)),x=.9*(max(dat$School.attendance)),label=paste("R^2: ",r2(dat$educ.per.cap.ave,dat$School.attendance)), parse=T)+
#   labs(title="Government education spending and school attendance",y="Average annual government spending\nper capita",x="Households with a child not attending school")+
#   theme_classic()+
#   theme(legend.title=element_blank())+
#   scale_y_continuous(labels=scales::dollar)+
#   scale_x_continuous(labels=scales::percent)
# ggsave("graphics/education_gov_absenteeism.png")



ggplot(dat, aes(y=health.per.cap.ave,x=np20,label=region))+
  geom_point(color="grey")+
  geom_text(check_overlap=T,  size=2.5)+
  geom_text(y=.9*(max(dat$health.per.cap.ave)),x=.75,label=paste("R^2: ",r2(dat$health.per.cap.ave,dat$np20)), parse=T)+
  labs(title="Average government health spending and share of households in national P20",y="Average annual health spending per capita",x="Households in the national P20 (bottom wealth quintile)")+
  theme_classic()+
  theme(legend.title=element_blank())+
  scale_y_continuous(labels=scales::dollar)+
  scale_x_continuous(labels=scales::percent)
ggsave("graphics/health_np20.png")

ggplot(dat, aes(y=health.per.cap.ave,x=ntl_pov_hc,label=region))+
  geom_point(color="grey")+
  geom_text(check_overlap=T,  size=2.5)+
  geom_text(y=.9*(max(dat$health.per.cap.ave)),x=.9*(max(dat$ntl_pov_hc)),label=paste("R^2: ",r2(dat$health.per.cap.ave,dat$ntl_pov_hc)), parse=T)+
  labs(title="Average government health spending and\nshare of households in national P20",y="Average annual health spending per capita",x="Percentage of population below national poverty line")+
  theme_classic()+
  theme(legend.title=element_blank())+
  scale_y_continuous(labels=scales::dollar)+
  scale_x_continuous(labels=scales::percent)
ggsave("graphics/health_ntl_pov.png")