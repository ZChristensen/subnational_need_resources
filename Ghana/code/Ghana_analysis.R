list.of.packages <- c("data.table","readr","reshape2","ggplot2","readxl","openxlsx")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only=T)



wd = "E:/git/subnational_need_resources/Ghana"
setwd(wd)


r2=function(x,y){round(summary(lm(x ~ y))$r.squared,4)}


MPIsubnational=read_xlsx("E:/git/MPI_recalc/MPI_downloads/Table-5-Subnational-MPI-2018-1.xlsx", sheet="5.5 Raw Headcounts Region",range=cell_rows(9:1138))
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
MPIsubnational=MPIsubnational[which(MPIsubnational$Country=="Ghana"),]
names(MPIsubnational)=make.names(names(MPIsubnational))
MPIsubnational$County=MPIsubnational$Subnational...region
MPIsubnational$School.attendance=MPIsubnational$School.attendance/100
MPIsubnational$Child.mortality=MPIsubnational$Child.mortality/100
MPIsubnational$Years.of.schooling=MPIsubnational$Years.of.schooling/100

funding=read_csv("input/Ghana_ODA2015-17.csv")
funding=funding[3:12,]

govspend=read_excel("input/Ghana Gov regional spending on health and education.xlsx", sheet="Health_Educ_spending per capita" )
govspend=govspend[1:10,]
govspend=govspend[,c("Region"                                              
                     ,"Govt own Health spending (2015 budget, US$)"         
                     ,"Govt own education spending (2015 budget, US$)"      
                     ,"Population"                                          
                     ,"Enrolment"                                           
                     ,"Govt own Health spending (2015 budget, US$/capita)"  
                     ,"Govt own education spending (2015 budget, US$/child)")]
names(govspend)=make.names(names(govspend))
setnames(MPIsubnational,"Subnational...region","region")
setnames(govspend,"Region","region")
funding=merge(govspend,funding,by=c("region"))
dat=merge(funding,MPIsubnational,by=c("region"))
dat$MPI.pop.2016=dat$Population.size.by...region*1000


dat$health.per.cap.ave=dat$Govt.own.Health.spending..2015.budget..US..capita.
dat$educ.per.cap.ave=dat$Govt.own.education.spending..2015.budget..US..child.
dat$educ.and.health.per.cap.ave=dat$health.per.cap.ave+dat$educ.per.cap.ave

dat$educ.and.health.oda.per.cap.ave=dat$AVERAGE/dat$MPI.pop.2016
  
poverty=read.csv("input/Ghana_MPI_district.csv")
dat$region=toupper(dat$region)
poverty$region=toupper(poverty$region)
poverty=poverty[,c("region","p20","np20","ext")]
dat=merge(dat,poverty,by=c("region"))

ggplot(dat, aes(y=educ.and.health.per.cap.ave,x=MPI.of.the.region,label=County))+
  geom_point(color="grey")+
  geom_text(check_overlap=T,  size=2.5)+
  geom_text(y=.9*(max(dat$educ.and.health.per.cap.ave)),x=.9*(max(dat$MPI.of.the.region)),label=paste("R^2: ",r2(dat$educ.and.health.per.cap.ave,dat$MPI.of.the.region)), parse=T)+
  labs(title="Health and education government spending and MPI",y="Government health and education spending\nper capita",x="Multidimensional poverty index")+
  theme_classic()+
  theme(legend.title=element_blank())+
  scale_y_continuous(labels=scales::dollar)
ggsave("graphics/health_edu_MPI.png")

ggplot(dat, aes(y=dat$educ.and.health.oda.per.cap.ave,x=Child.mortality,label=County))+
  geom_point(color="grey")+
  geom_text(check_overlap=T,  size=2.5)+
  geom_text(y=.9*(max(dat$educ.and.health.oda.per.cap.ave)),x=.9*(max(dat$Child.mortality)),label=paste("R^2: ",r2(dat$educ.and.health.oda.per.cap.ave,dat$Child.mortality)), parse=T)+
  labs(title="Average social sector ODA spending and child mortality",y="Average annual social sector ODA per capita",x="Households having experienced a child mortality in last 5 years")+
  theme_classic()+
  theme(legend.title=element_blank())+
  scale_y_continuous(labels=scales::dollar)+
  scale_x_continuous(labels=scales::percent)
ggsave("graphics/social_sector_ODA_Child_mortality.png")




ggplot(dat, aes(y=health.per.cap.ave,x=Child.mortality,label=County))+
  geom_point(color="grey")+
  geom_text(check_overlap=T,  size=2.5)+
  geom_text(y=.9*(max(dat$health.per.cap.ave)),x=.9*(max(dat$Child.mortality)),label=paste("R^2: ",r2(dat$health.per.cap.ave,dat$Child.mortality)), parse=T)+
  labs(title="Government health spending and child mortality",y="Health government spending\nper capita",x="Households having experienced a child mortality in last 5 years")+
  theme_classic()+
  theme(legend.title=element_blank())+
  scale_y_continuous(labels=scales::dollar)+
  scale_x_continuous(labels=scales::percent)
ggsave("graphics/health_gov_Child_mortality.png")




ggplot(dat, aes(y=educ.per.cap.ave,x=School.attendance,label=County))+
  geom_point(color="grey")+
  geom_text(check_overlap=T,  size=2.5)+
  geom_text(y=.9*(max(dat$educ.per.cap.ave)),x=.9*(max(dat$School.attendance)),label=paste("R^2: ",r2(dat$educ.per.cap.ave,dat$School.attendance)), parse=T)+
  labs(title="Government education spending and school attendance",y="Average annual government spending\nper capita",x="Households with a child not attending school")+
  theme_classic()+
  theme(legend.title=element_blank())+
  scale_y_continuous(labels=scales::dollar)+
  scale_x_continuous(labels=scales::percent)
ggsave("graphics/education_gov_absenteeism.png")

ggplot(dat, aes(y=educ.and.health.oda.per.cap.ave,x=School.attendance,label=County))+
  geom_point(color="grey")+
  geom_text(check_overlap=T,  size=2.5)+
  geom_text(y=.9*(max(dat$educ.and.health.oda.per.cap.ave)),x=.9*(max(dat$School.attendance)),label=paste("R^2: ",r2(dat$educ.and.health.oda.per.cap.ave,dat$School.attendance)), parse=T)+
  labs(title="Social sector ODA and school attendance",y="Average annual social sector ODA spending\nper capita",x="Households with a child not attending school")+
  theme_classic()+
  theme(legend.title=element_blank())+
  scale_y_continuous(labels=scales::dollar)+
  scale_x_continuous(labels=scales::percent)
ggsave("graphics/social_sector_ODA_absenteeism.png")

ggplot(dat, aes(y=educ.and.health.oda.per.cap.ave,x=MPI.of.the.region,label=County))+
  geom_point(color="grey")+
  geom_text(check_overlap=T,  size=2.5)+
  geom_text(y=.9*(max(dat$educ.and.health.oda.per.cap.ave)),x=.9*(max(dat$MPI.of.the.region)),label=paste("R^2: ",r2(dat$educ.and.health.oda.per.cap.ave,dat$MPI.of.the.region)), parse=T)+
  labs(title="Social sector ODA and MPI",y="Social sector ODA\nper capita",x="Multidimensional poverty index")+
  theme_classic()+
  theme(legend.title=element_blank())+
  scale_y_continuous(labels=scales::dollar)
ggsave("graphics/social_sector_ODA_MPI.png")

ggplot(dat, aes(y=educ.and.health.oda.per.cap.ave,x=np20,label=County))+
  geom_point(color="grey")+
  geom_text(check_overlap=T,  size=2.5)+
  geom_text(y=.9*(max(dat$educ.and.health.oda.per.cap.ave)),x=.9*(max(dat$np20)),label=paste("R^2: ",r2(dat$educ.and.health.oda.per.cap.ave,dat$np20)), parse=T)+
  labs(title="Social sector ODA and the national P20",y="Social sector ODA\nper capita",x="Percentage of population in national P20 (bottom wealth quintile)")+
  theme_classic()+
  theme(legend.title=element_blank())+
  scale_x_continuous(labels=scales::percent)+
  scale_y_continuous(labels=scales::dollar)
ggsave("graphics/social_sector_ODA_np20.png")


ggplot(dat, aes(y=educ.and.health.oda.per.cap.ave,x=p20,label=County))+
  geom_point(color="grey")+
  geom_smooth(method="lm")+
  geom_text(check_overlap=T,  size=2.5)+
  geom_text(y=.9*(max(dat$educ.and.health.oda.per.cap.ave)),x=.9*(max(dat$p20)),label=paste("R^2: ",r2(dat$educ.and.health.oda.per.cap.ave,dat$p20)), parse=T)+
  labs(title="Social sector ODA and the P20",y="Social sector ODA\nper capita",x="Percentage of population in P20 (bottom 20% of global population)")+
  theme_classic()+
  theme(legend.title=element_blank())+
  scale_x_continuous(labels=scales::percent)+
  scale_y_continuous(labels=scales::dollar)
ggsave("graphics/social_sector_ODA_p20.png")

ggplot(dat, aes(y=educ.and.health.per.cap.ave,x=p20,label=County))+
  geom_point(color="grey")+
  geom_smooth(method="lm")+
  geom_text(check_overlap=T,  size=2.5)+
  geom_text(y=.9*(max(dat$educ.and.health.per.cap.ave)),x=.9*(max(dat$p20)),label=paste("R^2: ",r2(dat$educ.and.health.per.cap.ave,dat$p20)), parse=T)+
  labs(title="Government health & education spending and the P20",y="Govt health and education spending\nper capita",x="Percentage of population in P20 (bottom 20% of global population)")+
  theme_classic()+
  theme(legend.title=element_blank())+
  scale_x_continuous(labels=scales::percent)+
  scale_y_continuous(labels=scales::dollar)
ggsave("graphics/health_education_gov_p20.png")

ggplot(dat, aes(y=educ.and.health.oda.per.cap.ave,x=ext,label=County))+
  geom_point(color="grey")+
  geom_text(check_overlap=T,  size=2.5)+
  geom_text(y=.9*(max(dat$educ.and.health.oda.per.cap.ave)),x=.9*(max(dat$ext)),label=paste("R^2: ",r2(dat$educ.and.health.oda.per.cap.ave,dat$ext)), parse=T)+
  labs(title="Social sector ODA and extreme poverty",y="Social sector ODA\nper capita",x="Percentage of population in extreme poverty")+
  theme_classic()+
  theme(legend.title=element_blank())+
  scale_x_continuous(labels=scales::percent)+
  scale_y_continuous(labels=scales::dollar)
ggsave("graphics/social_sector_ODA_ext.png")




dat$need.vs.resources=NA
dat$need.vs.resources[which(dat$educ.and.health.per.cap.ave<=median(dat$educ.and.health.per.cap.ave)&dat$MPI<=median(dat$MPI))]="low poverty low resources"
dat$need.vs.resources[which(dat$educ.and.health.per.cap.ave>median(dat$educ.and.health.per.cap.ave)&dat$MPI<=median(dat$MPI))]="low poverty high resources"
dat$need.vs.resources[which(dat$educ.and.health.per.cap.ave>median(dat$educ.and.health.per.cap.ave)&dat$MPI>median(dat$MPI))]="high poverty high resources"
dat$need.vs.resources[which(dat$educ.and.health.per.cap.ave<=median(dat$educ.and.health.per.cap.ave)&dat$MPI>median(dat$MPI))]="high poverty low resources"

dat2=dat[c("region","educ.per.cap.ave","School.attendance","health.per.cap.ave","Child.mortality","MPI.of.the.region","need.vs.resources","MPI.pop.2016","Country")]
fwrite(dat2,"graphics/output.csv")

