list.of.packages <- c("data.table","readr","reshape2","ggplot2","readxl","openxlsx")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only=T)



wd = "E:/git/subnational_need_resources/Mozambique"
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
MPIsubnational=MPIsubnational[which(MPIsubnational$Country=="Mozambique"),]
names(MPIsubnational)=make.names(names(MPIsubnational))
setnames(MPIsubnational,"Subnational...region","region")

funding=read_excel("input/Mozambique Spending_health per capita and education per student 2018.xlsx",sheet="Educ-Health Spending by region")
names(funding)=make.names(names(funding))
setnames(funding,"Province","region")
funding$region=toupper(funding$region)
MPIsubnational$region=toupper(MPIsubnational$region)

dat=merge(MPIsubnational,funding,by=c("region"))

dat$MPI.pop.2016=dat$Population.size.by...region*1000



dat$health.per.cap.ave=dat$Government.own.revenue.spending.on.health..2018.budget.US../dat$MPI.pop.2016
dat$educ.per.cap.ave=dat$Government.own.revenue.spending.on.education..2018.budget.US../dat$MPI.pop.2016

dat$educ.and.health.per.cap.ave=dat$health.per.cap.ave+dat$educ.per.cap.ave


ggplot(dat, aes(y=educ.and.health.per.cap.ave,x=MPI.of.the.region,label=region))+
  geom_point(color="grey")+
  geom_text(check_overlap=T,  size=2.5)+
  geom_text(y=.9*(max(dat$educ.and.health.per.cap.ave)),x=.9*(max(dat$MPI.of.the.region)),label=paste("R^2: ",r2(dat$educ.and.health.per.cap.ave,dat$MPI.of.the.region)), parse=T)+
  labs(title="Health and education government spending and MPI",y="Government health and education spending\nper capita",x="Multidimensional poverty index")+
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


ggplot(dat, aes(y=educ.per.cap.ave,x=School.attendance,label=region))+
  geom_point(color="grey")+
  geom_text(check_overlap=T,  size=2.5)+
  geom_text(y=.9*(max(dat$educ.per.cap.ave)),x=.9*(max(dat$School.attendance)),label=paste("R^2: ",r2(dat$educ.per.cap.ave,dat$School.attendance)), parse=T)+
  labs(title="Government education spending and school attendance",y="Average annual government spending\nper capita",x="Households with a child not attending school")+
  theme_classic()+
  theme(legend.title=element_blank())+
  scale_y_continuous(labels=scales::dollar)+
  scale_x_continuous(labels=scales::percent)
ggsave("graphics/education_gov_absenteeism.png")



dat$need.vs.resources=NA
dat$need.vs.resources[which(dat$educ.and.health.per.cap.ave<=median(dat$educ.and.health.per.cap.ave)&dat$MPI<=median(dat$MPI))]="low poverty low resources"
dat$need.vs.resources[which(dat$educ.and.health.per.cap.ave>median(dat$educ.and.health.per.cap.ave)&dat$MPI<=median(dat$MPI))]="low poverty high resources"
dat$need.vs.resources[which(dat$educ.and.health.per.cap.ave>median(dat$educ.and.health.per.cap.ave)&dat$MPI>median(dat$MPI))]="high poverty high resources"
dat$need.vs.resources[which(dat$educ.and.health.per.cap.ave<=median(dat$educ.and.health.per.cap.ave)&dat$MPI>median(dat$MPI))]="high poverty low resources"

dat2=dat[c("region","educ.per.cap.ave","School.attendance","health.per.cap.ave","Child.mortality","MPI.of.the.region","need.vs.resources","MPI.pop.2016","Country")]

fwrite(dat2,"graphics/output.csv")
