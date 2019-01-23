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
# nationalpoverty=read.csv("https://raw.githubusercontent.com/devinit/digital-platform/master/user-data/kenya-poverty-headcount/csv/kenya-poverty-headcount.csv")
# nationalpoverty$County=toupper(nationalpoverty$district_name)
# nationalpoverty$County[which(nationalpoverty$County=="ELGEYO-MARAKWET")]="ELGEYO MARAK"
# nationalpoverty$County[which(nationalpoverty$County=="MURANG'A")]="MURANGA"
# nationalpoverty$County[which(nationalpoverty$County=="NITHI")]="THARAKA"
# fwrite(nationalpoverty,"input/national_pov.csv")
ntl_pov=read.csv("input/national_pov.csv")
ntl_pov$ntl_pov_hc=ntl_pov$value/100
ntl_pov=ntl_pov[,c("County","ntl_pov_hc")]

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
dat=merge(dat,ntl_pov,by=c("County"))
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

ggplot(dat, aes(y=health.per.cap.ave,x=ntl_pov_hc,label=County))+
  geom_point(color="grey")+
  geom_text(check_overlap=T,  size=2.5)+
  geom_text(y=.9*(max(dat$health.per.cap.ave)),x=.9*(max(dat$ntl_pov_hc)),label=paste("R^2: ",r2(dat$health.per.cap.ave,dat$ntl_pov_hc)), parse=T)+
  labs(title="Average government health spending and share of households in national P20",y="Average annual health spending per capita",x="Percentage of population below national poverty line")+
  theme_classic()+
  theme(legend.title=element_blank())+
  scale_y_continuous(labels=scales::dollar)+
  scale_x_continuous(labels=scales::percent)
ggsave("graphics/health_ntl_pov.png")




