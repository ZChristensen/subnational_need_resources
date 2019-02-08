list.of.packages <- c("data.table","reshape2","ggplot2","stargazer")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only=T)



wd = "C:/Users/aiddata_pau/Box/Subnational paper - Q1 2019/Uganda"
setwd(wd)
z.score=function(x){(x-mean(x))/sd(x)}
r2=function(x,y){round(summary(lm(x ~ y))$r.squared,4)}

# The DDH API is located here: http://212.111.41.68:8000/

# Available endpoints are
# `/all_tables` - lists all tables
# `/meta_data` - lists some DI concepts, may be more useful in the future
# `/single_table` - Allows access to a single DDH table
# `/multi_table` - Allows access to multiple DDH tables at once

# Let's start by seeing what's available
all_tables = fread("http://212.111.41.68:8000/all_tables?format=csv")


# The DDH uses "schemas" to organise data. Let's see the available ones
unique(all_tables$table_schema)
# And look at the Spotlight on Uganda specifically
sou_2017 = subset(all_tables,table_schema=="spotlight_on_uganda_2017")


uga= fread("http://212.111.41.68:8000/multi_table?indicators=uganda_primary_pupil_stance_ratio,uganda_health_funding,uganda_poverty_headcount,uganda_anc4_coverage,uganda_health_posts,uganda_dpt3_coverage,uganda_primary_pupil_classroom_ratio,uganda_primary_sit_write,uganda_total_pop,uganda_primary_enrol,uganda_secondary_enrol,uganda_leaving_exam_perf_rate,uganda_overall_health&start_year=2014&end_year=2014&format=csv")
uga=uga[,c("district_id","year","value","name","indicator")]


uga.m=melt(uga,id.vars=c("district_id","indicator","name","year"))
uga.w=dcast(uga.m,district_id+name+year~indicator+variable)
uga.w$region=toupper(uga.w$name)

regnames=read.xlsx("input/Ugandan Government spend on education and health spending by district.xlsx",sheet="District level")
regnames$region=regnames$Old.District
regnames=regnames[,c("region","SubRegion")]

regnames$region=toupper(regnames$region)
regnames$region=gsub(" DISTRICT","",regnames$region)
regnames$region[which(regnames$region=="SEMBABULE")]="SSEMBABULE"

output=fread("graphics/output.csv")
output$region[which(output$region=="SSEMBABULE")]="SEMBABULE"
dat=merge(uga.w,output,by=c("region"))
dat$uganda_health_funding_value[which(is.na(dat$uganda_health_funding_value))]=0


z.scores=data.table(dat)[,.(
  Health.Gov.Z=z.score(uganda_health_funding_value)
  ,Educ.Gov.Z=z.score(educ.per.cap.ave)
  ,MPI.Z=z.score(MPI.of.the.region)
  ,School.Attend.Z=z.score(School.attendance)
  ,Child.Mortality.Z=z.score(Child.mortality)
  ,National.Pov.Z=z.score(uganda_poverty_headcount_value)
  ,Exam.Score.Z=z.score(uganda_leaving_exam_perf_rate_value)
  ,Health.Outcome.Index.z=z.score(uganda_overall_health_value)
  ,Primary.Enroll.z=z.score(uganda_primary_enrol_value)
  ,Secondary.Enroll.z=z.score(uganda_secondary_enrol_value)
  ,Antenatal.Coverage.z=z.score(uganda_anc4_coverage_value)
  ,Tetanus.Vaccine.Coverage.z=z.score(uganda_dpt3_coverage_value)
  ,Pupil.classroom.ratio.z=z.score(uganda_primary_pupil_classroom_ratio_value)
  ,Pupil.sit.write.z=z.score(uganda_primary_sit_write_value)
  ,Pupil.toilet.z=z.score(uganda_primary_pupil_stance_ratio_value)
  ,pop.z=z.score(uganda_total_pop_value)
  ,region=region
),by=c("Country")]
dat=merge(dat,z.scores,by=c("region"))
dat=merge(dat,regnames,by=c("region"))
dat$health_edu.per.cap.ave <- dat$health.per.cap.ave + dat$educ.per.cap.ave #add health and education spend
fwrite(dat,"graphics/spotlight_on_uganda.csv")

#Harsh's code to subset top/bottom 10 districts
#subset poorest and least poor using MPI
#poorest
dat_poorest_MPI <- dat[with(dat,order(-MPI.of.the.region)),] #order by descending
dat_10poorest_MPI <- dat_poorest_MPI[1:10,] #subset to keep the first ten
dat_20poorest_MPI <- dat_poorest_MPI[1:20,] #subset to keep the first twenty
#least poor
dat_leastpoor_MPI <- dat[with(dat,order(MPI.of.the.region)),] #order by ascending
dat_10leastpoor_MPI <- dat_leastpoor_MPI[1:10,]
dat_20leastpoor_MPI <- dat_leastpoor_MPI[1:20,]
#10
top10_health_edu_MPI <- median(dat_10poorest_MPI$health_edu.per.cap.ave)
top10_education_MPI <- median(dat_10poorest_MPI$educ.per.cap.ave)
top10_health_MPI <- median(dat_10poorest_MPI$health.per.cap.ave)
bottom10_health_edu_MPI <- median(dat_10leastpoor_MPI$health_edu.per.cap.ave)
bottom10_education_MPI <- median(dat_10leastpoor_MPI$educ.per.cap.ave)
bottom10_health_MPI <- median(dat_10leastpoor_MPI$health.per.cap.ave)
#20
top20_health_edu_MPI <- median(dat_20poorest_MPI$health_edu.per.cap.ave)
top20_education_MPI <- median(dat_20poorest_MPI$educ.per.cap.ave)
top20_health_MPI <- median(dat_20poorest_MPI$health.per.cap.ave)
bottom20_health_edu_MPI <- median(dat_20leastpoor_MPI$health_edu.per.cap.ave)
bottom20_education_MPI <- median(dat_20leastpoor_MPI$educ.per.cap.ave)
bottom20_health_MPI <- median(dat_20leastpoor_MPI$health.per.cap.ave)

#Harsh's code to subset top/bottom 10 districts
#subset poorest and least poor using national uganda_poverty rates
#poorest
dat_poorest_uganda_poverty_headcount_value <- dat[with(dat,order(-uganda_poverty_headcount_value)),] #order by descending
dat_10poorest_uganda_poverty_headcount_value <- dat_poorest_uganda_poverty_headcount_value[1:10,] #subset to keep the first ten
dat_20poorest_uganda_poverty_headcount_value <- dat_poorest_uganda_poverty_headcount_value[1:20,] #subset to keep the first twenty
#least poor
dat_leastpoor_uganda_poverty_headcount_value <- dat[with(dat,order(uganda_poverty_headcount_value)),] #order by ascending
dat_10leastpoor_uganda_poverty_headcount_value <- dat_leastpoor_uganda_poverty_headcount_value[1:10,]
dat_20leastpoor_uganda_poverty_headcount_value <- dat_leastpoor_uganda_poverty_headcount_value[1:20,]
#10
top10_health_edu_uganda_poverty_headcount_value <- median(dat_10poorest_uganda_poverty_headcount_value$health_edu.per.cap.ave)
top10_education_uganda_poverty_headcount_value <- median(dat_10poorest_uganda_poverty_headcount_value$educ.per.cap.ave)
top10_health_uganda_poverty_headcount_value <- median(dat_10poorest_uganda_poverty_headcount_value$health.per.cap.ave)
bottom10_health_edu_uganda_poverty_headcount_value <- median(dat_10leastpoor_uganda_poverty_headcount_value$health_edu.per.cap.ave)
bottom10_education_uganda_poverty_headcount_value <- median(dat_10leastpoor_uganda_poverty_headcount_value$educ.per.cap.ave)
bottom10_health_uganda_poverty_headcount_value <- median(dat_10leastpoor_uganda_poverty_headcount_value$health.per.cap.ave)
#20
top20_health_edu_uganda_poverty_headcount_value <- median(dat_20poorest_uganda_poverty_headcount_value$health_edu.per.cap.ave)
top20_education_uganda_poverty_headcount_value <- median(dat_20poorest_uganda_poverty_headcount_value$educ.per.cap.ave)
top20_health_uganda_poverty_headcount_value <- median(dat_20poorest_uganda_poverty_headcount_value$health.per.cap.ave)
bottom20_health_edu_uganda_poverty_headcount_value <- median(dat_20leastpoor_uganda_poverty_headcount_value$health_edu.per.cap.ave)
bottom20_education_uganda_poverty_headcount_value <- median(dat_20leastpoor_uganda_poverty_headcount_value$educ.per.cap.ave)
bottom20_health_uganda_poverty_headcount_value <- median(dat_20leastpoor_uganda_poverty_headcount_value$health.per.cap.ave)

#combine all with cbind
medians_uganda <- cbind(top10_health_edu_MPI,top10_education_MPI,top10_health_MPI,bottom10_health_edu_MPI,bottom10_education_MPI,bottom10_health_MPI,
                        top20_health_edu_MPI,top20_education_MPI,top20_health_MPI,bottom20_health_edu_MPI,bottom20_education_MPI,bottom20_health_MPI,
                        top10_health_edu_uganda_poverty_headcount_value,top10_education_uganda_poverty_headcount_value,top10_health_uganda_poverty_headcount_value,bottom10_health_edu_uganda_poverty_headcount_value,bottom10_education_uganda_poverty_headcount_value,bottom10_health_uganda_poverty_headcount_value,
                        top20_health_edu_uganda_poverty_headcount_value,top20_education_uganda_poverty_headcount_value,top20_health_uganda_poverty_headcount_value,bottom20_health_edu_uganda_poverty_headcount_value,bottom20_education_uganda_poverty_headcount_value,bottom20_health_uganda_poverty_headcount_value)
colnames(medians_uganda) <- c("10 Poorest Districts (MPI) - Total Spending","10 Poorest Districts (MPI) - Education Spending","10 Poorest Districts (MPI) - Health Spending",
                              "10 Least Poor Districts (MPI) - Total Spending","10 Least Poor Districts (MPI) - Education Spending","10 Least Poor Districts (MPI) - Health Spending",
                              "20 Poorest Districts (MPI) - Total Spending","20 Poorest Districts (MPI) - Education Spending","20 Poorest Districts (MPI) - Health Spending",
                              "20 Least Poor Districts (MPI) - Total Spending","20 Least Poor Districts (MPI) - Education Spending","20 Least Poor Districts (MPI) - Health Spending",
                              "10 Poorest Districts (National Poverty) - Total Spending","10 Poorest Districts (National Poverty) - Education Spending","10 Poorest Districts (National Poverty) - Health Spending",
                              "10 Least Poor Districts (National Poverty) - Total Spending","10 Least Poor Districts (National Poverty) - Education Spending","10 Least Poor Districts (National Poverty) - Health Spending",
                              "20 Poorest Districts (National Poverty) - Total Spending","20 Poorest Districts (National Poverty) - Education Spending","20 Poorest Districts (National Poverty) - Health Spending",
                              "20 Least Poor Districts (National Poverty) - Total Spending","20 Least Poor Districts (National Poverty) - Education Spending","20 Least Poor Districts (National Poverty) - Health Spending")
medians_uganda <- t(medians_uganda)
colnames(medians_uganda) <- c("spending (medians)")
write.csv(format(medians_uganda,digits = 3),"graphics/medians_uganda.csv") #and output all combined

datsubregion=data.table(dat)[,.(
  Health.Gov=weighted.mean(uganda_health_funding_value, by=c("uganda_total_pop_value"))
  ,Educ.Gov=weighted.mean(educ.per.cap.ave,by=c("uganda_total_pop_value"))
  ,MPI=weighted.mean(MPI.of.the.region,by=c("uganda_total_pop_value"))
  ,School.Attend=weighted.mean(School.attendance,by=c("uganda_total_pop_value"))
  ,Child.Mortality=weighted.mean(Child.mortality,by=c("uganda_total_pop_value"))
  ,National.Pov=weighted.mean(uganda_poverty_headcount_value,by=c("uganda_total_pop_value"))
  ,Exam.Score=weighted.mean(uganda_leaving_exam_perf_rate_value,by=c("uganda_total_pop_value"))
  ,Health.Outcome.Index=weighted.mean(uganda_overall_health_value,by=c("uganda_total_pop_value"))
  ,Primary.Enroll=weighted.mean(uganda_primary_enrol_value,by=c("uganda_total_pop_value"))
  ,Secondary.Enroll=weighted.mean(uganda_secondary_enrol_value,by=c("uganda_total_pop_value"))
  ,Antenatal.Coverage=weighted.mean(uganda_anc4_coverage_value,by=c("uganda_total_pop_value"))
  ,Tetanus.Vaccine.Coverage=weighted.mean(uganda_dpt3_coverage_value,by=c("uganda_total_pop_value"))
  ,Pupil.classroom.ratio=weighted.mean(uganda_primary_pupil_classroom_ratio_value,by=c("uganda_total_pop_value"))
  ,Pupil.sit.write=weighted.mean(uganda_primary_sit_write_value,by=c("uganda_total_pop_value"))
  ,Pupil.toilet=weighted.mean(uganda_primary_pupil_stance_ratio_value,by=c("uganda_total_pop_value"))
  ,uganda_total_pop_value=sum(uganda_total_pop_value)
  ,SubRegion=SubRegion
),by=c("SubRegion")]
subregion$Country="Uganda"
z.scores.sub=data.table(subregion)[,.(
  Health.Gov.Z=z.score(Health.Gov)
  ,Educ.Gov.Z=z.score(Educ.Gov)
  ,MPI.Z=z.score(MPI)
  ,School.Attend.Z=z.score(School.Attend)
  ,Child.Mortality.Z=z.score(Child.Mortality)
  ,National.Pov.Z=z.score(National.Pov)
  ,Exam.Score.Z=z.score(Exam.Score)
  ,Health.Outcome.Index.z=z.score(Health.Outcome.Index)
  ,Primary.Enroll.z=z.score(Primary.Enroll)
  ,Secondary.Enroll.z=z.score(Secondary.Enroll)
  ,Antenatal.Coverage.z=z.score(Antenatal.Coverage)
  ,Tetanus.Vaccine.Coverage.z=z.score(Tetanus.Vaccine.Coverage)
  ,Pupil.classroom.ratio.z=z.score(Pupil.classroom.ratio)
  ,Pupil.sit.write.z=z.score(Pupil.sit.write)
  ,Pupil.toilet.z=z.score(Pupil.toilet)
  ,pop.z=z.score(uganda_total_pop_value)
  ,SubRegion=SubRegion
),by=c("Country")]


dat2=dat
dat3=dat[which(dat$region!="KALANGALA"),]
# ggplot(dat3, aes(y=Health.Gov.Z,x=National.Pov.Z,label=region))+
#   geom_point(color="grey")+
#   geom_text(check_overlap=T,  size=2.5)+
#   geom_text(y=.9*(max(dat3$Health.Gov.Z)),x=.8*(max(dat3$National.Pov.Z)),label=paste("R^2: ",r2(dat$Health.Gov.Z,dat$National.Pov.Z)), parse=T)+
#   labs(title="Health government spending and poverty rate z scores",y="Government health spending\nper capita z score",x="National poverty rate z score",caption="This drops Kalangala district which appears to be an extreme outlier")+
#   theme_classic()+
#   theme(legend.title=element_blank())+  
#   geom_smooth(method="lm")+
#   geom_vline(xintercept = 0)+
#   geom_hline(yintercept=0) 
# ggsave("graphics/health_ntl_pov_z.png")
# 
# ggplot(dat3, aes(y=Health.Gov.Z,x=MPI.Z,label=region))+
#   geom_point(color="grey")+
#   geom_text(check_overlap=T,  size=2.5)+
#   geom_text(y=.9*(max(dat3$Health.Gov.Z)),x=.8*(max(dat3$MPI.Z)),label=paste("R^2: ",r2(dat$Health.Gov.Z,dat$MPI.Z)), parse=T)+
#   labs(title="Health government spending and MPI z scores",y="Government health spending\nper capita z score",x="MPI z score",caption="This drops Kalangala district which appears to be an extreme outlier")+
#   theme_classic()+
#   theme(legend.title=element_blank())+  
#   geom_smooth(method="lm")+
#   geom_vline(xintercept = 0)+
#   geom_hline(yintercept=0) 
# ggsave("graphics/health_gov_MPI_z.png")
# 
# ggplot(dat3[which(dat3$region!="AMUDAT"),], aes(y=Health.Gov.Z,x=Health.Outcome.Index.z,label=region))+
#   geom_point(color="grey")+
#   geom_text(check_overlap=T,  size=2.5)+
#   geom_text(y=.9*(max(dat3$Health.Gov.Z)),x=.8*(max(dat3$Health.Outcome.Index.z)),label=paste("R^2: ",r2(dat$Health.Gov.Z,dat$Health.Outcome.Index.z)), parse=T)+
#   labs(title="Health government spending and health outcome index z scores",y="Government health spending\nper capita z score",x="Health Outcome Index z score",caption="This drops Kalangala and Amudat districts which appear to be extreme outliers")+
#   theme_classic()+
#   theme(legend.title=element_blank())+  
#   geom_smooth(method="lm")+
#   geom_vline(xintercept = 0)+
#   geom_hline(yintercept=0) 
# ggsave("graphics/health_gov_health_index.png")
# 
# ggplot(dat3[which(dat3$region!="AMUDAT"),], aes(y=Health.Gov.Z,x=Antenatal.Coverage.z,label=region))+
#   geom_point(color="grey")+
#   geom_text(check_overlap=T,  size=2.5)+
#   geom_text(y=.9*(max(dat3$Health.Gov.Z)),x=.8*(max(dat3$Antenatal.Coverage.z)),label=paste("R^2: ",r2(dat$Health.Gov.Z,dat$Antenatal.Coverage.z)), parse=T)+
#   labs(title="Health government spending and health outcome index z scores",y="Government health spending\nper capita z score",x="Antenatal care coverage rate z score",caption="This drops Amudat district which appear to be an extreme outlier")+
#   theme_classic()+
#   theme(legend.title=element_blank())+  
#   geom_smooth(method="lm")+
#   geom_vline(xintercept = 0)+
#   geom_hline(yintercept=0) 
# ggsave("graphics/health_gov_antenatal_care.png")
# 
# ggplot(dat3[which(dat3$region!="AMUDAT"),], aes(y=Health.Gov.Z,x=Tetanus.Vaccine.Coverage.z,label=region))+
#   geom_point(color="grey")+
#   geom_text(check_overlap=T,  size=2.5)+
#   geom_text(y=.9*(max(dat3$Health.Gov.Z)),x=.8*(max(dat3$Tetanus.Vaccine.Coverage.z)),label=paste("R^2: ",r2(dat$Health.Gov.Z,dat$Tetanus.Vaccine.Coverage.z)), parse=T)+
#   labs(title="Health government spending and health outcome index z scores",y="Government health spending\nper capita z score",x="Tetanus vaccine coverage rate z score",caption="This drops Amudat district which appear to be an extreme outlier")+
#   theme_classic()+
#   theme(legend.title=element_blank())+  
#   geom_smooth(method="lm")+
#   geom_vline(xintercept = 0)+
#   geom_hline(yintercept=0) 
# ggsave("graphics/health_gov_tetanus_care.png")
# 
# ggplot(dat, aes(y=Educ.Gov.Z,x=Exam.Score.Z,label=region))+
#   geom_point(color="grey")+
#   geom_text(check_overlap=T,  size=2.5)+
#   geom_text(y=.9*(max(dat$Educ.Gov.Z)),x=.8*(max(dat$Exam.Score.Z)),label=paste("R^2: ",r2(dat$Educ.Gov.Z,dat$Exam.Score.Z)), parse=T)+
#   labs(title="Education government spending and leaving exam performance  z scores",y="Government education spending\nper capita z score",x="Leaving exam performance index z score")+
#   theme_classic()+
#   theme(legend.title=element_blank())+  
#   geom_smooth(method="lm")+
#   geom_vline(xintercept = 0)+
#   geom_hline(yintercept=0) 
# ggsave("graphics/educ_gov_exam.png")
# 
# ggplot(dat, aes(y=Educ.Gov.Z,x=Primary.Enroll.z,label=region))+
#   geom_point(color="grey")+
#   geom_text(check_overlap=T,  size=2.5)+
#   geom_text(y=.9*(max(dat$Educ.Gov.Z)),x=.8*(max(dat$Primary.Enroll.z)),label=paste("R^2: ",r2(dat$Educ.Gov.Z,dat$Primary.Enroll.z)), parse=T)+
#   labs(title="Education government spending and primary enrollment rate z scores",y="Government education spending per capita\nz score",x="Primary Net Enrollment rate z score")+
#   theme_classic()+
#   theme(legend.title=element_blank())+  
#   geom_smooth(method="lm")+
#   geom_vline(xintercept = 0)+
#   geom_hline(yintercept=0) 
# ggsave("graphics/educ_gov_primary_enrollment.png")
# 
# ggplot(dat, aes(y=Educ.Gov.Z,x=National.Pov.Z,label=region))+
#   geom_point(color="grey")+
#   geom_text(check_overlap=T,  size=2.5)+
#   geom_text(y=.9*(max(dat$Educ.Gov.Z)),x=.8*(max(dat$National.Pov.Z)),label=paste("R^2: ",r2(dat$Educ.Gov.Z,dat$National.Pov.Z)), parse=T)+
#   labs(title="Poverty rate at national poverty line and government education spending z scores",y="Government education spending per capita\nz score",x="Poverty rate z score")+
#   theme_classic()+
#   theme(legend.title=element_blank())+  
#   geom_smooth(method="lm")+
#   geom_vline(xintercept = 0)+
#   geom_hline(yintercept=0) 
# ggsave("graphics/educ_gov_ntl_pov.png")
# 
# ggplot(dat, aes(y=Educ.Gov.Z,x=MPI.Z,label=region))+
#   geom_point(color="grey")+
#   geom_text(check_overlap=T,  size=2.5)+
#   geom_text(y=.9*(max(dat$Educ.Gov.Z)),x=.8*(max(dat$MPI.Z)),label=paste("R^2: ",r2(dat$Educ.Gov.Z,dat$MPI.Z)), parse=T)+
#   labs(title="MPI and government education spending z scores",y="Government education spending per capita\nz score",x="MPI z score")+
#   theme_classic()+
#   theme(legend.title=element_blank())+  
#   geom_smooth(method="lm")+
#   geom_vline(xintercept = 0)+
#   geom_hline(yintercept=0) 
# ggsave("graphics/educ_gov_MPI.png")
# 
# 
# 
# ggplot(dat, aes(y=Educ.Gov.Z,x=Pupil.classroom.ratio.z,label=region))+
#   geom_point(color="grey")+
#   geom_text(check_overlap=T,  size=2.5)+
#   geom_text(y=.9*(max(dat$Educ.Gov.Z)),x=.8*(max(dat$Pupil.classroom.ratio.z)),label=paste("R^2: ",r2(dat$Educ.Gov.Z,dat$Pupil.classroom.ratio.z)), parse=T)+
#   labs(title="Pupil to classroom ratio and government education spending z scores",y="Government education spending per capita\nz score",x="Primary pupil to classroom z score")+
#   theme_classic()+
#   theme(legend.title=element_blank())+  
#   geom_smooth(method="lm")+
#   geom_vline(xintercept = 0)+
#   geom_hline(yintercept=0) 
# ggsave("graphics/educ_gov_pupil_classroom.png")
# 
# ggplot(dat, aes(y=Educ.Gov.Z,x=Pupil.sit.write.z,label=SubRegion))+
#   geom_point(color="grey")+
#   geom_text(check_overlap=T,  size=2.5)+
#   geom_text(y=.9*(max(dat$Educ.Gov.Z)),x=.8*(max(dat$Pupil.sit.write.z)),label=paste("R^2: ",r2(dat$Educ.Gov.Z,dat$Pupil.sit.write.z)), parse=T)+
#   labs(title="Students have space to write and read\nand government education spending z scores",y="Government education spending per capita\nz score",x="Primary pupils with adequate sitting\nand writing space z score")+
#   theme_classic()+
#   theme(legend.title=element_blank())+  
#   geom_smooth(method="lm")+
#   geom_vline(xintercept = 0)+
#   geom_hline(yintercept=0) 
# ggsave("graphics/educ_gov_pupil_space.png")
# 
# 
# ggplot(dat, aes(y=Educ.Gov.Z,x=Pupil.toilet.z,label=SubRegion))+
#   geom_point(color="grey")+
#   geom_text(check_overlap=T,  size=2.5)+
#   geom_text(y=.9*(max(dat$Educ.Gov.Z)),x=.8*(max(dat$Pupil.toilet.z)),label=paste("R^2: ",r2(dat$Educ.Gov.Z,dat$Pupil.toilet.z)), parse=T)+
#   labs(title="Primary school pupil to toilet ratio\nand government education spending z scores",y="Government education spending per capita\nz score",x="Primary pupils to toilet ratio z score")+
#   theme_classic()+
#   theme(legend.title=element_blank())+  
#   geom_smooth(method="lm")+
#   geom_vline(xintercept = 0)+
#   geom_hline(yintercept=0) 
# ggsave("graphics/educ_gov_pupil_toilet.png")
# 
# ggplot(dat3, aes(y=Health.Gov.Z,x=National.Pov.Z,label=region))+
#   geom_point(color="grey")+
#   geom_text(check_overlap=T,  size=2.5)+
#   geom_text(y=.9*(max(dat3$Health.Gov.Z)),x=.8*(max(dat3$National.Pov.Z)),label=paste("R^2: ",r2(dat$Health.Gov.Z,dat$National.Pov.Z)), parse=T)+
#   labs(title="Health government spending and poverty rate z scores",y="Government health spending\nper capita z score",x="National poverty rate z score",caption="This drops Kalangala district which appears to be an extreme outlier")+
#   theme_classic()+
#   theme(legend.title=element_blank())+  
#   geom_smooth(method="lm")+
#   geom_vline(xintercept = 0)+
#   geom_hline(yintercept=0) 
# ggsave("graphics/health_ntl_pov_z.png")
# 
# ggplot(dat3, aes(y=Health.Gov.Z,x=MPI.Z,label=region))+
#   geom_point(color="grey")+
#   geom_text(check_overlap=T,  size=2.5)+
#   geom_text(y=.9*(max(dat3$Health.Gov.Z)),x=.8*(max(dat3$MPI.Z)),label=paste("R^2: ",r2(dat$Health.Gov.Z,dat$MPI.Z)), parse=T)+
#   labs(title="Health government spending and MPI z scores",y="Government health spending\nper capita z score",x="MPI z score",caption="This drops Kalangala district which appears to be an extreme outlier")+
#   theme_classic()+
#   theme(legend.title=element_blank())+  
#   geom_smooth(method="lm")+
#   geom_vline(xintercept = 0)+
#   geom_hline(yintercept=0) 
# ggsave("graphics/health_gov_MPI_z.png")
# 
# ggplot(dat3[which(dat3$region!="AMUDAT"),], aes(y=Health.Gov.Z,x=Health.Outcome.Index.z,label=region))+
#   geom_point(color="grey")+
#   geom_text(check_overlap=T,  size=2.5)+
#   geom_text(y=.9*(max(dat3$Health.Gov.Z)),x=.8*(max(dat3$Health.Outcome.Index.z)),label=paste("R^2: ",r2(dat$Health.Gov.Z,dat$Health.Outcome.Index.z)), parse=T)+
#   labs(title="Health government spending and health outcome index z scores",y="Government health spending\nper capita z score",x="Health Outcome Index z score",caption="This drops Kalangala and Amudat districts which appear to be extreme outliers")+
#   theme_classic()+
#   theme(legend.title=element_blank())+  
#   geom_smooth(method="lm")+
#   geom_vline(xintercept = 0)+
#   geom_hline(yintercept=0) 
# ggsave("graphics/health_gov_health_index.png")
# 
# ggplot(dat3[which(dat3$region!="AMUDAT"),], aes(y=Health.Gov.Z,x=Antenatal.Coverage.z,label=region))+
#   geom_point(color="grey")+
#   geom_text(check_overlap=T,  size=2.5)+
#   geom_text(y=.9*(max(dat3$Health.Gov.Z)),x=.8*(max(dat3$Antenatal.Coverage.z)),label=paste("R^2: ",r2(dat$Health.Gov.Z,dat$Antenatal.Coverage.z)), parse=T)+
#   labs(title="Health government spending and health outcome index z scores",y="Government health spending\nper capita z score",x="Antenatal care coverage rate z score",caption="This drops Amudat district which appear to be an extreme outlier")+
#   theme_classic()+
#   theme(legend.title=element_blank())+  
#   geom_smooth(method="lm")+
#   geom_vline(xintercept = 0)+
#   geom_hline(yintercept=0) 
# ggsave("graphics/health_gov_antenatal_care.png")
# 
# ggplot(dat3[which(dat3$region!="AMUDAT"),], aes(y=Health.Gov.Z,x=Tetanus.Vaccine.Coverage.z,label=region))+
#   geom_point(color="grey")+
#   geom_text(check_overlap=T,  size=2.5)+
#   geom_text(y=.9*(max(dat3$Health.Gov.Z)),x=.8*(max(dat3$Tetanus.Vaccine.Coverage.z)),label=paste("R^2: ",r2(dat$Health.Gov.Z,dat$Tetanus.Vaccine.Coverage.z)), parse=T)+
#   labs(title="Health government spending and health outcome index z scores",y="Government health spending\nper capita z score",x="Tetanus vaccine coverage rate z score",caption="This drops Amudat district which appear to be an extreme outlier")+
#   theme_classic()+
#   theme(legend.title=element_blank())+  
#   geom_smooth(method="lm")+
#   geom_vline(xintercept = 0)+
#   geom_hline(yintercept=0) 
# ggsave("graphics/health_gov_tetanus_care.png")
# 
# ggplot(dat3[which(dat3$region!="AMUDAT"),], aes(y=Health.Gov.Z,x=Tetanus.Vaccine.Coverage.z,label=region))+
#   geom_point(color="grey")+
#   geom_text(check_overlap=T,  size=2.5)+
#   geom_text(y=.9*(max(dat3$Health.Gov.Z)),x=.8*(max(dat3$Tetanus.Vaccine.Coverage.z)),label=paste("R^2: ",r2(dat$Health.Gov.Z,dat$Tetanus.Vaccine.Coverage.z)), parse=T)+
#   labs(title="Health government spending and health outcome index z scores",y="Government health spending\nper capita z score",x="Tetanus vaccine coverage rate z score",caption="This drops Amudat district which appear to be an extreme outlier")+
#   theme_classic()+
#   theme(legend.title=element_blank())+  
#   geom_smooth(method="lm")+
#   geom_vline(xintercept = 0)+
#   geom_hline(yintercept=0) 
# ggsave("graphics/health_gov_tetanus_care.png")
# 
# ggplot(dat, aes(y=Educ.Gov.Z,x=Exam.Score.Z,label=region))+
#   geom_point(color="grey")+
#   geom_text(check_overlap=T,  size=2.5)+
#   geom_text(y=.9*(max(dat$Educ.Gov.Z)),x=.8*(max(dat$Exam.Score.Z)),label=paste("R^2: ",r2(dat$Educ.Gov.Z,dat$Exam.Score.Z)), parse=T)+
#   labs(title="Education government spending and leaving exam performance  z scores",y="Government education spending\nper capita z score",x="Leaving exam performance index z score")+
#   theme_classic()+
#   theme(legend.title=element_blank())+  
#   geom_smooth(method="lm")+
#   geom_vline(xintercept = 0)+
#   geom_hline(yintercept=0) 
# ggsave("graphics/educ_gov_exam.png")
# 
# ggplot(dat, aes(y=Educ.Gov.Z,x=Primary.Enroll.z,label=region))+
#   geom_point(color="grey")+
#   geom_text(check_overlap=T,  size=2.5)+
#   geom_text(y=.9*(max(dat$Educ.Gov.Z)),x=.8*(max(dat$Primary.Enroll.z)),label=paste("R^2: ",r2(dat$Educ.Gov.Z,dat$Primary.Enroll.z)), parse=T)+
#   labs(title="Education government spending and primary enrollment rate z scores",y="Government education spending per capita\nz score",x="Primary Net Enrollment rate z score")+
#   theme_classic()+
#   theme(legend.title=element_blank())+  
#   geom_smooth(method="lm")+
#   geom_vline(xintercept = 0)+
#   geom_hline(yintercept=0) 
# ggsave("graphics/educ_gov_primary_enrollment.png")
# 
# ggplot(dat, aes(y=Educ.Gov.Z,x=National.Pov.Z,label=region))+
#   geom_point(color="grey")+
#   geom_text(check_overlap=T,  size=2.5)+
#   geom_text(y=.9*(max(dat$Educ.Gov.Z)),x=.8*(max(dat$National.Pov.Z)),label=paste("R^2: ",r2(dat$Educ.Gov.Z,dat$National.Pov.Z)), parse=T)+
#   labs(title="Poverty rate at national poverty line and government education spending z scores",y="Government education spending per capita\nz score",x="Poverty rate z score")+
#   theme_classic()+
#   theme(legend.title=element_blank())+  
#   geom_smooth(method="lm")+
#   geom_vline(xintercept = 0)+
#   geom_hline(yintercept=0) 
# ggsave("graphics/educ_gov_ntl_pov.png")
# 
# ggplot(dat, aes(y=Educ.Gov.Z,x=MPI.Z,label=region))+
#   geom_point(color="grey")+
#   geom_text(check_overlap=T,  size=2.5)+
#   geom_text(y=.9*(max(dat$Educ.Gov.Z)),x=.8*(max(dat$MPI.Z)),label=paste("R^2: ",r2(dat$Educ.Gov.Z,dat$MPI.Z)), parse=T)+
#   labs(title="MPI and government education spending z scores",y="Government education spending per capita\nz score",x="MPI z score")+
#   theme_classic()+
#   theme(legend.title=element_blank())+  
#   geom_smooth(method="lm")+
#   geom_vline(xintercept = 0)+
#   geom_hline(yintercept=0) 
# ggsave("graphics/educ_gov_MPI.png")
# 
# 
# 
# ggplot(dat, aes(y=Educ.Gov.Z,x=Pupil.classroom.ratio.z,label=region))+
#   geom_point(color="grey")+
#   geom_text(check_overlap=T,  size=2.5)+
#   geom_text(y=.9*(max(dat$Educ.Gov.Z)),x=.8*(max(dat$Pupil.classroom.ratio.z)),label=paste("R^2: ",r2(dat$Educ.Gov.Z,dat$Pupil.classroom.ratio.z)), parse=T)+
#   labs(title="Pupil to classroom ratio and government education spending z scores",y="Government education spending per capita\nz score",x="Primary pupil to classroom z score")+
#   theme_classic()+
#   theme(legend.title=element_blank())+  
#   geom_smooth(method="lm")+
#   geom_vline(xintercept = 0)+
#   geom_hline(yintercept=0) 
# ggsave("graphics/educ_gov_pupil_classroom.png")
# 
# ggplot(dat, aes(y=Educ.Gov.Z,x=Pupil.sit.write.z,label=SubRegion))+
#   geom_point(color="grey")+
#   geom_text(check_overlap=T,  size=2.5)+
#   geom_text(y=.9*(max(dat$Educ.Gov.Z)),x=.8*(max(dat$Pupil.sit.write.z)),label=paste("R^2: ",r2(dat$Educ.Gov.Z,dat$Pupil.sit.write.z)), parse=T)+
#   labs(title="Students have space to write and read\nand government education spending z scores",y="Government education spending per capita\nz score",x="Primary pupils with adequate sitting\nand writing space z score")+
#   theme_classic()+
#   theme(legend.title=element_blank())+  
#   geom_smooth(method="lm")+
#   geom_vline(xintercept = 0)+
#   geom_hline(yintercept=0) 
# ggsave("graphics/educ_gov_pupil_sit_write.png")
# 
# ggplot(dat, aes(y=Educ.Gov.Z,x=Pupil.toilet.z,label=SubRegion))+
#   geom_point(color="grey")+
#   geom_text(check_overlap=T,  size=2.5)+
#   geom_text(y=.9*(max(dat$Educ.Gov.Z)),x=.8*(max(dat$Pupil.toilet.z)),label=paste("R^2: ",r2(dat$Educ.Gov.Z,dat$Pupil.toilet.z)), parse=T)+
#   labs(title="Ratio of primary pupils to latrine stalls\nand government education spending z scores",y="Government education spending per capita\nz score",x="Primary pupil to toilet stance z score")+
#   theme_classic()+
#   theme(legend.title=element_blank())+  
#   geom_smooth(method="lm")+
#   geom_vline(xintercept = 0)+
#   geom_hline(yintercept=0) 
# ggsave("graphics/educ_gov_pupil_toilet.png", caption="A toilet stance refers to the number of stalls in latrines")
# 
# ggplot(dat, aes(y=Educ.Gov.Z,x=Pupil.toilet.z,label=SubRegion))+
#   geom_point(color="grey")+
#   geom_text(check_overlap=T,  size=2.5)+
#   geom_text(y=.9*(max(dat$Educ.Gov.Z)),x=.8*(max(dat$Pupil.toilet.z)),label=paste("R^2: ",r2(dat$Educ.Gov.Z,dat$Pupil.toilet.z)), parse=T)+
#   labs(title="Ratio of primary pupils to latrine stalls\nand government education spending z scores",y="Government education spending per capita\nz score",x="Primary pupil to toilet stance z score")+
#   theme_classic()+
#   theme(legend.title=element_blank())+  
#   geom_smooth(method="lm")+
#   geom_vline(xintercept = 0)+
#   geom_hline(yintercept=0) 
# ggsave("graphics/educ_gov_pupil_toilet.png", caption="A toilet stance refers to the number of stalls in latrines")
# 
# 
# 
# ###Aggregated regions
# ggplot(z.scores.sub, aes(y=Health.Gov.Z,x=National.Pov.Z,label=SubRegion))+
#   geom_point(color="grey")+
#   geom_text(check_overlap=T,  size=2.5)+
#   geom_text(y=.9*(max(z.scores.sub$Health.Gov.Z)),x=.8*(max(z.scores.sub$National.Pov.Z)),label=paste("R^2: ",r2(z.scores.sub$Health.Gov.Z,z.scores.sub$National.Pov.Z)), parse=T)+
#   labs(title="Health government spending and poverty rate z scores",y="Government health spending\nper capita z score",x="National poverty rate z score")+
#   theme_classic()+
#   theme(legend.title=element_blank())+  
#   geom_smooth(method="lm")+
#   geom_vline(xintercept = 0)+
#   geom_hline(yintercept=0) 
# ggsave("graphics/health_ntl_pov_z_large_region.png")
# 
# ggplot(z.scores.sub, aes(y=Health.Gov.Z,x=pop.z,label=SubRegion))+
#   geom_point(color="grey")+
#   geom_text(check_overlap=T,  size=2.5)+
#   geom_text(y=.9*(max(z.scores.sub$Health.Gov.Z)),x=.8*(max(z.scores.sub$pop.z)),label=paste("R^2: ",r2(z.scores.sub$Health.Gov.Z,z.scores.sub$pop.z)), parse=T)+
#   labs(title="Health government spending and population z scores",y="Government health spending\nper capita z score",x="Population z score")+
#   theme_classic()+
#   theme(legend.title=element_blank())+  
#   geom_smooth(method="lm")+
#   geom_vline(xintercept = 0)+
#   geom_hline(yintercept=0) 
# ggsave("graphics/health_population_z_large_region.png")
# 
# ggplot(z.scores.sub, aes(y=Health.Gov.Z,x=MPI.Z,label=SubRegion))+
#   geom_point(color="grey")+
#   geom_text(check_overlap=T,  size=2.5)+
#   geom_text(y=.9*(max(z.scores.sub$Health.Gov.Z)),x=.8*(max(z.scores.sub$MPI.Z)),label=paste("R^2: ",r2(z.scores.sub$Health.Gov.Z,z.scores.sub$MPI.Z)), parse=T)+
#   labs(title="Health government spending and MPI z scores",y="Government health spending\nper capita z score",x="MPI z score")+
#   theme_classic()+
#   theme(legend.title=element_blank())+  
#   geom_smooth(method="lm")+
#   geom_vline(xintercept = 0)+
#   geom_hline(yintercept=0) 
# ggsave("graphics/health_gov_MPI_z_large_region.png")
# 
# ggplot(z.scores.sub, aes(y=Health.Gov.Z,x=Health.Outcome.Index.z,label=SubRegion))+
#   geom_point(color="grey")+
#   geom_text(check_overlap=T,  size=2.5)+
#   geom_text(y=.9*(max(z.scores.sub$Health.Gov.Z)),x=.8*(max(z.scores.sub$Health.Outcome.Index.z)),label=paste("R^2: ",r2(z.scores.sub$Health.Gov.Z,z.scores.sub$Health.Outcome.Index.z)), parse=T)+
#   labs(title="Health government spending and health outcome index z scores",y="Government health spending\nper capita z score",x="Health Outcome Index z score")+
#   theme_classic()+
#   theme(legend.title=element_blank())+  
#   geom_smooth(method="lm")+
#   geom_vline(xintercept = 0)+
#   geom_hline(yintercept=0) 
# ggsave("graphics/health_gov_health_index_large_region.png")
# 
# ggplot(z.scores.sub, aes(y=Health.Gov.Z,x=Antenatal.Coverage.z,label=SubRegion))+
#   geom_point(color="grey")+
#   geom_text(check_overlap=T,  size=2.5)+
#   geom_text(y=.9*(max(z.scores.sub$Health.Gov.Z)),x=.8*(max(z.scores.sub$Antenatal.Coverage.z)),label=paste("R^2: ",r2(z.scores.sub$Health.Gov.Z,z.scores.sub$Antenatal.Coverage.z)), parse=T)+
#   labs(title="Health government spending and health outcome index z scores",y="Government health spending\nper capita z score",x="Antenatal care coverage rate z score")+
#   theme_classic()+
#   theme(legend.title=element_blank())+  
#   geom_smooth(method="lm")+
#   geom_vline(xintercept = 0)+
#   geom_hline(yintercept=0) 
# ggsave("graphics/health_gov_antenatal_care_large_region.png")
# 
# ggplot(z.scores.sub, aes(y=Health.Gov.Z,x=Tetanus.Vaccine.Coverage.z,label=SubRegion))+
#   geom_point(color="grey")+
#   geom_text(check_overlap=T,  size=2.5)+
#   geom_text(y=.9*(max(z.scores.sub$Health.Gov.Z)),x=.8*(max(z.scores.sub$Tetanus.Vaccine.Coverage.z)),label=paste("R^2: ",r2(z.scores.sub$Health.Gov.Z,z.scores.sub$Tetanus.Vaccine.Coverage.z)), parse=T)+
#   labs(title="Health government spending and health outcome index z scores",y="Government health spending\nper capita z score",x="Tetanus vaccine coverage rate z score")+
#   theme_classic()+
#   theme(legend.title=element_blank())+  
#   geom_smooth(method="lm")+
#   geom_vline(xintercept = 0)+
#   geom_hline(yintercept=0) 
# ggsave("graphics/health_gov_tetanus_care_large_region.png")
# 
# ggplot(z.scores.sub, aes(y=Educ.Gov.Z,x=Exam.Score.Z,label=SubRegion))+
#   geom_point(color="grey")+
#   geom_text(check_overlap=T,  size=2.5)+
#   geom_text(y=.9*(max(z.scores.sub$Educ.Gov.Z)),x=.8*(max(z.scores.sub$Exam.Score.Z)),label=paste("R^2: ",r2(z.scores.sub$Educ.Gov.Z,z.scores.sub$Exam.Score.Z)), parse=T)+
#   labs(title="Education government spending and leaving exam performance  z scores",y="Government education spending\nper capita z score",x="Leaving exam performance index z score")+
#   theme_classic()+
#   theme(legend.title=element_blank())+  
#   geom_smooth(method="lm")+
#   geom_vline(xintercept = 0)+
#   geom_hline(yintercept=0) 
# ggsave("graphics/educ_gov_exam_large_region.png")
# 
# ggplot(z.scores.sub, aes(y=Educ.Gov.Z,x=Primary.Enroll.z,label=SubRegion))+
#   geom_point(color="grey")+
#   geom_text(check_overlap=T,  size=2.5)+
#   geom_text(y=.9*(max(z.scores.sub$Educ.Gov.Z)),x=.8*(max(z.scores.sub$Primary.Enroll.z)),label=paste("R^2: ",r2(z.scores.sub$Educ.Gov.Z,z.scores.sub$Primary.Enroll.z)), parse=T)+
#   labs(title="Education government spending and primary enrollment rate z scores",y="Government education spending per capita\nz score",x="Primary Net Enrollment rate z score")+
#   theme_classic()+
#   theme(legend.title=element_blank())+  
#   geom_smooth(method="lm")+
#   geom_vline(xintercept = 0)+
#   geom_hline(yintercept=0) 
# ggsave("graphics/educ_gov_primary_enrollment_large_region.png")
# 
# ggplot(z.scores.sub, aes(y=Educ.Gov.Z,x=National.Pov.Z,label=SubRegion))+
#   geom_point(color="grey")+
#   geom_text(check_overlap=T,  size=2.5)+
#   geom_text(y=.9*(max(z.scores.sub$Educ.Gov.Z)),x=.8*(max(z.scores.sub$National.Pov.Z)),label=paste("R^2: ",r2(z.scores.sub$Educ.Gov.Z,z.scores.sub$National.Pov.Z)), parse=T)+
#   labs(title="Poverty rate at national poverty line and government education spending z scores",y="Government education spending per capita\nz score",x="Poverty rate z score")+
#   theme_classic()+
#   theme(legend.title=element_blank())+  
#   geom_smooth(method="lm")+
#   geom_vline(xintercept = 0)+
#   geom_hline(yintercept=0) 
# ggsave("graphics/educ_gov_ntl_pov_large_region.png")
# 
# ggplot(z.scores.sub, aes(y=Educ.Gov.Z,x=MPI.Z,label=SubRegion))+
#   geom_point(color="grey")+
#   geom_text(check_overlap=T,  size=2.5)+
#   geom_text(y=.9*(max(z.scores.sub$Educ.Gov.Z)),x=.8*(max(z.scores.sub$MPI.Z)),label=paste("R^2: ",r2(z.scores.sub$Educ.Gov.Z,z.scores.sub$MPI.Z)), parse=T)+
#   labs(title="MPI and government education spending z scores",y="Government education spending per capita\nz score",x="MPI z score")+
#   theme_classic()+
#   theme(legend.title=element_blank())+  
#   geom_smooth(method="lm")+
#   geom_vline(xintercept = 0)+
#   geom_hline(yintercept=0) 
# ggsave("graphics/educ_gov_MPI_large_region.png")
# 
# 
# 
# ggplot(z.scores.sub, aes(y=Educ.Gov.Z,x=Pupil.classroom.ratio.z,label=SubRegion))+
#   geom_point(color="grey")+
#   geom_text(check_overlap=T,  size=2.5)+
#   geom_text(y=.9*(max(z.scores.sub$Educ.Gov.Z)),x=.8*(max(z.scores.sub$Pupil.classroom.ratio.z)),label=paste("R^2: ",r2(z.scores.sub$Educ.Gov.Z,z.scores.sub$Pupil.classroom.ratio.z)), parse=T)+
#   labs(title="Pupil to classroom ratio and government education spending z scores",y="Government education spending per capita\nz score",x="Primary pupil to classroom z score")+
#   theme_classic()+
#   theme(legend.title=element_blank())+  
#   geom_smooth(method="lm")+
#   geom_vline(xintercept = 0)+
#   geom_hline(yintercept=0) 
# ggsave("graphics/educ_gov_pupil_classroom_large_region.png")
# 
# ggplot(z.scores.sub, aes(y=Educ.Gov.Z,x=pop.z,label=SubRegion))+
#   geom_point(color="grey")+
#   geom_text(check_overlap=T,  size=2.5)+
#   geom_text(y=.9*(max(z.scores.sub$Educ.Gov.Z)),x=.8*(max(z.scores.sub$pop.z)),label=paste("R^2: ",r2(z.scores.sub$Educ.Gov.Z,z.scores.sub$pop.z)), parse=T)+
#   labs(title="Population\nand government education spending z scores",y="Government education spending per capita\nz score",x="Population z score")+
#   theme_classic()+
#   theme(legend.title=element_blank())+  
#   geom_smooth(method="lm")+
#   geom_vline(xintercept = 0)+
#   geom_hline(yintercept=0) 
# ggsave("graphics/educ_gov_population_large_region.png")
# 
# ggplot(z.scores.sub, aes(y=Health.Gov.Z,x=National.Pov.Z,label=SubRegion))+
#   geom_point(color="grey")+
#   geom_text(check_overlap=T,  size=2.5)+
#   geom_text(y=.9*(max(z.scores.sub$Health.Gov.Z)),x=.8*(max(z.scores.sub$National.Pov.Z)),label=paste("R^2: ",r2(z.scores.sub$Health.Gov.Z,z.scores.sub$National.Pov.Z)), parse=T)+
#   labs(title="Health government spending and poverty rate z scores",y="Government health spending\nper capita z score",x="National poverty rate z score")+
#   theme_classic()+
#   theme(legend.title=element_blank())+  
#   geom_smooth(method="lm")+
#   geom_vline(xintercept = 0)+
#   geom_hline(yintercept=0) 
# ggsave("graphics/health_ntl_pov_z_large_region.png")
# 
# ggplot(z.scores.sub, aes(y=Health.Gov.Z,x=MPI.Z,label=SubRegion))+
#   geom_point(color="grey")+
#   geom_text(check_overlap=T,  size=2.5)+
#   geom_text(y=.9*(max(z.scores.sub$Health.Gov.Z)),x=.8*(max(z.scores.sub$MPI.Z)),label=paste("R^2: ",r2(z.scores.sub$Health.Gov.Z,z.scores.sub$MPI.Z)), parse=T)+
#   labs(title="Health government spending and MPI z scores",y="Government health spending\nper capita z score",x="MPI z score")+
#   theme_classic()+
#   theme(legend.title=element_blank())+  
#   geom_smooth(method="lm")+
#   geom_vline(xintercept = 0)+
#   geom_hline(yintercept=0) 
# ggsave("graphics/health_gov_MPI_z_large_region.png")
# 
# ggplot(z.scores.sub, aes(y=Health.Gov.Z,x=Health.Outcome.Index.z,label=SubRegion))+
#   geom_point(color="grey")+
#   geom_text(check_overlap=T,  size=2.5)+
#   geom_text(y=.9*(max(z.scores.sub$Health.Gov.Z)),x=.8*(max(z.scores.sub$Health.Outcome.Index.z)),label=paste("R^2: ",r2(z.scores.sub$Health.Gov.Z,z.scores.sub$Health.Outcome.Index.z)), parse=T)+
#   labs(title="Health government spending and health outcome index z scores",y="Government health spending\nper capita z score",x="Health Outcome Index z score")+
#   theme_classic()+
#   theme(legend.title=element_blank())+  
#   geom_smooth(method="lm")+
#   geom_vline(xintercept = 0)+
#   geom_hline(yintercept=0) 
# ggsave("graphics/health_gov_health_index_large_region.png")
# 
# ggplot(z.scores.sub, aes(y=Health.Gov.Z,x=Antenatal.Coverage.z,label=SubRegion))+
#   geom_point(color="grey")+
#   geom_text(check_overlap=T,  size=2.5)+
#   geom_text(y=.9*(max(z.scores.sub$Health.Gov.Z)),x=.8*(max(z.scores.sub$Antenatal.Coverage.z)),label=paste("R^2: ",r2(z.scores.sub$Health.Gov.Z,z.scores.sub$Antenatal.Coverage.z)), parse=T)+
#   labs(title="Health government spending and health outcome index z scores",y="Government health spending\nper capita z score",x="Antenatal care coverage rate z score")+
#   theme_classic()+
#   theme(legend.title=element_blank())+  
#   geom_smooth(method="lm")+
#   geom_vline(xintercept = 0)+
#   geom_hline(yintercept=0) 
# ggsave("graphics/health_gov_antenatal_care_large_region.png")
# 
# ggplot(z.scores.sub, aes(y=Health.Gov.Z,x=Tetanus.Vaccine.Coverage.z,label=SubRegion))+
#   geom_point(color="grey")+
#   geom_text(check_overlap=T,  size=2.5)+
#   geom_text(y=.9*(max(z.scores.sub$Health.Gov.Z)),x=.8*(max(z.scores.sub$Tetanus.Vaccine.Coverage.z)),label=paste("R^2: ",r2(z.scores.sub$Health.Gov.Z,z.scores.sub$Tetanus.Vaccine.Coverage.z)), parse=T)+
#   labs(title="Health government spending and health outcome index z scores",y="Government health spending\nper capita z score",x="Tetanus vaccine coverage rate z score")+
#   theme_classic()+
#   theme(legend.title=element_blank())+  
#   geom_smooth(method="lm")+
#   geom_vline(xintercept = 0)+
#   geom_hline(yintercept=0) 
# ggsave("graphics/health_gov_tetanus_care_large_region.png")
# 
# ggplot(z.scores.sub, aes(y=Educ.Gov.Z,x=Exam.Score.Z,label=SubRegion))+
#   geom_point(color="grey")+
#   geom_text(check_overlap=T,  size=2.5)+
#   geom_text(y=.9*(max(z.scores.sub$Educ.Gov.Z)),x=.8*(max(z.scores.sub$Exam.Score.Z)),label=paste("R^2: ",r2(z.scores.sub$Educ.Gov.Z,z.scores.sub$Exam.Score.Z)), parse=T)+
#   labs(title="Education government spending and leaving exam performance  z scores",y="Government education spending\nper capita z score",x="Leaving exam performance index z score")+
#   theme_classic()+
#   theme(legend.title=element_blank())+  
#   geom_smooth(method="lm")+
#   geom_vline(xintercept = 0)+
#   geom_hline(yintercept=0) 
# ggsave("graphics/educ_gov_exam_large_region.png")
# 
# ggplot(z.scores.sub, aes(y=Educ.Gov.Z,x=Primary.Enroll.z,label=SubRegion))+
#   geom_point(color="grey")+
#   geom_text(check_overlap=T,  size=2.5)+
#   geom_text(y=.9*(max(z.scores.sub$Educ.Gov.Z)),x=.8*(max(z.scores.sub$Primary.Enroll.z)),label=paste("R^2: ",r2(z.scores.sub$Educ.Gov.Z,z.scores.sub$Primary.Enroll.z)), parse=T)+
#   labs(title="Education government spending and primary enrollment rate z scores",y="Government education spending per capita\nz score",x="Primary Net Enrollment rate z score")+
#   theme_classic()+
#   theme(legend.title=element_blank())+  
#   geom_smooth(method="lm")+
#   geom_vline(xintercept = 0)+
#   geom_hline(yintercept=0) 
# ggsave("graphics/educ_gov_primary_enrollment_large_region.png")
# 
# ggplot(z.scores.sub, aes(y=Educ.Gov.Z,x=National.Pov.Z,label=SubRegion))+
#   geom_point(color="grey")+
#   geom_text(check_overlap=T,  size=2.5)+
#   geom_text(y=.9*(max(z.scores.sub$Educ.Gov.Z)),x=.8*(max(z.scores.sub$National.Pov.Z)),label=paste("R^2: ",r2(z.scores.sub$Educ.Gov.Z,z.scores.sub$National.Pov.Z)), parse=T)+
#   labs(title="Poverty rate at national poverty line and government education spending z scores",y="Government education spending per capita\nz score",x="Poverty rate z score")+
#   theme_classic()+
#   theme(legend.title=element_blank())+  
#   geom_smooth(method="lm")+
#   geom_vline(xintercept = 0)+
#   geom_hline(yintercept=0) 
# ggsave("graphics/educ_gov_ntl_pov_large_region.png")
# 
# ggplot(z.scores.sub, aes(y=Educ.Gov.Z,x=MPI.Z,label=SubRegion))+
#   geom_point(color="grey")+
#   geom_text(check_overlap=T,  size=2.5)+
#   geom_text(y=.9*(max(z.scores.sub$Educ.Gov.Z)),x=.8*(max(z.scores.sub$MPI.Z)),label=paste("R^2: ",r2(z.scores.sub$Educ.Gov.Z,z.scores.sub$MPI.Z)), parse=T)+
#   labs(title="MPI and government education spending z scores",y="Government education spending per capita\nz score",x="MPI z score")+
#   theme_classic()+
#   theme(legend.title=element_blank())+  
#   geom_smooth(method="lm")+
#   geom_vline(xintercept = 0)+
#   geom_hline(yintercept=0) 
# ggsave("graphics/educ_gov_MPI_large_region.png")
# 
# 
# 
# ggplot(z.scores.sub, aes(y=Educ.Gov.Z,x=Pupil.classroom.ratio.z,label=SubRegion))+
#   geom_point(color="grey")+
#   geom_text(check_overlap=T,  size=2.5)+
#   geom_text(y=.9*(max(z.scores.sub$Educ.Gov.Z)),x=.8*(max(z.scores.sub$Pupil.classroom.ratio.z)),label=paste("R^2: ",r2(z.scores.sub$Educ.Gov.Z,z.scores.sub$Pupil.classroom.ratio.z)), parse=T)+
#   labs(title="Pupil to classroom ratio and government education spending z scores",y="Government education spending per capita\nz score",x="Primary pupil to classroom z score")+
#   theme_classic()+
#   theme(legend.title=element_blank())+  
#   geom_smooth(method="lm")+
#   geom_vline(xintercept = 0)+
#   geom_hline(yintercept=0) 
# ggsave("graphics/educ_gov_pupil_classroom_large_region.png")
# 
# ggplot(z.scores.sub, aes(y=Educ.Gov.Z,x=Pupil.sit.write.z,label=SubRegion))+
#   geom_point(color="grey")+
#   geom_text(check_overlap=T,  size=2.5)+
#   geom_text(y=.9*(max(z.scores.sub$Educ.Gov.Z)),x=.8*(max(z.scores.sub$Pupil.sit.write.z)),label=paste("R^2: ",r2(z.scores.sub$Educ.Gov.Z,z.scores.sub$Pupil.sit.write.z)), parse=T)+
#   labs(title="Students have space to write and read\nand government education spending z scores",y="Government education spending per capita\nz score",x="Primary pupils with adequate sitting\nand writing space z score")+
#   theme_classic()+
#   theme(legend.title=element_blank())+  
#   geom_smooth(method="lm")+
#   geom_vline(xintercept = 0)+
#   geom_hline(yintercept=0) 
# ggsave("graphics/educ_gov_pupil_sit_write_large_region.png")
# 
# 
# ggplot(z.scores.sub, aes(y=Educ.Gov.Z,x=Pupil.toilet.z,label=SubRegion))+
#   geom_point(color="grey")+
#   geom_text(check_overlap=T,  size=2.5)+
#   geom_text(y=.9*(max(z.scores.sub$Educ.Gov.Z)),x=.8*(max(z.scores.sub$Pupil.toilet.z)),label=paste("R^2: ",r2(z.scores.sub$Educ.Gov.Z,z.scores.sub$Pupil.toilet.z)), parse=T)+
#   labs(title="Pupils to toilet stall ratio and government education spending z scores",y="Government education spending per capita\nz score",x="Elementary pupils per latrine stalls z score")+
#   theme_classic()+
#   theme(legend.title=element_blank())+  
#   geom_smooth(method="lm")+
#   geom_vline(xintercept = 0)+
#   geom_hline(yintercept=0) 
# ggsave("graphics/educ_gov_pupil_toilet_large_region.png")
# 
# ggplot(z.scores.sub, aes(y=Educ.Gov.Z,x=pop.z,label=SubRegion))+
#   geom_point(color="grey")+
#   geom_text(check_overlap=T,  size=2.5)+
#   geom_text(y=.9*(max(z.scores.sub$Educ.Gov.Z)),x=.8*(max(z.scores.sub$pop.z)),label=paste("R^2: ",r2(z.scores.sub$Educ.Gov.Z,z.scores.sub$pop.z)), parse=T)+
#   labs(title="Population and government education spending z scores",y="Government education spending per capita\nz score",x="Population z score")+
#   theme_classic()+
#   theme(legend.title=element_blank())+  
#   geom_smooth(method="lm")+
#   geom_vline(xintercept = 0)+
#   geom_hline(yintercept=0) 
# ggsave("graphics/educ_gov_pupil_toilet_large_region.png")




uga.m=melt(uga,id.vars=c("district_id","indicator","name","year"))
uga.w=dcast(uga.m,district_id+name+year~indicator+variable)

uga.m2=uga.m
setnames(uga.m2,"name","region")
uga.m2$region=toupper(uga.m2$region)
uga.m2=merge(uga.m2,output,by=c("region"))



uga.m2$var.description=NA
uga.m2$var.description[which(uga.m2$indicator=="uganda_anc4_coverage")]="Antenatal care coverage"
uga.m2$var.description[which(uga.m2$indicator=="uganda_health_posts")]="Proportion of approved health job posts filled"
uga.m2$var.description[which(uga.m2$indicator=="uganda_dpt3_coverage")]="Tetanus vaccine treatment coverage rate"
uga.m2$var.description[which(uga.m2$indicator=="uganda_overall_health")]="Composite index of all health indicators"
uga.m2$var.description[which(uga.m2$indicator=="uganda_total_pop")]="Population total"
uga.m2$var.description[which(uga.m2$indicator=="uganda_poverty_headcount")]="People below national poverty line"
uga.m2$var.description[which(uga.m2$indicator=="uganda_primary_pupil_classroom_ratio")]="Primary pupils per classroom"
uga.m2$var.description[which(uga.m2$indicator=="uganda_primary_sit_write")]="Percentage of pupils with adequate room to sit and write"           
uga.m2$var.description[which(uga.m2$indicator=="uganda_primary_pupil_stance_ratio")]="Toilet stances (stalls) per primary student"           
uga.m2$var.description[which(uga.m2$indicator=="uganda_primary_enrol" )]="Primary net enrollment rate"               
uga.m2$var.description[which(uga.m2$indicator=="uganda_secondary_enrol"  )]="Secondary net enrollment rate"            
uga.m2$var.description[which(uga.m2$indicator=="uganda_leaving_exam_perf_rate")]="Primary leaving exam pass rate"




inputs.health=c(
                "uganda_anc4_coverage"                
              , "uganda_health_posts"                 
              , "uganda_dpt3_coverage" 
              ,"uganda_overall_health"
              ,"uganda_total_pop"
              ,"uganda_poverty_headcount")


inputs.educ=c(
  "uganda_primary_pupil_classroom_ratio"
  ,"uganda_primary_sit_write"            
  ,"uganda_total_pop"                    
  ,"uganda_primary_enrol"                
  ,"uganda_secondary_enrol"              
  ,"uganda_leaving_exam_perf_rate"
  ,"uganda_poverty_headcount"
  ,"uganda_primary_pupil_stance_ratio"
)
uga.m2$health.per.cap.ave.z=z.score(uga.m2$health.per.cap.ave)
uga.m2$educ.per.cap.ave.z=z.score(uga.m2$educ.per.cap.ave)


z.score.list=list()
file.remove("graphics/Regression outputs.tex")
cat("\\documentclass{article}\\begin{document}", file="graphics/Regression outputs.tex",sep="\n",append=T)
for(input in inputs.health){
  temp=subset(uga.m2, indicator==input)
  temp$z.score=z.score(temp$value)
  z.score.list[[input]]=data.frame(z.score=z.score(temp$value),input=input)
  x.label=unique(temp$var.description)
  y.label="Government health spending per capita"
  title.label=paste0(x.label," & ",y.label )
  file.name=paste0("graphics/","health_gov_",unique(temp$indicator),".png")
  ggplot(temp, aes(y=health.per.cap.ave,x=value,label=region))+
    geom_point(color="grey")+
    geom_text(check_overlap=T,  size=2.5)+
    geom_text(y=.9*(max(temp$health.per.cap.ave)),x=.8*(max(temp$value)),label=paste("R^2: ",r2(temp$health.per.cap.ave,temp$value)), parse=T)+
    labs(title=title.label,y=y.label,x=x.label)+
    theme_classic()+
    theme(legend.title=element_blank())+  
    geom_smooth(method="lm")+
    scale_y_continuous(labels=scales::dollar)
  ggsave(file.name)
  
  x.label.z=paste(unique(temp$var.description),"z score")
  y.label.z=paste("Government health spending per capita","z score")
  title.label.z=paste0(x.label.z," & ",y.label.z )
  file.name.z=paste0("graphics/","health_gov_",unique(temp$indicator),"_z.png")
  
  
  ggplot(temp, aes(y=health.per.cap.ave.z,x=z.score,label=region))+
    geom_point(color="grey")+
    geom_text(check_overlap=T,  size=2.5)+
    geom_text(y=.9*(max(temp$health.per.cap.ave.z)),x=.8*(max(temp$z.score)),label=paste("R^2: ",r2(temp$health.per.cap.ave.z,temp$z.score)), parse=T)+
    labs(title=title.label.z,y=y.label.z,x=x.label.z)+
    theme_classic()+
    theme(legend.title=element_blank())+  
    geom_smooth(method="lm")+
    geom_vline(xintercept = 0)+
    geom_hline(yintercept=0) 
  ggsave(file.name.z)
  
  reg=lm(temp$value~temp$health.per.cap.ave)
  out=capture.output(stargazer(reg,dep.var.labels="Health spending per capita",covariate.labels = x.label))
  cat(out, file="graphics/Regression outputs.tex",sep="\n",append=T)
  
}


for(input in inputs.educ){
  temp=subset(uga.m2, indicator==input)
  temp$z.score=z.score(temp$value)
  z.score.list[[input]]=data.frame(z.score=z.score(temp$value),input=input,region=temp$region)
  x.label=unique(temp$var.description)
  y.label="Government education spending per capita"
  title.label=paste0(x.label," & ",y.label )
  file.name=paste0("graphics/","educ_gov_",unique(temp$indicator),".png")
  ggplot(temp, aes(y=educ.per.cap.ave,x=value,label=region))+
    geom_point(color="grey")+
    geom_text(check_overlap=T,  size=2.5)+
    geom_text(y=.9*(max(temp$educ.per.cap.ave)),x=.8*(max(temp$value)),label=paste("R^2: ",r2(temp$educ.per.cap.ave,temp$value)), parse=T)+
    labs(title=title.label,y=y.label,x=x.label)+
    theme_classic()+
    theme(legend.title=element_blank())+  
    geom_smooth(method="lm")+
    scale_y_continuous(labels=scales::dollar)
  # geom_vline(xintercept = 0)+
  # geom_hline(yintercept=0) 
  ggsave(file.name)
  
  x.label.z=paste(unique(temp$var.description),"z score")
  y.label.z=paste("Government education spending per capita","z score")
  title.label.z=paste0(x.label.z," & ",y.label.z )
  file.name.z=paste0("graphics/","educ_gov_",unique(temp$indicator),"_z.png")
  
  
  ggplot(temp, aes(y=educ.per.cap.ave.z,x=z.score,label=region))+
    geom_point(color="grey")+
    geom_text(check_overlap=T,  size=2.5)+
    geom_text(y=.9*(max(temp$educ.per.cap.ave.z)),x=.8*(max(temp$z.score)),label=paste("R^2: ",r2(temp$educ.per.cap.ave.z,temp$z.score)), parse=T)+
    labs(title=title.label.z,y=y.label.z,x=x.label.z)+
    theme_classic()+
    theme(legend.title=element_blank())+  
    geom_smooth(method="lm")+
    geom_vline(xintercept = 0)+
    geom_hline(yintercept=0) 
  ggsave(file.name.z)
  
  
  reg=lm(temp$value~temp$educ.per.cap.ave)
  out=capture.output(stargazer(reg,dep.var.labels="Education spending per capita",covariate.labels = x.label))
  cat(out, file="graphics/Regression outputs.tex",sep="\n",append=T)
  
}
z.score.df=rbindlist(z.score.list,fill=T)
cat("\\end{document}", file="graphics/Regression outputs.tex",sep="\n",append=T)




