list.of.packages <- c("data.table","openxlsx","reshape2","ggplot2","plyr","stargazer","scales")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only=T)



wd="C:/Users/Zach/Box/Subnational paper - Q1 2019/Uganda"
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


uga= fread("http://212.111.41.68:8000/multi_table?indicators=uganda_primary_pupil_stance_ratio,uganda_health_funding,uganda_poverty_headcount,uganda_anc4_coverage,uganda_health_posts,uganda_dpt3_coverage,uganda_primary_pupil_classroom_ratio,uganda_primary_sit_write,uganda_total_pop,uganda_primary_enrol,uganda_secondary_enrol,uganda_leaving_exam_perf_rate,uganda_pop_dens,uganda_overall_health&start_year=2014&end_year=2014&format=csv")
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

output=fread("graphics_Harshcopy/output.csv")
output$region[which(output$region=="SSEMBABULE")]="SEMBABULE"
Childmort=output[,c("region","Child.mortality")]
Schoolatt=output[,c("region","School.attendance")]
MPI=output[,c("region","MPI.of.the.region")]
Childmort$indicator="child_mortality"
Schoolatt$indicator="school_absenteeism"
MPI$indicator="mpi_of_region"
setnames(Childmort,"Child.mortality","value")
setnames(Schoolatt,"School.attendance","value")
setnames(MPI,"MPI.of.the.region","value")
MPIs=rbind(Childmort,Schoolatt)
MPI.outcomes=rbind(MPIs,MPI)

# dat=merge(uga.w,output,by=c("region"))
# dat$uganda_health_funding_value[which(is.na(dat$uganda_health_funding_value))]=0


# z.scores=data.table(dat)[,.(
#   Health.Gov.Z=z.score(uganda_health_funding_value)
#   ,Educ.Gov.Z=z.score(educ.per.cap.ave)
#   ,MPI.Z=z.score(MPI.of.the.region)
#   ,School.Attend.Z=z.score(School.attendance)
#   ,Child.Mortality.Z=z.score(Child.mortality)
#   ,National.Pov.Z=z.score(uganda_poverty_headcount_value)
#   ,Exam.Score.Z=z.score(uganda_leaving_exam_perf_rate_value)
#   ,Health.Outcome.Index.z=z.score(uganda_overall_health_value)
#   ,Primary.Enroll.z=z.score(uganda_primary_enrol_value)
#   ,Secondary.Enroll.z=z.score(uganda_secondary_enrol_value)
#   ,Antenatal.Coverage.z=z.score(uganda_anc4_coverage_value)
#   ,Tetanus.Vaccine.Coverage.z=z.score(uganda_dpt3_coverage_value)
#   ,Pupil.classroom.ratio.z=z.score(uganda_primary_pupil_classroom_ratio_value)
#   ,Pupil.sit.write.z=z.score(uganda_primary_sit_write_value)
#   ,Pupil.toilet.z=z.score(uganda_primary_pupil_stance_ratio_value)
#   ,pop.z=z.score(uganda_total_pop_value)
#   ,region=region
# ),by=c("Country")]
# dat=merge(dat,z.scores,by=c("region"))
# dat=merge(dat,regnames,by=c("region"))



# subregion=data.table(dat)[,.(
#   Health.Gov=weighted.mean(uganda_health_funding_value, by=c("uganda_total_pop_value"))
#   ,Educ.Gov=weighted.mean(educ.per.cap.ave,by=c("uganda_total_pop_value"))
#   ,MPI=weighted.mean(MPI.of.the.region,by=c("uganda_total_pop_value"))
#   ,School.Attend=weighted.mean(School.attendance,by=c("uganda_total_pop_value"))
#   ,Child.Mortality=weighted.mean(Child.mortality,by=c("uganda_total_pop_value"))
#   ,National.Pov=weighted.mean(uganda_poverty_headcount_value,by=c("uganda_total_pop_value"))
#   ,Exam.Score=weighted.mean(uganda_leaving_exam_perf_rate_value,by=c("uganda_total_pop_value"))
#   ,Health.Outcome.Index=weighted.mean(uganda_overall_health_value,by=c("uganda_total_pop_value"))
#   ,Primary.Enroll=weighted.mean(uganda_primary_enrol_value,by=c("uganda_total_pop_value"))
#   ,Secondary.Enroll=weighted.mean(uganda_secondary_enrol_value,by=c("uganda_total_pop_value"))
#   ,Antenatal.Coverage=weighted.mean(uganda_anc4_coverage_value,by=c("uganda_total_pop_value"))
#   ,Tetanus.Vaccine.Coverage=weighted.mean(uganda_dpt3_coverage_value,by=c("uganda_total_pop_value"))
#   ,Pupil.classroom.ratio=weighted.mean(uganda_primary_pupil_classroom_ratio_value,by=c("uganda_total_pop_value"))
#   ,Pupil.sit.write=weighted.mean(uganda_primary_sit_write_value,by=c("uganda_total_pop_value"))
#   ,Pupil.toilet=weighted.mean(uganda_primary_pupil_stance_ratio_value,by=c("uganda_total_pop_value"))
#   ,uganda_total_pop_value=sum(uganda_total_pop_value)
#   ,SubRegion=SubRegion
# ),by=c("SubRegion")]
# subregion$Country="Uganda"
# z.scores.sub=data.table(subregion)[,.(
#   Health.Gov.Z=z.score(Health.Gov)
#   ,Educ.Gov.Z=z.score(Educ.Gov)
#   ,MPI.Z=z.score(MPI)
#   ,School.Attend.Z=z.score(School.Attend)
#   ,Child.Mortality.Z=z.score(Child.Mortality)
#   ,National.Pov.Z=z.score(National.Pov)
#   ,Exam.Score.Z=z.score(Exam.Score)
#   ,Health.Outcome.Index.z=z.score(Health.Outcome.Index)
#   ,Primary.Enroll.z=z.score(Primary.Enroll)
#   ,Secondary.Enroll.z=z.score(Secondary.Enroll)
#   ,Antenatal.Coverage.z=z.score(Antenatal.Coverage)
#   ,Tetanus.Vaccine.Coverage.z=z.score(Tetanus.Vaccine.Coverage)
#   ,Pupil.classroom.ratio.z=z.score(Pupil.classroom.ratio)
#   ,Pupil.sit.write.z=z.score(Pupil.sit.write)
#   ,Pupil.toilet.z=z.score(Pupil.toilet)
#   ,pop.z=z.score(uganda_total_pop_value)
#   ,SubRegion=SubRegion
# ),by=c("Country")]


# dat2=dat
# dat3=dat[which(dat$region!="KALANGALA"),]
setnames(MPI.outcomes,"region","name")
MPI.outcomes$district_id=NA
MPI.outcomes$year=2016
uga=rbind(MPI.outcomes,uga)
uga=uga[,c("name","value","indicator","year")]
uga.m=melt(uga,id.vars=c("indicator","name","year"))
uga.w=dcast(uga.m,name+year~indicator+variable)

uga.m2=uga.m

setnames(uga.m2,"name","region")
uga.m2$region=toupper(uga.m2$region)
output=output[,c("region","educ.per.cap.ave","health.per.cap.ave","MPI.pop.2016")]
uga.m2=join(uga.m2,output,by=c("region"))



uga.m2$var.description=NA
uga.m2$var.description[which(uga.m2$indicator=="uganda_anc4_coverage")]="Antenatal care coverage"
uga.m2$var.description[which(uga.m2$indicator=="uganda_health_posts")]="Proportion of approved health job posts filled"
uga.m2$var.description[which(uga.m2$indicator=="uganda_dpt3_coverage")]="Tetanus vaccine treatment coverage rate"
uga.m2$var.description[which(uga.m2$indicator=="uganda_overall_health")]="Composite index of all health indicators"
uga.m2$var.description[which(uga.m2$indicator=="uganda_total_pop")]="Population total"
uga.m2$var.description[which(uga.m2$indicator=="uganda_poverty_headcount")]="People below national poverty line"
uga.m2$var.description[which(uga.m2$indicator=="uganda_primary_pupil_classroom_ratio")]="Primary pupils per classroom"
uga.m2$var.description[which(uga.m2$indicator=="uganda_primary_sit_write")]="Share of pupils with adequate room to sit/write"           
uga.m2$var.description[which(uga.m2$indicator=="uganda_primary_pupil_stance_ratio")]="Toilet stances (stalls) per primary student"           
uga.m2$var.description[which(uga.m2$indicator=="uganda_primary_enrol" )]="Primary net enrollment rate"               
uga.m2$var.description[which(uga.m2$indicator=="uganda_secondary_enrol"  )]="Secondary net enrollment rate"            
uga.m2$var.description[which(uga.m2$indicator=="uganda_leaving_exam_perf_rate")]="Primary leaving exam pass rate"
uga.m2$var.description[which(uga.m2$indicator=="child_mortality")]="Share households experiencing recent child death"
uga.m2$var.description[which(uga.m2$indicator=="school_absenteeism")]="Share households where child is missing school"
uga.m2$var.description[which(uga.m2$indicator=="mpi_of_region")]="Multidimensional Poverty Index"
uga.m2$var.description[which(uga.m2$indicator=="uganda_pop_dens")]="Population density"


 


inputs.health=c(
                "uganda_anc4_coverage"                
              , "uganda_health_posts"                 
              , "uganda_dpt3_coverage" 
              ,"uganda_overall_health"
              ,"uganda_total_pop"
              ,"uganda_poverty_headcount"
              ,"child_mortality"
              ,"mpi_of_region"
              ,"uganda_pop_dens")


inputs.educ=c(
  "uganda_primary_pupil_classroom_ratio"
  ,"uganda_primary_sit_write"            
  ,"uganda_total_pop"                    
  ,"uganda_primary_enrol"                
  ,"uganda_secondary_enrol"              
  ,"uganda_leaving_exam_perf_rate"
  ,"uganda_poverty_headcount"
  ,"uganda_primary_pupil_stance_ratio"
  ,"school_absenteeism"
  ,"mpi_of_region"
  ,"uganda_pop_dens"
)
uga.m2$health.per.cap.ave.z=z.score(uga.m2$health.per.cap.ave)
uga.m2$educ.per.cap.ave.z=z.score(uga.m2$educ.per.cap.ave)





