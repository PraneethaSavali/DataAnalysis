
library(survival)
library(data.table)
library(survminer)
kidney <- read.csv("C:/Users/praneetha/Desktop/statistics/spring 2018/Final Project/kidneytransplant.csv" )
#print(kidney)
View(kidney)

# this is for obstime and death per group
survdiff( Surv(obstime,death) ~ gender + race, data=kidney )
gender_race<-survfit( Surv(obstime,death) ~ gender + race, data=kidney )
ggsurvplot(gender_race)

ggsurvplot(survfit(Surv(obstime,death)~gender+race, data=kidney), pval=TRUE,
           pval.method = TRUE)

# this is for obstime and death per gender
survdiff( Surv(obstime,death) ~ gender , data=kidney )
gender_surv<-survfit( Surv(obstime,death) ~ gender, data=kidney )
ggsurvplot(gender_surv)


# this is for obstime and death per race
survdiff( Surv(obstime,death) ~ race , data=kidney )
race_surv<-survfit( Surv(obstime,death) ~ race, data=kidney )
ggsurvplot(race_surv)


### this is for age
library(dplyr)
kidney$agecat <- cut(kidney$age, breaks=c(0, 43, Inf), labels=c("young", "old"))


# or the dplyr way:
kidney <- kidney %>% 
  mutate(agecat=cut(age, breaks=c(0, 43, Inf), labels=c("young", "old")))

View(kidney)
## survival plot for age
#ggsurvplot(survfit(Surv(obstime,death)~agecat, data=kidney), pval=TRUE)
survdiff( Surv(obstime,death) ~ agecat , data=kidney )
age_surv<-survfit( Surv(obstime,death) ~ agecat, data=kidney )
ggsurvplot(age_surv)


# this is for obstime and death per group
survdiff( Surv(obstime,death) ~ gender + race+agecat, data=kidney )
gender_race_age<-survfit( Surv(obstime,death) ~ gender + race+agecat, data=kidney )
ggsurvplot(gender_race_age)


## Plot for race for gender 1
### How to find the race in plot
gender1_race<-survfit( Surv(obstime,death) ~ race, data=kidney,subset=gender==1 )
ggsurvplot(gender1_race)
survdiff( Surv(obstime,death) ~ race, data=kidney, subset=gender==1)

## plot for gender 2

gender2_race<-survfit( Surv(obstime,death) ~ race, data=kidney,subset=gender==2 )
ggsurvplot(gender2_race)
survdiff( Surv(obstime,death) ~ race, data=kidney, subset=gender==2 )

### plot for race 1
gender_race1<-survfit( Surv(obstime,death) ~ gender, data=kidney,subset=race==1)
ggsurvplot(gender_race1)
survdiff( Surv(obstime,death) ~ gender, data=kidney, subset=race==1 )

### plot for race 2
gender_race2<-survfit( Surv(obstime,death) ~ gender, data=kidney,subset=race==2 )
ggsurvplot(gender_race2)
survdiff( Surv(obstime,death) ~ gender, data=kidney, subset=race==2)

############################ Cox Model ##################3
#remove.packages(c("survminer", "survminer"))
#install.packages('survminer', dependencies = TRUE)
#install.packages("ggfortify")
#install.packages("psych", dependencies = TRUE)
library(ggplot2)
library(psych)
library(survminer)
library(ggfortify)

# cox model for the entire data
group<-coxph(Surv(obstime,death) ~ gender + race, data=kidney)
group
ggsurvplot(survfit(group,data=kidney), color = "#2E9FDF",ggtheme = theme_minimal())


## cox model with age for the entire data.
group<-coxph(Surv(obstime,death) ~ gender + race+agecat, data=kidney)
group
ggsurvplot(survfit(group,data=kidney), color = "#2E9FDF",ggtheme = theme_minimal())

################## for Gender#########################

gender_cox<-coxph(Surv(obstime,death) ~ gender, data=kidney)
gender_cox
ggsurvplot(survfit(gender_cox,data=kidney), color = "#2E9FDF",ggtheme = theme_minimal())


#################For Race ################3

race_cox<-coxph(Surv(obstime,death) ~ race, data=kidney)
race_cox
ggsurvplot(survfit(race_cox,data=kidney), color = "#2E9FDF",ggtheme = theme_minimal())

###################For Age ###############33
age_cox<-coxph(Surv(obstime,death) ~ agecat, data=kidney)
age_cox
ggsurvplot(survfit(age_cox,data=kidney), color = "#2E9FDF",ggtheme = theme_minimal())


################ cox for Gender 1 and Gender 2 for Race######################
cox_gender1<-coxph(Surv(obstime,death) ~ race, data=kidney, subset=gender==1)
cox_gender1
cox_gender2<-coxph(Surv(obstime,death) ~ race, data=kidney, subset=gender==2)
cox_gender2


gender_df <- with(kidney,
               data.frame(race = c(1, 2), 
                          gender = rep(mean(gender, na.rm = TRUE), 2)
                          #race = c(1, 1)
               )
)
gender_df
fit_Gender1 <- survfit(cox_gender2, newdata = gender_df)
ggsurvplot(fit_Gender1,data = gender_df, conf.int = TRUE, legend.labs=c("Race=1", "Race=2"),ggtheme = theme_minimal())

fit_Gender2 <- survfit(cox_gender2, newdata = gender_df)
ggsurvplot(fit_Gender2,data = gender_df, conf.int = TRUE, legend.labs=c("Race=1", "Race=2"),ggtheme = theme_minimal())

############### For subgroup Race 1 and Race 2 for Gender ############################
cox_race1<-coxph(Surv(obstime,death) ~ gender, data=kidney, subset=race==1)
cox_race1
cox_race2<-coxph(Surv(obstime,death) ~ gender, data=kidney, subset=race==2)
cox_race2

race_df <- with(kidney,
               data.frame(gender = c(1, 2), 
                          race = rep(mean(race, na.rm = TRUE), 2)
                          #race = c(1, 1)
               )
)
race_df
fit_race1 <- survfit(cox_race1, newdata = race_df)
ggsurvplot(fit_race1,data = race_df, conf.int = TRUE, legend.labs=c("Gender=1", "Gender=2"),ggtheme = theme_minimal())

fit_race2 <- survfit(cox_race2, newdata = race_df)
ggsurvplot(fit_race2,data = race_df, conf.int = TRUE, legend.labs=c("Gender=1", "Gender=2"),ggtheme = theme_minimal())
