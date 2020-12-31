# code for figures revision 1.
# recs: - truncate curve when @ risk < 10 patients - provide error bars 
# and also change the yaxis scale to %.
# in the revised figures use same color scheme as they will change colors anyway.

# get libraries

library(easypackages)
libraries(c("tidyverse",'Hmisc','rms','MASS','survival','mice','Smisc','magrittr',
            'readxl','survminer','tableone','flexsurv','survival',
            'survMisc','naniar','skimr', 'doBy','lubridate'))

d <- 
  read_csv("C:/github_rcode/midcab/midcab_paper/newdf.csv")

dim(d)

f <- read_excel('C:/github_rcode/midcab/midcab_paper/follow_up_data.xlsx',
                sheet = 1)

glimpse(f)

pat_id <- d$Pat_ID

f$within <- with(f, ifelse(Pat_ID %in% pat_id, 1, 0))

f %>% count(within)

f2 <- f %>% filter(within == 1)

d2 <- left_join(d, f2, by = "Pat_ID")

glimpse(d2)

# convert surv_days to years

d2$surv_years <- d2$surv_days/365.24

# overall survival
# surv object

s <- with(d2, Surv(surv_years, died))

summary(s)

#- now the data is set for drawing the figures.
#- use ggsurvplot as we can change the y scale to % 

#- overall survival 

i <- survfit(s ~ 1, data = d2)

ggsurvplot(i,data = d2,palette = 'lancet',
           conf.int = T,censor.size = 0,
           surv.scale = 'percent', xlim = c(0,20),
           ylab = 'Percent Surviving', xlab = 'Follow-up:Years')


#- time period

t <- survfit(s ~ era, data = d2)


ggsurvplot(t,data = d2,palette = 'lancet',
           conf.int = T,censor.size = 0,
           surv.scale = 'percent', xlim = c(0,20),
           ylab = 'Percent Surviving', 
           xlab = 'Follow-up:Years',
           legend.labs = c('Group A',"Group B","Group C")
           )


#- dm plot


dm.surv <- survfit(s ~ diabetes, data = d2)


ggsurvplot(dm.surv,data = d2,palette = 'lancet',
           conf.int = T,censor.size = 0,
           surv.scale = 'percent', xlim = c(0,15),
           ylab = 'Percent Surviving', 
           xlab = 'Follow-up:Years',
           legend.labs = c('DM -', 'DM +'))


#- lv dysfunction

d2$lvd <- with(d2, ifelse(pre_LVEF_function == 1, 0, 1))

d2 %>% count(lvd)

lv.surv <- survfit(s ~ lvd, data = d2)


ggsurvplot(lv.surv,data = d2,palette = 'lancet',
           conf.int = T,censor.size = 0,
           surv.scale = 'percent', xlim = c(0,15),
           ylab = 'Percent Surviving', 
           xlab = 'Follow-up:Years',
           legend.labs = c('low LVEF -', 'low LVEF +'))

#- age 


d2$age_g[d2$age_at_surgery<= 50]<- 1
d2$age_g[50 < d2$age_at_surgery & d2$age_at_surgery<= 80]<- 2
d2$age_g[d2$age_at_surgery > 80]<- 3

d2 %>% count(age_g)


a <- survfit(Surv(surv_years, died) ~ age_g, data = d2)


summary(a, times = c(5,10))

survdiff(Surv(surv_years, died) ~ age_g, data = d2)

ggsurvplot(a,data = d2,palette = 'lancet',
           conf.int = T,censor.size = 0,
           surv.scale = 'percent', xlim = c(0,20),
           ylab = 'Percent Surviving', 
           xlab = 'Follow-up:Years',
           legend.labs = c('< 50 years', 
                           '50 - 80 years'
                          ))

#- all the revised figures created 
#- now to do the rest of the analysis 
# sex

sex <- survfit(s ~ female, data = d2)

summary(sex, times = c(5,10))


library(survminer)

ggsurvplot(sex, censor.size = 0, data = d2,
           conf.int = T, palette = c("blue","red"),
            xlim = c(0,20),
           surv.scale = "percent")