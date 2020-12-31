# script for LVEF < 40%
# JTCVS rev2.

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

# see lvef 

describe(d2$pre_LVEF)


d2$pre_LVEF[d2$pre_LVEF == -9]<- 54


d2$low_ef = with(d2, ifelse(pre_LVEF <= 40, 1, 0))


d2 %>% count(low_ef)

# low_ef by time period.

CreateCatTable(vars = c('low_ef'), data = d2,
               strata = c("era"))


# survival object

s = Surv(d2$fupyears, d2$died)


lvef <- survfit(s ~ low_ef, data = d2)

summary(lvef, times = c(5,10,15,20))

tiff("C:\\github_rcode\\midcab\\midcab_paper\\JTCVS_rev1\\JTCVS_rev2\\lvef.tiff",
     height = 5, width = 8, units = "in", res = 1200)

ggsurvplot(lvef,data = d2,palette = 'lancet',
           conf.int = T,censor.size = 0,
           surv.scale = 'percent', xlim = c(0,20),
           ylab = 'Percent Surviving', xlab = 'Follow-up:Years',
           risk.table = F,
           legend.labs = c("LVEF > 40%", 'LVEF </= 40%'))

dev.off()

# at risk and values 

summary(lvef, times = c(0,5,10,15,20))

