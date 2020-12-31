# JTCVS rev2 - MIDCAB paper.

# 1. Re-label figure 4B 
# 2. Add German pop survival in Figure 4A.

# need to get the year from surgery date 

library(lubridate);library(tidyverse)
library(haven); library(survival)
library(relsurv);library(broom)
library(ggsci);library(Hmisc)

df = read_csv("c:/github_rcode/midcab/midcab_paper/newdf.csv")




#- now to do the relative survival 

#- ge the rate table first

table_germany <- transrate.hmd(
  male = "C:/github_rcode/midcab/midcab_paper/JTCVS_rev1/males_w.txt",
  female = "C:/github_rcode/midcab/midcab_paper/JTCVS_rev1/females_w.txt")


df$age = as.integer(df$age_at_surgery*365.24)

# survival estimates for the German population obtained from the 
# survival package in R.


ov = survexp(Surv(survival_days, died) ~ 1, 
             data = df,
             ratetable = table_germany,
             method = "ederer",
             rmap = list(age = age, sex = gender, year = surg_year),
             scale = 365.25)


plot(ov$time, ov$surv,type = "s")



c_surv <- survfit(Surv(fupyears, died) ~ 1, data = df)

df = broom::tidy(c_surv) %>% tbl_df()

plot(c_surv)


lines(ov$time, ov$surv, col = "red")

# create the plot with German population also.

## Transparent colors
## Mark Gardener 2015
## www.dataanalytics.org.uk

t_col <- function(color, percent = 50, name = NULL) {
  #      color = color name
  #    percent = % transparency
  #       name = an optional name for the color
  
  ## Get RGB values for named color
  rgb.val <- col2rgb(color)
  
  ## Make new color using input color as base and alpha set by transparency
  t.col <- rgb(rgb.val[1], rgb.val[2], rgb.val[3],
               max = 255,
               alpha = (100 - percent) * 255 / 100,
               names = name)
  
  ## Save the color
  invisible(t.col)
}
## END

tiff("C:\\github_rcode\\midcab\\midcab_paper\\JTCVS_rev1\\JTCVS_rev2\\plots\\survival.tiff",
     height = 5, width = 7, units = "in", res = 1200)

plot(df$time, df$estimate*100, type = "s", xlab = "Follow-up:Years",
     ylab = "Survival(%)", col = "blue", xlim = c(0,20), ylim = c(0,100),frame.plot = F,
     )
polygon(c(df$time, rev(df$time)),
        c(df$conf.high*100, rev(df$conf.low*100)),
        col = t_col("blue"), border = NA)
lines(ov$time, ov$surv*100, col = "red")

dev.off()

# CMP for hazard rate for mortality.

cmp <- cmp.rel(Surv(survival_days, died) ~ 1, 
               data = df, ratetable = table_germany,
               tau = 20,
               rmap = list(age = age, sex = gender, year = surg_year))

cmp

summary(cmp, times = c(5,10,15), scale = 365.241,area = T)


tiff("C:\\github_rcode\\midcab\\midcab_paper\\JTCVS_rev1\\JTCVS_rev2\\plots\\cmp_plot.tiff",
height = 5, width = 7, units = "in", res = 1200)

plot(cmp, ylim = c(0,0.01), conf.int = c(1,2), 
     col = c('red','blue'), xlab = c('Followup-Years'),
     ylab = c('Death Rate per Year'),
     curvlab = c("MIDCAB Cohort","German Population"))

dev.off()

# calculating O/E ratio for each period.


df = read_csv("c:/github_rcode/midcab/midcab_paper/newdf.csv")

# calculate the predicted survival using the logistic Euroscore.

ps = glm(died_inhouse ~ logistic_euroscore, 
         data = df, family = "binomial")

df$E = fitted.values(ps)

describe(df$E)

df$OE = df$died_inhouse/df$E

doBy::summaryBy(OE ~ era, data = df)

# O/E ratio for each period.

# <dbl>   <dbl>
#   1     1   1.00 
# 2     2   0.629
# 3     3   0.719

describe(df$logistic_euroscore)

# df$logistic_euroscore 
# n  missing distinct     Info     Mean      Gmd      .05 
# 2667        0      562    0.996    3.666    3.766    0.880 
# .10      .25      .50      .75      .90      .95 
# 0.880    1.220    2.070    3.860    7.602   11.350 
# 
# lowest :  0.88  0.94  1.01  1.07  1.15, highest: 53.62 54.91 59.71 61.91 70.46


# logistic Euroscore per time period.

df %>% count(era)

describe(df$logistic_euroscore)

library(tableone)

le <- CreateContTable(vars = 'logistic_euroscore',
                data = df,
                strata = c("era"))

print(le, nonnormal = 'logistic_euroscore')