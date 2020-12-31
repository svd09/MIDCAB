# parametric model for data

# fit a simple km plot

d2$surv_p = d2$surv_years + 0.01

s <- with(d2, Surv(surv_p, died))

surv_f <- survfit(s ~ 1, data = d2)

plot(surv_f)


# check exponential model

exp <- flexsurv::flexsurvreg(s ~ 1, data = d2, dist = "exponential")

plot(exp, xlim = c(0,15), ylab = "Proportion Surviving",
     xlab = "Time:Years", main = "Kaplan Meier curve fitted with 
     the Exponential model (red)")

exp



# weibull

plot(surv_f,fun = 'cumhaz')

w <- flexsurv::flexsurvreg(s ~ 1, data = d2, dist = 'weibull')

plot(w,xlim = c(0,15), ylab = "Proportion Surviving",
     xlab = "Time:Years", main = "Kaplan Meier curve fitted with 
     the Weibull model (red)"
     )

w
exp

#- do the parametric model now

d2$lvd <- with(d2, ifelse(pre_LVEF_function == 1, 0, 1))

d2 %>% count(lvd)


y <- c('Pat_ID','age_at_surgery', "diabetes", "hyperlipemia", "COPD", "pre_dialysis", 
        "prior_PCI", "pre_MI", "pre_CVA", "prior_cardiac_surgery", 
       "pad",  "era",'died','fupyears','incompl_revascularisation','female','pre_LVEF')

pdf <- d2[,c(y)]



pdf$age.g <- with(pdf, ifelse(age_at_surgery< 50, 0,
                                  ifelse(age_at_surgery > 49 & age_at_surgery < 80, 1, 2)))

pdf %>% count(age.g)

pdf$age.g <- factor(pdf$age.g, levels = c(0,1,2), 
                    labels = c("< 50", "50 - 80", "> 80"))

# save the df in dta to use in stata

library(haven)

write_csv(pdf, "C:/github_rcode/midcab/midcab_paper/JTCVS_rev1/pdf.csv")

#- see the missing data

library(naniar)

naniar::miss_var_summary(pdf)

#- simple imputation was performed for the data.
#- impute for the most common value
#- all missing data had factor variables in it
#- apart from DM, other variables did not have a lot of missing data
#- sensitivity analysis for DM with and without imputation to see diff
#- in coefficients


hyperlipemia + COPD + pre_dialysis +  female + 
  lvd + prior_PCI + pre_MI + pre_CVA + prior_cardiac_surgery + 
  pad + era + incompl_revascularisation + age.g, 

model <- flexsurvreg(s  ~ diabetes + female,  
  data = pdf, dist = 'weibull') 

library(broom)

plot(model, type = "hazard")

plot(model, newdata = data.frame(diabetes = c(0,1), female = c("no","no")))


