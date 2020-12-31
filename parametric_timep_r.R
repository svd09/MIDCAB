# script for the exponential model for each time period.
# doing this in R as easier that STATA.
# main model done in STATA.

library(pacman)
mypackages <- p_load(tidyverse,survival,flexsurv,rms,Hmisc,haven,
                     tableone,broom)


mypackages # all packages loaded.

# data.

df = 
read_stata('C:\\github_rcode\\midcab\\midcab_paper\\JTCVS_rev1\\JTCVS_rev2\\model_data.dta')

glimpse(df)

# keep time period 1.

df %>% count(era)

df1 = df[df$era == 1, ]

glimpse(df1)


formula <- Surv(fupyears2, died) ~ diabetes + hyperlipemia + copd + pre_dialysis + prior_pci +
  pre_mi + pre_cva + prior_cardiac_surgery + pad + age_at_surgery +
  female_n + low_lvef 

df1_model = flexsurv::flexsurvreg(formula, data = df1,dist = "exponential")


df1_model

df1_model.res <- df1_model$res
df1_model.wald <- df1_model.res[,1]/df1_model.res[,4]
df1_model.p <- 2*pnorm(-abs(df1_model.wald))
                       
summary(df1_model, tidy = T)                   

# function to extract p-values from the flexsurvreg model.

extract.p <- function(model){
  require(tidyverse)
  
  model.res <- model$res
  model.wald <- model.res[,1]/model.res[,4]
  model.p <- 2*pnorm(-abs(model.wald))

  model.p
}

extract.p(df1_model)    

# period 2.


df2 = df[df$era == 2, ]

glimpse(df2)



df2_model = flexsurv::flexsurvreg(formula, data = df2,
                                  dist = "exponential")


df2_model

extract.p(df2_model)

# Period 3.


df3 = df[df$era == 3, ]

glimpse(df3)



df3_model = flexsurv::flexsurvreg(formula, data = df3,
                                  dist = "exponential")


df3_model

extract.p(df3_model)
