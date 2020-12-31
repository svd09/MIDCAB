# code for cusum script for 30 day mortality


library(easypackages)
libraries(c("tidyverse",'Hmisc','rms','MASS','survival','mice','Smisc','magrittr',
            'readxl','survminer','tableone','cusum','haven',
            'survMisc','naniar','skimr', 'doBy','lubridate'))

d <- 
  read_stata("C:/github_rcode/midcab/midcab_paper/newdf.dta")



dim(d)

#- am going to risk adjusted cusum
#- need to order the patients according to date of surgery 
#- keep only the variables of interest for the cusum


glimpse(d)

keep <- c( "pat_id", "date_of_surgery", 
           "died", "died_inhouse","date3","logistic_euroscore")

d2 <- d[,c(keep)]

dim(d2)

glimpse(d2)

library(lubridate)

d2$date3 <- as_date(d2$date3)

glimpse(d2$date3)

d3 <- d2 %>% arrange(date3)

glimpse(d3)

head(d3, 10)

#- do the cusum model and then plot
#- first create a glm model for log euroscore 

m = glm(died ~ logistic_euroscore, data = d3, 
        family = "binomial"(link = "logit"))

d3$pred = predict(m, type = "response")

describe(d3$pred)



patient_risks <- d3$pred

patient_outcomes <- d3$died



racusum_limit <- racusum_limit_sim(patient_risks,
                                   odds_multiplier = 2,
                                   n_simulation = 1000,
                                   alpha = 0.05,
                                   seed = 1974)


racusum_cs <- racusum(patient_risks,
                      patient_outcomes,
                      limit = racusum_limit,
                      odds_multiplier = 2,
                      reset = FALSE)


plot(racusum_cs)