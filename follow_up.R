# determine follow-up period.

library(tidyverse);library(lubridate)


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

d3 = d %>% arrange(last_contact_date)

str(d3$final.date)

d3$final.date = as.Date(d3$final.date,origin = "1960 -01-01")

str(d3$final.date)

d3$final.date = as_date(d3$final.date,)

d3$recent = with(d3, ifelse(final.date > ""))