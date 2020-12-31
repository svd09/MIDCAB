library(tidyverse);library(gtable)
library(readxl)

d = read_excel("D:\\sheet_p.xlsx", sheet = 1)

glimpse(d)

d2 = d %>% select(Pat_ID, era, cad)

d2

chisq.test(table(d2$era, d2$cad))

df = 
read_csv('C:\\github_rcode\\midcab\\midcab_paper\\early_data2.csv'
                )

patid = d2$Pat_ID

df2 = df[df$Pat_ID %in% patid, ]

glimpse(df2)

df3 = df2 %>% select(Hybrid, Pat_ID)


d3 = left_join(d2, df3, by = "Pat_ID")


h = table(d3$era, d3$Hybrid)

fisher.test(h)
