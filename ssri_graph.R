library(dtplyr)
library(dplyr)
library(ggplot2)

df_base <- data.frame("occurring_country" = c("Japan","Not Japan")[1+round(runif(10^3))],
                      "base_name" = c("SSRI1", "SSRI1", "SSRI3")[1+round(2*runif(10^3))],
                      "pt_name" = c("pt1", "pt2", "pt3")[1+round(2*runif(10^3))])

df <- lazy_dt(df_base)
df_counts <- df %>% count(occurring_country, base_name, pt_name) %>% as.data.frame()

ggplot(df_counts, aes(y=n, fill=pt_name, x = base_name)) + 
  geom_bar(stat="identity") + coord_flip() + facet_grid(~occurring_country)
