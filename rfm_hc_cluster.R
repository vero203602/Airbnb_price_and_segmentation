##### ALLUVIAL PLOT FOR PRESENTATION #####
library(alluvial)
library(dplyr)

# starting from the final dataset
setwd()
write.csv(x=iorgall, file="/Users/veronicacipriani/Desktop/tesi/dati/ISTAT_sgmentation/finale_segm.csv")

iorgall$km <- mapvalues(iorgall$kmeangroupthree,
                             from=c('Group 1','Group 2','Group 3'),
                             to=c('cheap travellers','globetrotters','luxury travellers'))

iorgall %>%
  dplyr::group_by(kmeangroupthree) %>%
  dplyr::summarise(avg = mean(amount),
                   fr  = mean(transaction_count))

df <- iorgall %>% 
  count(km, segment, hcgroup) 

unique(df$segment)

alluvial(df %>% select(-n),
         freq=df$n, border=NA, alpha = 0.5,
         col=case_when(df$segment == "One time cheap"      ~ "deepskyblue",
                       df$segment == "Cheap Globetrotters" ~ "black",
                       df$segment == "One time avg."       ~ "palegreen1",
                       df$segment == "Avg.Globetrotters"   ~ "gold1",
                       df$segment == "One time lux."       ~ "orchid",
                       TRUE ~ "tomato"),
         cex=0.75,
         axis_labels = c("RFM", "kmeans", "hierarc"))


