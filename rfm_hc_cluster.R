##### ALLUVIAL PLOT FOR PRESENTATION #####
library(alluvial)
library(dplyr)
library(plyr)
library(ggplot2)
library(ggalluvial)

# starting from the final dataset
setwd("/Users/veronicacipriani/Desktop/tesi/dati/ISTAT_sgmentation")

iorgall    <- read.csv('finale_segm.csv')[, -1]
iorgall$km <- plyr::mapvalues(iorgall$kmeangroupthree,
                              from=c('Group 1','Group 2','Group 3'),
                              to=c('cheap travellers','globetrotters','luxury travellers'))

iorgall$km <- factor(iorgall$km, levels = 
                       c('cheap travellers','globetrotters','luxury travellers'))


iorgall$segment <- factor(iorgall$segment, levels = 
                            c("One time cheap","Cheap Globetrotters","One time avg.","Avg.Globetrotters",
                              "One time lux.","Lux.Globetrotters"))


iorgall$hc <- factor(iorgall$hc, levels = 
                       c('Group 1','Group 2',
                         'Group 3', 'Group 4'))

iorgall %>%
  dplyr::group_by(kmeangroupthree) %>%
  dplyr::summarise(avg = mean(amount),
                   fr  = mean(transaction_count))

df <- iorgall %>% 
  dplyr::count(segment, km, hcgroup) 

alluvial(df %>% select(-n),
         freq=df$n, border=NA, alpha = 0.5,
         col=case_when(df$segment == "One time cheap"      ~ "deepskyblue",
                       df$segment == "Cheap Globetrotters" ~ "black",
                       df$segment == "One time avg."       ~ "palegreen1",
                       df$segment == "Avg.Globetrotters"   ~ "gold1",
                       df$segment == "One time lux."       ~ "orchid",
                       TRUE ~ "tomato"),
         cex=0.75,
         axis_labels = c("RFM", "k-means", "hier.\n clustering"),
         blocks = 'TRUE')

ggplot(data = df,
       aes(y = n, axis1 = segment, axis2 = km, axis3 = hcgroup)) +
  geom_alluvium(aes(fill = segment),width = 1/2, knot.pos = 0)+ 
  #width = 1/12) +
  geom_stratum(width = 1/12, fill = "black", color = "grey") +
  geom_label(stat = "stratum", aes(label = after_stat(stratum))) +
  scale_x_discrete(limits = c("RFM", "k-means", 'Hier.\n clustering'), expand = c(.05, .05)) +
  scale_fill_manual(values = c("orchid","black","palegreen1",
                               "gold1","deepskyblue","tomato")) +
  theme_minimal() +
  theme(legend.position = 'none',
        axis.title.y=element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank())







