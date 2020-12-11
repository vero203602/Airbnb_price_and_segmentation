##########################################################################
########################### ISTAT: segmentation ########################## 
##########################################################################

# upload libraries
library(plyr)
library(dplyr)
library(purrr)
library(ggplot2)
library(treemapify)
library(gridExtra)
library(grid)
library(scales)
library(cluster)
library(FactoMineR)
library(devtools)
devtools::install_github("XanderHorn/autoEDA")
library(autoEDA)
library(cowplot)
library(factoextra)
library(NbClust)
library(fastDummies)
library(fpc)
library(dendextend)
library(rfm)
library(readr) 
library(Rtsne)
library(GGally)
library(corrplot)
library(wesanderson)
library(pvclust)


setwd('/Users/veronicacipriani/Desktop/tesi/dati/ISTAT_sgmentation/VV_2019_IT_TXT/MICRODATI')
data <- read.delim2 ('VV_Microdati_2019_Viaggi.txt',  header=T , sep="\t",  
                     quote="",  na.strings = ".")
# data source: anno 2019 https://www.istat.it/it/archivio/178695

##########################################################################
############################# Data Cleaning ##############################
##########################################################################

# first remove rows with all NA
data <- data[rowSums(is.na(data)) != ncol(data), ]
# Some columns are dropped a priori cause there is no need for such specific info
data <- data[, -c(1,2,3,7,8,9,10,14,15,16,17,18,21,22,28,31)]

data$GGINIZ        <- formatC(data$GGINIZ, width = 2, format = "d", flag = "0")
data$MMINIZ        <- formatC(data$MMINIZ, width = 2, format = "d", flag = "0")
data$GGINIZ        <- paste(data$AAINIZ,data$MMINIZ,data$GGINIZ, sep="-")
data$GGINIZ        <- format(as.Date(data$GGINIZ), "%Y-%m-%d")
colnames(data)[10] <- "date"
data$AAINIZ        <- NULL
data$MMINIZ        <- NULL
data$recency       <- as.integer(difftime(as.Date("2019-12-31"), 
                                          as.Date(data$date)),
                                 units = 'days')

# change to a dummy: other participants = 1, noone = 0
data$ALPART  <- ifelse(data$ALPART  == 1, 1, 0)
# change to a dummy: italy = 1, abroad = 0
data$DEST_IE <- ifelse(data$DEST_IE == 1, 1, 0)

data$TIPOMARE  <- ifelse(data$TIPOMARE  == 1, 1, 0)
data$TIPOCROC  <- ifelse(data$TIPOCROC  == 1, 1, 0)
data$TIPOMONT  <- ifelse(data$TIPOMONT  == 1, 1, 0)
data$TIPOCITTA <- ifelse(data$TIPOCITTA == 1, 1, 0)
data$TIPOCAMP  <- ifelse(data$TIPOCAMP  == 1, 1, 0)
data$TIPOALTRO <- ifelse(data$TIPOALTRO == 1, 1, 0)

data$piattall1 <- ifelse(data$piattall1 == 1, 1, 0)
data$piattall2 <- ifelse(data$piattall2 == 1, 1, 0)
data$piattall3 <- ifelse(data$piattall3 == 1, 1, 0)

# working status of the individual
data$condogg <- mapvalues(data$condogg, #trasformo in binaria work 1, not-working 0
                          from=c(1, 2, 3), 
                          to=c(1, # Occupato
                               0, #In cerca di occupazione
                               0)) #Inattivo

data$posiz4  <- mapvalues(data$posiz4, 
                          from=c(1, 2, 3, 4), 
                          to=c('Dirigente/impiegato', 
                               'Operaio', 
                               'Lib.professionista', 
                               'Proprio')) 

# I jsut want to retain the main reason of the trip: holiday or business
# change to a dummy: holiday = 1, work = 0
data$TIPOVGG <- ifelse(grepl('^V', data$TIPOVGG), 1, 0)

# change alloggio to a factor with different types
data$ALLOG <- mapvalues(data$ALLOG, 
                        from=c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18), 
                        to= c("Stanza hotel",        # Albergo/motel/pensione
                              "Stanza privata",      # Residenza per cure fisiche ed estetiche
                              "Altro",               # Campo lavoro e vacanza
                              "Altro",               # Mezzo pubblico di trasporto
                              "Stanza privata",      # Centro congressi e conferenze
                              "Intero appartamento", # Villaggio vacanza 
                              "Altro",               # Campeggio
                              "Altro",               # Marina (Barca in porto, darsena)
                              "Stanza privata",      # Istituto religioso
                              "Stanza privata",      # Altra struttura collettiva
                              "Stanza privata",      # Stanza in affitto
                              "Intero appartamento", # Abitazione in affitto
                              "Intero appartamento", # Abitazione in proprieta o multiprop.
                              "Intero appartamento", # Abitazione di parenti o amici gratuita
                              "Altro",               # Barca in sito non organizzato
                              "Altro",               # Altro tipo di sistemazione privata
                              "Stanza privata",      # Agriturismo
                              "Stanza privata"       # Bed-breakfast
                              ))  

# change to a dummy: use of internet = 1, no/don't know = 0
data$IORGALL <- ifelse(data$IORGALL == 1, 1, 0)

# change to a dummy: use of internet = 1, no/don't know = 0
data$IORGTRA <- ifelse(data$IORGTRA == 1, 1, 0)

#### create another dataset only with numeric
data_numeric <- data
data_numeric$sesso <- ifelse(data_numeric$sesso == 2, 1, 0) 
                      # 1 for female
data_numeric$eta10 <- mapvalues(data_numeric$eta10, 
                        from = c(1, 2, 3, 4, 5, 6, 7, 8),
                        to = c(1,1,2,2,3,3,4,4)) 

### using the info in PGM_2019_Viaggi_IT_DELIMITED.r the factors are recoded ###
# sex recoded as m/f
data$sesso <- mapvalues(data$sesso,
                        from = c(1,2),
                        to = c('M', 'F'))

# age recoded and treated as factor
data$eta10 <- mapvalues(data$eta10, 
                        from = c(1, 2, 3, 4, 5, 6, 7, 8),
                        to = c('<=24 years', '<=24 years', '25-44 years', '25-44 years', 
                               '45-64 years', '45-64 years', '>=65 years', 
                               '>=65 years')) 
data$eta10 <- factor(data$eta10, levels = 
                       c('<=24 years', '25-44 years', 
                         '45-64 years', '>=65 years'))

# level of education: recoded and change into factor
data$istr4 <- mapvalues(data$istr4, 
                        from=c(1, 2, 3, 4), 
                        to=c('No title', 
                             'Middle school', 
                             'High school', 
                             'Graduated')) 
data$istr4 <- factor(data$istr4, levels = 
                       c('No title', 
                         'Middle school', 
                         'High school', 
                         'Graduated'))

# FINAL DATASET
# The number of people using Airbnb is very small in our sample --> 98
# The analysis therefore investigates first the subset of people using internet
# to book accommodation, and then try to understand if there is a significant
# difference in variables for this group and subgroup --> these preliminary analysis
# could help identifying who should be targeted next

# in order to analyze the variable IORGALL, i drop all the observations with na value
iorgall <- data[!(is.na(data$IORGALL)),]
int_numeric         <- data_numeric[!(is.na(data_numeric$IORGALL)),]
int_numeric$TIPOVGG <- as.numeric(int_numeric$TIPOVGG)

##########################################################################
################################## EDA ###################################
##########################################################################

# nrow(data[!(is.na(data$piattall2)),])
# Airbnb <- data[data$piattall2 == 1,]

#head(iorgall)
plot <- iorgall %>%
  dplyr::count(eta10, IORGALL, istr4) %>%
  dplyr::group_by(eta10) %>%
  dplyr::mutate(p = n/sum(n)) %>%
  dplyr::group_by(IORGALL,eta10) %>%
  dplyr::mutate(n = sum(n))

pal = wes_palette("Darjeeling2", n = 4)
ggplot(plot, aes(x = IORGALL, y= p, fill = istr4)) +
  scale_fill_manual(values = pal) + 
  geom_col(position = "fill") + 
  theme_minimal() +
  #geom_label(aes(label = n), position = "fill", color = "white", vjust = 1, show.legend = FALSE) +
  #scale_y_continuous(labels = n) +
  labs(fill='edu') +
  scale_x_discrete(labels= c('no', 'yes')) +
  facet_grid(~eta10) +
  labs(title="Bar plot of use of internet", 
       subtitle="Age and level of education, use of internet for booking accommodation",
       caption = 'Source: ISTAT',
       x = 'use of internet',
       y = 'percentage of user')

plot2 <- iorgall %>%
  filter(piattall2 == c(0, 1)) %>%
  dplyr::count(eta10, piattall2, istr4) %>%
  dplyr::group_by(eta10) %>%
  dplyr::mutate(p = n/sum(n)) %>%
  dplyr::group_by(piattall2,eta10) %>%
  dplyr::mutate(n = sum(n))

ggplot(plot2, aes(x = piattall2, y= p, fill = istr4)) +
  scale_fill_manual(values = pal) + 
  geom_col(position = "fill") + 
  theme_minimal() +
  labs(fill='edu') +
  #geom_label(aes(label = n), position = "fill", color = "white", vjust = 1, show.legend = FALSE) +
  #scale_y_continuous(labels = n) +
  scale_x_discrete(labels= c('no', 'yes')) +
  facet_grid(~eta10) +
  labs(title="Bar plot of use of sharing platoform", 
       subtitle="Age and level of education, use of sharing platforms while booking accommodation",
       caption = 'Source: ISTAT',
       x = 'use of sharing platforms',
       y = 'percentage of user') 
# for labels: 
#https://stackoverflow.com/questions/41128877/counts-of-categorical-values-on-bar-chart-in-ggplot

# absolute numbers
iorgall$IORGALLplot <- factor(iorgall$IORGALL)
ggplot(iorgall, aes(IORGALLplot)) +
  scale_fill_brewer(palette = 'Set2') + 
  geom_bar(aes(fill=sesso)) +
  geom_text(aes(label=..count..),stat="count",vjust = 0,size=3) +
  theme_minimal() +
  scale_x_discrete(labels= c('no', 'yes')) +
  labs(fill='sex') +
  facet_grid(~eta10) +
  labs(title="Bar plot of booking via web", 
       subtitle="Age and sex on use of internet",
       caption = 'Source: ISTAT',
       x = 'use of internet') 
iorgall$IORGALLplot <- NULL

# check for which platform is used
iorgall$piattall2plot <- factor(iorgall$piattall2)
ggplot(iorgall, aes(piattall2plot)) +
  scale_fill_brewer(palette = 'Set2') + 
  geom_bar(aes(fill=sesso)) +
  geom_text(aes(label=..count..),stat="count",vjust = 0,size=3) +
  theme_minimal() +
  scale_x_discrete(labels= c('no', 'yes', 'No info')) +
  labs(fill='sex') +
  facet_grid(~eta10) +
  labs(title="Bar plot of booking through sharing platforms", 
       subtitle="Age and sex on use of platforms",
       caption = 'Source: ISTAT',
       x = 'use of platforms') 
iorgall$piattall2plot <- NULL

#age
age_plot <- iorgall %>%
  group_by(eta10) %>%
  tally()

a <- ggplot(data=age_plot, aes(x=eta10, y=n)) +
  geom_bar(stat="identity", fill = 'tomato2',  width = 0.75) +
  theme_minimal() +
  coord_cartesian(ylim=c(0,1500)) +
  geom_text(aes(label = percent(n/sum(n))), vjust=-1) +
  #theme(axis.text.x = element_text(angle = 45)) + 
  labs(#title="", 
       subtitle="Age",
       caption = '',
       y = 'count',
       x = 'Age')

#sex
sex_plot <- iorgall %>%
  group_by(sesso) %>%
  tally()
s <- ggplot(data=sex_plot, aes(x=sesso, y=n)) +
  geom_bar(stat="identity", fill = 'orange', width = 0.75) +
  theme_minimal() +
  coord_cartesian(ylim=c(0,1500)) +
  geom_text(aes(label = percent(n/sum(n))), vjust=-1) +
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank()) +
  labs(#title="sex distrinbution", 
       subtitle="Gender",
       caption = '',
       y = 'count',
       x = 'Sex')

#edu
edu_plot <- iorgall %>%
  group_by(istr4) %>%
  tally()
e <- ggplot(data=edu_plot, aes(x=istr4, y=n)) +
  geom_bar(stat="identity", fill = 'orchid1', width = 0.75) +
  theme_minimal() +
  coord_cartesian(ylim=c(0,1500)) +
  geom_text(aes(label = percent(n/sum(n))), vjust=-1) +
  labs(#title=, 
       subtitle="Education",
       caption = '',
       y = 'count',
       x = 'Edu level')

#work
work_plot <- iorgall %>%
  group_by(condogg) %>%
  tally()
work_plot$condogg <- mapvalues(work_plot$condogg, 
                               from = c(0, 1, NA),
                               to = c('Not working', 'Working', 'No info')) 
work_plot$condogg <- factor(work_plot$condogg,
                            levels = c('Working', 'Not working', 'No info'))
w <- ggplot(data=work_plot, aes(x=condogg, y=n)) +
  geom_bar(stat="identity", fill = 'seagreen1', width = 0.75) +
  theme_minimal() +
  coord_cartesian(ylim=c(0,1500)) +
  geom_text(aes(label = percent(n/sum(n))), vjust=-1) +
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank()) +
  labs(#title="", 
       subtitle="Working status",
       caption = 'Source: ISTAT',
       y = 'count',
       x = 'Working status')

grid.arrange(a, s, e , w, nrow = 2, 
             top = textGrob("Socio-demographic variables: Bar plots",
                            gp=gpar(fontsize=15,font=2)))

#table(iorgall$progvia)

##########################################################################
############################## SEGMENTATION ##############################
##########################################################################
####### Segmentation will be performed in two different ways: 
#######   First, we consider PROFIT POTENTIAL (with RFM)
#######   Then, we perform different clustering methods on demographics  
####### and past purchases (kmeans, hierarchical)
##########################################################################
########################### NUMERICAL DATA ###############################
##########################################################################
# --------------------------------- ## --------------------------------- #
#####################################
# RFM: RECENCY, FREQUENCY, MONETARY #
### RFM scores
# The idea is to divide customers according to the three scores obtained.
rfm_data <- iorgall %>% 
  group_by(progind) %>%
  dplyr::summarise(revenue= sum(ESPE_CO),
                   # Spesa media per viaggio: ammontare di spesa del 
                   # viaggio riferito al singolo partecipante al viaggio
                   most_recent_visit = min(date),
                   number_of_orders =n_distinct(progvia),
                   recency_days = min(recency))
colnames(rfm_data) <- c('customer_id', names(rfm_data)[2:5])
date_rfm <- lubridate::as_date("2019-12-31")
rfm_data$most_recent_visit <- as.Date(rfm_data$most_recent_visit)

rfm_df_results <- rfm_table_customer(rfm_data, customer_id, number_of_orders,
                                   recency_days, revenue, date_rfm, 
                                   recency_bins = 3, frequency_bins = 3, 
                                   monetary_bins = 3)

rfm_df_results$rfm %>%
  group_by(rfm_score) %>%
  tally()

rfm_df_results$rfm
dffinale <- as.data.frame(rfm_df_results$rfm)
rfm_heatmap(rfm_df_results) 
rfm_bar_chart(rfm_df_results)
rfm_fm_plot(rfm_df_results)

sort(unique(rfm_df_results$rfm$rfm_score))

# Divide tourists into 7 categories
most_prof   <- c(123, 133, 223, 233, 323, 333) # luxury globetrotters
onetime_lux <- c(113, 213, 313)
avg_globe   <- c(132, 222, 232, 323, 332) #average traveler 2-4 times per year
avg         <- c(112, 212, 312) # avg one time
cheap_often <- c(131, 231, 331)
less_p      <- c(111, 121, 211, 221, 311, 321)


rfm_segment <- as.vector(rfm_df_results$rfm$rfm_score)
rfm_segment[which(rfm_segment %in% onetime_lux)] <- "One time lux."
rfm_segment[which(rfm_segment %in% most_prof)]   <- "Lux.Globetrotters"
rfm_segment[which(rfm_segment %in% avg)]         <- "One time avg."
rfm_segment[which(rfm_segment %in% avg_globe)]   <- "Avg.Globetrotters"
rfm_segment[which(rfm_segment %in% less_p)]      <- "One time cheap"
rfm_segment[which(rfm_segment %in% cheap_often)] <- "Cheap Globetrotters"

dffinale$segment <- factor(rfm_segment, 
                           levels = c('One time cheap', "Cheap Globetrotters",
                                      "One time avg.", "Avg.Globetrotters",
                                      "One time lux.","Lux.Globetrotters"))

rfm_data$segment <- dffinale$segment
rfm_data %>% 
  dplyr::count(segment)
id_segment <- rfm_data[, c(1,6)] # MERGE WITH THE RESULT OF K-MEANS


aggregate(dffinale$recency_days, by = list(dffinale$recency_score), 
          FUN = function(x) c(mn = min(x), mx = max(x) ) )
aggregate(dffinale$transaction_count, by = list(dffinale$frequency_score), 
          FUN = function(x) c(mn = min(x), mx = max(x) ) )
aggregate(dffinale$amount, by = list(dffinale$monetary_score), 
          FUN = function(x) c(mn = min(x), mx = max(x) ) )

aggregate(dffinale$amount, by = list(dffinale$segment), 
          FUN = function(x) c(mn = min(x), mx = max(x) ) )

aggregate(dffinale$amount, by = list(dffinale$segment), mean)
aggregate(dffinale$transaction_count, by = list(dffinale$segment), mean)
aggregate(dffinale$recency_days, by = list(dffinale$segment), mean)

# visualize data
length(unique(dffinale$customer_id))
dffinale %>%
  group_by(segment)%>%
  tally()

treemapdf <- dffinale %>%
  dplyr::count(segment) %>%
  dplyr::mutate(p = round((n/sum(n)), 2)) %>%
  dplyr::group_by(segment)
treemapdf <- as.data.frame(treemapdf)

ggplot(treemapdf, 
       aes(area = n, 
           fill = segment,
           label = paste(segment, percent(p) ,sep="\n"))) +
  geom_treemap() +
  scale_fill_brewer(palette = "PuBu") + 
  geom_treemap_text(colour = "black", 
                    place = "centre", 
                    grow = FALSE) +
  labs(title="Tree map of RFM segmentation", 
       subtitle="Segments and percentage on total number of tourists",
       caption = 'Source: ISTAT') 

rfm_iorgalldf <- iorgall %>% 
  group_by(progind) %>%
  dplyr::mutate(DURATA = round(mean(DURATA), 1),
                TIPOVGG = round(mean(as.numeric(TIPOVGG), na.rm = TRUE), 1),
                DEST_IE = round(mean(as.numeric(DEST_IE), na.rm = TRUE), 1),
                IORGALL = round(mean(as.numeric(IORGALL)), 1),
                piattall2 = round(mean(as.numeric(piattall2), na.rm = TRUE), 1)  )
rfm_iorgalldf <- rfm_iorgalldf[!duplicated(rfm_iorgalldf$progind), ]
rfm_segments_analysis <- merge(dffinale[, c(1,9)],
                               rfm_iorgalldf[, c(1:5, 8, 11, 17, 33)], 
                               by.x = 'customer_id',  
                               by.y = 'progind',
                               all.x = TRUE)

ggplot(rfm_segments_analysis, aes(sesso)) +
  scale_fill_brewer(palette = 'Accent') + 
  geom_bar(aes(fill=eta10)) +
  geom_text(aes(label=..count..),stat="count",vjust = 0,size=3) +
  theme_minimal() +
  scale_x_discrete(labels= c('F', 'M')) +
  theme(axis.text.x = element_text(angle = 90)) +
  labs(fill='age') +
  facet_grid(~segment) +
  labs(title="Bar plot segments' demographic", 
       subtitle="Age and sex on RFM segments",
       caption = 'Source: ISTAT',
       x = 'sex') 

plot_rfm <- rfm_segments_analysis %>%
  dplyr::count(eta10, sesso, segment) %>%
  dplyr::group_by(segment) %>%
  dplyr::mutate(p = n/sum(n)) %>%
  dplyr::group_by(eta10, segment) %>%
  dplyr::mutate(n = sum(n))

ggplot(plot_rfm, aes(x = sesso, y= p, fill = eta10)) +
  scale_fill_brewer(palette = 'Accent') + 
  geom_col(position = "fill") + 
  theme_minimal() +
  labs(fill='age') +
  #geom_label(aes(label = n), position = "fill", color = "white", vjust = 1, show.legend = FALSE) +
  #scale_y_continuous(labels = n) +
  scale_x_discrete(labels= c('F', 'M')) +
  theme(axis.text.x = element_text(angle = 90)) +
  facet_grid(~segment) +
  labs(title="Bar plot of segments' demographic", 
       subtitle="Age and gender on RFM segments",
       caption = 'Source: ISTAT',
       x = 'age',
       y = 'percentage of user')


rfm_int_numericdf <- int_numeric %>%
  group_by(progind) %>%
  dplyr::mutate(DURATA    = round(mean(DURATA), 1),
                TIPOVGG   = round(mean(as.numeric(TIPOVGG),   na.rm = TRUE), 1),
                DEST_IE   = round(mean(as.numeric(DEST_IE),   na.rm = TRUE), 1),
                IORGALL   = round(mean(as.numeric(IORGALL),   na.rm = TRUE), 1),
                piattall2 = round(mean(as.numeric(piattall2), na.rm = TRUE), 1),
                npart     = round(mean(as.numeric(npart),     na.rm = TRUE), 1)  
                )
rfm_int_numericdf <- rfm_int_numericdf[!duplicated(rfm_int_numericdf$progind), ]
rfm_segments_analysis_num <- merge(dffinale[, c(1,3,4,9)],
                               rfm_int_numericdf[, c(1:5, 8, 9, 11, 17, 28, 33)], 
                               by.x = 'customer_id',  
                               by.y = 'progind')
rfm_segments_analysis_num[is.na(rfm_segments_analysis_num)] <- 0
rfm_segments_analysis_num$avg_per_trip <- rfm_segments_analysis_num$amount/rfm_segments_analysis_num$transaction_count

rfm_segments_analysis_num %>%
  group_by(segment) %>%
  tally()

rfm_segments_analysis_num %>%
  select(avg_per_trip, amount, sesso, eta10, istr4, condogg, TIPOVGG, 
         DEST_IE, DURATA, IORGALL, piattall2, segment) %>%
  group_by(segment) %>%
  summarise_all(mean) 



### MERGE DATASET WITH INFO
iorgall <- merge(iorgall,
                 dffinale[, c(1,3,4,9)],
                 by.x = 'progind',
                 by.y = 'customer_id')

# --- #
# Data viz
ggplot(iorgall %>% 
         dplyr::count(segment, sesso, eta10) %>%
         dplyr::group_by(segment, eta10) %>%
         dplyr::mutate(pct=n/sum(n),
                       ypos = cumsum(n) - 0.5*n),  
       aes(eta10, n, fill=sesso)) +
  geom_bar(stat="identity") +
  scale_fill_brewer(palette = 'Set2') + 
  theme_minimal() +
  scale_x_discrete(labels= c('<=24', '25-44', '45-64', '>=65')) +
  theme(axis.text.x = element_text(angle = 90)) +
  labs(fill='sex') +
  facet_grid(~segment) +
  geom_text(aes(label=paste0(sprintf("%1.1f", pct*100),"%")), 
            position=position_stack(vjust=0.5), size=3) +
  labs(title="Bar plot for sex and age for each segment", 
       subtitle="Distribution of socio-demographic feature with respect to RFM scores",
       caption = 'Source: ISTAT',
       x = 'age')
# confirms table
# --- #
int_numeric <- merge(int_numeric,
                     dffinale[, c(1,3,4,9)],
                     by.x = 'progind',
                     by.y = 'customer_id')

##########################################################################
##########################################################################
### Only DURATION, RECENCY, FREQUENCY --> pure numeric variables
# 3 different groups
df_kclus <- merge(int_numeric[, c(1,2,3,4,5,8,9,11,17,28,33)], 
                  dffinale[, c(1:4)], 
                  by.x = 'progind',
                  by.y = 'customer_id')

df_kclus[is.na(df_kclus)] <- 0 # change iorgall and piattall2 into binary

df_kclus2 <- df_kclus[, c(8, 12:14)]
df_kclus2 <- as.data.frame(sapply(df_kclus2, as.numeric))
corrplot(cor(df_kclus2), type="upper", method="ellipse", tl.cex=0.9)

df_kclus_scale <- as.data.frame(scale(df_kclus2))
#df_kclus_scale <- df_kclus2
#best performing number of clusters
set.seed(00)

b <- c()
w <- c()

for(i in 1:10){
  
  # For each k, calculate betweenss, tot.withinss and silhouette
  km <- kmeans(df_kclus_scale, centers=i, nstart = 25)
  b[i] <- km$betweenss
  w[i] <- km$tot.withinss
  
}

avg_sil <- function(k){
  km.res <- kmeans(df_kclus_scale, centers = k, nstart = 25)
  ss <- silhouette(km.res$cluster, dist(df_kclus_scale))
  mean(ss[,3])
}

avg_sil_values <- map_dbl(2:10, avg_sil)

k1 <- qplot(1:10, b, geom=c("point", "line"), 
            xlab="Number of clusters", ylab="Between-cluster sum of squares") +
  scale_x_continuous(breaks=seq(0, 10, 1)) +
  theme_minimal() 

k2 <- qplot(1:10, w, geom=c("point", "line"),
            xlab="Number of clusters", ylab="Total within-cluster sum of squares") +
  scale_x_continuous(breaks=seq(0, 10, 1)) +
  theme_minimal() 

k3 <- qplot(2:10, avg_sil_values, geom=c("point", "line"),
            xlab="Number of clusters", ylab="Average Silhouettes") +
  scale_x_continuous(breaks=seq(1, 10, 1)) +
  theme_minimal() 

# plots with betweeness and tot.withiness for each k --> i need to maximize the 
# first while controlling for the latter
grid.arrange(k1, k2, k3, nrow = 1, 
             top = textGrob("Best number of clusters",
                            gp=gpar(fontsize=15,font=2))) # 6


kmean_alg <- kmeans(df_kclus_scale, centers=3, nstart = 25)
kmean_alg$size
kmean_alg$betweenss
kmean_alg$withinss
kmean_alg$tot.withinss

# Mean values of each cluster
aggregate(df_kclus2, by=list(kmean_alg$cluster), mean)

km_plot <- eclust(df_kclus_scale, "kmeans", k = 3, nstart = 25, graph = FALSE)
fviz_cluster(km_plot, geom = "point", 
             palette = "Set2",
             ellipse.type = "norm", 
             ggtheme = theme_minimal(),
             main = "Cluster plot with k = 3")
respca <- prcomp(df_kclus2,  scale = TRUE)
fviz_pca_ind(respca)
fviz_pca_var(respca)
fviz_pca_var(respca, col.var="contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE) + 
  theme_minimal() + 
  ggtitle("Variables - PCA")

sil <- silhouette(kmean_alg$cluster, dist(df_kclus_scale))
fviz_silhouette(sil, 
                palette = "Set2", 
                ggtheme = theme_classic())
neg_sil_index <- which(sil[, "sil_width"] < 0)
sil[neg_sil_index, , drop = FALSE]

df_kclus$kmean <- kmean_alg$cluster

# check for the entries in wrong group 
errorclu <- df_kclus[neg_sil_index,] #summary of wrongly assigned entries
s <- as.data.frame(sil[neg_sil_index, , drop = FALSE])
errorclu$right <- s$neighbor

as.data.frame(errorclu) %>%
  dplyr::filter(kmean == 3, right == 1) %>%
  dplyr::select(DURATA, recency_days, transaction_count,amount) %>%
  dplyr::summarise_all(mean) 
as.data.frame(df_kclus) %>%
  dplyr::filter(kmean == c(1)) %>%
  dplyr::select(DURATA, recency_days, transaction_count,amount) %>%
  dplyr::summarise_all(mean) 
as.data.frame(df_kclus) %>%
  dplyr::filter(kmean == c(3)) %>%
  dplyr::select(DURATA, recency_days, transaction_count,amount) %>%
  dplyr::summarise_all(mean) 
as.data.frame(df_kclus) %>%
  dplyr::filter(kmean == c(2)) %>%
  dplyr::select(DURATA, recency_days, transaction_count,amount) %>%
  dplyr::summarise_all(mean) 


# ------------------------------------------ #
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

finale_kmeans2 <- df_kclus %>%
  dplyr::group_by(progind) %>%
  dplyr::mutate(kmean = Mode(kmean))

finale_kmeans2 <- merge(id_segment, 
                        df_kclus, 
                        by.x = 'customer_id',  
                        by.y = 'progind')

finale_kmeans2 <- finale_kmeans2[!duplicated(finale_kmeans2$customer_id),]

finale_kmeans2$kmeangroupthree <- mapvalues(finale_kmeans2$kmean,
                                          from=c(1, 2, 3), 
                                          to=c('Group 1',
                                               'Group 2',
                                               'Group 3'))

# wrong assignment
finale_kmeans2[finale_kmeans2$customer_id %in% errorclu$progind, ]


# add kmeans into dataset
iorgall <- merge(iorgall, 
                 finale_kmeans2[, c(1,17)], 
                 by.x = 'progind',
                 by.y = 'customer_id')
int_numeric <- merge(int_numeric, 
                 finale_kmeans2[, c(1,17)], 
                 by.x = 'progind',
                 by.y = 'customer_id')

finale_kmeans2[is.na(finale_kmeans2)] <- 0

finale_kmeans2 %>%
  dplyr::group_by(kmean)%>%
  dplyr::select(sesso, eta10, istr4, condogg, TIPOVGG, DEST_IE, npart, DURATA, 
                recency_days, transaction_count, amount, IORGALL, piattall2) %>%
  dplyr::summarise_all(mean) 

# represents different segments inside k-means groups
# percentage
ggplot(finale_kmeans2 %>% 
         dplyr::count(segment, kmeangroupthree) %>%
         dplyr::group_by(segment) %>%
         dplyr::mutate(pct=n/sum(n),
                       ypos = cumsum(n) - 0.5*n),  
       aes(segment, n, fill=kmeangroupthree)) +
  geom_bar(stat="identity") +
  scale_fill_brewer(palette = 'Pastel1') + 
  scale_x_discrete(labels= c('One time\n cheap', "Cheap\n Globet.",
                             "One time\n avg.", "Avg.Globet.",
                             "One time\n lux.","Lux.Globet.")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90)) +
  labs(fill='k-means groups') +
  geom_text(aes(label=paste0(sprintf("%1.1f", pct*100),"%")), 
            position=position_stack(vjust=0.5), size=3) +
  labs(title="Bar plot for RFM and K-means", 
       subtitle="Composition of RFM segments with respect to k-means partitioning",
       caption = 'Source: ISTAT',
       x = 'RFM',
       y = 'count')


ggplot(finale_kmeans2 %>% 
         dplyr::count(segment, kmeangroupthree) %>%
         dplyr::group_by(kmeangroupthree) %>%
         dplyr::mutate(pct=n/sum(n),
                       ypos = cumsum(n) - 0.5*n),  
       aes(kmeangroupthree, n, fill=segment)) +
  geom_bar(stat="identity") +
  scale_fill_brewer(palette = 'Pastel1') + 
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90)) +
  labs(fill='RFM') +
  geom_text(aes(label=paste0(sprintf("%1.1f", pct*100),"%")), 
            position=position_stack(vjust=0.5), size=3) +
  labs(title="Bar plot for RFM and K-means", 
       subtitle="Composition of k-means groups with respect to RFM scores",
       caption = 'Source: ISTAT',
       x = 'k-means groups',
       y = 'count')


# eda on socio-demo per groups
ggplot(finale_kmeans2 %>%
         dplyr::count(eta10, sesso, kmeangroupthree) %>%
         dplyr::group_by(eta10) %>%
         dplyr::mutate(pct=n/sum(n),
                       ypos = cumsum(n) - 0.5*n),
       aes(as.factor(eta10), n, fill=as.factor(sesso))) +
  geom_bar(stat="identity") +
  scale_x_discrete(labels= c(' <=24  ', ' 25-44  ', ' 45-64  ', ' >=65  ')) +
  scale_fill_brewer(palette = 'Set2') + 
  theme_minimal() +
  facet_grid(~kmeangroupthree) +
  theme(axis.text.x = element_text(angle = 90)) +
  labs(fill='Gender') +
  geom_text(aes(label=n), # 
            position=position_stack(vjust=0.5), size=3) +
  labs(title="Bar plot for age and gender in k-means group", 
       subtitle="Composition of k-means groups with respect to age and gender",
       caption = 'Source: ISTAT',
       x = 'age',
       y = 'count')

ggplot(finale_kmeans2 %>%
         dplyr::count(istr4, sesso, kmeangroupthree) %>%
         dplyr::group_by(kmeangroupthree, istr4) %>%
         dplyr::mutate(pct=n/sum(n),
                       ypos = cumsum(n) - 0.5*n),
       aes(as.factor(istr4), n, fill=as.factor(sesso))) +
  geom_bar(stat="identity") +
  scale_x_discrete(labels= c('No tit.', 
                             'Middle S.', 
                             'High S.', 
                             'Grad')) +
  scale_fill_brewer(palette = 'Set2') + 
  theme_minimal() +
  facet_grid(~kmeangroupthree) +
  theme(axis.text.x = element_text(angle = 90)) +
  labs(fill='Gender') +
  geom_text(aes(label=n), #paste0(sprintf("%1.1f", pct*100),"%") 
            position=position_stack(vjust=0.5), size=3) +
  labs(title="Bar plot for education level and gender in k-means group", 
       subtitle="Composition of k-means groups with respect to education and gender",
       caption = 'Source: ISTAT',
       x = 'edu level',
       y = 'count')

ggplot(finale_kmeans2 %>%
         dplyr::count(condogg, sesso, kmeangroupthree) %>%
         dplyr::group_by(kmeangroupthree, condogg) %>%
         dplyr::mutate(pct=n/sum(n),
                       ypos = cumsum(n) - 0.5*n),
       aes(as.factor(condogg), n, fill=as.factor(sesso))) +
  geom_bar(stat="identity") +
  scale_x_discrete(labels= c('Not working', 'Working')) +
  scale_fill_brewer(palette = 'Set2') + 
  theme_minimal() +
  facet_grid(~kmeangroupthree) +
  theme(axis.text.x = element_text(angle = 90)) +
  labs(fill='Gender') +
  geom_text(aes(label=n), # paste0(sprintf("%1.1f", pct*100),"%")
            position=position_stack(vjust=0.5), size=3) +
  labs(title="Bar plot for working status and gender in k-means group", 
       subtitle="Composition of k-means groups with respect to working status and gender",
       caption = 'Source: ISTAT',
       x = 'working status',
       y = 'count')



##########################################################################
######################### CATEGORICAL DATA ###############################
##########################################################################

# HIERARCHICAL METHOD # --> k means is more efficient
# First, subset with the variable i want to consider
# I want a dataset with a single obs for individual
# First, when possible, I keep variable as overall mean
df_clus <- iorgall[, c(1:5,8,9,11,17,33,35,36,37,38,39)] %>%
  dplyr::group_by(progind) %>%
  dplyr::mutate(DURATA    = round(mean(DURATA), 1),
                #npart     = round(mean(npart), 1),
                TIPOVGG   = Mode(TIPOVGG),
                IORGALL   = Mode(IORGALL), 
                piattall2 = Mode(piattall2))
df_clus[is.na(df_clus)] <- 0

# FOR THE ONE WITH npart
df_hierclus <- df_clus

df_hierclus <- df_hierclus[!duplicated(df_hierclus), ]
df_hierclus$progind <- NULL
df_hierclus$TIPOVGG <- ifelse(df_hierclus$TIPOVGG == '1', 
                              'Holiday', 
                              'Bussiness Trip') 
df_hierclus$TIPOVGG <- as.factor(df_hierclus$TIPOVGG)

df_hierclus$piattall2[is.na(df_hierclus$piattall2)] <- 0
df_hierclus$piattall2 <- as.factor(ifelse(df_hierclus$piattall2 == 1, 'Yes', 'No'))
df_hierclus$IORGALL   <- as.factor(ifelse(df_hierclus$IORGALL   == 1, 'Yes', 'No'))
df_hierclus$DEST_IE   <- as.factor(ifelse(df_hierclus$DEST_IE   == 1, 'Italy', 'Abroad'))

df_hierclus$condogg[is.na(df_hierclus$condogg)] <- 0
df_hierclus$condogg <- as.factor(ifelse(df_hierclus$condogg     == 1, 'Work', 'NoWork'))

# sex is changed into a factor
df_hierclus$sesso <- as.factor(df_hierclus$sesso)


df_hierclus <- subset(df_hierclus, select = -c(IORGALL,
                                               piattall2,
                                               segment,
                                               kmeangroupthree))
# --------------------------------- #
### Hierarchical CLUSTER with GOWER DISTANCE

# complute the distance matrix in Gower's
df_dist <- daisy(df_hierclus, metric = "gower")
# HELP: “Gower's distance” is chosen by metric "gower" or automatically if some columns
# of x are not numeric. Also known as Gower's coefficient (1971), 
# expressed as a dissimilarity, this implies that a particular standardisation will 
# be applied to each variable, and the “distance” between two units is the sum
# of all the variable-specific distances, see the details section.
# https://dpmartin42.github.io/posts/r/cluster-mixed-types --> how it works
# check for the best number of cluster with best linkage method
set.seed(1)

#linkage <- c( "average", "single", "complete", "ward.D2")
linkage <- c( "average", "ward.D2")

for (i in (1:length(linkage))){
  
  mtd <- linkage[i]
  # average produce very unbalanced clusters
  # single linkage is completely unbalanced: for k = 4: 2276,2,3,1
  # complete prob the best choice is 6:
  # ward.D2 4 or 6, in both cases better performance than complete linkage
  hc_clust_loop <- hclust(df_dist, method = mtd)
  
  for (i in 2:10){
    statistics <- unlist(cluster.stats(d = df_dist, 
                                       clustering = cutree(hc_clust_loop,
                                                           k = i)))[c("cluster.number",
                                                                      "average.between",
                                                                      "average.within",
                                                                      "within.cluster.ss",
                                                                      "avg.silwidth")]
    clusters_size <- unlist(cluster.stats(d = df_dist, 
                                          clustering = cutree(hc_clust_loop,
                                                              k = i)))[c("cluster.size1",
                                                                         "cluster.size2",       
                                                                         "cluster.size3",
                                                                         "cluster.size4",
                                                                         "cluster.size5",
                                                                         "cluster.size6",
                                                                         "cluster.size7",
                                                                         "cluster.size8",
                                                                         "cluster.size9",
                                                                         "cluster.size10")]
    
    cmd <- sprintf("For %i clusters statistics with %s linkage method are as follow", i, mtd)
    print(eval(cmd))  
    print(statistics)
    
    print('Cluster size:')
    print(clusters_size)
    print(' ')
  }
}

clusters_size6 <- unlist(cluster.stats(d = df_dist, 
                                      clustering = cutree(hclust(df_dist, method = "ward.D2"),
                                                          k = 3)))[c("cluster.size1",
                                                                     "cluster.size2",       
                                                                     "cluster.size3",
                                                                     "cluster.size4",
                                                                     "cluster.size5",
                                                                     "cluster.size6")]
clusters_size4 <- unlist(cluster.stats(d = df_dist, 
                                       clustering = cutree(hclust(df_dist, method = "ward.D2"),
                                                           k = 4)))[c("cluster.size1",
                                                                      "cluster.size2",       
                                                                      "cluster.size3",
                                                                      "cluster.size4"
                                                                      )]
clusters_size3
clusters_size4
# difference between 4 and 6 is that 'group 1' in the second case counts 
# groups 1, 2 and 6 of the other
# 6 groups capture the age difference, 4 do not
set.seed(1)
defalt_par <- par()
par(lwd=3.5)
hc_clust <- hclust(df_dist, method = "ward.D2")
plot(hc_clust, hang = -1, lwd = 1.5)
rect.hclust(hc_clust, k = 4, border = 2:5)
rect.hclust(hc_clust, k = 3, border = 2:4)
w_cut4 <- cutree(hc_clust, k = 4)
w_cut6 <- cutree(hc_clust, k = 3)
par(defalt_par)

# cut4
w_cut4 <- cutree(hc_clust, k = 4)
df_clus$hc <- w_cut4
df_clus_fin <- df_clus %>%
  dplyr::group_by(progind) %>%
  dplyr::mutate(hc = Mode(hc))

#df_clus <- df_clus[!duplicated(df_clus$progind),]

iorgall$hc <- df_clus_fin$hc
iorgall$hcgroup <- mapvalues(iorgall$hc,
                             from=c(1, 2, 3, 4),
                             to=c('Group 1','Group 2','Group 3', 'Group 4'))
int_numeric$hc <- df_clus_fin$hc
int_numeric$hcgroup <- mapvalues(int_numeric$hc,
                                 from=c(1, 2, 3, 4),
                                 to=c('Group 1','Group 2','Group 3', 'Group 4'))


int_numeric[is.na(int_numeric)] <- 0
int_numeric %>%
  dplyr::group_by(hc)%>%
  dplyr::select(sesso, eta10, istr4, condogg, TIPOVGG, DEST_IE, DURATA, recency, 
                transaction_count, amount, IORGALL, piattall2) %>%
  dplyr::summarise_all(mean) 
int_numeric %>%
  dplyr::group_by(hc)%>%
  tally()


# represents different segments inside k-means groups
# percentage
ggplot(iorgall %>% 
         dplyr::count(segment, hcgroup) %>%
         dplyr::group_by(segment) %>%
         dplyr::mutate(pct=n/sum(n),
                       ypos = cumsum(n) - 0.5*n),  
       aes(segment, n, fill=hcgroup)) +
  geom_bar(stat="identity") +
  scale_fill_brewer(palette = 'Pastel1') + 
  scale_x_discrete(labels= c('One time\n cheap', "Cheap\n Globet.",
                             "One time\n avg.", "Avg.Globet.",
                             "One time\n lux.","Lux.Globet.")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90)) +
  labs(fill='hc groups') +
  geom_text(aes(label=paste0(sprintf("%1.1f", pct*100),"%")), 
            position=position_stack(vjust=0.5), size=3) +
  labs(title="Bar plot for RFM and HC partitioning", 
       subtitle="Composition of RFM segments with respect to hierarchical clustering",
       caption = 'Source: ISTAT',
       x = 'RFM')

ggplot(iorgall %>% 
         dplyr::count(kmeangroupthree, hcgroup) %>%
         dplyr::group_by(hcgroup) %>%
         dplyr::mutate(pct=n/sum(n),
                       ypos = cumsum(n) - 0.5*n),  
       aes(hcgroup, n, fill=kmeangroupthree)) +
  geom_bar(stat="identity") +
  scale_fill_brewer(palette = 'Pastel1') + 
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90)) +
  labs(fill='RFM') +
  geom_text(aes(label=paste0(sprintf("%1.1f", pct*100),"%")), 
            position=position_stack(vjust=0.5), size=3) +
  labs(title="Bar plot for RFM and hc", 
       subtitle="Composition of hc groups with respect to RFM scores",
       caption = 'Source: ISTAT',
       x = 'hc groups')

ggplot(iorgall %>% 
         dplyr::count(segment, hcgroup) %>%
         dplyr::group_by(hcgroup) %>%
         dplyr::mutate(pct=n/sum(n),
                       ypos = cumsum(n) - 0.5*n),  
       aes(hcgroup, n, fill=segment)) +
  geom_bar(stat="identity") +
  scale_fill_brewer(palette = 'Pastel1') + 
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90)) +
  labs(fill='RFM') +
  geom_text(aes(label=paste0(sprintf("%1.1f", pct*100),"%")), 
            position=position_stack(vjust=0.5), size=3) +
  labs(title="Bar plot for RFM and hc", 
       subtitle="Composition of hc groups with respect to RFM scores",
       caption = 'Source: ISTAT',
       x = 'hc groups')



# eda on socio-demo per groups
ggplot(iorgall %>%
         dplyr::count(eta10, sesso, hcgroup) %>%
         dplyr::group_by(eta10) %>%
         dplyr::mutate(pct=n/sum(n),
                       ypos = cumsum(n) - 0.5*n),
       aes(as.factor(eta10), n, fill=as.factor(sesso))) +
  geom_bar(stat="identity") +
  scale_x_discrete(labels= c(' <=24  ', ' 25-44  ', ' 45-64  ', ' >=65  ')) +
  scale_fill_brewer(palette = 'Set2') + 
  theme_minimal() +
  facet_grid(~hcgroup) +
  theme(axis.text.x = element_text(angle = 90)) +
  labs(fill='Gender') +
  geom_text(aes(label=n), # 
            position=position_stack(vjust=0.5), size=3) +
  labs(title="Bar plot for age and gender in hc group", 
       subtitle="Composition of hierarchical clusters with respect to age and gender",
       caption = 'Source: ISTAT',
       x = 'age',
       y = 'count')

ggplot(iorgall %>%
         dplyr::count(istr4, sesso, hcgroup) %>%
         dplyr::group_by(hcgroup, istr4) %>%
         dplyr::mutate(pct=n/sum(n),
                       ypos = cumsum(n) - 0.5*n),
       aes(as.factor(istr4), n, fill=as.factor(sesso))) +
  geom_bar(stat="identity") +
  scale_x_discrete(labels= c('No tit.', 
                             'Middle s.', 
                             'High s.', 
                             'Grad')) +
  scale_fill_brewer(palette = 'Set2') + 
  theme_minimal() +
  facet_grid(~hcgroup) +
  theme(axis.text.x = element_text(angle = 90)) +
  labs(fill='Gender') +
  geom_text(aes(label=n), #paste0(sprintf("%1.1f", pct*100),"%") 
            position=position_stack(vjust=0.5), size=3) +
  labs(title="Bar plot for education level and gender in hc group", 
       subtitle="Composition of hierarchical cluster with respect to education and gender",
       caption = 'Source: ISTAT',
       x = 'edu level',
       y = 'count')

ggplot(iorgall %>%
         dplyr::count(condogg, sesso, hcgroup) %>%
         dplyr::group_by(hcgroup, condogg) %>%
         dplyr::mutate(pct=n/sum(n),
                       ypos = cumsum(n) - 0.5*n),
       aes(as.factor(condogg), n, fill=as.factor(sesso))) +
  geom_bar(stat="identity") +
  scale_x_discrete(labels= c('Not working', 'Working')) +
  scale_fill_brewer(palette = 'Set2') + 
  theme_minimal() +
  facet_grid(~hcgroup) +
  theme(axis.text.x = element_text(angle = 90)) +
  labs(fill='Gender') +
  geom_text(aes(label=n), # paste0(sprintf("%1.1f", pct*100),"%")
            position=position_stack(vjust=0.5), size=3) +
  labs(title="Bar plot for working status and gender in hc group", 
       subtitle="Composition of hierarchical clusters with respect to working status and gender",
       caption = 'Source: ISTAT',
       x = 'working status',
       y = 'count')









# cut3
w_cut3 <- cutree(hc_clust, k = 3)
df_clus$hc <- w_cut3
df_clus_fin <- df_clus %>%
  dplyr::group_by(progind) %>%
  dplyr::mutate(hc = Mode(hc))

#df_clus <- df_clus[!duplicated(df_clus$progind),]

iorgall$hc <- df_clus_fin$hc
iorgall$hcgroup <- mapvalues(iorgall$hc,
                             from=c(1, 2, 3),
                             to=c('Group 1','Group 2','Group 3'))
int_numeric$hc <- df_clus_fin$hc
int_numeric$hcgroup <- mapvalues(int_numeric$hc,
                                 from=c(1, 2, 3),
                                 to=c('Group 1','Group 2','Group 3'))

int_numeric[is.na(int_numeric)] <- 0
int_numeric %>%
  dplyr::group_by(hc)%>%
  dplyr::select(sesso, eta10, istr4, condogg, TIPOVGG, DEST_IE, DURATA, recency, 
                transaction_count, amount, IORGALL, piattall2) %>%
  dplyr::summarise_all(mean) 
int_numeric %>%
  dplyr::group_by(hc)%>%
  tally()


# represents different segments inside k-means groups
# percentage
ggplot(iorgall %>% 
         dplyr::count(segment, hcgroup) %>%
         dplyr::group_by(segment) %>%
         dplyr::mutate(pct=n/sum(n),
                       ypos = cumsum(n) - 0.5*n),  
       aes(segment, n, fill=hcgroup)) +
  geom_bar(stat="identity") +
  scale_fill_brewer(palette = 'Pastel1') + 
  scale_x_discrete(labels= c('One time\n cheap', "Cheap\n Globet.",
                             "One time\n avg.", "Avg.Globet.",
                             "One time\n lux.","Lux.Globet.")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90)) +
  labs(fill='hc groups') +
  geom_text(aes(label=paste0(sprintf("%1.1f", pct*100),"%")), 
            position=position_stack(vjust=0.5), size=3) +
  labs(title="Bar plot for RFM and HC partitioning", 
       subtitle="Composition of RFM segments with respect to hierarchical clustering",
       caption = 'Source: ISTAT',
       x = 'RFM')

ggplot(iorgall %>% 
         dplyr::count(kmeangroupthree, hcgroup) %>%
         dplyr::group_by(hcgroup) %>%
         dplyr::mutate(pct=n/sum(n),
                       ypos = cumsum(n) - 0.5*n),  
       aes(hcgroup, n, fill=kmeangroupthree)) +
  geom_bar(stat="identity") +
  scale_fill_brewer(palette = 'Pastel1') + 
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90)) +
  labs(fill='RFM') +
  geom_text(aes(label=paste0(sprintf("%1.1f", pct*100),"%")), 
            position=position_stack(vjust=0.5), size=3) +
  labs(title="Bar plot for RFM and hc", 
       subtitle="Composition of hc groups with respect to RFM scores",
       caption = 'Source: ISTAT',
       x = 'hc groups')

ggplot(iorgall %>% 
         dplyr::count(segment, hcgroup) %>%
         dplyr::group_by(hcgroup) %>%
         dplyr::mutate(pct=n/sum(n),
                       ypos = cumsum(n) - 0.5*n),  
       aes(hcgroup, n, fill=segment)) +
  geom_bar(stat="identity") +
  scale_fill_brewer(palette = 'Pastel1') + 
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90)) +
  labs(fill='RFM') +
  geom_text(aes(label=paste0(sprintf("%1.1f", pct*100),"%")), 
            position=position_stack(vjust=0.5), size=3) +
  labs(title="Bar plot for RFM and hc", 
       subtitle="Composition of hc groups with respect to RFM scores",
       caption = 'Source: ISTAT',
       x = 'hc groups')


##### Choice of accommodation #####
ggplot(iorgall %>%
         dplyr::count(ALLOG, hcgroup),
       aes(as.factor(ALLOG), n, fill = ALLOG)) +
  geom_bar(stat="identity") +
  scale_x_discrete(labels= c('Other', 'Entire \nApt.', 'Hotel \nroom', 'Private \nroom')) +
  scale_fill_brewer(palette = 'Spectral') + 
  theme_minimal() +
  facet_grid(~hcgroup) +
  theme(axis.text.x = element_text(angle = 90)) +
  theme(legend.position = "none") +
  geom_text(aes(label=n), # paste0(sprintf("%1.1f", pct*100),"%")
            position=position_stack(vjust=0.5), size=3) +
  labs(title="Choice of accommodation according to HC groups", 
       subtitle="Groups according to socio-dem grouping",
       caption = 'Source: ISTAT',
       x = 'room type',
       y = 'count')


ggplot(iorgall %>%
         dplyr::count(ALLOG, kmeangroupthree),
       aes(as.factor(ALLOG), n, fill = ALLOG)) +
  geom_bar(stat="identity") +
  scale_x_discrete(labels= c('Other', 'Entire \nApt.', 'Hotel \nroom', 'Private \nroom')) +
  scale_fill_brewer(palette = 'Spectral') + 
  theme_minimal() +
  facet_grid(~kmeangroupthree) +
  theme(axis.text.x = element_text(angle = 90)) +
  theme(legend.position = "none") +
  geom_text(aes(label=n), # paste0(sprintf("%1.1f", pct*100),"%")
            position=position_stack(vjust=0.5), size=3) +
  labs(title="Choice of accommodation according to K groups", 
       subtitle="Groups according to k-mean cluster",
       caption = 'Source: ISTAT',
       x = 'room type',
       y = 'count')


ggplot(iorgall %>%
         dplyr::count(ALLOG, segment),
       aes(as.factor(ALLOG), n, fill = ALLOG)) +
  geom_bar(stat="identity") +
  scale_x_discrete(labels= c('Other', 'Entire Apt.', 'Hotel room', 'Private room')) +
  scale_fill_brewer(palette = 'Spectral') + 
  theme_minimal() +
  facet_grid(~segment) +
  theme(axis.text.x = element_text(angle = 90)) +
  theme(legend.position = "none") +
  geom_text(aes(label=n), # paste0(sprintf("%1.1f", pct*100),"%")
            position=position_stack(vjust=0.5), size=3) +
  labs(title="Choice of accommodation according to RFM groups", 
       subtitle="Groups according to RFM",
       caption = 'Source: ISTAT',
       x = 'room type',
       y = 'count')



#### Type dest ####

ggplot(iorgall %>%
         dplyr::count(TIPOCITTA, hcgroup),
       aes(as.factor(TIPOCITTA), n, fill = as.factor(TIPOCITTA))) +
  geom_bar(stat="identity") +
  scale_x_discrete(labels= c('City', 'Other', 'No info')) +
  scale_fill_brewer(palette = 'YlGnBu') + 
  theme_minimal() +
  facet_grid(~hcgroup) +
  theme(axis.text.x = element_text(angle = 90)) +
  theme(legend.position = "none") +
  geom_text(aes(y = 0, label = n), # paste0(sprintf("%1.1f", pct*100),"%")
            position=position_dodge(width = .9),
            size=2) +
  labs(title="Choice of accommodation according to HC groups", 
       subtitle="Groups according to socio-dem grouping",
       caption = 'Source: ISTAT',
       x = 'room type',
       y = 'count')


ggplot(iorgall %>%
         dplyr::count(TIPOCITTA, kmeangroupthree),
       aes(as.factor(TIPOCITTA), n, fill = as.factor(TIPOCITTA))) +
  geom_bar(stat="identity") +
  scale_x_discrete(labels= c('City', 'Other', 'No info')) +
  scale_fill_brewer(palette = 'YlGnBu') + 
  theme_minimal() +
  facet_grid(~kmeangroupthree) +
  theme(axis.text.x = element_text(angle = 90)) +
  theme(legend.position = "none") +
  geom_text(aes(y = 0, label = n), # paste0(sprintf("%1.1f", pct*100),"%")
            position=position_dodge(width = .9),
            size=2) +
  labs(title="Choice of destination according to K groups", 
       subtitle="Groups according to k-mean cluster",
       caption = 'Source: ISTAT',
       x = 'room type',
       y = 'count')


ggplot(iorgall %>%
         dplyr::count(TIPOCITTA, segment),
       aes(as.factor(TIPOCITTA), n, fill = as.factor(TIPOCITTA))) +
  geom_bar(stat="identity") +
  scale_x_discrete(labels= c('City', 'Other', 'No info')) +
  scale_fill_brewer(palette = 'Set2') + 
  theme_minimal() +
  facet_grid(~segment) +
  theme(axis.text.x = element_text(angle = 90)) +
  theme(legend.position = "none") +
  geom_text(aes(y = 0, label = n), # paste0(sprintf("%1.1f", pct*100),"%")
            position=position_dodge(width = .9),
            size=2) +
  labs(title="Choice of accommodation according to RFM groups", 
       subtitle="Groups according to RFM",
       caption = 'Source: ISTAT',
       x = 'room type',
       y = 'count')


### REFERENCES

# https://rpubs.com/omerperach/RFM_IDC
# https://www.kaggle.com/hendraherviawan/customer-segmentation-using-rfm-analysis-r
# segments
# https://medium.com/@triimamwicaksono_47213/customer-segmentation-and-strategy-using-rfm-analysis-in-rstudio-be79118c8235
# https://towardsdatascience.com/find-your-best-customers-with-customer-segmentation-in-python-61d602f9eee6

# https://uc-r.github.io/kmeans_clustering#fn:kauf
# https://lukedaniels1.github.io/Bio381_2018/Daniels_Cluster_Analysis_Lecture.html
# https://www.datanovia.com/en/lessons/cluster-validation-statistics-must-know-methods/#dunn-index

# https://rstudio-pubs-static.s3.amazonaws.com/423873_adfdb38bce8d47579f6dc916dd67ae75.html#mixed-numericcategorical-data
# https://towardsdatascience.com/hierarchical-clustering-on-categorical-data-in-r-a27e578f2995
# https://uc-r.github.io/hc_clustering
# https://dpmartin42.github.io/posts/r/cluster-mixed-types

# -- #
# https://nextjournal.com/pc-methods/hierarchical-clustering-pcs
# https://www.linkedin.com/pulse/automated-exploratory-data-analysis-r-xander-horn/



