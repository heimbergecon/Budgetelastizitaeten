rm(list = ls()) #clear list

#load packages
library(xlsx)
library(calibrate) 
library (stargazer)
library(sandwich)
library(lmtest)
library(getopt)
library(CausalGAM)
library(ggplot2)
library(reshape2)
library(xts)
library(lattice)
library(gridExtra)
library(gtable)
library(plm)
library(lfe)
library(lmtest)
library(car)
library(tis)
library(foreign)
library(MASS)
library(quantreg)
library(ggrepel)
library(dplyr)
library(stringr)
library(ggplot2)
library(datasets)
library(rio)
library(psych)
library(systemfit)
library(foreign)
library(MatchIt)
library(CRTgeeDR)
library(eurostat)
library(plyr)
library(zoo)
library(ggthemes)
library("robumeta")
library("metafor")
library("dplyr")
library(clubSandwich)
library(Hmisc)
library(metafor)
library(pracma)
library(broom)
library(sjPlot)
library(here)
library(data.table)
library(countrycode)
library(nlme)
library(tidyr)

#read data:
data <- fread(here("Dataset_output_ec.csv"), header=TRUE)
#take Data from year 1991 onward, because 1990 is NA in OECD data
Germany <- subset(data, year %in% c('1991', '1992', '1993', '1994', '1995', '1996', '1997', '1998', '1999','2000','2001','2002','2003', '2004', '2005', '2006', '2007', '2008', '2009', '2010', '2011', '2012', '2013'))
#Alternative option: take data from 1992 onward:
#Germany <- subset(data, year %in% c('1992', '1993', '1994', '1995', '1996', '1997', '1998', '1999','2000','2001','2002','2003', '2004', '2005', '2006', '2007', '2008', '2009', '2010', '2011', '2012', '2013'))


#### Prepare Data ###

#output gap
Germany$GDPV_GDPVTR <- Germany$deu_gdpv / Germany$deu_gdpvtr
Germany$log_GDPV_GDPVTR <- log(Germany$GDPV_GDPVTR)
Germany <- Germany %>% 
  mutate(diff_log_GDPV_GDPVTR = log_GDPV_GDPVTR - lag(log_GDPV_GDPVTR)) %>% 
  ungroup
Germany$lag_GDPV_GDPVTR <- lag(Germany$deu_gdpv) / lag(Germany$deu_gdpvtr)
Germany$log_lag_GDPV_GDPVTR <- log(Germany$lag_GDPV_GDPVTR)

#divide wages through nominal potential output: wsss_gdptr
Germany$wsss_GDPTR <- Germany$deu_wsss / Germany$deu_gdptr
Germany$lag_wsss_GDPTR <- lag(Germany$wsss_GDPTR)
Germany$log_lag_wsss_GDPTR <- log(Germany$lag_wsss_GDPTR)
Germany$log_wsss_GDPTR <- log(Germany$wsss_GDPTR)
Germany <- Germany %>% 
  mutate(diff_log_wsss_GDPTR = log_wsss_GDPTR - lag(log_wsss_GDPTR)) %>% 
  ungroup
Germany$lag_diff_log_wsss_GDPTR <- lag(Germany$diff_log_wsss_GDPTR)

#wsshg
Germany$lag_wsshg <- lag(Germany$deu_wsshg)
Germany$log_lag_wsshg <- log(Germany$lag_wsshg)
Germany$log_wsshg <- log(Germany$deu_wsshg)
Germany <- Germany %>% 
  mutate(diff_log_wsshg = log_wsshg - lag(log_wsshg)) %>% 
  ungroup
Germany$lag_diff_log_wsshg <- lag(Germany$diff_log_wsshg)

#YSE
Germany$lag_yseg <- lag(Germany$deu_yseg)
Germany$log_lag_yseg <- log(Germany$lag_yseg)
Germany$log_yseg <- log(Germany$deu_yseg)
Germany <- Germany %>% 
  mutate(diff_log_yseg = log_yseg - lag(log_yseg)) %>% 
  ungroup
Germany$lag_diff_log_yseg <- lag(Germany$diff_log_yseg)

#divide through nominal potential output: yse_gdptr
Germany$yse_GDPTR <- Germany$deu_yse / Germany$deu_gdptr
Germany$lag_yse_GDPTR <- lag(Germany$yse_GDPTR)
Germany$log_lag_yse_GDPTR <- log(Germany$lag_yse_GDPTR)
Germany$log_yse_GDPTR <- log(Germany$yse_GDPTR)
Germany <- Germany %>% 
  mutate(diff_log_yse_GDPTR = log_yse_GDPTR - lag(log_yse_GDPTR)) %>% 
  ungroup
Germany$lag_diff_log_yse_GDPTR <- lag(Germany$diff_log_yse_GDPTR)

#GOS
Germany$lag_gosbg <- lag(Germany$deu_gosbg)
Germany$log_lag_gosbg <- log(Germany$lag_gosbg)
Germany$log_gosbg <- log(Germany$deu_gosbg)
Germany <- Germany %>% 
  mutate(diff_log_gosbg = log_gosbg - lag(log_gosbg)) %>% 
  ungroup
Germany$lag_diff_log_gosbg <- lag(Germany$diff_log_gosbg)

#divide through nominal potential output: gosb_gdptr
Germany$gosb_GDPTR <- Germany$deu_gosb / Germany$deu_gdptr
Germany$lag_gosb_GDPTR <- lag(Germany$gosb_GDPTR)
Germany$log_lag_gosb_GDPTR <- log(Germany$lag_gosb_GDPTR)
Germany$log_gosb_GDPTR <- log(Germany$gosb_GDPTR)
Germany <- Germany %>% 
  mutate(diff_log_gosb_GDPTR = log_gosb_GDPTR - lag(log_gosb_GDPTR)) %>% 
  ungroup
Germany$lag_diff_log_gosb_GDPTR <- lag(Germany$diff_log_gosb_GDPTR)

#ypeg
Germany$lag_ypeg <- lag(Germany$deu_ypeg)
Germany$log_lag_ypeg <- log(Germany$lag_ypeg)
Germany$log_ypeg <- log(Germany$deu_ypeg)
Germany <- Germany %>% 
  mutate(diff_log_ypeg = log_ypeg - lag(log_ypeg)) %>% 
  ungroup
Germany$lag_diff_log_ypeg <- lag(Germany$diff_log_ypeg)

#divide through nominal potential output: ype_gdptr
Germany$ype_GDPTR <- Germany$deu_ype / Germany$deu_gdptr
Germany$lag_ype_GDPTR <- lag(Germany$ype_GDPTR)
Germany$log_lag_ype_GDPTR <- log(Germany$lag_ype_GDPTR)
Germany$log_ype_GDPTR <- log(Germany$ype_GDPTR)
Germany <- Germany %>% 
  mutate(diff_log_ype_GDPTR = log_ype_GDPTR - lag(log_ype_GDPTR)) %>% 
  ungroup
Germany$lag_diff_log_ype_GDPTR <- lag(Germany$diff_log_ype_GDPTR)

#UNR
Germany$lag_unr <- lag(Germany$deu_unr)
Germany$log_lag_unr <- log(Germany$lag_unr)
Germany$log_unr <- log(Germany$deu_unr)
Germany <- Germany %>% 
  mutate(diff_log_unr = log_unr - lag(log_unr)) %>% 
  ungroup
Germany$lag_diff_log_unr <- lag(Germany$diff_log_unr)

#additional estimates: DZ, KOM, BMWK
#PO
data_add <- fread(here("Different PO estimates Spring 2022.csv"), header=TRUE)
Germany_PO <- subset(data_add, year %in% c('1991', '1992', '1993', '1994', '1995', '1996', '1997', '1998', '1999','2000','2001','2002','2003', '2004', '2005', '2006', '2007', '2008', '2009', '2010', '2011', '2012', '2013'))

#NAWRU
diff_NAWRU <- fread(here("Different NAWRU estimates Spring 2022.csv"), header=TRUE)
diff_NAWRU <- subset(diff_NAWRU, year %in% c('1991', '1992', '1993', '1994', '1995', '1996', '1997', '1998', '1999','2000','2001','2002','2003', '2004', '2005', '2006', '2007', '2008', '2009', '2010', '2011', '2012', '2013'))
diff_NAWRU <- select(diff_NAWRU, ccode, year, KOM, BMWK, DZ)
colnames(diff_NAWRU) <- c('ccode', 'year', 'NAWRU_KOM', 'NAWRU_BMWK', 'NAWRU_DZ')
diff_NAWRU$year <- as.character(diff_NAWRU$year)

#save real values
Germany_PO$KOM_real <- Germany_PO$KOM
Germany_PO$BMWK_real <- Germany_PO$BMWK
Germany_PO$DZ_real <- Germany_PO$DZ
Germany_PO$Nominal_GDP_KOM_real <- Germany_PO$Real_GDP_KOM

#nominal values
Germany_PO$KOM <- Germany_PO$KOM * Germany_PO$GDP_Deflator / 100
Germany_PO$BMWK <- Germany_PO$BMWK * Germany_PO$GDP_Deflator / 100
Germany_PO$DZ <- Germany_PO$DZ * Germany_PO$GDP_Deflator / 100
Germany_PO$Nominal_GDP_KOM <- Germany_PO$Real_GDP_KOM * Germany_PO$GDP_Deflator / 100
Germany_PO$OECD <- Germany_PO$OECD * Germany_PO$GDP_Deflator / 100

df <- Germany_PO %>%
  select(year, KOM, BMWK, DZ, OECD) %>%
  gather(key = "Institution", value = "value", -year)
head(df)

plot_diff_PO <- ggplot(df, aes(x = year, y = value)) + 
  geom_line(aes(color = Institution, linetype = Institution)) + 
  labs(title = "")+
  scale_color_manual(values = c("darkred", "steelblue", "black", "red"))+
  labs(title = "")+
  ggtitle("Produktionspotenzial:\n Unterschiedliche Schätzungen im Frühjahr 2022\n Zeitraum 1991-2013")+
  theme(panel.background = element_rect(fill = "white")) +
  xlab("Jahr") +
  ylab("nominales Produktionspotenzial (zu konstanten Preisen)") +
  theme(title=element_text(size=12, face='bold'))+
  theme(axis.text.x=element_text(size=14))+
  theme(axis.title.x=element_text(size=14)) +
  theme(axis.text.y=element_text(size=10))+
  theme(axis.title.y=element_text(size=10))
plot_diff_PO

#save plot
filename <- "figures/Produktionspotenzial.jpg"
ggsave(filename, plot = plot_diff_PO, width = 10, height = 6)

#GDP/PO
#KOM
Germany_PO$GDPV_GDPVTR_KOM <- Germany_PO$Nominal_GDP_KOM / Germany_PO$KOM
Germany_PO$log_GDPV_GDPVTR_KOM <- log(Germany_PO$GDPV_GDPVTR_KOM)
Germany_PO <- Germany_PO %>% 
  mutate(diff_log_GDPV_GDPVTR_KOM = log_GDPV_GDPVTR_KOM - lag(log_GDPV_GDPVTR_KOM)) %>% 
  ungroup
Germany_PO$lag_GDPV_GDPVTR_KOM <- lag(Germany_PO$Nominal_GDP_KOM) / lag(Germany_PO$KOM)
Germany_PO$log_lag_GDPV_GDPVTR_KOM <- log(Germany_PO$lag_GDPV_GDPVTR_KOM)

#GDP/PO
#BMWK
Germany_PO$GDPV_GDPVTR_BMWK <- Germany_PO$Nominal_GDP_KOM / Germany_PO$BMWK
Germany_PO$log_GDPV_GDPVTR_BMWK <- log(Germany_PO$GDPV_GDPVTR_BMWK)
Germany_PO <- Germany_PO %>% 
  mutate(diff_log_GDPV_GDPVTR_BMWK = log_GDPV_GDPVTR_BMWK - lag(log_GDPV_GDPVTR_BMWK)) %>% 
  ungroup
Germany_PO$lag_GDPV_GDPVTR_BMWK <- lag(Germany_PO$Nominal_GDP_KOM) / lag(Germany_PO$BMWK)
Germany_PO$log_lag_GDPV_GDPVTR_BMWK <- log(Germany_PO$lag_GDPV_GDPVTR_BMWK)

#GDP/PO
#DZ
Germany_PO$GDPV_GDPVTR_DZ <- Germany_PO$Nominal_GDP_KOM / Germany_PO$DZ
Germany_PO$log_GDPV_GDPVTR_DZ <- log(Germany_PO$GDPV_GDPVTR_DZ)
Germany_PO <- Germany_PO %>% 
  mutate(diff_log_GDPV_GDPVTR_DZ = log_GDPV_GDPVTR_DZ - lag(log_GDPV_GDPVTR_DZ)) %>% 
  ungroup
Germany_PO$lag_GDPV_GDPVTR_DZ <- lag(Germany_PO$Nominal_GDP_KOM) / lag(Germany_PO$DZ)
Germany_PO$log_lag_GDPV_GDPVTR_DZ <- log(Germany_PO$lag_GDPV_GDPVTR_DZ)

#merge data
Germany_merge <- cbind(Germany, Germany_PO)
NAWRU <- select(diff_NAWRU, NAWRU_KOM, NAWRU_BMWK, NAWRU_DZ)
Germany_merge <- cbind(Germany_merge, NAWRU)

#Divide by real potential output
Germany_merge$wsshg_KOM <- Germany_merge$deu_wsss / Germany_merge$KOM_real / 1000000000
Germany_merge$yseg_KOM <- Germany_merge$deu_yse / Germany_merge$KOM_real / 1000000000
Germany_merge$gosbg_KOM <- Germany_merge$deu_gosb / Germany_merge$KOM_real / 1000000000
Germany_merge$ypeg_KOM <- Germany_merge$deu_ype / Germany_merge$KOM_real / 1000000000

Germany_merge$wsshg_BMWK <- Germany_merge$deu_wsss / Germany_merge$BMWK_real / 1000000000
Germany_merge$yseg_BMWK <- Germany_merge$deu_yse / Germany_merge$BMWK_real / 1000000000
Germany_merge$gosbg_BMWK <- Germany_merge$deu_gosb / Germany_merge$BMWK_real / 1000000000
Germany_merge$ypeg_BMWK <- Germany_merge$deu_ype / Germany_merge$BMWK_real / 1000000000

Germany_merge$wsshg_DZ <- Germany_merge$deu_wsss / Germany_merge$DZ_real / 1000000000
Germany_merge$yseg_DZ <- Germany_merge$deu_yse / Germany_merge$DZ_real / 1000000000
Germany_merge$gosbg_DZ <- Germany_merge$deu_gosb / Germany_merge$DZ_real / 1000000000
Germany_merge$ypeg_DZ <- Germany_merge$deu_ype / Germany_merge$DZ_real / 1000000000

#Divide by nominal potential output
Germany_merge$wsshg_KOMnom <- Germany_merge$deu_wsss / Germany_merge$KOM / 1000000000
Germany_merge$yseg_KOMnom <- Germany_merge$deu_yse / Germany_merge$KOM / 1000000000
Germany_merge$gosbg_KOMnom <- Germany_merge$deu_gosb / Germany_merge$KOM / 1000000000
Germany_merge$ypeg_KOMnom <- Germany_merge$deu_ype / Germany_merge$KOM / 1000000000

Germany_merge$wsshg_BMWKnom <- Germany_merge$deu_wsss / Germany_merge$BMWK / 1000000000
Germany_merge$yseg_BMWKnom <- Germany_merge$deu_yse / Germany_merge$BMWK / 1000000000
Germany_merge$gosbg_BMWKnom <- Germany_merge$deu_gosb / Germany_merge$BMWK / 1000000000
Germany_merge$ypeg_BMWKnom <- Germany_merge$deu_ype / Germany_merge$BMWK / 1000000000

Germany_merge$wsshg_DZnom <- Germany_merge$deu_wsss / Germany_merge$DZ / 1000000000
Germany_merge$yseg_DZnom <- Germany_merge$deu_yse / Germany_merge$DZ / 1000000000
Germany_merge$gosbg_DZnom <- Germany_merge$deu_gosb / Germany_merge$DZ / 1000000000
Germany_merge$ypeg_DZnom <- Germany_merge$deu_ype / Germany_merge$DZ / 1000000000

#UNR - divide by NAWRU
Germany_merge$ung_KOM <- Germany_merge$deu_unr/Germany_merge$NAWRU_KOM
Germany_merge$ung_BMWK <- Germany_merge$deu_unr/Germany_merge$NAWRU_BMWK
Germany_merge$ung_DZ <- Germany_merge$deu_unr/Germany_merge$NAWRU_DZ

#fiscal variables
#KOM
#WSSS
Germany_merge$lag_wsshg_KOM <- lag(Germany_merge$wsshg_KOM)
Germany_merge$log_lag_wsshg_KOM <- log(Germany_merge$lag_wsshg_KOM)
Germany_merge$log_wsshg_KOM <- log(Germany_merge$wsshg_KOM)
Germany_merge <- Germany_merge %>% 
  mutate(diff_log_wsshg_KOM = log_wsshg_KOM - lag(log_wsshg_KOM)) %>% 
  ungroup
Germany_merge$lag_diff_log_wsshg_KOM <- lag(Germany_merge$diff_log_wsshg_KOM)

#YSE
Germany_merge$lag_yseg_KOM <- lag(Germany_merge$yseg_KOM)
Germany_merge$lag_yseg_KOM
Germany_merge$log_lag_yseg_KOM <- log(Germany_merge$lag_yseg_KOM)
Germany_merge$log_yseg_KOM <- log(Germany_merge$yseg_KOM)
Germany_merge <- Germany_merge %>% 
  mutate(diff_log_yseg_KOM = log_yseg_KOM - lag(log_yseg_KOM)) %>% 
  ungroup
Germany_merge$lag_diff_log_yseg_KOM <- lag(Germany_merge$diff_log_yseg_KOM)

#GOS
Germany_merge$lag_gosbg_KOM <- lag(Germany_merge$gosbg_KOM)
Germany_merge$log_lag_gosbg_KOM <- log(Germany_merge$lag_gosbg_KOM)
Germany_merge$log_gosbg_KOM <- log(Germany_merge$gosbg_KOM)
Germany_merge <- Germany_merge %>% 
  mutate(diff_log_gosbg_KOM = log_gosbg_KOM - lag(log_gosbg_KOM)) %>% 
  ungroup
Germany_merge$lag_diff_log_gosbg_KOM <- lag(Germany_merge$diff_log_gosbg_KOM)

#YPE
Germany_merge$lag_ypeg_KOM <- lag(Germany_merge$ypeg_KOM)
Germany_merge$log_lag_ypeg_KOM <- log(Germany_merge$lag_ypeg_KOM)
Germany_merge$log_ypeg_KOM <- log(Germany_merge$ypeg_KOM)
Germany_merge <- Germany_merge %>% 
  mutate(diff_log_ypeg_KOM = log_ypeg_KOM - lag(log_ypeg_KOM)) %>% 
  ungroup
Germany_merge$lag_diff_log_ypeg_KOM <- lag(Germany_merge$diff_log_ypeg_KOM)

#UNR
Germany_merge$lag_ung_KOM <- lag(Germany_merge$ung_KOM)
Germany_merge$log_lag_ung_KOM <- log(Germany_merge$lag_ung_KOM)
Germany_merge$log_ung_KOM <- log(Germany_merge$ung_KOM)
Germany_merge <- Germany_merge %>% 
  mutate(diff_log_ung_KOM = log_ung_KOM - lag(log_ung_KOM)) %>% 
  ungroup
Germany_merge$lag_diff_log_ung_KOM <- lag(Germany_merge$diff_log_ung_KOM)

#BMWK
#WSSS
Germany_merge$lag_wsshg_BMWK <- lag(Germany_merge$wsshg_BMWK)
Germany_merge$log_lag_wsshg_BMWK <- log(Germany_merge$lag_wsshg_BMWK)
Germany_merge$log_wsshg_BMWK <- log(Germany_merge$wsshg_BMWK)
Germany_merge <- Germany_merge %>% 
  mutate(diff_log_wsshg_BMWK = log_wsshg_BMWK - lag(log_wsshg_BMWK)) %>% 
  ungroup
Germany_merge$lag_diff_log_wsshg_BMWK <- lag(Germany_merge$diff_log_wsshg_BMWK)

#YSE
Germany_merge$lag_yseg_BMWK <- lag(Germany_merge$yseg_BMWK)
Germany_merge$log_lag_yseg_BMWK <- log(Germany_merge$lag_yseg_BMWK)
Germany_merge$log_yseg_BMWK <- log(Germany_merge$yseg_BMWK)
Germany_merge <- Germany_merge %>% 
  mutate(diff_log_yseg_BMWK = log_yseg_BMWK - lag(log_yseg_BMWK)) %>% 
  ungroup
Germany_merge$lag_diff_log_yseg_BMWK <- lag(Germany_merge$diff_log_yseg_BMWK)

#GOS
Germany_merge$lag_gosbg_BMWK <- lag(Germany_merge$gosbg_BMWK)
Germany_merge$log_lag_gosbg_BMWK <- log(Germany_merge$lag_gosbg_BMWK)
Germany_merge$log_gosbg_BMWK <- log(Germany_merge$gosbg_BMWK)
Germany_merge <- Germany_merge %>% 
  mutate(diff_log_gosbg_BMWK = log_gosbg_BMWK - lag(log_gosbg_BMWK)) %>% 
  ungroup
Germany_merge$lag_diff_log_gosbg_BMWK <- lag(Germany_merge$diff_log_gosbg_BMWK)

#YPE
Germany_merge$lag_ypeg_BMWK <- lag(Germany_merge$ypeg_BMWK)
Germany_merge$log_lag_ypeg_BMWK <- log(Germany_merge$lag_ypeg_BMWK)
Germany_merge$log_ypeg_BMWK <- log(Germany_merge$ypeg_BMWK)
Germany_merge <- Germany_merge %>% 
  mutate(diff_log_ypeg_BMWK = log_ypeg_BMWK - lag(log_ypeg_BMWK)) %>% 
  ungroup
Germany_merge$lag_diff_log_ypeg_BMWK <- lag(Germany_merge$diff_log_ypeg_BMWK)

#UNR
Germany_merge$lag_ung_BMWK <- lag(Germany_merge$ung_BMWK)
Germany_merge$log_lag_ung_BMWK <- log(Germany_merge$lag_ung_BMWK)
Germany_merge$log_ung_BMWK <- log(Germany_merge$ung_BMWK)
Germany_merge <- Germany_merge %>% 
  mutate(diff_log_ung_BMWK = log_ung_BMWK - lag(log_ung_BMWK)) %>% 
  ungroup
Germany_merge$lag_diff_log_ung_BMWK <- lag(Germany_merge$diff_log_ung_BMWK)

#DZ
#WSSS
Germany_merge$lag_wsshg_DZ <- lag(Germany_merge$wsshg_DZ)
Germany_merge$log_lag_wsshg_DZ <- log(Germany_merge$lag_wsshg_DZ)
Germany_merge$log_wsshg_DZ <- log(Germany_merge$wsshg_DZ)
Germany_merge <- Germany_merge %>% 
  mutate(diff_log_wsshg_DZ = log_wsshg_DZ - lag(log_wsshg_DZ)) %>% 
  ungroup
Germany_merge$lag_diff_log_wsshg_DZ <- lag(Germany_merge$diff_log_wsshg_DZ)

#YSE
Germany_merge$lag_yseg_DZ <- lag(Germany_merge$yseg_DZ)
Germany_merge$log_lag_yseg_DZ <- log(Germany_merge$lag_yseg_DZ)
Germany_merge$log_yseg_DZ <- log(Germany_merge$yseg_DZ)
Germany_merge <- Germany_merge %>% 
  mutate(diff_log_yseg_DZ = log_yseg_DZ - lag(log_yseg_DZ)) %>% 
  ungroup
Germany_merge$lag_diff_log_yseg_DZ <- lag(Germany_merge$diff_log_yseg_DZ)

#GOS
Germany_merge$lag_gosbg_DZ <- lag(Germany_merge$gosbg_DZ)
Germany_merge$log_lag_gosbg_DZ <- log(Germany_merge$lag_gosbg_DZ)
Germany_merge$log_gosbg_DZ <- log(Germany_merge$gosbg_DZ)
Germany_merge <- Germany_merge %>% 
  mutate(diff_log_gosbg_DZ = log_gosbg_DZ - lag(log_gosbg_DZ)) %>% 
  ungroup
Germany_merge$lag_diff_log_gosbg_DZ <- lag(Germany_merge$diff_log_gosbg_DZ)

#YPE
Germany_merge$lag_ypeg_DZ <- lag(Germany_merge$ypeg_DZ)
Germany_merge$log_lag_ypeg_DZ <- log(Germany_merge$lag_ypeg_DZ)
Germany_merge$log_ypeg_DZ <- log(Germany_merge$ypeg_DZ)
Germany_merge <- Germany_merge %>% 
  mutate(diff_log_ypeg_DZ = log_ypeg_DZ - lag(log_ypeg_DZ)) %>% 
  ungroup
Germany_merge$lag_diff_log_ypeg_DZ <- lag(Germany_merge$diff_log_ypeg_DZ)

#UNR
Germany_merge$lag_ung_DZ <- lag(Germany_merge$ung_DZ)
Germany_merge$log_lag_ung_DZ <- log(Germany_merge$lag_ung_DZ)
Germany_merge$log_ung_DZ <- log(Germany_merge$ung_DZ)
Germany_merge <- Germany_merge %>% 
  mutate(diff_log_ung_DZ = log_ung_DZ - lag(log_ung_DZ)) %>% 
  ungroup
Germany_merge$lag_diff_log_ung_DZ <- lag(Germany_merge$diff_log_ung_DZ)

#divide through nominal PO:
#KOM
#WSSS
Germany_merge$lag_wsshg_KOMnom <- lag(Germany_merge$wsshg_KOMnom)
Germany_merge$log_lag_wsshg_KOMnom <- log(Germany_merge$lag_wsshg_KOMnom)
Germany_merge$log_wsshg_KOMnom <- log(Germany_merge$wsshg_KOMnom)
Germany_merge <- Germany_merge %>% 
  mutate(diff_log_wsshg_KOMnom = log_wsshg_KOMnom - lag(log_wsshg_KOMnom)) %>% 
  ungroup
Germany_merge$lag_diff_log_wsshg_KOMnom <- lag(Germany_merge$diff_log_wsshg_KOMnom)

#YSE
Germany_merge$lag_yseg_KOMnom <- lag(Germany_merge$yseg_KOMnom)
Germany_merge$lag_yseg_KOMnom
Germany_merge$log_lag_yseg_KOMnom <- log(Germany_merge$lag_yseg_KOMnom)
Germany_merge$log_yseg_KOMnom <- log(Germany_merge$yseg_KOMnom)
Germany_merge <- Germany_merge %>% 
  mutate(diff_log_yseg_KOMnom = log_yseg_KOMnom - lag(log_yseg_KOMnom)) %>% 
  ungroup
Germany_merge$lag_diff_log_yseg_KOMnom <- lag(Germany_merge$diff_log_yseg_KOMnom)

#GOS
Germany_merge$lag_gosbg_KOMnom <- lag(Germany_merge$gosbg_KOMnom)
Germany_merge$log_lag_gosbg_KOMnom <- log(Germany_merge$lag_gosbg_KOMnom)
Germany_merge$log_gosbg_KOMnom <- log(Germany_merge$gosbg_KOMnom)
Germany_merge <- Germany_merge %>% 
  mutate(diff_log_gosbg_KOMnom = log_gosbg_KOMnom - lag(log_gosbg_KOMnom)) %>% 
  ungroup
Germany_merge$lag_diff_log_gosbg_KOMnom <- lag(Germany_merge$diff_log_gosbg_KOMnom)

#YPE
Germany_merge$lag_ypeg_KOMnom <- lag(Germany_merge$ypeg_KOMnom)
Germany_merge$log_lag_ypeg_KOMnom <- log(Germany_merge$lag_ypeg_KOMnom)
Germany_merge$log_ypeg_KOMnom <- log(Germany_merge$ypeg_KOMnom)
Germany_merge <- Germany_merge %>% 
  mutate(diff_log_ypeg_KOMnom = log_ypeg_KOMnom - lag(log_ypeg_KOMnom)) %>% 
  ungroup
Germany_merge$lag_diff_log_ypeg_KOMnom <- lag(Germany_merge$diff_log_ypeg_KOMnom)

#BMWK
#WSSS
Germany_merge$lag_wsshg_BMWKnom <- lag(Germany_merge$wsshg_BMWKnom)
Germany_merge$log_lag_wsshg_BMWKnom <- log(Germany_merge$lag_wsshg_BMWKnom)
Germany_merge$log_wsshg_BMWKnom <- log(Germany_merge$wsshg_BMWKnom)
Germany_merge <- Germany_merge %>% 
  mutate(diff_log_wsshg_BMWKnom = log_wsshg_BMWKnom - lag(log_wsshg_BMWKnom)) %>% 
  ungroup
Germany_merge$lag_diff_log_wsshg_BMWKnom <- lag(Germany_merge$diff_log_wsshg_BMWKnom)

#YSE
Germany_merge$lag_yseg_BMWKnom <- lag(Germany_merge$yseg_BMWKnom)
Germany_merge$log_lag_yseg_BMWKnom <- log(Germany_merge$lag_yseg_BMWKnom)
Germany_merge$log_yseg_BMWKnom <- log(Germany_merge$yseg_BMWKnom)
Germany_merge <- Germany_merge %>% 
  mutate(diff_log_yseg_BMWKnom = log_yseg_BMWKnom - lag(log_yseg_BMWKnom)) %>% 
  ungroup
Germany_merge$lag_diff_log_yseg_BMWKnom <- lag(Germany_merge$diff_log_yseg_BMWKnom)

#GOS
Germany_merge$lag_gosbg_BMWKnom <- lag(Germany_merge$gosbg_BMWKnom)
Germany_merge$log_lag_gosbg_BMWKnom <- log(Germany_merge$lag_gosbg_BMWKnom)
Germany_merge$log_gosbg_BMWKnom <- log(Germany_merge$gosbg_BMWKnom)
Germany_merge <- Germany_merge %>% 
  mutate(diff_log_gosbg_BMWKnom = log_gosbg_BMWKnom - lag(log_gosbg_BMWKnom)) %>% 
  ungroup
Germany_merge$lag_diff_log_gosbg_BMWKnom <- lag(Germany_merge$diff_log_gosbg_BMWKnom)

#YPE
Germany_merge$lag_ypeg_BMWKnom <- lag(Germany_merge$ypeg_BMWKnom)
Germany_merge$log_lag_ypeg_BMWKnom <- log(Germany_merge$lag_ypeg_BMWKnom)
Germany_merge$log_ypeg_BMWKnom <- log(Germany_merge$ypeg_BMWKnom)
Germany_merge <- Germany_merge %>% 
  mutate(diff_log_ypeg_BMWKnom = log_ypeg_BMWKnom - lag(log_ypeg_BMWKnom)) %>% 
  ungroup
Germany_merge$lag_diff_log_ypeg_BMWKnom <- lag(Germany_merge$diff_log_ypeg_BMWKnom)

#DZ
#WSSS
Germany_merge$lag_wsshg_DZnom <- lag(Germany_merge$wsshg_DZnom)
Germany_merge$log_lag_wsshg_DZnom <- log(Germany_merge$lag_wsshg_DZnom)
Germany_merge$log_wsshg_DZnom <- log(Germany_merge$wsshg_DZnom)
Germany_merge <- Germany_merge %>% 
  mutate(diff_log_wsshg_DZnom = log_wsshg_DZnom - lag(log_wsshg_DZnom)) %>% 
  ungroup
Germany_merge$lag_diff_log_wsshg_DZnom <- lag(Germany_merge$diff_log_wsshg_DZnom)

#YSE
Germany_merge$lag_yseg_DZnom <- lag(Germany_merge$yseg_DZnom)
Germany_merge$log_lag_yseg_DZnom <- log(Germany_merge$lag_yseg_DZnom)
Germany_merge$log_yseg_DZnom <- log(Germany_merge$yseg_DZnom)
Germany_merge <- Germany_merge %>% 
  mutate(diff_log_yseg_DZnom = log_yseg_DZnom - lag(log_yseg_DZnom)) %>% 
  ungroup
Germany_merge$lag_diff_log_yseg_DZnom <- lag(Germany_merge$diff_log_yseg_DZnom)

#GOS
Germany_merge$lag_gosbg_DZnom <- lag(Germany_merge$gosbg_DZnom)
Germany_merge$log_lag_gosbg_DZnom <- log(Germany_merge$lag_gosbg_DZnom)
Germany_merge$log_gosbg_DZnom <- log(Germany_merge$gosbg_DZnom)
Germany_merge <- Germany_merge %>% 
  mutate(diff_log_gosbg_DZnom = log_gosbg_DZnom - lag(log_gosbg_DZnom)) %>% 
  ungroup
Germany_merge$lag_diff_log_gosbg_DZnom <- lag(Germany_merge$diff_log_gosbg_DZnom)

#YPE
Germany_merge$lag_ypeg_DZnom <- lag(Germany_merge$ypeg_DZnom)
Germany_merge$log_lag_ypeg_DZnom <- log(Germany_merge$lag_ypeg_DZnom)
Germany_merge$log_ypeg_DZnom <- log(Germany_merge$ypeg_DZnom)
Germany_merge <- Germany_merge %>% 
  mutate(diff_log_ypeg_DZnom = log_ypeg_DZnom - lag(log_ypeg_DZnom)) %>% 
  ungroup
Germany_merge$lag_diff_log_ypeg_DZnom <- lag(Germany_merge$diff_log_ypeg_DZnom)




##############################################################################
######################## Regressions #########################################
##############################################################################


################ Replicate OECD Estimates ##########################

########### Wages #########
#OECD-estimate: 0,657 (SE: 0,166)

#Use ARIMA function (see https://www.rdocumentation.org/packages/stats/versions/3.6.2/topics/arima)
#Create matrix of regressors:
X <- cbind(Germany$diff_log_GDPV_GDPVTR, Germany$log_lag_wsshg, Germany$log_lag_GDPV_GDPVTR)
X
#ARIMA regression with method CSS:
reg_ECT_wsshg_arima_CSS <- arima(Germany$diff_log_wsshg, order = c(1,0,0), xreg = X, method = "CSS")
reg_ECT_wsshg_arima_CSS

#Comparison with other ARIMA setups:
#default method:
reg_ECT_wsshg_arima <- arima(Germany$diff_log_wsshg, order = c(1,0,0), xreg = X)
reg_ECT_wsshg_arima
#CSS-ML:
reg_ECT_wsshg_arima_CSS_ML <- arima(Germany$diff_log_wsshg, order = c(1,0,0), xreg = X, method = "CSS-ML")
reg_ECT_wsshg_arima_CSS_ML
#ML:
reg_ECT_wsshg_arima_ML <- arima(Germany$diff_log_wsshg, order = c(1,0,0),xreg = X, method = "ML")
reg_ECT_wsshg_arima_ML

########### self-employed #########
#OECD-estimate: 1,749 (SE: 0,417)

#Create matrix of regressors:
X <- cbind(Germany$diff_log_GDPV_GDPVTR, Germany$log_lag_yseg, Germany$log_lag_GDPV_GDPVTR)
X
#ARIMA regression with method CSS:
reg_ECT_yseg_arima_CSS <- arima(Germany$diff_log_yseg, order = c(1,0,0), xreg = X, method = "CSS")
reg_ECT_yseg_arima_CSS

#Comparison with other ARIMA setups:
#default method:
reg_ECT_yseg_arima <- arima(Germany$diff_log_yseg, order = c(1,0,0), xreg = X)
reg_ECT_yseg_arima
#CSS-ML:
reg_ECT_yseg_arima_CSS_ML <- arima(Germany$diff_log_yseg, order = c(1,0,0), xreg = X, method = "CSS-ML")
reg_ECT_yseg_arima_CSS_ML
#ML:
reg_ECT_yseg_arima_ML <- arima(Germany$diff_log_yseg, order = c(1,0,0), method = "ML")
reg_ECT_yseg_arima_ML

########### gross operating surplus #########
#OECD-estimate: 1,140 (SE: 0,306)

#Create matrix of regressors:
X <- cbind(Germany$diff_log_GDPV_GDPVTR, Germany$log_lag_gosbg, Germany$log_lag_GDPV_GDPVTR)
X
#ARIMA regression with method CSS:
reg_ECT_gosbg_arima_CSS <- arima(Germany$diff_log_gosbg, order = c(1,0,0), xreg = X, method = "CSS")
reg_ECT_gosbg_arima_CSS

#Comparison with other ARIMA setups:
#default method:
reg_ECT_gosbg_arima <- arima(Germany$diff_log_gosbg, order = c(1,0,0), xreg = X)
reg_ECT_gosbg_arima
#CSS-ML:
reg_ECT_gosbg_arima_CSS_ML <- arima(Germany$diff_log_gosbg, order = c(1,0,0), xreg = X, method = "CSS-ML")
reg_ECT_gosbg_arima_CSS_ML
#ML:
reg_ECT_gosbg_arima_ML <- arima(Germany$diff_log_gosbg, order = c(1,0,0), method = "ML")
reg_ECT_gosbg_arima_ML

########### capital income #########
#OECD-estimate: 1,32 (SE: not provided);

#Create matrix of regressors:
X <- cbind(Germany$diff_log_GDPV_GDPVTR, Germany$log_lag_ypeg, Germany$log_lag_GDPV_GDPVTR)
X
#ARIMA regression with method CSS:
reg_ECT_ypeg_arima_CSS <- arima(Germany$diff_log_ypeg, order = c(1,0,0), xreg = X, method = "CSS")
reg_ECT_ypeg_arima_CSS

#Comparison with other ARIMA setups:
#default method:
reg_ECT_ypeg_arima <- arima(Germany$diff_log_ypeg, order = c(1,0,0), xreg = X)
reg_ECT_ypeg_arima
#CSS-ML:
reg_ECT_ypeg_arima_CSS_ML <- arima(Germany$diff_log_ypeg, order = c(1,0,0), xreg = X, method = "CSS-ML")
reg_ECT_ypeg_arima_CSS_ML
#ML:
reg_ECT_ypeg_arima_ML <- arima(Germany$diff_log_ypeg, order = c(1,0,0), method = "ML")
reg_ECT_ypeg_arima_ML

########### Unemployment #########
#OECD-estimate: -3,302 (SE: 0,665)

#create matrix of regressors:
X <- cbind(Germany$diff_log_GDPV_GDPVTR, Germany$log_lag_unr, Germany$log_lag_GDPV_GDPVTR)
X
#ARIMA regression with method CSS:
reg_ECT_unr_arima_CSS <- arima(Germany$diff_log_unr, order = c(1,0,0), xreg = X, method = "CSS")
reg_ECT_unr_arima_CSS

#Comparison with other ARIMA setups:
#default method:
reg_ECT_unr_arima <- arima(Germany$diff_log_unr, order = c(1,0,0), xreg = X)
reg_ECT_unr_arima
#CSS-ML:
reg_ECT_unr_arima_CSS_ML <- arima(Germany$diff_log_unr, order = c(1,0,0), xreg = X, method = "CSS-ML")
reg_ECT_unr_arima_CSS_ML
#ML:
reg_ECT_unr_arima_ML <- arima(Germany$diff_log_unr, order = c(1,0,0), method = "ML")
reg_ECT_unr_arima_ML


################ Estimates for KOM, BMWK and EZ  ##############################


######### Wages #################
#OECD-estimate: 0,657 (SE: 0,166)

########### KOM #########

#create matrix of regressors:
X <- cbind(Germany_merge$diff_log_GDPV_GDPVTR_KOM, Germany_merge$log_lag_wsshg_KOM, Germany_merge$log_lag_GDPV_GDPVTR_KOM)
X
#ARIMA regression with option CSS:
reg_ECT_wsshg_KOM_arima_CSS <- arima(Germany_merge$diff_log_wsshg_KOM, order = c(1,0,0), xreg = X, method = "CSS")
reg_ECT_wsshg_KOM_arima_CSS

########### BMWK #########

#create matrix of regressors:
X <- cbind(Germany_merge$diff_log_GDPV_GDPVTR_BMWK, Germany_merge$log_lag_wsshg_BMWK, Germany_merge$log_lag_GDPV_GDPVTR_BMWK)
X
#ARIMA regression with method CSS:
reg_ECT_wsshg_BMWK_arima_CSS <- arima(Germany_merge$diff_log_wsshg_BMWK, order = c(1,0,0), xreg = X, method = "CSS")
reg_ECT_wsshg_BMWK_arima_CSS

#comparison with OLS:
reg_ECT_wsshg_BMWK <- lm(diff_log_wsshg_BMWK ~ diff_log_GDPV_GDPVTR_BMWK + log_lag_wsshg_BMWK + log_lag_GDPV_GDPVTR_BMWK + lag_diff_log_wsshg_BMWK, data=Germany_merge)
summary(reg_ECT_wsshg_BMWK)

########### DZ #########

#create matrix of regressors:
X <- cbind(Germany_merge$diff_log_GDPV_GDPVTR_DZ, Germany_merge$log_lag_wsshg_DZ, Germany_merge$log_lag_GDPV_GDPVTR_DZ)
X
#ARIMA regression with method CSS:
reg_ECT_wsshg_DZ_arima_CSS <- arima(Germany_merge$diff_log_wsshg_DZ, order = c(1,0,0), xreg = X, method = "CSS")
reg_ECT_wsshg_DZ_arima_CSS

########### self-employed #########
#OECD-estimate: 1,749 (SE: 0,417)

#KOM

#create matrix of regressors:
X <- cbind(Germany_merge$diff_log_GDPV_GDPVTR_KOM, Germany_merge$log_lag_yseg_KOM, Germany_merge$log_lag_GDPV_GDPVTR_KOM)
X
#ARIMA regression with method CSS:
reg_ECT_yseg_KOM_arima_CSS <- arima(Germany_merge$diff_log_yseg_KOM, order = c(1,0,0), xreg = X, method = "CSS")
reg_ECT_yseg_KOM_arima_CSS

#BMWK

#create matrix of regressors:
X <- cbind(Germany_merge$diff_log_GDPV_GDPVTR_BMWK, Germany_merge$log_lag_yseg_BMWK, Germany_merge$log_lag_GDPV_GDPVTR_BMWK)
X
#ARIMA regression with method CSS:
reg_ECT_yseg_BMWK_arima_CSS <- arima(Germany_merge$diff_log_yseg_BMWK, order = c(1,0,0), xreg = X, method = "CSS")
reg_ECT_yseg_BMWK_arima_CSS

#DZ

#create matrix of regressors:
X <- cbind(Germany_merge$diff_log_GDPV_GDPVTR_DZ, Germany_merge$log_lag_yseg_DZ, Germany_merge$log_lag_GDPV_GDPVTR_DZ)
X
#ARIMA regression with method CSS:
reg_ECT_yseg_DZ_arima_CSS <- arima(Germany_merge$diff_log_yseg_DZ, order = c(1,0,0), xreg = X, method = "CSS")
reg_ECT_yseg_DZ_arima_CSS

########### gross operating surplus #########
#OECD-estimate: 1,140 (SE: 0,306)

####### KOM#######

#create matrix of regressors:
X <- cbind(Germany_merge$diff_log_GDPV_GDPVTR_KOM, Germany_merge$log_lag_gosbg_KOM, Germany_merge$log_lag_GDPV_GDPVTR_KOM)
X
#ARIMA regression with method CSS:
reg_ECT_gosbg_KOM_arima_CSS <- arima(Germany_merge$diff_log_gosbg_KOM, order = c(1,0,0), xreg = X, method = "CSS")
reg_ECT_gosbg_KOM_arima_CSS

####### BMWK #######

#create matrix of regressors:
X <- cbind(Germany_merge$diff_log_GDPV_GDPVTR_BMWK, Germany_merge$log_lag_gosbg_BMWK, Germany_merge$log_lag_GDPV_GDPVTR_BMWK)
X
#ARIMA regression with method CSS:
reg_ECT_gosbg_BMWK_arima_CSS <- arima(Germany_merge$diff_log_gosbg_BMWK, order = c(1,0,0), xreg = X, method = "CSS")
reg_ECT_gosbg_BMWK_arima_CSS

#comparison with OLS:
reg_ECT_gosbg_BMWK <- lm(diff_log_gosbg_BMWK ~ diff_log_GDPV_GDPVTR_BMWK + log_lag_gosbg_BMWK + log_lag_GDPV_GDPVTR_BMWK + lag_diff_log_gosbg_BMWK, data=Germany_merge)
summary(reg_ECT_gosbg_BMWK)

####### DZ #######

#create matrix of regressors:
X <- cbind(Germany_merge$diff_log_GDPV_GDPVTR_DZ, Germany_merge$log_lag_gosbg_DZ, Germany_merge$log_lag_GDPV_GDPVTR_DZ)
X
#ARIMA regression with method CSS:
reg_ECT_gosbg_DZ_arima_CSS <- arima(Germany_merge$diff_log_gosbg_DZ, order = c(1,0,0), xreg = X, method = "CSS")
reg_ECT_gosbg_DZ_arima_CSS

########### capital income #########
#OECD-estimate: 1,32 (SE: not provided)

#### KOM ####

#create matrix of regressors:
X <- cbind(Germany_merge$diff_log_GDPV_GDPVTR_KOM, Germany_merge$log_lag_ypeg_KOM, Germany_merge$log_lag_GDPV_GDPVTR_KOM)
X
#ARIMA regression with method CSS:
reg_ECT_ypeg_KOM_arima_CSS <- arima(Germany_merge$diff_log_ypeg_KOM, order = c(1,0,0), xreg = X, method = "CSS")
reg_ECT_ypeg_KOM_arima_CSS

#### BMWK ####

#create matrix of regressors:
X <- cbind(Germany_merge$diff_log_GDPV_GDPVTR_BMWK, Germany_merge$log_lag_ypeg_BMWK, Germany_merge$log_lag_GDPV_GDPVTR_BMWK)
X
#ARIMA regression with method CSS:
reg_ECT_ypeg_BMWK_arima_CSS <- arima(Germany_merge$diff_log_ypeg_BMWK, order = c(1,0,0), xreg = X, method = "CSS")
reg_ECT_ypeg_BMWK_arima_CSS

#### DZ ####

#create matrix of regressors:
X <- cbind(Germany_merge$diff_log_GDPV_GDPVTR_DZ, Germany_merge$log_lag_ypeg_DZ, Germany_merge$log_lag_GDPV_GDPVTR_DZ)
X
#ARIMA regression with method CSS:
reg_ECT_ypeg_DZ_arima_CSS <- arima(Germany_merge$diff_log_ypeg_DZ, order = c(1,0,0), xreg = X, method = "CSS")
reg_ECT_ypeg_DZ_arima_CSS

########### Unemployment #########
#OECD-estimate: -3,302 (SE: 0,665)

### KOM ###

#create matrix of regressors:
X <- cbind(Germany_merge$diff_log_GDPV_GDPVTR_KOM, Germany_merge$log_lag_ung_KOM, Germany_merge$log_lag_GDPV_GDPVTR_KOM)
X
#ARIMA regression with method CSS:
reg_ECT_unr_KOM_arima_CSS <- arima(Germany_merge$diff_log_ung_KOM, order = c(1,0,0), xreg = X, method = "CSS")
reg_ECT_unr_KOM_arima_CSS

### BMWK ###

#create matrix of regressors:
X <- cbind(Germany_merge$diff_log_GDPV_GDPVTR_BMWK, Germany_merge$log_lag_ung_BMWK, Germany_merge$log_lag_GDPV_GDPVTR_BMWK)
X
#ARIMA regression with method CSS:
reg_ECT_unr_BMWK_arima_CSS <- arima(Germany_merge$diff_log_ung_BMWK, order = c(1,0,0), xreg = X, method = "CSS")
reg_ECT_unr_BMWK_arima_CSS

### DZ ###

#create matrix of regressors:
X <- cbind(Germany_merge$diff_log_GDPV_GDPVTR_DZ, Germany_merge$log_lag_ung_DZ, Germany_merge$log_lag_GDPV_GDPVTR_DZ)
X
#ARIMA regression with method CSS:
reg_ECT_unr_DZ_arima_CSS <- arima(Germany_merge$diff_log_ung_DZ, order = c(1,0,0), xreg = X, method = "CSS")
reg_ECT_unr_DZ_arima_CSS

#use NAIRU from OECD but continue with DZ-output gap:
#create matrix of regressors:
X <- cbind(Germany_merge$diff_log_GDPV_GDPVTR_DZ, Germany$log_lag_unr, Germany_merge$log_lag_GDPV_GDPVTR_DZ)
X
#ARIMA regression with method CSS:
reg_ECT_unr_DZ_arima_CSS_OECD_NAIRU <- arima(Germany$diff_log_unr, order = c(1,0,0), xreg = X, method = "CSS")
reg_ECT_unr_DZ_arima_CSS_OECD_NAIRU

#estimate OLS without AR1:
reg_ECT_ung_2_DZ <- lm(diff_log_ung_DZ ~ diff_log_GDPV_GDPVTR_DZ + log_lag_ung_DZ + log_lag_GDPV_GDPVTR_DZ, data=Germany_merge)
summary(reg_ECT_ung_2_DZ)

#GlS model with AR1:
library(rms)
reg_GLS_ung_DZ <- Gls(diff_log_ung_DZ ~ diff_log_GDPV_GDPVTR_DZ, correlation=corAR1(form= ~diff_log_GDPV_GDPVTR_DZ), data=Germany_merge)
reg_GLS_ung_DZ


################ Using nominal potential output to normalize ##########################

########### Wages #########
#OECD-estimate: 0,657 (SE: 0,166)

#create matrix of regressors:
X <- cbind(Germany$diff_log_GDPV_GDPVTR, Germany$log_lag_wsss_GDPTR, Germany$log_lag_GDPV_GDPVTR)
X
#ARIMA regression with method CSS:
reg_ECT_wsss_GDPTR_arima_CSS <- arima(Germany$diff_log_wsss_GDPTR, order = c(1,0,0), xreg = X, method = "CSS")
reg_ECT_wsss_GDPTR_arima_CSS

########### self-employed #########
#OECD-estimate: 1,749 (SE: 0,417)

#create matrix of regressors:
X <- cbind(Germany$diff_log_GDPV_GDPVTR, Germany$log_lag_yse_GDPTR, Germany$log_lag_GDPV_GDPVTR )
X
#ARIMA regression with method CSS:
reg_ECT_yse_GDPTR_arima_CSS <- arima(Germany$diff_log_yse_GDPTR, order = c(1,0,0), xreg = X, method = "CSS")
reg_ECT_yse_GDPTR_arima_CSS

########### gross operating surplus #########
#OECD-estimate: 1,140 (SE: 0,306)

#create matrix of regressors:
X <- cbind(Germany$diff_log_GDPV_GDPVTR, Germany$log_lag_gosb_GDPTR, Germany$log_lag_GDPV_GDPVTR)
X
#ARIMA regression with method CSS:
reg_ECT_gosbg_GDPTR_arima_CSS <- arima(Germany$diff_log_gosb_GDPTR, order = c(1,0,0), xreg = X, method = "CSS")
reg_ECT_gosbg_GDPTR_arima_CSS

########### capital income #########
#OECD-estimate: 1,32 (SE: not provided)

#create matrix of regressors:
X <- cbind(Germany$diff_log_GDPV_GDPVTR, Germany$log_lag_ype_GDPTR, Germany$log_lag_GDPV_GDPVTR)
X
#ARIMA regression with method CSS:
reg_ECT_ype_GDPTR_arima_CSS <- arima(Germany$diff_log_ype_GDPTR, order = c(1,0,0), xreg = X, method = "CSS")
reg_ECT_ype_GDPTR_arima_CSS


### Estimates with nominal potential output for KOM, BMWK and EZ  ###

######### Wages #################
#OECD-estimate: 0,657 (SE: 0,166)

########### KOM #########

#create matrix of regressors:
X <- cbind(Germany_merge$diff_log_GDPV_GDPVTR_KOM, Germany_merge$log_lag_wsshg_KOMnom, Germany_merge$log_lag_GDPV_GDPVTR_KOM)
X
#ARIMA regression with mwthod CSS:
reg_ECT_wsshg_KOMnom_arima_CSS <- arima(Germany_merge$diff_log_wsshg_KOMnom, order = c(1,0,0), xreg = X, method = "CSS")
reg_ECT_wsshg_KOMnom_arima_CSS

########### BMWK #########

#create matrix of regressors:
X <- cbind(Germany_merge$diff_log_GDPV_GDPVTR_BMWK, Germany_merge$log_lag_wsshg_BMWKnom, Germany_merge$log_lag_GDPV_GDPVTR_BMWK)
X
#ARIMA regression with method CSS:
reg_ECT_wsshg_BMWKnom_arima_CSS <- arima(Germany_merge$diff_log_wsshg_BMWKnom, order = c(1,0,0), xreg = X, method = "CSS")
reg_ECT_wsshg_BMWKnom_arima_CSS

########### DZ #########

#create matrix of regressors:
X <- cbind(Germany_merge$diff_log_GDPV_GDPVTR_DZ, Germany_merge$log_lag_wsshg_DZnom, Germany_merge$log_lag_GDPV_GDPVTR_DZ)
X
#ARIMA regression with method CSS:
reg_ECT_wsshg_DZnom_arima_CSS <- arima(Germany_merge$diff_log_wsshg_DZnom, order = c(1,0,0), xreg = X, method = "CSS")
reg_ECT_wsshg_DZnom_arima_CSS

########### self-employed #########
#OECD-estimate: 1,749 (SE: 0,417)

### KOM ###

#create matrix of regressors:
X <- cbind(Germany_merge$diff_log_GDPV_GDPVTR_KOM, Germany_merge$log_lag_yseg_KOMnom, Germany_merge$log_lag_GDPV_GDPVTR_KOM)
X
#ARIMA regression with method CSS:
reg_ECT_yseg_KOMnom_GDPTR_arima_CSS <- arima(Germany_merge$diff_log_yseg_KOMnom, order = c(1,0,0), xreg = X, method = "CSS")
reg_ECT_yseg_KOMnom_GDPTR_arima_CSS

### BMWK ###

#create matrix of regressors:
X <- cbind(Germany_merge$diff_log_GDPV_GDPVTR_BMWK, Germany_merge$log_lag_yseg_BMWKnom, Germany_merge$log_lag_GDPV_GDPVTR_BMWK)
X
#ARIMA regression with method CSS:
reg_ECT_yseg_BMWKnom_GDPTR_arima_CSS <- arima(Germany_merge$diff_log_yseg_BMWKnom, order = c(1,0,0), xreg = X, method = "CSS")
reg_ECT_yseg_BMWKnom_GDPTR_arima_CSS

### DZ ###

#create matrix of regressors:
X <- cbind(Germany_merge$diff_log_GDPV_GDPVTR_DZ, Germany_merge$log_lag_yseg_DZnom, Germany_merge$log_lag_GDPV_GDPVTR_DZ)
X
#ARIMA regression with method CSS:
reg_ECT_yseg_DZnom_GDPTR_arima_CSS <- arima(Germany_merge$diff_log_yseg_DZnom, order = c(1,0,0), xreg = X, method = "CSS")
reg_ECT_yseg_DZnom_GDPTR_arima_CSS

########### gross operating surplus #########
#OECD-estimate: 1,140 (SE: 0,306)

####### KOM #######

#create matrix of regressors:
X <- cbind(Germany_merge$diff_log_GDPV_GDPVTR_KOM, Germany_merge$log_lag_gosbg_KOMnom, Germany_merge$log_lag_GDPV_GDPVTR_KOM)
X
#ARIMA regression with method CSS:
reg_ECT_gosbg_KOMnom_arima_CSS <- arima(Germany_merge$diff_log_gosbg_KOMnom, order = c(1,0,0), xreg = X, method = "CSS")
reg_ECT_gosbg_KOMnom_arima_CSS

####### BMWK #######

#create matrix of regressors:
X <- cbind(Germany_merge$diff_log_GDPV_GDPVTR_BMWK, Germany_merge$log_lag_gosbg_BMWKnom, Germany_merge$log_lag_GDPV_GDPVTR_BMWK)
X
#ARIMA regression with method CSS:
reg_ECT_gosbg_BMWKnom_arima_CSS <- arima(Germany_merge$diff_log_gosbg_BMWKnom, order = c(1,0,0), xreg = X, method = "CSS")
reg_ECT_gosbg_BMWKnom_arima_CSS

####### DZ #######

#create matrix of regressors:
X <- cbind(Germany_merge$diff_log_GDPV_GDPVTR_DZ, Germany_merge$log_lag_gosbg_DZnom, Germany_merge$log_lag_GDPV_GDPVTR_DZ)
X
#ARIMA regression with method CSS:
reg_ECT_gosbg_DZnom_arima_CSS <- arima(Germany_merge$diff_log_gosbg_DZnom, order = c(1,0,0), xreg = X, method = "CSS")
reg_ECT_gosbg_DZnom_arima_CSS

########### capital income #########
#OECD-estimate: 1,032 (SE: not provided)

#### KOM ####

#create matrix of regressors:
X <- cbind(Germany_merge$diff_log_GDPV_GDPVTR_KOM, Germany_merge$log_lag_ypeg_KOMnom, Germany_merge$log_lag_GDPV_GDPVTR_KOM)
X
#ARIMA regression with method CSS:
reg_ECT_ypeg_KOMnom_arima_CSS <- arima(Germany_merge$diff_log_ypeg_KOMnom, order = c(1,0,0), xreg = X, method = "CSS")
reg_ECT_ypeg_KOMnom_arima_CSS

#### BMWK ####

#create matrix of regressors:
X <- cbind(Germany_merge$diff_log_GDPV_GDPVTR_BMWK, Germany_merge$log_lag_ypeg_BMWKnom, Germany_merge$log_lag_GDPV_GDPVTR_BMWK)
X
#ARIMA regression with method CSS:
reg_ECT_ypeg_BMWKnom_arima_CSS <- arima(Germany_merge$diff_log_ypeg_BMWKnom, order = c(1,0,0), xreg = X, method = "CSS")
reg_ECT_ypeg_BMWKnom_arima_CSS

#### DZ ####

#create matrix of regressors:
X <- cbind(Germany_merge$diff_log_GDPV_GDPVTR_DZ, Germany_merge$log_lag_ypeg_DZnom, Germany_merge$log_lag_GDPV_GDPVTR_DZ)
X
#ARIMA regression with method CSS:
reg_ECT_ypeg_DZnom_arima_CSS <- arima(Germany_merge$diff_log_ypeg_DZnom, order = c(1,0,0), xreg = X, method = "CSS")
reg_ECT_ypeg_DZnom_arima_CSS
