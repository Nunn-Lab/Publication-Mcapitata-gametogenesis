library(ggplot2)
library(reshape)
library(lme4)
library(lmerTest)

symb.dat<-read.csv('UW_final_data_colonies.csv')
symb.dat$Time<-symb.dat$well_name
symb.dat$Colony<-symb.dat$well_name

symb.dat$Time<-sub(" .*", "", symb.dat$Time)
symb.dat$Colony<-sub("T1 ", "", symb.dat$Colony)
symb.dat$Colony<-sub("T2 ", "", symb.dat$Colony)
symb.dat$Colony<-sub("T3 ", "", symb.dat$Colony)
symb.dat$Colony<-sub("T4 ", "", symb.dat$Colony)
symb.dat$Colony<-sub("T5 ", "", symb.dat$Colony)
symb.dat$Colony<-sub("T6 ", "", symb.dat$Colony)

symb.dat$Treatment<-symb.dat$Colony
symb.dat$Colony<-sub('NB', '', symb.dat$Colony)
symb.dat$Colony<-sub('B', '', symb.dat$Colony)

symb.dat$Treatment<-gsub('[0-9]+', '', symb.dat$Treatment)

symb.sub<-subset(symb.dat, select=c("Time", 'Colony', 'Treatment', 'prop_d', 'prop_c'))
symb.melt<-melt(symb.sub, id.vars=c('Time', 'Colony', 'Treatment'))

x_int<-interaction(symb.melt$Time, symb.melt$Treatment)
level_order<-c('T1.','T2.NB', 'T2.B', 'T6.NB','T6.B'  )

symb.pl1<-ggplot(data=symb.melt, aes(fill=interaction(variable,Treatment), y=value, x=interaction(Time, Treatment))) +
  geom_bar(position='stack', stat='identity') +
  facet_wrap(~Colony) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  xlab('Time and Treatment') +
  ylab('Proportion Clade') +
  ylim(0,1) +
  scale_fill_manual(values=c('goldenrod3', 'orchid4', 'goldenrod1', 'orchid1', 'goldenrod3', 'orchid4'), labels=c('T1 D', 'T1 C', 'D Bleached', 'D Bleached', 'D Control', 'C Control')) +
  scale_x_discrete(limits = level_order, labels=c('Sept. 2017', 'Oct. 2017\nCtrl', 'Oct. 2017\nBleached', 'July 2018\nCtrl', 'July 2018\nBleached'))


png("symbiont clades R and S v2.png", width=10, height=10, units = "in", res = 1200)
symb.pl1
dev.off()

#resilient only
#colonies 10, 13, 21, 25, 26, 36, 40, 43, 62, 64, 65, 66, 74
symb.R<-subset(symb.melt, Colony == 10| Colony == 13|Colony == 21|Colony == 25|Colony == 26|Colony == 36|Colony == 40|Colony == 43|Colony == 62|Colony == 64|Colony == 65|Colony == 66|Colony == 74)

symb.pl2<-ggplot(data=symb.R, aes(fill=interaction(variable,Treatment), y=value, x=interaction(Time, Treatment))) +
  geom_bar(position='stack', stat='identity') +
  facet_wrap(~Colony) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  xlab('Time and Treatment') +
  ylab('Proportion Clade') +
  ylim(0,1) +
  scale_fill_manual(values=c('goldenrod3', 'orchid4', 'goldenrod1', 'orchid1', 'goldenrod3', 'orchid4'), labels=c('T1 D', 'T1 C', 'D Bleached', 'D Bleached', 'D Control', 'C Control')) +
  scale_x_discrete(limits = level_order, labels=c('Sept. 2017', 'Oct. 2017\nCtrl', 'Oct. 2017\nBleached', 'July 2018\nCtrl', 'July 2018\nBleached'))


png("symbiont clades resilient only v2.png", width=10, height=10, units = "in", res = 1200)
symb.pl2
dev.off()

#create column for dominant clade
symb.dat$abundance<-gsub(">.*","",symb.dat$abundance)

symb.R2<-subset(symb.dat, Colony == 10| Colony == 13|Colony == 21|Colony == 25|Colony == 26|Colony == 36|Colony == 40|Colony == 43|Colony == 62|Colony == 64|Colony == 65|Colony == 66|Colony == 74)
symb.R2$Colony<-as.factor(symb.R2$Colony)
symb.R2["Treatment"][symb.R2["Treatment"] == ''] <- NA
clade.lm <- glmer(prop_d ~ Treatment + Time + abundance + (1|Colony), data=symb.R2, family = binomial)
summary(clade.lm)

