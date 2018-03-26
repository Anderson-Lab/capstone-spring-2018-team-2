library(corrplot)
source("Join_Data.R")
meps <- Join_MEPS()
mepsPublic<-Public_Filter(meps)
mepsPrivate<-Private_Filter(meps)

numeric <- c('PERWT15F', 'FAMINC15', 'AGE15X', 'BMINDX53', 'PHOLDER', 'IPDIS15',
             'TOTEXP15')
c <- cor(meps[,numeric])
corrplot(c, method = 'color', title="All Plans", tl.srt = 45, tl.cex=1, mar=c(0,0,1,0))
c.pu <- cor(mepsPublic[,numeric])
corrplot(c.pu, method = 'color', title="Public Plans Only", tl.srt = 45, tl.cex=1, mar=c(0,0,1,0))
c.pr <- cor(mepsPrivate[,numeric])
corrplot(c.pr, method = 'color', title="Private Plans Only", tl.srt = 45, tl.cex=1, mar=c(0,0,1,0))




# POLICY HOLDERS ONLY
meps.p <- meps[meps$PHOLDER == 1,]
mepsPublic<-Public_Filter(meps.p)
mepsPrivate<-Private_Filter(meps.p)

numeric <- c('PERWT15F', 'FAMINC15', 'AGE15X', 'BMINDX53', 'IPDIS15',
             'TOTEXP15')
c <- cor(meps.p[,numeric])
corrplot(c, method = 'color', title="All Plans: Policy Holders Only", tl.srt = 45, tl.cex=1, mar=c(0,0,1,0))
c.pu <- cor(mepsPublic[,numeric])
corrplot(c.pu, method = 'color', title="Public Plans Only: Policy Holders Only", tl.srt = 45, tl.cex=1, mar=c(0,0,1,0))
c.pr <- cor(mepsPrivate[,numeric])
corrplot(c.pr, method = 'color', title="Private Plans Only: Policy Holders Only", tl.srt = 45, tl.cex=1, mar=c(0,0,1,0))