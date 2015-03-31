rm(list=ls())

wd <- paste0("Z:\\Consultancies\\CoB_MeelupRegionalPark_15061H03\\",
                "DATA\\Working\\Analysis_IndexDevelopment")
setwd(wd)

d <- read.csv("AllAverageData_20150218.csv", header = TRUE)

m235_aer <- lm(d$AerialPhoto ~ d$Index.2.3.5)
m235_pfc10bitt <- lm(d$PFCIF10.Bitt ~ d$Index.2.3.5)
m235_pfc20cano <- lm(d$PFCIF20.Cano ~ d$Index.2.3.5)
m235_pfctvif10bitt <- lm(d$PFCTVIF10.Bitt ~ d$Index.2.3.5)
m235_pfctvc <- lm(d$TotalVegCover ~ d$Index.2.3.5)

m35_aer <- lm(d$AerialPhoto ~ d$Index35)
m35_pfc10bitt <- lm(d$PFCIF10.Bitt ~ d$Index35)
m35_pfc20cano <- lm(d$PFCIF20.Cano ~ d$Index35)
m35_pfctvif10bitt <- lm(d$PFCTVIF10.Bitt ~ d$Index35)
m35_pfctvc <- lm(d$TotalVegCover ~ d$Index35)

## Akaike's AIC hashed best scores
AIC(m235_aer)
AIC(m35_aer)
AIC(m235_pfc10bitt)##
AIC(m35_pfc10bitt)
AIC(m235_pfc20cano)
AIC(m35_pfc20cano)
AIC(m235_pfctvif10bitt)##
AIC(m35_pfctvif10bitt)
AIC(m235_pfctvc)
AIC(m35_pfctvc)

par(mfrow = c(2,2))
plot(m235_pfc10bitt)

plot(m235_pfctvif10bitt)
par(mfrow = c(1,1))
summary(m235_pfc10bitt)
summary(m235_pfctvif10bitt)


hist(m235_pfc10bitt$resid)
hist(m235_pfctvif10bitt$resid)

## Strip down to make tables
m1 <- lm(d$AerialPhoto ~ d$Index.2.3.5)
m2 <- lm(d$PFCIF10.Bitt ~ d$Index.2.3.5)
m3 <- lm(d$PFCIF20.Cano ~ d$Index.2.3.5)
m4 <- lm(d$PFCTVIF10.Bitt ~ d$Index.2.3.5)
m5 <- lm(d$TotalVegCover ~ d$Index.2.3.5)
m6 <- lm(d$AerialPhoto ~ d$Index35)
m7 <- lm(d$PFCIF10.Bitt ~ d$Index35)
m8 <- lm(d$PFCIF20.Cano ~ d$Index35)
m9 <- lm(d$PFCTVIF10.Bitt ~ d$Index35)
m10 <- lm(d$TotalVegCover ~ d$Index35)

mlist <- list(summary(m1), summary(m2), summary(m3), summary(m4), summary(m5),
              summary(m6), summary(m7), summary(m8), summary(m9), summary(m10))



adj <- lapply(mlist, function(x) x$adj.r.squared)


lmp <- function (modelsummary) {
        f <- modelsummary$fstatistic
        p <- pf(f[1],f[2],f[3],lower.tail=F)
        attributes(p) <- NULL
        return(p)
}

p <- lapply(mlist, lmp)

mnames1 <- c("m1", "m2", "m3", "m4", "m5", "m6", "m7", "m8", "m9", "m10")






a <- adj(mlist)
mlist[[1]]$resid
names(mlist[[1]])





