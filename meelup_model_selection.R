## Analysis of models, model selction and plots for Meelup Regional
## Park work.

## Bart Huntley 02/04/2015


rm(list=ls())

library(xtable)
library(ggplot2)
library(GGally) # for ggplot pairs plot

wd <- paste0("Z:\\Consultancies\\CoB_MeelupRegionalPark_15061H03\\",
                "DATA\\Working\\Analysis_IndexDevelopment")
setwd(wd)

d <- read.csv("AllAverageData_20150218.csv", header = TRUE)



## Models for table
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

## model summary list
mslist <- list(summary(m1), summary(m2), summary(m3), summary(m4), summary(m5),
              summary(m6), summary(m7), summary(m8), summary(m9), summary(m10))

## model list
mlist <- list(m1, m2, m3, m4, m5, m6, m7, m8, m9, m10)

# helper to calculate p values
lmp <- function (modelsummary) {
        f <- modelsummary$fstatistic
        p <- pf(f[1],f[2],f[3],lower.tail=F)
        attributes(p) <- NULL
        return(p)
}

# elements for table: model name, adj r, p value, AIC
# model names
models <- c("m1", "m2", "m3", "m4", "m5", "m6", "m7", "m8", "m9", "m10")

# extract adj r squared
adj <- lapply(mslist, function(x) x$adj.r.squared)

## extract p values
p <- lapply(mslist, lmp)


## calculate AIC scores
aic <- lapply(mlist, function(x) AIC(x))


## construct table
df <- data.frame(models, adj = unlist(adj), pval = unlist(p), 
                 AIC = unlist(aic), stringsAsFactors = FALSE)
table <- xtable(df, digits = c(0, 0, 3, -3, 2)) # neg value forces sci notation
print.xtable(table, type="html", file="test.html")


## summary plots
png(filename = "R_analysis\\Meelup\\mod4 summary plots.png", width = 600, height = 480)
par(mfrow = c(2,2))
plot(m4)
par(mfrow( = c(1,1)))
dev.off()

## function to extract line equation
lm_eqn = function(m) {
        
        l <- list(a = format(coef(m)[1], digits = 2),
                  b = format(abs(coef(m)[2]), digits = 2),
                  r2 = format(summary(m)$adj.r.squared, digits = 3));
        
        if (coef(m)[2] >= 0)  {
                eq <- substitute(italic(y) == a + b %.% italic(x)*","~~italic(adj.r)^2~"="~r2,l)
        } else {
                eq <- substitute(italic(y) == a - b %.% italic(x)*","~~italic(adj.r)^2~"="~r2,l)    
        }
        
        as.character(as.expression(eq));                 
}

## model 4 plot index 235 and pfc PFCTVIF10.Bitt
ggplot(d, aes(x = Index.2.3.5, y = PFCTVIF10.Bitt)) +
        geom_point() +
        geom_smooth(method = "lm", se = FALSE) +
        theme_bw() +
        labs(x = "Index", y = "Projected foliage cover")+
        annotate("text", x = 87, y = 50, label = lm_eqn(m4), colour="black", 
                 size = 3, parse=TRUE)
ggsave("./R_analysis/Meelup/mod4.png", width=6, height=4, dpi=300)


## Correlations
png("./R_analysis/Meelup/PairsPlot.png", width = 1100, height = 1100)
ggpairs(d[,c("PFCTemplate","PFCIF10.Bitt",
                       "PFCIF20.Cano","PFCTVIF10.Bitt",
                       "Band1","Band2","Band3","Band4",
                       "Band5","Band6","Index35","Index.2.3.5")])
dev.off()
plot(density(d$PFCTVIF10.Bitt, na.rm = TRUE))



