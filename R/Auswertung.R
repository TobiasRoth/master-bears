rm(list = ls(all = TRUE))

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Settings ---
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Libraries
library(tidyverse)
library(broom)
library(readxl)
library(arm)

dat <- 
m <- mean(c(10, 20))

c(10, 20) %>% 
  mean()

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Prepare data ---
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Read in data from Excel and change column names / data types
dat <- read_excel("Data/Datasheet_Bear_2.0.xlsx") %>% 
  transmute(day = Collection %>% as.integer(),
            rep = Replication %>% as.integer,
            observer = ID,
            treatment = Setup,
            n_fish = `Number Fish`,
            total = as.integer(`Contact`)  + as.integer(`Contact > 10s`) + as.integer(`2m`) + 
              as.integer(`2m > 10s`) + as.integer(`4m`)  + as.integer(`4m > 10s`) + as.integer(`Response Contact`) +
              as.integer(`Response 2m`) + as.integer(`Response 4m`),
            total_2m = as.integer(`Contact`)  + as.integer(`Contact > 10s`) + as.integer(`2m`) + 
              as.integer(`2m > 10s`) + as.integer(`Response Contact`) +
              as.integer(`Response 2m`),
            Resp = `Response 2m` %>% as.integer())


tapply(dat$Resp, dat$treatment, sum) %>% barplot(las = 1)

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Analyse proportion of events in 2m circle or contact zone ---
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# For example the proportion of observations in the contact zone and the 2m circle
mod <- glmer(cbind(total_2m, total-total_2m) ~ treatment + (1|day), family = binomial, data = dat) 
summary(mod)
simres <- sim(mod, 100)@fixef

# Make plot for BB (intercept) vs WW
BB <- quantile(plogis(simres[, "(Intercept)"]), probs = c(0.025, 0.5, 0.975))
WW <- quantile(plogis(simres[, "(Intercept)"] + simres[, "treatmentWW"]), probs = c(0.025, 0.5, 0.975))
pdf("Results/BBvsWW.pdf", width = 4, height = 5)
par(mar = c(3, 4, 1, 1))
plot(1:2, c(BB["50%"], WW["50%"]), pch = 16, axes = FALSE, 
     ylim = c(0,0.5), ylab = "",
     xlim = c(0.7, 2.3), xlab = "")
segments(x0 = 1:2, y0 = c(BB["2.5%"], WW["2.5%"]), x1 = 1:2, y1 = c(BB["97.5%"], WW["97.5%"]))
axis(1, 1:2, c("BB", "WW"), pos = 0)
lines(c(0.7, 1), c(0, 0))
lines(c(2, 2.3), c(0, 0))
axis(2, las = 1, pos = 0.7)
mtext(text = "Treatment", side = 1, line = 1.5)
mtext(text = "Non-evasive behavior", side = 2, line = 3)
mtext(text = "(proportion of events in 2m circle or contact zone)", side = 2, line = 2.2, cex = 0.8)
dev.off()



