rm(list = ls(all = TRUE))

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Settings ---
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Libraries
#install.packages("tidyverse")
#install.packages("broom")
#install.packages("readxl")
#install.packages("arm")

library(tidyverse)
library(broom)
library(readxl)
library(arm)

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Prepare data ---
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#########
#Frage 1# Hier habe ich n_1, n_3 und n_5 hinzugefügt, um alle verschiedenen Kategorien zu haben. Siehe unten im ersten Modell.
#########
# Read in data from Excel and change column names / data types
dat <- read_excel("Bear.xlsx") %>% 
  transmute(day = Collection %>% as.integer,
            rep = Replication %>% as.integer,
            observer = ID,
            treatment = Setup,
            n_fish = `Number Fish`,
            n_0 = as.integer(`Contact`),
		n_1 = as.integer(`Contact > 10s`),
            n_2 = as.integer(`2m`),
		n_3 = as.integer(`2m > 10s`),
            n_4 = as.integer(`4m`),
		n_5 = as.integer(`4m > 10s`),
            n_tot = n_0 + n_1 + n_2 + n_3 + n_4 + n_5,
            resp_0 = as.integer(`Response Contact`),
            resp_2 = as.integer(`Response 2m`),
            resp_4 = as.integer(`Response 4m`))
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Check plausibility of data ---
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# The number of fishes should always be higher than the number of fishes counted
# in the contact zone?
dat %>% filter(n_fish < n_0)

# The number of response should not be higher than the number of observations in
# the respective circle?
dat %>% filter(n_0 < resp_0)
dat %>% filter(n_2 < resp_2)
dat %>% filter(n_4 < resp_4)

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Analyse proportion of events in 2m circle or contact zone BB vs WW
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#########
#Frage 1# Hier habe ich entsprechend das Modell angepasst. Sollte immernoch dasselbe dabei rauskommen, das Resultat ist aber anders. Wieso?
#########
# For example the proportion of observations in the contact zone and the 2m circle
mod <- glmer(cbind(n_0 + n_1 + n_2 + n_3, n_tot - n_0 - n_1 - n_2 - n_3) ~ treatment + (1|day), family = binomial, data = dat) 
summary(mod)
simres <- sim(mod, 100)@fixef

# Make plot for BB (intercept) vs WW
BB <- quantile(plogis(simres[, "(Intercept)"]), probs = c(0.025, 0.5, 0.975))
WW <- quantile(plogis(simres[, "(Intercept)"] + simres[, "treatmentWW"]), probs = c(0.025, 0.5, 0.975))
#########
#Frage 2# Wenn ich ein pdf erstellen will, bekomme ich folgende Fehlermeldung. Wieso? 
#########	"Fehler in pdf("Results/BBvsWW.pdf", width = 4, height = 5) : kann Datei 'Results/BBvsWW.pdf' nicht öffnen"

#pdf("Results/BBvsWW.pdf", width = 4, height = 5)
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
savePlot(filename="C:/Users/eggen/Documents/BBvsWW.jpg", type="jpeg")
#dev.off()

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Analyse proportion of events in 2m circle or contact zone BB vs WW vs FF
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# For example the proportion of observations in the contact zone and the 2m circle
mod <- glmer(cbind(n_0 + n_2, n_tot - n_0 - n_2) ~ treatment + (1|day), family = binomial, data = dat) 
summary(mod)
simres <- sim(mod, 100)@fixef

# Make plot for BB (intercept) vs WW
BB <- quantile(plogis(simres[, "(Intercept)"]), probs = c(0.025, 0.5, 0.975))
WW <- quantile(plogis(simres[, "(Intercept)"] + simres[, "treatmentWW"]), probs = c(0.025, 0.5, 0.975))
FF <- quantile(plogis(simres[, "(Intercept)"] + simres[, "treatmentFF"]), probs = c(0.025, 0.5, 0.975))

par(mar = c(3, 4, 1, 1))
plot(1:3, c(BB["50%"], WW["50%"], FF["50%"]), pch = 16, axes = FALSE, 
     ylim = c(0,0.6), ylab = "",
     xlim = c(0.7, 3.2), xlab = "")
segments(x0 = 1:3, y0 = c(BB["2.5%"], WW["2.5%"], FF["2.5%"]), x1 = 1:3, y1 = c(BB["97.5%"], WW["97.5%"], FF["97.5%"]))
axis(1, 1:3, c("BB", "WW", "FF"), pos = 0)
lines(c(0.7, 1), c(0, 0))
lines(c(2, 3.2), c(0, 0))
axis(2, las = 1, pos = 0.7)
mtext(text = "Treatment", side = 1, line = 1.5)
mtext(text = "Non-evasive behavior", side = 2, line = 3)
mtext(text = "(proportion of events in 2m circle or contact zone)", side = 2, line = 2.2, cex = 0.8)
savePlot(filename="C:/Users/eggen/Documents/BBvsWWvsFF.jpg", type="jpeg")

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Analyse proportion of events in 2m circle or contact zone FB vs FW
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# For example the proportion of observations in the contact zone and the 2m circle
mod <- glmer(cbind(n_0 + n_2, n_tot - n_0 - n_2) ~ treatment + (1|day), family = binomial, data = dat) 
summary(mod)
simres <- sim(mod, 100)@fixef

# Make plot for FB vs FW
FB <- quantile(plogis(simres[, "(Intercept)"] + simres[, "treatmentFB"]), probs = c(0.025, 0.5, 0.975))
FW <- quantile(plogis(simres[, "(Intercept)"] + simres[, "treatmentFW"]), probs = c(0.025, 0.5, 0.975))
par(mar = c(3, 4, 1, 1))
plot(1:2, c(FB["50%"], FW["50%"]), pch = 16, axes = FALSE, 
     ylim = c(0,0.5), ylab = "",
     xlim = c(0.7, 2.3), xlab = "")
segments(x0 = 1:2, y0 = c(FB["2.5%"], FW["2.5%"]), x1 = 1:2, y1 = c(FB["97.5%"], FW["97.5%"]))
axis(1, 1:2, c("FB", "FW"), pos = 0)
lines(c(0.7, 1), c(0, 0))
lines(c(2, 2.3), c(0, 0))
axis(2, las = 1, pos = 0.7)
mtext(text = "Treatment", side = 1, line = 1.5)
mtext(text = "Non-evasive behavior", side = 2, line = 3)
mtext(text = "(proportion of events in 2m circle or contact zone)", side = 2, line = 2.2, cex = 0.8)
savePlot(filename="C:/Users/eggen/Documents/FBvsFW.jpg", type="jpeg")

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Analyse proportion of events in 2m circle or contact zone BF vs WF
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# For example the proportion of observations in the contact zone and the 2m circle
mod <- glmer(cbind(n_0 + n_2, n_tot - n_0 - n_2) ~ treatment + (1|day), family = binomial, data = dat) 
summary(mod)
simres <- sim(mod, 100)@fixef

# Make plot for BF vs WF
WF <- quantile(plogis(simres[, "(Intercept)"] + simres[, "treatmentWF"]), probs = c(0.025, 0.5, 0.975))
BF <- quantile(plogis(simres[, "(Intercept)"] + simres[, "treatmentBF"]), probs = c(0.025, 0.5, 0.975))
par(mar = c(3, 4, 1, 1))
plot(1:2, c(BF["50%"], WF["50%"]), pch = 16, axes = FALSE, 
     ylim = c(0,0.5), ylab = "",
     xlim = c(0.7, 2.3), xlab = "")
segments(x0 = 1:2, y0 = c(BF["2.5%"], WF["2.5%"]), x1 = 1:2, y1 = c(BF["97.5%"], WF["97.5%"]))
axis(1, 1:2, c("BF", "WF"), pos = 0)
lines(c(0.7, 1), c(0, 0))
lines(c(2, 2.3), c(0, 0))
axis(2, las = 1, pos = 0.7)
mtext(text = "Treatment", side = 1, line = 1.5)
mtext(text = "Non-evasive behavior", side = 2, line = 3)
mtext(text = "(proportion of events in 2m circle or contact zone)", side = 2, line = 2.2, cex = 0.8)
savePlot(filename="C:/Users/eggen/Documents/BFvsWF.jpg", type="jpeg")


#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Analyse proportion of events in 2m circle or contact zone Control vs Frame
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# For example the proportion of observations in the contact zone and the 2m circle
mod <- glmer(cbind(n_0 + n_2, n_tot - n_0 - n_2) ~ treatment + (1|day), family = binomial, data = dat) 
summary(mod)
simres <- sim(mod, 100)@fixef

# Make plot for CC vs FF
CC <- quantile(plogis(simres[, "(Intercept)"] + simres[, "treatmentCC"]), probs = c(0.025, 0.5, 0.975))
FF <- quantile(plogis(simres[, "(Intercept)"] + simres[, "treatmentFF"]), probs = c(0.025, 0.5, 0.975))
par(mar = c(3, 4, 1, 1))
plot(1:2, c(CC["50%"], FF["50%"]), pch = 16, axes = FALSE, 
     ylim = c(0,0.6), ylab = "",
     xlim = c(0.7, 2.3), xlab = "")
segments(x0 = 1:2, y0 = c(CC["2.5%"], FF["2.5%"]), x1 = 1:2, y1 = c(CC["97.5%"], FF["97.5%"]))
axis(1, 1:2, c("CC", "FF"), pos = 0)
lines(c(0.7, 1), c(0, 0))
lines(c(2, 2.3), c(0, 0))
axis(2, las = 1, pos = 0.7)
mtext(text = "Treatment", side = 1, line = 1.5)
mtext(text = "Non-evasive behavior", side = 2, line = 3)
mtext(text = "(proportion of events in 2m circle or contact zone)", side = 2, line = 2.2, cex = 0.8)
savePlot(filename="C:/Users/eggen/Documents/CCvsFF.jpg", type="jpeg")


#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Analyse proportion of events short vs long events in 2m circle, BB vs WW
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#########
#Frage 3# Hier mein Versuch, die Events, die länger sind als 10s denjenigen, die kürzer sind als 10s im 2m Ring gegenüberzustellen.
#########	 Kann ich das so machen?

# For example the proportion of observations in the contact zone and the 2m circle
mod <- glmer(cbind(n_3, n_2 - n_3) ~ treatment + (1|day), family = binomial, data = dat) 
summary(mod)
simres <- sim(mod, 100)@fixef
# Make plot for BB (intercept) vs WW
BB <- quantile(plogis(simres[, "(Intercept)"]), probs = c(0.025, 0.5, 0.975))
WW <- quantile(plogis(simres[, "(Intercept)"] + simres[, "treatmentWW"]), probs = c(0.025, 0.5, 0.975))

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
savePlot(filename="C:/Users/eggen/Documents/BBvsWW2m10s.jpg", type="jpeg")

