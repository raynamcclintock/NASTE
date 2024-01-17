#Quicklook at the TA data from NASTE
#Need to normalize to volume and surface areas of fragments as well as convert to a rate

library(dplyr)
library(ggplot2)
library(car)
library(ggeffects)
library(effects)
library(ggResidpanel)
library(lme4)

setwd("/Users/raynamcclintock/Documents/GitHub/NASTE/TA")

data <- read.csv('NASTE_TA_2023.csv')

light <- data %>% filter(light.dark == "light")
dark <- data %>% filter(light.dark == "dark")
control <- data %>% filter(Treatment == "C")

#ALL DATA
ggplot(data, aes(x = Treatment, y = delTA)) +
  geom_boxplot() + geom_jitter(aes(color = Treatment), width = 0.2, alpha = 0.5) + facet_wrap(~Temp+light.dark) +
  labs(title = "delTA by Treatment", x = "Treatment", y = "delTA (umol/L)")

#ALL Data combo A&H
ggplot(data, aes(x = Treatment, y = delTA)) +
  geom_boxplot() + geom_jitter(aes(color = Treatment), width = 0.2, alpha = 0.5) + facet_wrap(~light.dark) +
  labs(title = "delTA by Treatment", x = "Treatment", y = "delTA (umol/L)")


#LIGHT
ggplot(light, aes(x = Treatment, y = delTA)) +
  geom_boxplot() + geom_jitter(aes(color = Treatment), width = 0.2, alpha = 0.5) + facet_wrap(~Temp) +
  labs(title = "delTA by Treatment", x = "Treatment", y = "delTA (umol/L)")


#DARK
ggplot(dark, aes(x = Treatment, y = delTA)) +
  geom_boxplot() + geom_jitter(aes(color = Treatment), width = 0.2, alpha = 0.5) + facet_wrap(~Temp) +
  labs(title = "delTA by Treatment", x = "Treatment", y = "delTA (umol/L)")


#Make things factors
data$Treatment <- as.factor(data$Treatment)
data$Temp <- as.factor(data$Temp)
data$Date_Sampled <- as.factor(data$Date_Sampled)
data$Titration_Batch <- as.factor(data$Titration_Batch)
data$Temp_Sampled <- as.factor(data$Temp_Sampled)
data$Time_Started <- as.factor(data$Time_Started)

#create the linear model
model = lm(delTA ~ Treatment, data=data)
#Anova type II test
Anova(model, type = 2)
#Plot the Residuals
resid_panel(model)

'
model2 = lm(delTA ~ Treatment*Date_Sampled, data=data)
Anova(model2, type = 2)
resid_panel(model2)

model3 <- lm(delTA ~ Treatment*light.dark, data=data)
Anova(model3, type = 2)
resid_panel(model3)'

model4 <- lm(delTA ~ Treatment*Temp*light.dark, data=data)
Anova(model4, type = 2)
resid_panel(model4)

#See if there was any significant interaction with Titration_Batch
model5 <- lm(delTA ~ Treatment*Temp*light.dark*Titration_Batch, data=data)
Anova(model5, type = 2)
resid_panel(model5)
#think about this more


#See if there was any significant interaction with 
model5 <- lm(delTA ~ Treatment*Temp*light.dark+Temp_Sampled, data=data)
Anova(model5, type = 2)
resid_panel(model5)

model5 <- lm(delTA ~ Treatment*Temp*light.dark*Temp_Sampled*Time_Started, data=data)
Anova(model5, type = 2)
resid_panel(model5)

mixed_model <- lmer(delTA ~ Treatment*light.dark*Temp + (1|Time_Started), data = data)
summary(mixed_model)
Anova(mixed_model, type = 2)
resid_panel(mixed_model)

#Temperature Sampled
ggplot(data, aes(x = Temp_Sampled, y = delTA)) +
  geom_boxplot() + geom_jitter(aes(color = Treatment), width = 0.2, alpha = 0.5) +
  labs(title = "delTA by Temp", x = "Temp", y = "delTA (umol/L)")

#Time Started
ggplot(data, aes(x = Time_Started, y = delTA)) +
  geom_boxplot() + geom_jitter(aes(color = Treatment), width = 0.2, alpha = 0.5) +
  labs(title = "delTA by Time", x = "Time", y = "delTA (umol/L)")

#Date Sampled
ggplot(data, aes(x = Date_Sampled, y = delTA)) +
  geom_boxplot() + geom_jitter(aes(color = Treatment), width = 0.2, alpha = 0.5) +
  labs(title = "delTA by Date", x = "Date", y = "delTA (umol/L)")

#Light vs. Dark
ggplot(data, aes(x = light.dark, y = delTA)) +
  geom_boxplot() + geom_jitter(aes(color = Treatment), width = 0.2, alpha = 0.5) + facet_wrap(~Treatment) +
  labs(title = "delTA by light", x = "light", y = "delTA (umol/L)")

#A vs. H
ggplot(data, aes(x = Temp, y = delTA)) +
  geom_boxplot() + geom_jitter(aes(color = Treatment), width = 0.2, alpha = 0.5) + facet_wrap(~Treatment) +
  labs(title = "delTA by Temp", x = "Temp", y = "delTA (umol/L)")

ggplot(data, aes(x = Temp, y = delTA)) +
  geom_boxplot() + geom_jitter(aes(color = Treatment), width = 0.2, alpha = 0.5) + facet_wrap(~Treatment) +
  labs(title = "delTA by Temp", x = "Temp", y = "delTA (umol/L)")

#plot the effects
effects <- ggeffect(model4, c("Treatment", "Temp", "light.dark"))
plot(effects)

#plot some more effects
model5 <- lm(delTA ~ Treatment*Time_Started, data=data)
Anova(model5, type = 2)
resid_panel(model5)
effects <- ggeffect(model5, c("Treatment", "Time_Started"))
plot(effects)

# Example ANOVA
modellight <- aov(delTA ~ Treatment, data = light)
# Tukey's HSD test for pairwise comparisons
tukey_result_light <- TukeyHSD(modellight)
# Print the results
print(tukey_result_light)

# Example ANOVA
modeldark <- aov(delTA ~ Treatment, data = dark)
# Tukey's HSD test for pairwise comparisons
tukey_result_dark <- TukeyHSD(modeldark)
# Print the results
print(tukey_result_dark)

library(multcomp)
model <- aov(delTA ~ Treatment, data = data)
model <- modeldark
model <- modellight
# Perform Tukey test
tukey_test <- glht(model, linfct = mcp(Treatment = "Tukey"))
# Display letter categories
letters <- cld(tukey_test)
print(letters)
print(tukey_test)


