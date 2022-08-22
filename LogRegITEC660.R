# Liam Spoletini
# ITEC 660
# Final Project

# =============================== Setup ========================================
library(tidyverse)

# Read in Data
data = read_csv("~/Downloads/full_data.csv")


# ============== Checking coverage of different features =======================
# Gender
data %>% filter(
  !is.na(domest.intl), !is.na(lead.cat.brd), app.enroll == 1
) %>% count(gender)

# Ed.Level.c = 51,965 
data %>% filter(
  !is.na(ed.level.c), ed.level.c != "Null") %>% select(ed.level.c, id)

# State.c = 34,779
data %>% filter(
  !is.na(state.c), state.c != "Null"
) %>% select(state.c)

# gender = 10,036
data %>% filter(
  !is.na(gender), gender != "Null"
) %>% select(gender)

# Domest.intl = 57,590
data %>% filter(
  !is.na(domest.intl), domest.intl != "Null"
) %>% select(domest.intl)

# App.submit = 1,830
data %>% filter(
  app.submit==1
) %>% select(domest.intl)

# app.enroll = 652
data %>% filter(
  app.enroll==1
) %>% select(domest.intl)

# All the aforementioned
data %>%
  filter(!is.na(ed.level.c),
         !is.na(state.c),
         !is.na(domest.intl),
         !is.na(lead.cat.brd))






# Modelling probability of enrollment using subset of features
data_paid <- data %>%
  filter(!(lead.cat.brd %in% c("Direct Apply", "Web", "Webinar")))
model1 <- glm(app.enroll ~ I(lead.cat.brd == "Direct Apply") +
                I(lead.cat.brd == "Web") +
                I(lead.cat.brd == "Webinar") +
                I(lead.cat.brd == "Display") +
                I(lead.cat.brd == "Paid Social") +
                I(lead.cat.brd == "Email") +
                I(lead.cat.brd == "Paid Search") , data=data %>% filter(app.submit == 1), family="binomial")          

summary(model1)
summary(aov(model1))


# Modelling probability of submission using subset of features
model2 <- glm(app.submit ~ 
                I(lead.cat.brd == "Display") +
                I(lead.cat.brd == "Paid Social") +
                I(lead.cat.brd == "Email") +
                I(lead.cat.brd == "Paid Search") , data=data_paid, family="binomial")          

summary(model2)
summary(aov(model1))

# Now, modelling enrollment given submission
model3 <- glm(app.admit ~ I(lead.cat.brd == "Direct Apply") +
                I(lead.cat.brd == "Web") +
                I(lead.cat.brd == "Webinar") +
                I(lead.cat.brd == "Display") +
                I(lead.cat.brd == "Paid Social") +
                I(lead.cat.brd == "Email") +
                I(lead.cat.brd == "Paid Search") , data=data %>% filter(app.submit == 1), family="binomial")          
summary(model3)
summary(aov(model3))

# Modelling admission given submission
model4 <- glm(app.admit ~ 
                I(lead.cat.brd == "Display") +
                I(lead.cat.brd == "Paid Social") +
                I(lead.cat.brd == "Email") +
                I(lead.cat.brd == "Paid Search") , data=data_paid %>% filter(app.submit == 1), family="binomial")        

summary(model4)
anova(model4)
summary(aov(model4))

# repeating previous model with narrow lead categories
data %>% filter(!(lead.cat.brd %in% c("Direct Apply", "Web", "Webinar"))) %>% count(lead.cat.nrw)
model4 <- glm(app.enroll ~ 
                I(lead.cat.nrw == "Display") + 
                I(lead.cat.nrw == "Email") + 
                I(lead.cat.nrw == "GoogleSEM") + 
                I(lead.cat.nrw == "Facebook") + 
                I(lead.cat.nrw == "Instagram") + 
                I(lead.cat.nrw == "LinkedIn") + 
                I(lead.cat.nrw == "MarinPPC") + 
                I(lead.cat.nrw == "Other") + 
                I(lead.cat.nrw == "OtherPPC") + 
                I(lead.cat.nrw == "Remarket") + 
                I(lead.cat.nrw == "Twitter") , data=data_paid %>% filter(app.submit == 1), family="binomial")        

summary(model4)
anova(model4)
summary(aov(model4))

