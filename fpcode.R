# Modeling Final Project Code

# setting working directory 
setwd("~/Desktop/MathModeling/Final Project")
library(dplyr)
library(ggplot2)
library(tidyr)
library(tidyverse)
library(plotly)
library(tidytext)

# Load dataset 'dat' and modify the dataset for the analysis and model 
library(readxl)
Mental_Health_in_Tech_Survey_Responses_ <- read_excel("Mental Health in Tech Survey (Responses).xlsx")
dat <- Mental_Health_in_Tech_Survey_Responses_
dat <- dat[,-1] # remove timestamp column 
dat <- dat[,-26] # remove comments column
names(dat) <- tolower(names(dat)) # lower-case the column name 
names(dat)[names(dat) == "if you live in the united states, which state or territory do you live in?"] <- "state"
names(dat)[names(dat) == "are you self-employed?"] <- "employed"
names(dat)[names(dat) == "do you have a family history of mental illness?"] <- "family"
names(dat)[names(dat) == "have you sought treatment for a mental health condition?"] <- "treatment"
names(dat)[names(dat) == "if you have a mental health condition, do you feel that it interferes with your work?"] <- "interfere"

names(dat)[names(dat) == "how many employees does your company or organization have?"] <- "numemployee"
names(dat)[names(dat) == "do you work remotely (outside of an office) at least 50% of the time?"] <- "remote"
names(dat)[names(dat) == "is your employer primarily a tech company/organization?"] <- "tech"

# Employer's perspective 
names(dat)[names(dat) == "do you know the options for mental health care your employer provides?"] <- "knowoption"
names(dat)[names(dat) == "has your employer ever discussed mental health as part of an employee wellness program?"] <- "discusswellness"
names(dat)[names(dat) == "is your anonymity protected if you choose to take advantage of mental health or substance abuse treatment resources?"] <- "anonymity"
names(dat)[names(dat) == "does your employer provide mental health benefits?"] <- "benefit"
names(dat)[names(dat) == "does your employer provide resources to learn more about mental health issues and how to seek help?"] <- "provideresource"
names(dat)[names(dat) == "do you feel that your employer takes mental health as seriously as physical health?"] <- "employerseriousmental"

# Employee's perspective 
names(dat)[names(dat) == "how easy is it for you to take medical leave for a mental health condition?"] <- "easyleave"
names(dat)[names(dat) == "do you think that discussing a mental health issue with your employer would have negative consequences?"] <- "negativeresult"
names(dat)[names(dat) == "do you think that discussing a physical health issue with your employer would have negative consequences?"] <- "physicalnegativeresult"
names(dat)[names(dat) == "would you be willing to discuss a mental health issue with your coworkers?"] <- "discusscoworker"
names(dat)[names(dat) == "would you be willing to discuss a mental health issue with your direct supervisor(s)?"] <- "discusssupervisor"
names(dat)[names(dat) == "would you bring up a mental health issue with a potential employer in an interview?"] <- "interview"
names(dat)[names(dat) == "would you bring up a physical health issue with a potential employer in an interview?"] <- "interviewphysical"
names(dat)[names(dat) == "have you heard of or observed negative consequences for coworkers with mental health conditions in your workplace?"] <- "observenegative"

# Re-naming the gender column
dat$gender[dat$gender == "male"] <- "Male"
dat$gender[dat$gender == "m"] <- "Male"
dat$gender[dat$gender == "M"] <- "Male"
dat$gender[dat$gender == "Mail"] <- "Male"
dat$gender[dat$gender == "maile"] <- "Male"
dat$gender[dat$gender == "Make"] <- "Male"
dat$gender[dat$gender == "Mal"] <- "Male"
dat$gender[dat$gender == "Male (CIS)"] <- "Male"
dat$gender[dat$gender == "Male-ish"] <- "Male"
dat$gender[dat$gender == "Malr"] <- "Male"
dat$gender[dat$gender == "Man"] <- "Male"
dat$gender[dat$gender == "msle"] <- "Male"
dat$gender[dat$gender == "something kinda male?"] <- "Male"
dat$gender[dat$gender == "cis male"] <- "Male"
dat$gender[dat$gender == "Cis Male"] <- "Male"
dat$gender[dat$gender == "Cis Man"] <- "Male"
dat$gender[dat$gender == "Guy (-ish) ^_^"] <- "Male"

dat$gender[dat$gender == "Cis Female"] <- "Female"
dat$gender[dat$gender == "cis-female/femme"] <- "Female"
dat$gender[dat$gender == "f"] <- "Female"
dat$gender[dat$gender == "F"] <- "Female"
dat$gender[dat$gender == "femail"] <- "Female"
dat$gender[dat$gender == "Femake"] <- "Female"
dat$gender[dat$gender == "female"] <- "Female"
dat$gender[dat$gender == "Female (cis)"] <- "Female"
dat$gender[dat$gender == "woman"] <- "Female"
dat$gender[dat$gender == "Woman"] <- "Female"

dat$gender[dat$gender == "A little about you"] <- "Others"
dat$gender[dat$gender == "Agender"] <- "Others"
dat$gender[dat$gender == "All"] <- "Others"
dat$gender[dat$gender == "Androgyne"] <- "Others"
dat$gender[dat$gender == "Enby"] <- "Others"
dat$gender[dat$gender == "Female (trans)"] <- "Others"
dat$gender[dat$gender == "fluid"] <- "Others"
dat$gender[dat$gender == "Genderqueer"] <- "Others"
dat$gender[dat$gender == "male leaning androgynous"] <- "Others"
dat$gender[dat$gender == "Nah"] <- "Others"
dat$gender[dat$gender == "Neuter"] <- "Others"
dat$gender[dat$gender == "non-binaryr"] <- "Others"
dat$gender[dat$gender == "non-binary"] <- "Others"
dat$gender[dat$gender == "ostensibly male, unsure what that really means"] <- "Others"
dat$gender[dat$gender == "p"] <- "Others"
dat$gender[dat$gender == "queer"] <- "Others"
dat$gender[dat$gender == "queer/she/they"] <- "Others"
dat$gender[dat$gender == "Trans woman"] <- "Others"
dat$gender[dat$gender == "Trans-female"] <- "Others"

dat$age[dat$age == -29] = 29
dat$age[dat$age == -1726] = 26 
dat$age[dat$age == -1] = 20
dat$age[dat$age == 3.29e+02] = 50
dat$age[391] = 50

# Only tech people data 
Techdat <- dat %>%
  + filter(dat$tech == "Yes")
# Among tech people, only mental treatment
mental <- Techdat %>%
  + filter(Techdat$treatment == "Yes")




# Data Analysis 
# 1. Age Distribution 
summary(dat$age)
# This is for the entire respondents 
ggplot(dat,aes(x=dat$age))+geom_histogram(aes(y=..Density..),fill="#62AB61")+geom_density(col="#3438BD",size=1)+labs(x="Age",title="Distribution of Age")


#2. Gender
# This is for the Tech 
piegender <- Techdat %>%
       count(gender) %>% # Counting the people in "family_history" to put in chart
       plot_ly(
             labels = ~gender,
             values = ~n,
             type = "pie", # Setting type of chart to pie
             textposition = 'inside', # Setting area for text
             textinfo = 'label+percent', # Setting labels
             font = list(size = 20),
             hoverinfo = 'text', # Setting text on hover (see text variable on next line)
             text = ~paste(n, "Respondents"), # Setting text on hover
             textfont = list(color = "#000000", size = 18),
             marker = list(colors = cbgender)) %>% # Setting up colors for clarity
       layout(title = "Percentage of Gender in Tech Industry")

piegender

# Among Mental illness
piementalgender <- mental %>%
       count(gender) %>% 
       plot_ly(
             labels = ~gender,
             values = ~n,
             type = "pie", # Setting type of chart to pie
             textposition = 'inside', # Setting area for text
             textinfo = 'label+percent', # Setting labels
             hoverinfo = 'text', # Setting text on hover (see text variable on next line)
             text = ~paste(n, "Respondents"), # Setting text on hover
             marker = list(colors = cbgender)) %>% # Setting up colors for clarity
       layout(title = "Percentage of Gender among People with MI in Tech Industry")
piementalgender

#3. Family History 
# Tech Industry 
piefamily <- Techdat %>%
      count(family) %>% # Counting the people in "family_history" to put in chart
       plot_ly(
             labels = ~family,
             values = ~n,
             type = "pie", # Setting type of chart to pie
             textposition = 'inside', # Setting area for text
             textinfo = 'label+percent', # Setting labels
             hoverinfo = 'text', # Setting text on hover (see text variable on next line)
             text = ~paste(n, "Respondents"), # Setting text on hover
             marker = list(colors = cb)) %>% # Setting up colors for clarity
       layout(title = "Percentage of Family History of Mental Illness in Tech Industry")

piefamily

piementalfamily <- mental %>%
       count(family) %>% 
       plot_ly(
             labels = ~family,
             values = ~n,
             type = "pie", # Setting type of chart to pie
             textposition = 'inside', # Setting area for text
             textinfo = 'label+percent', # Setting labels
             hoverinfo = 'text', # Setting text on hover (see text variable on next line)
             text = ~paste(n, "Respondents"), # Setting text on hover
             marker = list(colors = cb)) %>% # Setting up colors for clarity
       layout(title = "Percentage of family history among People with MI in Tech Industry")

piementalfamily

# 3. Interfere in mental illness 
cbinterfere <- c("Often" = "#99CCFF", "Never" = "#FF9999", "Rarely" = "#FFFF99", "Sometimes" = "#99FFCC", "No Answer" = "#E0E0E0")
pieinterferemental<-mental %>%
        count(interfere) %>% # Counting the people in "work_interfere" to put in chart
         plot_ly(
             labels = ~interfere,
             values = ~n,
             type = "pie", # Setting type of chart to pie
             textposition = 'inside', # Setting area for text
             textinfo = 'label+percent', # Setting labels
             hoverinfo = 'text', # Setting text on hover (see text variable on next line)
             text = ~paste(n, "Respondents"), # Setting text on hover
             marker = list(colors = cbinterfere)) %>% # Setting up colors for clarity
       layout(title = "Percentage of feeling interference with works among People with mental illness")
pieinterferemental


#---------Employee's Perspective toward mental illness----------------------------

#4. How easy leave for medical leave for a mental illness
cbleave <- c("Very easy" = "#99CCFF", "Very difficult" = "#FF9999", "Somewhat difficult" = "#FFFF99", "Somewhat easy" = "#99FFCC", "Don't Know" = "#E0E0E0")
order_leave <- c("Very easy", "Very difficult", "Somewhat difficult", "Somewhat easy", "Don't Know")

treat
Techdat$treatment <-
       ifelse(Techdat$treatment == "No", "Not Sought Treatment",
       ifelse(Techdat$treatment == "Yes", "Sought Treatment", Techdat$treatment)) %>% as.factor()

barleave<- Techdat %>%
       count(easyleave, treatment) %>% # Counting people to add in chart
       plot_ly(
             x = ~easyleave,
             y = ~n,
             type = "bar", # Setting type of plot to bar
             hoverinfo = 'text', # Setting hover info (look at "text" on next line)
             text = ~paste(n, "Respondents"), # Setting hover info
             color = ~treatment) %>% # setting colors of bars
       layout(title = "How easy to take medical leave for a mental illness in the workplace", # Setting title
                           xaxis = list(title = "Participants' Responses", # Setting up x-axis title
                                                                  categoryorder = 'array',
                                                                  categoryarray = c("Very easy", "Very difficult", "Somewhat difficult", "Somewhat easy")), # Setting up order of bars
                           yaxis = list(title = "Number of Respondents"), # Setting up y-axis title
                           legend = list(x = 0.1, y=0.9)) # Moving legend
barleave

#5. Negative Results of Discussing with employers about mental illness vs physical illness 
order_negative <- c("Maybe", "No", "Yes")
cbnegative <- c("Maybe" = "#CCFF99", "No" = "#FF9999", "Yes" = "#99CCFF")
barnegativeresult <- Techdat %>%
       count(negativeresult, treatment) %>% # Counting people by category to add in chart
       plot_ly(
             x = ~negativeresult,
             y = ~n,
             type = "bar", # Setting type of plot to bar
             hoverinfo = 'text', # Setting hover info (look at "text" on next line)
             text = ~paste(n, "Respondents"), # Setting hover info
             color = ~treatment) %>% # setting colors of bars
       layout(title = "Opinions about Negative Results by Discussing mental illness with Employers", # Setting title
                           xaxis = list(title = "Participants' Responses", # Setting up x-axis title
                                                                 categoryorder = 'array',
                                                               categoryarray = order_negative), # Setting up order of bars
                      yaxis = list(title = "Number of Respondents"),
                      legend = list(x = 0.7, y = 0.7)) # Setting up y-axis title
barnegativeresult

barphysicalnegativeresult <- Techdat %>%
       count(physicalnegativeresult, treatment) %>% # Counting people by category to add in chart
      plot_ly(
          x = ~physicalnegativeresult,
           y = ~n,
             type = "bar", # Setting type of plot to bar
             hoverinfo = 'text', # Setting hover info (look at "text" on next line)
             text = ~paste(n, "Respondents"), # Setting hover info
             color = ~treatment) %>% # setting colors of bars
       layout(title = "Opinions about Negative Results by Discussing physical illness with Employers", # Setting title
                       xaxis = list(title = "Participants' Responses", # Setting up x-axis title
                                                       categoryorder = 'array',
                                                                  categoryarray = order_negative), # Setting up order of bars
                          yaxis = list(title = "Number of Respondents"),
                           legend = list(x = 0.7, y = 0.7)) # Setting up y-axis title
barphysicalnegativeresult

#6. Discuss a mental health issue with coworkers - supervisors
order_discuss <- c("No", "Some of them", "Yes")
cbdiscuss <- c("No" = "#FF9999", "Some of them" = "#CCFF99", "Yes" = "#99CCFF")
bardiscusscoworker <-Techdat %>%
       count(discusscoworker, treatment) %>% # Counting people by category to add in chart
       plot_ly(
             x = ~discusscoworker,
             y = ~n,
             type = "bar", # Setting type of plot to bar
             hoverinfo = 'text', # Setting hover info (look at "text" on next line)
             text = ~paste(n, "Respondents"), # Setting hover info
             color = ~treatment) %>% # setting colors of bars
       layout(title = "Willingness to discuss mental illness with co-workers", # Setting title
                           xaxis = list(title = "Responses", # Setting up x-axis title
                                                                  categoryorder = 'array',
                                                                  categoryarray = order_discuss), # Setting up order of bars
                           yaxis = list(title = "Number of Respondents"), # Setting up y-axis title
                           legend = list(x = 0.7, y=0.7)) # Moving legend
bardiscusscoworker

bardiscusssupervisor <-Techdat %>%
       count(discusssupervisor, treatment) %>% # Counting people by category to add in chart
       plot_ly(
             x = ~discusssupervisor,
             y = ~n,
             type = "bar", # Setting type of plot to bar
             hoverinfo = 'text', # Setting hover info (look at "text" on next line)
             text = ~paste(n, "Respondents"), # Setting hover info
             color = ~treatment) %>% # setting colors of bars
       layout(title = "Willingness to discuss mental illness with supervisors", # Setting title
                           xaxis = list(title = "Responses", # Setting up x-axis title
                                                                 categoryorder = 'array',
                                                                  categoryarray = order_discuss), # Setting up order of bars
                           yaxis = list(title = "Number of Respondents"), # Setting up y-axis title
                           legend = list(x = 0.1, y=0.95)) # Moving legend
bardiscusssupervisor

#7. Discuss mental vs physical issue during interview of recruitment 

order_interview <- c("Maybe", "No", "Yes")
barinterviewmental <- Techdat %>%
       count(interview, treatment) %>% # Counting people by category to add in chart
       plot_ly(
             x = ~interview,
             y = ~n,
             type = "bar", # Setting type of plot to bar
             hoverinfo = 'text', # Setting hover info (look at "text" on next line)
             text = ~paste(n, "Respondents"), # Setting hover info
             color = ~treatment) %>% # setting colors of bars
       layout(title = "Opinions about telling mental health issues to employers during interview of recruitment", # Setting title
                        xaxis = list(title = "Participants' Responses", # Setting up x-axis title
                                                            categoryorder = 'array',
                                                           categoryarray = order_interview), # Setting up order of bars
                    yaxis = list(title = "Number of Respondents"),
                     legend = list(x = 0.7, y = 0.7)) # Setting up y-axis title
barinterviewmental

barinterviewphysical <- Techdat %>%
       count(interviewphysical, treatment) %>% # Counting people by category to add in chart
       plot_ly(
             x = ~interviewphysical,
             y = ~n,
             type = "bar", # Setting type of plot to bar
             hoverinfo = 'text', # Setting hover info (look at "text" on next line)
             text = ~paste(n, "Respondents"), # Setting hover info
             color = ~treatment) %>% # setting colors of bars
       layout(title = "Opinions about telling physical health issues to employers during interview of recruitment", # Setting title
                          xaxis = list(title = "Participants' Responses", # Setting up x-axis title
                                                          categoryorder = 'array',
                                                            categoryarray = order_interview), # Setting up order of bars
                        yaxis = list(title = "Number of Respondents"),
                        legend = list(x = 0.7, y = 0.65)) # Setting up y-axis title

barinterviewphysical

#8. Observe negative sequences of mental health issues in the workplace 
order_observe <- c("No", "Yes")
barobserve <- Techdat %>%
       count(observenegative, treatment) %>% # Counting people by category to add in chart
       plot_ly(
             x = ~observenegative,
             y = ~n,
             type = "bar", # Setting type of plot to bar
             hoverinfo = 'text', # Setting hover info (look at "text" on next line)
             text = ~paste(n, "Respondents"), # Setting hover info
             color = ~treatment) %>% # setting colors of bars
       layout(title = "Observation of negative results of co-workers with mental illness in the workplace", # Setting title
                           xaxis = list(title = "Participants' Responses", # Setting up x-axis title
                                                             categoryorder = 'array',
                                                             categoryarray = order_observe), # Setting up order of bars
                          yaxis = list(title = "Number of Respondents"),
                        legend = list(x = 0.7, y = 0.7)) # Setting up y-axis title

barobserve

#---------Employee's Perspective toward employer's thoughts about mental illness----------------------------

#1. Employer provide mental health Benefits? 

order_benefit <- c("Don't know", "No", "Yes")
barbenefit<- Techdat %>%
       count(benefit, treatment) %>% # Counting people by category to add in chart
       plot_ly(
             x = ~benefit,
             y = ~n,
             type = "bar", # Setting type of plot to bar
             hoverinfo = 'text', # Setting hover info (look at "text" on next line)
             text = ~paste(n, "Respondents"), # Setting hover info
             color = ~treatment) %>% # setting colors of bars
       layout(title = "Existence of Employers' Offers of mental illness benefits", # Setting title
                           xaxis = list(title = "Participants' Responses", # Setting up x-axis title
                                                                categoryorder = 'array',
                                                               categoryarray = order_benefit), # Setting up order of bars
                          yaxis = list(title = "Number of Respondents"), # Setting up y-axis title
                           legend = list(x = 0.1, y=0.95)) # Moving legend
barbenefit

#2. Know the options for mental health care that employer provide? 
order_option <- c("No", "Not sure", "Yes")
baroption <- Techdat %>%
       count(knowoption, treatment) %>% # Counting people by category to add in chart
       plot_ly(
             x = ~knowoption,
             y = ~n,
             type = "bar", # Setting type of plot to bar
             hoverinfo = 'text', # Setting hover info (look at "text" on next line)
             text = ~paste(n, "Respondents"), # Setting hover info
             color = ~treatment) %>% # setting colors of bars
       layout(title = "Employees' Perceivement of Mental Care Options from Employers", # Setting title
                           xaxis = list(title = "Participants' Responses", # Setting up x-axis title
                                                                  categoryorder = 'array',
                                                                  categoryarray = order_option), # Setting up order of bars
                           yaxis = list(title = "Number of Respondents"),
                           legend = list(x = 0.5, y = 0.9)) # Setting up y-axis title
baroption

#3. Has discussed with your employer mental health as part of an employee wellness program 
order_wellness <- c("Don't know", "No", "Yes")
barwellness <- Techdat %>%
  count(discusswellness, treatment) %>% # Counting people by category to add in chart
  plot_ly(
    x = ~discusswellness,
    y = ~n,
    type = "bar", # Setting type of plot to bar
    hoverinfo = 'text', # Setting hover info (look at "text" on next line)
    text = ~paste(n, "Respondents"), # Setting hover info
    color = ~treatment) %>% # setting colors of bars
  layout(title = "Employees' experience about discussing with employers mental health as one of wellness program", # Setting title
         xaxis = list(title = "Participants' Responses", # Setting up x-axis title
                      categoryorder = 'array',
                      categoryarray = order_wellness), # Setting up order of bars
         yaxis = list(title = "Number of Respondents"),
         legend = list(x = 0.1, y = 0.9)) # Setting up y-axis title
barwellness

#4. Does your employer provide resources to learn more about mental health issues and how to seek help? 
order_resource <- c("Don't know", "No", "Yes")
barresource <- Techdat %>%
  count(provideresource, treatment) %>% # Counting people by category to add in chart
  plot_ly(
    x = ~provideresource,
    y = ~n,
    type = "bar", # Setting type of plot to bar
    hoverinfo = 'text', # Setting hover info (look at "text" on next line)
    text = ~paste(n, "Respondents"), # Setting hover info
    color = ~treatment) %>% # setting colors of bars
  layout(title = "Employers' Provision of Mental Health Care Resources", # Setting title
         xaxis = list(title = "Participants' Responses", # Setting up x-axis title
                      categoryorder = 'array',
                      categoryarray = order_resource), # Setting up order of bars
         yaxis = list(title = "Number of Respondents"),
         legend = list(x = 0.7, y = 0.8)) # Setting up y-axis title

barresource


#5. Anonymity protected if using mental health resources 

order_anonymity <- c("Don't know", "No", "Yes")
baranonymity <- Techdat %>%
  count(anonymity, treatment) %>% # Counting people by category to add in chart
  plot_ly(
    x = ~anonymity,
    y = ~n,
    type = "bar", # Setting type of plot to bar
    hoverinfo = 'text', # Setting hover info (look at "text" on next line)
    text = ~paste(n, "Respondents"), # Setting hover info
    color = ~treatment) %>% # setting colors of bars
  layout(title = "Protection of Anonymity during the Usages of Mental Health Resources", # Setting title
         xaxis = list(title = "Participants' Responses", # Setting up x-axis title
                      categoryorder = 'array',
                      categoryarray = order_anonymity), # Setting up order of bars
         yaxis = list(title = "Number of Respondents"),
         legend = list(x = 0.5, y = 0.8)) # Setting up y-axis title

baranonymity

#6. Employer consider mental health as seriously as physical health? 

order_serious <- c("Don't know", "No", "Yes")
barserious <- Techdat %>%
  count(employerseriousmental, treatment) %>% # Counting people by category to add in chart
  plot_ly(
    x = ~employerseriousmental,
    y = ~n,
    type = "bar", # Setting type of plot to bar
    hoverinfo = 'text', # Setting hover info (look at "text" on next line)
    text = ~paste(n, "Respondents"), # Setting hover info
    color = ~treatment) %>% # setting colors of bars
  layout(title = "Employee's experience about the employers considering mental health as serious as physical health", # Setting title
         xaxis = list(title = "Participants' Responses", # Setting up x-axis title
                      categoryorder = 'array',
                      categoryarray = order_serious), # Setting up order of bars
         yaxis = list(title = "Number of Respondents"),
         legend = list(x = 0.5, y = 0.8)) # Setting up y-axis title

barserious

#-----------------------------------------------------------------------------------------------------------------------#

# Factor Importance 
# Response Variable = Treatment 

# 1. Set a dataset for prediction model 
predictdat <- Techdat[,-1]
View(predictdat)
predictdat <- Techdat[,-3]
predictdat <- Techdat[,-4]
predictdat <- Techdat[,-5]
predictdat <- predictdat[,-3]
predictdat <- predictdat[,-3]
predictdat <- predictdat[,-7]
predictdat <- predictdat[,-7]

predictdat$age[predictdat$age <= 32] = "< 32"
predictdat$age[predictdat$age > 32] = "> 32"

predictdat$age <- as.factor(predictdat$age)
predictdat$gender <- as.factor(predictdat$gender)
predictdat$family <- as.factor(predictdat$family)
predictdat$interfere <- as.factor(predictdat$interfere)
predictdat$numemployee <- as.factor(predictdat$numemployee)
predictdat$benefit <- as.factor(predictdat$benefit)
predictdat$knowoption <- as.factor(predictdat$knowoption)
predictdat$discusswellness <- as.factor(predictdat$discusswellness)
predictdat$provideresource <- as.factor(predictdat$provideresource)
predictdat$anonymity <- as.factor(predictdat$anonymity)
predictdat$easyleave <- as.factor(predictdat$easyleave)
predictdat$negativeresult <- as.factor(predictdat$negativeresult)
predictdat$physicalnegativeresult <- as.factor(predictdat$physicalnegativeresult)
predictdat$discusscoworker <- as.factor(predictdat$discusscoworker)
predictdat$discusssupervisor <- as.factor(predictdat$discusssupervisor)
predictdat$interview <- as.factor(predictdat$interview)
predictdat$interviewphysical <- as.factor(predictdat$interviewphysical)
predictdat$employerseriousmental <- as.factor(predictdat$employerseriousmental)
predictdat$observenegative <- as.factor(predictdat$observenegative)


#--------------------Stepwise Regression ----------------

library(MASS)

logit_personal <- glm(treatment~age+gender+family, data = predictdat, family = binomial)
summary(logit_personal)

forward <- stepAIC(logit_personal, direction = "forward", trace = FALSE)
forward$anova
backward <- stepAIC(logit_personal, direction = "backward", trace = FALSE)
backward$anova
step <- stepAIC(logit_personal, direction = "both", trae = FALSE)
step$anova 

# Result: treatment ~ age + gender + family (All important)

logit_employee <- glm(treatment~easyleave+negativeresult+discusscoworker+discusssupervisor+interview+observenegative, data = predictdat, family = binomial)
summary(logit_employee)

forward_employee <- stepAIC(logit_employee, direction = "forward", trace = FALSE)
forward_employee$anova
backward_employee <- stepAIC(logit_employee, direction = "backward", trace = FALSE)
backward_employee$anova
step_employee <- stepAIC(logit_employee, direction = "both", trae = FALSE)
step_employee$anova 

# Result: treatment ~ easyleave + negativeresult + discusscoworker + interview + observenegative

logit_employer <- glm(treatment~benefit+knowoption+discusswellness+provideresource+anonymity+employerseriousmental, data = predictdat, family = binomial)
summary(logit_employer)

forward_employer <- stepAIC(logit_employer, direction = "forward", trace = FALSE)
forward_employer$anova
backward_employer <- stepAIC(logit_employer, direction = "backward", trace = FALSE)
backward_employer$anova
step_employer <- stepAIC(logit_employer, direction = "both", trae = FALSE)
step_employer$anova 

# Result: treatment ~ benefit + knowoption + employerseriousmental

predictdat <- predictdat[,-6]
predictdat <- predictdat[,-13]
predictdat <- predictdat[,-16]

logit_all <- glm(treatment~., data = predictdat, family = binomial)
summary(logit_all)

forward_all <- stepAIC(logit_all, direction = "forward", trace = FALSE)
forward_all$anova
backward_all <- stepAIC(logit_all, direction = "backward", trace = FALSE)
backward_all$anova
step_all<- stepAIC(logit_all, direction = "both", trae = FALSE)
step_all$anova 

#Result: treatment ~ gender + family + interfere + knowoption + anonymity + discusscoworker

library(randomForest)
set.seed(100)
train <- sample(nrow(predictdat), 0.7*nrow(predictdat), replace = FALSE)
Trainset <- predictdat[train,]
Validset <- predictdat[-train,]

summary(Trainset)
summary(Validset)

model_rf <- randomForest(treatment~ ., data = Trainset, importance = TRUE, na.action = na.roughfix)

model_rf <- randomForest(treatment~ ., data = Trainset, ntree=500, mtry = 6, importance = TRUE, na.action = na.roughfix)

predtrain_rf <- predict(model_rf, Trainset, type = "class")
table(predtrain_rf, Trainset$treatment)

varImpPlot(model_rf, n.var = 5, type = 2, main = "Top 5 Variable Importance")

#----------------Convert Categorical Data to Numeric Data---------------------
numdat <- predictdat
numdat$age <- Techdat$age

numdat$gender <- factor(numdat$gender, levels = c('Male', 'Female', "Others"), labels = c(1, 2, 3))
numdat$family <- factor(numdat$family, levels = c('No', 'Yes'), labels = c(0,1))
numdat$treatment <- factor(numdat$treatment, levels = c('Not Sought Treatment', 'Sought Treatment'), labels = c(0,1))
numdat$interfere <- factor(numdat$interfere, levels = c('Never', 'Often', 'Rarely', 'Sometimes'), labels = c(0,3,1,2))
numdat$benefit <- factor(numdat$benefit, levels = c("Don't know", "No", "Yes"), labels = c(0.5, 0, 1))

numdat$knowoption <- factor(numdat$knowoption, levels = c("Not sure", "No", "Yes"), labels = c(0.5, 0, 1))
numdat$discusswellness <- factor(numdat$discusswellness, levels = c("Don't know", "No", "Yes"), labels = c(0.5, 0, 1))
numdat$provideresource<- factor(numdat$provideresource, levels = c("Don't know", "No", "Yes"), labels = c(0.5, 0, 1))
numdat$anonymity<- factor(numdat$anonymity, levels = c("Don't know", "No", "Yes"), labels = c(0.5, 0, 1))
numdat$employerseriousmental<- factor(numdat$employerseriousmental, levels = c("Don't know", "No", "Yes"), labels = c(0.5, 0, 1))

numdat$easyleave<- factor(numdat$easyleave, levels = c("Don't know", "Somewhat difficult", "Somewhat easy", "Very difficult", "Very easy"), labels = c(0.5,0.25,0.75, 0, 1))
numdat$employerseriousmental<- factor(numdat$employerseriousmental, levels = c("Don't know", "No", "Yes"), labels = c(0.5, 0, 1))

numdat$negativeresult<- factor(numdat$negativeresult, levels = c("Maybe", "No", "Yes"), labels = c(0.5, 0, 1))
numdat$discusscoworker<- factor(numdat$discusscoworker, levels = c("Some of them", "No", "Yes"), labels = c(0.5, 0, 1))
numdat$discusssupervisor<- factor(numdat$discusssupervisor, levels = c("Some of them", "No", "Yes"), labels = c(0.5, 0, 1))
numdat$interview<- factor(numdat$interview, levels = c("Maybe", "No", "Yes"), labels = c(0.5, 0, 1))
numdat$observenegative <- factor(numdat$observenegative, levels = c('No', 'Yes'), labels = c(0,1))

#-----------------------Mosaic Plot --------------------------------------
library(vcd)
arwork <- group_by(predictdat, treatment, age, family)
arwork1 <- summarise(arwork,  count=n())
a.lm <- loglm(count~ age +treatment+ family, data=arwork1)
a.lm1<-mosaic(a.lm , clip=FALSE, gp_args = list(interpolate = c(1, 1.8)))

grwork <- group_by(predictdat, treatment, gender)
grwork1 <- summarise(grwork,  count=n())
g.lm <- loglm(count~ gender +treatment, data=grwork1)
g.lm1<-mosaic(g.lm , clip=FALSE, gp_args = list(interpolate = c(1, 1.8)))

mtiwork <- group_by(predictdat, treatment, interview)
mtiwork1 <- summarise(mtiwork,  count=n())
mti.lm <- loglm(count~ interview + treatment, data=mtiwork1)
mti.lm1<-mosaic(mti.lm , clip=FALSE, gp_args = list(interpolate = c(1, 1.8)))

ctwork <- group_by(predictdat, treatment, knowoption)
ctwork1 <- summarise(ctwork,  count=n())
ct.lm <- loglm(count~ knowoption +treatment, data=ctwork1)
ct.lm1<-mosaic(ct.lm , clip=FALSE, gp_args = list(interpolate = c(1, 1.8)))

itwork <- group_by(predictdat, treatment, interfere)
itwork1 <- summarise(itwork,  count=n())
it.lm <- loglm(count~ interfere +treatment, data=itwork1)
it.lm1<-mosaic(it.lm , clip=FALSE, abbreviate_labs=TRUE)

mcowork <- group_by(predictdat, treatment, discusscoworker)
mcowork1 <- summarise(mcowork,  count=n())
v.lm <- loglm(count~ discusscoworker + treatment, data=mcowork1)
v.m1<-mosaic(v.lm , clip=FALSE, gp_args = list(interpolate = c(1, 1.8)))
msup <- group_by(predictdat, treatment, discusssupervisor)
msup1 <- summarise(msup,  count=n())
v.lm1 <- loglm(count~ discusssupervisor+treatment, data=msup1)
v.m1<-mosaic(v.lm1 , clip=FALSE, gp_args = list(interpolate = c(1, 1.8)))

mseek <- group_by(predictdat, treatment, provideresource)
mseek1 <- summarise(mseek,  count=n())
v.lm2 <- loglm(count~ provideresource+treatment, data=mseek1)
v.m2<-mosaic(v.lm2 , clip=FALSE, gp_args = list(interpolate = c(1, 1.8)))

mment <- group_by(predictdat, treatment, observenegative)
mment1 <- summarise(mment,  count=n())
v.lm4 <- loglm(count~ observenegative+treatment, data=mment1)
v.m4<-mosaic(v.lm4 , clip=FALSE, gp_args = list(interpolate = c(1, 1.8)))

mnegresult <- group_by(predictdat, treatment, negativeresult)
mment1 <- summarise(mnegresult,  count=n())
v.lm4 <- loglm(count~ negativeresult+treatment, data=mment1)
v.m4<-mosaic(v.lm4 , clip=FALSE, gp_args = list(interpolate = c(1, 1.8)))

# Personal Level 
mpersonal <- group_by(predictdat, treatment, age, family, gender)
mpersonal1 <- summarise(mpersonal, count = n())
a.lmpersonal <- loglm(count~age+treatment+family+gender, data = mpersonal1)
a.lmpersonal1 <- mosaic(a.lmpersonal, clip = FALSE, gp_args = list(interpolate = c(1,1.8)))

#------------------------Logistic Regression--------------------------------
set.seed()
