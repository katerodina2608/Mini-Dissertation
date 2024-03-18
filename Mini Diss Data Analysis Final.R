library(tidyverse)
library(ggplot2)
library(pwr2)
library(kableExtra)
library(ggplot2)
library(broom.mixed)
library(parameters)


#Power Analysis 
drinkdataRaw$Gender <- as.factor(drinkdataRaw$Gender)
table(drinkdataRaw$Gender)
pwr.2way(a=2, b=2, alpha = 0.05, size.A = 10, size.B = 22, f.A = 0.5, f.B = 0.5)


#Data cleaning

#removing unnecessary info
drinkdata <- drinkdataRaw[ -c(1,2,4:17) ]
drinkdata <- drinkdata[ -c(1:3), ]

#removing fake questionnaire items
drinkdata <- drinkdata[ -c(7:14) ]

#naming variables
names(drinkdata) <- c("Status", "Consent", "Gender", "Age", "Consume_Alc", "PID", 
                      "Q1", "Q2", "Q3", "Q4", "Q5", "Q6", "Q7", "Q8", "Female", "Male" )

#removing preview answers
drinkdata = drinkdata[drinkdata$Status != "Survey Preview", ] 

#removing another preview
drinkdata = drinkdata[drinkdata$PID != "Kate's response delete later!", ] 


#removing non-binary
drinkdata = drinkdata[drinkdata$Gender != "Non-binary / third gender", ] 

#removing non-drinkers
drinkdata = drinkdata[drinkdata$Consume_Alc != "no", ] 


#setting alcohol intake questions as numeric
drinkdata$Q1 <- as.numeric(drinkdata$Q1)
drinkdata$Q2 <- as.numeric(drinkdata$Q2)
drinkdata$Q3 <- as.numeric(drinkdata$Q3)
drinkdata$Q4 <- as.numeric(drinkdata$Q4)
drinkdata$Q5 <- as.numeric(drinkdata$Q5)
drinkdata$Q6 <- as.numeric(drinkdata$Q6)
drinkdata$Q7 <- as.numeric(drinkdata$Q7)
drinkdata$Q8 <- as.numeric(drinkdata$Q8)

#replacing NA's with 0
drinkdata[is.na(drinkdata)] <- 0

#converting each variable into units
drinkdata <- drinkdata %>%
  mutate(
    Q1Unit = (568*5.2*Q1)/1000,
    Q2Unit = (175*12*Q2)/1000,
    Q3Unit = (568*5.2*Q3)/1000,
    Q4Unit = (175*12*Q4)/1000,
    Q5Unit = (25*40*Q5)/1000,
    Q6Unit = (25*40*Q6)/1000,
    Q7Unit = (25*40*Q7)/1000,
    Q8Unit = (25*40*Q8)/1000
      )

#creating a sum alcohol unit variable
drinkdata <- drinkdata %>%
  mutate(
    sumUnit = rowSums(drinkdata[, 17:24], na.rm = TRUE)
  )

#removing the other alcohol unit questions
drinkdata <- drinkdata[, -c(7:14, 17:24)]

#Condition variable recoding

drinkdata$Female <- recode(drinkdata$Female, "Results(F,Threat)" = "Threat", "Results(F,control)" = "Control")
drinkdata$Male <- recode(drinkdata$Male, "Results(M,Threat)" = "Threat", "Results(M,Control)" = "Control")


drinkdata <- drinkdata %>%
mutate(
  Condition = paste(drinkdata$Female, drinkdata$Male, sep="")
)

drinkdata <- drinkdata[, -c(7,8)]

drinkdata <- drinkdata %>%
  mutate (
    Condition = factor(Condition,
                   levels = c("Control", "Threat"),
                   labels = c("0", "1")))

drinkdata <- na.omit(drinkdata)
is.na(drinkdata$Condition)

#choosing reference levels
levels(drinkdata$Condition)
drinkdata$Condition <- fct_relevel(drinkdata$Condition, "Control")


#building a model
mdl <- aov(sumUnit ~ Gender * Condition,
           data = drinkdata)

# print results
summary(mdl)

#assumption check
#normality - does not meet the normality assumptions
qqnorm(resid(mdl), main = "Residuals")
qqline(resid(mdl))

shapiro.test(mdl$resid)
hist(mdl$residuals)

#homogeneity of variances
plot(mdl, which = 3)

#high influence cases
drink_diag <- 
  tibble(
    mdl$model,
    residuals = residuals(mdl),
    cooksdistance = cooks.distance(mdl)
  )
#outliers: 3
drink_diag %>% filter(abs(stresiduals)>2) %>%
  filter(cooksdistance> (4/(75-2-1))) %>% 
  kable(caption = "Table 6: Model outliers") %>%
  kable_styling()


drinkdata <- drinkdata %>% filter(!PID %in% c("Kumquat", "爽", "hahaha"))

#new model without influential case
mdl1 <- aov(sumUnit ~ Gender * Condition,
            data = drinkdata)
summary(mdl1)


#Descriptive statistics
drinkdata$Age <- as.numeric(drinkdata$Age)

drinkdata %>%
  summarise(
    Mean_Age = mean(Age),
    SD_Age = sd(Age),
  )
desc <- drinkdata %>%
  summarise(
    mean_units = mean(sumUnit),
    SD = sd(sumUnit)
  )
desc
drinkdata_desc <- drinkdata %>%
  group_by(Gender, Condition) %>%
  summarise(
    Mean_Units = round(mean(sumUnit),2),
    SD = round(sd(sumUnit),2),
    Min = round(min(sumUnit),2),
    Max = round(max(sumUnit),2),
    n = n()
  ) %>%
  kable(caption = "Table 1: Decriptive Statistics") %>%
  kable_styling()

drinkdata_desc

#descriptive stats visualization
descr_boxplot <- 
  ggplot(data = drinkdata, aes(x=Gender, y=sumUnit, colour = Condition))+
  geom_boxplot()+
  labs(x="Gender Threat Condition", y="N Alcohol Units", title = "Lalala")

descr_boxplot







