library(tidyverse)
library(ggplot2)
library(pwr2)
library(kableExtra)
library(ggsignif)
library(broom.mixed)
library(parameters)
library(jtools)


#Data cleaning 1

#removing unnecessary info
drinkdataRaw1 <- drinkdataRaw[ -c(1,2,4:17) ]
drinkdataRaw1 <- drinkdataRaw1[ -c(1:3), ]

#removing fake questionnaire items
drinkdataRaw1 <- drinkdataRaw1[ -c(7:14) ]

#naming variables
names(drinkdataRaw1) <- c("Status", "Consent", "Gender", "Age", "Consume_Alc", "PID", 
                      "Q1", "Q2", "Q3", "Q4", "Q5", "Q6", "Q7", "Q8", "Female", "Male")

#removing preview answers
drinkdataRaw1 <- drinkdataRaw1[drinkdataRaw1$Status != "Survey Preview", ] 

#removing another preview
drinkdataRaw1 <- drinkdataRaw1[drinkdataRaw1$PID != "Kate's response delete later!", ] 

#descriptive statistics raw

drinkdataRaw1$Age <- as.numeric(drinkdataRaw1$Age)
drinkdataRaw1 <- na.omit(drinkdataRaw1)
drinkdataRaw1 %>%
  summarise(
    Mean_Age = mean(Age),
    SD_Age = sd(Age),
  )

gender_countsRaw <- table(drinkdataRaw1$Gender)

gender_countsRaw

#Power Analysis 
pwr.2way(a=2, b=2, alpha = 0.05, size.A = 11, size.B = 32, f.A = 0.5, f.B = 0.5)


#removing non-binary
drinkdata <- drinkdataRaw1[drinkdataRaw1$Gender != "Non-binary / third gender", ] 

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
                   labels = c("Control", "Threat")))

drinkdata <- na.omit(drinkdata)
is.na(drinkdata$Condition)

#choosing reference levels
levels(drinkdata$Condition)
drinkdata$Condition <- fct_relevel(drinkdata$Condition, "Control")


#building a model
mdl <- aov(sumUnit ~ Gender * Condition,
           data = drinkdata)

#high influence cases
drink_diag <- 
  tibble(
    mdl$model,
    residuals = residuals(mdl),
    cooksdistance = cooks.distance(mdl)
  )
#outliers: 3
drink_diag %>% filter(abs(residuals)>2) %>%
  filter(cooksdistance> (4/(75-2-1))) %>% 
  kable(caption = "Table 6: Model outliers") %>%
  kable_styling()


drinkdata <- drinkdata %>% filter(!PID %in% c("Kumquat", "爽", "hahaha"))

#new model without influential case
mdl1 <- aov(sumUnit ~ Gender * Condition,
            data = drinkdata)
summary(mdl1)

#assumption check
#normality - does not meet the normality assumptions
qqnorm(resid(mdl1), main = "Residuals")
qqline(resid(mdl1))

shapiro.test(mdl1$resid)
hist(mdl1$residuals)

#homogeneity of variances
plot(mdl1, which = 3)


#Descriptive statistics
desc <- drinkdata %>%
  summarise(
    mean_units = mean(sumUnit),
    SD = sd(sumUnit)
  )
desc
drinkdata_desc <- drinkdata %>%
  group_by(Gender, Condition) %>%
  summarise(
    Mean_Units = mean(sumUnit),
    SD = sd(sumUnit),
    n = n()
  )
gender_counts <- table(drinkdata$Gender)

gender_counts

drinkdata %>%
  summarize(
    mean_age = mean(Age)
  )

drinkdata_desc
mdl1
#descriptive stats visualization
descr_boxplot <- 
  ggplot(data = mdl1, aes(x=Gender, y=sumUnit, colour = Condition))+
  geom_boxplot()+
  labs(x="Gender Threat Condition", y="N Alcohol Units", title = "Lalala")

# results plot

plot1 <- ggplot(drinkdata_desc) +
  geom_bar(aes(x=Condition, y = Mean_Units), stat = "identity") +
  geom_errorbar(
    aes(x=Condition, ymin = Mean_Units - SD, ymax = Mean_Units + SD, width = 0.2)) +
  facet_wrap(~Gender) +
  scale_x_discrete() + xlab("Condition") +
  ylab("Mean Alcohol Units") +
  theme_minimal()

plot1 +
  theme_apa(
    legend.pos = "right",
    legend.use.title = FALSE,
    legend.font.size = 12,
    x.font.size = 12,
    y.font.size = 12,
    facet.title.size = 12,
    remove.y.gridlines = TRUE,
    remove.x.gridlines = TRUE
  )

