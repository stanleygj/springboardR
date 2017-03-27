
#Load Dataset

noshowraw <- read_csv("F:/0Springboard/No Show Analysis/No-show-Issue-Comma-300k.csv")

#Describe Dataset
str(noshowraw)
glimpse(noshowraw)
summary(noshowraw)

noshowraw1<-noshowraw

#Data Wrangling

#1Age transformation

noshowraw1 %<>%
  mutate(Age=abs(Age))

summary(noshowraw1$Age)

#2Transforming integers to Factor

cols<-c("Gender","Status","Diabetes","Alcoolism","HiperTension","Handcap","Smokes","Scholarship","Tuberculosis")

noshowraw1 %<>%
  mutate_each_(funs(factor(.)),cols)

str(noshowraw1)

#Exploratory Data Analysis

table(noshowraw1$Sms_Reminder)

summary(noshowraw1$AwaitingTime)
# The awaitingTime is the number of days someone awaits, 
#that means it is the difference between the two data variables AppointmentRegistraion & AppointmentData

summary(noshowraw1$Scholarship)
#the Brazilian government gives this scholarship as a subsidy for very low-income families
#if they have they children going to school and other more specific requirements. 

# No Shows by Age
Age <- ggplot(noshowraw1, aes(x=Status, y=Age, col=Status)) + geom_boxplot()

# No shows by Gender
g_Gender_1 <- ggplot(noshowraw1, aes(x=Gender, fill=Gender)) + geom_bar(position="dodge")
ggplot(noshowraw1, aes(x=Gender, fill=Status)) + geom_bar(position="fill")

# No shows by day of week

ggplot(noshowraw1, aes(x=DayOfTheWeek, fill=Status )) + geom_bar(position="fill") + theme(axis.text.x = element_text(angle = 45, hjust = 1))

# No shows by Alcholism
g_Alcoolism <- ggplot(noshowraw1, aes(x=Alcoolism, fill=Status)) + geom_bar(position="fill")

# No shows by Scholarship
g_Scholarship <- ggplot(noshowraw1, aes(x=Scholarship, fill=Status)) + geom_bar(position="fill")

# Data Mining

#Split dataset to test and train
set.seed(100) 
testnrow <- sample(nrow(noshowraw1),0.3*nrow(noshowraw1))  
noshowraw1.train <- noshowraw1[-testnrow,]  
noshowraw1.test <- noshowraw1[testnrow,] 

nrow(noshowraw1.test)
nrow(noshowraw1.train)

summary(noshowraw1.test)
summary(noshowraw1.train)

#Checking proportions of no show in test and train

27191/(27191+62809)
#[1] 0.3021222
63540/(63540+146460)
#[1] 0.3025714
27191/300000
#[1] 0.09063667
63540/300000
#[1] 0.2118
9+21
#[1] 30
27191+63540/300000
#[1] 27191.21
(27191+63540)/300000
#[1] 0.3024367

#building the model
glm.1 <- glm(Status~.,data=noshowraw1.train,family=binomial)
summary(glm.1)

glm1.probs <- predict(glm.1,noshowraw1.test,type="response")

summary(glm1.probs)

table(noshowraw1.test$Status,glm1.probs>0.6)

#         FALSE  TRUE
#No-Show   147 27044
#Show-Up   192 62617


plot(glm1.probs)

# Second model

glm.2<-glm(Status~Alcoolism+AwaitingTime,data = noshowraw1.train,family = binomial)
summary(glm.2)

glm2.probs <- predict(glm.2,newdata=noshowraw1.test,type="response")

table(noshowraw1.test$Status,glm2.probs>0.71)

table(noshowraw1$Status,noshowraw1$Scholarship)

glm2.probs <- predict(glm.2,type="response",newdata=noshowraw1.test)

