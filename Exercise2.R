titanic1 <- read_excel("F:/0Springboard/DWEx2/titanic3_original.xls")

titanic2<-titanic1[1:1309, ]

titanic2<-titanic2%>%
  mutate(embarked=replace(embarked, is.na(embarked),"S"))

titanic2<-titanic2%>%
  mutate(age=replace(age, is.na(age),mean(age, na.rm=TRUE)))

titanic2<-titanic2%>%
  mutate(boat=replace(boat, is.na(boat),"None"))

titanic2<-titanic2%>%
  mutate(has_cabin_number= ifelse(is.na(cabin), "0", "1"))

summary(titanic2)

View(titanic2)

write.csv(titanic2, "titanic_clean.csv")