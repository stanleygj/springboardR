refine2<-read_excel("F:/0Springboard/DWEx1/refine.xlsx") 

refine2$company <- gsub("fillips|phillips|phillipS|Phillips|phillps|phlips|phllips","philips",refine2$company)
refine2$company <- gsub("AKZO|Akzo|akzO|akz0|ak zo","akzo",refine2$company)
refine2$company <- gsub("Van Houten|van Houten|Van houten|van houten","vanhouten",refine2$company)
refine2$company <- gsub("unilver|Unilever","unilever",refine2$company)

refine2<-separate(refine2,'Product code / number', c("product_code", "product_number"),sep="-")

lut<-c("p"="Smartphone","v"="TV","x"="Laptop","q"="Tablet")
refine2$product_category<-lut[refine2$product_code]

refine2<-mutate(refine2,full_address=paste(address,city,country,sep = ','))

refine2<-spread(refine2,company,company,fill=0)

refine2<-refine2%>%
  mutate(akzo=replace(akzo,akzo=="akzo","1"))
  
  refine2<-refine2%>%  
  mutate(philips=replace(philips,philips=="philips","1"))
  
  refine2<-refine2%>%
  mutate(vanhouten = replace(vanhouten,vanhouten == "vanhouten","1"))
  
  refine2<-refine2%>%
  mutate(unilever=replace(unilever,unilever=="unilever","1"))

refine2<-spread(refine2,product_category,product_category,fill=0)

refine2<-refine2%>%
  mutate(Laptop=replace(Laptop,Laptop=="Laptop","1"))

refine2<-refine2%>%
  mutate(Smartphone=replace(Smartphone,Smartphone=="Smartphone","1"))

refine2<-refine2%>%
  mutate(Tablet=replace(Tablet,Tablet=="Tablet","1"))

refine2<-refine2%>%
  mutate(TV=replace(TV,TV=="TV","1"))

refine2<-rename(refine2,product_laptop = Laptop,product_smartphone=Smartphone,product_tablet=Tablet,product_tv=TV)


View(refine2)

write.csv(refine2, "refine_clean.csv")