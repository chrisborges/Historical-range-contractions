# Create a function to run GLMMs and determination coefficent

mam.glmm=function(data, group="All"){
  
  library(lme4) #glmm
  library(piecewiseSEM) # pseudo R2
  
  data=data[complete.cases(data$IUCN.NUM), ] # remove Data Deficient species
  
  if(group=="All"){
    reg.glmm=glmer(IUCN.NUM ~ LogDR + (1|GroupBMg), family=binomial(link="logit"), data=data, control=glmerControl(optimizer="bobyqa"))
    resu=summary(reg.glmm)
    r2=rsquared(reg.glmm, method="theoretical")
  }
  
  
  if(group == "Afrosoricida"){
    data=data[which(data$Order=="AFROSORICIDA"), ]
    reg.glmm=glmer(IUCN.NUM ~ LogDR + (1|GroupBMg), family=binomial(link="logit"), data=data, control=glmerControl(optimizer="bobyqa"))
    resu=summary(reg.glmm)
    r2=rsquared(reg.glmm, method="theoretical")
  }
  
  if(group == "Carnivora"){
    data=data[which(data$Order=="CARNIVORA"), ]
    reg.glmm=glmer(IUCN.NUM ~ LogDR + (1|GroupBMg), family=binomial(link="logit"), data=data, control=glmerControl(optimizer="bobyqa"))
    resu=summary(reg.glmm)
    r2=rsquared(reg.glmm, method="theoretical")
  }
  
  if(group == "Cetartiodactyla"){
    data=data[which(data$Order=="CETARTIODACTYLA"), ]
    reg.glmm=glmer(IUCN.NUM ~ LogDR + (1|GroupBMg), family=binomial(link="logit"), data=data, control=glmerControl(optimizer="bobyqa"))
    resu=summary(reg.glmm)
    r2=rsquared(reg.glmm, method="theoretical")
  }
  
  if(group == "Chiroptera"){
    data=data[which(data$Order=="CHIROPTERA"), ]
    reg.glmm=glmer(IUCN.NUM ~ LogDR + (1|GroupBMg), family=binomial(link="logit"), data=data, control=glmerControl(optimizer="bobyqa"))
    resu=summary(reg.glmm)
    r2=rsquared(reg.glmm, method="theoretical")
  }
  
  if(group == "Cingulata"){
    data=data[which(data$Order=="CINGULATA"), ]
    reg.glmm=glmer(IUCN.NUM ~ LogDR + (1|GroupBMg), family=binomial(link="logit"), data=data, control=glmerControl(optimizer="bobyqa"))
    resu=summary(reg.glmm)
    r2=rsquared(reg.glmm, method="theoretical")
  }
  
  if(group == "Dasyuromorphia"){
    data=data[which(data$Order=="DASYUROMORPHIA"), ]
    reg.glmm=glmer(IUCN.NUM ~ LogDR + (1|GroupBMg), family=binomial(link="logit"), data=data, control=glmerControl(optimizer="bobyqa"))
    resu=summary(reg.glmm)
    r2=rsquared(reg.glmm, method="theoretical")
  }
  
  if(group == "Didelphimorphia"){
    data=data[which(data$Order=="DIDELPHIMORPHIA"), ]
    reg.glmm=glmer(IUCN.NUM ~ LogDR + (1|GroupBMg), family=binomial(link="logit"), data=data, control=glmerControl(optimizer="bobyqa"))
    resu=summary(reg.glmm)
    r2=rsquared(reg.glmm, method="theoretical")
  }
  
  if(group == "Diprotodontia"){
    data=data[which(data$Order=="DIPROTODONTIA"), ]
    reg.glmm=glmer(IUCN.NUM ~ LogDR + (1|GroupBMg), family=binomial(link="logit"), data=data, control=glmerControl(optimizer="bobyqa"))
    resu=summary(reg.glmm)
    r2=rsquared(reg.glmm, method="theoretical")
  }
  
  if(group == "Eulipotyphla"){
    data=data[which(data$Order=="EULIPOTYPHLA"), ]
    reg.glmm=glmer(IUCN.NUM ~ LogDR + (1|GroupBMg), family=binomial(link="logit"), data=data, control=glmerControl(optimizer="bobyqa"))
    resu=summary(reg.glmm)
    r2=rsquared(reg.glmm, method="theoretical")
  }
  
  if(group == "Hyracoidea"){
    data=data[which(data$Order=="HYRACOIDEA"), ]
    reg.glmm=glmer(IUCN.NUM ~ LogDR + (1|GroupBMg), family=binomial(link="logit"), data=data, control=glmerControl(optimizer="bobyqa"))
    resu=summary(reg.glmm)
    r2=rsquared(reg.glmm, method="theoretical")
  }
  
  if(group == "Lagomorpha"){
    data=data[which(data$Order=="LAGOMORPHA"), ]
    reg.glmm=glmer(IUCN.NUM ~ LogDR + (1|GroupBMg), family=binomial(link="logit"), data=data, control=glmerControl(optimizer="bobyqa"))
    resu=summary(reg.glmm)
    r2=rsquared(reg.glmm, method="theoretical")
  }
  
  if(group == "Macroscelidea"){
    data=data[which(data$Order=="MACROSCELIDEA"), ]
    reg.glmm=glmer(IUCN.NUM ~ LogDR + (1|GroupBMg), family=binomial(link="logit"), data=data, control=glmerControl(optimizer="bobyqa"))
    resu=summary(reg.glmm)
    r2=rsquared(reg.glmm, method="theoretical")
  }
  
  if(group == "Monotremata"){
    data=data[which(data$Order=="MONOTREMATA"), ]
    reg.glmm=glmer(IUCN.NUM ~ LogDR + (1|GroupBMg), family=binomial(link="logit"), data=data, control=glmerControl(optimizer="bobyqa"))
    resu=summary(reg.glmm)
    r2=rsquared(reg.glmm, method="theoretical")
  }
  
  if(group == "Paucituberculata"){
    data=data[which(data$Order=="PAUCITUBERCULATA"), ]
    reg.glmm=glmer(IUCN.NUM ~ LogDR + (1|GroupBMg), family=binomial(link="logit"), data=data, control=glmerControl(optimizer="bobyqa"))
    resu=summary(reg.glmm)
    r2=rsquared(reg.glmm, method="theoretical")
  }
  
  if(group == "Peramelemorphia"){
    data=data[which(data$Order=="PERAMELEMORPHIA"), ]
    reg.glmm=glmer(IUCN.NUM ~ LogDR + (1|GroupBMg), family=binomial(link="logit"), data=data, control=glmerControl(optimizer="bobyqa"))
    resu=summary(reg.glmm)
    r2=rsquared(reg.glmm, method="theoretical")
  }
  
  if(group == "Perissodactyla"){
    data=data[which(data$Order=="PERISSODACTYLA"), ]
    reg.glmm=glmer(IUCN.NUM ~ LogDR + (1|GroupBMg), family=binomial(link="logit"), data=data, control=glmerControl(optimizer="bobyqa"))
    resu=summary(reg.glmm)
    r2=rsquared(reg.glmm, method="theoretical")
  }
  
  if(group == "Pholidota"){
    data=data[which(data$Order=="PHOLIDOTA"), ]
    reg.glmm=glmer(IUCN.NUM ~ LogDR + (1|GroupBMg), family=binomial(link="logit"), data=data, control=glmerControl(optimizer="bobyqa"))
    resu=summary(reg.glmm)
    r2=rsquared(reg.glmm, method="theoretical")
  }
  
  if(group == "Pilosa"){
    data=data[which(data$Order=="PILOSA"), ]
    reg.glmm=glmer(IUCN.NUM ~ LogDR + (1|GroupBMg), family=binomial(link="logit"), data=data, control=glmerControl(optimizer="bobyqa"))
    resu=summary(reg.glmm)
    r2=rsquared(reg.glmm, method="theoretical")
  }
  
  if(group == "Primates"){
    data=data[which(data$Order=="PRIMATES"), ]
    reg.glmm=glmer(IUCN.NUM ~ LogDR + (1|GroupBMg), family=binomial(link="logit"), data=data, control=glmerControl(optimizer="bobyqa"))
    resu=summary(reg.glmm)
    r2=rsquared(reg.glmm, method="theoretical")
  }
  
  if(group == "Rodentia"){
    data=data[which(data$Order=="RODENTIA"), ]
    reg.glmm=glmer(IUCN.NUM ~ LogDR + (1|GroupBMg), family=binomial(link="logit"), data=data, control=glmerControl(optimizer="bobyqa"))
    resu=summary(reg.glmm)
    r2=rsquared(reg.glmm, method="theoretical")
  }
  
  if(group == "Scandentia"){
    data=data[which(data$Order=="SCANDENTIA"), ]
    reg.glmm=glmer(IUCN.NUM ~ LogDR + (1|GroupBMg), family=binomial(link="logit"), data=data, control=glmerControl(optimizer="bobyqa"))
    resu=summary(reg.glmm)
    r2=rsquared(reg.glmm, method="theoretical")
  }
  
  if(group== "Methatheria"){
    Didelphimorphia=data[which(data$Order=="DIDELPHIMORPHIA"), ]
    Paucituberculata=data[which(data$Order=="PAUCITUBERCULATA"), ]
    Microbiotheria=data[which(data$Order=="MICROBIOTHERIA"), ]
    Notoryctemorphia=data[which(data$Order=="NOTORYCTEMORPHIA"), ]
    Dasyuromorphia=data[which(data$Order=="DASYUROMORPHIA"), ]
    Diprotodontia=data[which(data$Order=="DIPROTODONTIA"), ]
    Peramelemorphia=data[which(data$Order=="PERAMELEMORPHIA"), ]
    
    data=rbind(Didelphimorphia,Paucituberculata,Microbiotheria,Notoryctemorphia,Dasyuromorphia,Diprotodontia,Peramelemorphia)
    reg.glmm=glmer(IUCN.NUM ~ LogDR + (1|GroupBMg), family=binomial(link="logit"), data=data, control=glmerControl(optimizer="bobyqa"))
    resu=summary(reg.glmm)
    r2=rsquared(reg.glmm, method="theoretical")
  }
  
  if(group== "Afrotheria"){
    Proboscidea=data[which(data$Order=="PROBOSCIDEA"), ]
    Hyracoidea=data[which(data$Order=="HYRACOIDEA"), ]
    Macroscelidea=data[which(data$Order=="MACROSCELIDEA"), ]
    Tubulidentata=data[which(data$Order=="TUBULIDENTATA"), ]
    Afrosoricida=data[which(data$Order=="AFROSORICIDA"), ]
    
    data=rbind(Proboscidea,Hyracoidea,Macroscelidea,Tubulidentata,Afrosoricida) 
    reg.glmm=glmer(IUCN.NUM ~ LogDR + (1|GroupBMg), family=binomial(link="logit"), data=data, control=glmerControl(optimizer="bobyqa"))
    resu=summary(reg.glmm)
    r2=rsquared(reg.glmm, method="theoretical")
  }
  
  if(group== "Xenarthra"){
    Pilosa=data[which(data$Order=="PILOSA"), ]
    Cingulata=data[which(data$Order=="CINGULATA"), ]
    
    data=rbind(Pilosa,Cingulata)
    reg.glmm=glmer(IUCN.NUM ~ LogDR + (1|GroupBMg), family=binomial(link="logit"), data=data, control=glmerControl(optimizer="bobyqa"))
    resu=summary(reg.glmm)
    r2=rsquared(reg.glmm, method="theoretical")
  }
  
  if(group== "Ungulata"){
    Perissodactyla=data[which(data$Order=="PERISSODACTYLA"), ]
    Cetartiodactyla=data[which(data$Order=="CETARTIODACTYLA"), ]
    
    data=rbind(Perissodactyla,Cetartiodactyla)
    reg.glmm=glmer(IUCN.NUM ~ LogDR + (1|GroupBMg), family=binomial(link="logit"), data=data, control=glmerControl(optimizer="bobyqa"))
    resu=summary(reg.glmm)
    r2=rsquared(reg.glmm, method="theoretical")
  }
  
  if(group=="Ferae"){
    Pholidota=data[which(data$Order=="PHOLIDOTA"), ]
    Carnivora=data[which(data$Order=="CARNIVORA"), ]
    
    data=rbind(Pholidota, Carnivora)
    reg.glmm=glmer(IUCN.NUM ~ LogDR + (1|GroupBMg), family=binomial(link="logit"), data=data, control=glmerControl(optimizer="bobyqa"))
    resu=summary(reg.glmm)
    r2=rsquared(reg.glmm, method="theoretical")
  }
  
  if(group== "MinorClades"){
    Dermoptera=data[which(data$Order=="DERMOPTERA"), ]
    Scandentia=data[which(data$Order=="SCANDENTIA"), ]
    Monotremata=data[which(data$Order=="MONOTREMATA"), ]
    
    data=rbind(Dermoptera,Scandentia,Monotremata)
    reg.glmm=glmer(IUCN.NUM ~ LogDR + (1|GroupBMg), family=binomial(link="logit"), data=data, control=glmerControl(optimizer="bobyqa"))
    resu=summary(reg.glmm)
    r2=rsquared(reg.glmm, method="theoretical")
  }
  
  return(list(resu,r2))
  
}


# import data
data=read.table("Data-World.txt", h=T)

# WORLD
mam.glmm(data=data, group="All")
mam.glmm(data, "Afrotheria")
mam.glmm(data, "Chiroptera")
mam.glmm(data, "Eulipotyphla")
mam.glmm(data, "Ferae")
mam.glmm(data, "Carnivora")
mam.glmm(data, "Lagomorpha")
mam.glmm(data, "Methatheria")
mam.glmm(data, "Primates")
mam.glmm(data, "Rodentia")
mam.glmm(data, "Ungulata")
mam.glmm(data, "Xenarthra")
mam.glmm(data, "MinorClades")

mam.glmm(data, "Afrosoricida")
mam.glmm(data, "Cetartiodactyla")
mam.glmm(data, "Perissodactyla")
mam.glmm(data, "Dasyuromorphia")
mam.glmm(data, "Didelphimorphia")
mam.glmm(data, "Diprotodontia")

# not enough N
#mam.glmm(data, "Cingulata") 
#mam.glmm(data, "Hyracoidea")
#mam.glmm(data, "Macroscelidea")
#mam.glmm(data, "Monotremata")
#mam.glmm(data, "Paucituberculata")
#mam.glmm(data, "Peramelemorphia")
#mam.glmm(data, "Pholidota")
#mam.glmm(data, "Pilosa")
#mam.glmm(data, "Scandentia")

# AFR
data=read.table("Afr-Data.txt", h=T)

mam.glmm(data=data, group="All")
mam.glmm(data, "Afrotheria")
mam.glmm(data, "Chiroptera")
mam.glmm(data, "Eulipotyphla")
mam.glmm(data, "Ferae")
mam.glmm(data, "Carnivora")
mam.glmm(data, "Primates")
mam.glmm(data, "Rodentia")
mam.glmm(data, "Ungulata")

mam.glmm(data, "Afrosoricida")
mam.glmm(data, "Cetartiodactyla")
mam.glmm(data, "Perissodactyla")

# AUS
data=read.table("Aus-Data.txt", h=T)

mam.glmm(data=data, group="All")
mam.glmm(data, "Chiroptera")
mam.glmm(data, "Methatheria")
mam.glmm(data, "Rodentia")

mam.glmm(data, "Dasyuromorphia")
mam.glmm(data, "Diprotodontia")

# IND
data=read.table("Ind-Data.txt", h=T)

mam.glmm(data=data, group="All")
mam.glmm(data, "Chiroptera")
mam.glmm(data, "Eulipotyphla")
mam.glmm(data, "Ferae")
mam.glmm(data, "Carnivora")
mam.glmm(data, "Lagomorpha")
mam.glmm(data, "Primates")
mam.glmm(data, "Rodentia")
mam.glmm(data, "Ungulata")

mam.glmm(data, "Cetartiodactyla")
mam.glmm(data, "Perissodactyla")

# NEA
data=read.table("Nea-Data.txt", h=T)

mam.glmm(data=data, group="All")
mam.glmm(data, "Chiroptera")
mam.glmm(data, "Eulipotyphla")
mam.glmm(data, "Carnivora")
mam.glmm(data, "Lagomorpha")
mam.glmm(data, "Rodentia")
mam.glmm(data, "Ungulata")
mam.glmm(data, "Cetartiodactyla")

# NEO
data=read.table("Neo-Data.txt", h=T)

mam.glmm(data=data, group="All")
mam.glmm(data, "Chiroptera")
mam.glmm(data, "Eulipotyphla")
mam.glmm(data, "Carnivora")
mam.glmm(data, "Methatheria")
mam.glmm(data, "Primates")
mam.glmm(data, "Rodentia")
mam.glmm(data, "Ungulata")
mam.glmm(data, "Xenarthra")

mam.glmm(data, "Cetartiodactyla")
mam.glmm(data, "Didelphimorphia")

# PAL
data=read.table("Pal-Data.txt", h=T)

mam.glmm(data=data, group="All")
mam.glmm(data, "Chiroptera")
mam.glmm(data, "Eulipotyphla")
mam.glmm(data, "Ferae")
mam.glmm(data, "Carnivora")
mam.glmm(data, "Lagomorpha")
mam.glmm(data, "Rodentia")
mam.glmm(data, "Ungulata")

mam.glmm(data, "Cetartiodactyla")
mam.glmm(data, "Perissodactyla")
