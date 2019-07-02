# Correlations between present-day range and Delta Range

# import the data
data=read.table("Data-World.txt", h=T)

# Pearson correlation between present-day range and untransformed Delta Range
cor(data$Range0k, data$DeltaRange)

# Pearson correlation between present-day range and Log-transformed Delta Range
cor(data$Range0k, data$LogDR)

## chi-square

# Delta Range
# remove Data Deficient species
worlddata=data[complete.cases(data$IUCN.NUM),]

# organize the data
IUCN=worlddata$IUCN.NUM
DR=(worlddata$LogDR)
data=cbind(IUCN,DR)
data=as.data.frame(data)
data$IUCN2=ifelse(data$IUCN==0, yes="Non-Threatened", no="Threatened") # create categories
data$ID <- seq.int(nrow(data))
data=data[,c(4,3,2)]
data$CatDR=ifelse(data$DR<0, yes="Contraction", no="Expansion") # create categories
head(data)

table(data$IUCN2, data$CatDR)

# chi-square test of indepedence
chisq.test(data$IUCN2, data$CatDR, correct=FALSE)

# Delta Suitability
# remove populations not assessed
worldpopdata=data[complete.cases(data$POP.NUM),]

# organize the data
POP=worldpopdata$POP.NUM
DS=(worldpopdata$LogDS)
data=cbind(POP,DS)
data=as.data.frame(data)
data$POP2=ifelse(data$POP==0, yes="Increasing", no="Decreasing") # create categories
data$ID <- seq.int(nrow(data))
data=data[,c(4,3,2)]
data$CatDS=ifelse(data$DS<0, yes="Contraction", no="Expansion") # create categories
head(data)

table(data$POP2, data$CatDS)

# chi-square test of indepedence
chisq.test(data$POP2, data$CatDS, correct=FALSE)
