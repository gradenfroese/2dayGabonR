## Make the elephant file more accesible

elephant <- read.csv("RawData/ElephantData.csv", header=T)

# Simplify
elephant <- elephant[elephant$YEAR==1,]
elephant$HomeRange <- round(elephant$KDE.95,2)

elephant <- elephant[,c("Name", "Sex", "Region","HomeRange", "NDVI")]
head(elephant)
names(elephant) <-c("Name","Sex","Region","HomeRange", "NDVI")                       
head(elephant)
elephant$TuskLength <- NA
elephant$GroupSize <- rpois(48,4)
elephant$GroupSize[elephant$Group.Size==0]<-1

n     <- 48                    # length of vector
rho   <- 0.7                   # desired correlation = cos(angle)
theta <- acos(rho)             # corresponding angle
x1    <- elephant$HomeRange        # fixed given data
x2    <- rnorm(n, 2, 0.5)      # new random data
X     <- cbind(x1, x2)         # matrix
Xctr  <- scale(X, center=TRUE, scale=FALSE)   # centered columns (mean 0)

Id   <- diag(n)                               # identity matrix
Q    <- qr.Q(qr(Xctr[ , 1, drop=FALSE]))      # QR-decomposition, just matrix Q
P    <- tcrossprod(Q)          # = Q Q'       # projection onto space defined by x1
x2o  <- (Id-P) %*% Xctr[ , 2]                 # x2ctr made orthogonal to x1ctr
Xc2  <- cbind(Xctr[ , 1], x2o)                # bind to matrix
Y    <- Xc2 %*% diag(1/sqrt(colSums(Xc2^2)))  # scale columns to length 1
x <- Y[ , 2] + (1 / tan(theta)) * Y[ , 1]     # final new vector
cor(x1, x) 

elephant$TuskLength  <- 0.7 + x*2
elephant$TuskLength[elephant$TuskLength<0] <- 0.1
elephant$TuskLength <- round(elephant$TuskLength,2)
plot(elephant$HomeRange, elephant$TuskLength)

elephant$NDVI[10] <- NA

head(elephant)

write.csv(elephant, "ClassData/Elephant.csv", row.names = F)


# Build a disease dataset
eleDisease <-data.frame(Name=elephant$Name, Nematodes=0, Abcess=0, Injury=0) 

eleDisease$Name <- sort(eleDisease$Name)

eleDisease$Nematodes[eleDisease$Name %in% sample(eleDisease$Name,size=13,replace=F)] <- 1
eleDisease$Abcess[eleDisease$Name %in% sample(eleDisease$Name,size=3,replace=F)] <- 1
eleDisease$Injury[eleDisease$Name %in% sample(eleDisease$Name,size=27,replace=F)] <- 1
head(eleDisease)
eleDisease$Disease <- rowSums(eleDisease[,2:4])
eleDisease$Disease <- eleDisease$Disease>0

write.csv(eleDisease, "ClassData/DiseaseData.csv", row.names = F)
