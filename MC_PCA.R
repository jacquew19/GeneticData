#TRAIN DATA
df <- Dataset16
dim(df)
df$Region<- paste(df$Region, df$Position, df$Gene, sep = "_")
df$Position <- NULL
df$Gene <- NULL
df <- t(df)
DF <- cbind(rownames(df), data.frame(df, row.names=NULL))
DF[] <- lapply(DF, as.character)
colnames(DF) <- DF[1, ]
DF <- DF[-1 ,]
colnames(DF)[1] <- "Target"
DF[2,1] <- 'DiseaseA'
DF[4,1] <- 'DiseaseC'
DF[5,1] <- 'DiseaseA'
DF[6,1] <- 'DiseaseC'
DF[7,1] <- 'DiseaseA'
DF[9,1] <- 'Healthy'
DF[10,1] <- 'Healthy'
DF[11,1] <- 'Healthy'
DF[12,1] <- 'Healthy'
DF[14,1] <- 'DiseaseB'
DF[15,1] <- 'DiseaseC'
DF[16,1] <- 'DiseaseB'
Target <- as.factor(DF$Target)
train <- data.frame(sapply(DF[,-1], function(x) as.numeric(as.character(x))))
train$Target <- Target
str(train$Target)

#TEST DATA

df1 <- Dataset24
df1$X__1<- paste(df1$X__1, df1$X__2, df1$X__3, sep = "_")
df1$X__2 <- NULL
df1$X__3 <- NULL
df1$X__4 <- NULL
df1 = df1[-1,]
df1 <- t(df1)
DF1 <- cbind(rownames(df1), data.frame(df1, row.names=NULL))
DF1[] <- lapply(DF1, as.character)
colnames(DF1) <- DF1[1, ]
DF1 <- DF1[-1 ,]
colnames(DF1)[1] <- "Target" 
DF1[2,1] <- 'DiseaseA'
DF1[9,1] <- 'DiseaseB'
DF1[5,1] <- 'UNKNOWN'
DF1[6,1] <- 'UNKNOWN'
DF1[4,1] <- 'UNKNOWN'
DF1[8,1] <- 'UNKNOWN'
DF1[10,1] <- 'UNKNOWN'
DF1[11,1] <- 'UNKNOWN'
DF1[12,1] <- 'UNKNOWN'
DF1[13,1] <- 'UNKNOWN'
DF1[14,1] <- 'UNKNOWN'
DF1[15,1] <- 'UNKNOWN'
DF1[16,1] <- 'UNKNOWN'
DF1[17,1] <- 'UNKNOWN'
DF1[18,1] <- 'UNKNOWN'
DF1[19,1] <- 'UNKNOWN'
DF1[20,1] <- 'UNKNOWN'
DF1[21,1] <- 'UNKNOWN'
DF1[22,1] <- 'UNKNOWN'
DF1[23,1] <- 'UNKNOWN'
DF1[24,1] <- "UNKNOWN"
test <- data.frame(sapply(DF1[,-1], function(x) as.numeric(as.character(x))))
test$Target <- as.factor(DF1$Target)
str(test$Target)



#subsetting of equal dimensions
cols_to_keep <- intersect(colnames(train),colnames(test))
train <- train[,cols_to_keep, drop=FALSE]
dim(train)
test <- test[,cols_to_keep, drop=FALSE]
dim(test)


#PCA train

input <- train[,-4586]
input <- input[,sapply(input, function(v) var(v, na.rm=TRUE)!=0)]
Sample.scaled <- apply(input,2,scale)
Sample.scaled<- t(na.omit(t(Sample.scaled)))
pca_train <- prcomp(Sample.scaled,center = TRUE, scale. = TRUE)
attributes(pca_train)
print(pca_train)
summary(pca_train)

#PLotting PCA

library(psych)
pairs.panels(pca_train$x, gap=0)
library(devtools)
library(ggbiplot)
g <- ggbiplot(pca_train, 
              obs.scale = 1,
              var.scale = 1,
              groups = train$Target,
              ellipse = TRUE,
              labels = train$Target,
              circle = TRUE,
              var.axes = FALSE,
              ellipse.prob = 0.50)
g <- g + scale_color_discrete(name ='')
g <- g + theme(legend.direction = 'horizontal',
               legend.position = 'top')
g

#PCA test

str(test)
inputest <- test[,-4586]
inputest <- inputest[,sapply(inputest, function(v) var(v, na.rm=TRUE)!=0)]
Sample.scaled <- apply(inputest,2,scale)
Sample.scaled<- t(na.omit(t(Sample.scaled)))
pca_test <- prcomp(Sample.scaled,center = TRUE, scale. = TRUE)
attributes(pca_test)
print(pca_test)
summary(pca_test)

#PLotting PCA

library(psych)
pairs.panels(pca_test$x, gap=0)
library(devtools)
library(ggbiplot)
g <- ggbiplot(pca_test, 
              obs.scale = 1,
              var.scale = 1,
              groups = test$Target,
              ellipse = TRUE,
              labels = test$Target,
              circle = TRUE,
              var.axes = FALSE,
              ellipse.prob = 0.95)
g <- g + scale_color_discrete(name ='')
g <- g + theme(legend.direction = 'horizontal',
               legend.position = 'top')
g

################################################################################################################
#############################MODELLLLLLLLLLLLLLLLLL#############################################################
a <- pca_train$x
a <- a[,1:3]
a <- data.frame(a)
a$Target <- train$Target

b <- pca_test$x
b <- b[,1:3]
b <- data.frame(b)
b$Target <- test$Target
library(nnet)
model <- randomForest(Target ~., data = a, mtry = 1)
p <- data.frame(predict(model, b))
compare1 <- data.frame(p)
compare1
model <- multinom(Target ~., data = a)
p <- data.frame(predict(model, b))
compare2 <- data.frame(p)
compare2
finalcomparison <- data.frame(b$Target,compare1, compare2)
colnames(finalcomparison) <- c("Target","RF","LOGISTIC")


test$Target <- NULL
test$Target <- finalcomparison$RF
str(test)
inputest <- test[,-4586]
inputest <- inputest[,sapply(inputest, function(v) var(v, na.rm=TRUE)!=0)]
Sample.scaled <- apply(inputest,2,scale)
Sample.scaled<- t(na.omit(t(Sample.scaled)))
pca_test <- prcomp(Sample.scaled,center = TRUE, scale. = TRUE)
attributes(pca_test)
print(pca_test)
summary(pca_test)

#PLotting PCA

library(psych)
pairs.panels(pca_test$x, gap=0)
library(devtools)
library(ggbiplot)
g <- ggbiplot(pca_test, 
              obs.scale = 1,
              var.scale = 1,
              groups = test$Target,
              ellipse = TRUE,
              labels = test$Target,
              circle = TRUE,
              var.axes = FALSE,
              ellipse.prob = 0.95)
g <- g + scale_color_discrete(name ='')
g <- g + theme(legend.direction = 'horizontal',
               legend.position = 'top')
g

a <- pca_test$x
a <- a[,1:3]
a <- data.frame(a)
a$Target <- test$Target

b <- pca_train$x
b <- b[,1:3]
b <- data.frame(b)
b$Target <- train$Target
library(nnet)
model <- randomForest(Target ~., data = a, mtry = 1)
p <- data.frame(predict(model, b))
compare1 <- data.frame(p)
compare1
model <- multinom(Target ~., data = a)
p <- data.frame(predict(model, b))
compare2 <- data.frame(p)
compare2
finalcomparison <- data.frame(b$Target,compare1, compare2)
colnames(finalcomparison) <- c("Target","RF","LOGISTIC")