rm(list=ls(all=TRUE))

getwd()
#setwd('/Users/sra1/Documents/UCI HAR Dataset/train')
setwd('/Users/sra1/Documents/Insofe/HAR')

#source <- DirSource("/Users/sitamadhavi/Documents/INFOSE/Project/HAR/train")

#Subjects who took the test
subjects <- read.csv("/Users/sra1/Documents/UCI HAR Dataset/train/subject_train.txt",header=F)
names(subjects) <- 's'
View(subjects)
#Training set
metrics <- read.delim("/Users/sra1/Documents/UCI HAR Dataset/train/X_train.txt",header=F,sep="")
View(metrics)

#Training set labels
act_labels <- read.csv("/Users/sra1/Documents/UCI HAR Dataset/train/y_train.txt",header=F)
names(act_labels) <- 'a'
View(act_labels)

#Calculating the summary statistics
metric_mean <- sapply(metrics,mean)
range(metric_mean)

metric_sd <- sapply(metrics,sd)
range(metric_sd)

metric_var <- sapply(metrics,var)
range(metric_var)

#Means and Variances per group
dataset <- cbind(subjects,act_labels,metrics)
View(dataset)

# check for data skewness by subjects
variablenames <- c(names(subjects),names(metrics))
samplesizes <- aggregate(as.matrix(dataset[3:563]) ~ dataset$s, FUN = length)
names(samplesizes) <- variablenames
samplesizes <- samplesizes[,1:2]
View(samplesizes)

# Means by subjects
grpmeans <- aggregate(as.matrix(dataset[3:563]) ~ dataset$s, FUN = mean)
names(grpmeans) <- variablenames
View(grpmeans)

# SD by subjects
grpsd <- aggregate(as.matrix(dataset[3:563]) ~ dataset$s, FUN = sd)
names(grpsd) <- variablenames
View(grpsd)

# check for data skewness by activity labels
variablenames <- c(names(act_labels),names(metrics))
samples_act <- aggregate(as.matrix(dataset[3:563]) ~ dataset$a, FUN = length)
names(samples_act) <- variablenames
samples_act <- samples_act[,1:2]
View(samples_act)

# Means by activity labels
grpmeans_act <- aggregate(as.matrix(dataset[3:563]) ~ dataset$a, FUN = mean)
names(grpmeans_act) <- variablenames
View(grpmeans_act)

# SD by activity labels
grpsd_act <- aggregate(as.matrix(dataset[3:563]) ~ dataset$a, FUN = sd)
names(grpsd_act) <- variablenames
View(grpsd_act)

# Function for calculating within group variance
calcWithinGroupsVariance <- function(variable,groupvariable)
{
    # find out how many values the group variable can take
    groupvariable2 <- as.factor(groupvariable[[1]])
    levels <- levels(groupvariable2)
    numlevels <- length(levels)
    # get the mean and standard deviation for each group:
    numtotal <- 0
    denomtotal <- 0
    for (i in 1:numlevels)
    {
        leveli <- levels[i]
        levelidata <- variable[groupvariable==leveli,]
        levelilength <- length(levelidata)
        # get the standard deviation for group i:
        sdi <- sd(levelidata)
        numi <- (levelilength - 1)*(sdi * sdi)
        denomi <- levelilength
        numtotal <- numtotal + numi
        denomtotal <- denomtotal + denomi
    }
    # calculate the within-groups variance
    Vw <- numtotal / (denomtotal - numlevels)
    return(Vw)
}

# within groups variance by subjects
ln <- length(dataset)
grpvarbysub <- list()

for (i in 3:ln)
{
    grpvarbysub <- c(grpvarbysub,calcWithinGroupsVariance(dataset[i],dataset[1]))
}

grpvarbysub <- data.frame(grpvarbysub)
colnames(grpvarbysub) <- names(metrics)
rownames(grpvarbysub) <- 'grpVar'
grpvarbysub <- data.frame(t(grpvarbysub))
View(grpvarbysub)

# within groups variance by activity labels
grpvarbyact <- list()

for (i in 3:ln)
{
    grpvarbyact <- c(grpvarbyact,calcWithinGroupsVariance(dataset[i],dataset[2]))
}

grpvarbyact <- data.frame(grpvarbyact)
colnames(grpvarbyact) <- names(metrics)
rownames(grpvarbyact) <- 'grpVar'
grpvarbyact <- data.frame(t(grpvarbyact))
View(grpvarbyact)

# Function for calculation of between group variance
calcBetweenGroupsVariance <- function(variable,groupvariable)
{
    # find out how many values the group variable can take
    groupvariable2 <- as.factor(groupvariable[[1]])
    levels <- levels(groupvariable2)
    numlevels <- length(levels)
    # calculate the overall grand mean:
    grandmean <- mean(variable)
    # get the mean and standard deviation for each group:
    numtotal <- 0
    denomtotal <- 0
    for (i in 1:numlevels)
    {
        leveli <- levels[i]
        levelidata <- variable[groupvariable==leveli]
        levelilength <- length(levelidata)
        # get the mean and standard deviation for group i:
        meani <- mean(levelidata)
        sdi <- sd(levelidata)
        numi <- levelilength * ((meani - grandmean)^2)
        denomi <- levelilength
        numtotal <- numtotal + numi
        denomtotal <- denomtotal + denomi
    }
    # calculate the between-groups variance
    Vb <- numtotal / (numlevels - 1)
    Vb <- Vb[[1]]
    return(Vb)
}

# between groups variance by subjects
btwgrpvarbysub <- list()

calcBetweenGroupsVariance(dataset[,3],dataset[1])

for (i in 3:ln)
{
    btwgrpvarbysub <- c(btwgrpvarbysub,calcBetweenGroupsVariance(dataset[,i],dataset[1]))
}

btwgrpvarbysub <- data.frame(btwgrpvarbysub)
colnames(btwgrpvarbysub) <- names(metrics)
rownames(btwgrpvarbysub) <- 'btwgrpVar'
btwgrpvarbysub <- data.frame(t(btwgrpvarbysub))
View(btwgrpvarbysub)

# between groups variance by activity labels
btwgrpvarbyact <- list()

calcBetweenGroupsVariance(dataset[,3],dataset[2])

for (i in 3:ln)
{
    btwgrpvarbyact <- c(btwgrpvarbyact,calcBetweenGroupsVariance(dataset[,i],dataset[2]))
}

btwgrpvarbyact <- data.frame(btwgrpvarbyact)
colnames(btwgrpvarbyact) <- names(metrics)
rownames(btwgrpvarbyact) <- 'btwgrpVar'
btwgrpvarbyact <- data.frame(t(btwgrpvarbyact))
View(btwgrpvarbyact)

# Calculating seperation for groups by subject
sepbysub <- cbind(grpvarbysub,btwgrpvarbysub)
div <- sepbysub[2]/sepbysub[1]
colnames(div) <- 'd'
sepbysub <- cbind(sepbysub,div)
sepbysub <- sepbysub[order(-sepbysub[,3]),]
View(sepbysub)

# Calculating seperation for groups by activity
sepbyact <- cbind(grpvarbyact,btwgrpvarbyact)
div <- sepbyact[2]/sepbyact[1]
colnames(div) <- 'd'
sepbyact <- cbind(sepbyact,div)
sepbyact <- sepbyact[order(-sepbyact[,3]),]
View(sepbyact)

#class_file <- as.factor(file3$V1)
#dataset <- cbind(file1,file2,class_file)

## Correlation matrix with p-values.
## See http://goo.gl/nahmV for documentation of this function
cor.prob <- function (X, dfr = nrow(X) - 2) {
    R <- cor(X, use="pairwise.complete.obs")
    above <- row(R) < col(R)
    r2 <- R[above]^2
    Fstat <- r2 * dfr/(1 - r2)
    R[above] <- 1 - pf(Fstat, 1, dfr)
    R[row(R) == col(R)] <- NA
    R
}

## Use this to dump the cor.prob output to a 4 column matrix
## with row/column indices, correlation, and p-value.
## See StackOverflow question: http://goo.gl/fCUcQ
flattenSquareMatrix <- function(m) {
    if( (class(m) != "matrix") | (nrow(m) != ncol(m))) stop("Must be a square matrix.")
    if(!identical(rownames(m), colnames(m))) stop("Row and column names must be equal.")
    ut <- upper.tri(m)
    data.frame(i = rownames(m)[row(m)[ut]],
    j = rownames(m)[col(m)[ut]],
    cor=t(m)[ut],
    p=m[ut])
}


# correlation matrix with p-values
# and "flatten" that table
datpval <- flattenSquareMatrix(cor.prob(dataset[3:563]))
datpval <- datpval[order(-datpval[,3],datpval[,4]),]
View(datpval)
View(dataset[1:2])


#Standardising the metrics dataset
stdmetrics <- as.data.frame(scale(dataset[3:563]))
View(stdmetrics)

#Checking for standardised metrics dataset
sapply(stdmetrics,mean)
sapply(stdmetrics,sd)

#applying Principal Component Analysis
pcametrics <- princomp(stdmetrics)
loadings(pcametrics)


#Summary of Principal Component Analysis
summary(pcametrics)

screeplot(pcametrics,type='lines')
plot(pcametrics)

plot(pcametrics$loadings[,1],pcametrics$loadings[,2])
#text(pcametrics$x[,1],pcametrics$x[,2], dataset$s, cex=0.7, pos=4, col="red")
text(pcametrics$loadings[,1],pcametrics$loadings[,2], dataset$a, cex=0.7, pos=4, col="red")


var1<-predict(pcametrics)[,1]
var2<-predict(pcametrics)[,2]
var3<-predict(pcametrics)[,3]
var4<-predict(pcametrics)[,4]
var5<-predict(pcametrics)[,5]
var6<-predict(pcametrics)[,6]
var7<-predict(pcametrics)[,7]
var8<-predict(pcametrics)[,8]
var9<-predict(pcametrics)[,9]
var10<-predict(pcametrics)[,10]
var11<-predict(pcametrics)[,11]
var12<-predict(pcametrics)[,12]
var13<-predict(pcametrics)[,13]
var14<-predict(pcametrics)[,14]
var15<-predict(pcametrics)[,15]
var16<-predict(pcametrics)[,16]
var17<-predict(pcametrics)[,17]
var18<-predict(pcametrics)[,18]
var19<-predict(pcametrics)[,19]
var20<-predict(pcametrics)[,20]
var21<-predict(pcametrics)[,21]
var22<-predict(pcametrics)[,22]
var23<-predict(pcametrics)[,23]
var24<-predict(pcametrics)[,24]
var25<-predict(pcametrics)[,25]
var26<-predict(pcametrics)[,26]
var27<-predict(pcametrics)[,27]
var28<-predict(pcametrics)[,28]
var29<-predict(pcametrics)[,29]
var30<-predict(pcametrics)[,30]
var31<-predict(pcametrics)[,31]
var32<-predict(pcametrics)[,32]
var33<-predict(pcametrics)[,33]
var34<-predict(pcametrics)[,34]
var35<-predict(pcametrics)[,35]
var36<-predict(pcametrics)[,36]
var37<-predict(pcametrics)[,37]
var38<-predict(pcametrics)[,38]
var39<-predict(pcametrics)[,39]
var40<-predict(pcametrics)[,40]
var41<-predict(pcametrics)[,41]
var42<-predict(pcametrics)[,42]
var43<-predict(pcametrics)[,43]
var44<-predict(pcametrics)[,44]
var45<-predict(pcametrics)[,45]
var46<-predict(pcametrics)[,46]
var47<-predict(pcametrics)[,47]
var48<-predict(pcametrics)[,48]
var49<-predict(pcametrics)[,49]
var50<-predict(pcametrics)[,50]
var51<-predict(pcametrics)[,51]
var52<-predict(pcametrics)[,52]
var53<-predict(pcametrics)[,53]
var54<-predict(pcametrics)[,54]
var55<-predict(pcametrics)[,55]
var56<-predict(pcametrics)[,56]
var57<-predict(pcametrics)[,57]
var58<-predict(pcametrics)[,58]
var59<-predict(pcametrics)[,59]
var60<-predict(pcametrics)[,60]
var61<-predict(pcametrics)[,61]
View(metrics)
View(stdmetrics)

#-----------------------------------------------------------------------------------------------

predset <- cbind(dataset$a,var1,var2,var3,var4,var5,var6,var7,
var8,var9,var10,var11,var12,var13,var14,
var15,var16,var17,var18,var19,var21,
var22,var23,var24,var25,var26,var27,var28,
var30,var31,var32,var33,var34,var35,
var36,var37,var38,var39,var40,var42,
var43,var44,var45,var46,var47,var48,
var50,var51,var52,var53,var55,var56,
var57,var59,var61)

View(predset)
class(predset)

predset <- data.frame(predset)

predset$V1 <- as.factor(predset$V1)


#########Oblique trees
View(predset)
library(oblique.tree)
detach(predset)

dtOb = oblique.tree(V1~., data = predset, oblique.splits = "only")
summary(dtOb)

plot(dtOb)

a <- table(predset$V1, predict(dtOb, newdata=predset, type="class"))

sum(diag(a))/sum(a)*100

a=table(test$quality, predict(dtOb, newdata=test, type="class"))

a[2,2]/(a[1,2]+a[2,2])*100