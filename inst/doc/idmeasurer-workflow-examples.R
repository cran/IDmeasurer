## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ------------------------------------------------------------------------
library(IDmeasurer)

## ------------------------------------------------------------------------
summary(CCformants)

## ------------------------------------------------------------------------
CCformants[1:20,]

## ------------------------------------------------------------------------
id0 <- GenerateUnivariate(nindivs=10, nobs=10, betweenM=1000, individuality=0.01)
id3 <- GenerateUnivariate(nindivs=10, nobs=10, betweenM=1000, individuality=3)

## ---- fig.show='hold'----------------------------------------------------
boxplot(paramvec~id, data=id0, xlab = 'Individual no.', ylab = 'variable 1', main = 'Individuality = 0.01')
boxplot(paramvec~id, data=id3, xlab = 'Individual no.', ylab = 'variable 1', main = 'Individuality = 3')

## ------------------------------------------------------------------------
calcF(id0)
calcF(id3)

## ------------------------------------------------------------------------
fewobs <- GenerateUnivariate(nindivs=10, nobs=10, betweenM=1000, individuality=0.5)
manyobs <- GenerateUnivariate(nindivs=10, nobs=50, betweenM=1000, individuality=0.5)

## ------------------------------------------------------------------------
calcF(fewobs)
calcF(manyobs)

## ------------------------------------------------------------------------
calcPIC(fewobs)
calcPIC(manyobs)

## ------------------------------------------------------------------------
postrait <- GenerateUnivariate(nindivs=10, nobs=10, betweenM=1000, individuality=0.5)
posnegtrait <- GenerateUnivariate(nindivs=10, nobs=10, betweenM=0, individuality=0.5)

## ------------------------------------------------------------------------
calcPIC(postrait)
calcPIC(posnegtrait)

## ------------------------------------------------------------------------
calcPIC(id0)
calcPICbetweentot(id0)
calcPICbetweenmeans(id0)

## ------------------------------------------------------------------------
calcHS(fewobs)[2]
calcHS(manyobs)[2]

## ------------------------------------------------------------------------
calcHS(postrait)[2]
calcHS(posnegtrait)[2]

## ------------------------------------------------------------------------
id1 <- GenerateUnivariate(nindivs=10, nobs=10, betweenM=1000, individuality=1)
id2 <- GenerateUnivariate(nindivs=10, nobs=10, betweenM=1000, individuality=2)
id3 <- GenerateUnivariate(nindivs=10, nobs=10, betweenM=1000, individuality=3)
id4 <- GenerateUnivariate(nindivs=10, nobs=10, betweenM=1000, individuality=4)

PICs <- c(calcPIC(id1), calcPIC(id2), calcPIC(id3), calcPIC(id4))
HSs <- c(calcHS(id1)[2], calcHS(id2)[2], calcHS(id3)[2], calcHS(id4)[2])

## ---- fig.cap="Large overlap of trait values suggests low individuality"----
plot((2^HSs) ~ PICs, xlim=c(0,5), ylim=c(0,5), pch=16)
abline(lm((2^HSs) ~ PICs))

## ------------------------------------------------------------------------
calcPIC(id4)
2^calcHS(id4)[2]

## ------------------------------------------------------------------------
calcHS(id3)
calcHSnpergroup(id3)
calcHSntot(id3)
calcHSngroups(id3)
calcHSvarcomp(id3)

## ------------------------------------------------------------------------
calcF(CCformants)
calcPIC(CCformants)
calcHS(CCformants, sumHS=F)

## ------------------------------------------------------------------------
library(MASS)

## ------------------------------------------------------------------------
id0 <- GenerateMultivariate(nindivs=5, nobs=10, nvar=2, covar=0.9, individuality=0.01)
id5 <- GenerateMultivariate(nindivs=5, nobs=10, nvar=2, covar=0.9, individuality=5)

## ---- fig.show='hold'----------------------------------------------------
plot(id0[,2], id0[,3], xlab='trait 1', ylab='trait 2', pch=16, col=id0[,1])
plot(id5[,2], id5[,3], xlab='trait 1', ylab='trait 2', pch=16, col=id5[,1])

## ------------------------------------------------------------------------
calcDS(id0)
calcDS(id5)

## ------------------------------------------------------------------------
id5fewinds <- GenerateMultivariate(nindivs=5, nobs=10, nvar=2, covar=0.9, individuality=5)
id5manyinds <- GenerateMultivariate(nindivs=100, nobs=10, nvar=2, covar=0.9, individuality=5)
calcDS(id5fewinds)
calcDS(id5manyinds)

## ------------------------------------------------------------------------
calcDS(id5fewinds)/0.2 # expected classification by chance for 5 individuals: 1 / 5 = 0.2
calcDS(id5manyinds)/0.01 # expected classification by chance for 5 individuals: 1 / 100 = 0.01

## ------------------------------------------------------------------------
calcDS(calcPCA(id5fewinds))/0.2 # expected classification by chance for 5 individuals: 1 / 5 = 0.2
calcDS(calcPCA(id5manyinds))/0.01 # expected classification by chance for 5 individuals: 1 / 100 = 0.01

## ------------------------------------------------------------------------
id5fewobs <- GenerateMultivariate(nindivs=10, nobs=5, nvar=2, covar=0.9, individuality=1)
id5manyobs <- GenerateMultivariate(nindivs=10, nobs=100, nvar=2, covar=0.9, individuality=1)
calcDS(id5fewobs)
calcDS(id5manyobs)

## ------------------------------------------------------------------------
id5fewinds <- GenerateMultivariate(nindivs=5, nobs=10, nvar=2, covar=0.9, individuality=2)
id5manyinds <- GenerateMultivariate(nindivs=100, nobs=10, nvar=2, covar=0.9, individuality=2)
calcDS(id5fewinds)
calcDS(id5manyinds)
calcMI(id5fewinds)
calcMI(id5manyinds)

## ------------------------------------------------------------------------
calcHS(calcPCA(id5), sumHS=T)

## ------------------------------------------------------------------------
calcHS(id5, sumHS=T) # HS is overestimated if trait variables are correlated

## ------------------------------------------------------------------------
id5 <- GenerateMultivariate(nindivs=10, nobs=10, nvar=2, covar=0, individuality=5)
calcHS(id5)

## ------------------------------------------------------------------------
calcHM(id5)

## ------------------------------------------------------------------------
calcHM(id5)*2

## ------------------------------------------------------------------------
id1 <- GenerateMultivariate(nindivs=10, nobs=10, nvar=5, covar=0, individuality=1)
id3 <- GenerateMultivariate(nindivs=10, nobs=10, nvar=5, covar=0, individuality=3)
id5 <- GenerateMultivariate(nindivs=10, nobs=10, nvar=5, covar=0, individuality=5)
x <- c(calcHS(id1)[2],calcHS(id3)[2],calcHS(id5)[2])
y <- c(calcHM(id1)*5, calcHM(id3)*5, calcHM(id5)*5) # HM is multiplied by 5 because we have 5 trait variables.
plot(x,y)
abline(lm(y~x))

## ------------------------------------------------------------------------
id5covar0 <- GenerateMultivariate(nindivs=50, nobs=10, nvar=5, covar=0, individuality=5) # uncorrelated traits
id5covar1 <- GenerateMultivariate(nindivs=50, nobs=10, nvar=5, covar=1, individuality=5) # perfectly correlated traits

## ------------------------------------------------------------------------
calcHS(calcPCA(id5covar0))
calcHS(calcPCA(id5covar1))

## ------------------------------------------------------------------------
calcDS(calcPCA(id5covar0))
calcDS(calcPCA(id5covar1))

## ------------------------------------------------------------------------
calcHM(calcPCA(id5covar0))*5 # HM is multiplied by the number of variables to get individuality of entire signal
calcHM(calcPCA(id5covar1))*5 # In this case, total individuality of entire signal is overestimated

## ------------------------------------------------------------------------
temp <- GenerateMultivariate(nindivs=20, nobs=10, nvar=2, covar=0, individuality=5)
HS <- calcHS(temp)[2]
DS <- calcDS(temp)
HSest <- convertDStoHS(nindivs=10, nobs=10, DS=DS)
print(paste('real HS = ', HS, '; estimated HSest = ', round(HSest, 2)))
DSest <- convertHStoDS(nindivs=10, nobs=10, HS=HS)
print(paste('real DS = ', DS, '; estimated DSest = ', round(DSest, 2)))

