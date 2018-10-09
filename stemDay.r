library(readr) ## read in the csvs faster
library(dplyr)
library(survey)

## Median Annual Salary STEM
## Number/Percent in STEM field

## load in data: 5-year ACS


states <- read.csv('../data/acs5yr2016/states.csv')
stemCodes <- read.csv('stemCodes.csv',colClasses = "character")
stemRelatedCodes <- read.csv('stemRelatedCodes.csv',colClasses = "character")

pVars <- c('SERIALNO','DEAR','ST','AGEP','ADJINC','SCHL','ESR','WKW','WKHP','SCH','DDRS','DEYE','DOUT','DPHY','DRAT','DRATX','DREM','ENG','WAGP', 'PERNP', 'PINCP', 'SSIP', 'RAC1P','HISP','SCIENGP','SCIENGRLP','CIT','SEX','OCCP','PWGTP',paste0('PWGTP',1:80))

    #hVars <- c('SERIALNO','TYPE','LNGI','HUPAC','HUPAOC','HUPARC','FPARC','FES','HHT','PARTNER','WKEXREL','WIF','HINCP')

## need: DEAR, attain, employment,PERNP, fulltime

firstTry <- read_csv('../data/acs5yr2016/ss16pusa.csv',n_max=2)
ccc <- ifelse(names(firstTry)%in%pVars,
              ifelse(names(firstTry)=='OCCP','c','i'),'-')
ccc <- paste(ccc,collapse='')

sdat <- read_csv('../data/acs5yr2016/ss16pusa.csv',col_types=ccc)


for(pp in c('b','c','d')){
    sdat2 <- read_csv(paste0('../data/acs5yr2016/ss16pus',pp,'.csv'),col_types=ccc)
    sdat <- rbind(sdat[,pVars],sdat2[,pVars])
}


rm(sdat2)
gc()

names(sdat) <- tolower(names(sdat))

sdat$state <- states$abb[match(sdat$ST,states$x)]

sdat$stem <- sdat$occp%in%stemCodes$code
sdat$stemrel <- sdat$occp%in%c(stemRelatedCodes$code,stemCodes$code)

sdat$stemjob <-

sdat$adj <- sdat$adjinc/1e6

sdat <- sdat%>%filter(agep>24,agep<65)%>%
    mutate(hs = schl>=16,
           ba = schl>=21,
           employed = esr%in%c(1,2,4,5),
           unemployed = esr==3,
           fulltime=(wkw==1 & wkhp>=35),
           earn=pernp*adj,
           inc=pincp*adj,

           raceEth=ifelse(hisp>1,"Hispanic",
                   ifelse(rac1p==2,"African American",
                   ifelse(rac1p==6| rac1p==7,"Asian/PacIsl",
                   ifelse(rac1p%in%c(3,4,5),'American Indian',
                   ifelse(rac1p==1,"White","Other"))))))

sdat <- sdat%>%filter(fulltime)%>%mutate(stemJob=factor(ifelse(stem,'stem',
                                                        ifelse(stemrel,'stem-related','non-stem')),
                                                        levels=c('stem','stem-related','non-stem')),
                                         BA=factor(ifelse(ba&(sciengp==1),'stem',
                                                   ifelse(ba&(sciengrlp==1),'stem-related',
                                                   ifelse(ba,'non-stem','none'))),
                                                   levels=c('stem','stem-related','non-stem','none')))

## checks:
xtabs(~stemJob+stem+stemrel,data=sdat)
xtabs(~ba+BA,data=sdat)
xtabs(~sciengp+sciengrlp+BA,data=filter(sdat,ba))

raceNs <- xtabs(~raceEth,data=sdat)

## des <- svrepdesign(weight=~pwgtp,repweights='pwgtp[0-9]+',scale=4/80,rscales=rep(1,80),mse=TRUE,type='JK1',data=sdat[1:1000,])
## des$mse <- TRUE


## median income
inc <- data.frame(deaf.med=numeric(),deaf.se=numeric(),hear.med=numeric(),hear.se=numeric())

## med <- function(x,design){
##     qq <- svyquantile(x=x,design=design,quantile=0.5,na.rm=TRUE)#,se=FALSE)
##     c(qq[1,1],SE=sqrt(attr(qq,'var')[1,1]))
## }

source('quant2.r')

deaf <- subset(sdat,dear==1)
hear <- subset(sdat,dear==2)


estimate <- function(DEAF=TRUE,INC=TRUE){
    ddd <- filter(sdat,dear==ifelse(DEAF,1,2))
    form <- reformulate(ifelse(INC,'inc','earn'))
    overall <- prettyNum(round(c(med(form,ddd),nrow(ddd))),',')

    estfun <- function(sss){
        sss <- enquo(sss)
        dsub <- filter(ddd,!! sss)
        prettyNum(round(c(med(form,dsub),nrow(dsub))),',')
    }

    out <- overall
    subs <- c('','')
    for(job in levels(sdat$stemJob)){
        subs <- rbind(subs,c(job,''))
        out <- rbind(out,estfun(stemJob==job))
    }
    for(bach in levels(sdat$BA)){
        subs <- rbind(subs,c('',bach))
        out <- rbind(out,estfun(BA==bach))
    }
    for(job in levels(sdat$stemJob))
        for(bach in levels(sdat$BA)){
            subs <- rbind(subs,c(job,bach))
            out <- rbind(out,
                         estfun(stemJob==job & BA==bach))
        }

    out <- data.frame(subs,out)
    names(out) <- c('job','Bachelors',paste(ifelse(DEAF,'Deaf','Hearing'),c('Median','SE','n')))
    out
}

results <- list()
for(D in c(TRUE,FALSE))
    for(I in c(TRUE,FALSE))
        results[[paste(ifelse(D,'Deaf','Hearing'),ifelse(I,'Income','Earnings'))]] <- estimate(D,I)

openxlsx::write.xlsx(results,'stemDay.xlsx')


##     out <- rbind(overall,
##                  `full time`=estfun(fulltime),
##                  `stem job`=estfun(stem),
##                  `not stem job`=estfun(!stem),
##                  `full time stem job`=estfun(stem&fulltime),
##                  `full time non-stem job`=estfun(!stem&fulltime),
##                  `stem-related job`=estfun(stemrel),
##                  `non-stem-related job`=estfun(!stemrel),
##                  `full time stem-related job`=estfun(stemrel&fulltime),
##                  `full time non-stem-related job`=estfun(!stemrel&fulltime),
##                  `stem BA`=estfun((sciengp==1)&ba),
##                  `non-stem BA`=estfun((sciengp!=1)&ba),
##                  `stem-related BA`=estfun((sciengrlp==1)|(sciengp==1)&ba),
##                  `non-stem-related BA`=estfun((sciengrlp!=1)|(sciengp!=1)&ba)
##                  )
##     out
## }

##         if(DEAF) ddd <- de
##             if(INC) med(~inc,deaf) else med(~earn,deaf)
##         else if(INC) med(~inc,hear) else med(~earn,hear)

## deafInc <- rbind(
##     overall=c(med(~inc,deaf),nrow(deaf)))
## deafInc <- rbind(deafInc,
##                  fulltime=subD(fulltime))
## deafInc <- rbind(deafInc,
##                  stemOcc=subD(stem))
## deafInc <- rbind(deafInc,
##                  nonStemOcc=subD(!stem))
## deafInc <- rbind(deafInc,
##                  stemOccFT=subD(stem&fulltime))
## deafInc <- rbind(deafInc,
##                  nonStemOccFT=subD(!stem&fulltime))
## deafInc <- rbind(deafInc,
##                  stemOccrel=subD(stemrel))
## deafInc <- rbind(deafInc,
##                  nonStemOccrel=subD(!stemrel))
## deafInc <- rbind(deafInc,
##                  stemOccrelFT=subD(stemrel&fulltime))
## deafInc <- rbind(deafInc,
##                  nonStemOccrelFT=subD(!stemrel&fulltime))
## deafInc <- rbind(deafInc,
##                  stemBA=subD((sciengp==1)&ba))
## deafInc <- rbind(deafInc,
##     nonStemBA=subD((sciengp!=1)&ba),
##     stemrelBA=subD((sciengrlp==1)&ba),
##     nonStemrelBA=subD((sciengrlp!=1)&ba))
## save(deafInc,file='deafInc.RData')



## deafEarn <- rbind(
##     overall=c(med(~earn,deaf),nrow(deaf)))
## deafEarn <- rbind(deafEarn,
##                  fulltime=earnD(fulltime))
## deafEarn <- rbind(deafEarn,
##                  stemOcc=earnD(stem))
## deafEarn <- rbind(deafEarn,
##                  nonStemOcc=earnD(!stem))
## deafEarn <- rbind(deafEarn,
##                  stemOccFT=earnD(stem&fulltime))
## deafEarn <- rbind(deafEarn,
##                  nonStemOccFT=earnD(!stem&fulltime))
## deafEarn <- rbind(deafEarn,
##                  stemOccrel=earnD(stemrel))
## deafEarn <- rbind(deafEarn,
##                  nonStemOccrel=earnD(!stemrel))
## deafEarn <- rbind(deafEarn,
##                  stemOccrelFT=earnD(stemrel&fulltime))
## deafEarn <- rbind(deafEarn,
##                  nonStemOccrelFT=earnD(!stemrel&fulltime))
## deafEarn <- rbind(deafEarn,
##                  stemBA=earnD((sciengp==1)&ba))
## deafEarn <- rbind(deafEarn,
##     nonStemBA=earnD((sciengp!=1)&ba),
##     stemrelBA=earnD((sciengrlp==1)&ba),
##     nonStemrelBA=earnD((sciengrlp!=1)&ba))
## save(deafEarn,file='deafEarn.RData')

## hear <- filter(sdat,dear==2)



## hearEarn <- rbind(
##     overall=c(med(~earn,hear),nrow(hear)))
## hearEarn <- rbind(hearEarn,
##                  fulltime=earnH(fulltime))
## hearEarn <- rbind(hearEarn,
##                  stemOcc=earnH(stem))
## hearEarn <- rbind(hearEarn,
##                  nonStemOcc=earnH(!stem))
## hearEarn <- rbind(hearEarn,
##                  stemOccFT=earnH(stem&fulltime))
## hearEarn <- rbind(hearEarn,
##                  nonStemOccFT=earnH(!stem&fulltime))
## hearEarn <- rbind(hearEarn,
##                  stemOccrel=earnH(stemrel))
## hearEarn <- rbind(hearEarn,
##                  nonStemOccrel=earnH(!stemrel))
## hearEarn <- rbind(hearEarn,
##                  stemOccrelFT=earnH(stemrel&fulltime))
## hearEarn <- rbind(hearEarn,
##                  nonStemOccrelFT=earnH(!stemrel&fulltime))
## hearEarn <- rbind(hearEarn,
##                  stemBA=earnH((sciengp==1)&ba))
## hearEarn <- rbind(hearEarn,
##     nonStemBA=earnH((sciengp!=1)&ba),
##     stemrelBA=earnH((sciengrlp==1)&ba),
##     nonStemrelBA=earnH((sciengrlp!=1)&ba))
## save(hearEarn,file='hearEarn.RData')

## hearInc <- rbind(
##     overall=c(med(~inc,hear),nrow(hear)))
## hearInc <- rbind(hearInc,
##                  fulltime=subH(fulltime))
## hearInc <- rbind(hearInc,
##                  stemOcc=subH(stem))
## hearInc <- rbind(hearInc,
##                  nonStemOcc=subH(!stem))
## hearInc <- rbind(hearInc,
##                  stemOccFT=subH(stem&fulltime))
## hearInc <- rbind(hearInc,
##                  nonStemOccFT=subH(!stem&fulltime))
## hearInc <- rbind(hearInc,
##                  stemOccrel=subH(stemrel))
## hearInc <- rbind(hearInc,
##                  nonStemOccrel=subH(!stemrel))
## hearInc <- rbind(hearInc,
##                  stemOccrelFT=subH(stemrel&fulltime))
## hearInc <- rbind(hearInc,
##                  nonStemOccrelFT=subH(!stemrel&fulltime))
## hearInc <- rbind(hearInc,
##                  stemBA=subH((sciengp==1)&ba))
## hearInc <- rbind(hearInc,
##     nonStemBA=subH((sciengp!=1)&ba),
##     stemrelBA=subH((sciengrlp==1)&ba),
##     nonStemrelBA=subH((sciengrlp!=1)&ba))
## save(hearInc,file='hearInc.RData')

## income=as.data.frame(cbind(deafInc,hearInc))
## earnings <- as.data.frame(cbind(deafEarn,hearEarn))

