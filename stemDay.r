library(readr) ## read in the csvs faster
library(dplyr)
library(survey)

## Median Annual Salary STEM
## Number/Percent in STEM field

## load in data: 5-year ACS


states <- read.csv('states.csv')
stemCodes <- read.csv('stemCodes.csv',colClasses = "character")
stemRelatedCodes <- read.csv('stemRelatedCodes.csv',colClasses = "character")

pVars <- c('SERIALNO','DEAR','ST','AGEP','ADJINC','SCHL','ESR','WKW','WKHP','SCH','DDRS','DEYE','DOUT','DPHY','DRAT','DRATX','DREM','ENG','WAGP', 'PERNP', 'PINCP', 'SSIP', 'RAC1P','HISP','SCIENGP','SCIENGRLP','CIT','SEX','OCCP','PWGTP',paste0('PWGTP',1:80))

    #hVars <- c('SERIALNO','TYPE','LNGI','HUPAC','HUPAOC','HUPARC','FPARC','FES','HHT','PARTNER','WKEXREL','WIF','HINCP')

## need: DEAR, attain, employment,PERNP, fulltime

firstTry <- read_csv('../data/acs5yr2016/ss16pusa.csv',n_max=2)
ccc <- ifelse(names(firstTry)%in%pVars,
              ifelse(names(firstTry)=='OCCP','c','i'),'-')
ccc <- paste(ccc,collapse='')

sdat <- read_csv('../data/acs5yr2016/ss16pusa.csv',col_types=ccc) ### CHANGE TO APPROPRIATE LOCATION


for(pp in c('b','c','d')){
    sdat2 <- read_csv(paste0('../data/acs5yr2016/ss16pus',pp,'.csv'),col_types=ccc) ## CHANGE TO APPROPRIATE LOCATION
    sdat <- rbind(sdat[,pVars],sdat2[,pVars])
}


rm(sdat2)
gc()

names(sdat) <- tolower(names(sdat))

sdat$state <- states$abb[match(sdat$ST,states$x)]

sdat$stem <- sdat$occp%in%stemCodes$code
sdat$stemrel <- sdat$occp%in%c(stemRelatedCodes$code,stemCodes$code)

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


source('quant2.r') ## function for estimating median


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


### number in stem field

### estimate proportion
wmean <- function(x,w)
    sum(x*w)/sum(w)

estProp <- function(x,w,wrep){
    est <- wmean(x,w)
    reps <- apply(wrep,2,function(ww) wmean(x,ww))
    c(est,sqrt(4*mean((reps-est)^2)))
}

estNum <- function(ddd){
    est <- sum(ddd$pwgtp)
    wrep <- ddd[,paste0('pwgtp',1:80)]
    c(est,sqrt(4*mean((colSums(wrep)-est)^2)))
}

numProp <- function(DEAF){
    ddd <- filter(sdat,dear==ifelse(DEAF,1,2))
    wrep <- ddd[,paste0('pwgtp',1:80)]
    w <- ddd$pwgtp

    overall <- prettyNum(c(100,0,round(estNum(ddd)),nrow(ddd)),',')

    estfun <- function(sss){
        sss <- enquo(sss)
        ddd <- ddd%>%mutate(x=!! sss)
        prop <- estProp(ddd$x,w,wrep)*100
        num <- estNum(filter(ddd,!! sss))

        prettyNum(c(round(prop,1),round(num),nrow(ddd)),',')
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
    names(out) <- c('job','Bachelors',paste(ifelse(DEAF,'Deaf','Hearing'),c('Percentage','SE.p','Number','SE.tot','n')))
    out
}

numprop <- list()
for(D in c(TRUE,FALSE))
    numprop[[paste(ifelse(D,'Deaf','Hearing'))]] <- numProp(D)

openxlsx::write.xlsx(numprop,'proportionStem.xlsx')
