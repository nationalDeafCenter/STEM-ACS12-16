med <- function(x,dat){
    x <-model.frame(x,dat)[[1]]
    oo <- order(x)
    w <- dat$pwgtp
    cum.w <- cumsum(w[oo])/sum(w)
    Qf<-approxfun(cum.w,x[oo],method='linear',f=1,
                  yleft=min(x),yright=max(x),
                  ties=min)

    point.est <-Qf(0.5)
    estfun<-as.numeric(x<point.est)
    est <- sum(w*estfun)/sum(w)
    wr <- dat[,paste0('pwgtp',1:80)]
    reps <- apply(wr,2,function(ww) sum(estfun*ww)/sum(ww))
    se <- sqrt(4*mean((reps-est)^2))
    ci <- Qf(c(est+2*se,est-2*se))
    SE <- ((ci[1]-ci[2])/4)
    c(point.est,SE)
}
