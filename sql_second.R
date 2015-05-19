##install.packages("RODBC")
##library(RODBC)
require(RODBC)
sqlconn <- odbcDriverConnect(connection = "Driver={Oracle in instantclient11_1};DbQ=nrmodsp1.nrm.minn.seagate.com;UId=7496;Pwd=FLYFISH;")
                   stsample<-sqlQuery(sqlconn,
                                      paste(
                                        "SELECT A.MDATETIME MEAS_DATE, A.LOT TPW_LOT, A.MEAS_PT TEST_TESTD, A.PT_LOC_X X, A.PT_LOC_Y Y, A.PT_STATUS GOOD_PTS from waf.sem_hist_pts A,
                                        (select max(mdatetime) mdatetime, lot
                                        from waf.sem_hist_pts
                                        where mdatetime>sysdate-10 AND PROD IN ('9C','DJ') AND STAGE='WP2_VE'
                                        AND STEP='MEAS_CD_SEM' AND MEAS_TYPE='TEST_TESTD'
                                        group by lot) b
                                        WHERE a.mdatetime = b.mdatetime and A.PROD IN ('9C','DJ') AND A.STAGE='WP2_VE'
                                        AND STEP='MEAS_CD_SEM' AND MEAS_TYPE='TEST_TESTD' and a.lot = b.lot"
                                      )
                   )
                   stsample["trans"] <- 0.0
                   stsample2<-subset(stsample, stsample$GOOD_PTS[]=="VALID")
                   ##analysis=by(stsample$TEST_TESTD,stsample$TPW_LOT,mean) 
                   ##lotsum<-data.frame(t(vapply(analysis,unlist,unlist(analysis[[1]]))))
                   
                   ##install.packages("plyr")
                   ##library(plyr)
                   require(plyr)  
                   tinker<-ddply(stsample2, .(TPW_LOT), summarize,
                                 mean = mean(TEST_TESTD) )
                   a2="rr"
                   for (i in  1:nrow(stsample2) )
                   {
                     a1=stsample2$TPW_LOT[i]
                     if(a2!=a1)
                     {for (j in 1:nrow(tinker))
                     {
                       if (stsample2$TPW_LOT[i]==tinker$TPW_LOT[j]) 
                       { a2=tinker$TPW_LOT[j]
                         a3=tinker$mean[j]
                         j=nrow(tinker)    
                       }
                       
                     }
                     
                     }
                     
                     if(a2==a1)
                     {stsample2$trans[i]=stsample2$TEST_TESTD[i]-a3}
                   }
                   
                  
stsample2$R <- with(stsample2,sqrt( X^2 + Y^2)/90967.942418)
stsample2$THETA <- 1.0
 for (k in 1:nrow(stsample2))
   {
       if (stsample2$X[k]>0 && stsample2$Y[k]>0) 
         { stsample2$THETA[k]=57.2957795131*atan(stsample2$Y[k]/stsample2$X[k])  
               }
       else if  (stsample2$X[k]>0 && stsample2$Y[k]<0)
         {stsample2$THETA[k]=57.2957795131*atan(stsample2$Y[k]/stsample2$X[k])+360}
       
       
         else if (stsample2$X[k]<0 && stsample2$Y[k]<0)
             {stsample2$THETA[k]=57.2957795131*atan(stsample2$Y[k]/stsample2$X[k])+180}
        else
       {stsample2$THETA[k]=57.2957795131*atan(stsample2$Y[k]/stsample2$X[k])+180}
   }
stsample2$ZN1M1P <- stsample2$R * cos(stsample2$THETA / 57.2957795131 )
stsample2$ZN1M1M <- stsample2$R * sin(stsample2$THETA / 57.2957795131 )
stsample2$ZN1M0 <-  (-1) + 2 * stsample2$R ^ 2
stsample2$ZN2M1P <-stsample2$R * ((-2) + 3 * stsample2$R ^ 2) * cos( stsample2$THETA / 57.2957795131 )
stsample2$ZN2M1M <-stsample2$R * ((-2) + 3 * stsample2$R ^ 2) * sin( stsample2$THETA / 57.2957795131 )
stsample2$ZN2M0 <-(1 - 6 * stsample2$R ^ 2) + 6 * stsample2$R ^ 4
stsample2$ZN2M2P <-stsample2$R ^ 2 * cos( 2 * (stsample2$THETA / 57.2957795131) )
stsample2$ZN2M2M <-stsample2$R ^ 2 * sin( 2 * (stsample2$THETA  / 57.2957795131) )

##library(latticeExtra)

##col.l <- colorRampPalette(c('blue', 'cyan', 'green', 'yellow', 'orange', 'red'))
##col.divs<-20
##levelplot(stsample2$ZN2M1P ~ stsample2$X * stsample2$Y, stsample2, cuts=50, contour=TRUE, col.regions=col.l,
##         at=seq(from=-1,to=1,length=col.divs), panel=panel.2dsmoother)


models <- sapply(unique(as.character(stsample2$TPW_LOT)),
                 function(LOT)lm(trans~ZN1M1P+ZN1M1M+ZN1M0+ZN2M1P+ZN2M1M+ZN2M0+ZN2M2P+ZN2M2M,stsample2,subset=(TPW_LOT==LOT)),
                 simplify=FALSE,USE.NAMES=TRUE)
tinker2<- ldply(models, coef)
tinker2<-rename(tinker2, c(".id"="TPW_LOT"))
total <- merge(tinker,tinker2,by="TPW_LOT")
total

