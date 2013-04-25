###########################################Functions#####################################
########################################GALEX variables##################################

###################################
###########LOAD THE FILE###########
###################################
loadit <- function(str1) {
  # load the test file
  test <- read.table(str1,header=T,sep=",",nrows=50)
  classes <- sapply(test,class)
  # check for coaddObjid and survey and covert their classes in the table
  if (any(names(test) == "coaddObjid") || any(names(test) == "coaddobjid") ||       
      any(names(test) == "coaddSurvey") || any(names(test) =="coaddsurvey") ||
      any(names(test) == "visitSurvey") || any(names(test) =="visitsurvey")) 
    {
      classes[which(names(test) == "coaddSurvey")] <- "character"
      classes[which(names(test) == "coaddsurvey")] <- "character"
      classes[which(names(test) == "visitSurvey")] <- "character"
      classes[which(names(test) == "visitsurvey")] <- "character"
      classes[which(names(test) == "coaddobjid" )] <- "factor"
      classes[which(names(test) == "coaddObjid" )] <- "factor"
   
    # read the csv now with the changes class list
    read.table(str1,header=T,sep=",",colClasses=classes)

  } else {  read.table(str1,header=T,sep=",",colClasses=classes)
  }
}

##################################################################################
### FUNCTION: WRITETOFILE ######################################################## 
##################################################################################
#This function writes a RAW data file used to compute the period

writeToFile <- function (ID, 
                         var, 
                         coaddObjid, 
                         photoobsMJD, 
                         visitNuv_mag, 
                         visitNuv_magErr, 
                         visitFuv_mag, 
                         visitFuv_magErr) 
  {
    #################
    # Load NUV data #
    #################
    dNUV <- subset(var, coaddObjid==ID & visitNuv_mag>0, select=c(coaddObjid,photoobsMJD,visitNuv_mag,visitNuv_magErr))

    # Create vars
    t.n <- dNUV$photoobsMJD
    m.n <- dNUV$visitNuv_mag
    e.n <- dNUV$visitNuv_magErr

    # Calculate min/max
    tmin.n <- min(t.n)
    tmax.n <- max(t.n)
    mmin.n <- min(m.n)
    mmax.n <- max(m.n)
    
    # Write data to file in pNUV directory
    toFile.n <- data.frame(t.n,m.n,e.n)
    toFileSorted.n <- toFile.n[order(t.n,m.n,e.n),]
    filename.n <- paste("./MS_Period/pNUV/g",ID,sep="")
    write.table(toFileSorted.n, file = filename.n, sep = " ", quote = F)
    
    #################
    # Load FUV data #
    #################
    dFUV <- subset(var, coaddObjid==ID & visitFuv_mag>0, select=c(coaddObjid,photoobsMJD,visitFuv_mag,visitFuv_magErr))

    # Create vars
    t.f <- dFUV$photoobsMJD
    m.f <- dFUV$visitFuv_mag
    e.f <- dFUV$visitFuv_magErr

    # Calculate min/max
    tmin.f <- min(t.f)
    tmax.f <- max(t.f)
    mmin.f <- min(m.f)
    mmax.f <- max(m.f)    
    
    # Write data to file in pNUV directory
    toFile.f <- data.frame(t.f,m.f,e.f)
    toFileSorted.f <- toFile.f[order(t.f,m.f,e.f),]
    filename.f <- paste("./MS_Period/pFUV/g",ID,sep="")
    write.table(toFileSorted.f, file = filename.f, sep = " ", quote = F)

    #############
    # Plot data #
    #############  
    # Set limits for error bars
#     limits.f <- aes(ymax= m.f + e.f, ymin= m.f -e.f)
#      raw <- qplot(t.n, m.n, data = var,ylim=c(mmin.n,mmax.n)) +
#        scale_y_reverse() +
#        geom_errorbar(data=dNUV,limits.n)

    print("Writing completed")
  }

##################################################################################
### FUNCTION: SHOWRAW ############################################################ 
##################################################################################
#This function writes a RAW data file used to compute the period

showRaw <- function (ID, 
                         var, 
                         coaddObjid, 
                         photoobsMJD, 
                         visitNuv_mag, 
                         visitNuv_magErr, 
                         visitFuv_mag, 
                         visitFuv_magErr) 
  {
    #################
    # Load NUV data #
    #################
    dNUV <- subset(var, coaddObjid==ID & visitNuv_mag>0, select=c(coaddObjid,photoobsMJD,visitNuv_mag,visitNuv_magErr))
  
    if (dim(dNUV)[1] != 0) {# Create vars
    t.n <- dNUV$photoobsMJD
    m.n <- dNUV$visitNuv_mag
    e.n <- dNUV$visitNuv_magErr

    # Calculate min/max
    tmin.n <- min(t.n)
    tmax.n <- max(t.n)
    mmin.n <- min(m.n)
    mmax.n <- max(m.n)
      
    # Set limits for error bars
     #limits.n <- aes(ymax= m.n + e.n, ymin= m.n -e.n)
    # Create plot
      raw.n <- qplot(t.n, m.n,data = dNUV,ylim=c(mmin.n,mmax.n),main="GALEX NUV",
                     xlab="MJD", ylab="FUV magnitude") + scale_y_reverse() 
                #geom_errorbar(data=dNUV,limits.n)
    dummy1 <- 1
  } else dummy1 <- 0
    #################
    # Load FUV data #
    #################
    dFUV <- subset(var, coaddObjid==ID & visitFuv_mag>0, select=c(coaddObjid,photoobsMJD,visitFuv_mag,visitFuv_magErr))
    
    if (dim(dFUV)[1] != 0) { # Create vars
    t.f <- dFUV$photoobsMJD
    m.f <- dFUV$visitFuv_mag
    e.f <- dFUV$visitFuv_magErr

    # Calculate min/max
    tmin.f <- min(t.f)
    tmax.f <- max(t.f)
    mmin.f <- min(m.f)
    mmax.f <- max(m.f)    
    
    
    # Set limits for error bars
   #limits.f <- aes(ymax= m.f + e.f, ymin= m.f -e.f)
    # Create plot

    raw.f <- qplot(t.f, m.f, data = dFUV,ylim=c(mmin.f,mmax.f), main="GALEX FUV", 
                     xlab="MJD", ylab="FUV magnitude") + scale_y_reverse() 
                  #geom_errorbar(data=dFUV,limits.f)
    dummy2 <- 1
    } else dummy2 <- 0
    
    #dummy <- 0
    dummy <- dummy1 + dummy2
    #############
    # Plot data #
    #############  
    if (dummy == 2) { grid.arrange(raw.n,raw.f,nrow=2)}
    if (dummy == 1) { if (dummy1 == 1) {grid.arrange(raw.n)}else if (dummy2 == 1)   
                   {grid.arrange(raw.f)} }
    #if (dummy ==0){ return NULL}
}

##################################################################################
### FUNCTION: SHOWPERIOD ######################################################### 
##################################################################################
showPeriod <- function (ID, 
                        var, 
                        myPERIOD, 
                        coaddObjid, 
                        photoobsMJD, 
                        visitNuv_mag, 
                        visitNuv_magErr,
                        visitFuv_mag, 
                        visitFuv_magErr) 
  {

    #################
    # Load NUV data #
    #################
    dNUV <- subset(var, 
                   coaddObjid==ID & visitNuv_mag>0,
                   select=c(coaddObjid,photoobsMJD,visitNuv_mag,visitNuv_magErr,visitSurvey))

    # Create vars
    t.n <- dNUV$photoobsMJD
    m.n <- dNUV$visitNuv_mag
    e.n <- dNUV$visitNuv_magErr
    s.n <- dNUV$visitSurvey

    # Calculate min/max
    tmin.n <- min(t.n)
    tmax.n <- max(t.n)
    mmin.n <- min(m.n)
    mmax.n <- max(m.n)
    
    # Load period from file
    file.n <- paste("./MS_Period/pNUV/g",ID,".gx.candidate",sep="")
    periods.n <-read.table(file.n,header=F,sep="",na.string="N",col.names=c("p0","p","chi2"))
    
    if(missing(myPERIOD))
    {
        PERIOD.n <- periods.n$p[1]
        print(periods.n)
    }
    else
    {
      PERIOD.n <- myPERIOD
    }

    # Pick a starting point
    t0.n <- t.n[m.n==mmin.n]
    phi.n <- (t.n-t0.n)/PERIOD.n
    # get decimal portion of the phase, cycle is irrelevant
    # phi <- phi - trunc(phi)
    phip.n <- phi.n %%1
    # create an extended phase, just for the plot
    phim.n <- phip.n-1;
    # combine + and - phase, and other variables for plot.
    phiNew.n <- c(phim.n,phip.n)
    mNew.n <- c(m.n,m.n)
    eNew.n <- c(e.n,e.n)
    sNew.n <- c(s.n,s.n)
    dNew.n <- data.frame(phiNew.n,mNew.n,eNew.n,sNew.n)
    
    #################
    # Load FUV data #
    #################
    dFUV <- subset(var, 
                   coaddObjid==ID & visitFuv_mag>0,
                   select=c(coaddObjid,photoobsMJD,visitFuv_mag,visitFuv_magErr,visitSurvey))

    # Create vars
    t.f <- dFUV$photoobsMJD
    m.f <- dFUV$visitFuv_mag
    e.f <- dFUV$visitFuv_magErr
    s.f <- dFUV$visitSurvey

    # Calculate min/max
    tmin.f <- min(t.f)
    tmax.f <- max(t.f)
    mmin.f <- min(m.f)
    mmax.f <- max(m.f)
    
    # Load period from file
    file.f <- paste("./MS_Period/pFUV/g",ID,".gx.candidate",sep="")
    periods.f <- read.table(file.f,header=F,sep="",na.string="N",col.names=c("p0","p","chi2"))
    
    if(missing(myPERIOD))
    {
        PERIOD.f <- periods.f$p[1]
        print(periods.f)
    }
    else
    {
      PERIOD.f <- myPERIOD
    }   

    # Pick a starting point
    t0.f <- t.f[m.f==mmin.f]
    phi.f <- (t.f-t0.f)/PERIOD.f
    # get decimal portion of the phase, cycle is irrelevant
    # phi <- phi - trunc(phi)
    phip.f <- phi.f %%1
    # create an extended phase, just for the plot
    phim.f <- phip.f-1;
    # combine + and - phase, and other variables for plot.
    phiNew.f <- c(phim.f,phip.f)
    mNew.f <- c(m.f,m.f)
    eNew.f <- c(e.f,e.f)
    sNew.f <- c(s.f,s.f)
    dNew.f <- data.frame(phiNew.f,mNew.f,eNew.f,sNew.f)
        
    plot.title <- paste("GALEX Object ID",
                        ID,
                        "\nPeriod FUV =",toString(PERIOD.f),
                        " / Period NUV =",toString(PERIOD.n))

    phase <- ggplot() + 
      geom_point(aes(phiNew.n,mNew.n),data=dNew.n,size = 4) + 
      geom_point(aes(phiNew.f,mNew.f),data=dNew.f,size = 4,shape=0) + 
      scale_y_continuous(limits=c(mmin.n,mmax.f)) +
      scale_y_reverse() + 
      geom_errorbar(aes(x=phiNew.n,ymax=mNew.n+eNew.n,ymin=mNew.n-eNew.n), data=dNew.n) +
      geom_errorbar(aes(x=phiNew.f,ymax=mNew.f+eNew.f,ymin=mNew.f-eNew.f), data=dNew.f) + 
      opts(title=plot.title) + xlab("Phase") + ylab("GALEX Magnitude") +
      theme_bw()

    color <- ggplot() + 
      geom_point(aes(phiNew.n,mNew.n),data=dNew.n,size = 4) +  
      scale_y_continuous(limits=c(mmin.n,mmax.f)) +
      scale_y_reverse() + 
      geom_errorbar(aes(x=phiNew.n,ymax=mNew.n+eNew.n,ymin=mNew.n-eNew.n), data=dNew.n) +
      geom_errorbar(aes(x=phiNew.f,ymax=mNew.f+eNew.f,ymin=mNew.f-eNew.f), data=dNew.f) + 
      opts(title=plot.title) + xlab("Phase") + ylab("GALEX Magnitude") +
      theme_bw()
    
    phase.n <- ggplot() + 
      geom_point(aes(phiNew.n,mNew.n),data=dNew.n) + 
      scale_y_continuous(limits=c(mmin.n,mmax.n)) +
      scale_y_reverse() + 
      geom_errorbar(aes(x=phiNew.n,ymax=mNew.n+eNew.n,ymin=mNew.n-eNew.n), data=dNew.n) +
      xlab("Phase") + ylab("GALEX NUV Magnitude")

    phase.f <- ggplot() + 
      geom_point(aes(phiNew.f,mNew.f),data=dNew.f) + 
      scale_y_continuous(limits=c(mmin.f,mmax.f)) +
      scale_y_reverse() + 
      geom_errorbar(aes(x=phiNew.f,ymax=mNew.f+eNew.f,ymin=mNew.f-eNew.f), data=dNew.f) + 
      xlab("Phase") + ylab("GALEX FUV Magnitude")

    grid.arrange(phase,nrow=1)

#     limits.n <- aes(ymax= m.n + e.n, ymin= m.n -e.n)
#     raw.n <- qplot(t.n, m.n, data = var,ylim=c(mmin.n,mmax.n),colour = factor(s.n)) +
#       scale_y_reverse() +
#       geom_errorbar(data=dNUV,limits.n)
#     
#     limits.f <- aes(ymax= m.f + e.f, ymin= m.f -e.f)
#     raw.f <- qplot(t.f, m.f, data = var,ylim=c(mmin.f,mmax.f),colour = factor(s.f)) +
#       scale_y_reverse() +
#       geom_errorbar(data=dNUV,limits.f)
#   
#    grid.arrange(phase,raw.f,ncol=2)

  }
 
#####################################################################################
### FUNCTION: SHOWPERIOD_S_ ######################################################### 
#####################################################################################
showPeriods <- function (ID,
                        var, 
                        coaddObjid, 
                        photoobsMJD, 
                        visitNuv_mag, 
                        visitNuv_magErr,
                        visitFuv_mag, 
                        visitFuv_magErr) 
  {
  
    ############################
    ##### Global Variables #####
    ############################
    errbarSize <- 0.1
    textSize <- 20 
  
    ############################
    # Load ALL NUV + FUV  data #
    ############################
    dNUV <- subset(var, 
                   coaddObjid==ID & visitNuv_mag>0,,
                   select=c(coaddObjid,photoobsMJD,visitNuv_mag,visitNuv_magErr,visitSurvey))

    dFUV <- subset(var, 
                   coaddObjid==ID & visitFuv_mag>0,                   
                   select=c(coaddObjid,photoobsMJD,visitFuv_mag,visitFuv_magErr,visitSurvey))
    
    #print(dim(dNUV))
    #print(dim(dFUV))

    print(ID)
    outputFile <- paste("./Phase/ID",ID,".pdf",sep="")
    pdf(file=outputFile)

    #########################
    # Create NUV + FUV vars #
    #########################    
    t.n <- dNUV$photoobsMJD
    m.n <- dNUV$visitNuv_mag
    e.n <- dNUV$visitNuv_magErr
    s.n <- dNUV$visitSurvey

    t.f <- dFUV$photoobsMJD
    m.f <- dFUV$visitFuv_mag
    e.f <- dFUV$visitFuv_magErr
    s.f <- dFUV$visitSurvey

    ###################################
    # Calculate min/max for NUV + FUV #
    ###################################
    tmin.n <- min(t.n)
    tmax.n <- max(t.n)
    mmin.n <- min(m.n)
    mmax.n <- max(m.n)

    tmin.f <- min(t.f)
    tmax.f <- max(t.f)
    mmin.f <- min(m.f)
    mmax.f <- max(m.f)
    
    ##################################
    # Load period file for NUV + FUV #
    ##################################
    file.n <- paste("./MS_Period/pNUV/g",ID,".gx.candidate",sep="")
    periods.n <-read.table(file.n,header=F,sep="",na.string="N",
                           col.names=c("n_p0","n_p","n_chi2"),nrows=5)
    
    file.f <- paste("./MS_Period/pFUV/g",ID,".gx.candidate",sep="")
    periods.f <- read.table(file.f,header=F,sep="",na.string="N",
                            col.names=c("f_p0","f_p","f_chi2"),nrows=5)

    ###########################################
    # Set NUV + FUV periods for smallest CHI2 #
    ###########################################
    sort.periods.f <- periods.f[order(periods.f$f_chi2),]
    sort.periods.n <- periods.n[order(periods.n$n_chi2),]

    # Extract period with the minimum CHI2 for each period
    PERIOD.f <- sort.periods.f$f_p[1] 
    PERIOD.n <- sort.periods.n$n_p[1]
    
    # Extract the first period
    #PERIOD.f <- periods.f$f_p[1] 
    #PERIOD.n <- periods.n$n_p[1]
    
    ##########
    # Titles #
    ##########
    plot.f.title <- paste("Object ID",ID,"\nPeriod FUV =",toString(PERIOD.f))
    plot.n.title <- paste("Object ID",ID,"\nPeriod NUV =",toString(PERIOD.n))

    ################################
    # Fold NUV + FUV with PERIOD.f #
    ################################
    t0.n <- t.n[m.n==mmin.n]
    phi.n <- (t.n-t0.n)/PERIOD.f
    # get decimal portion of the phase, cycle is irrelevant
    # phi <- phi - trunc(phi)
    phip.n <- phi.n %%1
    # create an extended phase, just for the plot
    phim.n <- phip.n-1;
    # combine + and - phase, and other variables for plot.
    phiNew.n <- c(phim.n,phip.n)
    mNew.n <- c(m.n,m.n)
    eNew.n <- c(e.n,e.n)
    sNew.n <- c(s.n,s.n)
    dNew.n <- data.frame(phiNew.n,mNew.n,eNew.n,sNew.n)

    t0.f <- t.f[m.f==mmin.f]
    phi.f <- (t.f-t0.f)/PERIOD.f
    # get decimal portion of the phase, cycle is irrelevant
    # phi <- phi - trunc(phi)
    phip.f <- phi.f %%1
    # create an extended phase, just for the plot
    phim.f <- phip.f-1;
    # combine + and - phase, and other variables for plot.
    phiNew.f <- c(phim.f,phip.f)
    mNew.f <- c(m.f,m.f)
    eNew.f <- c(e.f,e.f)
    sNew.f <- c(s.f,s.f)
    dNew.f <- data.frame(phiNew.f,mNew.f,eNew.f,sNew.f)

    #######################
    # Plots with PERIOD.f #
    #######################
    phase.f_f <- ggplot() + 
      geom_point(aes(phiNew.f,mNew.f),data=dNew.f) + 
      scale_y_continuous(limits=c(mmin.f,mmax.f)) +
      scale_y_reverse() + 
      geom_errorbar(aes(x=phiNew.f,ymax=mNew.f+eNew.f,ymin=mNew.f-eNew.f), 
                    data=dNew.f, size=errbarSize) + 
      opts(title=plot.f.title) +
      xlab("Phase") + ylab("FUV") + 
      theme_bw()

    phase.n_f <- ggplot() + 
      geom_point(aes(phiNew.n,mNew.n),data=dNew.n) + 
      scale_y_continuous(limits=c(mmin.n,mmax.n)) +
      scale_y_reverse() + 
      geom_errorbar(aes(x=phiNew.n,ymax=mNew.n+eNew.n,ymin=mNew.n-eNew.n), 
                    data=dNew.n, size=errbarSize) +
      xlab("Phase") + ylab("NUV") + 
      theme_bw()

    
    ################################
    # Fold NUV + FUV with PERIOD.n #
    ################################
    t0.n <- t.n[m.n==mmin.n]
    phi.n <- (t.n-t0.n)/PERIOD.n
    # get decimal portion of the phase, cycle is irrelevant
    # phi <- phi - trunc(phi)
    phip.n <- phi.n %%1
    # create an extended phase, just for the plot
    phim.n <- phip.n-1;
    # combine + and - phase, and other variables for plot.
    phiNew.n <- c(phim.n,phip.n)
    mNew.n <- c(m.n,m.n)
    eNew.n <- c(e.n,e.n)
    sNew.n <- c(s.n,s.n)
    dNew.n <- data.frame(phiNew.n,mNew.n,eNew.n,sNew.n)

    t0.f <- t.f[m.f==mmin.f]
    phi.f <- (t.f-t0.f)/PERIOD.n
    # get decimal portion of the phase, cycle is irrelevant
    # phi <- phi - trunc(phi)
    phip.f <- phi.f %%1
    # create an extended phase, just for the plot
    phim.f <- phip.f-1;
    # combine + and - phase, and other variables for plot.
    phiNew.f <- c(phim.f,phip.f)
    mNew.f <- c(m.f,m.f)
    eNew.f <- c(e.f,e.f)
    sNew.f <- c(s.f,s.f)

    #color.n = mNew.f - mNew.n
    dNew.f <- data.frame(phiNew.f,mNew.f,eNew.f,sNew.f)
    
    #######################
    # Plots with PERIOD.n #
    #######################
    phase.f_n <- ggplot() + 
      geom_point(aes(phiNew.f,mNew.f),data=dNew.f) + 
      scale_y_continuous(limits=c(mmin.f,mmax.f)) +
      scale_y_reverse() + 
      geom_errorbar(aes(x=phiNew.f,ymax=mNew.f+eNew.f,ymin=mNew.f-eNew.f), 
                    data=dNew.f, size=errbarSize) + 
      opts(title=plot.n.title) + 
      xlab("Phase") + ylab("FUV") + 
      theme_bw()

    phase.n_n <- ggplot() + 
      geom_point(aes(phiNew.n,mNew.n),data=dNew.n) + 
      scale_y_continuous(limits=c(mmin.n,mmax.n)) +
      scale_y_reverse() + 
      geom_errorbar(aes(x=phiNew.n,ymax=mNew.n+eNew.n,ymin=mNew.n-eNew.n), 
                    data=dNew.n, size=errbarSize) +
      xlab("Phase") + ylab("NUV") + 
      theme_bw()

#      color.f <- mNew.f - mNew.n
#      cmin = min(color.f)
#      cmax = max(color.f)
#     
#     phase.color_f <- ggplot() + 
#       geom_point(aes(phiNew.f,color.f),data=dNew.f,size = 4) +   
#       scale_y_continuous(limits=c(-1,5)) +
#       xlab("Phase") + ylab("GALEX Magnitude") + 
#       theme_bw()
# 
#     phase.color_n <- ggplot() + 
#       geom_point(aes(phiNew.n,mNew.n),data=dNew.n,size = 4) +  
#       scale_y_continuous(limits=c(mmin.n,mmax.f)) +
#       scale_y_reverse() + 
#       geom_errorbar(aes(x=phiNew.n,ymax=mNew.n+eNew.n,ymin=mNew.n-eNew.n), data=dNew.n) +
#       geom_errorbar(aes(x=phiNew.f,ymax=mNew.f+eNew.f,ymin=mNew.f-eNew.f), data=dNew.f) + 
#       xlab("Phase") + ylab("GALEX Magnitude") + 
#       theme_bw()

    grid.arrange(phase.f_f,phase.f_n,
                 phase.n_f,phase.n_n,
                 #phase.color_f,phase.color_n,
                 nrow=2,ncol=2)
    
#     d1 <- data.frame(x=phiNew.n, y=mNew.n)
#     xy <- expand.grid(x=phiNew.n, y=mNew.n)
#     d2 <- data.frame(x=xy$x, y=xy$y, z= jitter(xy$x + xy$y))
#     
#     d1$panel <- "FUV"
#     d2$panel <- "NUV"
#     d1$z <- d1$x
#     
#     d <- rbind(d1, d2)
# 
#     p <- ggplot(data = d, mapping = aes(x = x, y = y))
#     p <- p + facet_grid(panel~., scale="free")
#     p <- p + layer(data= d1, geom = c( "point"), stat = "identity")
#     p <- p + layer(data= d2, geom = c("point"), stat = "identity")
#     
#     p

    dev.off()
    }

##################################################################################
### FUNCTION: ScorePeriod ######################################################## 
##################################################################################
# This function scores the periods like follows:
# 1. Compare P_fuv and P_nuv and select the one with smaller chi2
# 2. 
# First 5 periods are computed using the *** multi-harmonic function method ***
# Remaining 5 periods are computers using the *** phase-dispersion minimization ***
ScorePeriod <- function (ID) 
  {

    # Load FUV period from file
    file.f <- paste("./MS_Period/pFUV/g",ID,".gx.candidate",sep="")
    periods.f <- read.table(file.f,header=F,sep="",na.string="N",
                            col.names=c("f_p0","f_p","f_chi2"),nrows=5)

    # Load NUV period from file
    file.n <- paste("./MS_Period/pNUV/g",ID,".gx.candidate",sep="")
    periods.n <-read.table(file.n,header=F,sep="",na.string="N",
                           col.names=c("n_p0","n_p","n_chi2"),nrows=5)
    
    # Sort by chi2 in increasing order
    sort.periods.f <- periods.f[order(periods.f$f_chi2),]
    sort.periods.n <- periods.n[order(periods.n$n_chi2),]

    # Extract period with the minimum CHI2 for each period
    Pfuv <- sort.periods.f$f_p[1] 
    Pnuv <- sort.periods.n$n_p[1]
    
    print(periods.f)
    print(periods.n)
    print("********")
    print(sort.periods.f)
    print(sort.periods.n)
    
    print(Pfuv)
    print(Pnuv)
    print(Pfuv/Pnuv)
    

    print("Done")
  }

##################################################################################
### FUNCTION: ShowLikelihood ######################################################## 
##################################################################################
# This function show the log-likelihood for all periods for each 

ShowLikelihood <- function (ID, 
                         var, 
                         coaddObjid, 
                         photoobsMJD, 
                         visitNuv_mag, 
                         visitNuv_magErr, 
                         visitFuv_mag, 
                         visitFuv_magErr) 
  {
    ############################
    # Load ALL NUV + FUV  data #
    ############################
    dNUV <- subset(var, 
                   coaddObjid==ID & visitNuv_mag>0,,
                   select=c(coaddObjid,photoobsMJD,visitNuv_mag,visitNuv_magErr,visitSurvey))

    dFUV <- subset(var, 
                   coaddObjid==ID & visitFuv_mag>0,                   
                   select=c(coaddObjid,photoobsMJD,visitFuv_mag,visitFuv_magErr,visitSurvey))
    
    print(ID)
    outputFile <- paste("./Phase/ID",ID,".pdf",sep="")
    pdf(file=outputFile)

    
    #####
    ds <- read.table("~/Dropbox/R/MS_Period/184010-0047.chi2", quote="\"",nrows=101)
    x <- ds$V1
    y <- -log10(ds$V2)
    y <- y-min(y)
    y1 <- y/max(y)
    fit <- lm( y1 ~ poly(x,6), data=ds)
    ggplot(ds,aes(x,y1)) + geom_point() + stat_smooth(method = "lm", formula = y1 ~ poly(x,6)) + geom_hline(yintercept=exp(x=-0.5))
    
    fit <- lm( y1 ~ poly(x,6), data=ds)
    c <- coef(fit)
    yf = predict(fit) - exp(-0.5)  
    lkhood <- c[1] + c[2]*x + c[3]*x^2 + c[4]*x^3 + c[5]*x^4 + c[6]*x^5 + c[7]*x^7
    plot(x,yf)
    
    library("rootSolve")
    roots <- uniroot.all(yf,c(0,0.4))
    
}

