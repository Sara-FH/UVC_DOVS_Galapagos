#Code from Langlois, T.J., Fitzpatrick, B., Wakefield, C.B., Fairclough, D.V., Hesp, A., 
#McLean, D., Meeuwig, J.J., Harvey, E.S., in prep. 
#Similarities between fish length-frequency distributions estimated from line fishing 
#and baited stereo-video: novel application of kernel density estimates.

#Making dataframe from DOVS and UVC data, first run script Github_UVC_DOVS
KDE_all <- DOVS %>% select(ValidName, Length_cm, Method) %>% 
  rbind(UVC %>% select(ValidName, Length_cm, Method)) %>% 
  mutate(Length_cm = Length_cm*10) %>% 
  rename(Length = Length_cm) %>% 
  ungroup()

#Defining Sphyrna lewini to be used in formula for KDE
Sphyrna_lewini <- KDE_all %>% filter(ValidName == "Sphyrna lewini")

#Function kde.compare from Langlois et al. 2012
kde.compare <- function(length = Sphyrna_lewini$Length,
                        group = Sphyrna_lewini$Method,
                        align = c('no','by.median')[1],
                        nboot = 500,
                        xlab = 'Fish Length (mm)',
                        ylab = 'Probability Density',
                        main = ''
){
  
  #check for required packages ('sm' and 'KernSmooth') and load if they are already installed or 
  #install and load them if they are not already installed
  check <- require(sm)
  if(check == FALSE) {install.packages('sm')}
  ks.check <- require(KernSmooth,quietly = TRUE)
  if(ks.check == FALSE) {install.packages('KernSmooth')}
  print(paste(c('Dependencies Installed:',require(sm,quietly = TRUE),require(KernSmooth,quietly = TRUE))))
  
  library(sm) #should load without error
  
  library(KernSmooth) ##should load without error
  
  Data <- data.frame(length,group)
  
  #distribution alignment
  
  method <- unique(Data$group) #Here I replaced levels with unique, to make it work
  
  length.aligner <- function(Data){
    
    for(i in 1:length(method)){
      
      yi=Data[Data$group==method[i],]$length
      med=median(yi)
      sc=diff(quantile(yi,c(0.25,0.75)))/1.349
      Data[Data$group==method[i],]$length<-(yi - med)/sc
    }
    
    return(Data)
    
  }    
  
  if(align==c('by.median')){Data <- length.aligner(Data)}
  
  #bandwidth calculation
  
  h.mean <- function(Data){
    
    hl=dpik(Data$length[Data$group==method[1]],kernel='normal')
    hv=dpik(Data$length[Data$group==method[2]],kernel='normal')
    h.m=(hl+hv)/2 #arithmetic mean of the KDEs
    return(h.m)
  }
  
  #####################
  
  sm.density.compare3 <- function (x, group, h, model = "none", ...) 
  {
    if (!is.vector(x)) 
      stop("sm.density.compare can handle only 1-d data")
    opt <- sm.options(list(...))
    sm:::replace.na(opt, ngrid, 50)
    sm:::replace.na(opt, display, "line")
    sm:::replace.na(opt, xlab, deparse(substitute(x)))
    sm:::replace.na(opt, ylab, "Density")
    sm:::replace.na(opt, xlim, c(min(x) - diff(range(x))/4, max(x) + 
                                   diff(range(x))/4))
    sm:::replace.na(opt, eval.points, seq(opt$xlim[1], opt$xlim[2], 
                                          length = opt$ngrid))
    if (is.na(opt$band)) {
      if (model == "none") 
        opt$band <- FALSE
      else opt$band <- TRUE
    }
    if ((model == "none") && opt$band) 
      opt$band <- FALSE
    band <- opt$band
    ngrid <- opt$ngrid
    xlim <- opt$xlim
    nboot <- opt$nboot
    y <- x
    if (is.na(opt$test)) {
      if (model == "none") 
        opt$test <- FALSE
      else opt$test <- TRUE
    }
    if ((model == "none") && opt$test) 
      opt$test <- FALSE
    test <- opt$test
    if (opt$display %in% "none") 
      band <- FALSE
    fact <- factor(group)
    fact.levels <- levels(fact)
    nlev <- length(fact.levels)
    ni <- table(fact)
    if (band & (nlev > 2)) {
      cat("Reference band available to compare two groups only.", 
          "\n")
      band <- FALSE
    }
    if (length(opt$lty) < nlev) 
      opt$lty <- 1:nlev
    if (length(opt$col) < nlev) 
      opt$col <- 2:(nlev + 1)
    if (missing(h)) 
      h <- h.select(x, y = NA, group = group, ...)
    opt$band <- band
    opt$test <- test
    estimate <- matrix(0, ncol = opt$ngrid, nrow = nlev)
    se <- matrix(0, ncol = opt$ngrid, nrow = nlev)
    for (i in 1:nlev) {
      sm <- sm.density(y[fact == fact.levels[i]], h = h, display = "none", 
                       eval.points = opt$eval.points)
      estimate[i, ] <- sm$estimate
      se[i, ] <- sm$se
    }
    eval.points <- sm$eval.points
    if (!(opt$display %in% "none" | band)) {
      plot(xlim, c(0, 1.1 * max(as.vector(estimate))), xlab = opt$xlab, 
           ylab = opt$ylab, type = "n")
      for (i in 1:nlev) lines(eval.points, estimate[i, ], lty = opt$lty[i], 
                              col = opt$col[i])
    }
    est <- NULL
    p <- NULL
    if (model == "equal" & test) {
      if (nlev == 2) {
        ts <- sum((estimate[1, ] - estimate[2, ])^2)
      }
      else {
        sm.mean <- sm.density(y, h = h, xlim = opt$xlim, 
                              ngrid = opt$ngrid, display = "none")$estimate
        ts <- 0
        for (i in 1:nlev) ts <- ts + ni[i] * sum((estimate[i, 
        ] - sm.mean)^2)
      }
      p <- 0
      est.star <- matrix(0, ncol = opt$ngrid, nrow = nlev)
      for (iboot in 1:nboot) {
        ind <- (1:length(y))
        for (i in 1:nlev) {
          indi <- sample((1:length(ind)), ni[i])
          est.star[i, ] <- sm.density(y[ind[indi]], h = h, 
                                      ngrid = opt$ngrid, xlim = opt$xlim, display = "none")$estimate
          ind <- ind[-indi]
        }
        if (nlev == 2) {
          ts.star <- sum((est.star[1, ] - est.star[2, ])^2)
        }
        else {
          sm.mean <- sm.density(y, h = h, xlim = opt$xlim, 
                                ngrid = opt$ngrid, display = "none")$estimate
          ts.star <- 0
          for (i in 1:nlev) {
            ts.star <- ts.star + ni[i] * sum((est.star[i, 
            ] - sm.mean)^2)
          }
        }
        if (ts.star > ts) 
          p <- p + 1
        if (opt$verbose > 1) {
          cat(iboot)
          cat(" ")
        }
      }
      p <- p/nboot
      cat("\nTest of equal densities:  p-value = ", round(p, 
                                                          3), "\n")
      est <- list(p = p, h = h)
    }
    if (model == "equal" & band) {
      av <- (sqrt(estimate[1, ]) + sqrt(estimate[2, ]))/2
      se <- sqrt(se[1, ]^2 + se[2, ]^2)
      upper <- (av + se)^2
      lower <- pmax(av - se, 0)^2
      plot(xlim, c(0, 1.1 * max(as.vector(estimate), upper)), 
           xlab = opt$xlab, ylab = opt$ylab, type = "n")
      polygon(c(eval.points, rev(eval.points)), c(upper, rev(lower)), 
              col = opt$col.band, border = 0) #my sole edit: the colour of the variability band produced by this line of code now is drawn from the colour argument supplied by sm.options() rather than ignoring this option and always producing a cyan band as it did in the original sm.density.compare() function
      lines(eval.points, estimate[1, ], lty = opt$lty[1], col = opt$col[1])
      lines(eval.points, estimate[2, ], lty = opt$lty[2], col = opt$col[2])
      est <- list(p = p, upper = upper, lower = lower, h = h)
    }
    invisible(est)
  }
  
  with(Data,sm.density.compare3(x = length, group = group, h =  h.mean(Data),nboot=nboot, model = "equal",ngrid=500,col=c('black','black'),lty=c(1,2),xlab=paste(xlab),ylab=paste(ylab),col.band="grey"))
  #SFH: I have changed the lty from c(2,3) to c(1,2)
  
  mtext(paste(main),font=3,cex=1,side=3,line=1.5)
  
  legend(x='topright',lty=c(1,2),legend=c(paste(method[1]),paste(method[2]))) 
  #SFH: I have changed the lty from c(2,3) to c(1,2)
  
} # End of Function: kde.compare

#KDE for Sphyrna lewini
kde.compare(length = Sphyrna_lewini$Length, group = Sphyrna_lewini$Method, 
            align = 'no', nboot = 500, xlab = 'Fish Length (mm)', ylab = 'Probability Density',
            main = 'Sphyrna lewini')


# KDE for species of interest ---------------------------------------------

#Defining variables for species
Aetobatus_laticeps <- KDE_all %>% filter(ValidName == "Aetobatus laticeps")
Caranx_lugubris <- KDE_all %>% filter(ValidName == "Caranx lugubris")
Caranx_melampygus <- KDE_all %>% filter(ValidName == "Caranx melampygus")
Carcharhinus_galapagensis <- KDE_all %>% filter(ValidName == "Carcharhinus galapagensis")
Carcharhinus_limbatus <- KDE_all %>% filter(ValidName == "Carcharhinus limbatus")
Dermatolepis_dermatolepis <- KDE_all %>% filter(ValidName == "Dermatolepis dermatolepis")
Hoplopagrus_guentherii <- KDE_all %>% filter(ValidName == "Hoplopagrus guentherii")
Hypanus_dipterurus <- KDE_all %>% filter(ValidName == "Hypanus dipterurus")
Lutjanus_argentiventris <- KDE_all %>% filter(ValidName == "Lutjanus argentiventris")
Lutjanus_novemfasciatus <- KDE_all %>% filter(ValidName == "Lutjanus novemfasciatus")
Mycteroperca_olfax <- KDE_all %>% filter(ValidName == "Mycteroperca olfax")
Paralabrax_albomaculatus <- KDE_all %>% filter(ValidName == "Paralabrax albomaculatus")
Seriola_rivoliana <- KDE_all %>% filter(ValidName == "Seriola rivoliana")
Triaenodon_obesus <- KDE_all %>% filter(ValidName == "Triaenodon obesus")


#Making KDE's for species of interest

#Sphyrna_lewini
kde.compare(length = Sphyrna_lewini$Length, group = Sphyrna_lewini$Method, 
            align = 'no', nboot = 500, xlab = 'Fish Length (mm)', ylab = 'Probability Density',
            main = 'Sphyrna lewini') #It works when I do not define them
#But then how can I arrange them in one big plot?

a <- kde.compare(length = Sphyrna_lewini$Length, group = Sphyrna_lewini$Method, 
            align = 'no', nboot = 500, xlab = 'Fish Length (mm)', ylab = 'Probability Density',
            main = 'Sphyrna lewini')
a
#Aetobatus_laticeps
b <- kde.compare(length = Aetobatus_laticeps$Length, group = Aetobatus_laticeps$Method, 
            align = 'no', nboot = 500, xlab = 'Fish Length (mm)', ylab = 'Probability Density',
            main = 'Aetobatus laticeps')
b
#Caranx_lugubris
c <- kde.compare(length = Caranx_lugubris$Length, group = Caranx_lugubris$Method, 
            align = 'no', nboot = 500, xlab = 'Fish Length (mm)', ylab = 'Probability Density',
            main = 'Caranx lugubris')
c
#Caranx_melampygus
d <- kde.compare(length = Caranx_melampygus$Length, group = Caranx_melampygus$Method, 
            align = 'no', nboot = 500, xlab = 'Fish Length (mm)', ylab = 'Probability Density',
            main = 'Caranx melampygus')
d
#Carcharhinus_galapagensis
e <- kde.compare(length = Carcharhinus_galapagensis$Length, group = Carcharhinus_galapagensis$Method, 
            align = 'no', nboot = 500, xlab = 'Fish Length (mm)', ylab = 'Probability Density',
            main = 'Carcharhinus galapagensis')
e
#Carcharhinus_limbatus
f <- kde.compare(length = Carcharhinus_limbatus$Length, group = Carcharhinus_limbatus$Method, 
            align = 'no', nboot = 500, xlab = 'Fish Length (mm)', ylab = 'Probability Density',
            main = 'Carcharhinus limbatus')
f
#Dermatolepis_dermatolepis
g <- kde.compare(length = Dermatolepis_dermatolepis$Length, group = Dermatolepis_dermatolepis$Method, 
            align = 'no', nboot = 500, xlab = 'Fish Length (mm)', ylab = 'Probability Density',
            main = 'Dermatolepis dermatolepis')
g
#Hoplopagrus_guentherii
h <- kde.compare(length = Hoplopagrus_guentherii$Length, group = Hoplopagrus_guentherii$Method, 
            align = 'no', nboot = 500, xlab = 'Fish Length (mm)', ylab = 'Probability Density',
            main = 'Hoplopagrus guentherii')
h
#Hypanus_dipterurus
i <- kde.compare(length = Hypanus_dipterurus$Length, group = Hypanus_dipterurus$Method, 
            align = 'no', nboot = 500, xlab = 'Fish Length (mm)', ylab = 'Probability Density',
            main = 'Hypanus dipterurus')
i
#Lutjanus_argentiventris
j <- kde.compare(length = Lutjanus_argentiventris$Length, group = Lutjanus_argentiventris$Method, 
            align = 'no', nboot = 500, xlab = 'Fish Length (mm)', ylab = 'Probability Density',
            main = 'Lutjanus argentiventris')
j
#Lutjanus_novemfasciatus
k <- kde.compare(length = Lutjanus_novemfasciatus$Length, group = Lutjanus_novemfasciatus$Method, 
            align = 'no', nboot = 500, xlab = 'Fish Length (mm)', ylab = 'Probability Density',
            main = 'Lutjanus novemfasciatus')
k
#Mycteroperca_olfax
l <- kde.compare(length = Mycteroperca_olfax$Length, group = Mycteroperca_olfax$Method, 
            align = 'no', nboot = 500, xlab = 'Fish Length (mm)', ylab = 'Probability Density',
            main = 'Mycteroperca olfax')
l
#Paralabrax_albomaculatus
m <- kde.compare(length = Paralabrax_albomaculatus$Length, group = Paralabrax_albomaculatus$Method, 
            align = 'no', nboot = 500, xlab = 'Fish Length (mm)', ylab = 'Probability Density',
            main = 'Paralabrax albomaculatus')
m
#Seriola_rivoliana
n <- kde.compare(length = Seriola_rivoliana$Length, group = Seriola_rivoliana$Method, 
            align = 'no', nboot = 500, xlab = 'Fish Length (mm)', ylab = 'Probability Density',
            main = 'Seriola rivoliana')
n
#Triaenodon_obesus
o <- kde.compare(length = Triaenodon_obesus$Length, group = Triaenodon_obesus$Method, 
            align = 'no', nboot = 500, xlab = 'Fish Length (mm)', ylab = 'Probability Density',
            main = 'Triaenodon obesus')
o

#Species of interest KDE one figure
ggarrange(a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, 
          ncol = 3, nrow = 5)
#This does not work
#I think it has something to do with the kde.compare function not being saved as a plot in the letters

