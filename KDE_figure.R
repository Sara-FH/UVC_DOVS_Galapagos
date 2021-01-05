##########################################################
#
# KDE figure for species of interest
#
##########################################################

# Load libraries ----------------------------------------------------------

library(grid) #For textGrob in ggplot


# Prepare species data for KDE --------------------------------------------
#Run code from Github_UVC_DOVS script from the beginning to "Biomass boxplot"

#Making dataframe from DOVS and UVC data, first run script Github_UVC_DOVS
KDE_all <- DOVS %>% select(ValidName, Length_cm, Method) %>% 
  #Length to mm
  mutate(Length_cm = Length_cm*10) %>% 
  #Renaming length column
  rename(Length = Length_cm) %>% 
  rbind(UVC %>% 
          select(ValidName, Length_cm, Method) %>% 
          #UVC length data in cm to mm
          mutate(Length_cm = Length_cm*10) %>% 
          rename(Length = Length_cm)) %>% 
  ungroup() %>% 
  #recoding Method name for DOVS to be Stereo-DOVs
  mutate(Method = recode(Method, "DOVS" = "Stereo-DOVs"))

#Defining Sphyrna lewini to be used in formula for KDE
Sphyrna_lewini <- KDE_all %>% filter(ValidName == "Sphyrna lewini")

# Code from Langlois et al. 2012 modified by D. F. Arcos ------------------

#Code from Langlois, T.J., Fitzpatrick, B., Wakefield, C.B., Fairclough, D.V., Hesp, A., McLean, D., 
#Meeuwig, J.J., Harvey, E.S., in prep. Similarities between fish length-frequency distributions estimated 
#from line fishing and baited stereo-video: novel application of kernel density estimates.

#Modified by Denisse Fierro Arcos (DFA): Using ggplot to obtain graphs as outputs for further manipulation 
#in addition to estimates and p values originally given by the sm.density.compare() function
#Date: 2020-03-26

kde.compare <- function(length = Sphyrna_lewini$Length,
                        group = Sphyrna_lewini$Method,
                        align = c('no','by.median')[1],
                        nboot = 500,
                        xlab = 'Fish Length (mm)',
                        ylab = 'Probability Density',
                        main = ''){
  
  #check for required packages ('sm' and 'KernSmooth') and load if they are already installed 
  #or install and load them if they are not already installed
  check <- require(sm)
  if(check == FALSE) {install.packages('sm')}
  ks.check <- require(KernSmooth, quietly = TRUE)
  if(ks.check == FALSE) {install.packages('KernSmooth')}
  #Adding tidyverse to be able to use pipes and ggplot - DFA
  tidy.check <- require(tidyverse, quietly = TRUE)
  if(tidy.check == FALSE) {install.packages('tidyverse')}
  print(paste(c('Dependencies Installed:', require(sm,quietly = TRUE), require(KernSmooth, quietly = TRUE),
                require(tidyverse, quietly = T))))
  
  library(sm) #should load without error
  library(KernSmooth) ##should load without error
  library(tidyverse)
  
  Data <- data.frame(length, group)
  
  #distribution alignment
  method <- unique(Data$group) #SFH: #Here I replaced levels with unique, to make it work
  length.aligner <- function(Data){
    for(i in 1:length(method)){
      yi = Data[Data$group == method[i],]$length
      med = median(yi)
      sc = diff(quantile(yi,c(0.25,0.75)))/1.349
      Data[Data$group==method[i],]$length <- (yi - med)/sc
    }
    return(Data)}    
  
  if(align == c('by.median')){Data <- length.aligner(Data)}
  
  #bandwidth calculation
  
  h.mean <- function(Data){
    hl = dpik(Data$length[Data$group == method[1]], kernel='normal')
    hv = dpik(Data$length[Data$group == method[2]], kernel='normal')
    h.m = (hl+hv)/2 #arithmetic mean of the KDEs
    return(h.m)}
  
  #####################
  
  sm.density.compare3 <- function (x, group, h, model = "none", ...) 
  {if (!is.vector(x)) 
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
      else opt$band <- TRUE}
    
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
      else opt$test <- TRUE}
    
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
      band <- FALSE}
    
    if (length(opt$lty) < nlev) 
      opt$lty <- 1:nlev
    
    if (length(opt$col) < nlev) 
      opt$col <- 2:(nlev + 1)
    
    if (missing(h)) 
      h <- h.select(x, y = NA, group = group, ...)
    opt$band <- band
    opt$test <- test
    #Estimate1 added as data frame to ease in the creation of graphs in ggplot - DFA
    estimate1 <- data.frame(est = rep(0, opt$ngrid*nlev), 
                            fact = rep(fact.levels, each = opt$ngrid))
    se <- matrix(0, ncol = opt$ngrid, nrow = nlev)
    
    for (i in 1:nlev) {
      sm <- sm.density(y[fact == fact.levels[i]], h = h, display = "none", 
                       eval.points = opt$eval.points)
      estimate1$est[estimate1$fact == fact.levels[i]] <- sm$estimate
      se[i, ] <- sm$se}
    
    #Estimate1 converted into matrix as set in original code by Bowan & Azzalini - DFA
    estimate <- estimate1 %>% mutate(id = rep(1:opt$ngrid, nlev)) %>% 
      pivot_wider(names_from = fact, values_from = est) %>% 
      select(-id) %>% t()
    eval.points <- sm$eval.points
    
    if (!(opt$display %in% "none" | band)) {
      #Swapping ggplot for base graphic commands to create graphs - DFA
      fig <- ggplot(mapping = aes(x = rep(eval.points, nlev), y = estimate1$est, 
                                  linetype = estimate1$fact))+
        xlim(xlim)+ylim(0, 1.1*max(as.vector(estimate1$est)))+
        labs(x = opt$xlab, y = opt$ylab, title = main)+
        geom_line()+theme_bw()+
        theme(legend.title = element_blank(),
              legend.position = c(0.85,0.85),
              axis.title = element_text(family = "sans", size = 12), 
              axis.text = element_text(family = "sans", size = 12),
              legend.text = element_text(family = "sans", size = 12))} 
    
    est <- NULL
    p <- NULL
    
    if (model == "equal" & test) {
      if (nlev == 2) {
        ts <- sum((estimate[1, ] - estimate[2, ])^2)
      }else {
        sm.mean <- sm.density(y, h = h, xlim = opt$xlim, 
                              ngrid = opt$ngrid, display = "none")$estimate
        ts <- 0
        for (i in 1:nlev) ts <- ts + ni[i] * sum((estimate[i,] - sm.mean)^2)}
      
      p <- 0
      est.star <- matrix(0, ncol = opt$ngrid, nrow = nlev)
      
      for (iboot in 1:nboot) {
        ind <- (1:length(y))
        for (i in 1:nlev) {
          indi <- sample((1:length(ind)), ni[i])
          est.star[i, ] <- sm.density(y[ind[indi]], h = h, 
                                      ngrid = opt$ngrid, xlim = opt$xlim, display = "none")$estimate
          ind <- ind[-indi]}
        
        if (nlev == 2) {
          ts.star <- sum((est.star[1, ] - est.star[2, ])^2)
        }else {
          sm.mean <- sm.density(y, h = h, xlim = opt$xlim, 
                                ngrid = opt$ngrid, display = "none")$estimate
          ts.star <- 0
          
          for (i in 1:nlev) {
            ts.star <- ts.star + ni[i] * sum((est.star[i,] - sm.mean)^2)}}
        
        if (ts.star > ts) 
          p <- p + 1
        
        if (opt$verbose > 1) {
          cat(iboot)
          cat(" ")}}
      
      p <- p/nboot
      cat("\nTest of equal densities:  p-value = ", round(p,3), "\n")
      est <- list(p = p, h = h)}
    
    if (model == "equal" & band) {
      av <- (sqrt(estimate[1, ]) + sqrt(estimate[2, ]))/2
      se <- sqrt(se[1, ]^2 + se[2, ]^2)
      upper <- (av + se)^2
      lower <- pmax(av - se, 0)^2
      #Swapping ggplot for base graphic commands to create graphs - DFA
      fig <- ggplot(mapping = aes(x = rep(eval.points, nlev), y = estimate1$est, 
                                  linetype = estimate1$fact))+
        xlim(xlim)+
        ylim(0, 1.1*max(as.vector(estimate1$est), upper))+
        #SFH: title added to x-axis formerly opt$xlab
        labs(x = "Fish length (mm)", y = opt$ylab, title = main)+ 
        geom_ribbon(aes(x = rep(eval.points, 2),
                        ymin = rep(lower,2), ymax = rep(upper,2)),
                    fill = opt$col.band, show.legend = F)+
        geom_line()+theme_bw()+
        theme(legend.title = element_blank(),
              legend.position = c(0.85,0.85),
              axis.title = element_text(color = "black", family = "sans", size = 12), 
              axis.text = element_text(color = "black", family = "sans", size = 12),
              legend.text = element_text(family = "sans", size = 12))
      est <- list(p = p, upper = upper, lower = lower, h = h)}
    
    invisible(est)
    #Creating output list containing two elements: ggplot and estimates - DFA
    return(list(graph = fig, est = est))}
  
  with(Data,sm.density.compare3(x = length, group = group, h = h.mean(Data),
                                nboot = nboot, model = "equal", ngrid=500,
                                col = c('black','black'),
                                xlab = paste(xlab),
                                ylab = paste(ylab), col.band = "grey"))} 
# End of Function: kde.compare

#Example species KDE
kde.compare(length = Sphyrna_lewini$Length,
            group = Sphyrna_lewini$Method,
            align = "no",
            nboot = 500,
            xlab = "",
            ylab = "Probability Density",
            main = "Sphyrna lewini")

# KDE for species of interest ---------------------------------------------

#Variable with species of interest
spInt <- data.frame(
  ValidName = c("Sphyrna lewini", "Lutjanus argentiventris", "Mycteroperca olfax", 
                "Triaenodon obesus", "Paralabrax albomaculatus", "Hypanus dipterurus",
                "Carcharhinus galapagensis", "Carcharhinus limbatus", "Caranx melampygus", 
                "Aetobatus laticeps", "Lutjanus novemfasciatus", "Hoplopagrus guentherii", 
                "Caranx lugubris", "Dermatolepis dermatolepis"))

#Creating empty lists to save figures
fig_list <- list()

#Loop to make KDE figures for all species of interest
for(i in seq_along(spInt$ValidName)){
  #Getting species from KDE_all data frame
  sp <- KDE_all %>% filter(ValidName == spInt$ValidName[i])
  #Producing KDE figure for a species
  f1 <- kde.compare(length = sp$Length,
                    group = sp$Method,
                    align = "no",
                    nboot = 500,
                    xlab = "",
                    ylab = "Probability Density",
                    main = sp$ValidName)
  
  #p-value for plot
  p <- ifelse(format(round(f1$est$p, digits = 3))>0, 
              paste0("p = ", format(round(f1$est$p, digits = 3))), 
              paste0("p < 0.001"))
  #Create text for figure
  grob <- grobTree(textGrob(c(paste0("n = ", nrow(sp)), p), 
                            x=c(0.74,0.74),  y=c(0.83, 0.93), hjust=c(0,0),
                            gp=gpar(col="black", fill = "white", fontsize=12, fontface="plain")))
  #Adding text to KDE plot
  f1$graph <- f1$graph + 
    annotation_custom(grob)
  
  #Saving figure in empty list created at the top
  fig_list[[i]] <- f1$graph
  
}


#Creating composite image using the list of plots saved in the loop above
KDE_fig <- ggarrange(plotlist = fig_list, ncol = 4, nrow = 4, 
                     labels = c("A)", "B)", "C)", "D)", "E)", "F)", "G)", "H)", 
                                "I)", "J)", "K)", "L)", "M)", "N)"),
                     font.label = list(size = 14),
                     common.legend = TRUE, hjust = -1.2,  vjust = 1.4)

KDE_fig

#Saving composite image as tiff file and changing dimensions
ggsave("Figures/KDE.tiff", KDE_fig, device = "tiff", dpi = 300, width = 15, height = 10)


#ERROR for this species - but why?
Seriola_rivoliana <- KDE_all %>% filter(ValidName == "Seriola rivoliana")

#Example species KDE
kde.compare(length = Seriola_rivoliana$Length,
            group = Seriola_rivoliana$Method,
            align = "no",
            nboot = 500,
            xlab = "",
            ylab = "Probability Density",
            main = "Seriola rivoliana")

rm(Seriola_rivoliana)


#Remove used variables
rm(KDE_all, Sphyrna_lewini, kde.compare, spInt, fig_list, 
   i, sp, f1, p, grob, KDE_fig)
