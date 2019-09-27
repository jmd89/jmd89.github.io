# useful functions from http://www.cookbook-r.com/Graphs/Plotting_means_and_error_bars_(ggplot2)/

## Gives count, mean, standard deviation, standard error of the mean, and confidence interval (default 95%).
##   data: a data frame.
##   measurevar: the name of a column that contains the variable to be summariezed
##   groupvars: a vector containing names of columns that contain grouping variables
##   na.rm: a boolean that indicates whether to ignore NA's
##   conf.interval: the percent range of the confidence interval (default is 95%)
summarySE <- function(data=NULL, measurevar, groupvars=NULL, na.rm=FALSE,
                      conf.interval=.95, .drop=TRUE) {
  library(plyr)
  
  # New version of length which can handle NA's: if na.rm==T, don't count them
  length2 <- function (x, na.rm=FALSE) {
    if (na.rm) sum(!is.na(x))
    else       length(x)
  }
  
  # This does the summary. For each group's data frame, return a vector with
  # N, mean, and sd
  datac <- ddply(data, groupvars, .drop=.drop,
                 .fun = function(xx, col) {
                   c(N    = length2(xx[[col]], na.rm=na.rm),
                     mean = mean   (xx[[col]], na.rm=na.rm),
                     sd   = sd     (xx[[col]], na.rm=na.rm)
                   )
                 },
                 measurevar
  )
  
  # Rename the "mean" column    
  datac <- rename(datac, c("mean" = measurevar))
  
  datac$se <- datac$sd / sqrt(datac$N)  # Calculate standard error of the mean
  
  # Confidence interval multiplier for standard error
  # Calculate t-statistic for confidence interval: 
  # e.g., if conf.interval is .95, use .975 (above/below), and use df=N-1
  ciMult <- qt(conf.interval/2 + .5, datac$N-1)
  datac$ci <- datac$se * ciMult
  
  return(datac)
}

## Norms the data within specified groups in a data frame; it normalizes each
## subject (identified by idvar) so that they have the same mean, within each group
## specified by betweenvars.
##   data: a data frame.
##   idvar: the name of a column that identifies each subject (or matched subjects)
##   measurevar: the name of a column that contains the variable to be summariezed
##   betweenvars: a vector containing names of columns that are between-subjects variables
##   na.rm: a boolean that indicates whether to ignore NA's
normDataWithin <- function(data=NULL, idvar, measurevar, betweenvars=NULL,
                           na.rm=FALSE, .drop=TRUE) {
  library(plyr)
  
  # Measure var on left, idvar + between vars on right of formula.
  data.subjMean <- ddply(data, c(idvar, betweenvars), .drop=.drop,
                         .fun = function(xx, col, na.rm) {
                           c(subjMean = mean(xx[,col], na.rm=na.rm))
                         },
                         measurevar,
                         na.rm
  )
  
  # Put the subject means with original data
  data <- merge(data, data.subjMean)
  
  # Get the normalized data in a new column
  measureNormedVar <- paste(measurevar, "_norm", sep="")
  data[,measureNormedVar] <- data[,measurevar] - data[,"subjMean"] +
    mean(data[,measurevar], na.rm=na.rm)
  
  # Remove this subject mean column
  data$subjMean <- NULL
  
  return(data)
}

## Summarizes data, handling within-subjects variables by removing inter-subject variability.
## It will still work if there are no within-S variables.
## Gives count, un-normed mean, normed mean (with same between-group mean),
##   standard deviation, standard error of the mean, and confidence interval.
## If there are within-subject variables, calculate adjusted values using method from Morey (2008).
##   data: a data frame.
##   measurevar: the name of a column that contains the variable to be summariezed
##   betweenvars: a vector containing names of columns that are between-subjects variables
##   withinvars: a vector containing names of columns that are within-subjects variables
##   idvar: the name of a column that identifies each subject (or matched subjects)
##   na.rm: a boolean that indicates whether to ignore NA's
##   conf.interval: the percent range of the confidence interval (default is 95%)
summarySEwithin <- function(data=NULL, measurevar, betweenvars=NULL, withinvars=NULL,
                            idvar=NULL, na.rm=FALSE, conf.interval=.95, .drop=TRUE) {
  
  # Ensure that the betweenvars and withinvars are factors
  factorvars <- vapply(data[, c(betweenvars, withinvars), drop=FALSE],
                       FUN=is.factor, FUN.VALUE=logical(1))
  
  if (!all(factorvars)) {
    nonfactorvars <- names(factorvars)[!factorvars]
    message("Automatically converting the following non-factors to factors: ",
            paste(nonfactorvars, collapse = ", "))
    data[nonfactorvars] <- lapply(data[nonfactorvars], factor)
  }
  
  # Get the means from the un-normed data
  datac <- summarySE(data, measurevar, groupvars=c(betweenvars, withinvars),
                     na.rm=na.rm, conf.interval=conf.interval, .drop=.drop)
  
  # Drop all the unused columns (these will be calculated with normed data)
  datac$sd <- NULL
  datac$se <- NULL
  datac$ci <- NULL
  
  # Norm each subject's data
  ndata <- normDataWithin(data, idvar, measurevar, betweenvars, na.rm, .drop=.drop)
  
  # This is the name of the new column
  measurevar_n <- paste(measurevar, "_norm", sep="")
  
  # Collapse the normed data - now we can treat between and within vars the same
  ndatac <- summarySE(ndata, measurevar_n, groupvars=c(betweenvars, withinvars),
                      na.rm=na.rm, conf.interval=conf.interval, .drop=.drop)
  
  # Apply correction from Morey (2008) to the standard error and confidence interval
  #  Get the product of the number of conditions of within-S variables
  nWithinGroups    <- prod(vapply(ndatac[,withinvars, drop=FALSE], FUN=nlevels,
                                  FUN.VALUE=numeric(1)))
  correctionFactor <- sqrt( nWithinGroups / (nWithinGroups-1) )
  
  # Apply the correction factor
  ndatac$sd <- ndatac$sd * correctionFactor
  ndatac$se <- ndatac$se * correctionFactor
  ndatac$ci <- ndatac$ci * correctionFactor
  
  # Combine the un-normed means with the normed results
  merge(datac, ndatac)
}

# for plotting 

errBars <- function(means, error, xpos = NULL, whiskerWidth = .1){
  # conveniance function for plotting error bars
  # xpos allows user to specify x axis position (i.e. for jittering)
  if (is.null(xpos)){
    xpos = 1:length(means)
  }
  # error bar
  segments(x0 = xpos, y0 = means - error, x1 = xpos, y1 = means + error)
  # bottom whisker
  segments(x0 = xpos - whiskerWidth/2, y0 = means - error, x1 = xpos + whiskerWidth/2, y1 = means - error)
  # top whisker
  segments(x0 = xpos - whiskerWidth/2, y0 = means + error, x1 = xpos + whiskerWidth/2, y1 = means + error)
}

logistic <- function(x){
  1/(1 + exp(-x))
}

faintCol <- function(colLabs, alpha = 30){
  colVec = c()
  for (i in colLabs){
    RGB = col2rgb(i)
    newcol = rgb(red = RGB[1], green = RGB[2], blue = RGB[3], alpha = alpha, maxColorValue = 255)
    colVec = c(colVec, newcol)
  }
  return(colVec)
}

#### FUNCTIONS FOR SERIAL POSITION

getSerialPos <- function(data, list_length){
  # reshapes data to look at serial order
  tmp.dat = subset(data, memSpan == list_length) 
  
  tmp.accs = matrix(NA, nrow = nrow(tmp.dat), ncol=list_length)
  
  for (i in 1:nrow(tmp.dat)){
    tmp.accs[i,] <- as.integer(strsplit(tmp.dat$memItems[i], split='-')[[1]] == strsplit(tmp.dat$memResps[i], split='-')[[1]])
  }
  
  colnames(tmp.accs) <- paste0('pos.', 1:list_length)
  
  tmp.dat <- cbind(tmp.dat, tmp.accs)
  
  tmp.dat = gather(data = tmp.dat, pos, Acc,  grep(pattern = 'pos.', x = colnames(tmp.dat))) %>% 
    arrange(ID, condition, trial)
  
  tmp.dat$pos = as.factor(tmp.dat$pos)
  
  return(tmp.dat)
}


plotSerialPos <- function(data, ylim = c(.4, 1), title=''){
  library(RColorBrewer)
  
  # plot serial position curves
  # data = an aggregated data set (initially created using getSerialPos then aggregated)
  shapes = 16:21
  colors = brewer.pal(6, "Dark2")
  
  #par(mfrow=c(2,1), mar=c(.8, .4, 0, 0), oma = c(3,3.5,2,2))
  
  list_l = length(unique(data$pos))
  
  plot(NA, xlim=c(1, list_l), ylim=ylim, ylab='', xlab='', axes=F)
  box()
  for (l in levels(data$condition)){
    
    ln = which(l == levels(data$condition))
    
    with(subset(data, condition==l), errBars(means = acc, error = ci))
    with(subset(data, condition==l), points(acc, type='b', pch=shapes[ln], col=colors[ln]))
    
    #mtext(cond, 4, line = .2)
    axis(2)
  }
  axis(1)
  
  mtext(title, 3, line=.3, outer=T)
  mtext('Proportion Correct', 2, line = 2.5)
  mtext('Position', 1, line = 2.5)
}

getTranspositions <- function(data, list_length){
  # reshapes data and gets transposition distance
  # and type of response (correct, extra list, pass, transpositions)
  
  tmp.dat = subset(data, memSpan == list_length) 
  
  tmp.accs = matrix(NA, nrow = nrow(tmp.dat), ncol=list_length)
  
  for (i in 1:nrow(tmp.dat)){
    p = strsplit(tmp.dat$memItems[i], split='-')[[1]]
    r = strsplit(tmp.dat$memResps[i], split='-')[[1]]
    
    vec = rep(99, list_length)
    for (pos in 1:list_length){
      # for each recalled item
      if (r[pos] %in% p){
        vec[pos] = pos - which(p == r[pos]) # recalled position minus presented position
      }
      if (r[pos] == '0'){
        vec[pos] = NA
      }
    }
    #print(vec)
    tmp.accs[i,] <- vec
  }
  
  colnames(tmp.accs) <- paste0('pos.', 1:list_length)
  
  tmp.dat <- cbind(tmp.dat, tmp.accs)
  
  tmp.dat = gather(data = tmp.dat, pos, Acc,  grep(pattern = 'pos.', x = colnames(tmp.dat))) %>% 
    arrange(ID, condition, trial)
  
  tmp.dat$respType = 'correct'
  
  tmp.dat$respType[!(tmp.dat$Acc %in% c(0,99, NA))] = 'transposition'
  
  tmp.dat$respType[tmp.dat$Acc == 99] = 'extra-list'
  
  tmp.dat$respType[is.na(tmp.dat$Acc)] = 'pass'
  
  return(tmp.dat)
}

plotTranspositions <- function(data){
  # plot transposition gradiants
  #par(mfrow=c(2,1), mar=c(.8, .4, 0, 0), oma = c(3,3.5,2,2))
  
  shapes = 16:21
  colors = brewer.pal(6, "Dark2")
  
  tmp.table = with(data, table(Acc, condition, pres_recall))
  
  tmp.table <- tmp.table/apply(tmp.table, 2, sum)
  
  #tmp.table[,,'AO'] <- tmp.table[,,'AO']/apply(tmp.table[,,'AO'], 2, sum)
  #tmp.table[,,'VT'] <- tmp.table[,,'VT']/apply(tmp.table[,,'VT'], 2, sum)
  
  tmp.data = as.data.frame(tmp.table)
  
  tmp.data$Acc <- as.numeric(as.character(tmp.data$Acc))
  
  tmp.data <- tmp.data[!(tmp.data$Acc %in% c(0,99)),]
  
  plot(NA, xlim=range(tmp.data$Acc), ylim = c(0, max(tmp.data$Freq)), ylab='', xlab='', axes=F)
  box()
  axis(2)
  
  for (l in 1:length(levels(tmp.data$condition))){
    cond = levels(tmp.data$condition)[l]
    with(subset(tmp.data, condition == cond & Acc>0), points(Acc, Freq, type='b', pch=shapes[l], col=colors[l]))
    with(subset(tmp.data, condition == cond & Acc<0), points(Acc, Freq, type='b', pch=shapes[l], col=colors[l]))
    #mtext(format, 4, line = .2)
  }

  axis(1)
  mtext('Proportion of Responses', 2, line = 2.5)
  mtext('Distance', 1, line = 2.5)
}

plotRespTypePanel = function(data, xaxis=F){
  # plot a single panel (called from higher function)
  # plots proportion category of response for each serial position
  tmp.data = with(data, table(respType, pos))
  
  tmp.data = tmp.data/apply(tmp.data, 2, sum) # correct, extra list, pass, transpositions
  
  barplot(tmp.data, names.arg = 1:ncol(tmp.data), col = c('lightgreen', 'pink', 'lightblue', 'yellow'), beside =T, ylim=c(0,1), axes=F, axisnames = xaxis)
  box()
}

plotRespType <- function(data){
  # plots 2 x 6 matrix of response categories across condition
  # calls plotRespTypePanel
  par(mfcol=c(6,2), mar=c(.4, .4, 0, 0), oma = c(4,4,2,2))
  
  groups=c("Younger", "Older")

  for (ag in 1:2){
    for (co in levels(data$condition)){
      
      xlab = co == rev(levels(data$condition))[1]
      
      plotRespTypePanel(subset(data, ageGroup==groups[ag] & condition == co), xaxis = xlab)
      
      if (ag==2){
        mtext(text = co, side = 4, line = .5)
      } else{
        axis(2)
      }
      
      if (co== levels(data$condition)[1]){
        mtext(groups[ag], 3)
      }
    }
  }

  
  mtext('Proportion of Responses', 2, line = 2, outer = T)
  mtext('Position', 1, line = 2.3, outer = T)
}

backwardsDiffMat <- function(k, names = NULL){
  mat = matrix(data = NA, nrow = k, ncol = k-1)
  for (r in 1:(k-1)){
    v1 = -(k-(1:(k-1)))/k
    v2 = 1:(k-1)/k
    mat[r,] = c(v2[0:(r-1)], v1[r:(k-1)])
  }
  mat[k,] = 1:(k-1)/k
  
  if (is.null(names)){
    colnames(mat) <- paste0('c', 1:(k-1))
  } else {
    colnames(mat) <- names
  }
  return(mat)
}

isOrthogonal <- function(x, detail=F){
  t1 = all(round(apply(X = x, 1, sum), 2) == 0)
  
  CPsum = c()
  pairs = t(combn(x = 1:nrow(x), 2))
  for (i in 1:nrow(pairs)){
    CPsum = c(CPsum, sum(x[pairs[i,1],]*x[pairs[i,2],]))
  }
  
  t2 = round(CPsum, 2) == 0
  
  if (detail){
    return(list(t1, cbind(pairs, is_orth = t2)))
  } else{
    if (all(t1, t2)){
      return(TRUE)
    } else {
      return(FALSE)
    }
  }
}
