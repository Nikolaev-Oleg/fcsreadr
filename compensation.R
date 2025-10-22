#| let x be a true signal in the channel X 
#| and y be a true signal in channel Y
#| x' is a measured signal in X given y in Y
#| y' is a measured signal in Y given x in X
#| 
#| then for no-stain controls:
#| x = x'
#| y = y'
#| for single-stain ctrl in X:
#| x = x'
#| (1) y' = y + ax = y + ax'
#| for single-stain ctrl in Y:
#| y = y'
#| (2) x' = x + by = x + by'
#| 
#| The aim is to estimate the spillover coefficients a and b
#| 
#| WAY #1....................................................................... 
#| IF NO no-stain CTRL USED, y and a may be estimated as a constant using least-squares
#| Equivalently, x and b may be estimated with least-squares from single-stain ctrl in Y
#| 
#| WAY #2 (Not recommended).....................................................
#| Similar to WAY #1, but here we assume true x and y to be exactly 0 in 
#| no-stain ctrls. Thus, equations (1) and (2) are simplified to:
#| 
#| (3) y' = ax = ax'
#| (4) x' = by = by'
#| 
#| WAY #3 (probably the best if reliable no-stain ctrls are present)............
#| To obtain an estimate for a, 
#| 1. take no-stain ctrl,
#| 2. use least-squars to estimate coefficients c, d and e for
#| 
#| (5) y = y' = c + d*FSC_A + e*SSC_A
#| 
#| 3. for single-stain ctrl in X, predict y from (5). From (1) we than have:
#| 
#| y'-y = y'' = ax'
#| 
#| 4. Estimate a with least squares (!zero intercept!)
#| 
#| Equivalently, estimate b.
#| ............................................................................
#| Repeat operations for all pairs of X and Y


spillover.constant<-function(data, channels, singlestains){
  spillover <- matrix(ncol = length(channels),
                      nrow = length(channels))
  for(i in 1:length(channels)){
    spillover[i,i] <- 1
  }
  colnames(spillover) <- channels
  rownames(spillover) <- channels
  
  names(singlestains)<-channels
  
  for(i in channels){
    for(j in channels){
      if(i != j){
        # Estimate a
        x. <- singlestains[[i]][[i]]
        y. <- singlestains[[i]][[j]]
        a<-lm(y. ~ x.)$coefficients['x.']
        
        # Estimate b
        x. <- singlestains[[j]][[i]]
        y. <- singlestains[[j]][[j]]
        b<-lm(x. ~ y.)$coefficients['y.']
        
        spillover[i, j] <- a
        spillover[j, i] <- b
      }
    }
  }
  return(spillover)
}
spillover.zero<-function(data, channels, singlestains){
  spillover <- matrix(ncol = length(channels),
                      nrow = length(channels))
  for(i in 1:length(channels)){
    spillover[i,i] <- 1
  }
  colnames(spillover) <- channels
  rownames(spillover) <- channels
  
  names(singlestains)<-channels
  
  for(i in channels){
    for(j in channels){
      if(i != j){
        # Estimate a
        x. <- singlestains[[i]][[i]]
        y. <- singlestains[[i]][[j]]
        a<-lm(y. ~ 0 + x.)$coefficients['x.']
        
        # Estimate b
        x. <- singlestains[[j]][[i]]
        y. <- singlestains[[j]][[j]]
        b<-lm(x. ~ 0 + y.)$coefficients['y.']
        
        spillover[i, j] <- a
        spillover[j, i] <- b
      }
    }
  }
  return(spillover)
}
spillover.nostain<-function(data, channels, singlestains, nostain){
  spillover <- matrix(ncol = length(channels),
                      nrow = length(channels))
  for(i in 1:length(channels)){
    spillover[i,i] <- 1
  }
  colnames(spillover) <- channels
  rownames(spillover) <- channels
  
  names(singlestains)<-channels
  
  for(i in channels){
    for(j in channels){
      if(i != j){
        # Estimate a
        y <- nostain[[j]]
        c <- lm(y ~ nostain$FSC_A + nostain$SSC_A)$coefficients['(Intercept)']
        d <- lm(y ~ nostain$FSC_A + nostain$SSC_A)$coefficients['nostain$FSC_A']
        e <- lm(y ~ nostain$FSC_A + nostain$SSC_A)$coefficients['nostain$SSC_A']
        
        y <- c + d*singlestains[[i]]$FSC_A + e*singlestains[[i]]$SSC_A
        x. <- singlestains[[i]][[i]]
        y. <- singlestains[[i]][[j]]
        y.. <- y. - y
        a<-lm(y.. ~ 0 + x.)$coefficients['x.']
        
        # Estimate b
        x <- nostain[[i]]
        c <- lm(x ~ nostain$FSC_A + nostain$SSC_A)$coefficients['(Intercept)']
        d <- lm(x ~ nostain$FSC_A + nostain$SSC_A)$coefficients['nostain$FSC_A']
        e <- lm(x ~ nostain$FSC_A + nostain$SSC_A)$coefficients['nostain$SSC_A']
        
        x <- c + d*singlestains[[j]]$FSC_A + e*singlestains[[j]]$SSC_A
        x. <- singlestains[[j]][[i]]
        y. <- singlestains[[j]][[j]]
        x.. <- x. - x
        b<-lm(x.. ~ 0 + y.)$coefficients['y.']
        
        spillover[i, j] <- a
        spillover[j, i] <- b
      }
    }
  }
  
  return(spillover)
}

comp_matrix<-function(data, channels, singlestains, nostain = NULL, method = 'constant'){
  if(method != 'constant' & method != 'zero' & method != 'nostain')stop('Unknown method')
  if(method == 'constant'){
    if(!is.null(nostain))warning("method = 'constant', no-stain control will not be used")
    spillover <- spillover.constant(data = data, 
                                    channels = channels, 
                                    singlestains = singlestains)
  }
  if(method == 'zero'){
    if(!is.null(nostain))warning("method = 'zero', no-stain control will not be used")
    spillover <- spillover.zero(data = data, 
                                channels = channels, 
                                singlestains = singlestains)
  }
  if(method == 'nostain'){
    if(is.null(nostain)){
      stop("method = 'nostain', no-stain control required")
    }else{
      spillover<-spillover.nostain(data = data, 
                                  channels = channels, 
                                  singlestains = singlestains,
                                  nostain = nostain)
    }
  }
  
  comp_matrix<-solve(spillover)
}

compensate<-function(data, compensation_matrix, channels = NULL){
  if(!identical(rownames(compensation_matrix), colnames(compensation_matrix))){
    stop('Col. names and row. names of the compensation matrix are not equal')
  }
  out<-data
  if(is.null(channels))channels<-rownames(compensation_matrix)
  for(j in channels){
    comp<-c()
    for(i in channels){
      comp<-cbind(comp, compensation_matrix[i,j]*data[[i]])
    }
    comp<-rowSums(comp)
    out[[j]]<-comp
  }
  return(out)
}

decompensate<-function(data, metadata){
  out<-compensate(data, metadata$spillover)
}



