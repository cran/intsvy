intsvy.log.pv <- 
  function(pvnames, x, cutoff, by, data, export=FALSE, name= "output", folder=getwd(), config) {

  log.pv.input <- function(pvnames, x, cutoff, data, config) {
    if (any(sapply(data[x], function(i) all(duplicated(i))))) {
      return(data.frame("Coef."=NA, "Std. Error"=NA, "t value"=NA, "OR"=NA, "CI95low"=NA, 
                        "CI95up"=NA, check.names=F))
    }
    
    #  JK with weight variables
    if (config$parameters$weights == "JK with weights") {
    
      #pvnames <- paste0("^", config$variables$pvlabelpref, "*[0-9].*", pvnames)
      #pvnames <- grep(pvnames, names(data), value = TRUE)
      weights <- grep(paste0("^", config$variables$weightJK , ".*[0-9]+$"), 
                      names(data), value = TRUE)
      
      # remove missings in pvalues and weights
      data <- data[complete.cases(data[, c(pvnames[1], weights[1], config$variables$weight)]), ]    
      
      
      # data is empty
      if (sum(is.na((data[[pvnames[1]]])))==length(data[[pvnames[1]]])) {
        result <- data.frame(NA, "Freq"=0, "Percentage"=NA, "Std.err."= NA)  
        names(result)[1] <- pvnames[1] 
        return(result)
      }
     
      # Dependent binary variable
      di <- as.data.frame(sapply(pvnames, function(pv) ifelse(data[[pv]] > cutoff, 1, 0)))
      names(di) <- paste("DI", 1:length(pvnames), sep="")
      data <- cbind(data, di)
      
      # List of formulas for each PV
      regform <- lapply(names(di), function(i) paste(i, "~", paste(x, collapse="+")))
      
      # Replicate weighted coefficients for sampling error (PV1 only), normalised weights
      
      coef.rp1 <- suppressWarnings(lapply(1:length(pvnames), function(m)
        lapply(1:length(weights), function(rp)  summary(glm(formula=as.formula(regform[[m]]), 
        family=quasibinomial("logit"), weights=data[[weights[rp]]], data=data)))))
      
      # Retrieving coefficients
      rp.coef <- lapply(1:length(pvnames), function(m) sapply(1:length(weights), 
                 function(rp) coef.rp1[[m]][[rp]]$coefficients[,1]))
      
      # Total weighted coefficient for each PV for imputation (between) error
      reg.pv <- suppressWarnings(lapply(regform, function(pv) 
        summary(glm(formula=as.formula(pv), family=quasibinomial("logit"), 
                    weights=nrow(data)*data[[config$variables$weight]]/sum(data[[config$variables$weight]]), data=data))))
      
      pv.coef <- sapply(1:length(pvnames), function(pv) reg.pv[[pv]]$coefficients[, 1])
      
      # Mean total coefficients (across PVs)
      mean.coef <- apply(pv.coef, 1, mean)
      
      # Sampling error (variance within)
      var.w <- apply(sapply(1:length(pvnames), function(m) 
        apply((rp.coef[[m]] - pv.coef[,1])^2, 1, sum)), 1, mean)
      
      
      # Imputation error (variance between)
      var.b <- (1+1/length(pvnames))*apply(pv.coef, 1, var, na.rm=TRUE)
      
      coef.se <- (var.w+(1+1/length(pvnames))*var.b)^(1/2)
      t.stat <- mean.coef/coef.se
      
      # Odds ratios and confidence intervals
      OR<- exp(mean.coef)
      
      # OR confidence intervals 
      CI95low <- exp(mean.coef - 1.96*coef.se)
      CI95up <- exp(mean.coef + 1.96*coef.se)
      
      # Table with estimates
      log.tab <- round(data.frame("Coef."=mean.coef, "Std. Error"=coef.se, "t value"=t.stat, 
                                  as.data.frame(cbind(OR, CI95low, CI95up)), check.names=F),2)
      
      return(log.tab)          
      
    }
    
    # BRR / JK
    if (config$parameters$weights == "BRR") {
      # balanced repeated replication
      # Replicate weighted %s (sampling error)
      # in PISA
      
      #pvnames <- paste0(pvnames, ".*[0-9]|[0-9].*", pvnames)
      #pvnames <- grep(pvnames, names(data), value = TRUE)
      weights <- grep(paste0("^", config$variables$weightBRR , ".*[0-9]+$"), 
                      names(data), value = TRUE)
      
      # remove missings in pvalues and weights
      data <- data[complete.cases(data[, c(pvnames[1], weights[1], config$variables$weightFinal)]), ]    
      
      # Dependent binary variable
      di <- as.data.frame(sapply(pvnames, function(pv) ifelse(data[[pv]] > cutoff, 1, 0)))
      names(di) <- paste("DI", 1:length(pvnames), sep="")
      data <- cbind(data, di)
      
      # List of formulas for each PV
      regform <- lapply(names(di), function(i) paste(i, "~", paste(x, collapse="+")))
      # Replicate weighted coefficients for sampling error (PVs), normalised weights
      coef.rp <- suppressWarnings(lapply(regform, function(k) lapply(1:config$parameters$BRRreps, function(i) 
        summary(glm(formula=as.formula(k), family=quasibinomial("logit"), 
                    weights=nrow(data)*data[[weights[i]]]/sum(data[[weights[i]]]),
                    data=data)))))

      # Retrieving coefficients
      rp.coef <- lapply(1:length(pvnames), function(pv) 
                  sapply(1:config$parameters$BRRreps, function(i) 
                      coef.rp[[pv]][[i]]$coefficients[,1]))
      # Total weighted coefficient for each PV for imputation (between) error
      reg.pv <- suppressWarnings(lapply(regform, function(i) 
        summary(glm(formula=as.formula(i), family=quasibinomial("logit"), 
                    weights=nrow(data)*data[[config$variables$weightFinal]]/sum(data[[config$variables$weightFinal]]), data=data))))

      pv.coef <- sapply(1:length(pvnames), function(pv) reg.pv[[pv]]$coefficients[, 1])
      # Mean total coefficients (across PVs)
      mean.coef <- apply(pv.coef, 1, mean)
      
      # Sampling error (variance within)
      cc = 1/(length(weights)*(1-0.5)^2)
      
      var.w <- apply(cc*sapply(lapply(1:length(pvnames), 
                      function(pv) (rp.coef[[pv]]-pv.coef[,pv])^2), 
                                 function(e) apply(e, 1, sum)), 1, mean)
      
      # Imputation error (variance between)
      var.b <- (1/(length(pvnames)- 1))*apply(sapply(1:length(pvnames), function(pv) 
        (pv.coef[, pv] - mean.coef)^2), 1, sum)
      
      coef.se <- (var.w+(1+1/length(pvnames))*var.b)^(1/2)
      t.stat <- mean.coef/coef.se
      
      # Odds ratios and confidence intervals
      OR<- exp(mean.coef)
      
      # OR confidence intervals 
      CI95low <- exp(mean.coef - 1.96*coef.se)
      CI95up <- exp(mean.coef + 1.96*coef.se)
      
      # Table with estimates
      log.tab <- round(data.frame("Coef."=mean.coef, "Std. Error"=coef.se, "t value"=t.stat, 
                                  as.data.frame(cbind(OR, CI95low, CI95up)), check.names=FALSE), 2)
      return(log.tab)
      
    } 
    if (config$parameters$weights == "JK") {
      # jack knife
      # in PIRLS / TIMSS
      
      #pvnames <- grep(pvnames, names(data), value = TRUE)
      
      # Dependent binary variable
      di <- as.data.frame(sapply(pvnames, function(pv) ifelse(data[[pv]] > cutoff, 1, 0)))
      names(di) <- paste("DI", 1:length(pvnames), sep="")
      data <- cbind(data, di)
      
      # List of formulas for each PV
      regform <- lapply(names(di), function(i) paste(i, "~", paste(x, collapse="+")))
      
      # Replicate weights
      rp.wt <- sapply(1:max(data[[config$variables$jackknifeZone]]), function(rp) 
                    ifelse(data[[config$variables$jackknifeZone]] == rp, 
                               2*data[[config$variables$weight]]*data[[config$variables$jackknifeRep]], data[[config$variables$weight]]))
      
      if (isTRUE(config$parameters$varpv1)) {
      
      rp.wt.n <- nrow(data)*rp.wt/apply(rp.wt, 2, sum)
      
      # Replicate weighted coefficients for sampling error (PV1 only), normalised weights
      coef.rp1 <- suppressWarnings(lapply(1:max(data[[config$variables$jackknifeZone]]), function(rp) 
                      summary(glm(formula=as.formula(regform[[1]]), 
                           family=quasibinomial("logit"), weights=rp.wt.n[, rp], data=data))))

      # Retrieving coefficients
      rp.coef <- do.call(cbind, lapply(1:max(data[[config$variables$jackknifeZone]]), function(rp) 
                    coef.rp1[[rp]]$coefficients[,1]))
      
      # Total weighted coefficient for each PV for imputation (between) error
      reg.pv <- suppressWarnings(lapply(regform, function(pv) 
        summary(glm(formula=as.formula(pv), family=quasibinomial("logit"), 
        weights=nrow(data)*data[[config$variables$weight]]/sum(data[[config$variables$weight]]), data=data))))
      pv.coef <- sapply(1:length(pvnames), function(pv) reg.pv[[pv]]$coefficients[, 1])
      
      # Mean total coefficients (across PVs)
      mean.coef <- apply(pv.coef, 1, mean)

      # Sampling error (variance within)
      var.w <- apply((rp.coef - pv.coef[,1])^2, 1, sum)/2
      
      # Imputation error (variance between)
      var.b <- (1+1/length(pvnames))*apply(pv.coef, 1, var, na.rm=TRUE)
      
      coef.se <- (var.w+(1+1/length(pvnames))*var.b)^(1/2)
      t.stat <- mean.coef/coef.se
      
      # Odds ratios and confidence intervals
      OR<- exp(mean.coef)
      
      # OR confidence intervals 
      CI95low <- exp(mean.coef - 1.96*coef.se)
      CI95up <- exp(mean.coef + 1.96*coef.se)
      
      # Table with estimates
      log.tab <- round(data.frame("Coef."=mean.coef, "Std. Error"=coef.se, "t value"=t.stat, 
                                  as.data.frame(cbind(OR, CI95low, CI95up)), check.names=F),2)
      
      return(log.tab)

      } else {
      
        rp.wt2 <- sapply(1:max(data[[config$variables$jackknifeZone]]), function(x) 
          ifelse(data[[config$variables$jackknifeZone]] == x, 
                 2*data[[config$variables$weight]]*ifelse(data[[config$variables$jackknifeRep]]==1,0,1), data[[config$variables$weight]]))
        
        rp.wt <- cbind(rp.wt2, rp.wt)
        

        rp.wt.n <- nrow(data)*rp.wt/apply(rp.wt, 2, sum)
        
        # Replicate weighted coefficients for sampling error (PV1 only), normalised weights
        
        coef.rp1 <- suppressWarnings(lapply(1:length(pvnames), function(m)
          lapply(1:ncol(rp.wt), function(rp)  summary(glm(formula=as.formula(regform[[m]]), 
                      family=quasibinomial("logit"), weights=rp.wt.n[, rp], data=data)))))
        
        # Retrieving coefficients
        rp.coef <- lapply(1:length(pvnames), function(m) sapply(1:ncol(rp.wt), 
        function(rp) coef.rp1[[m]][[rp]]$coefficients[,1]))
        
        # Total weighted coefficient for each PV for imputation (between) error
        reg.pv <- suppressWarnings(lapply(regform, function(pv) 
          summary(glm(formula=as.formula(pv), family=quasibinomial("logit"), 
                      weights=nrow(data)*data[[config$variables$weight]]/sum(data[[config$variables$weight]]), data=data))))
      
        pv.coef <- sapply(1:length(pvnames), function(pv) reg.pv[[pv]]$coefficients[, 1])
        
        # Mean total coefficients (across PVs)
        mean.coef <- apply(pv.coef, 1, mean)
        
        # Sampling error (variance within)
        var.w <- apply(sapply(1:length(pvnames), function(m) 
        apply((rp.coef[[m]] - pv.coef[,1])^2, 1, sum)), 1, mean)
        
        
        # Imputation error (variance between)
        var.b <- (1+1/length(pvnames))*apply(pv.coef, 1, var, na.rm=TRUE)
        
        coef.se <- (var.w+(1+1/length(pvnames))*var.b)^(1/2)
        t.stat <- mean.coef/coef.se
        
        # Odds ratios and confidence intervals
        OR<- exp(mean.coef)
        
        # OR confidence intervals 
        CI95low <- exp(mean.coef - 1.96*coef.se)
        CI95up <- exp(mean.coef + 1.96*coef.se)
        
        # Table with estimates
        log.tab <- round(data.frame("Coef."=mean.coef, "Std. Error"=coef.se, "t value"=t.stat, 
                                    as.data.frame(cbind(OR, CI95low, CI95up)), check.names=F),2)
        
        return(log.tab)        
      }
    }
    if (config$parameters$weights == "mixed_piaac") {
      # mixed design, different for different coutnries
      # PIAAC
      
      stop("Not implemented yet")
    } 
  }
  
  # If by no supplied, calculate for the complete sample    
  if (missing(by)) { 
    output <- log.pv.input(pvnames=pvnames, x=x, cutoff=cutoff, data=data, config=config) 
  } else {
    output <- lapply(split(data, droplevels(data[by])), function(i) 
      log.pv.input(pvnames=pvnames, cutoff=cutoff, x=x, data=i, config=config))
  }

  if (export)  {
    write.csv(output, file=file.path(folder, paste(name, ".csv", sep="")))
  }
  return(output)
}

