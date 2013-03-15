ResidualOutlierTrim <- function(fitted.model, z.criterion=2, plot.it=FALSE){
  # Removes obs. that stress the fitted model, based on Baayen & Milin, 2010
  # ARGS: . fitted.model . must include data in @frame component (lmer default)
  #       . z.criterion . exclude standardized residuals outside this (abs) z-score
  #       . plot.it . show a quantile-quantile plot of new and old residuals?
  # RETURNS: . new.model . the model refitted with the trimmed data
  
  # N.B.: The dependent variable must have been in the original data data.frame
  # that the model was fitted against -- do not include transformations in the 
  # left-hand side of the formula call, but create a new column
  
  # extract the data from the model
  if(isS4(fitted.model)){
    fitted.model@frame -> current.data
  }else{ fitted.model$data -> current.data }
  
  # scale residuals, ID outliers by z.criterion
  resid(fitted.model) -> fitted.resids
  scale(fitted.resids) -> norm.fitted.resids
  which(abs(norm.fitted.resids) < z.criterion) -> keep.resids
  
  # exclude outliers and update model
  current.data[keep.resids, ] -> new.data
  update(fitted.model, data = new.data) -> new.model
  
  # how many observations were excluded?
  pct.trimmed <- round(100*(1 - (nrow(new.data) / nrow(current.data))), 2)
  print(paste(pct.trimmed, 
              "% obs. trimmed",sep=""))
  
  # 2-up qqnorm plots of residuals
  if(plot.it){
    
    # 2-up
    par(mfrow=c(1,2))
    
    # tests of normality
    test.0 <- shapiro.test(resid(fitted.model))
    test.1 <- shapiro.test(resid(new.model))
    
    # concat subtitle text
    plot.text <- function(test){
       paste("Theoretical Quantiles",
            "\n (Shap.-Wilk test W: ",round(test$statistic,3), 
            ", p: ", round(test$p.value,3),")", sep="")
    }
    
    # qqplot of original model
    qqnorm(resid(fitted.model), main="Original model",xlab=plot.text(test.0))
    qqline(resid(fitted.model))
    abline(v=c(-1,1)*z.criterion, 
           col="darkgrey", lty="dashed", lwd=2)
    title.text <- paste("Re-fitted model\n",pct.trimmed,"% trim")

    # qqplot of new model
    qqnorm(resid(new.model), main=title.text, xlab=plot.text(test.1))
    qqline(resid(new.model))   

    
    }
  
  return(new.model)
  
}