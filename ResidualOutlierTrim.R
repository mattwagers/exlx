ResidualOutlierTrim <- function(fitted.model, z.criterion=2, plot.it=FALSE){
  # Based on Baayen & Milin 2010 - removes obs. that stress the fitted model
  # ARGS: fitted.model, must include data in @frame component (lmer default)
  #       z.criterion, exclude standardized residuals outside this (abs) z-score
  #       plot.it, show a quantile-quantile plot of new and old residuals?
  # RETURNS: the model refitted with the trimmed data
  
  # N.B.: The dependent variable must have been in the original data data.frame
  # that the model was fitted against -- do not include transformations in the 
  # left-hand side of the formula call, but create a new column
  
  fitted.model@frame -> current.data
  
  resid(fitted.model) -> fitted.resids
  scale(fitted.resids) -> norm.fitted.resids
  which(abs(norm.fitted.resids) < z.criterion) -> keep.resids
  
  current.data[keep.resids, ] -> new.data
  
  update(fitted.model, data = new.data) -> new.model
  pct.trimmed <-round(100*(1 - nrow(new.data)/nrow(current.data)),2)
  print(paste(pct.trimmed, 
              "% obs. trimmed",sep=""))
  
  if(plot.it){
    
    dev.off()
    par(mfrow=c(1,2))
    
    test.0 <- shapiro.test(resid(fitted.model))
    test.1 <- shapiro.test(resid(new.model))
    
    plot.text <- paste("Theoretical Quantiles",
                       "\n (Shap.-Wilk test W: ",round(test.0$statistic,3), 
                       ", p: ", round(test.0$p.value,3),")", sep="")
    qqnorm(resid(fitted.model), main="Original model",xlab=plot.text)
    qqline(resid(fitted.model))
    abline(h=c(-1,1)*z.criterion, 
           col="darkgrey", lty="dashed", lwd=2)
   
    title.text <- paste("Re-fitted model\n",pct.trimmed,"% trim")
    plot.text <- paste("Theoretical Quantiles",
                       "\n (Shap.-Wilk test W: ",round(test.1$statistic,3), 
                       ", p: ", round(test.1$p.value,3),")", sep="")
    par=c(5,2,2,2)
    qqnorm(resid(new.model), main=title.text, xlab=plot.text)
    qqline(resid(new.model))   

    
    }
  
  return(new.model)
  
}