

ancova=function(Xt,SM,factor_name,fdr,alpha) {

  fn = function(x,SM,factor_name,formula,alpha) {
    # prep data.frame
    df=data.frame(x=x,SM[,factor_name],check.names=FALSE)
    df[[1]]=as.factor(df[[1]]) # assume first factor is a a factor, next factor is continuous confounder for ancova

    # linear model
    mod=lm(formula,data=df)
    # anova
    ava=aov(mod,data=df)
    # anova summary
    s=summary(ava)[[1]]
    # group labels
    n=trimws(row.names(s))
    # append p-value string
    np=interaction(n,'p-value')
    # append sig string
    ns=interaction(n,'sig')
    # ge the pvals
    pvals=data.frame(s$`Pr(>F)`)
    # set the labels
    rownames(pvals)=np
    # see if significant
    sig=pvals<alpha
    # set the labels
    rownames(sig)=ns
    # merge the tables
    out=rbind(pvals,sig)
    # tukey pairwise comparisons
    tuk=TukeyHSD(ava)
    # number of terms
    K = length(tuk)
    # term names
    tn = names(tuk)
    # for each term, get the pvalues
    for (k in 1:K) {
      # get p-values
      temp=data.frame(tuk[[k]][,4])
      tempn=rownames(temp)
      colnames(temp)=colnames(out)
      rownames(temp)=interaction(tn[k],tempn,'p-value')
      out=rbind(out,temp)
      temp=temp<alpha
      rownames(temp)=interaction(tn[k],tempn,'sig')
      out=rbind(out,temp)
    }
    return(out)
  }


  if (length(factor_name)!=2) {
    stop('must have two factor names for ANCOVA')
  }

  factor_name=make.names(factor_name)
  colnames(SM)=make.names(colnames(SM))

  formula=as.formula(paste0("x~",paste(factor_name, collapse="+")))

  out=apply(Xt,MARGIN=2,FUN=fn,SM,factor_name,formula,alpha)
  return(out)
}
