# simple auxiliary function to compute % of females per vector vec
genderShare<-function(vec){ signif(100*prop.table(table(vec$gender))['woman'],2)}

# function to calculate simple data.frame objects with median and mean pay gap, from hourly wages
gapEstimator<-function(tab)# specific to DLL data
{
  # mean wages man and woman, keep in separate tables for future needs (might need to report on hourly wage?)
  femaleMean<-aggregate(wage~gender,data=subset(tab,gender=='woman'),FUN=mean)
  maleMean<-aggregate(wage~gender,data=subset(tab,gender=='man'),FUN=mean)
  tab1<-rbind.data.frame(maleMean,femaleMean)
  meangap<-100*(subset(tab1,gender=='man')$wage-subset(tab1,gender=='woman')$wage)/subset(tab1,gender=='man')$wage

 # median wages man and woman, keep in separate tables for future needs
  femaleMedian<-aggregate(wage~gender,data=subset(tab,gender=='woman'),FUN=median)
  maleMedian<-aggregate(wage~gender,data=subset(tab,gender=='man'),FUN=median)
  tab2<-rbind.data.frame(maleMedian,femaleMedian)
  mediangap<-100*(subset(tab2,gender=='man')$wage-subset(tab2,gender=='woman')$wage)/subset(tab2,gender=='man')$wage
  
  return(list('mean'=meangap,'median'=mediangap))
  
}

# generate table with median/average pay gap, per group, along with % women in each group
unadjustedTab<-function(df,namedf){
  # create table with results
  share<-genderShare(df)
  if(share==100){#if only women in this role
    metrics<-cbind.data.frame(0,0)
    colnames(metrics)<-c('Average gap %','Median gap %')
    size<-nrow(df)
    return( res<-cbind.data.frame('Population'=namedf, 'Size'=format(size,big.mark = ','), metrics,'%women'=as.character(share))
    )}
  
  else{
  metrics<-rbind.data.frame(unlist(gapEstimator(df)))
  colnames(metrics)<-c('Average gap %','Median gap %')
  size<-nrow(df)
  res<-cbind.data.frame('Population'=namedf, 'Size'=format(size,big.mark = ','), metrics,'%women'=as.character(share))
  # row.names(res)<-NULL
  return(res)
}}

# formatting of linear regression objects for reporting 
adjusted<-function(lmobject){
  B0<-lmobject$coefficients['(Intercept)']
  B1<-lmobject$coefficients['genderman']#factor genderman/genderwoman
  adjustedPG=100*signif(B1/(B0+B1),3)
  a<-summary(lmobject)
  R2<-signif(100*a$r.squared,2)
  pval<-a$coefficients[,4]['genderman']
  pval<-ifelse(pval<0.05,'yes','no')
  # return(cbind.data.frame('adjustedPG'=adjustedPG,'significant'=pval,'R2'=R2))
  return(cbind.data.frame('Adjusted'=adjustedPG,'R2'=as.character(R2)))
}


# function to create summary of the data that can be used to plot overview of man/woman particpation per group
popOverview<-function(df,namedf){
  # create table with results
  share<-genderShare(df)
  size<-nrow(df)
  regime<-(signif(100*prop.table(table(df$regime))['part-time'],2))
  if(is.na(regime)) regime<-'0'
  else regime<-as.character(regime)
  res<-cbind.data.frame('Population'=namedf, 'Size'=format(size,big.mark = ','),'% PT'=regime, '% Women'=as.character(share))
  # row.names(res)<-NULL
  return(res)
}

# returns a ggplot2 plot of the paybands
plotbands<-function(tab, epsilon,nametab){
    "
    function to plot pay-bands, based on shape and ggplot2
    ----------
    tab: dataframe with the population data
    epsilon: tolerance threshold for the found bands, this is useful when the vast majority of associates make the same salary,
    it is then not possible to find the percentiles

    Return
    ------
    plot of the pay-bands
    "
  # require reshape and ggplot2 if not insattked print, you need to insatll ggplot2 and reshape to use this function 
  if(!require(ggplot2)) print('install ggplot2 package')
  if(!require(reshape)) print('install reshape package')
  #pay bands
  qtab<-quantile(tab$wage,prob=c(0.25,0.50,0.75,1),type = 1)
  band1<-subset(tab,wage<= qtab[1])
  band2<-subset(tab,wage>  qtab[1] & wage<=qtab[2])
  band3<-subset(tab,wage>  qtab[2] & wage<=qtab[3])
  band4<-subset(tab,wage>  qtab[3] & wage<=qtab[4])

  vec<-c(nrow(band1)/nrow(tab),nrow(band2)/nrow(tab),nrow(band3)/nrow(tab),nrow(band4)/nrow(tab))
  if (any(vec)>25+epsilon){
	return('There are too many ties in the income distributions, pay-bands cannot be computed')}
  else{
  # create a data.frame with % of females in each band
  bandsTab0<-cbind.data.frame(
    c('Lower quartile',
      'Lower middle quartile',
      'Upper middle quartile',
      'Upper quartile'),
    rbind(round_preserve_sum(100*prop.table(table(band1$gender))),
          round_preserve_sum(100*prop.table(table(band2$gender))),
          round_preserve_sum(100*prop.table(table(band3$gender))),
          round_preserve_sum(100*prop.table(table(band4$gender)))))
  colnames(bandsTab0)[1]<-c('Quartile')
  bandsTab<-melt(bandsTab0)
  
  fill <-  c("#69b3a2", "#404080")

  # reorder for plotting with ggplot2
  bandsTab$Quartile<-factor(bandsTab$Quartile, levels =  c('Lower quartile',
                                                           'Lower middle quartile',
                                                           'Upper middle quartile',
                                                           'Upper quartile'))
  
  
  # save plot for rpeort  
  # pdf(filepath, height = 5, width = 8.5)
  paybands<-ggplot() +   geom_bar(aes(y = value, x = Quartile,fill=variable),data = bandsTab,stat="identity")+
    labs(y='',x='')+ coord_flip()+
    theme(axis.text.x = element_text(angle = 0, hjust = 1), plot.margin = margin(0.5,0.5,0,0.5, "cm"))+
    ggtitle(paste('Pay quartiles per gender for group', nametab,sep=' '))+
    scale_fill_manual(values=fill)+
    theme(legend.title=element_blank(), axis.text=element_text(size=14),axis.title=element_text(size=12))+
    geom_text(data=bandsTab, aes(x = Quartile,fill=variable ,y = value+0.2,label = paste(signif(value,2),'%')),
              color='black', size=4,
              position = position_stack(0.5), size=5)+
              
    NULL
  # dev.off()
  return(paybands)
  }

}


# function to preserve the sum to 100% when computing proportions
round_preserve_sum <- function(x, digits = 0) {
  up = 10 ^ digits
  x = x * up
  y = floor(x)
  indices = tail(order(x-y), round(sum(x)) - sum(y))
  y[indices] = y[indices] + 1
  y / up
}






