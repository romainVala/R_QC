
plot_table = function(param,colind,fn,isel=NULL)
{
  require(gplots)
  
  if (is.character(colind)) {
    for (k in 1:length(colind)) colind[k] = grep(colind[k],colnames(param))
    colind =  as.numeric(colind)
  } 
  
  pcol = colnames(param[,colind])
  
  if (!is.null(isel)){
    paramorig = param
    param = param[isel,]
    print(sprintf('selecting %d rows (%f%%)',length(which(isel)),length(which(isel))/length(isel)))
  }
  
  colconcat = c()
  for (k in 1:length(pcol)){
    colconcat=paste(colconcat,param[,colind[k]])  
  }
  param$FConcat = colconcat
  ii = grep('^FConcat$',colnames(param))
  colind = c(colind,ii)
  pcol = colnames(param[,colind])
  
  restable=data.frame()
  for (k in 1:length(pcol)){
    tt = table(param[,colind[k]],useNA = "ifany")
    aa=data.frame(tt); colnames(aa)[1] = pcol[k]
    aa=aa[!(aa$Freq==0),]
    
    if (exists('paramorig')){
      t2=table(paramorig[,colind[k]])
      rtn = rownames(tt)
      rtn2 = rownames(t2)
      dft2 = data.frame(t2)
      aa$Freqorig = dft2$Freq[match(rtn,rtn2)]
    }
    
    #order by freque
    
    aa = aa[order(aa$Freq,decreasing = TRUE),]
    rownames(restable)=NULL
    
    if (k==1) {restable=aa }
    else {
      #browser()
      #print(restable) 
      #print(class(aa))
      if(nrow(restable)>nrow(aa)) aa[nrow(restable),] = NA     
      else if (nrow(restable)<nrow(aa)) restable[nrow(aa),ncol(restable)] = NA
      restable=cbind(restable,aa) 
    } 
  }
  
  
  if ( class(fn)=="Pandoc" ){
    fn$add(restable)
  }
  else if (class(fn)=='character'){
    png(fn,height=(200+nrow(restable)*20),width = ncol(restable)*200)
    textplot(restable,show.rownames = FALSE)
    dev.off()      
  }
  return(restable)
}


## Correlation matrix with p-values. See http://goo.gl/nahmV for documentation of this function
cor.prob <- function (X, dfr = nrow(X) - 2) {
  R <- cor(X, use="pairwise.complete.obs")
  above <- row(R) < col(R)
  r2 <- R[above]^2
  Fstat <- r2 * dfr/(1 - r2)
  R[above] <- 1 - pf(Fstat, 1, dfr)
  R[row(R) == col(R)] <- NA
  R
}

## Use this to dump the cor.prob output to a 4 column matrix
## with row/column indices, correlation, and p-value.
## See StackOverflow question: http://goo.gl/fCUcQ
flattenSquareMatrix <- function(m) {
  if( (class(m) != "matrix") | (nrow(m) != ncol(m))) stop("Must be a square matrix.")
  if(!identical(rownames(m), colnames(m))) stop("Row and column names must be equal.")
  ut <- upper.tri(m)
  data.frame(i = rownames(m)[row(m)[ut]],
             j = rownames(m)[col(m)[ut]],
             cor=t(m)[ut],
             p=m[ut])
}

find_isel_fromlist =function(val,field_list){
  
  qq=field_list

  if (is.list(qq)) {
    name='';isel=integer();
    for (qqq in qq) {name=paste0(name,'_',qqq); isel=c(isel,which(val==qqq))} }
  
  else  { print('r');isel=which(val==qq);  name =qq   }   
  return(list(isel,name))
}

plot.roc = function(rr,name_field,qcfield){
  
  
  val = rr[,name_field]
  io = order(val); val=val[io]
  QC = rr$QC[io] ; 
  list[iq,namesel] = find_isel_fromlist(QC,qcfield)

  nb_motion = length(iq)
  tot_good = length(QC)-nb_motion
  #iq=which(!(QC=="" | QC=='good')) ; nb_motion = length(iq)
  
  vv=unique(round(val*1000)/1000)
  
  #PPV Positive predictiv value NPV Negative predictiv value
  
  Sens = numeric(); Spec = numeric() ;SpecPPV = numeric() ; SensNPV = numeric(); View=numeric()

  for ( k in 1:length(vv)){
    ii = which(val<=vv[k]) ;   
    #iq = grep('motion',QC[ii])
    list[iq,namesel] = find_isel_fromlist(QC[ii],qcfield)
    
    nbnotview = length(which(QC[ii]==""));nbview = length(which(QC[ii]!=""));  View[k] = nbview/(nbnotview+nbview)
    
    nbbad = length(iq); nbtot = length(ii)
    VP = nbbad; #Vrai Positif
    FP = nbtot-nbbad #FP
    VN = tot_good - FP
    FN = nb_motion - VP

    SpecPPV[k] = VP/(VP+FP) ; #nbbad/nbtot  # VP/(VP+FP)
    SensNPV[k] = VN/(VN+FN)
    Sens[k] = VP/(VP+FN) ; #nbbad/nb_motion   #VP/(VP+FN)
    Spec[k] = VN / (VN + FP)
    
  }
  # positive and negative predictive values (PPV and NPV respectively) 
  plot(vv,Spec,type='l',col='red',ylim=c(0,1),ylab='percentage',xlab=NA)
  lines(vv,Sens,col='green',lty=1)
  lines(vv,SpecPPV,lty=2,col='red')
  lines(vv,SensNPV,col='green',lty=2)
  
  lines(vv,View,col='blue')
  #legend(0.01,0.7,c('Specificity','Sensitivity','reviewed'),col=c('red','green','blue'),lty=c(1,1,2,2))
  #legend('right',c('Spec','Sens','PPV','NPV','reviewed'),col=c('red','green','red','green','blue'),lty=c(1,1,2,2,1))
  
  par(new=T)
  # hist(val,256,axes=F,xlab=NA,ylab=NA,main=sprintf('%s Sel %s',name_field,namesel)); axis(side=4)
  hist(val,256,axes=F,xlab=NA,ylab=NA,main=sprintf('%s',name_field)); #axis(side=4)
  grid()
  plot((1-Spec),Sens,xlim=c(0,1),ylim=c(0,1),type='l')
  lines(c(0,1),c(0,1),col='blue')
  
  return(list(Spec,Sens))
}

multi.hist <- function(x,nsize=c(3,4),breaks=256,file=NULL) {
  nvar <- dim(x)[2]  #number of variables
  
  if (!is.null(file))    pdf(file=file,paper='a4r',width=20, height=20)
  
  if (is.null(nsize)) {nsize=trunc(sqrt(nvar))+1 ; nsize = c(nsize,nsize)}  #size of graphic
  
  old.par <- par(no.readonly = TRUE) # all par settings which can be changed
  par(mfrow=nsize)       #set new graphic parameters
  for (i in 1:nvar) {
    xname=names(x)[i]                #get the names for the variables
    xvar = x[,i];
    
    nbNA = length(which(is.na(xvar)))
    nbbreaks = breaks
    nbf = length(unique(xvar))
    
    titre = sprintf('%s \n N:%d Na:%d',xname,nbf,nbNA)
    
    if (is.character(xvar) | is.factor(xvar)) {
      
      if (nbf == length(xvar))  message(sprintf('Skiping %s probably an ID',xname)) 
      else{
        if (nbf>100) {
          xvar=factor(xvar); levels(xvar) = 1:nbf
        }
        dotchart(table(xvar),main=titre)
        #barplot(table(xvar),main=titre,xlab=xname)
      }
    }
    if (is.integer(xvar) | is.numeric(xvar)){
      
      if (nbf==1)  message(sprintf('Skining interge %s only one value ',xname))
      else{        
        if (nbf<breaks & is.integer(xvar)) nbbreaks = diff(range(xvar,na.rm = TRUE))+1
        if (nbbreaks<256) titre = paste0(titre,sprintf(' nbreak:%d',nbbreaks))
        
        if (nbf<=10) dotchart(table(xvar),main=titre)
        else        hist(xvar,breaks=nbbreaks,main=titre)          
      }            
    }
      
  }    
  
  if (!is.null(file))    dev.off()
  
  #hist(xvar,main=name,xlab=name) }  #draw the histograms for each variable
  on.exit(par(old.par))   #set the graphic parameters back to the original
}


if(FALSE){
  # plot the data
  library(PerformanceAnalytics)
  chart.Correlation(mydata) 
  
  #scatterplots with color
  plot(x,y,col=rgb(0,100,0,50,maxColorValue = 255),pch=16)
  #similar with count
  library(hexbin) 
  plot(hexbin(x,y,xbins = 50))
  
  # Spinning 3d Scatterplot
  library(rgl)  
  plot3d(wt, disp, mpg, col="red", size=3) 

  #pour ajouter un fit plus elipse sur un scatter plot
  library(psych)
  ellipses(x, y, add = TRUE)

}
