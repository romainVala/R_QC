
library(oro.nifti)
library(pastecs)
library(neurobase)
library('mmand')
library(psych)
#rm(list=ls())
source(file.path(Sys.getenv("HOME"),'dvpt/R_QC/init_path.R'))

#outsufix ='90'
outsufix ='crop'
NORMALIZE = TRUE
CROP = FALSE
REMOVE_ZEROS = TRUE
GETSIG=TRUE

args <- commandArgs(trailingOnly = TRUE)
print('arguments are')
print(args)
fname = args[1]

basefn=strsplit( basename(fname) , "\\.")[[1]][1]
sujn = basename(dirname(fname))

print(sprintf('Working on %s %s ',sujn,fname))
      
img <- readNIfTI(fname,reorient = FALSE)

if (NORMALIZE){
  img = (img-min(img))
  img = img/max(img)*256        
}

dim = img@dim_[2:4]
#for img with low snr you get only one peak with all, so take only the corner to isolate noise peak
if (CROP){
  ccrop1 = round(dim[1]/10)
  ccrop2 = round(dim[2]/10)
  aa = img[c(1:ccrop1,(dim[1]-ccrop1):dim[1]),c(1:ccrop2,(dim[2]-ccrop2):dim[2]),]
  xn <- as.vector(aa) #img_3d = drop_img_dim(img)
} else{
  xn <- as.vector(img) #img_3d = drop_img_dim(img)
}

if (REMOVE_ZEROS) {xn = xn[xn>0]}

#dq = data.frame(t(quantile(xn,probs=c(0.1,0.25,0.75,0.8,0.9))))
#doR <- density(xn[xn<dq$X90.], na.rm = TRUE,kernel="cosine",n=512,bw=4)
doR <- density(xn, na.rm = TRUE,kernel="cosine",n=512,bw=4)
if (CROP){
  doRall =  density(as.vector(img), na.rm = TRUE,kernel="cosine",n=512,bw=4)
}
  
# xn <- as.vector(img)
x <- unlist(doR[1])
y <- unlist(doR[2])

Vol_tot = length(img)
res=data.frame(Vol_tot)
res$Vol_img = length(img)


#smooth before peak finding 
ysw=smooth.spline(x,y,spar=0.1,keep.data = FALSE)

tp<-turnpoints(ysw$y)

if (tp$firstispeak) res$first_is_peak = 1 else res$first_is_peak = 0
firstpeak = 1 #argg+ res$first_is_peak #si le premier est un peak alors on prend le 2iem


yp=y[tp$peaks]
xp=x[tp$peaks]

yym = max(yp)
filter_cond = yp/yym>0.2  #remove small peack
yp = yp[filter_cond]
xp = xp[filter_cond]  

xmax = xp[firstpeak]
ymax = yp[firstpeak]
xpmin = x[tp$pits]
ypmin = y[tp$pits]

res$x_nois_pos = xmax
res$x_nois_end =xpmin[xpmin>xmax][1]
res$x_nois_end_max =(xmax-x[1])*2

yy = ypmin[xpmin>xmax][1]
lstr = sprintf('xnois = %f ',res$x_nois_end)

png(filename = paste0('histo_',basefn,'_',outsufix,'.png'),antialias = c('subpixel'),
    width = 800, height = 800)

plot(doR,main=sujn)
lines(ysw$x,ysw$y,col=2,lty = 'dotdash')
points(x[tp$peaks],y[tp$peaks],col="red")
points(x[tp$pits],y[tp$pits],col="green")
points(xp,yp,col="red",pch=22,lwd=4)
#points(res$x_nois_end,yy,col='black',pch=22,lwd=3)
lines(c(res$x_nois_end,res$x_nois_end),c(yy,ymax),type = 'l')

text(res$x_nois_end,ymax/2,labels = lstr,pos=4)

#itle(sub=sujn)
if (CROP){
  par(new=TRUE)
  plot(doRall,axes=FALSE)
  axis(3)
  axis(4)
}
dev.off()

xx=xn[xn<res$x_nois_end]

res$noise_mean = mean(xx)
res$noise_std = sd(xx)

write.csv(t(res), paste0('histo_',basefn,'_quantif',outsufix,'.csv'))

#ma = mask_img(img,img<res$x_nois_end)    ca garde les valeurs de imgs
#dim = img@dim_[2:4]
#mas = copyNIfTIHeader(img=img,arr=(array(xn<res$x_nois_end,dim=dim)))
filename_mask = paste0('bg_mask_',outsufix,'.nii.gz')
if (!file.exists(filename_mask)){
  mas = copyNIfTIHeader(img=img,arr=(img<res$x_nois_end)) 
  
  filtre=c(5,5,5)#c(2,2,2)#
  skern = shapeKernel(filtre,type='disc')
  #ca laisse du bruit exn = erode(xn,filtre)
  exn_erode = erode(mas,skern)
  
  exn = dilate(exn_erode,skern)
  
  skern = shapeKernel(c(2,2,2),type='disc')
  exn = erode(exn,skern)
  
  #ortho2(exn)
  nim = copyNIfTIHeader(img=img,arr = exn)
  
  writenii(nim,filename_mask)
  #writenii(nim,'mask_bg80.nii.gz')
} else exn = readNIfTI(filename_mask,reorient = FALSE)

if (file.exists('roi_bg.nii.gz')){
  print('taking backgrou from roi_bg')
  imgmask <- readNIfTI('roi_bg.nii.gz',reorient = FALSE)
} else {  imgmask = exn}

xx=as.vector(img[imgmask>0])

xx=xx[xx>0]

res$Vol_mask = length(exn[exn>0])
res$noise_mean_maskBG = mean(xx)
res$noise_std_maskBG  = sd(xx)

#rr = estimate_rician_sigma(xx)
#res = merge(res,rr)

write.csv(t(res), paste0('histo_',basefn,'_quantif',outsufix,'.csv'))

if(GETSIG){
#  if (res$Vol_mask/res$Vol_tot >0.8){ #too big so erode
#    exn = erode(exn,shapeKernel(c(5,5,5),type='disc'))
#  }
  xxs = as.vector(img[exn==0]) #but not imgmask which may be too small
  xxs = xxs[xxs>0]
  
  doR <- density(xxs, na.rm = TRUE,kernel="cosine",n=512,bw=4)
  x <- unlist(doR[1])
  y <- unlist(doR[2])
  
  #smooth before peak finding 
  ysw=smooth.spline(x,y,spar=0.1,keep.data = FALSE)
  tp<-turnpoints(ysw$y)
  

  ymax=max(y)
  xmax = x[y==ymax][1]
  yp=y[tp$peaks]
  
  png(filename = paste0('histo_',basefn,'_Signal.png'),antialias = c('subpixel'),
      width = 800, height = 800,)
  plot(doR,main=sujn)
  lines(ysw$x,ysw$y,col=2,lty = 'dotdash')
  points(x[tp$peaks],y[tp$peaks],col="red")
  points(x[tp$pits],y[tp$pits],col="green")

  dev.off()
  
  if (tp$nturns%%2==1) {
    if ( length(which(tp$pits)) > length(which(tp$peaks))) {
      print('ARGG should not happend more pits !')
      tp$pits[tail(which(tp$pits),1)] = FALSE #remove the last pits
    } else {
      #tp$peaks[tail(which(tp$peaks),1)] = FALSE #remove the last peaks
      #make first points a pits
      tp$pits[1] = TRUE
    }  
  } else {
    #for only 7 suj 
    if (tp$firstispeak) {
      tp$pits[1] = TRUE ; tp$pits[tail(which(tp$pits),1)] = FALSE #remove the last pits
    }
  }
  
  if ( length(which(tp$pits)) != length(which(tp$peaks))) print('ARGG still different length')
  
  #diff between min and max Attention $peak ont moins de points
  diffrel = (y[tp$pos[tp$peaks]]-y[tp$pos[tp$pits]])/max(y[tp$pos[tp$peaks]])
  #remove small diff <1% du max de density
  filtre_diff = diffrel>0.01
  
  res$x_extrem2 <- min(x[tp$pos[tp$pits]][filtre_diff==FALSE],na.rm = TRUE)
  #after the max
  mmax = x[y == max(y[tp$pos[tp$peaks]],na.rm = TRUE)]
  res$xpos_max = mmax
  res$x_extrem3 <- min(x[tp$pos[tp$pits]][ filtre_diff==FALSE & x[tp$pos[tp$pits]] >mmax ],na.rm = TRUE)
  
  res$data_nb_peak_inf = length(which(x[tp$pos[tp$peaks]]<= res$xpos_max))
  
  np = length(diffrel[filtre_diff])
  res$data_nb_peak <- np
  
  if (np>0) {
    peak_higth <- diffrel[filtre_diff]
    peak_pos <- x[tp$pos[tp$peaks]][filtre_diff]
    peak_den <- y[tp$pos[tp$peaks]][filtre_diff]
    pits_pos <- x[tp$pos[tp$pits]][filtre_diff]
    pits_den <- y[tp$pos[tp$pits]][filtre_diff]
    
    for (k in 1:np){
      fn=sprintf('x_data_P%s_pos',k)  
      d= data.frame(peak_pos[k])
      colnames(d)=fn
      res = merge(res,d)
    }
    
    for (k in 1:np){
      fn=sprintf('x_data_Min%s_pos',k)  
      d= data.frame(pits_pos[k])
      colnames(d)=fn
      res = merge(res,d)
    }
  }
  write.csv(t(res), paste0('histo_',basefn,'_quantif',outsufix,'.csv'))
}
  
print('done')
