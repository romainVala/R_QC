get_nois_stats = function(xn,ers=FALSE){
  xn1D = xn[!xn==0]
  
  #library(psych)
  dq = data.frame(t(quantile(xn1D,probs=c(0.1,0.25,0.75,0.9,0.99,0.999))))
  colnames(dq) = paste0("Qt_",colnames(dq))
  # si on garde que le non zero ca transform en vector   dd = describe(as.vector(xn))
  dd = describe(xn1D)
  colnames(dd) = paste0("St_",colnames(dd))
  res=merge(dd,dq)
  
  #library('mmand')
  xnsignal = xn
  xnsignal[xn<res$Qt_X90.] = 0
  xnsignal[xn>=res$Qt_X90.] = 1
  filtre=c(3,3,3)#c(2,2,2)#
  skern = shapeKernel(filtre,type='disc')
  #ca laisse du bruit exn = erode(xn,filtre)
  exn_erode = erode(xnsignal,skern)
  exn = dilate(exn_erode,skern)
  #fuck que en 2D ee=ConnCompLabel(exn)   hb3 = hist(ee,max(ee),plot=FALSE)$count
  #library(neuroim)
  
  co=data.frame(nb_component=0,vol_allc=length(which(exn>0)))
  
  if (co$vol_allc>0){
    tryCatch({
      eec=connComp3D(exn>0)$size
      connsize = unique(eec[eec>0])
      connsize = connsize[order(connsize,decreasing = TRUE)]
      
      co$nb_component = length(connsize)
      co$vol_allcc = sum(connsize)
      co$vol_firstc = connsize[1]
      co$mean_erode = mean(xn[exn_erode>0])      
    },
    error=function(ee){
      co$nb_component = NaN
      message("Enconre un problem with connCeomp3D")
      message(ee)
    }    )
        
  }
  
  colnames(co) = paste0("Conn_",colnames(co))
  
  res=merge(res,co)
  #back to nifti    #library(fslr)    #imgexn = niftiarr(img_raw[,,sl_z_up],exn)
  #layout(matrix(1:25,5,5))      for (i in 1:20) image(exn[,,i])
  
  
  if (ers){
    if (length(which(exn>0))){
      exn = dilate(exn,skern)
      exn = dilate(exn,skern)
      exn = dilate(exn,skern)
      
      xn[exn>0] = 0
      xn1D = xn[!xn==0]
      dd = describe(xn1D)
      colnames(dd) = paste0("Stclean_",colnames(dd))
      res = merge(res,dd)
      #resplace with new value
      #colnames(dd) = paste0("St_",colnames(dd))
      #for (nn in colnames(dd)) res[,nn] = dd[,nn]
    }
    
    rr = estimate_rician_sigma(xn1D)
    res = merge(res,rr)      
  }
  #y=xn[xn>0]
  return(res)
}
args <- commandArgs(trailingOnly = TRUE)
