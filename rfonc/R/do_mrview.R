do_mrview = function(res,select,vals=NULL){
  if (is.logical(select) )  fn = res$filename[which(select)]  
  else fn = res$filename[select]
  
  if (!is.null(vals)){
    indorder = order(vals[which(select)])
    fn = fn[indorder]
  }
  
  ff=""
  for (k in 1:length(fn) )  ff = sprintf('%s %s ',ff,fn[k])
  cmd = sprintf('mrview %s ',ff)
  print(length(fn))
  system(cmd)
  return(fn)  
}
do_mrviewraw = function(res,select=rep(TRUE,length(res$filename)),vals=NULL,view='mrview'){
  
  if (is.logical(select) )  fn = res$filename[which(select)]  
  else fn = res$filename[select]

  if (!is.null(vals)){
    indorder = order(vals[which(select)])
    fn = fn[indorder]
  }
  
  ff=""
  for (k in 1:length(fn) )  {
    fname = list.files(dirname(fn[k]),pattern='^ms.*gz',full.names = TRUE)[1] 
    ff = sprintf('%s %s ',ff,fname)
  }
  cmd = sprintf('%s %s ',view,ff)
  print(cmd)
  print(length(fn))
  system(cmd)
  return(fn)  
}

view_qc = function(rr,qc_jpeg_dir='/servernas/home/romain/datac/QC/fig/all_jpg_3x6_raw/'){
  
  fn = paste0(qc_jpeg_dir,'SID_',rr$Sid,'_SID*')
  
  ff=""
  for (k in 1:length(fn)) {
    #fname = list.files(dirname(fn[k]),pattern=jpg,full.names = TRUE)[1] 
    ff = sprintf('%s %s',ff,fn[k]) }
  cmd = sprintf('eog %s ',ff)
  print(length(fn))
  
  system(cmd)
}
view_histo = function(res,select,vals=NULL,ft=1,decreasing=FALSE){
  
  switch(as.character(ft),
         "1"={jpg='histo_rmniMask_fit.png'},
         "2"={jpg='histo_rmniMask_all.png'},
         "3"={jpg='histo_ms_.*_all.jpg'},
         "4"={jpg='histo_ms_.*_data.jpg'})
  
  fn = res$filename[select]  
  if (!is.null(vals)){
    indorder = order(vals[which(select)],decreasing=decreasing)
    fn = fn[indorder]
    vals = vals[which(select)][indorder]
  }
  
  ff=""
  for (k in 1:length(fn)) {
    fname = list.files(dirname(fn[k]),pattern=jpg,full.names = TRUE)[1] 
    ff = sprintf('%s %s',ff,fname) }
  cmd = sprintf('eog %s ',ff)
  print(length(fn))
  oo=data.frame(fn)
  if (!is.null(vals)){
    oo$vals = vals
  }
  system(cmd)
  return(oo)
}
#view_histo(rr,rr$ms_x_nois_end>400,vals=rr$ms_x_nois_end,ft=3,decreasing = TRUE)
