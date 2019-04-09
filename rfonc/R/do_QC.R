
do_qc = function(res,select=rep(TRUE,length(res$filename)),
                 prefix='SID',jpeg_path='/home/romain/datac/QC/fig/all_jpg_3x6_raw',
                 qc_file = '/home/romain/datac/QC/QCout.csv',
                 select_file=tempfile() ,
                 msg = "good:motion1:motion2:motion3:SNR:artefact:other",
                 doprint=TRUE){

  
  res = res[select,]
  # = res$rn_suj  
  #fo = get_parent_path(suj,4,concat = TRUE,remove_ext = TRUE); fo = fo$f  
  #if (!is.null(par$order_val)) fo[k] = paste0(sprintf('S%.4d_%f_',k,par$order_val[k]),fo[k])
  
  if (file.exists(select_file)){ sprintf('taking select file %s',select_file)}
  else    {
    if (prefix == 'SID'){
      prefix = paste0('*SID_',res$Sid,'_SID_*')    
    }
    
    k=1;files=''
    for (ff in prefix) {files[k] = list.files(jpeg_path,pattern = ff , full.names = TRUE);k=k+1}
    
    #if (prefix[1] != '') fo = paste0(prefix,fo)
    aa=data.frame(id=res$Sid,filespath=files)
    write.table(aa,file=select_file ,row.names = FALSE,quote=FALSE,sep=',',col.names=FALSE)
    
  }
  
  cmd = sprintf('cd %s\n qcview.py -f %s -q %s -m %s',jpeg_path,select_file,qc_file,msg)
  
  if (doprint) sprintf('\n%s\n',cmd)
  else  system(cmd)
                
                
}

read_qc = function(rr,resfile = file.path(getwd(),'QCout.csv')){
  
  reshist = read.csv(resfile, stringsAsFactors=FALSE)
  colnames(reshist) = c('id','qc')
  iipos = match(reshist$i,rr$Sid)
  rr$QC = rep('',length(rr$Sid))
  rr$QC[iipos] = reshist$qc
  return(rr)
}
  
get_qc_jpg_filepath = function(rr,resdir){
  
  return(paste0(resdir,'/SID_',rr$Sid,'_SID*jpg'))
}