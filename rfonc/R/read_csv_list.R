

#' @description
#' take a list of csv files written in collumns (first collumns is the header and 2nd the value)
#' and it return a dataframe type =3 to read json
#' @param ll : csvlist
#' @title read_csv_list
#' @export
#' 

read_qc_view_csv = function(qc_file='',ref_path,remove_path_after = "_spm12"){
  
  qc=read.csv(qc_file,header = F)

  regexpat = paste0("(",remove_path_after,").*")
  print(sprintf('Reading %d lines',nrow(qc)))
  for (ln in levels(qc$V2)){
    iil = grepl(ln,qc$V2)
    #pp = sub("^([^.]*).*", "\\1",qc$V1[iil]);  #remove file extension
    
    pp = sub(regexpat, "\\1",qc$V1[iil])
    aa=ref_path%in%pp
    daa= data.frame(aa); colnames(daa) = ln
    print(sprintf('%s %d suj',colnames(daa),length(which(daa[,1]))))
    if (length(which(aa)) != length(pp) ) { print(sprintf('WARNING should fine %d',length(pp)))}
    if (!exists('myres') ) { myres=daa} else {  myres=c(myres,daa) }#res = merge(res,daa)}
  }; 
  myres = data.frame(myres)
  return(myres)
}


read_csv_list = function(ll,type=1){
  
  require(gtools)
  if (type==3){
    require(jsonlite)
  }
  
  tryCatch({
    for (k in 1:length(ll) ) {
      if (file.exists(ll[k])){
        
        if (type==1){ #value write in lines
          dfl = read.csv(ll[k],header = FALSE,stringsAsFactors=FALSE,nrow=1)    
          d1= read.csv(ll[k],header = FALSE,stringsAsFactors=FALSE,skip=1)    
          dd = data.frame(d1[,2])
          rownames(dd) = d1[,1]
          #colnames(dd)=d1[1,2]
          a2=data.frame(t(dd))
          a2$filename = dfl[2]        
        }
        if (type==11){ #values in column
          a2 = read.csv(ll[k],header = T,stringsAsFactors=FALSE,nrow=1)              
          a2$filename = ll[k]        
        }
        if (type==2){
          dfl = read.csv(ll[k],sep=':',header=FALSE)
          if (nrow(dfl)==16){
            dd= data.frame(dfl[c(3,4,5,6),2])
            rownames(dd) = paste0('wms_',dfl[c(3,4,5,6),1])
            aa = data.frame(t(dd))
            
            dd2 = data.frame(dfl[8:11,2])
            rownames(dd2) = paste0('nl_',dfl[8:11,1])
            aa2 = data.frame(t(dd2))
            
            dd3 = data.frame(dfl[13:16,2])
            rownames(dd3) = paste0('segwm_',dfl[13:16,1])
            aa3 = data.frame(t(dd3))
            
            a2 = merge(aa,merge(aa2,aa3))
            a2$suj=dfl[1,2]            
          } else message(sprintf('Incompltre fres %d : %s',k,ll[k]))
          
        }
        if (type==3){
          dfl = fromJSON(ll[k])
          a2 = data.frame(dfl)
          a2$filename = ll[k]
        }
      } else { print(sprintf('Warning skiping %s not exist',ll[k])); a2 = data.frame(filename=ll[k])}
      
      if (k==1) res = a2
      else {
        res=smartbind(res,a2)
      }      

    }  
  },
  error=function(ee){
    message(sprintf("Enconre un problem with reading file %d",k))
    message(ee)
  }    )

  
  return(res)
}
