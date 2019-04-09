
get_files = function(dirlist,reg,nbf=0){
  
  fo = character(0)
  for (dd in dirlist){
    files = list.files(dd,pattern = reg, full.names = TRUE)
    
    if (nbf) if (length(files) != nbf){
      msg = sprintf('error can not find %d with reg %s in %s ',nbf,reg,dd)
      stop(msg)
    }
    
    fo = c(fo,files)
  }
  return(fo)
}

get_files_no = function(dirlist,reg){
  
  do = character(0)
  
  for (dd in dirlist){
    files = list.files(dd,pattern = reg, full.names = TRUE)    
    if (length(files) ==0)  do = c(do,dd)
  }
  return(do)
}

get_dirs = function(dirlist,reg){

  if (is(reg,'list')){
    o = dirlist
    for (regg in reg){
      o = get_files(o,regg)
    }
    return(o)
  }
 
  fo = character(0)
  for (dd in dirlist){
    all = list.files(dd,pattern = reg, full.names = TRUE)
    fo = c(fo,all[file.info(all)$isdir])
  }
  return(fo)
}


addprefix = function(fi,prefix){
  dd=get_parent_path(fi)
  fo = mapply(function(x,y) file.path(x,y),dd$p,paste0(prefix,dd$f),SIMPLIFY = TRUE)
  return(as.character(fo))
} 


get_parent_path = function(fi,level=1,concat=FALSE,csep='_',remove_ext=FALSE){
  
  for (k in 1:level){
    fo = character(0); po = character(0)
    for (dd in fi){
      po = c(po,dirname(dd))
      ff=basename(dd)
      if (remove_ext & k==1) ff = substr(ff,1,regexpr('\\.',ff)-1)
      fo = c(fo,ff)      
    }
    fi = po
    if (concat) if (!exists('fout')) fout = fo else fout = paste(fo,fout,sep=csep)
    else fout = fo
  }
  return(list(p=po,f=fout))
  # pour concatener ff=mapply(function(x,y) file.path(x,y),res$p,res$f,SIMPLIFY = TRUE)
}


complete_param = function(p1,p2){ #this will add to p1 names in p2 that not exist in p1
  n1 = names(p1)
  n2 = names(p2)
  newname = !n2%in%n1
  p1[n2[newname]] = p2[newname]
  return(p1)
}
