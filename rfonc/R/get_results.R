get_new_results = function(resdir='/home/romain.valabregue/datal/dicom/res',remove=FALSE){
  
  resmriqc = read.csv(file.path(resdir,'res_mriqc_same_as_new.csv'))
  resmriqcn = read.csv(file.path(resdir,'res_new_mriqc_same_as_old.csv'))
  
}
  

  
get_all_results = function(resdir='/home/romain.valabregue/datac/QC/',remove=FALSE){

  ######## reshist
  reshist = read.csv(file.path(resdir,'volume_histo3.csv'), stringsAsFactors=FALSE)
  #order
  reshist = reshist[order(reshist$filename),]
  
  ######## respar
  #respar = read.csv(file.path(resdir,'param10528.csv'), stringsAsFactors=FALSE)
  respar = get_sql_param()
  #renamed column
  colnames(respar)[grep("ExamName",colnames(respar))] = 'Proto'
  colnames(respar)[grep("type_patho",colnames(respar))] = 'pathos'
  #colnames(respar)[grep("PatientsAge",colnames(respar))] = 'age'
  respar$bin_age = round(respar$PatientsAge/5)*5
  respar$ImageType = gsub('ORIGINAL_PRIMARY_M_','',respar$ImageType) #they all start with the same name
  respar$pathos = gsub('patient_','P_',respar$pathos) #they all start with the same name
  
  respar$voxprod = respar$sizeX * respar$sizeY * respar$sizeZ
  respar$PatMode[grep('PAT',respar$PatMode)] = 'Ipat2' ; respar$PatMode = factor(respar$PatMode)
  #regroup patho
  respar$patho = as.factor(respar$pathos)
  print('regrouping parkinson'); print(levels(respar$patho)[c(4,5,11,13)]) ; levels(respar$patho)[c(4,5,11,13)] = 'P_parki'
  print('regrouping lesion'); print(levels(respar$patho)[c(3,9,13,15)]); levels(respar$patho)[c(3,9,13,15)] = 'P_lesion'
  print('regrouping autre ');  print(levels(respar$patho)[c(2,5:12)]); levels(respar$patho)[c(2,5:12)] = 'P_other'
  
  #respar = subset(respar,select=-c(Affine,Orient))
  respar = subset(respar,select=-c(Affine))
  
  
  
  resvbm = read.csv(file.path(resdir,'seg_vbm10528.csv'), stringsAsFactors=FALSE)
  #remove last empty column and lapmean with value 0
  resvbm = resvbm[,-ncol(resvbm)]
  ind_val_0 <- which(apply(resvbm,2,sd)==0)
  resvbm <- resvbm[, -ind_val_0]
  #order
  resvbm = resvbm[order(resvbm$suj),]
  
  resvbmnl = read.csv(file.path(resdir,'seg12nl_10528.csv'), stringsAsFactors=FALSE)
  #remove first column with linge names and lapmean with value 0
  resvbmnl = resvbmnl[,-1]
  ind_val_0 <- which(apply(resvbmnl,2,function(x) sd(x,na.rm=T))==0)
  resvbmnl <- resvbmnl[, -ind_val_0]
  #order
  resvbmnl = resvbmnl[order(resvbmnl$filename),]
  
  #rnoise = read.csv('histo_rmniMask/histo_fit_s10528_all_noise.csv', stringsAsFactors=FALSE)
  rnoise = read.csv(file.path(resdir,'volume_noise.csv'), stringsAsFactors=FALSE)
  #get colnames with regex and assign to reshist???
  colnames(rnoise) = paste0('rn_',colnames(rnoise))
  #order
  rnoise = rnoise[order(rnoise$rn_suj),]
  icc=grep('St_vars',colnames(rnoise)) #valeur unique
  rnoise = subset(rnoise,select=-icc)
  icc=grep('rn_filename',colnames(rnoise)) #valeur unique V1 .?
  rnoise = subset(rnoise,select=-icc)
  
  resaff = read.csv(file.path(resdir,'spm8_mat_affine.csv'), stringsAsFactors=FALSE)
  resaff = resaff[order(resaff$suj),]
  #affine param of anat volume are all nul sz are voxel size
  resaff = subset(resaff,select=-c(ax,vol_ay,vol_az, vol_sx,vol_sy,vol_sz))
  cc=colnames(resaff); cc[-(1:13)] = c('spm_orig_tx','spm_orig_ty','spm_orig_tz','spm_orig_rx','spm_orig_ry','spm_orig_rz')
  colnames(resaff) = cc;
  
  resmeas = read.csv(file.path(resdir,'reg_meas.csv'), stringsAsFactors=FALSE)
  resmeas = resmeas[order(resmeas$suj),]

  
  #check and remove ids
  reshist$suj = get_parent_path(reshist$suj)$p   
  
  check=TRUE
  if (check){
    refsuj = reshist$suj   
    refid = get_parent_path(refsuj,level=3,concat = TRUE,csep = '')$f
    
    csuj = get_parent_path(resvbm$suj)$p
    cid = get_parent_path(csuj,level=3,concat = TRUE,csep = '')$f    
    cc = cid %in% refid 
    stopifnot(cc)
    
    csuj = get_parent_path(rnoise$rn_suj)$p
    cid = get_parent_path(csuj,level=3,concat = TRUE,csep = '')$f
    cc = cid %in% refid 
    stopifnot(cc)
    
    cid = resaff$suj
    cc = cid %in% refid 
    stopifnot(cc)
    
    cid = get_parent_path(resmeas$suj,level=3,concat = TRUE,csep = '')$f
    cc = cid %in% refid 
    stopifnot(cc)
 
    cid = get_parent_path(respar$nifti_dir,level=3,concat = TRUE,csep = '')$f
    cc = cid %in% refid 
    stopifnot(cc)
    
  }
  
  #clean remove IDs
  resvbm = subset(resvbm,select=-c(suj))
  #keep it because it has the path of preproc rnoise = subset(rnoise,select=-c(rn_suj))
  resaff = subset(resaff,select=-c(suj))
  resmeas = subset(resmeas,select=-c(suj))
  respar = subset(respar,select=-c(nifti_dir))
  
  
  #regroup
  rr=cbind(reshist,resvbm,respar,rnoise,resaff,resmeas)
  
  fileQC = file.path(resdir,'QCout.csv')
  if (file.exists(fileQC))   rr=read_qc(rr,resfile = fileQC)
  
  #remove phantome
  if (remove){
    ii = grep('2012_07_28_IMAGEN_072000000401',rr$suj)
    rr=rr[-ii,]
    reshist=reshist[-ii,] ; resvbm=resvbm[-ii,] ; resvbmnl=resvbmnl[-ii,] ; respar=respar[-ii,] ; 
    rnoise=rnoise[-ii,] ; resaff=resaff[-ii,] ; resmeas=resmeas[-ii,]    
  }
  
  #remove missing mriqc (because of rename exam or failled)
  iinew = c(3032,3167,3168,3193,10429,10430) 
  iiall=c(ii,iinew); ii=iinew
  rr=rr[-ii,];  reshist=reshist[-ii,] ; resvbm=resvbm[-ii,] ; resvbmnl=resvbmnl[-ii,] ; respar=respar[-ii,] ; 
  rnoise=rnoise[-ii,] ; resaff=resaff[-ii,] ; resmeas=resmeas[-ii,]    
  
  print(sprintf('removing %d suj',length(iiall)))
  
  ######## res mriqc and cat12  already restricted number of suj
  resmriqc = read.csv(file.path(resdir,'mriqc_allres_10521.csv'), stringsAsFactors=FALSE)
  rescat   = read.csv(file.path(resdir,'cat12_allres_10521.csv'), stringsAsFactors=FALSE)

  #regroup
  print(length(rr))
  rr=cbind(rr,resmriqc,rescat)
  
  #change file name new location is given by cat12 or mriqc juilly 2017
  rr$filename = rescat$suj
  
  return(list(rr,reshist,resvbm,resvbmnl,respar,rnoise,resaff,resmeas,resmriqc,rescat,iiall))
}

read_cormat_txt = function(filep,header='/export/data/users/romain.valabregue/job/cormat_regnl_brain/matrix_files.txt'){
  #read.csv(file,sep="\t",header=FALSE)
  Afull = read.table(filep,sep="",header=FALSE)
  Afull = as.matrix(Afull) 
  colnames(Afull)=NULL
  
  hdr = read.table(header,sep="",header=FALSE,stringsAsFactors = F)
  io=order(hdr)
  Afull=Afull[io,io]
  return(Afull)
}


#read histo and normalise
read_histo_matrix = function(resdir=getwd()) {
  
  require(signal) #for function interp1 in histo normalisation
  
  load(file.path(resdir,'histo_matrix.Rdata'))
  #your get 4 variables
  # histoMatY : dim nbsujx512 each line is the amplitude of the histogram (density)
  # histoMatX : dim nbsujx512 each line is the x axis of the histogram (range of value in each volume)
  # meanSignal : a normalisation factor for each sujbect (the mean value of gray and white voxel in the brain)
  # fa a string that gives the path of each subject data 
  
  
  new_X=seq(0.5,150,0.5) ; 
  new_Y =  matrix(,nrow=nrow(histoMatX),ncol=length(new_X))
  #y2= matrix(,nrow=nrow(histoMatX),ncol=length(xx))
  
  for (k in seq(1,nrow(histoMatX)) ) {
    y=histoMatY[k,]
    x=histoMatX[k,]
    #scale x for intensity  with mean gray/white intensity
    x2 = x/meanSignal[k]*100  
    #interpolate to have the same x sample
    y2 = interp1(x2,y,new_X)    
    #the extremite of the histogram (max of x) varie, so put zero if not defined
    y2[is.na(y2)]=0    
    new_Y[k,] = y2    
  }
  
  return(list(new_Y,fa))
  
}

get_sql_param = function(){
  library('RMySQL')
  #mydb = dbConnect(MySQL(), user='cenir', password='pschitt', dbname ='cenir', host='prepost')
  mydb = dbConnect(MySQL(), user='cenir', password='pschitt', dbname ='cenir', host='localhost')
  dbListTables(mydb)
  dbListFields(mydb,'exam')
    
  cmdsqlwhere = ' from ExamSeries e, results_anat r where e.Sid=r.Sid and r.status=1 and ';
  
  cmdsqlwhere = paste0(cmdsqlwhere, ' dimZ>150 and ExamName not like \'%HIFU%\' and e.ExamName not like \'%TIWI%\' and e.ExamName not like \'%MONK%\' ')
  cmdsqlwhere = paste0(cmdsqlwhere, 'and e.ExamName not like \'PROTO_PSP\' and e.ExamName not like \'%SPINE%\' and  e.SName not like \'QC_Phantom_MPRAGE\' ')
  cmdsqlwhere = paste0(cmdsqlwhere, 'and e.PatientsName not like \'%fant%\' and e.PatientsName not like \'%PHAN%\'  ')
  
  
  cmdsqlini = 'select ExamName , PatientsName, SName, e.Sid, TR, TI , FA, Orient, TE, PatMode , sizeX, sizeY, sizeZ, dimX ,dimY, dimZ ,PhaseAngle, PhaseDir,Affine,PixelBw,TablePos,vbmgrayvol,status'
  cmdsqlini = paste0( cmdsqlini, ', e.CoilName , e.MachineName , type_patho , PatientsAge , ImageType , nifti_dir' )
  cmdsql = paste0(cmdsqlini, cmdsqlwhere)
  #un peu trop cmdsql = paste('select * ',cmdsqlwhere)
  rr=dbSendQuery(mydb,cmdsql)
  rs = fetch(rr,n=-1)
  
  cmdsql = 'select Sid from quality_serie where content like \'phantome\''
  rr = dbSendQuery(mydb,cmdsql)
  esid = fetch(rr,n=-1)
  ii=match(c(esid$Sid,124328),rs$Sid)    #suj 2012_05_31_RECAP_Sujet02LJ has bad mni recal
  rs=  rs[-na.omit(ii),] 
  
  dbDisconnect(mydb)
  
  #order rs
  oo = order(rs$nifti_dir)
  rs = rs[oo,]
  
  return(rs)  
  
}

get_sql_serie = function(){
  library('RMySQL')
  #mydb = dbConnect(MySQL(), user='cenir', password='pschitt', dbname ='cenir', host='prepost')
  mydb = dbConnect(MySQL(), user='cenir', password='pschitt', dbname ='cenir', host='10.5.9.213')
  dbListTables(mydb)
  dbListFields(mydb,'exam')
    
  cmdsqlwhere = ' from ExamSeries e where ';
  
  cmdsqlwhere = paste0(cmdsqlwhere, ' dimZ>150 and ExamName not like \'%HIFU%\' and e.ExamName not like \'%TIWI%\' and e.ExamName not like \'%MONK%\' ')
  cmdsqlwhere = paste0(cmdsqlwhere, 'and e.ExamName not like \'%SPINE%\' and e.ExamName not like \'%CATALO%\'  ')
#  cmdsqlwhere = paste0(cmdsqlwhere, 'and e.PatientsName not like \'%fant%\' and e.PatientsName not like \'%PHAN%\'  ')
  cmdsqlwhere = paste0(cmdsqlwhere, 'and e.SeqType=\'MPRAGE\' and date(AcqTime)<(\'2017-08-31\') ')
  
  #cmdsqlini = 'select ExamName , PatientsName, SName, e.Sid, TR, TI , FA, Orient, TE, PatMode , sizeX, sizeY, sizeZ, dimX ,dimY, dimZ ,PhaseAngle, PhaseDir,Affine,PixelBw,TablePos,vbmgrayvol,status'
  #cmdsqlini = paste0( cmdsqlini, ', e.CoilName , e.MachineName , type_patho , PatientsAge , ImageType , nifti_dir' )
  cmdsqlini = paste('select * , count(*) as c ')
  cmdsql = paste0(cmdsqlini, cmdsqlwhere, '  group by  substring(e.AcqTime,1,16)')
  
  rr=dbSendQuery(mydb,cmdsql)
  rs = fetch(rr,n=-1)
  
  cmdsql = 'select Sid from quality_serie where content like \'phantome\''
  rr = dbSendQuery(mydb,cmdsql)
  esid = fetch(rr,n=-1)
  ii=match(c(esid$Sid,124328),rs$Sid)    #suj 2012_05_31_RECAP_Sujet02LJ has bad mni recal
  rs=  rs[-na.omit(ii),] 
  
  dbDisconnect(mydb)
  
  #order rs
  oo = order(rs$nifti_dir)
  rs = rs[oo,]
  
  return(rs)  
  
}


prescan_normalise = function(){
  ######get prescan Normalize
  dicj=paste0(rr$suj,'/dic*json')
  k=1; for (dic in dicj){  cmd = sprintf('strings %s |grep sNormalizeFilter |wc -l',dic); a[k]=system(cmd,intern=T);k=k+1}
  anum=as.numeric(a)
  save(ind,file='ind_prescan_normalize.Rdata')
  
}
