do_fsl_slicer_multi = function(fins,inds,outdir='',par=NULL){
  
  if (outdir=='') outdir = getwd()
  if (!file.exists(outdir)) dir.create(outdir)
  
  if ( is.data.frame(indsee) ){
    for (nn in names(inds))
    {
      fin = fins[inds[[nn]]]
      outdir_one=file.path(outdir,nn)
      do_fsl_slicer(fin,outdir = outdir_one,par = par)
    }
  } else {
    print('todo')
  }
    
  
}
do_fsl_slicer = function(fin,outdir='',par=NULL){
  
  defpar = list(type='3x1',type_slice='x 43',jobname='fslslicer',walltime='00:20:00',prefix='',sge=1,
                montage=0,tile="8x32", color='grey' ,
                sel_img='',sel_mask='',sel_mask_crop='',crop=FALSE, inv_mask = FALSE,
                sel_reslice='',reslice=FALSE,sel_overlay='',sel_overlay2='',
                order_val=NULL,scale=TRUE)
  
  if (length(par)==0) par=defpar else par = complete_param(par,defpar)
  
  if (outdir=='') outdir = getwd()
  if (!file.exists(outdir)) dir.create(outdir)
  
  if (par$sel_img != '') {    
    din=fin ; fin = get_files(fin,par$sel_img, nbf = 1) 
    if (par$sel_mask != '') fmask = get_files(din,par$sel_mask, nbf = 1)
    if (par$crop){ fmask_crop = get_files(din,par$sel_mask_crop,nbf = 1) }
    if (par$reslice) freslice = get_files(din,par$sel_reslice,nbf=1)
  } else {
    if (par$sel_mask != '') fmask = par$sel_mask
    if (par$crop) fmask_crop = par$sel_mask_crop
    if (par$reslice) freslice = par$sel_reslice }
    
  
  fo = get_parent_path(fin,5,concat = TRUE,remove_ext = TRUE); fo = fo$f
  
  if (par$prefix[1] != '') fo = paste0(par$prefix,fo)
  k=0;
  
  mytmpdir = file.path(outdir,'tmp')
  if (!file.exists(mytmpdir)) dir.create(mytmpdir)
  
  print(sprintf('preparing  %d volumes in %s ',length(fin),outdir))
  for (ff in fin){
    k=k+1
    td=tempfile(tmpdir = mytmpdir) #td = tempdir()
        
    cmd = c(sprintf('mkdir -p %s',td),sprintf('cd %s',td))
    
    if (exists('fmask')) ffmask = fmask[k]
    if (par$crop){
      if(par$reslice){
        cmd = c(cmd,sprintf(' mrtransform -quiet %s -template %s rs.nii',ff,freslice[k]))
        cmd = c(cmd,sprintf(' mrtransform -quiet %s -template %s rmask.nii',ffmask,freslice[k]))
        cmd = c(cmd,sprintf(' mrtransform -quiet %s -template %s rmaskcrop.nii',fmask_crop[k],freslice[k]))
        cmd = c(cmd,sprintf('mrcrop -quiet rs.nii -mask rmaskcrop.nii crop.nii'))
        cmd = c(cmd,sprintf('mrcrop -quiet rmask.nii -mask rmaskcrop.nii crop_mask.nii'))        
        
      }
      else{
        cmd = c(cmd,sprintf('mrcrop -quiet %s -mask %s crop.nii',ff,fmask_crop[k]))
        cmd = c(cmd,sprintf('mrcrop -quiet %s -mask %s crop_mask.nii',ffmask,fmask_crop[k]))        
      }
      ff = 'crop.nii'
      ffmask = 'crop_mask.nii'
    } 
    
    if (par$inv_mask){
      cmd = c(cmd,sprintf('fslmaths %s -add -1 -abs inv_mask',ffmask))
      ffmask = 'inv_mask.nii.gz'
    }
    
    if (par$scale) {
      if (exists('fmask')) { cmd = c(cmd,sprintf('maxi=`fslstats %s -k %s -P 98`',ff,ffmask)) }
      else { cmd = c(cmd,sprintf('maxi=`fslstats %s  -P 98`;echo $maxi',ff)) }      
      c3dscale = '-stretch 0 $maxi 0 255 -clip 0 255' 
    } else c3dscale=''
    
    if (!is.null(par$order_val)) fo[k] = paste0(sprintf('S%.4d_%f_',k,par$order_val[k]),fo[k])
    
    if (par$type=='3x1fsl'){
      cmd = c(cmd,"var=`which fslmaths`","dirfsl=$(dirname $var)",
              sprintf('$dirfsl/slicer %s -x 0.52  x1.png -y 0.52  x2.png -z 0.52  x3.png ',ff),
              sprintf('convert x* +append %s/%s.jpg',outdir,fo[k]))
    }
    else{ #c3d way
      
      if(par$reslice & !par$crop){
        cmd = c(cmd, paste(sprintf('c3d %s %s -reslice-identity %s -as OO \\',freslice[k],ff,c3dscale)))
      }
      else  cmd = c(cmd, paste(sprintf('c3d %s %s -as OO \\',ff,c3dscale)))
      
      if (par$type=='2x2'){
        
        if (par$sel_overlay[1] != ''){
          cmds = sprintf('%s  -thresh 0.9 inf 2 0 -reslice-identity -as SEG  ', par$sel_overlay[k])
          cmds = paste(cmds,sprintf('-clear -push OO -push SEG -foreach -slice x 43% -flip xy -endfor -oli ~/itklockup.txt 0.3 -type uchar -omc x1.png '))
          cmds = paste(cmds,sprintf('-clear -push OO -push SEG -foreach -slice y 45% -flip xy -endfor -oli ~/itklockup.txt 0.3 -type uchar -omc y1.png '))
          cmds = paste(cmds,sprintf('-clear -push OO -push SEG -foreach -slice z 55% -flip xy -endfor -oli ~/itklockup.txt 0.3 -type uchar -omc z1.png '))
          cmds = paste(cmds,sprintf('-clear -push OO -push SEG -foreach -slice z 65% -flip xy -endfor -oli ~/itklockup.txt 0.3 -type uchar -omc z2.png '))
          cmd = c(cmd,cmds)
        }
        else  cmd = c(cmd, paste('-push OO -slice x 43% -flip xy -type uchar -o x1.png ',
                                 '-push OO -slice y 45% -flip xy -type uchar -o y1.png ',
                                 '-push OO -slice z 45% -flip xy -type uchar -o z1.png ',
                                 '-push OO -slice z 55% -flip xy -type uchar -o z2.png '))
        
        if(F){
          cmd = c(cmd,'convert x1.png -crop 210x210+30+0 x1.png')
          cmd = c(cmd,'convert y1.png -crop 210x210+0+0 y1.png')
          cmd = c(cmd,'convert z1.png -crop 210x210+0+35 z1.png')
          cmd = c(cmd,'convert z2.png -crop 210x210+0+35 z2.png')
          
        }
        
        cmd = c(cmd, sprintf(' montage y1.png x1.png z* -tile 2x2 -mode Concatenate -background black  %s/%s.jpg',outdir,fo[k]))    
      }
      if (par$type=='3x6'){
        slice_pos = c('y 25%','y 35%','y 45%','y 55%','y 65%','y 55%',
                      'x 23%','x 33%','x 43%','x 53%','x 63%','x 73%',
                      'z 25%','z 35%','z 45%','z 55%','z 65%','z 75%')
        slice_name = c('y1','y2','y3','y4','y5','y6','x1','x2','x3','x4','x5','x6','z1','z2','z3','z4','z5','z6')
        cmds=''
        for (nbs in 1:length(slice_pos) ){
          cmds = paste(cmds,sprintf('-clear -push OO -slice %s -flip xy  -color-map %s -type uchar -omc %s.png',
                                    slice_pos[nbs],par$color,slice_name[nbs]))
        }
        cmd = c(cmd,cmds)

        cmd = c(cmd, sprintf(' montage y* x*.png z* -tile 6x3 -mode Concatenate -background black  %s/%s.jpg',outdir,fo[k]))    
      }
      else if (par$type=='2x1a'){
        
        cmd = c(cmd, paste('-push OO -slice x 45% -flip xy -type uchar -o z1.png ',
                           '-push OO -slice z 55% -flip xy -type uchar -o z2.png '))
        
        cmd = c(cmd, sprintf(' montage y1.png x1.png z* -tile 2x1 -mode Concatenate -background black  %s/%s.jpg',outdir,fo[k]));
      }
      else if (par$type=='1x1'){
        if (par$sel_overlay2[1]!=''){
          cmds = sprintf('%s -reslice-identity -thresh 0.9 inf 3 0 -as SEG2  ', par$sel_overlay2[k])
          cmds = paste(cmds,sprintf('%s -reslice-identity  -thresh 0.9 inf 2 0 -push SEG2 -add -as SEG -clear -push OO -push SEG -foreach ', par$sel_overlay[k]))
          cmds = paste(cmds,sprintf(' -slice %s% -flip xy -endfor -oli ~/itklockup.txt 0.5 -type uchar -omc  %s/%s.png ',par$type_slice,outdir,fo[k]))
          cmd = c(cmd,cmds)
          
        }
        else if (par$sel_overlay[1] != ''){
          cmds = sprintf('%s  -thresh 0.9 inf 1 0 -reslice-identity -as SEG -clear -push OO -push SEG -foreach ', par$sel_overlay[k])
          cmds = paste(cmds,sprintf(' -slice %s% -flip xy -endfor -oli ~/itklockup.txt 0.5 -type uchar -omc  %s/%s.png ',par$type_slice,outdir,fo[k]))
          cmd = c(cmd,cmds)
        }
        else cmd = c(cmd, paste(sprintf('-clear -push OO -slice %s% -flip xy -type uchar -o  %s/%s.png ',par$type_slice,outdir,fo[k])))
        #cmd = c(cmd, paste(sprintf('-clear -push OO -slice %s -flip xy -type uchar -omc  %s/%s.png ',par$type_slice,outdir,fo[k])))
      }
      
    }
    
    cmd = c(cmd,'cd',sprintf('rm -rf %s',td))

    if (!exists('job')) job = list(cmd) else job = c(job,list(cmd))
  }
  
  res = do_cmd(job,par)
  
  if (par$montage){
    setwd(par$jobname)
    #[vv ii] = sort(fo);
    nbm=1
    for (k in seq(1,length(fo),par$montage)) {      
      cmd = 'montage '
      jend = k+par$montage-1 
      if (jend > length(fo)) jend = length(fo)
      for (j in k:jend) cmd = paste0(cmd,sprintf(' %s.jpg ',fo[j]))
      if (par$type=='3x1'){
        cmd = paste(cmd,'-resize 700x256^ -gravity center -extent 700x256 -geometry 700x256,0,0 ') }
      else if (par$type=='2x2'){
        cmd = paste(cmd,'-resize 500x500 -background black -gravity center -extent 500x500 -geometry 500x500,0,0 ') }
      else if (par$type=='2x1a'){
        cmd = paste(cmd,'-resize 500x250 -background black -gravity center -extent 500x250 -geometry 500x250,0,0 ') }
      #-resize 500x500^  ca coupe la plus grand dimension
      
      cmd = paste(cmd,sprintf('-tile %s',par$tile))
      
      cmd = paste(cmd,sprintf('montage%.3d.png',nbm))
      cmd = c(sprintf('cd %s',outdir),cmd)
      
      if (!exists('job2')) job2 = list(cmd) else job2 = c(job2,list(cmd))
      nbm = nbm+1
    }
    par$jobname=paste0(par$jobname,'montage') ; par$job_pack=0
    par['qsubappend'] = res$fqsub       
    res = do_cmd(job2,par)
    setwd('..')
  }
  
  
  #return(res)
}

do_montage=function(fin,resdir = getwd(),name='montage',par=NULL){
  defpar = list(tile="16x16", mix_line=FALSE,extent='',resize='',crop='',
                jobname='job_montage',walltime='00:20:00',sge=1,test=FALSE  )
  
  if (length(par)==0) par=defpar else par = complete_param(par,defpar)
  
  
  if (resdir=='') resdir=dirname(fin[1])
  
  nbline = as.integer(gsub('x.*',"",par$tile));  nbcol = as.integer(gsub('[0-9]*x',"",par$tile))
  montage = nbline*nbcol

  if (par$test) { montage=4; fin=fin[1:4]; par$tile='2x2'}
  
  if (par$mix_lin){
    indm=matrix(1,nbline,nbcol)
    indm[,seq(2,nbline,2)]=0
    
    fmix=fin
    fmix[indm==1] = head(fin,montage/2)
    fmix[indm==0] = head(rev(fin),montage/2)
    fin=fmix    
  }
  
  nbm=1
  for (k in seq(1,length(fin),montage)) {      
    cmd = 'montage '
    if (par$crop!='') { cmd = paste(cmd,sprintf('-crop %s',par$crop)) }
      
    jend = k+montage-1 
    if (jend > length(fin)) jend = length(fin)
    for (j in k:jend) cmd = paste0(cmd,sprintf(' %s ',fin[j]))
    
    if (par$resize!='') cmd = paste(cmd,sprintf('-resize %s',par$resize))
    
    if (par$extent!='') {
    cmd = paste(cmd,sprintf('-background black -gravity center -extent %s -geometry +0+0',par$extent))   }
    else cmd = paste(cmd,'-background black -gravity center -geometry +0+0')
    
#     if (par$type=='3x1'){
#       cmd = paste(cmd,'-resize 700x256^ -gravity center -extent 700x256 -geometry 700x256,0,0 ') }
#     else if (par$type=='2x2'){
#       cmd = paste(cmd,'-resize 500x500 -background black -gravity center -extent 500x500 -geometry 500x500,0,0 ') }
#     else if (par$type=='2x1a'){
#       cmd = paste(cmd,'-resize 500x250 -background black -gravity center -extent 500x250 -geometry 500x250,0,0 ') }
#     #-resize 500x500^  ca coupe la plus grand dimension
#     
    #if (par$tile!='')
    cmd = paste(cmd,sprintf('-tile %s',par$tile))
    
    cmd = paste(cmd,file.path(resdir,sprintf('%s%.3d.jpg',name,nbm)))
    
    #cmd = c(sprintf('cd %s',outdir),cmd)
    
    #if (!exists('job2')) job2 = list(cmd) else job2 = c(job2,list(cmd))
    nbm = nbm+1
    
    if (!exists('job2')) job2 = list(cmd) else job2 = c(job2,list(cmd))
    
    #system(cmd)
  }
  
  do_cmd(job2,par)
  
}

#option montage, -resize 500x500\! ignore le aspect ratio
#-resize 64x64\>  ne rezie que les plus grande -resize 64x64\< que les plus petite