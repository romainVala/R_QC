# source('~/dvpt/R/tools/sge_cmd/do_cmd.R')
# f=resvbm$suj
# system.time(ff<-get_files(f,'^ms'))
# j=do_fsl_slicer(ff,par=list(montage=256,job_pack=50))
# do_cmd(j,list(job_pack=10))

# suj = get_parent_path(f)[[1]]
# dnl = paste0(suj,'/nl'); fnl = get_files(dnl,'^nr')

#' write a list of cmd in bash job with the slurm submition
#'
#' @param job a list of string cmd
#' @title do_cmd
#' @example
#' cmd = c('echo coucou','echo byby')
#' do_cmd(cmd)
#'
#' @export
do_cmd = function(job,par=NULL)
{
  defpar = list(sge=1,queue = 'normal',job_pack=1, walltime='',nb_thread=1, mem='',
                jobdir='',jobname='jobname',job_append = TRUE, qsubappend='',remove_jobdir=FALSE,
                xvfb=FALSE)

  if (length(par)==0) par=defpar else par = complete_param(par,defpar)

  if (par$qsubappend!='') par$job_append=TRUE
  jobdir = par$jobdir

  #convert walltime should be hh:mm:ss
  if (par$walltime != '') {
    hh = as.integer(substr(par$walltime,1,2))
    dday = floor(hh/24); hh = hh%%24
    par$walltime = sprintf('%d-%.2d%s',dday,hh,substr(par$walltime,3,nchar(par$walltime)))
  }

  if (as.numeric(par$job_pack)>1){
    for(k in seq(1,length(job),par$job_pack)) {
      kkend = k+par$job_pack-1
      if(kkend> length(job)) kkend=length(job)
      aa = job[[k]]
      for (kk in (k+1):kkend) aa = c(aa,job[[kk]])
      if (!exists('jobnew')) jobnew = list(aa) else jobnew = c(jobnew,list(aa))
    }
    job = jobnew ; rm(jobnew)
  }
  if (par$sge ==1) {
    if (jobdir=='') jobdir = getwd()
    jobdir = file.path(jobdir,par$jobname)
    if (par$remove_jobdir) unlink(paste0(jobdir,'/*'),recursive = TRUE)

    dir.create(jobdir,showWarnings = FALSE)
    print(sprintf('writing %d job in %s',length(job),jobdir))

    if (par$job_append)   kinit = length( dir(jobdir,pattern='^j.*') )    else  kinit = 0

    k=kinit;cmd_loc=character(0)
    for (j in job){
      k=k+1
      jname = sprintf('j%.2d_%s',k,par$jobname)
      fjob = file.path(jobdir,jname)
      #fjoblog = past0(fjob,'.log')
      j = c('#!/bin/bash',j)
      write(j,fjob)

      cmd_loc = c(cmd_loc,sprintf('bash %s > %s.log 2> %s.err',fjob,jname,jname))
    }
    write(cmd_loc,file=file.path(jobdir,'do_all_local.sh'))

    f_do_array = file.path(jobdir,'do_job_array.sh')

    cmd_ja=c('#!/bin/bash','echo started on $HOSTNAME',' date','tic="$(date +%s)"',
             sprintf(' cmd=$( printf "j%%02d_%s" ${SLURM_ARRAY_TASK_ID})',par$jobname))
    if (par$xvfb)    cmd_ja = c(cmd_ja,sprintf('xvfb-run -w 6 -n ${SLURM_ARRAY_TASK_ID} bash %s/$cmd',jobdir))
    else    cmd_ja = c(cmd_ja,sprintf(' bash %s/$cmd',jobdir))
    cmd_ja = c(cmd_ja,'toc="$(date +%s)";','sec="$(expr $toc - $tic)";','min="$(expr $sec / 60)";','heu="$(expr $sec / 3600)";','echo Elapsed time: $min min $heu H')
    write(cmd_ja,f_do_array)

    cmd=sprintf('export jobid=`sbatch -p %s -N 1 --cpus-per-task=%d --job-name=%s --export=NONE ',par$queue,par$nb_thread,par$jobname)

    if (par$walltime!='') cmd = paste0(cmd, paste0(' -t ',par$walltime) )
    if (par$mem!='') cmd = paste0(cmd, sprintf(' --mem=%d ',par$mem))
    if (par$qsubappend!='')  cmd = paste0(cmd, ' --depend=afterany:$jobid ')

    cmd = c(paste0(cmd,sprintf(' -o %s/log-%%A_%%a  -e %s/err-%%A_%%a  --array=1-%d %s |awk \'{print $4}\'` ',jobdir,jobdir,k,f_do_array)),
            'echo submitted job $jobid')

    if (par$qsubappend!='') {  f_do_qsub = par$qsubappend ; AA=TRUE  } else { f_do_qsub = file.path(jobdir,'do_qsub.sh'); AA=FALSE }

    write(cmd,f_do_qsub,append=AA)

    return(list(j=job,fqsub=f_do_qsub))
  }
}
