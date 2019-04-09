\name{do_cmd}
\alias{do_cmd}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
do_cmd(job, par = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{job}{
%%     ~~Describe \code{job} here~~
}
  \item{par}{
%%     ~~Describe \code{par} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (job, par = NULL) 
{
    defpar = list(sge = 1, queue = "long", job_pack = 1, walltime = "", 
        nb_thread = 1, mem = "", jobdir = "", jobname = "jobname", 
        job_append = FALSE, qsubappend = "")
    if (length(par) == 0) 
        par = defpar
    else par = complete_param(par, defpar)
    if (par$qsubappend != "") 
        par$job_append = TRUE
    jobdir = par$jobdir
    if (par$walltime != "") {
        hh = as.integer(substr(par$walltime, 1, 2))
        dday = floor(hh/24)
        hh = hh\%\%24
        par$walltime = sprintf("\%d-\%.2d\%s", dday, hh, substr(par$walltime, 
            3, nchar(par$walltime)))
    }
    if (par$job_pack > 1) {
        for (k in seq(1, length(job), par$job_pack)) {
            kkend = k + par$job_pack - 1
            if (kkend > length(job)) 
                kkend = length(job)
            aa = job[[k]]
            for (kk in (k + 1):kkend) aa = c(aa, job[[kk]])
            if (!exists("jobnew")) 
                jobnew = list(aa)
            else jobnew = c(jobnew, list(aa))
        }
        job = jobnew
        rm(jobnew)
    }
    if (par$sge == 1) {
        if (jobdir == "") 
            jobdir = getwd()
        jobdir = file.path(jobdir, par$jobname)
        dir.create(jobdir, showWarnings = FALSE)
        print(sprintf("writing \%d job in \%s", length(job), jobdir))
        if (par$job_append) 
            kinit = length(dir(jobdir, pattern = "^j.*"))
        else kinit = 0
        k = kinit
        cmd_loc = character(0)
        for (j in job) {
            k = k + 1
            jname = sprintf("j\%.2d_\%s", k, par$jobname)
            fjob = file.path(jobdir, jname)
            j = c("#!/bin/bash", j)
            write(j, fjob)
            cmd_loc = c(cmd_loc, sprintf("bash \%s > \%s.log 2> \%s.err", 
                fjob, jname, jname))
        }
        write(cmd_loc, file = file.path(jobdir, "do_all_local.sh"))
        f_do_array = file.path(jobdir, "do_job_array.sh")
        cmd_ja = c("#!/bin/bash", "echo started on $HOSTNAME", 
            " date", "tic=\"$(date +\%s)\"", sprintf(" cmd=$( printf \"j\%\%02d_\%s\" ${SLURM_ARRAY_TASK_ID})", 
                par$jobname), sprintf(" bash \%s/$cmd", jobdir), 
            "toc=\"$(date +\%\%s)\";", "sec=\"$(expr $toc - $tic)\";", 
            "min=\"$(expr $sec / 60)\";", "heu=\"$(expr $sec / 3600)\";", 
            "echo Elapsed time: $min min $heu H")
        write(cmd_ja, f_do_array)
        cmd = sprintf("export jobid=`sbatch -p \%s -N 1 --cpus-per-task=\%d --job-name=\%s --export=NONE ", 
            par$queue, par$nb_thread, par$jobname)
        if (par$walltime != "") 
            cmd = paste0(cmd, paste0(" -t ", par$walltime))
        if (par$mem != "") 
            cmd = paste0(cmd, sprintf(" --mem=\%d ", par$mem))
        if (par$qsubappend != "") 
            cmd = paste0(cmd, " --depend=afterok:$jobid ")
        cmd = c(paste0(cmd, sprintf(" -o \%s/log-\%\%A_\%\%a  -e \%s/err-\%\%A_\%\%a  --array=1-\%d \%s |awk '{print $4}'` ", 
            jobdir, jobdir, k, f_do_array)), "echo submitted job $jobid")
        if (par$qsubappend != "") {
            f_do_qsub = par$qsubappend
            AA = TRUE
        }
        else {
            f_do_qsub = file.path(jobdir, "do_qsub.sh")
            AA = FALSE
        }
        write(cmd, f_do_qsub, append = AA)
        return(list(j = job, fqsub = f_do_qsub))
    }
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
