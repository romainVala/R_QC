\name{do_fsl_slicer}
\alias{do_fsl_slicer}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
do_fsl_slicer(fin, outdir = "", par = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{fin}{
%%     ~~Describe \code{fin} here~~
}
  \item{outdir}{
%%     ~~Describe \code{outdir} here~~
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
function (fin, outdir = "", par = NULL) 
{
    defpar = list(type = "3x3", jobname = "fslslicer", walltime = "00:20:00", 
        prefix = "", sge = 1, montage = 0)
    if (length(par) == 0) 
        par = defpar
    else par = complete_param(par, defpar)
    if (outdir == "") 
        outdir = getwd()
    fo = get_parent_path(fin, 4, concat = TRUE, remove_ext = TRUE)
    fo = fo$f
    if (par$prefix != "") 
        fo = paste0(par$prefix, fo)
    k = 0
    for (ff in fin) {
        k = k + 1
        td = tempfile()
        cmd = c(sprintf("mkdir -p \%s", td), sprintf("cd \%s", 
            td), "var=`which fslmaths`", "dirfsl=$(dirname $var)", 
            sprintf("$dirfsl/slicer \%s -x 0.52  x1.png -y 0.52  x2.png -z 0.52  x3.png ", 
                ff), sprintf("convert x* +append \%s/\%s.jpg", 
                outdir, fo[k]), "cd", sprintf("rm -rf \%s", td))
        if (!exists("job")) 
            job = list(cmd)
        else job = c(job, list(cmd))
    }
    res = do_cmd(job, par)
    if (par$montage) {
        nbm = 1
        for (k in seq(1, length(fo), par$montage)) {
            cmd = "montage "
            jend = k + par$montage - 1
            if (jend > length(fo)) 
                jend = length(fo)
            for (j in k:jend) cmd = paste0(cmd, sprintf(" \%s.jpg ", 
                fo[j]))
            cmd = paste0(cmd, " -resize 700x256^ -gravity center -extent 700x256 -geometry 700x256,0,0 -tile 8x32 ")
            cmd = paste0(cmd, sprintf("montage\%.3d.png", nbm))
            cmd = c(sprintf("cd \%s", outdir), cmd)
            if (!exists("job2")) 
                job2 = list(cmd)
            else job2 = c(job2, list(cmd))
            nbm = nbm + 1
        }
        par$jobname = paste0(par$jobname, "montage")
        par$job_pack = 0
        par["qsubappend"] = res$fqsub
        res = do_cmd(job2, par)
    }
    return(res)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
