\name{get_parent_path}
\alias{get_parent_path}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
get_parent_path(fi, level = 1, concat = FALSE, remove_ext = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{fi}{
%%     ~~Describe \code{fi} here~~
}
  \item{level}{
%%     ~~Describe \code{level} here~~
}
  \item{concat}{
%%     ~~Describe \code{concat} here~~
}
  \item{remove_ext}{
%%     ~~Describe \code{remove_ext} here~~
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
function (fi, level = 1, concat = FALSE, remove_ext = FALSE) 
{
    for (k in 1:level) {
        fo = character(0)
        po = character(0)
        for (dd in fi) {
            po = c(po, dirname(dd))
            ff = basename(dd)
            if (remove_ext & k == 1) 
                ff = substr(ff, 1, regexpr("\\.", ff) - 1)
            fo = c(fo, ff)
        }
        fi = po
        if (concat) 
            if (!exists("fout")) 
                fout = fo
            else fout = paste(fo, fout, sep = "_")
        else fout = fo
    }
    return(list(p = po, f = fout))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
