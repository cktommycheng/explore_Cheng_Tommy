\name{is.binary}
\alias{is.binary}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Determine Binary Vector
}
\description{
This function determines whether a vector / column is binary
}
\usage{
is.binary(v)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{v}{
%%     ~~Describe \code{v} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
TRUE/ FALSE
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Tommy Cheng
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
function (v) 
{
    x <- unique(v)
    length(x) - sum(is.na(x)) == 2L
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ isBinary }% use one of  RShowDoc("KEYWORDS")

