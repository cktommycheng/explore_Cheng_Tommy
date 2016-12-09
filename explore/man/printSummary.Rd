\name{printSummary}
\alias{printSummary}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Print Summary Table
}
\description{
This function prints summary tables of all numerical variables from a dataframe
}
\usage{
printSummary(data)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
dataframe
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
It returns a summary table of all numerical variables 
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
function (data) 
{
    lapply(data[, sapply(data, is.numeric)], summary)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{printSummary }

