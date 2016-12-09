\name{freq_table}
\alias{freq_table}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Frequency Table
}
\description{
Prints a frequency of all categorical/factor variables
}
\usage{
freq_table(data)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
Dataframe
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
\item{tables}{A frequency table of all categorical/factor variables as a dataframe}

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
  categoricals <- sapply(data, function(x) (is.factor(x) || is.logical(x) || is.character(x))) #find     categorical columns
  table(data[,categoricals]) #categorical-only subset of data
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{frequencytable}% use one of  RShowDoc("KEYWORDS")

