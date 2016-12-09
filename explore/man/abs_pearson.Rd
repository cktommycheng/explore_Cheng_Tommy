\name{abs_pearson}
\alias{abs_pearson}

\title{
Absolute Pearson Table
}
\description{
Prints out a table of Pearson Coefficients of each pair of numerical variables that are greater than
the absolute value of user input threshold
}
\usage{
abs_pearson(dataset, threshold)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dataset}{User imported dataset from the function pearson()}

  \item{threshold}{Numerical input from 0 to 1 inclusively }
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{

  \item{datset}{a dataframe with the modified person table}

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


\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (dataset, threshold) 
{
    row_index <- which(abs(dataset[, 2]) > threshold)
    return(dataset[row_index, ])
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\concept{Absolute Pearson coefficient}

