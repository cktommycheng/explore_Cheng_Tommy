\name{explore}
\alias{explore}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Explore
}
\description{
Explore a dataframe. Returns different features and plots 
}
\usage{
explore(dataframe, plot_switch = on, thre = 0.2, binVec = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dataframe}{
dataframe
}
  \item{plot_switch}{
a string: "on", "off", "switch"
}
  \item{thre}{
a numeric value from 0 to 1 inclusively
}
  \item{binVec}{
a numeric vector
}
}
\details{
Explore function prints frequency table of categorical data, summary of numerical data, Pearson coefficients of each numerical variables based on user threshold input and Rsquare values of each pair of numerical variables. It also plots count/desnity histograms of numerical variables and bar graphs for categorical/logical variables
}
\value{
\item{new_dataframe}{frequency table of categorical variables}
\item{allSummary}{summary table of numerical variables}
\item{AbsCoeff_table}{table of pearson coefficients of each variable adjusted by user threshold input }
\item{Rsquare_table}{Rsquare values of each pair variable}
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
function (dataframe, plot_switch = on, thre = 0.2, binVec = NULL) 
UseMethod("explore")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{explore}

