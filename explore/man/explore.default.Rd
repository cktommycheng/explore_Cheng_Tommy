\name{explore.default}
\alias{explore.default}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Explore
}
\description{
Explore default function 
}
\usage{
explore.default(dataframe, plot_switch, thres, binVec, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dataframe}{
dataframe: User import dataset
}
  \item{plot_switch}{
A string: "on", "off", "grid"
}
  \item{thres}{
numeric: from 0 to 1 inclusive
}
  \item{binVec}{
numeric vector: c(x, y, z) 
}
  \item{\dots}{
other arguements
}
}
\details{
This function prints a frequency table of all the categorical variables, prints a 
summary table for numerical variables, Pearson coefficient tables of each pair variable that is adjusted by the user input
and a rsquare table of each pair variable
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
function (dataframe, plot_switch, thres, binVec, ...) 
{
    ex <- explore1(dataframe, plot_switch, thres, binVec)
    class(ex) <- "explore"
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\concept{explore default}

