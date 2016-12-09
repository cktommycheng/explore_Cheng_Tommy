\name{print.explore}
\alias{print.explore}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Print Explore Tables and Plots
}
\description{
This function prints all tables and plots from the explore function
}
\usage{
print.explore(x, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
dataframe
}
  \item{\dots}{
Other Arguements
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
function (x, ...) 
{
    cat("Frequency table for every categorical and logical variable: ")
    print(x$freq_table)
    cat("Summaryinstall table for each numerical variable: ")
    print(x$allSummary)
    cat("R-square of each pair of column: ")
    print(x$Rsquare_table)
    cat("Target Pearson table ")
    print(x$AbsCoeff_table)
    cat("Numeric Plots")
    print(x$numeric_plot)
    cat("Bar Plots")
    print(x$cata_binary_plot)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{printexplore}% use one of  RShowDoc("KEYWORDS")

