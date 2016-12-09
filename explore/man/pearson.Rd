\name{pearson}
\alias{pearson}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Pearson Correlation Coefficient Table
}
\description{
Prints a table with each pair of numerical variables with their Pearson Correlation Coefficients
}
\usage{
pearson(data)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
Dataframe: user input
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{

\item{data.frame(variable, Pcoeff)}{dataframe of the pearson table}
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
function (data) 
{
    num <- sapply(data, is.numeric)
    new_data <- data[, num]
    names <- colnames(new_data)
    combonames <- combn(names, 2)
    combo <- combn(length(colnames(new_data)), 2)
    variable <- paste(combonames[1, ], combonames[2, ], sep = "-")
    pearson <- Pcoeff <- c()
    for (i in 1:length(variable)) {
        p <- cor(x = new_data[combo[1, i]], y = new_data[combo[2, 
            i]])
        Pcoeff[i] <- p[1]
    }
    return(data.frame(variable, Pcoeff))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{pearson}% use one of  RShowDoc("KEYWORDS")

