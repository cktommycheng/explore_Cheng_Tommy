\name{find_Rsquare}
\alias{find_Rsquare}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
R square table
}
\description{
Prints out the rsquare values for each pair of variable
}
\usage{
find_Rsquare(data)
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
  \item{data.frame("Variables" = pairs, "R-Square" = r_sq)}{dataframe with variable-pair names and correspond rsquare values}
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
    numeric_data <- data[, num]
    combos <- combn(colnames(numeric_data), 2)
    pairs <- paste(combos[1, ], combos[2, ], sep = "~")
    r_sq <- unname(sapply(pairs, function(x) summary(lm(x, data = numeric_data))$r.squared))
    return(data.frame(Variables = pairs, `R-Square` = r_sq))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\concept{Rsquare table}% use one of  RShowDoc("KEYWORDS")

