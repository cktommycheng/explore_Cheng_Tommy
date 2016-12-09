\name{explore1}
\alias{explore1}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Explore 1.0
}
\description{
Explore function prints frequency table of categorical data, summary of numerical data, Pearson coefficients of each numerical variables based on user threshold input and Rsquare values of each pair of numerical variables. It also plots count/desnity histograms of numerical variables and bar graphs for categorical/logical variables
}
\usage{
explore1(dataframe, plot_switch, thres, binVec)
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
}
\details{
%%  ~~ If necessary, more details than the description above ~~
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
function (dataframe, plot_switch, thres, binVec) 
{
    while (!is.data.frame(dataframe)) {
        print("Your input is not a dataframe ")
        print("Choose your a new csv or txt file ")
        file1 <- file.choose()
        dataframe <- read(file1, header = T)
        dataframe <- as.data.frame(dataframe)
    }
    button <- plot_switch
    while (button != "off" && button != "on" && button != "grid") {
        print("invalid input for plot switch")
        n <- readline(prompt = "Enter your option(off / on / grid): ")
        button <- n
    }
    threshold <- thres
    while (!is.numeric(threshold) || threshold < 0 || threshold > 
        1) {
        print("correlation threshold must be numeric and in range [0,1]")
        a <- readline(prompt = "Enter your correlation threshold: ")
        threshold <- as.numeric(a)
    }
    while (TRUE \%in\% (binVec <= 0)) {
        a <- readline(prompt = "Enter or size of the bin Vector (or enter q to quit): ")
        if (a == "q") {
            stop("Quit")
        }
        else {
            binVec <- c()
            size <- as.numeric(a)
            for (i in 1:size) {
                bin <- readline(prompt = "Enter the number of bins: ")
                bin <- as.numeric(bin)
                binVec <- c(binVec, bin)
            }
        }
    }
    if (!is.integer(binVec)) {
        binVec <- round(binVec)
    }
    new_dataframe <- freq_table(dataframe)
    allSummary <- printSummary(dataframe)
    Coeff_table <- pearson(dataframe)
    AbsCoeff_table <- abs_pearson(Coeff_table, threshold)
    Rsquare_table <- find_Rsquare(dataframe)
    new_list <- list(new_dataframe, allSummary, Rsquare_table, 
        AbsCoeff_table)
    return(new_list)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{explore1}% use one of  RShowDoc("KEYWORDS")

