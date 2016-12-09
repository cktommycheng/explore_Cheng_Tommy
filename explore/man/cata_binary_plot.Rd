\name{cata_binary_plot}
\alias{cata_binary_plot}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Categorical-Binary Variable Bar graph Plots
}
\description{
Plot bar graphs for each categorical or binary variables
}
\usage{
cata_binary_plot(data, plot_switch)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
a dataframe: user import dataset
}
  \item{plot_switch}{
a string: "off", "on" or "grid"
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
function (data, plot_switch) 
{
    cata_binary <- sapply(data, function(x) (is.factor(x) || 
        is.logical(x)) || is.binary(x))
    cata_binary_data <- data[cata_binary]
    if (plot_switch == "on" || plot_switch == "grid") {
        for (name in colnames(cata_binary_data)) {
            j <- ggplot(cata_binary_data, aes_string(name), color = "grey") + 
                geom_bar(fill = "grey")
            print(j)
        }
    }
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\concept{Categorical Binary bar plot}

