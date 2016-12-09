\name{numeric_plot}
\alias{numeric_plot}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Numerical Variable Plot
}
\description{
This function plots count/density histogram of numerical variables from a dataframe
}
\usage{
numeric_plot(data, plot_switch, binVec)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
dataframe
}
  \item{plot_switch}{
a string: "on", "off", "grid"
}
  \item{binVec}{
a numeric vector. Example: c(20, 40, 60)
}
}
\details{
This function prints This function plots count/density histogram of numerical variables from a dataframe. A red line in each plot indicates the mean. If BinVec is Null, plots will use default bins. Otherwise, The fuction plots histograms with the size of the bins indicated in the binVec. This function imports a helper function multiplot. 
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
function (data, plot_switch, binVec) 
{
    num <- sapply(data, is.numeric)
    data <- data[, num]
    for (name in colnames(data)) {
        if (plot_switch == "on") {
            grid.newpage()
            m <- lapply(data[name], mean)
            plot1 <- ggplot(data, aes_string(name)) + geom_histogram(fill = "blue") + 
                geom_vline(xintercept = m[[1]], colour = "red")
            plot2 <- ggplot(data, aes_string(name)) + geom_histogram(aes(y = ..density..), 
                fill = "blue") + geom_vline(xintercept = m[[1]], 
                colour = "red")
            pushViewport(viewport(layout = grid.layout(1, 2)))
            print(plot1, vp = viewport(layout.pos.row = 1, layout.pos.col = 1))
            print(plot2, vp = viewport(layout.pos.row = 1, layout.pos.col = 2))
        }
        if (plot_switch == "grid") {
            count_plots <- list()
            density_plots <- list()
            if (missing(binVec)) {
                m <- lapply(data[name], mean)
                print(ggplot(data, aes_string(name), color = "blue") + 
                  geom_histogram(fill = "blue") + labs(title = "default bins"))
                print(ggplot(data, aes_string(name), color = "blue") + 
                  geom_histogram(aes(y = ..density..), fill = "blue") + 
                  labs(title = "default bins")) + geom_vline(xintercept = m[[1]], 
                  colour = "red")
            }
            else {
                for (i in 1:length(binVec)) {
                  m <- lapply(data[name], mean)
                  k <- ggplot(data, aes_string(name), color = "blue") + 
                    geom_histogram(fill = "blue", bins = binVec[i]) + 
                    labs(title = paste(binVec[i], "bins")) + 
                    geom_vline(xintercept = m[[1]], colour = "red")
                  count_plots[[i]] <- k
                }
                multiplot(plotlist = count_plots, cols = 2)
                for (i in 1:length(binVec)) {
                  m <- lapply(data[name], mean)
                  k <- ggplot(data, aes_string(name), color = "blue") + 
                    geom_histogram(aes(y = ..density..), fill = "blue", 
                      bins = binVec[i]) + labs(title = paste(binVec[i], 
                    "bins")) + geom_vline(xintercept = m[[1]], 
                    colour = "red")
                  density_plots[[i]] <- k
                }
                multiplot(plotlist = density_plots, cols = 2)
            }
        }
    }
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\concept{numeric plot}% use one of  RShowDoc("KEYWORDS")
