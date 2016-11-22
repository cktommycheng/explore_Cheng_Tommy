#Tommy Cheng 
#MAT510 HW6
#26 Oct 2016 
require(ggplot2)
require(grid)

# Stuart- Excellent job all around with the explore function and the error handling. Where is the mean line for plots?
# But when running your explore function on the iris data set Frequency table is not what is expected. 
# Also if you have only one numeric column your program returns an unexpected error (try us as.data.frame(data(Titanic)) ). 
# You might look at a data set like "mpg" to see some other possible types of error handling you could do. 
# First do a str or summary of mpg, and then run your explore function. What is missing? Why?

#############################  Helper functions  ##############################
##############################################################################

test_data<- diamonds[1:300, ]   #create a testing set from a built-in dataframe in ggplot2


#The is.binary function determines whether a vector is binary
#Input: a vector
#Output: TRUE or FALSE
is.binary <- function(v) {
  x <- unique(v)                    #check all the distinct and put those in a vector x
  length(x) - sum(is.na(x)) == 2L         #check to see if x only contains 2 distinct values
}


#The freq_table function finds out all the factor class column and prints a frequency table for each factor 
#column
#Input: dataframe 
#Output = table of factor class
freq_table <- function(data) {
  categoricals <- sapply(data, function(x) (is.factor(x) || is.logical(x))) #find categorical columns
  data <- data[,categoricals] #categorical-only subset of data
  tables <- sapply(data, function(x) summary(x)) #get frequency counts of each factor
  return(tables)   #prints out the freqency table 
}



#The printSummary function prints out the  a summary table of a dataframe
#Input: dataframe
printSummary <-function(data){
  lapply(data[, sapply(data,is.numeric)], summary)
}


#The pearson function takes any dataframe as a parameter and returns a dataframe that 
#contains each pair of column names in the first column as a single string
#seperated by a "-" and their corresponding Pearson correlation coefficient
#in the second column.
#Input: data= dataframe
#Ouput: the combination of column names and their Pearson coefficients
pearson<- function(data){
  num <- sapply(data, is.numeric)     #check to see if columns are numeric
  new_data <- data[,num]      #create a new dataframe to store the numeric columns 
  names <- colnames(new_data)     #create vector to store the numeric colnames 
  combonames <- combn(names, 2) #find all the combinations of any 2 col names 
  combo <- combn(length(colnames(new_data)), 2)     #find all the combination of the indices of the colnames
  variable <- paste(combonames[1,], combonames[2,], sep = '-')  #create vector to store the variables combinations
  pearson <- Pcoeff <- c()    #create empty vectors 
  
  for(i in 1:length(variable)){
    p <- cor(x= new_data[combo[1,i]], y = new_data[combo[2,i]])  #calculates the correlations between any two of the cols
    Pcoeff[i] <- p[1]      #extract the number from a list 
  }
  return(data.frame(variable, Pcoeff))    #combine as dataframe
}



#The abs_pearson function takes a dataframe of with pearson correlations of each 2 variables
#and extract the values that are greater than a threshold based on user input
#Input: dataframe
#Ouput: the combination of column names and their Pearson coefficients values 
#that are greater than the threshold
abs_pearson <- function(dataset, threshold){
  row_index <- which(abs(dataset[,2]) > threshold)      #determine which column is greater than threshold
  return(dataset[row_index, ])                           #return a new dataframe with the updated coefficients
}


#The find_Rsquare function takes a dataframe and determines the R-square values 
#of each 2 variables 
#Input: dataframe
#Ouput: the combination of column names and their R-square values 
find_Rsquare<- function(data){
  num <- sapply(data, is.numeric)       #check to see if columns are numeric
  new_data <- data[,num]      #create a new dataframe to store the numeric columns 
  names <- colnames(new_data)     #create vector to store the numeric colnames 
  combonames <- combn(names, 2)   #find all the combinations of any 2 col names 
  combo <- combn(length(colnames(new_data)), 2)   #find all the combination of the indices of the colnames
  variable <- paste(combonames[1,], combonames[2,], sep = '-')  #create vector to store the variables combinations
  Rsquare <- c()    #create empty vectors to store Rsquare value
  
  for(i in 1:length(variable)){                       
    regression <- paste0(combonames[1,i], " ~ ", combonames[2,i])     #maunally type in the regression formula to avoid input a list in lm() function
    r1 <- summary( lm(as.formula(regression), data=new_data) )$r.squared      #extract r square values
    Rsquare[i] <- r1                                          
  }
  return(data.frame(variable, Rsquare))    #combine as dataframe
}


#The multiplot is extracted from R-cookbook. It combines subplots plots into a grid and prints it 
#Reference: http://www.cookbook-r.com/Graphs/Multiple_graphs_on_one_page_(ggplot2)/
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}


#The numeric_plot function executes in the following directions: 
#If the plot switch parameter is “on” or “grid”, then plot a pair of blue histograms 
#with a vertical red line at the mean (one using counts and the other density) for 
#every numerical variable at each number of bins integer specified in the bin vector parameter. 
#If the plot switch is set to “grid”, then the function prints a grid for each count-bin 
#combination and a separate grid for each density-bin size combination.
#Input: dataframe, string, vector(optional)
#Output: grid plots with count and desnity histograms

numeric_plot <- function(data, plot_switch, binVec) {
  num <- sapply(data, is.numeric)         #check to see which column is numeric
  data <- data[,num]                      #extract numeric columns
  for(name in colnames(data)) {           #loop through the columns in the dataset
    
    if(plot_switch == "on"){              #Case when switch is "on"
      grid.newpage()          
      m <- lapply(data[name], mean)       #find the mean of that currently iterated column
      plot1 <- ggplot(data, aes_string(name)) + geom_histogram(fill="blue") + geom_vline(xintercept = m[[1]], colour="red") 
      plot2 <- ggplot(data, aes_string(name)) + geom_histogram(aes(y= ..density..), fill="blue") + geom_vline(xintercept = m[[1]], colour="red")
      #multiplot(plot1, plot2, cols = 1)
      pushViewport(viewport(layout = grid.layout(1, 2)))
      print(plot1, vp = viewport(layout.pos.row = 1, layout.pos.col = 1))
      print(plot2, vp = viewport(layout.pos.row = 1, layout.pos.col = 2))
    }
    
    if(plot_switch == "grid"){          #Case when switch is "grid"
      count_plots <- list()             #Create a empty list to store the count histogram subplots of each bin size
      density_plots <- list()           #Create a empty list to store the density histograms subplots of each bin size
      if(missing(binVec)){              #This takes of the case when the vector is null, prints histogram with default bins 30
        plot5 <- ggplot(data, aes_string(name), color = "blue") + geom_histogram(fill="blue")+ labs(title= "default bins")
        plot6 <- ggplot(data, aes_string(name), color = "blue") + geom_histogram(aes(y= ..density..), fill="blue")+ labs(title= "default bins")
        multiplot(plot5, plot6, cols = 1)
      }else{                            #This takes care of the case when the user enters a vector
        for(i in 1:length(binVec)) {    #loop through each bin size and create a subplot
          k <- ggplot(data, aes_string(name), color = "blue") + geom_histogram(fill="blue", bins = binVec[i])+ labs(title= paste(binVec[i], "bins"))
          count_plots[[i]] <- k           #Push each subplot to a list 
        }
        multiplot(plotlist = count_plots, cols = 2)     
        
        for(i in 1:length(binVec)) {    #loop through each bin size and create a subplot
          k <- ggplot(data, aes_string(name), color = "blue") + geom_histogram(aes(y= ..density..), fill="blue", bins = binVec[i])+ labs(title= paste(binVec[i], "bins"))
          density_plots[[i]] <- k       #Push each subplot to a list
        }
        multiplot(plotlist = density_plots, cols = 2)
        
      }
    }
  }
}


#The cata_binary_plot function plots a gray bar graph for every categorical and binary variable.
#when the plot switch parameter is “on” or “grid"
#Input: dataframe, string
#Output: bar graphs
cata_binary_plot <-function(data, plot_switch){
  cata_binary <- sapply(data, function(x) (is.factor(x) || is.logical(x)) || is.binary(x))    #check categorical and binary	columns
  cata_binary_data <- data[cata_binary]     #extract those columns
  
  if(plot_switch == "on" || plot_switch == "grid") {      #check condition
    for(name in colnames(cata_binary_data)) {             #loop through the sorted dataframe and plot bar graphs for each column
      j <- ggplot(cata_binary_data, aes_string(name), color = "grey") + geom_bar(fill="grey")
      print(j)
    }
  }
}


#############################  Main function  ##############################
###########################################################################
explore <- function(dataframe, plot_switch, thres, binVec){
  
  while(!is.data.frame(dataframe)){                 #check to see if user input a dataframe. If not, prompt the user to choose a new file and convert to df
    print("Your input is not a dataframe ")
    print("Choose your a new csv or txt file ")
    file1 <- file.choose()
    dataframe <- read.csv(file1, header = T)
  }
  
  button <- plot_switch
  while(button != "off" && button != "on" && button != "grid"){   #Check to see if plot_switch is valid input
    print("invalid input for plot switch")
    n <- readline(prompt="Enter your option(off / on / grid): ")  #re-enter the input
    button <- n
  }
  
  threshold <- thres
  while(!is.numeric(threshold) || threshold < 0 || threshold >1 ){    #check to see if threshold is a valid input
    print("correlation threshold must be numeric and in range [0,1]")
    a <- readline(prompt="Enter your correlation threshold: ")      #re-enter the input
    threshold <- as.numeric(a)
  }
  
  while (!missing(binVec) && TRUE %in% (binVec <= 0)) {                        #check to see if bins are positive, if not, prompt user input again
    a <- readline(prompt="Enter or size of the bin Vector (or enter q to quit): ")
    if(a == "q"){
      stop("Quit")
    }
    else{
      binVec <- c()
      size <- as.numeric(a)
      for(i in 1:size){
        bin <- readline(prompt="Enter the number of bins: ")
        bin <- as.numeric(bin)
        binVec <- c(binVec, bin)
      }
    }
    
  }
  
  if (!missing(binVec) && !is.integer(binVec)) {            #Check to see if bins are all integer, if not, round it 
    binVec <- round(binVec)
  }
  
  
  new_dataframe <- freq_table(dataframe)
  allSummary <- printSummary(dataframe)
  Coeff_table <- pearson(dataframe)
  AbsCoeff_table <-abs_pearson(Coeff_table, threshold)
  Rsquare_table <- find_Rsquare(dataframe)
  numeric_plot(dataframe, button, binVec)
  cata_binary_plot(dataframe, button)
  new_list <-list(new_dataframe, allSummary, Rsquare_table, AbsCoeff_table)
  return(new_list)
  
}

#TestCase
explore(test_data, "grid", 0.1)
#explore(test_data, "on", 0.1, c(70, 80, 30))
#explore(test_data, "grid", 0.2, c(38.4, 30.1))
#explore(test_data, "hello", -100000, c(-60, -80, -900))
#explore(c(30, 40, 50), "grid", 0.3)