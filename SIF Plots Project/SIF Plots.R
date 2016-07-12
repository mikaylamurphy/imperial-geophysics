SIF_plot <- function(filename){
  all_data <- read.table(filename, fill = TRUE, header = TRUE)
  require(scales)
  # Removes rows with header names.
  header_rows <- which(apply(all_data, 1, function(x) any(grepl("Step", x))))
  data <- all_data[-header_rows, ]
  
  # Creating column with fracture number as integers.
  data$'FractureNum' <- as.numeric(gsub("[^0-9]", "", data$'FractureName'))
  
  # Converting factor values in data frame to numeric values.
  #column <- sapply(data[,-2], is.factor)
  data[,-2] <- lapply(data[,-2], function(x) as.numeric(as.character(x)))
  
  # Preparing colours for plot.
  num_of_fractures <- length(unique(data$'FractureNum'))
  num_of_steps <- max(data$'Step') + 1
  colours <- rainbow(num_of_fractures)
  
  first_step <- subset(data, Step == 0)
  
  # Creates 2x2 matrix for four figures drawn below (Type I, II, III, and G values).
  par(mfrow=c(2,2))
  
  plot(data$'TipNr', data$'KI', main = paste("KI"), xlab = 'Tip Number', ylab= "KI SIF Value", pch= 20, col= muted(colours[data$'FractureNum'], l = ((data$'Step'+1)/num_of_steps)*100))
  plot(data$'TipNr', data$'KII', main = paste("KII"), xlab = 'Tip Number', ylab= "KII SIF Value", pch= 20, col= muted(colours[data$'FractureNum'], l = ((data$'Step'+1)/num_of_steps)*100))
  plot(data$'TipNr', data$'KIII', main = paste("KIII"), xlab = 'Tip Number', ylab= "KIIIscale_fill_hue(l=40) SIF Value", pch= 20, col= muted(colours[data$'FractureNum'], l = ((data$'Step'+1)/num_of_steps)*100))
  plot(data$'TipNr', data$'G', main = paste("G"), xlab = 'Tip Number', ylab= "G Value", pch= 20, col= muted(colours[data$'FractureNum'], l = ((data$'Step'+1)/num_of_steps)*100))
  }