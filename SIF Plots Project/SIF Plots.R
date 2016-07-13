SIF_plot <- function(filename){
  require(scales)
  
  # Converts raw .txt file to dataframe.
  all_data <- read.table(filename, fill = TRUE, header = TRUE)
  
  # Removes rows with header names from dataframe.
  header_rows <- which(apply(all_data, 1, function(x) any(grepl("Step", x))))
  data <- all_data[-header_rows, ]
  
  # Creating column with fracture number as integers.
  data$'FractureNum' <- as.numeric(gsub("[^0-9]", "", data$'FractureName'))
  
  # Converting factor values in data frame to numeric values.
  data[,-2] <- lapply(data[,-2], function(x) as.numeric(as.character(x)))
  
  # Preparing colours for plot.
  num_of_fractures <- length(unique(data$'FractureNum'))
  num_of_steps <- length(unique(data$'Step'))
  colours <- rainbow(num_of_fractures)
  
  # Saves plots as pdf.
  filename_no_ext <- substr(filename, 1, nchar(filename)-4)
  pdf_name <- paste(filename_no_ext, '_plots.pdf')
  pdf(file = pdf_name, title = pdf_name)
  plot_name <- unlist(strsplit(pdf_name, '/'))
  plot_name <- plot_name[length(plot_name)]

  # Creates 2x2 matrix for four raw data figures drawn below (Type I, II, III, and G values).
  par(mfrow=c(2,2), oma=c(0,0,3,0))
  
  # Raw data graphs.
  plot(data$'TipNr', data$'KI', main = paste("KI"), xlab = 'Tip Number', ylab= "KI SIF Value", pch= 20, col= muted(colours[data$'FractureNum'], l = ((data$'Step'+1)/num_of_steps)*100))
  plot(data$'TipNr', data$'KII', main = paste("KII"), xlab = 'Tip Number', ylab= "KII SIF Value", pch= 20, col= muted(colours[data$'FractureNum'], l = ((data$'Step'+1)/num_of_steps)*100))
  plot(data$'TipNr', data$'KIII', main = paste("KIII"), xlab = 'Tip Number', ylab= "KIII SIF Value", pch= 20, col= muted(colours[data$'FractureNum'], l = ((data$'Step'+1)/num_of_steps)*100))
  plot(data$'TipNr', data$'G', main = paste("G"), xlab = 'Tip Number', ylab= "G Value", pch= 20, col= muted(colours[data$'FractureNum'], l = ((data$'Step'+1)/num_of_steps)*100))
  mtext(plot_name, adj=0.5, side=3, outer=TRUE)
  
  # Calculating data statistics (mean, min, max).
  data_means <- aggregate(data[,-2], by = list(TipNr = data$'TipNr', FractureNum = data$'FractureNum'), FUN = "mean")
  data_mins <- aggregate(data[,-2], by = list(TipNr = data$'TipNr', FractureNum = data$'FractureNum'), FUN = "min")
  data_maxs <- aggregate(data[,-2], by = list(TipNr = data$'TipNr', FractureNum = data$'FractureNum'), FUN = "max")
  
  # Creates 2x2 matrix for four mean figures drawn below (Type I, II, III, and G values).
  par(mfrow=c(2,2))
  
  # Mean graphs.
  plot(data_means$'TipNr', data_means$'KI', main = paste("KI Mean"), xlab = 'Tip Number', ylab= "KI Avg SIF Value", pch= 20, col = colours[data_means$'FractureNum'])
  plot(data_means$'TipNr', data_means$'KII', main = paste("KII Mean"), xlab = 'Tip Number', ylab= "KII Avg SIF Value", pch= 20, col = colours[data_means$'FractureNum'])
  plot(data_means$'TipNr', data_means$'KIII', main = paste("KIII Mean"), xlab = 'Tip Number', ylab= "KIII Avg SIF Value", pch= 20, col = colours[data_means$'FractureNum'])
  plot(data_means$'TipNr', data_means$'G', main = paste("G Mean"), xlab = 'Tip Number', ylab= "Avg G Value", pch= 20, col = colours[data_means$'FractureNum'])
  
  # Creates 2x2 matrix for four min figures drawn below (Type I, II, III, and G values).
  par(mfrow=c(2,2))
  
  # Min graphs.
  plot(data_mins$'TipNr', data_mins$'KI', main = paste("KI Minimum Value"), xlab = 'Tip Number', ylab= "KI Min SIF Value", pch= 20, col = colours[data_mins$'FractureNum'])
  plot(data_mins$'TipNr', data_mins$'KII', main = paste("KII Minimum Value"), xlab = 'Tip Number', ylab= "KII Min SIF Value", pch= 20, , col = colours[data_mins$'FractureNum'])
  plot(data_mins$'TipNr', data_mins$'KIII', main = paste("KIII Minimum Value"), xlab = 'Tip Number', ylab= "KIII Min SIF Value", pch= 20, , col = colours[data_mins$'FractureNum'])
  plot(data_mins$'TipNr', data_mins$'G', main = paste("G Minimum Value"), xlab = 'Tip Number', ylab= "Min G Value", pch= 20, col = colours[data_mins$'FractureNum'])
  
  # Creates 2x2 matrix for four max figures drawn below (Type I, II, III, and G values).
  par(mfrow=c(2,2))
  
  # Max graphs.
  plot(data_maxs$'TipNr', data_maxs$'KI', main = paste("KI Max Value"), xlab = 'Tip Number', ylab= "KI Max SIF Value", pch= 20, col = colours[data_maxs$'FractureNum'])
  plot(data_maxs$'TipNr', data_maxs$'KII', main = paste("KII Max Value"), xlab = 'Tip Number', ylab= "KII Max SIF Value", pch= 20, col = colours[data_maxs$'FractureNum'])
  plot(data_maxs$'TipNr', data_maxs$'KIII', main = paste("KIII Max Value"), xlab = 'Tip Number', ylab= "KIII Max SIF Value", pch= 20, col = colours[data_maxs$'FractureNum'])
  plot(data_maxs$'TipNr', data_maxs$'G', main = paste("Max G Value"), xlab = 'Tip Number', ylab= "Max G Value", pch= 20, col = colours[data_maxs$'FractureNum'])
  
  # Calculating difference from step 0.
  
  # Number of rows at step 0.
  num_at_step_0  <- num_of_fractures * (max(data$'TipNr'+1))
  
  # Creating data frame with only Step 0 repeated to subtract from.
  data_at_step_0 <- do.call("rbind", replicate(num_of_steps, subset(data[1:num_at_step_0,]), simplify = FALSE))
  
  # Setting fracture and tip numbers to 0 so original values will be preserved after subtraction.
  data_at_step_0$'FractureNum' <- 0
  data_at_step_0$'TipNr' <- 0
  
  # Subtracting the two data frames.
  data_diff_from_step_0 <- data[,-2] - data_at_step_0[,-2]
  
  # Creates 2x2 matrix for four difference from step 0 figures drawn below (Type I, II, III, and G values).
  par(mfrow=c(2,2))
  
  # Difference in KI, KII, KIII, and G values for each step from step 0 at each tip graphs.
  plot(data_diff_from_step_0$'TipNr', data_diff_from_step_0$'KI', main = paste("Difference in KI values from Step 0"), xlab = 'Tip Number', ylab= "Delta KI SIF Value", pch= 20, col= muted(colours[data$'FractureNum'], l = ((data$'Step'+1)/num_of_steps)*100))
  plot(data_diff_from_step_0$'TipNr', data_diff_from_step_0$'KII', main = paste("Difference in KII values from Step 0"), xlab = 'Tip Number', ylab= "Delta KII SIF Value", pch= 20, col= muted(colours[data$'FractureNum'], l = ((data$'Step'+1)/num_of_steps)*100))
  plot(data_diff_from_step_0$'TipNr', data_diff_from_step_0$'KIII', main = paste("Difference in KIII values from Step 0"), xlab = 'Tip Number', ylab= "Delta KIII SIF Value", pch= 20, col= muted(colours[data$'FractureNum'], l = ((data$'Step'+1)/num_of_steps)*100))
  plot(data_diff_from_step_0$'TipNr', data_diff_from_step_0$'G', main = paste("Difference in G values from Step 0"), xlab = 'Tip Number', ylab= "Delta G Value", pch= 20, col= muted(colours[data$'FractureNum'], l = ((data$'Step'+1)/num_of_steps)*100))

  # Calculating difference from previous step.
  
  # Creating two data frames of equal dimensions, one with steps 0-(max-1) and one with steps 1-max.
  subtract_left <- data[!(data$'Step'== 0),]
  subtract_right <- data[!(data$'Step'== max(data$'Step')),]
  
  # Setting fracture, tip, and step numbers to 0 so original values will be preserved after subtraction.
  subtract_right$'FractureNum' <- 0
  subtract_right$'TipNr' <- 0
  subtract_right$'Step' <- 0

  # Subtracting two data frames to find difference.
  data_diff_from_prev <- subtract_left[,-2] - subtract_right[,-2]
  
  # Creates 2x2 matrix for four difference from previous step figures drawn below (Type I, II, III, and G values).
  par(mfrow=c(2,2))
  
  # Difference in KI, KII, KIII, and G values for each step from previous step at each tip graphs.
  plot(data_diff_from_prev$'TipNr', data_diff_from_prev$'KI', main = paste("Difference in KI values from \nPrevious Step"), xlab = 'Tip Number', ylab= "Delta KI SIF Value", pch= 20, col= muted(colours[data$'FractureNum'], l = ((data$'Step'+1)/num_of_steps)*100))
  plot(data_diff_from_prev$'TipNr', data_diff_from_prev$'KII', main = paste("Difference in KII values from \nPrevious Step"), xlab = 'Tip Number', ylab= "Delta KII SIF Value", pch= 20, col= muted(colours[data$'FractureNum'], l = ((data$'Step'+1)/num_of_steps)*100))
  plot(data_diff_from_prev$'TipNr', data_diff_from_prev$'KIII', main = paste("Difference in KIII values from \nPrevious Step"), xlab = 'Tip Number', ylab= "Delta KIII SIF Value", pch= 20, col= muted(colours[data$'FractureNum'], l = ((data$'Step'+1)/num_of_steps)*100))
  plot(data_diff_from_prev$'TipNr', data_diff_from_prev$'G', main = paste("Difference in G values from \nPrevious Step"), xlab = 'Tip Number', ylab= "Delta G Value", pch= 20, col= muted(colours[data$'FractureNum'], l = ((data$'Step'+1)/num_of_steps)*100))
  
  # Stops writing to pdf.
  dev.off()
  }