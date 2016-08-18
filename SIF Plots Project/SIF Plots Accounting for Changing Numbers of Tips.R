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
  
  # Making steps consecutive and starting at zero.
  uniqueSteps <- unique(data$Step)
  data$'Step' <- sapply(data$'Step', function(x) {match(x, uniqueSteps) - 1})
  
  # Creates identifier for each fracture and tip number. 
  data$'FractureTipID' <- paste(data$'FractureNum', data$'TipNr')
  
  # Calculating total number of fractures and steps.
  num_of_fractures <- length(unique(data$'FractureNum'))
  num_of_steps <- length(unique(data$'Step'))
  
  # Calculating max number of tips per fracture at final step.
  max_tips_per_fracture <- data[data$Step == (num_of_steps - 1),]
  max_tips_per_fracture <- aggregate(max_tips_per_fracture[,3], by = list(FractureNum = max_tips_per_fracture$'FractureNum'), FUN = "max")
  colnames(max_tips_per_fracture) <- c('FractureNum', 'MaxTipNr')

  # Calculating maximum radius of fracture at final step.
  fractureRadii <- data[data$Step == (num_of_steps - 1) & data$TipNr < 2,]
  fractureRadii <- aggregate(fractureRadii[,c(-2, -14)], by = list(FractureNum = fractureRadii$'FractureNum'), FUN = "diff")
  fractureRadii$'HalfDistance' <- sqrt(fractureRadii$'TipX'^2 + fractureRadii$'TipY'^2 + fractureRadii$'TipZ'^2) / 2
  fractureRadii$'Angle'[fractureRadii$'FractureNum' %in% max_tips_per_fracture$'FractureNum'] <- (2 * pi)/ max_tips_per_fracture$'MaxTipNr'
  fractureRadii$'Radius' <- fractureRadii$'HalfDistance' / sin(fractureRadii$'Angle')
  
  # Calculating max K value based on formula from Nejati's thesis.
  fractureRadii$'MaxKValue' <- 2 * 10^11 * sqrt(fractureRadii$'Radius' / pi)
  
  # Removing all columns except FractureNum and Max K Value.
  fractureRadii <- fractureRadii[ ,c(1,17)]
  
  # Eliminate rows with K or G values above threshold value of 1e+11 (100 GPa, 10x Young's Modulus) and KI values below zero.
  maxKvalue <- 1e+11
  data <- subset(data, abs(data$'KI') < maxKvalue & abs(data$'KII') < maxKvalue & abs(data$'KIII') < maxKvalue & abs(data$'G' < maxKvalue))

  # Preparing symbols and their sizes for plot.
  pchvalue <- c(20, 3, 2, 17, 8, 15, 18, 1)
  pchvalue <- rep(pchvalue, length.out = num_of_fractures)
  
  # Making points from the first step blue and the last step red.
  colours <- rep('black', num_of_steps)
  colours[1] <- 'blue'
  colours[num_of_steps] <- 'red'
  
  # Saves plots as pdf with title as original file name.
  filename_no_ext <- substr(filename, 1, nchar(filename)-4)
  pdf_name <- paste(filename_no_ext, '_plots.pdf')
  pdf(file = pdf_name, title = pdf_name)
  plot_name <- unlist(strsplit(pdf_name, '/'))
  plot_name <- plot_name[length(plot_name)]
  
  # Creates 2x2 matrix for four raw data figures drawn below (Type I, II, III, and G values).
  par(mfrow=c(2,2), oma=c(0,0,3,0))
  
  # Raw data graphs.
  plot(data$'TipNr', data$'KI', main = paste("KI"), xlab = 'Tip Number', ylab= "KI SIF Value", pch= pchvalue[data$'FractureNum'], col = colours[data$Step + 1], cex = (data$'Step' + 4)/(num_of_steps+1))
  plot(data$'TipNr', data$'KII', main = paste("KII"), xlab = 'Tip Number', ylab= "KII SIF Value", pch= pchvalue[data$'FractureNum'], col = colours[data$Step + 1], cex = (data$'Step' + 4)/(num_of_steps+1))
  plot(data$'TipNr', data$'KIII', main = paste("KIII"), xlab = 'Tip Number', ylab= "KIII SIF Value", pch= pchvalue[data$'FractureNum'], col = colours[data$Step + 1], cex = (data$'Step' + 4)/(num_of_steps+1))
  plot(data$'TipNr', data$'G', main = paste("G"), xlab = 'Tip Number', ylab= "G Value", pch= pchvalue[data$'FractureNum'], col = colours[data$Step + 1], cex = (data$'Step' + 4)/(num_of_steps+1))
  mtext(plot_name, adj=0.5, side=3, outer=TRUE)

  # Calculating data statistics (mean, min, max).
  data_means <- aggregate(data[,c(-2, -14)], by = list(TipNr = data$'TipNr', FractureNum = data$'FractureNum'), FUN = "mean")
  data_mins <- aggregate(data[,c(-2, -14)], by = list(TipNr = data$'TipNr', FractureNum = data$'FractureNum'), FUN = "min")
  data_maxs <- aggregate(data[,c(-2,-14)], by = list(TipNr = data$'TipNr', FractureNum = data$'FractureNum'), FUN = "max")

  # Creates 2x2 matrix for four mean figures drawn below (Type I, II, III, and G values).
  par(mfrow=c(2,2))

  # Mean graphs.
  plot(data_means$'TipNr', data_means$'KI', main = paste("KI Mean"), xlab = 'Tip Number', ylab= "KI Avg SIF Value", pch= pchvalue[data_means$'FractureNum'])
  plot(data_means$'TipNr', data_means$'KII', main = paste("KII Mean"), xlab = 'Tip Number', ylab= "KII Avg SIF Value", pch= pchvalue[data_means$'FractureNum'])
  plot(data_means$'TipNr', data_means$'KIII', main = paste("KIII Mean"), xlab = 'Tip Number', ylab= "KIII Avg SIF Value", pch= pchvalue[data_means$'FractureNum'])
  plot(data_means$'TipNr', data_means$'G', main = paste("G Mean"), xlab = 'Tip Number', ylab= "Avg G Value", pch= pchvalue[data_means$'FractureNum'])

  # Creates 2x2 matrix for four min figures drawn below (Type I, II, III, and G values).
  par(mfrow=c(2,2))

  # Min graphs.
  plot(data_mins$'TipNr', data_mins$'KI', main = paste("KI Minimum Value"), xlab = 'Tip Number', ylab= "KI Min SIF Value", pch= pchvalue[data_mins$'FractureNum'])
  plot(data_mins$'TipNr', data_mins$'KII', main = paste("KII Minimum Value"), xlab = 'Tip Number', ylab= "KII Min SIF Value", pch= pchvalue[data_mins$'FractureNum'])
  plot(data_mins$'TipNr', data_mins$'KIII', main = paste("KIII Minimum Value"), xlab = 'Tip Number', ylab= "KIII Min SIF Value", pch= pchvalue[data_mins$'FractureNum'])
  plot(data_mins$'TipNr', data_mins$'G', main = paste("G Minimum Value"), xlab = 'Tip Number', ylab= "Min G Value", pch= pchvalue[data_mins$'FractureNum'])

  # Creates 2x2 matrix for four max figures drawn below (Type I, II, III, and G values).
  par(mfrow=c(2,2))

  # Max graphs.
  plot(data_maxs$'TipNr', data_maxs$'KI', main = paste("KI Max Value"), xlab = 'Tip Number', ylab= "KI Max SIF Value", pch= pchvalue[data_maxs$'FractureNum'])
  plot(data_maxs$'TipNr', data_maxs$'KII', main = paste("KII Max Value"), xlab = 'Tip Number', ylab= "KII Max SIF Value", pch= pchvalue[data_maxs$'FractureNum'])
  plot(data_maxs$'TipNr', data_maxs$'KIII', main = paste("KIII Max Value"), xlab = 'Tip Number', ylab= "KIII Max SIF Value", pch= pchvalue[data_maxs$'FractureNum'])
  plot(data_maxs$'TipNr', data_maxs$'G', main = paste("Max G Value"), xlab = 'Tip Number', ylab= "Max G Value", pch= pchvalue[data_maxs$'FractureNum'])

  
  # Calculating difference from first step at which fracture + tip appear (generally step 0).
  data_diff_from_step_0 <- within(data, KI <- ave(KI, list(FractureTipID), FUN=function(x) x-x[1]))
  data_diff_from_step_0 <- within(data_diff_from_step_0, KII <- ave(KII, list(FractureTipID), FUN=function(x) x-x[1]))
  data_diff_from_step_0 <- within(data_diff_from_step_0, KIII <- ave(KIII, list(FractureTipID), FUN=function(x) x-x[1]))
  data_diff_from_step_0 <- within(data_diff_from_step_0, G <- ave(G, list(FractureTipID), FUN=function(x) x-x[1]))

  # Creates 2x2 matrix for four difference from step 0 figures drawn below (Type I, II, III, and G values).
  par(mfrow=c(2,2))

  # Difference in KI, KII, KIII, and G values for each step from step 0 at each tip graphs.
  plot(data_diff_from_step_0$'TipNr', data_diff_from_step_0$'KI', main = paste("Difference in KI values from Step 0"), xlab = 'Tip Number', ylab= "Delta KI SIF Value", pch= pchvalue[data_diff_from_step_0$'FractureNum'], col = colours[data_diff_from_step_0$Step + 1], cex = (data_diff_from_step_0$'Step' + 4)/(num_of_steps+1))
  plot(data_diff_from_step_0$'TipNr', data_diff_from_step_0$'KII', main = paste("Difference in KII values from Step 0"), xlab = 'Tip Number', ylab= "Delta KII SIF Value", pch= pchvalue[data_diff_from_step_0$'FractureNum'], col = colours[data_diff_from_step_0$Step + 1], cex = (data_diff_from_step_0$'Step' + 4)/(num_of_steps+1))
  plot(data_diff_from_step_0$'TipNr', data_diff_from_step_0$'KIII', main = paste("Difference in KIII values from Step 0"), xlab = 'Tip Number', ylab= "Delta KIII SIF Value", pch= pchvalue[data_diff_from_step_0$'FractureNum'], col = colours[data_diff_from_step_0$Step + 1], cex = (data_diff_from_step_0$'Step' + 4)/(num_of_steps+1))
  plot(data_diff_from_step_0$'TipNr', data_diff_from_step_0$'G', main = paste("Difference in G values from Step 0"), xlab = 'Tip Number', ylab= "Delta G Value", pch= pchvalue[data_diff_from_step_0$'FractureNum'], col = colours[data_diff_from_step_0$Step + 1], cex = (data_diff_from_step_0$'Step' + 4)/(num_of_steps+1))

  
  # Calculating difference from previous step.
  data_diff_from_prev_step <- within(data, KI <- ave(KI, list(FractureTipID), FUN=function(x) c(0, diff(x))))
  data_diff_from_prev_step<- within(data_diff_from_prev_step, KII <- ave(KII, list(FractureTipID), FUN=function(x) c(0, diff(x))))
  data_diff_from_prev_step<- within(data_diff_from_prev_step, KIII <- ave(KIII, list(FractureTipID), FUN=function(x) c(0, diff(x))))
  data_diff_from_prev_step <- within(data_diff_from_prev_step, G <- ave(G, list(FractureTipID), FUN=function(x) c(0, diff(x))))
  
  # Creates 2x2 matrix for four difference from previous step figures drawn below (Type I, II, III, and G values).
  par(mfrow=c(2,2))

  # Difference in KI, KII, KIII, and G values for each step from previous step at each tip graphs.
  plot(data_diff_from_prev_step$'TipNr', data_diff_from_prev_step$'KI', main = paste("Difference in KI values from \nPrevious Step"), xlab = 'Tip Number', ylab= "Delta KI SIF Value", pch= pchvalue[data_diff_from_prev_step$'FractureNum'], col = colours[data_diff_from_prev_step$Step + 1], cex = (data_diff_from_prev_step$'Step' + 4)/(num_of_steps+1))
  plot(data_diff_from_prev_step$'TipNr', data_diff_from_prev_step$'KII', main = paste("Difference in KII values from \nPrevious Step"), xlab = 'Tip Number', ylab= "Delta KII SIF Value", pch= pchvalue[data_diff_from_prev_step$'FractureNum'], col = colours[data_diff_from_prev_step$Step + 1], cex = (data_diff_from_prev_step$'Step' + 4)/(num_of_steps+1))
  plot(data_diff_from_prev_step$'TipNr', data_diff_from_prev_step$'KIII', main = paste("Difference in KIII values from \nPrevious Step"), xlab = 'Tip Number', ylab= "Delta KIII SIF Value", pch= pchvalue[data_diff_from_prev_step$'FractureNum'], col = colours[data_diff_from_prev_step$Step + 1], cex = (data_diff_from_prev_step$'Step' + 4)/(num_of_steps+1))
  plot(data_diff_from_prev_step$'TipNr', data_diff_from_prev_step$'G', main = paste("Difference in G values from \nPrevious Step"), xlab = 'Tip Number', ylab= "Delta G Value", pch= pchvalue[data_diff_from_prev_step$'FractureNum'], col = colours[data_diff_from_prev_step$Step + 1], cex = (data_diff_from_prev_step$'Step' + 4)/(num_of_steps+1))
  
  # Stops writing to pdf.
  dev.off()
  }