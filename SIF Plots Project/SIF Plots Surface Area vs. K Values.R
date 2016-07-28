SIF_plot <- function(filename, SA_filename){
  require(scales)
  
  # Converts raw .txt files to dataframes.
  all_data <- read.table(filename, fill = TRUE, header = TRUE)
  SA_data <- read.table(SA_filename, fill = TRUE)
  
  # Naming columns of surface area dataframe.
  colnames(SA_data) <- c('Step', 'FractureName', 'SurfaceArea')
  
  # Removes rows with header names from raw sif values dataframe.
  header_rows <- which(apply(all_data, 1, function(x) any(grepl("Step", x))))
  data <- all_data[-header_rows, ]
  
  # Adding surface area values to dataframe.
  data <- merge(data, SA_data, sort = FALSE)
  
  # Creating column with fracture number as integers.
  data$'FractureNum' <- as.numeric(gsub("[^0-9]", "", data$'FractureName'))

  # Converting factor values in data frame to numeric values.
  print(tail(data, 120))
  data[,-2] <- lapply(data[,-2], function(x) as.numeric(as.character(x)))
  
  # Making steps consecutive and starting at zero.
  uniqueSteps <- unique(data$Step)
  data$'Step' <- sapply(data$'Step', function(x) {match(x, uniqueSteps) - 1})
  
  # Creates identifier for each fracture and tip number. 
  data$'FractureTipID' <- paste(data$'FractureNum', data$'TipNr')
  
  # Calculating total number of fractures and steps.
  num_of_fractures <- length(unique(data$'FractureNum'))
  num_of_steps <- length(unique(data$'Step'))
  
  # Eliminate rows with K or G values above threshold value of 1e+11 (100 GPa, 10x Young's Modulus) and KI values below zero.
  maxKvalue <- 1e+11
  data <- subset(data, abs(data$'KI') < maxKvalue & abs(data$'KII') < maxKvalue & abs(data$'KIII') < maxKvalue & abs(data$'G' < maxKvalue))
  data <- subset(data, data$'KI' >= 0)

  # Preparing symbols for plot.
  pchvalue <- c(20, 3, 2, 17, 8, 15, 18, 1)
  pchvalue <- rep(pchvalue, length.out = num_of_fractures)
  
  # Making points from the first step blue and the last step red.
  colours <- rep('black', num_of_steps)
  colours[1] <- 'blue'
  colours[num_of_steps] <- 'red'
  
  # Finding maximum surface area for each fracture and adding it to dataframe.
  SA_maxes <- aggregate(data[,c(-2,-15)], by = list(FractureNum = data$'FractureNum'), FUN = "max")
  SA_maxes <- SA_maxes[ ,c(13, 14)]
  colnames(SA_maxes) <- c('MaxSurfaceArea', 'FractureNum')
  data <- merge(data, SA_maxes, sort = FALSE)

  # Saves plots as pdf with title as original file name.
  filename_no_ext <- substr(filename, 1, nchar(filename)-4)
  pdf_name <- paste(filename_no_ext, '_plots.pdf')
  pdf(file = pdf_name, title = pdf_name)
  plot_name <- unlist(strsplit(pdf_name, '/'))
  plot_name <- plot_name[length(plot_name)]
  
  # Creates 2x2 matrix for four raw data figures drawn below (Type I, II, III, and G values).
  par(mfrow=c(2,2), oma=c(0,0,3,0))
  
  # Raw data graphs.
  plot(data$'SurfaceArea', data$'KI', main = paste("KI"), xlab = 'Surface Area', ylab= "KI SIF Value", pch= pchvalue[data$'FractureNum'], col = colours[data$Step + 1], cex = data$'SurfaceArea'/data$'MaxSurfaceArea')
  plot(data$'SurfaceArea', data$'KII', main = paste("KII"), xlab = 'Surface Area', ylab= "KII SIF Value", pch= pchvalue[data$'FractureNum'], col = colours[data$Step + 1], cex = data$'SurfaceArea'/data$'MaxSurfaceArea')
  plot(data$'SurfaceArea', data$'KIII', main = paste("KIII"), xlab = 'Surface Area', ylab= "KIII SIF Value", pch= pchvalue[data$'FractureNum'], col = colours[data$Step + 1], cex = data$'SurfaceArea'/data$'MaxSurfaceArea')
  plot(data$'SurfaceArea', data$'G', main = paste("G"), xlab = 'Surface Area', ylab= "G Value", pch= pchvalue[data$'FractureNum'], col = colours[data$Step + 1], cex = data$'SurfaceArea'/data$'MaxSurfaceArea')
  mtext(plot_name, adj=0.5, side=3, outer=TRUE)
  
  # Calculating data statistics (mean, min, max).
  data_means <- aggregate(data[,c(-3, -15)], by = list(TipNr = data$'TipNr', FractureNum = data$'FractureNum'), FUN = "mean")
  data_mins <- aggregate(data[,c(-3, -15)], by = list(TipNr = data$'TipNr', FractureNum = data$'FractureNum'), FUN = "min")
  data_maxs <- aggregate(data[,c(-3,-15)], by = list(TipNr = data$'TipNr', FractureNum = data$'FractureNum'), FUN = "max")
  
  # Creates 2x2 matrix for four mean figures drawn below (Type I, II, III, and G values).
  par(mfrow=c(2,2))

  # Mean graphs.
  plot(data_means$'SurfaceArea', data_means$'KI', main = paste("KI Mean"), xlab = 'Surface Area', ylab= "KI Avg SIF Value", pch= pchvalue[data_means$'FractureNum'])
  plot(data_means$'SurfaceArea', data_means$'KII', main = paste("KII Mean"), xlab = 'Surface Area', ylab= "KII Avg SIF Value", pch= pchvalue[data_means$'FractureNum'])
  plot(data_means$'SurfaceArea', data_means$'KIII', main = paste("KIII Mean"), xlab = 'Surface Area', ylab= "KIII Avg SIF Value", pch= pchvalue[data_means$'FractureNum'])
  plot(data_means$'SurfaceArea', data_means$'G', main = paste("G Mean"), xlab = 'Surface Area', ylab= "Avg G Value", pch= pchvalue[data_means$'FractureNum'])

  # Creates 2x2 matrix for four min figures drawn below (Type I, II, III, and G values).
  par(mfrow=c(2,2))

  # Min graphs.
  plot(data_mins$'SurfaceArea', data_mins$'KI', main = paste("KI Minimum Value"), xlab = 'Surface Area', ylab= "KI Min SIF Value", pch= pchvalue[data_mins$'FractureNum'])
  plot(data_mins$'SurfaceArea', data_mins$'KII', main = paste("KII Minimum Value"), xlab = 'Surface Area', ylab= "KII Min SIF Value", pch= pchvalue[data_mins$'FractureNum'])
  plot(data_mins$'SurfaceArea', data_mins$'KIII', main = paste("KIII Minimum Value"), xlab = 'Surface Area', ylab= "KIII Min SIF Value", pch= pchvalue[data_mins$'FractureNum'])
  plot(data_mins$'SurfaceArea', data_mins$'G', main = paste("G Minimum Value"), xlab = 'Surface Area', ylab= "Min G Value", pch= pchvalue[data_mins$'FractureNum'])
  
  # Creates 2x2 matrix for four max figures drawn below (Type I, II, III, and G values).
  par(mfrow=c(2,2))

  # Max graphs.
  plot(data_maxs$'SurfaceArea', data_maxs$'KI', main = paste("KI Max Value"), xlab = 'Surface Area', ylab= "KI Max SIF Value", pch= pchvalue[data_maxs$'FractureNum'])
  plot(data_maxs$'SurfaceArea', data_maxs$'KII', main = paste("KII Max Value"), xlab = 'Surface Area', ylab= "KII Max SIF Value", pch= pchvalue[data_maxs$'FractureNum'])
  plot(data_maxs$'SurfaceArea', data_maxs$'KIII', main = paste("KIII Max Value"), xlab = 'Surface Area', ylab= "KIII Max SIF Value", pch= pchvalue[data_maxs$'FractureNum'])
  plot(data_maxs$'SurfaceArea', data_maxs$'G', main = paste("Max G Value"), xlab = 'Surface Area', ylab= "Max G Value", pch= pchvalue[data_maxs$'FractureNum'])

  # # Calculating difference from first step at which fracture + tip appear (generally step 0).
  # data_diff_from_step_0 <- within(data, KI <- ave(KI, list(FractureTipID), FUN=function(x) x-x[1]))
  # data_diff_from_step_0 <- within(data_diff_from_step_0, KII <- ave(KII, list(FractureTipID), FUN=function(x) x-x[1]))
  # data_diff_from_step_0 <- within(data_diff_from_step_0, KIII <- ave(KIII, list(FractureTipID), FUN=function(x) x-x[1]))
  # data_diff_from_step_0 <- within(data_diff_from_step_0, G <- ave(G, list(FractureTipID), FUN=function(x) x-x[1]))
  # 
  # # Creates 2x2 matrix for four difference from step 0 figures drawn below (Type I, II, III, and G values).
  # par(mfrow=c(2,2))
  # 
  # # Difference in KI, KII, KIII, and G values for each step from step 0 at each tip graphs.
  # plot(data_diff_from_step_0$'TipNr', data_diff_from_step_0$'KI', main = paste("Difference in KI values from Step 0"), xlab = 'Tip Number', ylab= "Delta KI SIF Value", pch= pchvalue[data_diff_from_step_0$'FractureNum'], col = colours[data_diff_from_step_0$Step + 1], cex = data_diff_from_step_0$'SurfaceArea'/data_diff_from_step_0$'MaxSurfaceArea')
  # plot(data_diff_from_step_0$'TipNr', data_diff_from_step_0$'KII', main = paste("Difference in KII values from Step 0"), xlab = 'Tip Number', ylab= "Delta KII SIF Value", pch= pchvalue[data_diff_from_step_0$'FractureNum'], col = colours[data_diff_from_step_0$Step + 1], cex = data_diff_from_step_0$'SurfaceArea'/data_diff_from_step_0$'MaxSurfaceArea')
  # plot(data_diff_from_step_0$'TipNr', data_diff_from_step_0$'KIII', main = paste("Difference in KIII values from Step 0"), xlab = 'Tip Number', ylab= "Delta KIII SIF Value", pch= pchvalue[data_diff_from_step_0$'FractureNum'], col = colours[data_diff_from_step_0$Step + 1], cex = data_diff_from_step_0$'SurfaceArea'/data_diff_from_step_0$'MaxSurfaceArea')
  # plot(data_diff_from_step_0$'TipNr', data_diff_from_step_0$'G', main = paste("Difference in G values from Step 0"), xlab = 'Tip Number', ylab= "Delta G Value", pch= pchvalue[data_diff_from_step_0$'FractureNum'], col = colours[data_diff_from_step_0$Step + 1], cex = data_diff_from_step_0$'SurfaceArea'/data_diff_from_step_0$'MaxSurfaceArea')
  # 
  # 
  # # Calculating difference from previous step.
  # data_diff_from_prev_step <- within(data, KI <- ave(KI, list(FractureTipID), FUN=function(x) c(0, diff(x))))
  # data_diff_from_prev_step<- within(data_diff_from_prev_step, KII <- ave(KII, list(FractureTipID), FUN=function(x) c(0, diff(x))))
  # data_diff_from_prev_step<- within(data_diff_from_prev_step, KIII <- ave(KIII, list(FractureTipID), FUN=function(x) c(0, diff(x))))
  # data_diff_from_prev_step <- within(data_diff_from_prev_step, G <- ave(G, list(FractureTipID), FUN=function(x) c(0, diff(x))))
  # 
  # # Creates 2x2 matrix for four difference from previous step figures drawn below (Type I, II, III, and G values).
  # par(mfrow=c(2,2))
  # 
  # # Difference in KI, KII, KIII, and G values for each step from previous step at each tip graphs.
  # plot(data_diff_from_prev_step$'TipNr', data_diff_from_prev_step$'KI', main = paste("Difference in KI values from \nPrevious Step"), xlab = 'Tip Number', ylab= "Delta KI SIF Value", pch= pchvalue[data_diff_from_prev_step$'FractureNum'], col = colours[data_diff_from_prev_step$Step + 1], cex = data_diff_from_prev_step$'SurfaceArea'/data_diff_from_prev_step$'MaxSurfaceArea')
  # plot(data_diff_from_prev_step$'TipNr', data_diff_from_prev_step$'KII', main = paste("Difference in KII values from \nPrevious Step"), xlab = 'Tip Number', ylab= "Delta KII SIF Value", pch= pchvalue[data_diff_from_prev_step$'FractureNum'], col = colours[data_diff_from_prev_step$Step + 1], cex = data_diff_from_prev_step$'SurfaceArea'/data_diff_from_prev_step$'MaxSurfaceArea')
  # plot(data_diff_from_prev_step$'TipNr', data_diff_from_prev_step$'KIII', main = paste("Difference in KIII values from \nPrevious Step"), xlab = 'Tip Number', ylab= "Delta KIII SIF Value", pch= pchvalue[data_diff_from_prev_step$'FractureNum'], col = colours[data_diff_from_prev_step$Step + 1], cex = data_diff_from_prev_step$'SurfaceArea'/data_diff_from_prev_step$'MaxSurfaceArea')
  # plot(data_diff_from_prev_step$'TipNr', data_diff_from_prev_step$'G', main = paste("Difference in G values from \nPrevious Step"), xlab = 'Tip Number', ylab= "Delta G Value", pch= pchvalue[data_diff_from_prev_step$'FractureNum'], col = colours[data_diff_from_prev_step$Step + 1], cex = data_diff_from_prev_step$'SurfaceArea'/data_diff_from_prev_step$'MaxSurfaceArea')
  # 
  # Stops writing to pdf.
  dev.off()
  }