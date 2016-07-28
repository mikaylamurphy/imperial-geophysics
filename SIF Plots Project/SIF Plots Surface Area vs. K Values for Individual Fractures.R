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
  pdf(file = pdf_name, title = pdf_name, width  = 8, height = 11.5)
  plot_name <- unlist(strsplit(pdf_name, '/'))
  plot_name <- plot_name[length(plot_name)]


  fractures <- unique(data$'FractureNum')
  
  # Creates matrix for raw data KI figures drawn below and margins for main title.
  par(mfrow=c(ceiling(length(fractures) / 2), 2), oma=c(0,0,3,0))
  
  # Iterates through fractures by drawing a KI vs. surface area plot for each fracture.
  for (fracture in fractures){
    data_subset <- subset(data, data$'FractureNum' == fracture)
    plot(data_subset$'SurfaceArea', data_subset$'KI', main = paste("Fracture", fracture, "KI"), xlab = 'Surface Area', ylab= "KI SIF Value", pch= 20, col = colours[data_subset$Step + 1], cex = data_subset$'SurfaceArea'/data_subset$'MaxSurfaceArea')
  }
  
  # Creates matrix for raw data KII figures drawn below.
  par(mfrow=c(ceiling(length(fractures) / 2), 2))
  
  # Iterates through fractures by drawing a KII vs. surface area plot for each fracture.
  for (fracture in fractures){
    data_subset <- subset(data, data$'FractureNum' == fracture)
    plot(data_subset$'SurfaceArea', data_subset$'KII', main = paste("Fracture", fracture, "KII"), xlab = 'Surface Area', ylab= "KII SIF Value", pch= 20, col = colours[data_subset$Step + 1], cex = data_subset$'SurfaceArea'/data_subset$'MaxSurfaceArea')
  }
  
  # Creates matrix for raw data KIII figures drawn below.
  par(mfrow=c(ceiling(length(fractures) / 2), 2))
  
  # Iterates through fractures by drawing a KIII vs. surface area plot for each fracture.
  for (fracture in fractures){
    data_subset <- subset(data, data$'FractureNum' == fracture)
    plot(data_subset$'SurfaceArea', data_subset$'KIII', main = paste("Fracture", fracture, "KIII"), xlab = 'Surface Area', ylab= "KIII SIF Value", pch= 20, col = colours[data_subset$Step + 1], cex = data_subset$'SurfaceArea'/data_subset$'MaxSurfaceArea')
  }
  
  # Creates matrix for raw data G figures drawn below.
  par(mfrow=c(ceiling(length(fractures) / 2), 2))
  
  # Iterates through fractures by drawing a G vs. surface area plot for each fracture.
  for (fracture in fractures){
    data_subset <- subset(data, data$'FractureNum' == fracture)
    plot(data_subset$'SurfaceArea', data_subset$'G', main = paste("Fracture", fracture, "G"), xlab = 'Surface Area', ylab= "G Value", pch= 20, col = colours[data_subset$Step + 1], cex = data_subset$'SurfaceArea'/data_subset$'MaxSurfaceArea')
  }
  
  # Adds main title to document.
  mtext(plot_name, adj=0.5, side=3, outer=TRUE)
  
  # Calculating data statistics (mean, min, max).
  data_means <- aggregate(data[,c(-3, -15)], by = list(TipNr = data$'TipNr', FractureNum = data$'FractureNum'), FUN = "mean")
  data_mins <- aggregate(data[,c(-3, -15)], by = list(TipNr = data$'TipNr', FractureNum = data$'FractureNum'), FUN = "min")
  data_maxs <- aggregate(data[,c(-3,-15)], by = list(TipNr = data$'TipNr', FractureNum = data$'FractureNum'), FUN = "max")
  
  # Creates matrix for mean KI figures drawn below.
  par(mfrow=c(ceiling(length(fractures) / 2), 2))

  # Iterates through fractures by drawing an avg KI vs. avg surface area plot for each fracture.
  for (fracture in fractures){
    data_subset <- subset(data_means, data_means$'FractureNum' == fracture)
    plot(data_subset$'SurfaceArea', data_subset$'KI', main = paste("Fracture", fracture, "KI Mean"), xlab = 'Avg Surface Area', ylab= "KI Avg SIF Value", pch= 20)
  }
  
  # Creates matrix for mean KII figures drawn below.
  par(mfrow=c(ceiling(length(fractures) / 2), 2))
  
  # Iterates through fractures by drawing an avg KII vs. avg surface area plot for each fracture.
  for (fracture in fractures){
    data_subset <- subset(data_means, data_means$'FractureNum' == fracture)
    plot(data_subset$'SurfaceArea', data_subset$'KII', main = paste("Fracture", fracture, "KII Mean"), xlab = 'Avg Surface Area', ylab= "KII Avg SIF Value", pch= 20)
  }
  
  # Creates matrix for mean KIII figures drawn below.
  par(mfrow=c(ceiling(length(fractures) / 2), 2))
  
  # Iterates through fractures by drawing an avg KIII vs. avg surface area plot for each fracture.
  for (fracture in fractures){
    data_subset <- subset(data_means, data_means$'FractureNum' == fracture)
    plot(data_subset$'SurfaceArea', data_subset$'KIII', main = paste("Fracture", fracture, "KIII Mean"), xlab = 'Avg Surface Area', ylab= "KIII Avg SIF Value", pch= 20)
  }
  
  # Creates matrix for mean G value figures drawn below.
  par(mfrow=c(ceiling(length(fractures) / 2), 2))
  
  # Iterates through fractures by drawing an avg G value vs. avg surface area plot for each fracture.
  for (fracture in fractures){
    data_subset <- subset(data_means, data_means$'FractureNum' == fracture)
    plot(data_subset$'SurfaceArea', data_subset$'G', main = paste("Fracture", fracture, "G Mean"), xlab = 'Avg Surface Area', ylab= "Avg G Value", pch= 20)
  }
  
  # Creates matrix for min KI figures drawn below.
  par(mfrow=c(ceiling(length(fractures) / 2), 2))
  
  # Iterates through fractures by drawing a min KI vs. min surface area plot for each fracture.
  for (fracture in fractures){
    data_subset <- subset(data_mins, data_mins$'FractureNum' == fracture)
    plot(data_subset$'SurfaceArea', data_subset$'KI', main = paste("Fracture", fracture, "KI Minimum Value"), xlab = 'Min Surface Area', ylab= "KI Min SIF Value", pch= 20)
  }

  # Creates matrix for min KII figures drawn below.
  par(mfrow=c(ceiling(length(fractures) / 2), 2))
  
  # Iterates through fractures by drawing a min KII vs. min surface area plot for each fracture.
  for (fracture in fractures){
    data_subset <- subset(data_mins, data_mins$'FractureNum' == fracture)
    plot(data_subset$'SurfaceArea', data_subset$'KII', main = paste("Fracture", fracture, "KII Minimum Value"), xlab = 'Min Surface Area', ylab= "KII Min SIF Value", pch= 20)
  }
  
  # Creates matrix for min KIII figures drawn below.
  par(mfrow=c(ceiling(length(fractures) / 2), 2))
  
  # Iterates through fractures by drawing a min KIII vs. min surface area plot for each fracture.
  for (fracture in fractures){
    data_subset <- subset(data_mins, data_mins$'FractureNum' == fracture)
    plot(data_subset$'SurfaceArea', data_subset$'KIII', main = paste("Fracture", fracture, "KIII Minimum Value"), xlab = 'Min Surface Area', ylab= "KIII Min SIF Value", pch= 20)
  }
  
  # Creates matrix for min G value figures drawn below.
  par(mfrow=c(ceiling(length(fractures) / 2), 2))
  
  # Iterates through fractures by drawing a min G value vs. min surface area plot for each fracture.
  for (fracture in fractures){
    data_subset <- subset(data_mins, data_mins$'FractureNum' == fracture)
    plot(data_subset$'SurfaceArea', data_subset$'G', main = paste("Fracture", fracture, "Minimum G Value"), xlab = 'Min Surface Area', ylab= "Min G Value", pch= 20)
  }
 
  # Creates matrix for max KI figures drawn below.
  par(mfrow=c(ceiling(length(fractures) / 2), 2))
  
  # Iterates through fractures by drawing a max KI vs. max surface area plot for each fracture.
  for (fracture in fractures){
    data_subset <- subset(data_maxs, data_maxs$'FractureNum' == fracture)
    plot(data_subset$'SurfaceArea', data_subset$'KI', main = paste("Fracture", fracture, "KI Maximum Value"), xlab = 'Max Surface Area', ylab= "KI Max SIF Value", pch= 20)
  }
  
  # Creates matrix for max KII figures drawn below.
  par(mfrow=c(ceiling(length(fractures) / 2), 2))
  
  # Iterates through fractures by drawing a max KII vs. max surface area plot for each fracture.
  for (fracture in fractures){
    data_subset <- subset(data_maxs, data_maxs$'FractureNum' == fracture)
    plot(data_subset$'SurfaceArea', data_subset$'KII', main = paste("Fracture", fracture, "KII Maximum Value"), xlab = 'Max Surface Area', ylab= "KII Max SIF Value", pch= 20)
  }
  
  # Creates matrix for max KIII figures drawn below.
  par(mfrow=c(ceiling(length(fractures) / 2), 2))
  
  # Iterates through fractures by drawing a max KI vs. max surface area plot for each fracture.
  for (fracture in fractures){
    data_subset <- subset(data_maxs, data_maxs$'FractureNum' == fracture)
    plot(data_subset$'SurfaceArea', data_subset$'KIII', main = paste("Fracture", fracture, "KIII Maximum Value"), xlab = 'Max Surface Area', ylab= "KIII Max SIF Value", pch= 20)
  }
  
  # Creates matrix for max G value figures drawn below.
  par(mfrow=c(ceiling(length(fractures) / 2), 2))
  
  # Iterates through fractures by drawing a max G value vs. max surface area plot for each fracture.
  for (fracture in fractures){
    data_subset <- subset(data_maxs, data_maxs$'FractureNum' == fracture)
    plot(data_subset$'SurfaceArea', data_subset$'G', main = paste("Fracture", fracture, "Maximum G Value"), xlab = 'Max Surface Area', ylab= "Max G Value", pch= 20)
  }

  # Stops writing to pdf.
  dev.off()
  }