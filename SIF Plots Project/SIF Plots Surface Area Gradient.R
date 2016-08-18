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
  row.has.na <- apply(data, 1, function(x) {any(is.na(x))} )

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
  pdf_name <- paste(filename_no_ext, '_plots_SA Gradient.pdf')
  pdf(file = pdf_name, title = pdf_name, width  = 8, height = 11.5)
  plot_name <- unlist(strsplit(pdf_name, '/'))
  plot_name <- plot_name[length(plot_name)]
  
  # Assigns color based on ratio of current surface area to largest surface area in fracture; blue is smallest ratio, red is largest.
  colour_palette <- colorRampPalette(c('blue','red'))
  data$'Colour' <- colour_palette(30)[as.numeric(cut((data$'SurfaceArea' / data$'MaxSurfaceArea'), breaks = 30))]

  # Creates 2x2 matrix for four raw data figures drawn below (Type I, II, III, and G values).
  par(mfrow=c(2,2), oma=c(0,0,3,0))
  
  # Creates vector of all fracture names for legend.
  fracture_names <- vector()
  for (fracture in unique(data$FractureNum)){
    fracture_names <- append(fracture_names, paste('Fracture', fracture))
  }
  
  # Raw data graphs scaled automatically.
  plot(data$'SurfaceArea', data$'KI', main = paste("KI"), xlab = 'Surface Area', ylab= "KI SIF Value", pch= pchvalue[data$'FractureNum'], col = data$'Colour', cex = data$'SurfaceArea'/data$'MaxSurfaceArea')
  #legend("topright", legend = fracture_names, pch = pchvalue, col = 'black')
  plot(data$'SurfaceArea', data$'KII', main = paste("KII"), xlab = 'Surface Area', ylab= "KII SIF Value", pch= pchvalue[data$'FractureNum'], col = data$'Colour', cex = data$'SurfaceArea'/data$'MaxSurfaceArea')
  plot(data$'SurfaceArea', data$'KIII', main = paste("KIII"), xlab = 'Surface Area', ylab= "KIII SIF Value", pch= pchvalue[data$'FractureNum'], col = data$'Colour', cex = data$'SurfaceArea'/data$'MaxSurfaceArea')
  plot(data$'SurfaceArea', data$'G', main = paste("G"), xlab = 'Surface Area', ylab= "G Value", pch= pchvalue[data$'FractureNum'], col = data$'Colour', cex = data$'SurfaceArea'/data$'MaxSurfaceArea')
  mtext(plot_name, adj=0.5, side=3, outer=TRUE)
  
  # Raw data graphs scaled manually to be equivalent throughout data files.
  plot(data$'SurfaceArea', data$'KI', main = paste("KI"), xlab = 'Surface Area', ylab= "KI SIF Value", pch= pchvalue[data$'FractureNum'], col = data$'Colour', cex = data$'SurfaceArea'/data$'MaxSurfaceArea', ylim = c(0e00, 5e10), xlim= c(0, 30))
  #legend("topright", legend = fracture_names, pch = pchvalue, col = 'black')
  plot(data$'SurfaceArea', data$'KII', main = paste("KII"), xlab = 'Surface Area', ylab= "KII SIF Value", pch= pchvalue[data$'FractureNum'], col = data$'Colour', cex = data$'SurfaceArea'/data$'MaxSurfaceArea', ylim = c(-1e10, 2e10), xlim= c(0, 30))
  plot(data$'SurfaceArea', data$'KIII', main = paste("KIII"), xlab = 'Surface Area', ylab= "KIII SIF Value", pch= pchvalue[data$'FractureNum'], col = data$'Colour', cex = data$'SurfaceArea'/data$'MaxSurfaceArea', ylim = c(-1e10, 1e10), xlim= c(0, 30))
  plot(data$'SurfaceArea', data$'G', main = paste("G"), xlab = 'Surface Area', ylab= "G Value", pch= pchvalue[data$'FractureNum'], col = data$'Colour', cex = data$'SurfaceArea'/data$'MaxSurfaceArea', ylim = c(0e00, 10e10), xlim= c(0, 30))
  
  # Calculates mean SIF values (KI, KII, KIII, and G).
  data_means <- aggregate(data[,c(-3, -15, -17)], by = list(TipNr = data$'TipNr', FractureNum = data$'FractureNum'), FUN = "mean")
  
  # Calculates min SIF values (KI, KII, KIII, and G).
  data_mins <- aggregate(data[,c(-3, -15, -17)], by = list(TipNr = data$'TipNr', FractureNum = data$'FractureNum'), FUN = "min")
  
  # Adds surface area value in original data set associated with the min KI value to data_mins dataframe. If multiple matching values are found, the mean is used.
  data_mins$'KI.SA' <- apply(data_mins, 1, function(row){
    TipNr1 <- as.numeric(row[1])
    FractureNum1 <- as.numeric(row[2])
    KI1 <- as.numeric(row[9])
    return(mean(data$'SurfaceArea'[which(data$'KI' == KI1 & data$'FractureNum' == FractureNum1 & data$'TipNr' == TipNr1)]))
  })
  
  # Adds surface area value in original data set associated with the min KII value to data_mins dataframe. If multiple matching values are found, the mean is used.
  data_mins$'KII.SA' <- apply(data_mins, 1, function(row){
    TipNr1 <- as.numeric(row[1])
    FractureNum1 <- as.numeric(row[2])
    KII1 <- as.numeric(row[10])
    return(mean(data$'SurfaceArea'[which(data$'KII' == KII1 & data$'FractureNum' == FractureNum1 & data$'TipNr' == TipNr1)]))
  })
  
  # Adds surface area value in original data set associated with the min KIII value to data_mins dataframe. If multiple matching values are found, the mean is used.
  data_mins$'KIII.SA' <- apply(data_mins, 1, function(row){
    TipNr1 <- as.numeric(row[1])
    FractureNum1 <- as.numeric(row[2])
    KIII1 <- as.numeric(row[11])
    return(mean(data$'SurfaceArea'[which(data$'KIII' == KIII1 & data$'FractureNum' == FractureNum1 & data$'TipNr' == TipNr1)]))
  })
  
  # Adds surface area value in original data set associated with the min G value to data_mins dataframe. If multiple matching values are found, the mean is used.
  data_mins$'G.SA' <- apply(data_mins, 1, function(row){
    TipNr1 <- as.numeric(row[1])
    FractureNum1 <- as.numeric(row[2])
    G1 <- as.numeric(row[12])
    return(mean(data$'SurfaceArea'[which(data$'G' == G1 & data$'FractureNum' == FractureNum1 & data$'TipNr' == TipNr1)]))
  })

  # Adds colour in original data set associated with the min KI value to data_mins dataframe. If multiple matching values are found, the first colour is selected.
  data_mins$'KI.Colour' <- apply(data_mins, 1, function(row){
    TipNr1 <- as.numeric(row[1])
    FractureNum1 <- as.numeric(row[2])
    KI1 <- as.numeric(row[9])
    return(data$'Colour'[which(data$'KI' == KI1 & data$'FractureNum' == FractureNum1 & data$'TipNr' == TipNr1)][1])
  })
  
  # Adds colour in original data set associated with the min KII value to data_mins dataframe. If multiple matching values are found, the first colour is selected.
  data_mins$'KII.Colour' <- apply(data_mins, 1, function(row){
    TipNr1 <- as.numeric(row[1])
    FractureNum1 <- as.numeric(row[2])
    KII1 <- as.numeric(row[10])
    return(data$'Colour'[which(data$'KII' == KII1 & data$'FractureNum' == FractureNum1 & data$'TipNr' == TipNr1)][1])
  })
  
  # Adds colour in original data set associated with the min KIII value to data_mins dataframe. If multiple matching values are found, the first colour is selected.
  data_mins$'KIII.Colour' <- apply(data_mins, 1, function(row){
    TipNr1 <- as.numeric(row[1])
    FractureNum1 <- as.numeric(row[2])
    KIII1 <- as.numeric(row[11])
    return(data$'Colour'[which(data$'KIII' == KIII1 & data$'FractureNum' == FractureNum1 & data$'TipNr' == TipNr1)][1])
  })
  
  # Adds colour in original data set associated with the min G value to data_mins dataframe. If multiple matching values are found, the first colour is selected.
  data_mins$'G.Colour' <- apply(data_mins, 1, function(row){
    TipNr1 <- as.numeric(row[1])
    FractureNum1 <- as.numeric(row[2])
    G1 <- as.numeric(row[12])
    return(data$'Colour'[which(data$'G' == G1 & data$'FractureNum' == FractureNum1 & data$'TipNr' == TipNr1)][1])
  })
  
  # Calculates max SIF values (KI, KII, KIII, and G).
  data_maxs <- aggregate(data[,c(-3, -15, -17)], by = list(TipNr = data$'TipNr', FractureNum = data$'FractureNum'), FUN = "max")
  
  # Adds surface area value in original data set associated with the max KI value to data_maxs dataframe. If multiple matching values are found, the mean is used.
  data_maxs$'KI.SA' <- apply(data_maxs, 1, function(row){
    TipNr1 <- as.numeric(row[1])
    FractureNum1 <- as.numeric(row[2])
    KI1 <- as.numeric(row[9])
    return(mean(data$'SurfaceArea'[which(data$'KI' == KI1 & data$'FractureNum' == FractureNum1 & data$'TipNr' == TipNr1)]))
  })
  
  # Adds surface area value in original data set associated with the max KII value to data_maxs dataframe. If multiple matching values are found, the mean is used.
  data_maxs$'KII.SA' <- apply(data_maxs, 1, function(row){
    TipNr1 <- as.numeric(row[1])
    FractureNum1 <- as.numeric(row[2])
    KII1 <- as.numeric(row[10])
    return(mean(data$'SurfaceArea'[which(data$'KII' == KII1 & data$'FractureNum' == FractureNum1 & data$'TipNr' == TipNr1)]))
  })
  
  # Adds surface area value in original data set associated with the max KIII value to data_maxs dataframe. If multiple matching values are found, the mean is used.
  data_maxs$'KIII.SA' <- apply(data_maxs, 1, function(row){
    TipNr1 <- as.numeric(row[1])
    FractureNum1 <- as.numeric(row[2])
    KIII1 <- as.numeric(row[11])
    return(mean(data$'SurfaceArea'[which(data$'KIII' == KIII1 & data$'FractureNum' == FractureNum1 & data$'TipNr' == TipNr1)]))
  })
  
  # Adds surface area value in original data set associated with the max G value to data_maxs dataframe. If multiple matching values are found, the mean is used.
  data_maxs$'G.SA' <- apply(data_maxs, 1, function(row){
    TipNr1 <- as.numeric(row[1])
    FractureNum1 <- as.numeric(row[2])
    G1 <- as.numeric(row[12])
    return(mean(data$'SurfaceArea'[which(data$'G' == G1 & data$'FractureNum' == FractureNum1 & data$'TipNr' == TipNr1)]))
  })
  
  # Adds colour in original data set associated with the max KI value to data_maxs dataframe. If multiple matching values are found, the first colour is selected.
  data_maxs$'KI.Colour' <- apply(data_maxs, 1, function(row){
    TipNr1 <- as.numeric(row[1])
    FractureNum1 <- as.numeric(row[2])
    KI1 <- as.numeric(row[9])
    return(data$'Colour'[which(data$'KI' == KI1 & data$'FractureNum' == FractureNum1 & data$'TipNr' == TipNr1)][1])
  })
  
  # Adds colour in original data set associated with the max KII value to data_maxs dataframe. If multiple matching values are found, the first colour is selected.
  data_maxs$'KII.Colour' <- apply(data_maxs, 1, function(row){
    TipNr1 <- as.numeric(row[1])
    FractureNum1 <- as.numeric(row[2])
    KII1 <- as.numeric(row[10])
    return(data$'Colour'[which(data$'KII' == KII1 & data$'FractureNum' == FractureNum1 & data$'TipNr' == TipNr1)][1])
  })
  
  # Adds colour in original data set associated with the max KIII value to data_maxs dataframe. If multiple matching values are found, the first colour is selected.
  data_maxs$'KIII.Colour' <- apply(data_maxs, 1, function(row){
    TipNr1 <- as.numeric(row[1])
    FractureNum1 <- as.numeric(row[2])
    KIII1 <- as.numeric(row[11])
    return(data$'Colour'[which(data$'KIII' == KIII1 & data$'FractureNum' == FractureNum1 & data$'TipNr' == TipNr1)][1])
  })
  
  # Adds colour in original data set associated with the max G value to data_maxs dataframe. If multiple matching values are found, the first colour is selected.
  data_maxs$'G.Colour' <- apply(data_maxs, 1, function(row){
    TipNr1 <- as.numeric(row[1])
    FractureNum1 <- as.numeric(row[2])
    G1 <- as.numeric(row[12])
    return(data$'Colour'[which(data$'G' == G1 & data$'FractureNum' == FractureNum1 & data$'TipNr' == TipNr1)][1])
  })
  
  # Creates 2x2 matrix for four mean figures drawn below (Type I, II, III, and G values).
  par(mfrow=c(2,2))
  
  # Mean graphs.
  plot(data_means$'SurfaceArea', data_means$'KI', main = paste("KI Mean"), xlab = 'Surface Area', ylab= "KI Avg SIF Value", pch= pchvalue[data_means$'FractureNum'])
  #legend("topright", legend = fracture_names, pch = pchvalue, col = 'black')
  plot(data_means$'SurfaceArea', data_means$'KII', main = paste("KII Mean"), xlab = 'Surface Area', ylab= "KII Avg SIF Value", pch= pchvalue[data_means$'FractureNum'])
  plot(data_means$'SurfaceArea', data_means$'KIII', main = paste("KIII Mean"), xlab = 'Surface Area', ylab= "KIII Avg SIF Value", pch= pchvalue[data_means$'FractureNum'])
  plot(data_means$'SurfaceArea', data_means$'G', main = paste("G Mean"), xlab = 'Surface Area', ylab= "Avg G Value", pch= pchvalue[data_means$'FractureNum'])
  
  # Creates 2x2 matrix for four min figures drawn below (Type I, II, III, and G values).
  par(mfrow=c(2,2))
  
  # Min graphs.
  plot(data_mins$'KI.SA', data_mins$'KI', main = paste("KI Minimum Value"), xlab = 'Surface Area', ylab= "KI Min SIF Value", col = data_mins$'KI.Colour', pch= pchvalue[data_mins$'FractureNum'])
  #legend("topright", legend = fracture_names, pch = pchvalue, col = 'black')
  plot(data_mins$'KII.SA', data_mins$'KII', main = paste("KII Minimum Value"), xlab = 'Surface Area', ylab= "KII Min SIF Value", col = data_mins$'KII.Colour', pch= pchvalue[data_mins$'FractureNum'])
  plot(data_mins$'KIII.SA', data_mins$'KIII', main = paste("KIII Minimum Value"), xlab = 'Surface Area', ylab= "KIII Min SIF Value", col = data_mins$'KIII.Colour', pch= pchvalue[data_mins$'FractureNum'])
  plot(data_mins$'G.SA', data_mins$'G', main = paste("G Minimum Value"), xlab = 'Surface Area', ylab= "Min G Value", col = data_mins$'G.Colour', pch= pchvalue[data_mins$'FractureNum'])
  
  # Creates 2x2 matrix for four max figures drawn below (Type I, II, III, and G values).
  par(mfrow=c(2,2))
  
  # Max graphs.
  plot(data_maxs$'KI.SA', data_maxs$'KI', main = paste("KI Max Value"), xlab = 'Surface Area', ylab= "KI Max SIF Value", col = data_maxs$'KI.Colour', pch= pchvalue[data_maxs$'FractureNum'])
  #legend("topright", legend = fracture_names, pch = pchvalue, col = 'black')
  plot(data_maxs$'KII.SA', data_maxs$'KII', main = paste("KII Max Value"), xlab = 'Surface Area', ylab= "KII Max SIF Value", col = data_maxs$'KII.Colour', pch= pchvalue[data_maxs$'FractureNum'])
  plot(data_maxs$'KIII.SA', data_maxs$'KIII', main = paste("KIII Max Value"), xlab = 'Surface Area', ylab= "KIII Max SIF Value", col = data_maxs$'KIII.Colour', pch= pchvalue[data_maxs$'FractureNum'])
  plot(data_maxs$'G.SA', data_maxs$'G', main = paste("Max G Value"), xlab = 'Surface Area', ylab= "Max G Value", col = data_maxs$'G.Colour', pch= pchvalue[data_maxs$'FractureNum'])
  
  # Stops writing to pdf.
  dev.off()
}