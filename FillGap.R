##--------------------------------------------------------------------
##
## Script name: FillGap
##
## keywords: impute, Time series, gap
##
## Purpose of script: Compute missing values on a time serie. Use a csv file with
## the time serie and gaps and export the same time serie on another csv file 
## with imputate values for the gaps. It also plots an image of the time serie
##
## Developed by Francisco Caldeira
##
## Version:1.0
## Date created: 07-03-2021
##
## Copyright:Modelling wheat productivity combining wheather data and Earth observation data © 2021 
## by Francisco Caldeira is licensed under CC BY 4.0
##
## List of parameters
##
## DF_NDVI_VALS -> It's the time series, with one variable only and gaps
##             
## DF_NDVI_VALS_IMPUTATED <- It's a csv file with the time series and values for the 
##gaps
##
#--------------------------------------------------------------------

library(imputeTS)

#Read the CSV file with the NDVI values
DF_NDVI_VALS <- read.csv("C:\\lab\\mestrado\\TESE\\DADOS\\EO\\LandSat\\DadosClassificacaoImagem\\LandSatData2.csv", header = TRUE) 

#Get the year column for future join
ColumnYear <- DF_NDVI_VALS[1] 

#Remove the year column to avoid invalid time series analysys
DF_NDVI_VALS <-  DF_NDVI_VALS[,-1]

#Get the column names
ColumnNames <- colnames(DF_NDVI_VALS)

#Number of columns in dataframe
NCol_Number <- ncol(DF_NDVI_VALS)

#Convert the datafrmae to vector to perform imputation
NDVI_VALS <- as.vector(t(DF_NDVI_VALS))

#Plot the distribution
ggplot_na_distribution(NDVI_VALS)

#For longer and more complex time series (with trend and seasonality) it's
#a good idena to try na.kalman and na.seadec 
Imputation_Kalman= na_kalman(NDVI_VALS)
colnames(Imputation_Kalman) <- c( "NDVI")

#Plot the result
ggplot_na_imputations(NDVI_VALS, Imputation_Kalman)

#Create the New Dataframe
DF_NDVI_VALS_IMPUTATED <- data.frame()

DataSeriesLength = length(Imputation_Kalman)
Position <- 1
pass <- NCol_Number 
#
while (DataSeriesLength > Position)
{
   ThisRow = Imputation_Kalman[Position:(Position+pass-1)]
   DF_NDVI_VALS_IMPUTATED <- rbind(DF_NDVI_VALS_IMPUTATED, ThisRow)
   Position = Position + pass
   
}

#Define the column names
colnames(DF_NDVI_VALS_IMPUTATED) <- ColumnNames

#Get the year column
DF_NDVI_VALS_IMPUTATED <- cbind(  ColumnYear, DF_NDVI_VALS_IMPUTATED)

#Save the new file
write.csv(DF_NDVI_VALS_IMPUTATED,"C:\\lab\\mestrado\\TESE\\DADOS\\EO\\LandSat\\DadosClassificacaoImagem\\LandSatDataxxx.csv", row.names = TRUE)


 