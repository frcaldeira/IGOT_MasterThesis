##--------------------------------------------------------------------
##
## Script name: GEE_NDVI
##
## keywords: Google Earth Engine, NDVI,  TERRACLIMATE
##
## Purpose of script: Get the median values of terraclimate, or NDVI for specific
##  area /shapefile) and a date
##
## Developed by Francisco Caldeira
##
## Version:1.0
## Date created: 12-01-2020
##
## Copyright:Modelling wheat productivity combining wheather data and
## Earth observation data © 2021 
## by Francisco Caldeira is licensed under CC BY 4.0
##
## Returns an csv file with ndvi or terraclimate value
##
## DataSource <- The GEE dataset identification
## DerivedDataSet <- "NDVI" Required if NDVI
## StartDate <- The start date to filter the scene or image
## EndDate <- The end Date to filter the scene or image
## FilterShape <- The shapefile with the areas of interest. The median
## values will be calculated only for the polygons in this shape
#--------------------------------------------------------------------



library(remotes)
library(reticulate)
library(rgee)
library(sf)
library(stars)
library(remotes)
library(base)




#initialize Earth engine
ee_Initialize("francisco.caldeira1973@gmail.com", drive = TRUE)


MedianValue <- function(DataSource, DerivedDataSet = "NDVI", StartDate, EndDate, FilterShape){
  
  print(paste(DataSource, StartDate , EndDate  , sep = "-"))
  
  My_ImageCollection <- ee$ImageCollection(DataSource)$select(DerivedDataSet)$filterDate(StartDate, EndDate)
  
  #median <- My_ImageCollection$reduce(ee$Reducer$median())
  median <- My_ImageCollection$reduce(ee$Reducer$max())
  
  x <- ee_extract(median, FilterShape,scale = 30,sf =FALSE)
  
  if ( length(x) > 0 )
  {
    print (x)
    return(x)
  }
  else
  {
    print ("Deu erro")
    return("NA") 
  }
  
  
}



#DataSet List from Google earth Engine
 
LandSat5_NDVI <- "LANDSAT/LT05/C01/T1_8DAY_NDVI" 
LandSat7_NDVI <- "LANDSAT/LE07/C01/T1_8DAY_NDVI"   
LandSat8_NDVI <- "LANDSAT/LC08/C01/T1_8DAY_NDVI"  
LandSat4_NDVI <- 'LANDSAT/LT04/C01/T1_8DAY_NDVI'
TerraClimate <- "IDAHO_EPSCOR/TERRACLIMATE" 
# TerraClimate: Monthly Climate and Climatic Water Balance for Global Terrestrial
##Surfaces, University of Idaho

## The valid options are!
# "soil" # "pdsi" # Palmer Drought Severity Index
TerraClimateDataSelect <- "pdsi" 
ProcessLandSat <- TRUE
ProcessTerraClimate <- FALSE

CalculatedValue = 0


if (ProcessLandSat == TRUE){
  #Add the header to the csv file
  lineData <- paste( "Year,LandSatSep, LAndSatOct, LandSatNov, LandSatDez, LAndSatJan, LandSatFev, LandSatMar, LandSatApril, LandSatMay, LandSatJune" ,  sep = "")
  write(lineData,file="C:\\lab\\mestrado\\TESE\\DADOS\\EO\\LANDSAT\\LandSatData.csv",append=FALSE)
  
  line <- ""
  write(line,file="C:\\lab\\mestrado\\TESE\\DADOS\\EO\\LANDSAT\\LogFile.txt",append=FALSE)
  
}

if (ProcessTerraClimate == TRUE){
  #Add the header to the csv file
  if (TerraClimateDataSelect == "soil")
  {
    lineData <- paste( "Year,SOILSep, SOILOct, SOILNov, SOILDez, SOILJan, SOILFev, SOILMar, SOILApril, SOILMay, SOILJune" ,  sep = "")
    write(lineData,file="C:\\lab\\mestrado\\TESE\\DADOS\\EO\\TERRACLIMATE\\SOILData.csv",append=FALSE)
  }
  
  if (TerraClimateDataSelect == "pdsi")
  {
    lineData <- paste( "Year,PDSISep, PDSIOct, PDSINov, PDSIDez, PDSIJan, PDSIFev, PDSIMar, PDSIApril, PDSIMay, PDSIJune" ,  sep = "")
    write(lineData,file="C:\\lab\\mestrado\\TESE\\DADOS\\EO\\TERRACLIMATE\\PDSIData.csv",append=FALSE)
  }
  
  
  
  line <- ""
  write(line,file="C:\\lab\\mestrado\\TESE\\DADOS\\EO\\TERRACLIMATE\\LogFile.txt",append=FALSE)
  
}


#Variables to define which month to start recording information
StartRecording <- "09" 
#Variables to define which month to Stop recording information
StopRecording <- "06"
#The inforation to record
information <- ""
#Variable to warn when to star recording the information
isRecording <- FALSE


IndexValue = 0
#Year list
YearList <- c("1985", "1986", "1987", "1988", "1989", "1990", "1991", "1992", "1993", "1994",
              "1995", "1996", "1997", "1998", "1999", "2000", "2001", "2002", "2003", "2004",
              "2005", "2006", "2007", "2008", "2009", "2010", "2011", "2012", "2013", "2014",
            "2015", "2016", "2017", "2018")

 

#Month list by numeric ID 01 January --> 12 Dezember
     MonthList <- c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12")
 
BeginMonthList <- c("12", "01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11")
  EndMonthList <- c("03", "04", "05", "06", "07", "08", "09", "10", "11", "12", "01", "02")
  

#Shapefile to filter the information this data from the classification
Classification1986 <- ee$FeatureCollection("users/ine/Wheat1986AL")
Classification1987 <- ee$FeatureCollection("users/ine/Wheat1987AL")
Classification1988 <- ee$FeatureCollection("users/ine/Wheat1988AL")
Classification1989 <- ee$FeatureCollection("users/ine/Wheat1989AL")


Classification1990 <- ee$FeatureCollection("users/ine/Wheat1990AL")
Classification1991 <- ee$FeatureCollection("users/ine/Wheat1991AL")
Classification1992 <- ee$FeatureCollection("users/ine/Wheat1992AL")
Classification1993 <- ee$FeatureCollection("users/ine/Wheat1993AL")
Classification1994 <- ee$FeatureCollection("users/ine/Wheat1994AL")
Classification1995 <- ee$FeatureCollection("users/ine/Wheat1995AL")
Classification1996 <- ee$FeatureCollection("users/ine/Wheat1996AL")
Classification1997 <- ee$FeatureCollection("users/ine/Wheat1997AL")
Classification1998 <- ee$FeatureCollection("users/ine/Wheat1998AL")
Classification1999 <- ee$FeatureCollection("users/ine/Wheat1999AL")


Classification2000 <- ee$FeatureCollection("users/ine/Wheat2000AL")
Classification2001 <- ee$FeatureCollection("users/ine/Wheat2001AL")
Classification2002 <- ee$FeatureCollection("users/ine/Wheat2002AL")
Classification2003 <- ee$FeatureCollection("users/ine/Wheat2003AL")
Classification2004 <- ee$FeatureCollection("users/ine/Wheat2004AL")
Classification2005 <- ee$FeatureCollection("users/ine/Wheat2005AL")
Classification2006 <- ee$FeatureCollection("users/ine/Wheat2006AL")
Classification2007 <- ee$FeatureCollection("users/ine/Wheat2007AL")
Classification2008 <- ee$FeatureCollection("users/ine/Wheat2008AL")

 
Classification2010 <- ee$FeatureCollection("users/ine/Wheat2010AL")
Classification2011 <- ee$FeatureCollection("users/ine/Wheat2011AL")
Classification2013 <- ee$FeatureCollection("users/ine/Wheat2013AL")
Classification2014 <- ee$FeatureCollection("users/ine/Wheat2014AL")
Classification2016 <- ee$FeatureCollection("users/ine/Wheat2016AL")
Classification2017 <- ee$FeatureCollection("users/ine/Wheat2017AL")
 
Classification2009 <- ee$FeatureCollection("users/ine/Wheat2009AL2")
Classification2012 <- ee$FeatureCollection("users/ine/Wheat2012AL2") 
Classification2015 <- ee$FeatureCollection("users/ine/Wheat2015AL2")
Classification2018 <- ee$FeatureCollection("users/ine/Wheat2018AL2")



#Set the loop to run the year list and then run the mont list
for (Year in YearList)
{
  IndexValue <- 2
  
  LandSatDataSet = switch (Year,
                           "1985" = {LandSat5_NDVI},
                           "1986" = {LandSat5_NDVI}, 
                           "1987" = {LandSat5_NDVI}, 
                           "1988" = {LandSat5_NDVI}, 
                           "1989" = {LandSat5_NDVI}, 
                           "1990" = {LandSat5_NDVI}, 
                           "1991" = {LandSat5_NDVI}, 
                           "1991" = {LandSat5_NDVI},
                           "1992" = {LandSat5_NDVI},
                           "1993" = {LandSat5_NDVI},
                           "1994" = {LandSat5_NDVI},
                           "1995" = {LandSat5_NDVI},
                           "1996" = {LandSat5_NDVI},
                           "1997" = {LandSat5_NDVI},
                           "1998" = {LandSat5_NDVI},
                           "1999" = {LandSat5_NDVI}, 
                           "2000" = {LandSat5_NDVI}, 
                           "2001" = {LandSat5_NDVI}, 
                           "2002" = {LandSat5_NDVI}, 
                           "2003" = {LandSat7_NDVI}, 
                           "2004" = {LandSat5_NDVI},
                           "2005" = {LandSat5_NDVI}, 
                           "2006" = {LandSat5_NDVI}, 
                           "2007" = {LandSat5_NDVI}, 
                           "2008" = {LandSat5_NDVI}, 
                           "2009" = {LandSat5_NDVI}, 
                           "2010" = {LandSat5_NDVI}, 
                           "2011" = {LandSat5_NDVI}, 
                           "2012" = {LandSat7_NDVI}, 
                           "2013" = {LandSat8_NDVI}, 
                           "2014" = {LandSat8_NDVI},
                           "2015" = {LandSat8_NDVI},
                           "2016" = {LandSat8_NDVI},
                           "2017" = {LandSat8_NDVI},
                           "2018" = {LandSat8_NDVI}
  )  

  TerraClimatetDataSet = switch (Year,
                           "1985" = {TerraClimate},
                           "1986" = {TerraClimate},
                           "1987" = {TerraClimate},
                           "1988" = {TerraClimate},
                           "1989" = {TerraClimate},
                           "1990" = {TerraClimate},
                           "1991" = {TerraClimate},
                           "1991" = {TerraClimate},
                           "1992" = {TerraClimate},
                           "1993" = {TerraClimate},
                           "1994" = {TerraClimate},
                           "1995" = {TerraClimate},
                           "1996" = {TerraClimate},
                           "1997" = {TerraClimate},
                           "1998" = {TerraClimate},
                           "1999" = {TerraClimate},
                           "2000" = {TerraClimate},
                           "2001" = {TerraClimate},
                           "2002" = {TerraClimate},
                           "2003" = {TerraClimate},
                           "2004" = {TerraClimate},
                           "2005" = {TerraClimate},
                           "2006" = {TerraClimate},
                           "2007" = {TerraClimate},
                           "2008" = {TerraClimate},
                           "2009" = {TerraClimate},
                           "2010" = {TerraClimate},
                           "2011" = {TerraClimate},
                           "2012" = {TerraClimate},
                           "2013" = {TerraClimate},
                           "2014" = {TerraClimate},
                           "2015" = {TerraClimate},
                           "2016" = {TerraClimate},
                           "2017" = {TerraClimate},
                           "2018" = {TerraClimate}
  )  
  
  
  
  FilterShapefile = switch (Year,
                            "1985" = {Classification1986},
                            "1986" = {Classification1986},
                            "1987" = {Classification1987},
                            "1988" = {Classification1988},
                            "1989" = {Classification1989},
                            "1990" = {Classification1990},
                            "1991" = {Classification1991},
                            "1992" = {Classification1992},
                            "1993" = {Classification1993},
                            "1994" = {Classification1994},
                            "1995" = {Classification1995},
                            "1996" = {Classification1996},
                            "1997" = {Classification1997},
                            "1998" = {Classification1998},
                            "1999" = {Classification1999},
                            "2000" = {Classification2000},
                            "2001" = {Classification2001},
                            "2002" = {Classification2002},
                            "2003" = {Classification2003},
                            "2004" = {Classification2004},
                            "2005" = {Classification2005},
                            "2006" = {Classification2006},
                            "2007" = {Classification2007},
                            "2008" = {Classification2008},
                            "2009" = {Classification2009},
                            "2010" = {Classification2010},
                            "2011" = {Classification2011},
                            "2012" = {Classification2012},
                            "2013" = {Classification2013},
                            "2014" = {Classification2014},
                            "2015" = {Classification2015},
                            "2016" = {Classification2016},
                            "2017" = {Classification2017},
                            "2018" = {Classification2018}
  )
  

  
  for (Month in MonthList)
  {
    print ("Processing month:")
    print (Month)
    print (IndexValue)
    #Prepare begin date that should look like '1985-09-01'
    BeginDate <- paste(Year, Month , "01" , sep = "-")

     if (Month == "12")
     {
       FinaldDate <- paste(as.integer(Year) + 1, "01"  , "01" , sep = "-")
     }
     else
     {
       FinaldDate <- paste(Year, MonthList[IndexValue]  , "01" , sep = "-")
     }

    
    #To enlarge the request
    # if (Month == "01")
    # {
    #   BeginDate <- paste(as.integer(Year) - 1, "12"  , "01" , sep = "-")
    # }
    # else
    # {
    #   BeginDate <- paste(Year, MonthList[IndexValue - 2]   , "01" , sep = "-")
    # }
    # 
    # 
    # 
    # 
    # if (Month == "12")
    # {
    #   FinaldDate <- paste(as.integer(Year) + 1, "02"  , "01" , sep = "-")
    # }
    # else
    # {
    #   FinaldDate <- paste(Year, MonthList[IndexValue + 1]   , "01" , sep = "-")
    # }
    # 
    # if (Month == "11")
    # {
    #   FinaldDate <- paste(as.integer(Year) + 1, "01"  , "01" , sep = "-")
    # }
    # 
 
    
    if (ProcessTerraClimate == TRUE){
      
   
      #CalculatedValue <- 10
      
      print(Year)
      print(BeginDate)
      print(FinaldDate)
      print (CalculatedValue )
      
      if (StartRecording == Month) 
      {
        isRecording <- TRUE
        
      }
      
      if (isRecording == TRUE){
        CalculatedValue <-MedianValue(DataSource = TerraClimatetDataSet, DerivedDataSet = TerraClimateDataSelect,  StartDate = BeginDate, EndDate = FinaldDate ,FilterShape =  FilterShapefile )
      }
      
      
      
      
      if (Month == "01")
      { 
        
        if (isRecording == TRUE)
        {
          if (information == "")
          {
            information = CalculatedValue
          }
          else
          {
            information = paste(information, CalculatedValue , sep = ",")
          }
        }
        
        
      }
      
      if (Month == "02")
      { 
        
        if (isRecording == TRUE)
        {
          if (information == "")
          {
            information = CalculatedValue
          }
          else
          {
            information = paste(information, CalculatedValue , sep = ",")
          }
          
        }
      }
      
      if (Month == "03")
      {
        if (isRecording == TRUE)
        {
          if (information == "")
          {
            information = CalculatedValue
          }
          else
          {
            information = paste(information, CalculatedValue , sep = ",")
          }
          
        }
      }
      
      if (Month == "04")
      { 
        if (isRecording == TRUE)
        {
          if (information == "")
          {
            information = CalculatedValue
          }
          else
          {
            information = paste(information, CalculatedValue , sep = ",")
          }
          
        }
      }
      
      if (Month == "05")
      { 
        if (isRecording == TRUE)
        {
          if (information == "")
          {
            information = CalculatedValue
          }
          else
          {
            information = paste(information, CalculatedValue , sep = ",")
          }
          
        }
      }
      
      if (Month == "06")
      { 
        if (isRecording == TRUE)
        {
          if (information == "")
          {
            information = CalculatedValue
          }
          else
          {
            information = paste(information, CalculatedValue , sep = ",")
          }
          
        }
      }
      
      if (Month == "07")
      { 
        if (isRecording == TRUE)
        {
          if (information == "")
          {
            information = CalculatedValue
          }
          else
          {
            information = paste(information, CalculatedValue , sep = ",")
          }
          
        }
      }
      
      if (Month == "08")
      { 
        if (isRecording == TRUE)
        {
          if (information == "")
          {
            information = CalculatedValue
          }
          else
          {
            information = paste(information, CalculatedValue , sep = ",")
          }
          
        }
      }
      
      if (Month == "09")
      { 
        if (isRecording == TRUE)
        {
          if (information == "")
          {
            information = CalculatedValue
          }
          else
          {
            information = paste(information, CalculatedValue , sep = ",")
          }
          
        }
      }
      
      if (Month == "10")
      { 
        if (isRecording == TRUE)
        {
          if (information == "")
          {
            information = CalculatedValue
          }
          else
          {
            information = paste(information, CalculatedValue , sep = ",")
          }
          
        }
      }
      
      if (Month == "11")
      { 
        if (isRecording == TRUE)
        {
          if (information == "")
          {
            information = CalculatedValue
          }
          else
          {
            information = paste(information, CalculatedValue , sep = ",")
          }
          
        }
      }
      
      if (Month == "12")
      {
        if (isRecording == TRUE)
        {
          if (information == "")
          {
            information = CalculatedValue
          }
          else
          {
            information = paste(information, CalculatedValue , sep = ",")
          }
          
        }
      } 
      
      
      
      
      if (isRecording == TRUE) {
        
        
        #This is the logfile
        line <- paste( Sys.Date() , ", DataSource=" , TerraClimateDataSelect , ", BeginDate="  , BeginDate, ", EndDate=" , FinaldDate, " Value= ", CalculatedValue ,  sep = "")
        write(line,file="C:\\lab\\mestrado\\TESE\\DADOS\\EO\\TERRACLIMATE\\LogFile.txt",append=TRUE)
      }
      
      
      if (StopRecording == Month) 
      {
        if (information == "")
        {
          
        }
        else
        {
          #This is the calculated data
          lineData <- paste( Year, information ,  sep = ",")
          if  (TerraClimateDataSelect == "soil")
          {
            write(lineData,file="C:\\lab\\mestrado\\TESE\\DADOS\\EO\\TERRACLIMATE\\SOILData.csv",append=TRUE)
          }
          if  (TerraClimateDataSelect == "pdsi")
          {
            write(lineData,file="C:\\lab\\mestrado\\TESE\\DADOS\\EO\\TERRACLIMATE\\PDSIData.csv",append=TRUE)
          }         
          

        }
        
        
        isRecording = FALSE
        information <- ""
        
      
      }
    }
    
    if (ProcessLandSat == TRUE){
      
      
      
      #CalculatedValue <- 10
      
      print(Year)
      print(BeginDate)
      print(FinaldDate)
      print (CalculatedValue )
      
      if (StartRecording == Month) 
      {
        isRecording <- TRUE
        
      }
      
      if (isRecording == TRUE){
        CalculatedValue <-MedianValue(DataSource = LandSatDataSet, StartDate = BeginDate, EndDate = FinaldDate ,FilterShape =  FilterShapefile )
      }
      
      
      
      
      if (Month == "01")
      { 
        
        if (isRecording == TRUE)
        {
          if (information == "")
          {
            information = CalculatedValue
          }
          else
          {
            information = paste(information, CalculatedValue , sep = ",")
          }
        }
        
        
      }
      
      if (Month == "02")
      { 
        
        if (isRecording == TRUE)
        {
          if (information == "")
          {
            information = CalculatedValue
          }
          else
          {
            information = paste(information, CalculatedValue , sep = ",")
          }
          
        }
      }
      
      if (Month == "03")
      {
        if (isRecording == TRUE)
        {
          if (information == "")
          {
            information = CalculatedValue
          }
          else
          {
            information = paste(information, CalculatedValue , sep = ",")
          }
          
        }
      }
      
      if (Month == "04")
      { 
        if (isRecording == TRUE)
        {
          if (information == "")
          {
            information = CalculatedValue
          }
          else
          {
            information = paste(information, CalculatedValue , sep = ",")
          }
          
        }
      }
      
      if (Month == "05")
      { 
        if (isRecording == TRUE)
        {
          if (information == "")
          {
            information = CalculatedValue
          }
          else
          {
            information = paste(information, CalculatedValue , sep = ",")
          }
          
        }
      }
      
      if (Month == "06")
      { 
        if (isRecording == TRUE)
        {
          if (information == "")
          {
            information = CalculatedValue
          }
          else
          {
            information = paste(information, CalculatedValue , sep = ",")
          }
          
        }
      }
      
      if (Month == "07")
      { 
        if (isRecording == TRUE)
        {
          if (information == "")
          {
            information = CalculatedValue
          }
          else
          {
            information = paste(information, CalculatedValue , sep = ",")
          }
          
        }
      }
      
      if (Month == "08")
      { 
        if (isRecording == TRUE)
        {
          if (information == "")
          {
            information = CalculatedValue
          }
          else
          {
            information = paste(information, CalculatedValue , sep = ",")
          }
          
        }
      }
      
      if (Month == "09")
      { 
        if (isRecording == TRUE)
        {
          if (information == "")
          {
            information = CalculatedValue
          }
          else
          {
            information = paste(information, CalculatedValue , sep = ",")
          }
          
        }
      }
      
      if (Month == "10")
      { 
        if (isRecording == TRUE)
        {
          if (information == "")
          {
            information = CalculatedValue
          }
          else
          {
            information = paste(information, CalculatedValue , sep = ",")
          }
          
        }
      }
      
      if (Month == "11")
      { 
        if (isRecording == TRUE)
        {
          if (information == "")
          {
            information = CalculatedValue
          }
          else
          {
            information = paste(information, CalculatedValue , sep = ",")
          }
          
        }
      }
      
      if (Month == "12")
      {
        if (isRecording == TRUE)
        {
          if (information == "")
          {
            information = CalculatedValue
          }
          else
          {
            information = paste(information, CalculatedValue , sep = ",")
          }
          
        }
      } 
      
      
  
      
      if (isRecording == TRUE) {
        
        
        #This is the logfile
        line <- paste( Sys.Date() , ", DataSource=" , LandSatDataSet , ", BeginDate="  , BeginDate, ", EndDate=" , FinaldDate, " Value= ", CalculatedValue ,  sep = "")
        write(line,file="C:\\lab\\mestrado\\TESE\\DADOS\\EO\\LANDSAT\\LogFile.txt",append=TRUE)
      }
      
      
      if (StopRecording == Month) 
      {
        if (information == "")
        {
          
        }
        else
        {
          #This is the calculated data
          lineData <- paste( Year, information ,  sep = ",")
          write(lineData,file="C:\\lab\\mestrado\\TESE\\DADOS\\EO\\LANDSAT\\LandSatData.csv",append=TRUE)
        }

        
        isRecording = FALSE
        information <- ""
        
        
      }        
      
      
    }
    
    
    IndexValue <- IndexValue + 1
  }
}

