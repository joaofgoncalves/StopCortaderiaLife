

fl <- list.files("D:/DATA/LifeCortaderia/Orthomosaics/__NEW_MOSAICS__2020",pattern=".tif$",
                 recursive = TRUE,full.names = TRUE)


fl <- fl[!grepl("orthomosaic|elevation|.files",fl)]


cat(paste("arcpy.BuildPyramids_management(\"",fl,"\")\n\n",sep=""))


cat(paste("arcpy.CalculateStatistics_management(\"",fl,"\")\n\n",sep=""))


