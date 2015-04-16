#######################################################################################################
#          *** Copyright (C) Andrew Sommerlot - All Rights Reserved ***           					  #
# Unauthorized Copying, Distrubution, or editing of this file, via any medium is strictly prohibited  #
#                               Proprietary and Confidential                                          #
#               Written by Andrew R Sommerlot <andrewrs@vt.edu>, March 2015                           #
#######################################################################################################

###############################################################################################
# 					           *** Version 0.1.1 *** 										  #
#				download and format meteorological data for use in                            #
#						hydrologic modeling													  #
# 				central function of the hydroMet packages                                     #
# Currently supports 																		  #
#		sources: CFSR                                                                         #
#		formats: SWAT                                                                         #              
###############################################################################################

#' Gets met data from a specified source and creates model input files in the specified format
#' @param locations
#' @param dataSource
#' @param outFormat
#' @param outDir
#' @return returns specified  met data in specified format
#' @export 





getMet <-  function(locations, dataSource = 'cfsr', outFormat = 'swat', outDir = getwd()){
	
	if(dataSource != 'cfsr'){
		  stop('The getMet function does not yet have support for data sources other than the cfsr model\n', 'Set dataSource to cfsr to continue')
	} else if(outFormat != 'swat') {
		  stop('The getMet function does not yet have support for output formats other than the swat model\n', 'Set outFormat to swat to continue')
	}else{
		  getSWATcfsr(locations, outDir = outDir)
	}
}