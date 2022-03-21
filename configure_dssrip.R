#'
#' This script establishes Java settings required to use the dssrip package
#'

# Check if we're in the right folder and the configure dssrip script is 
#   in the current working directory
if(!("configure_dssrip.R" %in% dir()))
  warning(paste0("\nThe current working directory doesn't appear to be",
                 "configured as expected to load\n\tthe dssrip package.",
                 "Use the 'setwd' function to set the current\n\tworking",
                 " directory to the folder containing this script. e.g.:\n\n\t",
                 "setwd('D:/scripts/R_Scripting_Basics')\n\n"))

# Make sure nothing is pre-configured for Java.
Sys.unsetenv(c("JAVA_HOME"))

# Establish path to HEC-DSSVue to load Java
baseJavaDir <- sprintf("%s\\resources\\HEC-DSSVue-v3.0.00.212", getwd())

# Load configuration file and rewrite so it's configured
#   relative to the base directory
library(rjson)
dssCnfg <- rjson::fromJSON(file = "config/jar_config.json")
dssCnfg$configs[[1]]$dss_location <- baseJavaDir
dssCnfg$configs[[1]]$JAVA_HOME <- file.path(baseJavaDir,"java")
writeLines(text = toJSON(x = dssCnfg),
           con = "config/jar_config.json", sep = "\\")

# Increasing Java memory allocation
options(java.parameters = "-Xms64m")

# Load package
library(dssrip)
