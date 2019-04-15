
#############################################################################################################
# this script downloads, unzips, creates csv files, and computes run expectancies from Retrosheet data      #
# it is adapted from code found here: https://bayesball.github.io/VB/Getting_Retrosheet_Files.html          #
# and here: https://baseballwithr.wordpress.com/2014/02/10/downloading-retrosheet-data-and-runs-expectancy/ #
# both blog posts by Jim Albert, one of the two authors of Analyzing Baseball Data with R                   #
#                                                                                                           #
# NOTE: must used Chadwick v0.6 files with this code...apparently v0.7 doesn't support wildcards            #
# Chadwick files available here: https://sourceforge.net/projects/chadwick/files/                           #
#                                                                                                           #
# misc issues and troubleshooting discussed in messages under earlier blog post                             #
#     (https://baseballwithr.wordpress.com/2014/02/10/downloading-retrosheet-data-and-runs-expectancy/)     #
#############################################################################################################

#library(devtools)

###################################################
# 1) Set working directory
###################################################
# within this folder, must have folder called "download.folder"
# within download.folder, must have folders "zipped" and "unzipped"
# fields.csv and Chadwick files (see above) must sit in unzipped folder
# futher details available at URLs above, if needed

setwd(choose.dir())

###################################################
# 2) Load function to download and parse data
###################################################
# function CAN be loaded from URL below (need to load devtools package first)
# but copied full function script here in case URL goes away

#source_gist("https://gist.github.com/bayesball/8892981",filename="parse.retrosheet2.pbp.R")

parse.retrosheet2.pbp = function(season){
  # ADJUSTED FOR MAC -- function will work for WINDOWS and MAC
  # download, unzip, append retrosheet data
  # assume current directory has a folder download.folder
  # download.folder has two subfolders unzipped and zipped
  # program cwevent.exe is in unzipped folder (for windows)
  
  download.retrosheet <- function(season){
    # get zip file from retrosheet website
    download.file(
      url=paste("http://www.retrosheet.org/events/", season, "eve.zip", sep="")
      , destfile=paste("download.folder", "/zipped/", season, "eve.zip", sep="")
    )
  }
  unzip.retrosheet <- function(season){
    #unzip retrosheet files
    unzip(paste("download.folder", "/zipped/", season, "eve.zip", sep=""), 
          exdir=paste("download.folder", "/unzipped", sep=""))
  }
  create.csv.file=function(year){
    # http://chadwick.sourceforge.net/doc/cwevent.html#cwtools-cwevent
    # shell("cwevent -y 2000 2000TOR.EVA > 2000TOR.bev")
    wd = getwd()
    setwd("download.folder/unzipped")
    if (.Platform$OS.type == "unix"){
      system(paste(paste("cwevent -y", year, "-f 0-96"), 
                   paste(year,"*.EV*",sep=""),
                   paste("> all", year, ".csv", sep="")))} else {
                     shell(paste(paste("cwevent -y", year, "-f 0-96"), 
                                 paste(year,"*.EV*",sep=""),
                                 paste("> all", year, ".csv", sep="")))              
                   }
    setwd(wd)
  }
  create.csv.roster = function(year){
    # creates a csv file of the rosters
    filenames <- list.files(path = "download.folder/unzipped/")
    filenames.roster = 
      subset(filenames, substr(filenames, 4, 11)==paste(year,".ROS",sep=""))
    read.csv2 = function(file)
      read.csv(paste("download.folder/unzipped/", file, sep=""),header=FALSE)
    R = do.call("rbind", lapply(filenames.roster, read.csv2))
    names(R)[1:6] = c("Player.ID", "Last.Name", "First.Name", 
                      "Bats", "Pitches", "Team")
    wd = getwd()
    setwd("download.folder/unzipped")
    write.csv(R, file=paste("roster", year, ".csv", sep=""))
    setwd(wd)
  }
  cleanup = function(){
    # removes retrosheet files not needed
    wd = getwd()
    setwd("download.folder/unzipped")
    if (.Platform$OS.type == "unix"){
      system("rm *.EVN")
      system("rm *.EVA")
      system("rm *.ROS")
      system("rm TEAM*")} else {
        shell("del *.EVN")
        shell("del *.EVA")
        shell("del *.ROS")
        shell("del TEAM*")
      }       
    setwd(wd)
    setwd("download.folder/zipped")
    if (.Platform$OS.type == "unix"){
      system("rm *.zip")} else {
        shell("del *.zip")
      }
    setwd(wd)
  }
  download.retrosheet(season)
  unzip.retrosheet(season)
  create.csv.file(season)
  create.csv.roster(season)
  cleanup()
}

#################################################################
# 3) run function to download and parse target sesason
#################################################################

parse.retrosheet2.pbp(2016)


