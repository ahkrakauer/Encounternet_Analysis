## Script for arranging timeseries data by behavior block
## Alan Krakauer
## January 2014

## Intended to process Pymaster accelerometer timeseries files for use in online accelerometer analysis script.

## input- a tab-delimited text file from Pymaster
## output- comma deliminted file 

## **NOTE- must manually change full path and file name for both input and output.**
## ** NOTE- check sample parameters below the file section (either time and sample rate or
##          number of samples)
##
## Current code does not use behavior code column if it is present
## 
## This goes without saying, but data should be in chronological order
## 
## 
rm()
###############################################
## File names

# Change Timeseries file name here 
# File name should have full file path, file extension, and be surrounded by quotes
# VVVVVVVVVVVV
accel <- read.table("/Users/ahkrakauer/Dropbox/84 timebeh majority exception.txt", header = TRUE, sep = "\t", skip = 7)
# /\/\/\/\/\/\/\/\/\/\/\/\/

# Change output file name here
# File name should have full file path, file extension, and be surrounded by quotes
# VVVVVVVVVVVV
outfile <- "/Users/ahkrakauer/Dropbox/84_timeseries_sorted6.txt"
# /\/\/\/\/\/\/\/\/\/\/\/\/
#################################
# SAMPLE PARAMETERS
# Check before running

NumSampOrSecs <- 1
# Set to 0 to directly input number of samples
# Set to 1 to input Seconds and sample rate

samplerate <- 12
# sample rate of accelerometer- if unsure check headers in timeseries file

blocksec <- 5
# number of seconds for one block

blocksamp <- 60
# set here for the number of samples to use
# note this value will be overwritten if 
# NumSampOrSec is set to 1

lastblockmin <- 0.5
# parameter for whether to keep the last partial sample or not
# if the remaining samples comprise more than this proportion of a full sample,
# keep the last block, otherwise use only the full blocks
# Set to 1 if you only want full blocks.

if (NumSampOrSecs ==1 ){
  blocksamp <- (blocksec * samplerate)
}


##################################
## Nuts and Bolts

nsamp <- length(accel$time) # get total number of samples in the event
block <- 0 # represents the block of rows in the same behavior

lastblocksamp <- nsamp %% blocksamp
#finding number of "remainder" samples 

if (lastblocksamp > (blocksamp * lastblockmin)){
  nblocks <- (nsamp %/% blocksamp)+1
} else {
  nblocks <- (nsamp %/% blocksamp)
}
# checking if remainder samples are greater than threshold set in 
# lastblockmin above- if so, number of blocks = quotient + 1
# if not, blocks = quotient
# 
j <- 0
block <- 0
outmat <- "0"

for (i in 1:nsamp){
  j <- j +1
  if (j == 1){
    block <- block + 1
    outmat[block] <- paste(accel$ch.1[(blocksamp * (block-1))+ j],accel$ch.2[(blocksamp * (block-1))+ j], accel$ch.3[(blocksamp * (block-1))+ j], sep = ",")
    
  } else {
    outmat[block] <- paste(outmat[block], accel$ch.1[(blocksamp * (block-1))+ j],accel$ch.2[(blocksamp * (block-1))+ j], accel$ch.3[(blocksamp * (block-1))+ j], sep = ",")
    if (j == blocksamp) {
      j <- 0
    }
  }
} # end for loop


write.csv(outmat, outfile, row.names = FALSE)
# write "outmat" to the file named above
