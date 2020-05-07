## sourcemem-online.R
##
## The top-level script for analysing the data from the online version
## of Jason Zhou's source memory experiment, including accessing the
## data from the Google App Engine instance, downloading the latest
## data (or working from a stored local copy), and plotting the data.


#####

## Connection parameters
SERVER.BASE.URL <- "https://jzhou-sourcemem-online.appspot.com"
SERVER.PORT <- NULL
SERVER.MASTER.API.KEY <- "" # <-- This needs to be in agreement with
                            # whatever is on the server.

#####

source("access-data.R")

## Check the experiment status.
server.status <- get.server.status(SERVER.BASE.URL,
                                   SERVER.PORT,
                                   SERVER.MASTER.API.KEY)
