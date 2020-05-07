#!/usr/bin/python3
# -*- coding: utf-8 -*-

"""Source Memory Online

An online version of the source memory experiment for Jason Zhou's
Ph.D thesis. This script (and the associated modules) handle the
server-side end of the requests: serving the experiment itself,
handling the submission of data from different participants, and
handling the request to download the data as JSON.

"""

from flask import Flask

## Global parameters
## =================
##
## These are the global parameters for specifying things like the
## limits on the number of experimental slots to keep open, to specify
## the Cloud Storage bucket name, etc.

## Experiment name (for the browser title)
EXPERIMENT_NAME = "Source Memory Experiment"

## Google Cloud Storage parameters.
STORAGE_BUCKET_NAME = "jzhou-sourcemem-online"

## Global setup
## ============
##
## This is the set up for each of the global components (the Flask app
## that handles the requests, the datastore client that handles
## transactions with the database, the Cloud Storage client that
## handles reads and writes to the persistent storage for image data,
## and hooking up the internal Python logging to the App Engine way of
## doing things).

## Set up the Flask app.
app = Flask(__name__) # pylint: disable=invalid-name

## Public endpoints
## ================
##
## Here are the public endpoints for enduser (participant) use.

@app.route("/")
def hello():
    """The handler for requests made to the root URL of the experiment.
    This is not the entrypoint for individual participants.

    """
    return "Hello world!"

if __name__ == "__main__":
    print("Source Memory Experiment Server")
    app.run(host="127.0.0.1", port=8080, debug=True)
