#!/usr/bin/python3
# -*- coding: utf-8 -*-

"""Source Memory Online

An online version of the source memory experiment for Jason Zhou's
Ph.D thesis. This script (and the associated modules) handle the
server-side end of the requests: serving the experiment itself,
handling the submission of data from different participants, and
handling the request to download the data as JSON.

"""

import logging
import random
import string
from flask import Flask
from google.cloud import datastore
from google.cloud import storage
import google.cloud.logging

import datahandling

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
## The key within cookie headers that corresponds to the session ID.
SID_COOKIE_KEY = "SID"

## Master API key. Do not distribute this part of the source code
## publicly! This is used to verify requests coming from the Admin to
## download data.
MASTER_API_KEY = "zjFdXfQ64sgAVwQMx84IhzqzUPygpSguUkeLKLqQBIyxo8kP3yphBqF9ysd4IQsA"
MASTER_API_FIELD = "API_ACCESS_KEY"

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

## Set up the client connection to the datastore.
DATASTORE_CLIENT = datastore.Client()

## Set up the connection to the Google Cloud Storage.
STORAGE_CLIENT = storage.Client()

## Hook the standard Python logging library into the Google Cloud
## Logging system.
LOGGING_CLIENT = google.cloud.logging.Client()
LOGGING_HANDLER = google.cloud.logging.handlers.CloudLoggingHandler(LOGGING_CLIENT)
google.cloud.logging.handlers.setup_logging(LOGGING_HANDLER)
logging.getLogger().setLevel(logging.DEBUG)

## Session handling and participant routing
## ========================================
##
## Procedures for handling the session data and routing the user based
## on their current progress through the experiment.
def generate_sid():
    """Generate a session ID from 32 random digits and letters."""
    return ''.join(random.choices(string.ascii_letters + string.digits, k=32))

def check_master_api_key(req):
    """Check whether the master API key matches the global variable within
    this file.

    """
    expected = MASTER_API_FIELD + "=" + MASTER_API_KEY
    return req.headers.get("Authorization") == expected

def set_cookie(res, sid):
    """Set the cookie to a particular session ID in a response."""
    res.set_cookie(SID_COOKIE_KEY, sid)

def unset_cookie(res):
    """Unset any existing session ID within a cookie. This uses the
    standard hacky technique of just setting the cookie to a dummy
    string that has an expiration date in the past (Unix time 0).

    """
    res.set_cookie(SID_COOKIE_KEY, '', expires=0)

def get_cookie(req):
    """Get the session ID from a cookie."""
    return req.cookies.get(SID_COOKIE_KEY)

def next_step_from_request(req):
    """Get the next step from the provided request, potentially requiring
    (if the session ID is stored within a cookie) a call to the
    datastore.

    """
    sid = get_cookie(req)
    if sid is None:
        return "NoSession"
    return datahandling.next_step(DATASTORE_CLIENT, sid)

def user_status_is_error(user_status):
    """Return True if the current status string is an error of any type
    (invalid SID, SID not found in database, some error, an unknown
    state). Returns True by default because that means the case
    analysis is hitting an unknown user status type.

    """
    user_status = user_status.lower()
    if user_status in ("pls", "ethics", "experiment", "complete",
                       "calibration", "nosession"):
        return False
    if user_status in ("error", "notfound", "invalidsid",
                       "unknownstate"):
        return True
    logging.error("Unknown user status (%s) in user_status_is_error",
                  user_status)
    return True


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
