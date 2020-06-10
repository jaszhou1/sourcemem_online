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
from flask import Flask, request, make_response, render_template, \
    redirect, url_for
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
                       "nosession"):
        return False
    if user_status in ("error", "notfound", "invalidsid",
                       "unknownstate"):
        return True
    logging.error("Unknown user status (%s) in user_status_is_error",
                  user_status)
    return True

def get_best_user_allocation():
    """Get the best allocation (true=sim; false=seq) for a new participant
    based on the currently allocated participants.

    """
    num_sim_users, num_seq_users = num_users()
    if num_sim_users > num_seq_users: # pylint: disable=no-else-return
        return False
    if num_seq_users > num_sim_users:
        return True
    return random.random() > 0.5

def get_user_between_subjects_status(req):
    """Get the Seq/Sim allocation of the user associated with the current
    request.

    """
    sid = get_cookie(req)
    return datahandling.get_between_subject_allocation_from_session_id(DATASTORE_CLIENT,
                                                                       sid)

def num_users():
    """Get the number of user sessions."""
    return datahandling.num_user_sessions(DATASTORE_CLIENT, True), \
        datahandling.num_user_sessions(DATASTORE_CLIENT, False)

def num_complete_datasets():
    """Get the number of complete data sets."""
    return datahandling.num_complete_datasets(DATASTORE_CLIENT, True), \
        datahandling.num_complete_datasets(DATASTORE_CLIENT, False)

def completed_users():
    """Get a list of completed user IDs."""
    return datahandling.completed_user_ids(DATASTORE_CLIENT)


## Error and template responses
## ============================
##
## Return standard issue template responses.
def not_found_error(attempted_url):
    """Return a "Not Found" error page."""
    return render_template("not-found-error.html",
                           url=attempted_url)

def server_error(message=None):
    """Return a "Not Found" error page."""
    return render_template("server-error.html",
                           message=message)

## Public endpoints
## ================
##
## Here are the public endpoints for enduser (participant) use.
@app.route("/entry", methods=["GET", "POST"])
def entry():
    """The entry point for users from SOMA, REP, or Amazon Mechanical Turk."""
    user_status = next_step_from_request(request).lower()
    status_is_error = user_status_is_error(user_status)
    if user_status not in ("invalidsid", "nosession", "notfound"):
        return redirect(url_for(".dispatch"))
    if request.method == "GET" or status_is_error:
        response = make_response(render_template("entry.html",
                                                 exp_name=EXPERIMENT_NAME,
                                                 msg=""))
        if user_status == "invalidsid":
            unset_cookie(response)
        return response
    if request.method == "POST":
        external_id = request.form.get("extid")
        x_forwarded = request.headers.getlist("X-Forwarded-For")
        user_agent = request.headers.get("User-Agent")
        new_sid = generate_sid()
        is_sim_present = get_best_user_allocation()
        if datahandling.is_valid_external_id(external_id):
            _ = datahandling.make_session(DATASTORE_CLIENT,
                                          new_sid,
                                          external_id,
                                          user_agent,
                                          x_forwarded,
                                          is_sim_present)
            response = redirect(url_for(".pls"))
            set_cookie(response, new_sid) # either do this or "after_this_request"
            return response
        return render_template("entry.html",
                               exp_name=EXPERIMENT_NAME,
                               msg="Please enter a valid external ID")
    return render_template("message.html",
                           msg="Invalid request type.")

@app.route("/pls", methods=["GET", "POST"])
def pls():
    """Display the plain language statement (PLS) to the client, getting
    them to acknowledge its contents before getting them to move on.
    If we get the affirmative from the participant, we set a flag on
    their client session entry in the datastore.

    """
    user_status = next_step_from_request(request).lower()
    if user_status != "pls":
        return redirect(url_for(".dispatch"))
    if request.method == "POST":
        _ = datahandling.set_pls_done(DATASTORE_CLIENT, get_cookie(request))
        return redirect(url_for(".dispatch"))
    return render_template("pls.html")

@app.route("/ethics", methods=["GET", "POST"])
def ethics():
    """Display the ethics statement to the client, getting them to
    acknowledge its contents before getting them to move on. If we get
    the affirmative from the participant, we set a flag on their
    client session entry in the datastore.

    """
    user_status = next_step_from_request(request).lower()
    if user_status != "ethics":
        return redirect(url_for(".dispatch"))
    if request.method == "POST":
        agree_value = request.form.get("ethics-agree")
        if agree_value == "agree":
            _ = datahandling.set_ethics_done(DATASTORE_CLIENT, get_cookie(request))
        return redirect(url_for(".dispatch"))
    return render_template("ethics.html")

@app.route("/experiment", methods=["GET"])
def experiment():
    """Show the actual experiment. Get or allocate an experimental slot to
    the participant and show the generated program text if available
    (otherwise show an error).

    """
    # user_status = next_step_from_request(request).lower()
    # if user_status != "experiment":
    #     return redirect(url_for(".dispatch"))
    # user_is_seq = get_user_between_subjects_status(request)
    # experiment_slot = allocate_or_get_experiment_slot(user_is_seq, request)
    # if experiment_slot:
    #     prog_text = experiment_slot["program_text"]
    #     return render_template("experiment.html",
    #                            exp_name=EXPERIMENT_NAME,
    #                            program_text=prog_text)
    # logging.error("No free slots available for users.")
    # return render_template("no-free-slots.html")
    return "Need to implement experiment return"

@app.route("/complete", methods=["GET"])
def complete():
    """Display a script that will be able to determine what settings the
    subsequent experiment should be.

    """
    user_status = next_step_from_request(request).lower()
    if user_status != "complete":
        return redirect(url_for(".dispatch"))
    sid = get_cookie(request)
    completion_code = datahandling.get_completion_code(DATASTORE_CLIENT,
                                                       sid)
    return render_template("completion.html",
                           completion_code=completion_code)

@app.route("/dispatch")
def dispatch():
    """This is the general user dispatch procedure that sends a user to
    the default location based on the latest ClientSession entity in
    the datastored associated with their session ID.

    """
    current_status = next_step_from_request(request).lower()
    if current_status == "pls":
        ## The user needs to acknowledge having sighted the Plain
        ## Language Statement. Redirect there.
        return redirect(url_for(".pls"))
    if current_status == "ethics":
        ## The user needs to agree to the ethics statement to proceed.
        ## Redirect there.
        return redirect(url_for(".ethics"))
    if current_status == "error":
        ## Some (unspecified) error. Send to error screen.
        return server_error("Unknown server error. Please contact " + \
                            "administrator at " + \
                            "<a href=\"mailto:lilburns@unimelb.edu.au\">" + \
                            "lilburns@unimelb.edu.au</a>."), 500
    if current_status == "notfound":
        ## Session not found. Clear the session ID and redirect back
        ## to the entry portal.
        return redirect(url_for(".entry"))
    if current_status == "experiment":
        ## Assigned to an experimental slot. Send to the experiment
        ## presentation.
        return redirect(url_for(".experiment"))
    if current_status == "complete":
        ## Experiment complete. Get the completion code.
        return redirect(url_for(".complete"))
    if current_status == "nosession":
        ## No session found. Send to entry portal.
        return redirect(url_for(".entry"))
    if current_status == "invalidsid":
        ## Invalid session ID. Clear the session ID and have a
        ## redirect back to the entry portal.
        raise NotImplementedError("Need to implement SID clearing and redirect here")
    if current_status == "unknownstate":
        logging.error("Unknown user status (%s) from next_step_from_request",
                      current_status)
    logging.error("Unknown user status (%s) in dispatch",
                  current_status)
    return server_error("Unknown server error. Please contact " + \
                        "administrator at " + \
                        "<a href=\"mailto:lilburns@unimelb.edu.au\">" + \
                        "lilburns@unimelb.edu.au</a>. " + \
                        "Cite: SID = %s / user_status = %s" % (str(get_cookie(request)),
                                                               current_status)), 500

@app.route("/")
def hello():
    """The handler for requests made to the root URL of the experiment.
    This is not the entrypoint for individual participants.

    """
    return "Hello world!"

if __name__ == "__main__":
    print("Source Memory Experiment Server")
    app.run(host="127.0.0.1", port=8080, debug=True)
