#!/usr/bin/python3
# -*- coding: utf-8 -*-

"""Source Memory Online

An online version of the source memory experiment for Jason Zhou's
Ph.D thesis. This script (and the associated modules) handle the
server-side end of the requests: serving the experiment itself,
handling the submission of data from different participants, and
handling the request to download the data as JSON.

"""

import datetime
import logging
import random
import string
from flask import Flask, request, make_response, render_template, \
    redirect, url_for, jsonify
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

## Minimum rest period between sessions (in hours)
MINIMUM_SESSION_REST_HRS = 20 #20
WAIT_UNTIL_TOMORROW_HRS = 10 #10

## Google Cloud Storage parameters.
STORAGE_BUCKET_NAME = "jzhou-sourcemem-online"
## The key within cookie headers that corresponds to the session ID.
SID_COOKIE_KEY = "SID"

## Master API key. Do not distribute this part of the source code
## publicly! This is used to verify requests coming from the Admin to
## download data.
MASTER_API_KEY = "zjFdXfQ64sgAVwQMx84IhzqzUPygpSguUkeLKLqQBIyxo8kP3yphBqF9ysd4IQsA"
MASTER_API_FIELD = "API_ACCESS_KEY"
CHECK_MASTER_API = True

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
    if not CHECK_MASTER_API:
        return True
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
    return True

def get_user_between_subjects_status(req):
    """Get the Seq/Sim allocation of the user associated with the current
    request.

    """
    sid = get_cookie(req)
    return datahandling.get_between_subject_allocation_from_session_id(DATASTORE_CLIENT,
                                                                       sid)
def get_user_entry_point(req):
    """Get the entry point type of the user associated with the current
    request.

    """
    sid = get_cookie(req)
    return datahandling.get_entry_point_from_session_id(DATASTORE_CLIENT,
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

def started_users():
    """Get a list of user IDs that have any data stored."""
    return datahandling.started_user_ids(DATASTORE_CLIENT)

def get_experiment_data(userid):
    """Query the datastore for the user's data if it exists."""
    return datahandling.get_last_experiment_data_by_user(DATASTORE_CLIENT, userid)

def get_experiment_data_by_session_id(userid, sessionid):
    """Query the datastore for the user's data if it exists."""
    return datahandling.get_last_experiment_data_by_user_by_id(DATASTORE_CLIENT,
                                                               userid,
                                                               sessionid)

def get_user_information(userid):
    """Query the datastore for the user's information (session, ID, etc.
    as opposed to experimental data) if it exists.

    """
    return datahandling.get_user_information(DATASTORE_CLIENT, userid)

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

## Debug endpoints
## ===============
##
## Endpoints for internal administrator use. These should not be
## anything that is desperately private (nothing that would nuke the
## dataset or server or anything), but stuff that is not useful for
## participants.
@app.route("/debug/clear-sid")
def clear_sid():
    """An endpoint to clear the session ID from a cookie. This is for
    debugging purposes only.

    """
    res = make_response(render_template("message.html",
                                        msg="Your SID has been cleared."))
    unset_cookie(res)
    return res

## Experimenter endpoints
## ======================
##
## Endpoints for the experimenter to get access to the completed
## datasets.
@app.route("/completed-users")
def completed_users_endpoint():
    """Return a list of all completed user IDs."""
    if not check_master_api_key(request):
        return jsonify({
            "status": "Forbidden"
        }), 403
    return jsonify(completed_users())

@app.route("/started-users")
def started_users_endpoint():
    """Return a list of all user IDs with some data."""
    if not check_master_api_key(request):
        return jsonify({
            "status": "Forbidden"
        }), 403
    return jsonify(started_users())

@app.route("/get-user-data/<int:userid>/<int:sessionid>")
def get_user_data_by_id_handler(userid, sessionid):
    """Return the specified user data if it exists."""
    if not check_master_api_key(request):
        return jsonify({
            "status": "Forbidden"
        }), 403
    return jsonify(get_experiment_data_by_session_id(userid,
                                                     sessionid))

@app.route("/get-user-data/<int:userid>")
def get_user_data_handler(userid):
    """Return the specified user data if it exists."""
    if not check_master_api_key(request):
        return jsonify({
            "status": "Forbidden"
        }), 403
    return jsonify(get_experiment_data(userid))

@app.route("/get-user-information/<int:userid>")
def get_user_information_handler(userid):
    """Return the specified user information if it exists."""
    if not check_master_api_key(request):
        return jsonify({
            "status": "Forbidden"
        }), 403
    return jsonify(get_user_information(userid))

@app.route("/get-user-completions")
def get_user_completion_handler():
    if not check_master_api_key(request):
        return jsonify({
            "status": "Forbidden"
        }), 403
    return jsonify(datahandling.get_sessions_complete(DATASTORE_CLIENT))

## Public endpoints
## ================
##
## Here are the public endpoints for enduser (participant) use.

@app.route("/entry-direct", methods=["GET", "POST"])
def entry_direct():
    """Ask client what entry point they should be getting with a button
    and direct them to the appropriate route.
    """
    if request.method == "POST":
        entry_value = request.form.get("entrypoint")
        if entry_value == "rep":
            redirect(url_for(".entry-rep"))
            #return redirect("https://jzhou-sourcemem-online.uc.r.appspot.com/entry-rep") #Seems to redirect to dispatch if I use redirect(url_for(".entry-rep"))
        else:
            #return redirect("https://jzhou-sourcemem-online.uc.r.appspot.com/entry-public")
            redirect(url_for(".entry-public"))
    return render_template("entry-direct.html")

@app.route("/entry-rep", methods=["GET", "POST"])
def entry_rep():
    """The entry point for users from REP."""
    user_status = next_step_from_request(request).lower()
    status_is_error = user_status_is_error(user_status)
    if user_status not in ("invalidsid", "nosession", "notfound"):
        return redirect(url_for(".dispatch"))
    if request.method == "GET" or status_is_error:
        response = make_response(render_template("entry-rep.html",
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
        is_rep = 1 #Entry point tag
        if datahandling.is_valid_external_id(external_id):
            _, sid = datahandling.make_or_get_session(DATASTORE_CLIENT,
                                                      new_sid,
                                                      external_id,
                                                      user_agent,
                                                      x_forwarded,
                                                      is_sim_present,
                                                      is_rep)
            response = redirect(url_for(".pls"))
            set_cookie(response, sid) # either do this or "after_this_request"
            return response
        return render_template("entry-rep.html",
                               exp_name=EXPERIMENT_NAME,
                               msg="Please enter a valid external ID")
    return render_template("message.html",
                           msg="Invalid request type.")

@app.route("/entry-public", methods=["GET", "POST"])
def entry_public():
    """The entry point for users from Prolific."""
    user_status = next_step_from_request(request).lower()
    status_is_error = user_status_is_error(user_status)
    if user_status not in ("invalidsid", "nosession", "notfound"):
        return redirect(url_for(".dispatch"))
    if request.method == "GET" or status_is_error:
        response = make_response(render_template("entry-public.html",
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
        is_rep = 0
        if datahandling.is_valid_external_id(external_id):
            _, sid = datahandling.make_or_get_session(DATASTORE_CLIENT,
                                                      new_sid,
                                                      external_id,
                                                      user_agent,
                                                      x_forwarded,
                                                      is_sim_present,
                                                      is_rep)
            response = redirect(url_for(".pls"))
            set_cookie(response, sid) # either do this or "after_this_request"
            return response
        return render_template("entry-public.html",
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
    user_status = next_step_from_request(request).lower()
    if user_status != "experiment":
        return redirect(url_for(".dispatch"))
    sid = get_cookie(request)
    user_is_sim = get_user_between_subjects_status(request)
    ## Check the whether we have session 1.
    completed_sessions = datahandling.get_completed_experimental_sessions(DATASTORE_CLIENT, sid)
    if len(completed_sessions) > 0:
        WAIT_PERIOD = datetime.timedelta(hours=MINIMUM_SESSION_REST_HRS)
        ## Are we allowed to continue on? Has the last session been
        ## completed too recently?
        last_completed_session = max(completed_sessions.values(),
                                     key=lambda x: x["completed"])
        current_utc_time = datetime.datetime.now(datetime.timezone.utc)
        time_rested = current_utc_time - last_completed_session["completed"]
        if time_rested < WAIT_PERIOD:
            ALMOST_READY = datetime.timedelta(hours=MINIMUM_SESSION_REST_HRS - 1)
            rested_hours = time_rested.seconds / (60 * 60)
            time_text = ""
            if time_rested > ALMOST_READY:
                time_text += "You are almost ready to begin the next" + \
                    " session. Please wait at least one more hour."
            elif time_rested < datetime.timedelta(hours=WAIT_UNTIL_TOMORROW_HRS):
                time_text += "You will need to wait until tomorrow before the next session."
            else:
                time_text += "You must wait " + \
                    str(round(MINIMUM_SESSION_REST_HRS - rested_hours)) + \
                    " more hours."
            return render_template("you-need-rest.html",
                                   wait_time=MINIMUM_SESSION_REST_HRS+4,
                                   time_text=time_text)
    has_completed_first_session = "1" in completed_sessions.keys()
    has_completed_second_session = "2" in completed_sessions.keys()
    has_completed_third_session = "3" in completed_sessions.keys()
    has_completed_fourth_session = "4" in completed_sessions.keys()
    has_completed_fifth_session = "5" in completed_sessions.keys()
    has_completed_sixth_session = "6" in completed_sessions.keys()
    has_completed_seventh_session = "7" in completed_sessions.keys()
    has_completed_eigth_session = "8" in completed_sessions.keys()
    has_completed_ninth_session = "9" in completed_sessions.keys()
    has_completed_tenth_session = "10" in completed_sessions.keys()
    if has_completed_first_session and has_completed_second_session and has_completed_third_session and has_completed_fourth_session and has_completed_fifth_session and has_completed_sixth_session and has_completed_seventh_session and has_completed_eigth_session and has_completed_ninth_session and has_completed_tenth_session:
        logging.warning("User completed all sessions in experiment handler")
    if not has_completed_first_session:
        return render_template("experiment-s1.html")
    if not has_completed_second_session:
        return render_template("experiment-s2.html")
    if not has_completed_third_session:
        return render_template("experiment-s3.html")
    if not has_completed_fourth_session:
        return render_template("experiment-s4.html")
    if not has_completed_fifth_session:
        return render_template("experiment-s5.html")
    if not has_completed_sixth_session:
        return render_template("experiment-s6.html")
    if not has_completed_seventh_session:
        return render_template("experiment-s7.html")
    if not has_completed_eigth_session:
        return render_template("experiment-s8.html")
    if not has_completed_ninth_session:
        return render_template("experiment-s9.html")
    return render_template("experiment-s10.html")

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

@app.route("/session-complete", methods=["GET"])
def session_complete():
    """Display a message which indicates that a session is complete, not
    the entire experiment.

    """

    user_is_rep = get_user_entry_point(request) # 1 = REP, 0 = Public
    next_step = next_step_from_request(request)
    sid = get_cookie(request)
    if user_is_rep:
        if next_step != "experiment": # No sessions remaining to complete?
            return redirect(url_for(".dispatch"))
        return render_template("session-complete.html")
    else:
        # Redirect participant to Prolific Completion URL (assigned by Prolific.co)
        completed_sessions = datahandling.get_completed_experimental_sessions(DATASTORE_CLIENT, sid)
        has_completed_first_session = "1" in completed_sessions.keys()
        has_completed_second_session = "2" in completed_sessions.keys()
        has_completed_third_session = "3" in completed_sessions.keys()
        has_completed_fourth_session = "4" in completed_sessions.keys()
        has_completed_fifth_session = "5" in completed_sessions.keys()
        has_completed_sixth_session = "6" in completed_sessions.keys()
        has_completed_seventh_session = "7" in completed_sessions.keys()
        has_completed_eigth_session = "8" in completed_sessions.keys()
        has_completed_ninth_session = "9" in completed_sessions.keys()
        has_completed_tenth_session = "10" in completed_sessions.keys()
        if has_completed_first_session and has_completed_second_session and has_completed_third_session and has_completed_fourth_session and has_completed_fifth_session and has_completed_sixth_session and has_completed_seventh_session and has_completed_eigth_session and has_completed_ninth_session and has_completed_tenth_session:
            return redirect("https://app.prolific.co/submissions/complete?cc=1F3FD55D")
        elif has_completed_first_session and has_completed_second_session and has_completed_third_session and has_completed_fourth_session and has_completed_fifth_session and has_completed_sixth_session and has_completed_seventh_session and has_completed_eigth_session and has_completed_ninth_session:
            return redirect("https://app.prolific.co/submissions/complete?cc=5C13E144")
        elif has_completed_first_session and has_completed_second_session and has_completed_third_session and has_completed_fourth_session and has_completed_fifth_session and has_completed_sixth_session and has_completed_seventh_session and has_completed_eigth_session:
            return redirect("https://app.prolific.co/submissions/complete?cc=75C12AC2")
        elif has_completed_first_session and has_completed_second_session and has_completed_third_session and has_completed_fourth_session and has_completed_fifth_session and has_completed_sixth_session and has_completed_seventh_session:
            return redirect("https://app.prolific.co/submissions/complete?cc=7D122125")
        elif has_completed_first_session and has_completed_second_session and has_completed_third_session and has_completed_fourth_session and has_completed_fifth_session and has_completed_sixth_session:
            return redirect("https://app.prolific.co/submissions/complete?cc=2FB75361")
        elif has_completed_first_session and has_completed_second_session and has_completed_third_session and has_completed_fourth_session and has_completed_fifth_session:
            return redirect("https://app.prolific.co/submissions/complete?cc=39530C08")
        elif has_completed_first_session and has_completed_second_session and has_completed_third_session and has_completed_fourth_session:
            return redirect("https://app.prolific.co/submissions/complete?cc=EDAA64B1")
        elif has_completed_first_session and has_completed_second_session and has_completed_third_session:
            return redirect("https://app.prolific.co/submissions/complete?cc=60DD1460")
        elif has_completed_first_session and has_completed_second_session:
            return redirect("https://app.prolific.co/submissions/complete?cc=24183B65")
        elif has_completed_first_session:
            return redirect("https://app.prolific.co/submissions/complete?cc=235C5D3D")
        else:
            return redirect(url_for(".dispatch")) # Catch-all?

@app.route("/submit-data/<int:sessionid>", methods=["POST"])
def submit_data_handler(sessionid):
    """POST requests to this endpoint will be add the final experimental
data, if valid, to the database."""
    if next_step_from_request(request).lower() != "experiment":
        return "Experimental data received from invalid user", 403
    sid = get_cookie(request)
    if sessionid not in [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]:
        return "Invalid session ID", 403
    is_valid, data_dict, data_str = \
        datahandling.convert_experimental_data(sid, request.json)
    if not is_valid:
        logging.warning("User " + sid + " submitted invalid data: " +
                        data_str)
        return "Invalid data: " + data_str, 403
    success = datahandling.valid_data_received(DATASTORE_CLIENT, sid,
                                               sessionid, data_dict)
    if not success:
        return jsonify({
            "success": False,
            "message": "Error in storing data"
        })
    return jsonify({
        "success": True,
        "message": "Ready for completion"
    })

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
        return redirect(url_for(".entry-direct"))
    if current_status == "experiment":
        ## Assigned to an experimental slot. Send to the experiment
        ## presentation.
        return redirect(url_for(".experiment"))
    if current_status == "complete":
        ## Experiment complete. Get the completion code.
        return redirect(url_for(".complete"))
    if current_status == "nosession":
        ## No session found. Send to entry portal.
        return redirect(url_for(".entry-direct"))
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

@app.route("/report-display-characteristics", methods=["POST"])
def report_display_characteristics():
    # pylint: disable=invalid-name
    """POST requests to this endpoint will store some basic details about
    the client's display characteristics.

    """
    DISPLAY_TYPES = {
        '0': "Laptop",
        '1': "Desktop"
    }
    if next_step_from_request(request).lower() != "experiment":
        return "Calibration data received from invalid user", 403
    sid = get_cookie(request)
    display_data = request.json
    if "button_pressed" in display_data:
        reported_display_type = DISPLAY_TYPES.get(display_data["button_pressed"],
                                                  "<unknown>")
    else:
        reported_display_type = "<unknown>"

    width = display_data.get("screen_width", "<unknown>")
    height = display_data.get("screen_height", "<unknown>")
    pixel_ratio = display_data.get("pixel_ratio", "<unknown>")
    pixel_depth = display_data.get("pixel_depth", "<unknown>")
    colour_depth = display_data.get("colour_depth", "<unknown>")
    datahandling.update_resolution_data(DATASTORE_CLIENT, sid,
                                        reported_display_type,
                                        width, height, pixel_ratio,
                                        pixel_depth,
                                        colour_depth)
    return jsonify({"status": "OK"})

@app.route("/")
def hello():
    """The handler for requests made to the root URL of the experiment.
    This is not the entrypoint for individual participants.

    """
    return render_template("landing.html")

if __name__ == "__main__":
    print("Source Memory Experiment Server")
    app.run(host="127.0.0.1", port=8080, debug=True)
