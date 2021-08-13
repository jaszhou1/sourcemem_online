
"""Procedures for interfacing with the Google Datastore, for inserting
and retrieving session data, and for inserting and retrieving
experimental data.

Simon Lilburn <lilburns AT unimelb DOT edu DOT au>

"""

import datetime
import logging
import random
import string
from google.cloud import datastore

CLIENT_SESSION_KEY = "ClientSession"
EXPERIMENTAL_DATA_KEY = "ExperimentalData"
TOTAL_NUM_SESSIONS = 10 ## <-- this is the number of sessions that need to be
                       ##     associated with a client session until they
                       ##     have the "completed" switch turned on their
                       ##     session entity.

# Large (but not insanely large) numbers for upper limit on query
# results.
MAX_DATASETS = 10000
MAX_USER_SESSIONS = 10000

def generate_completion_code():
    """Generate a complete code from 8 random digits and letters."""
    return ''.join(random.choices(string.ascii_letters + string.digits, k=8))

def is_valid_external_id(extid):
    """Check whether an external identifier (REP student number or Amazon
    Mechanical Turk ID) is valid.
    """
    return len(extid) > 1 # FIXME: This just returns true if there's
                          # more than one character in the external
                          # ID.

def missing_fields(trial_dict, fields):
    """Returns true if any of the fields are missing xfrom the trial_dict
    keys.

    """
    keys = trial_dict.keys()
    return any(map(lambda f: f not in keys, fields))

def convert_experimental_data(session_id, data):
    """Convert the experiment data from the JSON provided by the client
(hopefully from jsPsych) to a data structure we can put in the
datastore. This function returns three elements: a boolean indicating
whether the data was valid, a dictionary if the data parsed correctly
(or a None value if the data was not correct), and either the original
data handed in or (if the data was invalid) a message to store in the
datastore indicating the nature of the error.

    """
    ## We expect to receive a JSON structure that is a list of trial
    ## elements.
    if not isinstance(data, list):
        return False, None, "JSON did not contain list"
    ## The required fields for each type of trial.
    REQUIRED_CONF_FIELDS = ["trial_type", "rt", "block",
                            "trial", "trial_index", "response", "stimulus",
                            "key_press", "time_elapsed"]
    REQUIRED_PRESENT_FIELDS = ["trial_type", "num_fast_attempts",
                               "num_slow_attempts", "num_error_attempts",
                               "stimulus_word", "stimulus_angle",
                               "hitting_angle", "hitting_position", "angular_error",
                               "response_time", "display_angle_time", "display_word_time",
                               "block", "trial","trial_index", "time_elapsed"]
    REQUIRED_DISTRACTOR_FIELDS = ["trial_type", "subj_responses",
                                  "num1", "num2", "num3", "sums",
                                  "rts", "block", "trial",
                                  "trial_index", "time_elapsed"]
    REQUIRED_RECALL_FIELDS = ["trial_type", "num_fast_attempts",
                              "num_slow_attempts", "stimulus_word", "stimulus_angle",
                              "hitting_position", "hitting_angle",
                              "response_time", "display_time", "trial_index", "time_elapsed",
                              "correct", "angular_error"]
    trials = []
    present_trials = []
    recall_trials = []
    math_distractors = []
    confidence_trials = []
    for trial in data:
        if not isinstance(trial, dict):
            return False, None, "Non-dict element in JSON array"
        if "trial_type" not in trial:
            return False, None, "Dictionary did not contain trial type"
        if trial["trial_type"] == "html-keyboard-response":
            if "response" in trial:
                if missing_fields(trial, REQUIRED_CONF_FIELDS):
                    return False, None, "Missing fields in old-new confidence trials."
                this_trial = {
                    "trial_type": "confidence",
                    "rt": trial["rt"],
                    "block": trial["block"],
                    "trial": trial["trial"],
                    "index": trial["trial_index"],
                    "response": trial["response"],
                    "stimulus": trial["stimulus"],
                    "keycode": trial["key_press"],
                    "time_elapsed": trial["time_elapsed"]
                }
                trials.append(this_trial)
                confidence_trials.append(this_trial)
            else:
                continue
        elif trial["trial_type"] in ["contmemory-present",
                                     "contmemory-present-seq"]:
            if missing_fields(trial, REQUIRED_PRESENT_FIELDS):
                return False, None, "Missing fields in contmemory-present trials."
            sequential = trial["trial_type"] == "contmemory-present-seq"
            this_trial = {
                "trial_type": "present",
                "presentation_sequential": sequential,
                "num_fast_attempts": trial["num_fast_attempts"],
                "num_slow_attempts": trial["num_slow_attempts"],
                "num_error_attempts": trial["num_error_attempts"],
                "target_word": trial["stimulus_word"],
                "target_angle": trial["stimulus_angle"],
                "hitting_position_x": trial["hitting_position"][0],
                "hitting_position_y": trial["hitting_position"][1],
                "hitting_angle": trial["hitting_angle"],
                "angular_error": trial["angular_error"],
                "rt": trial["response_time"],
                "display_angle_time": trial["display_angle_time"],
                "display_word_time": trial["display_word_time"],
                "block": trial["block"],
                "trial": trial["trial"],
                "index": trial["trial_index"],
                "time_elapsed": trial["time_elapsed"]
            }
            trials.append(this_trial)
            present_trials.append(this_trial)
        elif trial["trial_type"] == "math-distractor":
            if missing_fields(trial, REQUIRED_DISTRACTOR_FIELDS):
                return False, None, "Missing fields in math-distractor"
            this_trial = {
                "trial_type": "math-distractor",
                "responses": trial["subj_responses"],
                "rt": trial["rts"],
                "num1": trial["num1"],
                "num2": trial["num2"],
                "num3": trial["num3"],
                "sums": trial["sums"],
                "index": trial["trial_index"],
                "time_elapsed": trial["time_elapsed"]
            }
            trials.append(this_trial)
            math_distractors.append(this_trial)
        elif trial["trial_type"] == "contmemory-recall":
            if missing_fields(trial, REQUIRED_RECALL_FIELDS):
                return False, None, "Missing fields in contmemory-recall trials"
            this_trial = {
                "trial_type": "recall",
                "num_fast_attempts": trial["num_fast_attempts"],
                "num_slow_attempts": trial["num_slow_attempts"],
                "hitting_position_x": trial["hitting_position"][0],
                "hitting_position_y": trial["hitting_position"][1],
                "hitting_angle": trial["hitting_angle"],
                "angular_error": trial["angular_error"],
                "block": trial["block"],
                "trial": trial["trial"],
                "correct": trial["correct"],
                "target_word": trial["stimulus_word"],
                "target_angle": trial["stimulus_angle"],
                "response_time": trial["response_time"],
                "display_time": trial["display_time"],
                "time_elapsed": trial["time_elapsed"],
                "index": trial["trial_index"]
            }
            trials.append(this_trial)
            recall_trials.append(this_trial)
        elif trial["trial_type"] == "call-function":
            continue ## Ignore this type of trial
        else:
            return False, None, "Unexpected trial type in data: " + trial["trial_type"]
    return True, {
        "created": datetime.datetime.utcnow(),
        "session_id": session_id,
        "trials": trials,
        "present_trials": present_trials,
        "recall_trials": recall_trials,
        "math_distractors": math_distractors,
        "confidence_trials": confidence_trials
    }, data

def construct_event(event_type, event):
    """Construct an event to add to the event array within a client
    session entity within the datastore.

    """
    return {
        "event_type": event_type,
        "event": event,
        "time": datetime.datetime.utcnow()
    }

def update_resolution_data(datastore_client, session_id,
                           reported_display_type,
                           width, height, pixel_ratio,
                           pixel_depth, colour_depth):
    """Get a user's ClientSession entity by their session ID and update
    the resolution/display characteristics data using the report from
    the calibration trials.

    """
    with datastore_client.transaction():
        ## Get the user entity
        user_query = datastore_client.query(kind=CLIENT_SESSION_KEY)
        user_query.add_filter("session_id", "=", session_id)
        user_query.order = ["-created"]
        user = list(user_query.fetch(1))
        if user is None or len(user) < 1:
            return False
        user = user[0]

        res_dict = {
            "reported": reported_display_type,
            "height": height,
            "width": width,
            "pixel_depth": pixel_depth,
            "pixel_ratio": pixel_ratio,
            "colour_depth": colour_depth
        }

        if "resolution" in user:
            user["resolution"].update(res_dict)
        else:
            user["resolution"] = res_dict

        datastore_client.put(user)

def make_or_get_session(datastore_client,
                        session_id, external_id,
                        user_agent_string, xforwarded,
                        is_sim_present, is_rep):
    """Check whether an external ID is in the database. If it is, retrieve
    the session ID, and return it. Otherwise, construct a
    ClientSession entity and insert it into the datastore. This entity
    corresponds to a participant, and should be able to be accessed
    via their `session_id`, which is also stored on the client's
    computer in a cookie (and, so, accessible in the HTTP headers to
    determine who is who). This ClientSession also contains some
    information about the client display properties, the user agent
    and IP address of the client, as well as the stages of the
    experiment the participant has gone through.

    """
    with datastore_client.transaction():
        ## Check whether the external ID already exists in the
        ## datastore.
        user_query = datastore_client.query(kind=CLIENT_SESSION_KEY)
        user_query.add_filter("external_id", "=", external_id)
        user_query.order = ["-created"]
        user_entities = list(user_query.fetch(1))
        if len(user_entities) > 0:
            return user_entities[0].key, user_entities[0]["session_id"]

        ## Create a database entity if the external ID does not exist
        ## in the datastore.
        key = datastore_client.key(CLIENT_SESSION_KEY)
        client_session = datastore.Entity(key,
                                          exclude_from_indexes=("xforwarded", "user_agent",
                                                                "completions", "events",
                                                                "resolution"))
        client_session.update({
            "created": datetime.datetime.utcnow(),
            "session_id": session_id,
            "external_id": external_id,
            "xforwarded": xforwarded,
            "completed": False,
            "started": False,
            "completions": {},
            "user_agent": user_agent_string,
            "ethics": False,
            "pls": False,
            "sim_present": is_sim_present,
            "is_rep": is_rep,
            "events": [construct_event("user", "created")],
            "resolution": {"reported": "<unknown>",
                           "height": "<unknown>",
                           "width": "<unknown>",
                           "pixel_depth": "<unknown>",
                           "pixel_ratio": "<unknown>",
                           "colour_depth": "<unknown>"}
        })
        datastore_client.put(client_session)
    return client_session.key, session_id

def get_completed_experimental_sessions(datastore_client, session_id):
    """Get the completed experimental sessions for a client session
    associated with a particular session ID.

    """
    user_query = datastore_client.query(kind=CLIENT_SESSION_KEY)
    user_query.add_filter("session_id", "=", session_id)
    user_query.order = ["-created"]
    user_entities = list(user_query.fetch(1))
    if user_entities and len(user_entities) > 0:
        return user_entities[0].get("completions", {})
    return False

def get_completed_experimental_sessions_by_id(datastore_client,
                                              session_id, experimental_session_id):
    """Get only the completed experimental sessions for a specific user,
    filtering by a particular experimental session ID.

    """
    completed_sessions = get_completed_experimental_sessions(datastore_client,
                                                             session_id)
    if completed_sessions:
        return completed_sessions.get(experimental_session_id)
    return False

def data_id_from_session_id(datastore_client, session_id):
    """Get the key for the most recent datastore entry associated with the
    provided session ID, if any datastore entry exists.

    """
    query = datastore_client.query(kind=CLIENT_SESSION_KEY)
    query.add_filter("session_id", "=", session_id)
    query.order = ['-created']
    query.keys_only()
    all_keys = list(query.fetch(1))
    if all_keys is None or len(all_keys) < 1:
        return None
    return all_keys[0]

def last_session_from_session_id(datastore_client, session_id):
    """Get the last session entry in the datastore associated with the
    provided session ID, if any datastore entry exists.

    """
    query = datastore_client.query(kind=CLIENT_SESSION_KEY)
    query.add_filter("session_id", "=", session_id)
    query.order = ['-created']
    last_session = list(query.fetch(1))
    if last_session is None or len(last_session) < 1:
        return None
    return last_session[0]

def get_completion_code(datastore_client, session_id):
    """Get the completion code from the user session."""
    session = last_session_from_session_id(datastore_client, session_id)
    ## Get the last completion.
    if len(session["completions"]) > 0:
        last_completion = max(session["completions"].values(), key=lambda x: x["completed"])
        return last_completion["completion_code"]
    return False

def valid_data_received(datastore_client, session_id, experimental_session_id,
                        data_dictionary):
    """We have received valid experimental session data. Update the status
    of the user. Returns a version of the data that can be stored to
    disk as a backup.

    """
    with datastore_client.transaction():
        ## Get the user entity
        user_query = datastore_client.query(kind=CLIENT_SESSION_KEY)
        user_query.add_filter("session_id", "=", session_id)
        user_query.order = ["-created"]
        user = list(user_query.fetch(1))
        if user is None or len(user) < 1:
            return False
        user = user[0]

        ## Create a valid data entry
        data_dictionary["user"] = user.key
        data_dictionary["session_number"] = experimental_session_id
        data_key = datastore_client.key(EXPERIMENTAL_DATA_KEY)
        experiment_data = datastore.Entity(data_key,
                                           exclude_from_indexes=("trials",
                                                                 "present_trials",
                                                                 "recall_trials",
                                                                 "math_distractors",
                                                                 "confidence_trials"))
        experiment_data.update(data_dictionary)
        datastore_client.put(experiment_data)

        ## Update the user entity.
        user["started"] = True
        user["completions"][str(experimental_session_id)] = {
            "experimental_session_id": experimental_session_id,
            "completed": datetime.datetime.utcnow(),
            "completion_code": generate_completion_code()
        }
        if len(user["completions"]) >= TOTAL_NUM_SESSIONS:
            user["completed"] = True
        user["events"].append(construct_event("experiment-session", "complete"))
        datastore_client.put(user)
    return True

def get_last_experiment_data_by_user(datastore_client, user_id):
    """Return the last experimental data added by the user."""
    data_query = datastore_client.query(kind=EXPERIMENTAL_DATA_KEY)
    data_query.add_filter("user", "=", datastore_client.key(CLIENT_SESSION_KEY, user_id))
    data_query.order = ["-created"]
    query_result = list(data_query.fetch(1))
    if query_result and len(query_result) > 0:
        return data_to_json_safe(query_result[0])
    return False

def get_last_experiment_data_by_user_by_id(datastore_client, user_id,
                                           session_id):
    """Return the last experimental data added by the user with the
    experimental session ID constraint.

    """
    data_query = datastore_client.query(kind=EXPERIMENTAL_DATA_KEY)
    data_query.add_filter("user", "=", datastore_client.key(CLIENT_SESSION_KEY, user_id))
    data_query.add_filter("session_number", "=", session_id)
    data_query.order = ["-created"]
    query_result = list(data_query.fetch(1))
    if query_result and len(query_result) > 0:
        return data_to_json_safe(query_result[0])
    return False

def get_user_information(datastore_client, user_id):
    """Return the user as specified by the `user_id`, if they exist."""
    #user_query = datastore_client.query(kind=CLIENT_SESSION_KEY)
    user_key = datastore_client.key(CLIENT_SESSION_KEY, user_id)
    user = datastore_client.get(user_key)
    if user:
        return data_to_json_safe(user)
    return False

def get_sessions_complete(datastore_client):
    """Return a dictionary of the sessions complete by users who have started."""
    data_query = datastore_client.query(kind=CLIENT_SESSION_KEY)
    data_query.add_filter("started", "=", True)
    query_result = list(data_query.fetch(MAX_USER_SESSIONS))
    res = {}
    for user in query_result:
        res[str(user.key.id_or_name)] = {
            "completed_sessions": list(map(lambda x: x.get("experimental_session_id",
                                                           "Missing session ID"),
                                           user["completions"].values())),
            "external_id": user.get("external_id", "Missing external ID")
        }
    return res

def set_ethics_done(datastore_client, session_id):
    """Set the ethics flag on the most recent client session associated
    with a session ID to True, indicating that ethics has been
    obtained. This also adds a client event to the event array to the
    session entity for auditing purposes.

    """
    last_session = last_session_from_session_id(datastore_client, session_id)
    ## Bail out if the session ID is not found in the datastore
    if last_session is None:
        return False
    last_session["ethics"] = True
    last_session["events"].append(construct_event("ethics", "done"))
    datastore_client.put(last_session)
    return True

def set_ethics_undone(datastore_client, session_id):
    """Set the ethics flag on the most recent client session associated
    with a session ID to False, indicating that ethics has not been
    obtained. This also adds a client event to the event array to the
    session entity for auditing purposes. (False is the default value
    for ethics completion and calling this procedure is principally
    for debugging purposes.)

    """
    last_session = last_session_from_session_id(datastore_client, session_id)
    ## Bail out if the session ID is not found in the datastore
    if last_session is None:
        return False
    last_session["ethics"] = False
    last_session["events"].append(construct_event("ethics", "undone"))
    datastore_client.put(last_session)
    return True

def set_pls_undone(datastore_client, session_id):
    """Set the PLS flag on the most recent client session associated with
    a particular session ID to False, indicating that the Plain
    Language Statement has not been sighted by the user. This also
    adds a client event to the event array to the session entity for
    auditing purposes.

    """
    last_session = last_session_from_session_id(datastore_client, session_id)
    ## Bail out if the session ID is not found in the datastore
    if last_session is None:
        return False
    last_session["pls"] = False
    last_session["events"].append(construct_event("pls", "undone"))
    datastore_client.put(last_session)
    return True

def set_pls_done(datastore_client, session_id):
    """Set the PLS flag on the most recent client session associated with
    a particular session ID to True, indicating that the Plain
    Language Statement has been sighted by the user. This also adds a
    client event to the event array to the session entity for auditing
    purposes.
    """
    last_session = last_session_from_session_id(datastore_client, session_id)
    ## Bail out if the session ID is not found in the datastore
    if last_session is None:
        return False
    last_session["pls"] = True
    last_session["events"].append(construct_event("pls", "done"))
    datastore_client.put(last_session)
    return True

def json_safe_element(element):
    """Make a particular value JSON safe."""
    if isinstance(element, datetime.datetime):
        return str(element)
    if isinstance(element, datastore.Key):
        return str(element.id_or_name)
    if isinstance(element, dict):
        return data_to_json_safe(element)
    if isinstance(element, list):
        return list(map(json_safe_element, element))
    return element

def data_to_json_safe(data):
    # pylint: disable=invalid-name
    """General procedure for handling the conversion of a data entity to a
    JSON safe format.

    """
    res = {}
    for k, v in data.items():
        res[k] = json_safe_element(v)
    if hasattr(data, 'key'):
        res["key"] = json_safe_element(data.key)
    return res

def num_complete_datasets(datastore_client, is_sim_present):
    """Return the number of completed datasets."""
    query = datastore_client.query(kind=EXPERIMENTAL_DATA_KEY)
    query.add_filter("sim_present", "=", is_sim_present)
    query.add_filter("completed", "=", True)
    query.keys_only()
    num_datasets = query.fetch(MAX_DATASETS)
    if num_datasets is None:
        return 0
    return len(list(num_datasets))

def num_started_datasets(datastore_client, is_sim_present):
    """Return the number of completed datasets."""
    query = datastore_client.query(kind=EXPERIMENTAL_DATA_KEY)
    query.add_filter("sim_present", "=", is_sim_present)
    query.add_filter("started", "=", True)
    query.keys_only()
    num_datasets = query.fetch(MAX_DATASETS)
    if num_datasets is None:
        return 0
    return len(list(num_datasets))

def num_user_sessions(datastore_client, is_sim_present):
    """Return the number of completed datasets."""
    query = datastore_client.query(kind=CLIENT_SESSION_KEY)
    query.add_filter("sim_present", "=", is_sim_present)
    query.keys_only()
    num_users = query.fetch(MAX_DATASETS)
    if num_users is None:
        return 0
    return len(list(num_users))

def completed_user_ids(datastore_client):
    """Return a list of the ClientSessions that have a completion code."""
    query = datastore_client.query(kind=CLIENT_SESSION_KEY)
    query.add_filter("completed", "=", True)
    query.keys_only()
    user_ids = query.fetch(MAX_DATASETS)
    if user_ids is None:
        return []
    return list(map(lambda x: str(x.key.id_or_name), user_ids))

def started_user_ids(datastore_client):
    """Return a list of the ClientSessions that have any data code."""
    query = datastore_client.query(kind=CLIENT_SESSION_KEY)
    query.add_filter("started", "=", True)
    query.keys_only()
    user_ids = query.fetch(MAX_DATASETS)
    if user_ids is None:
        return []
    return list(map(lambda x: str(x.key.id_or_name), user_ids))

def get_user_status_from_session(session_entry):
    """Return the user status from a session object."""
    state = "UnknownState"
    if session_entry is None:
        state = "InvalidSid"
        return state
    try:
        if session_entry["pls"]:
            if session_entry["ethics"]:
                if session_entry["completed"]:
                    state = "complete"
                else:
                    state = "experiment"
            else:
                state = "ethics"
        else:
            state = "PLS"
    except Exception as exce:
        logging.error("Key (%s) not found in session entry: ", str(exce))
        return "NotFound"
    return state

def next_step(datastore_client, session_id):
    """Get the last entry within the datastore associated with a
    particular session ID and check to see what their next step
    on progress through the experiment should be.

    """
    session_entry = last_session_from_session_id(datastore_client,
                                                 session_id)
    state = get_user_status_from_session(session_entry)
    return state

def get_between_subject_allocation_from_session_id(datastore_client,
                                                   session_id):
    """Get the between-subject allocation from the session ID."""
    session_entry = last_session_from_session_id(datastore_client,
                                                 session_id)
    return session_entry.get("sim_present")

def get_entry_point_from_session_id(datastore_client,
                                    session_id):
    """Get the user entry point type from the session ID."""
    session_entry = last_session_from_session_id(datastore_client,
                                                 session_id)
    return session_entry.get("is_rep")
