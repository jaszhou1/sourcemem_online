
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

def construct_event(event_type, event):
    """Construct an event to add to the event array within a client
    session entity within the datastore.

    """
    return {
        "event_type": event_type,
        "event": event,
        "time": datetime.datetime.utcnow()
    }

def make_session(datastore_client,
                 session_id, external_id,
                 user_agent_string, xforwarded,
                 is_sim_present):
    """Construct a ClientSession entity and insert it into the datastore.
    This entity corresponds to a participant, and should be able to be
    accessed via their `session_id`, which is also stored on the
    client's computer in a cookie (and, so, accessible in the HTTP
    headers to determine who is who). This ClientSession also contains
    some information about the client display properties, the user
    agent and IP address of the client, as well as the stages of the
    experiment the participant has gone through.

    """
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
        "completion_code": False,
        "user_agent": user_agent_string,
        "ethics": False,
        "pls": False,
        "sim_present": is_sim_present,
        "events": [construct_event("user", "created")],
        "resolution": {"reported": "<unknown>",
                       "height": "<unknown>",
                       "width": "<unknown>",
                       "pixel_depth": "<unknown>",
                       "pixel_ratio": "<unknown>",
                       "colour_depth": "<unknown>"}
    })
    datastore_client.put(client_session)
    return client_session.key

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
    return session["completion_code"]

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

def get_user_status_from_session(session_entry):
    """Return the user status from a session object."""
    state = "UnknownState"
    if session_entry is None:
        state = "InvalidSid"
        return state
    try:
        if session_entry["pls"]:
            if session_entry["ethics"]:
                if session_entry["completion_code"]:
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
