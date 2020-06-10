---
title: Deploying the experiment to the Google Cloud Platform
author: Simon Lilburn
date: June 9, 2020
---

This is a short guide to "deploying" your experiment on to the Google
Cloud Platform. "Deploy" is just tech jargon for "get the output of
some code into a state where it can actually be used by the end
users". Before we get to the preliminaries, here's a very quick cook's
tour through what Google Cloud Services is and why we need it in the
current case.

The [Google Cloud
Platform](https://en.wikipedia.org/wiki/Google_Cloud_Platform) (or
GCP) is a set of services provided by Google for using their (very
extensive) server and computing infrastructure. It is similar to
Amazon Web Services (AWS) and the Google App Engine, the primary
service we will use in the current experiment, is similar to other
virtualisation-based/serverless "platforms" (technically PaaS,
*platform-as-a-service*[^1]) like Heroku. It comprises a large number
of different services for different use cases, including many services
that are used for mobile development and deployment, as well as data
analysis and machine learning infrastructure. For the current
experiment, we will use only three components of the infrastructure:
the Google App Engine, the Google Datastore, and the logging
functionality. In other experiments you may also wish to use the
Google Cloud Storage functionality, although that isn't necessary in
the current case. The overall monitoring and control interface can be
found at the [Google Cloud Console](http://console.cloud.google.com).

[^1]: More abominable tech jargon. We'll get to what a "platform" is
    momentarily.

To understand why it helps to use the Google Cloud Platform, we should
first quickly cover how an online experiment works (which is,
basically, how any webpage works). To a first approximation, surfing
the web requires two types of computer: the "client", which has the
person surfing the web (almost always in a web browser), and the
"server", which provides the actual web content when asked. In simple
terms, the client computer *requests* a webpage and the server
computer *responds* with the content of that webpage. This is called a
*request--response transaction* (for fairly obvious reasons) is the
basis for almost all communication across the web[^http]. Servers are
regular computers that "listen" to incoming requests (on a *network
port* to use the slightly more technical language) and which have
software which can respond to valid requests. We will use the Flask
framework written in Python as the software for responding to valid
requests. Going into depth about how Flask works is beyond the scope
of this document, except for the description of the broad parameters
of operation.

[^http]: The way these computers communicate, at the top (most
    abstract) level, is defined by what is known as the Hypertext
    Transfer Protocol or
    [HTTP](https://en.wikipedia.org/wiki/Hypertext_Transfer_Protocol).
    This protocol---other than being a familiar friend at the front of
    URLs---determines the way in which the computers can talk to each
    other: the types of requests they can make, what constitutes a
    legal response, how to respond when things go wrong, *etc.*

The Google Cloud Platform---or more specifically, the Google App
Engine---handles the server part of the above description. In the
past, servers usually had to have some fairly sophisticated
configuration to work correctly. They had to have their hardware and
physical networking configured so that they could be reached by a
consistent address (corresponding to the "top" parts of a URL, the
domain name) and, second, had to have their internal software
(operating system *etc.*) configured to be able to correctly handle
these incoming requests. This is a real hassle, very easy to get
(somewhat) wrong, and very difficult to configure when large volumes
of traffic are encountered. Like many problems in software
engineering, the pain of implementing a solution doesn't scale
linearly with the size of the problem itself (or the number of times
that the solution needs to be implemented): if you have the expertise
to solve the problem correctly once, with some minor abstraction to
solve the class of related problems, then you can reproduce that
solution multiple times to a large neighbourhood of related problems
many times over. Specifically, in the case of Google App Engine, this
works by "abstracting out" everything beyond the programming language
interpreter (*e.g.,* the Python interpreter) and by knowing how to
"talk" with the specific components within the programming language
interpreter itself. Services which handle everything outside of the
programming language interpretation itself are called "platforms". For
the specific details about Flask and Python, and why we're using them
here, see [below](#why-python-what-is-flask).

# Preliminaries
Before you can deploy the experiment to the App Engine, you will need
to make sure that the appropriate software is installed, that a
project for the application has been created and configured, and that
the local software is configured correctly to be linked to your
account and project. **You can skip this subsection if I am handling
all of the deployment for you, but you may want to read the section in
case you have to deploy your experiment into the future.**

## Install `gcloud`
You will need the Google Cloud SDK (Software Development Kit)
installed. It is used, in this project, through the command-line
program `gcloud`. You will know that you have the Cloud SDK installed
when you can, in the terminal of your choosing, launch `gcloud`. For
the purposes of the current document, when I type `gcloud version` the
following information is printed out:

```
Google Cloud SDK 295.0.0
alpha 2020.05.29
app-engine-python 1.9.91
beta 2020.05.29
bq 2.0.57
cloud-datastore-emulator 2.1.0
core 2020.05.29
gsutil 4.51
kubectl 2020.05.29
```

The key values there are those next to `Google Cloud SDK` and
`app-engine-python`. If your version differs from the above
significantly, you may have to use different commands to achieve the
same outcomes.

## Authenticate `gcloud` with your Google account
The `gcloud` SDK on your computer needs to be linked to your Google
account (or the Google account associated with your Google Cloud
project) in order to properly deploy the code. The most
straightforward way to do this is to use the initialisation
subcommand, `gcloud init`. If you have since logged out, changed
details, or (for any other reason) need to log back in, you can use
the command `gcloud auth login`.

## Set up the project
All of the services within the larger Google Cloud Services
infrastructure are tied to a "project", which simply delineates the
use of Google services between different resources and large-scale
configuration between different sets of code.

For the current experiment, I have already taken the liberty of
setting up a project under the name of `jzhou-sourcemem-online`. You
will need to set your `gcloud` SDK global configuration to avoid
having to use the `--project` argument for all commands (see
[below](#ensure-that-the-project-is-active)).

For experiments, projects should roughly correspond to the overall
Python module used to serve an experiment. It is possible to put many
experiments into a single Python module (or to abstract how
experiments and data are handled so all of your experiments can be
handled in a uniform way), but this requires a substantially more
abstract design or, worse, piecing together a lot of reproduced code
in an unmanageable way. Some balance of *ad hoc* design and
extensibility for writing experiments usually means that you want to
copy most of the experimental software to create a new project for
most experiments, as writing more general software isn't generally
worth the effort. But, if you know you will have an almost identical
structure for a large number of experiments, you can write a single
Python package for all experiments and, therefore, use a single Google
Cloud Platform project. (This is essentially following the rule stated
in the introductory remarks: writing specific solutions scales
nonlinearly with the problem space if you can define the problem at
the right level of abstraction.)

## Configure the datastore
The datastore is where all of the client (participant) session data
and experimental data will be stored. "Datastore" here is just a fancy
word for "database". The datastore, as we will use it, is slightly
different from the standard database software in that it is
"schemaless". You don't have to know much about this, other than the
fact that it largely means we can store and retrieve Python
dictionaries (with many types of standard data included as fields of
those dictionaries) without having to do any conversion.

This has some upsides (mostly to do with the flexibility of writing to
a database without having to first specify and migrate "schema", which
are often required in databases generally) but has some downsides. The
most salient downside in the current situation is that compound
queries---those that operate on multiple fields---require explicit
specification in an index file. (See
[below](#update-the-datastore-index).)

To configure the datastore, you will need to log into the Google Cloud
Console and navigate to the "Datastore" section in the left-hand
panel. If the datastore has not yet been initialised, you will need to
do so by both selecting a "mode" for the datastore to operate in
(either "Native" mode, which is better for mobile apps, or "Datastore"
mode). **Select "Datastore" mode**. You will also need to indicate
where the datastore should be located. This is not critically
important, as our datastore usage is comparatively tiny, but selecting
the United States region (or a single instance on the west coast of
the United States) should be more than sufficient.

**Remember to [update the datastore
index](#update-the-datastore-index)** during deployment before
actually running any (somewhat complex) query.

## Activate the correct project
All of the deployment commands either take a `--project` command-line
argument or, if the `--project` argument is missing, use the project
as specified in the global configuration. You can set this, as well as
all of the global configuration values, using the `gcloud config`
subcommand. Specifically, if your project name is
`jzhou-sourcemem-online`, you can set the project using the command:

``` gcloud config set project jzhou-sourcemem-online ```

If you don't know what the current global project is currently
configured, you can use the command `gcloud config get-value project`
to print the global project value to the terminal. To get a list of
all of the projects on the current account, use the command `gcloud
projects list`.

# Deployment
Once the one-time installation and initialisation is complete, we can
turn our attention to actually deploying the experiment. Deployment
means, in the current situation, uploading all of the relevant Python
code, as well as the static files (*e.g.,* HTML, image, Javascript
files), to the Google App Engine so that it can load the Python and
start listening for incoming requests.

## Update the datastore index
This step is easy to miss but important. Whenever information is
retrieved from the datastore, the specific query requires a
corresponding *index*. This index is updated whenever data is modified
in the database. Simply retrieving or ordering records on the basis of
a single "field" within the datastore is almost always handled as
indexes are automatically created for each individual field upon the
creation of entries (or *entities*) in the datastore. Querying
entities by multiple fields, even if one is simply for ordering the
results, requires the construction of specified indexes.

These indexes can be specified in the `index.yaml` file in the
application directory. This file must be deployed separated from the
application using `gcloud app deploy index.yaml`. A few minutes should
be given while the index specification is propagated in the database,
otherwise a runtime error might be encountered when a query is run.
Failure to specify an index when querying will result in a runtime
error.

## Deploy the application
To deploy the application, change directory to the location of the
`app.yaml` configuration file and type `gcloud app deploy`.

Before uploading the application, `gcloud` will ask for confirmation
and indicate the project name that the application will be uploaded to
and the URL with which the application will be available: echeck that
the project name `gcloud` shows is the correct project before
confirming the deployment. When you are satisfied that the details are
correct, type `Y` to confirm the deployment and wait for the `gcloud`
software to upload the relevant files and set up the routing.

Deploying the application has two major components. First, the files
themselves are uploaded to Google's servers. Each version of uploaded
files is tracked by the App Engine, meaning that you can "roll back"
to a previous version at any time. For the most part, you want to make
sure that this version handling is done on your end (using `git`, for
instance) but it is good to have some redundancy in terms of tracking
what is uploaded and when it is uploaded. 

The second thing that the `deploy` command does is sets up the newly
uploaded version of the code to be the location where incoming traffic
will be routed. This is part of what actually occurs within the App
Engine: although writing the Python script might seem to indicate that
a single interpreter will be handling all requests in the system as
they come in, the computer handling the requests will actually create
many *instances* of the Python interpreter for handling different
requests. This means that new connections to the server will be sent
to (potentially many) different instances of the Python interpreter
running the new code.

This is the essential (software) part of allowing a single piece of
software to service many, many incoming requests efficiently. The
routing of an incoming request to a running Python instance is done
through a piece of software (or, occasionally, hardware) called a
*load balancer*. In practice, all this means is that responding to a
request shouldn't rely on the global state of a Python environment
unless that state is identical across all instances (*e.g.,* the state
is initialised in the same way for all instances). More succinctly, if
you need something to change the behaviour of how a request is
handled, that should be stored in a database and the actual request
behaviour determined by reading the appropriate values from the
database and handling accordingly.

For an example of this, we can examine how "client sessions" are
handled in the code for this experiment: we associate the client
session with a session identifier string which is stored in the
client's cookie and store in the database along with all of the
participant's details (whether they've completed the task, read the
PLS, clicked agree on the consent form, *etc.*). Whenever a request is
received, the session identifier in the cookie is read from the HTTP
headers. The user information is then read from the database and the
appropriate response is given. If we could absolutely ensure that only
a single Python process was ever going to handle this session data, we
could just store all of this information in memory, but that can not
be ensured in this case (and is generally not good practice unless it
is required for other reasons).

# Retrieving the data
Once data has been uploaded by the participants to the datastore, we
need a way of both monitoring experimental completions as well as
downloading the data on to a local computer for storage and analysis.
This can be done with special API endpoints in the Flask application
that list the completed datasets and allow the experimenter to access
each completed dataset by an identification number.

Although security is not the highest priority here (no sensitive data
is being stored and we expect all of the experimental data will be
available publicly at some later date), we put the data access
endpoints behind some authentication to prevent public access while
the experiment is running.

The R scripts in `analysis/` directory (specifically the
`sourcemem-online.R` top-level script and the procedures in the
`access-data.R` script) provide the means for polling the server
status, getting a list of the completed dataset IDs, and then (for
each ID) downloading the corresponding dataset.

# A little more information
## Why Python? What is Flask?
In setting up the experiment, you had to learn a little bit of
Javascript to correctly operate jsPsych. Now, in order to actually
deploy the experiment (or roll it out to the participants) we have to
use Python. It's reasonable to wonder why we suddenly have so many
different programming languages and why simply writing the experiment
in jsPsych is not sufficient.

The broad answer for why two different parts are required relates to
the first section, where I said that there are two parts to a
webpage's operations: a server and a client. Everything on the
server's side is, unsurprisingly, labelled with the adjective
*serverside* (and likewise for the client and *clientside*). 

The more specific answer about why Flask and Python are used for the
serverside is that Flask provides a relatively quick way of getting
the core request handling code together without a huge amount of
overhead: it allows a developer to map out different URLs, which
correspond to different request types on the part of the client, to
responses (that can call any Python code). The Google Cloud Platform
SDK also provides Python bindings for most of its functionality
(Storage Services, datastore, *etc.*).

It is possible to write the serverside procedures for handling
incoming requests in Javascript, using a special interpreter of
Javascript developed for server-type applications, known as
[*node.js*](https://nodejs.org/en/). There are some nice features
about Javascript that make this a viable option for people writing web
applications but it is a somewhat difficult language to wrangle at the
best of times. It is not possible (to a first approximation!) to write
the client-side experiment in anything other than Javascript, so we
can't escape [the myriad joys of
Javascript](https://www.destroyallsoftware.com/talks/wat) there.
