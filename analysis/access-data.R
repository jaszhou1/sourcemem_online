## access-data.R
##
## This file contains the procedures for connecting to the server,
## checking whether the experiment is complete, and downloading one or
## more participants' datasets.

library(httr)
library(rjson)

## Parameters for accessing the online source memory experiment. These
## need to be in agreement with the parameters in the access
## server-side API.
MASTER.API.FIELD <- "API_ACCESS_KEY"
STATUS.EXTENSION <- "server-status"
STARTED.USER.ID.EXTENSION <- "started-users"
COMPLETED.USER.ID.EXTENSION <- "completed-users"
DATA.ACCESS.EXTENSION <- "get-user-data"
USER.INFO.EXTENSION <- "get-user-information"

## Construct a URL from its constitutive components.
construct.url <- function(server.base, server.port=NULL,
                          server.extension=c()) {
    if(!is.null(server.port)) {
        server.port.str <- paste0(":", as.character(server.port))
    } else {
        server.port.str <- ""
    }
    path.str <- paste(server.extension, collapse="/")
    paste0(server.base, server.port.str, "/", path.str)
}

## Get the status of the experiment currently running at a given
## address.
get.server.status <- function(server.base.url, server.port=NULL,
                              api.key=NULL) {
    status.url <- construct.url(server.base.url, server.port,
                                STATUS.EXTENSION)
    if(!is.null(api.key)) {
        r <- GET(status.url, add_headers(Authorization=auth.string))
    } else {
        r <- GET(status.url)
    }
    if(status_code(r) == 404) {
        ## If we have a 404, then it means either the server base URL
        ## is not correct, the status path is not correct, or both.
        return(list(status="Not found"))
    }
    if(status_code(r) == 403) {
        ## If we have a 403, then the server API key is not correct.
        return(list(status="Forbidden"))
    }
    if(status_code(r) != 200) {
        ## We have a non-OK status for a completed HTTP response. This
        ## is some error we have not otherwise caught.
        return(list(status="Unknown error"))
    }
    return(content(r))
}

## Get started users.
get.started.users <- function(server.base.url, server.port=NULL,
                                api.key=NULL) {
  status.url <- construct.url(server.base.url, server.port,
                              STARTED.USER.ID.EXTENSION)
  if(!is.null(api.key)) {
    auth.string <- paste0(MASTER.API.FIELD, "=", api.key)
    r <- GET(status.url, add_headers(Authorization=auth.string))
  } else {
    r <- GET(status.url)
  }
  if(status_code(r)==404) {
    ## If we have a 404, this means either the server base URL is
    ## not correct, the status path is not correct, or both.
    return(list(status="Not found"))
  }
  if(status_code(r)==403) {
    ## If we have a 403, it means the server API path is not
    ## correct.
    return(list(status="Forbidden"))
  }
  if(status_code(r)!=200) {
    return(list(status="Unknown error"))
  }
  return(content(r))
}


## Get completed users.
get.completed.users <- function(server.base.url, server.port=NULL,
                                api.key=NULL) {
    status.url <- construct.url(server.base.url, server.port,
                                COMPLETED.USER.ID.EXTENSION)
    if(!is.null(api.key)) {
        auth.string <- paste0(MASTER.API.FIELD, "=", api.key)
        r <- GET(status.url, add_headers(Authorization=auth.string))
    } else {
        r <- GET(status.url)
    }
    if(status_code(r)==404) {
        ## If we have a 404, this means either the server base URL is
        ## not correct, the status path is not correct, or both.
        return(list(status="Not found"))
    }
    if(status_code(r)==403) {
        ## If we have a 403, it means the server API path is not
        ## correct.
        return(list(status="Forbidden"))
    }
    if(status_code(r)!=200) {
        return(list(status="Unknown error"))
    }
    return(content(r))
}

## Get the session information for the selected user.
get.user.information <- function(server.base.url, user.id,
                                 server.port=NULL,
                                 api.key=NULL) {
    status.url <- construct.url(server.base.url, server.port,
                                c(USER.INFO.EXTENSION, user.id))
    if(!is.null(api.key)) {
        auth.string <- paste0(MASTER.API.FIELD, "=", api.key)
        r <- GET(status.url, add_headers(Authorization=auth.string))
    } else {
        r <- GET(status.url)
    }
    if(status_code(r)==404) {
        ## If we have a 404, this means either the server base URL is
        ## not correct, the status path is not correct, or both.
        return(list(status="Not found"))
    }
    if(status_code(r)==403) {
        ## If we have a 403, it means the server API path is not
        ## correct.
        return(list(status="Forbidden"))
    }
    if(status_code(r)!=200) {
        return(list(status="Unknown error"))
    }
    return(content(r))
}

get.last.experiment.data.by.user.id <- function(server.base.url,
                                                user.id, server.port=NULL,
                                                api.key=NULL) {
    status.url <- construct.url(server.base.url, server.port,
                                c(DATA.ACCESS.EXTENSION, user.id))
    if(!is.null(api.key)) {
        auth.string <- paste0(MASTER.API.FIELD, "=", api.key)
        r <- GET(status.url, add_headers(Authorization=auth.string))
    } else {
        r <- GET(status.url)
    }
    if(status_code(r)==404) {
        ## If we have a 404, this means either the server base URL is
        ## not correct, the status path is not correct, or both.
        return(list(status="Not found"))
    }
    if(status_code(r)==403) {
        ## If we have a 403, it means the server API path is not
        ## correct.
        return(list(status="Forbidden"))
    }
    if(status_code(r)!=200) {
        return(list(status="Unknown error"))
    }
    return(content(r))
}

get.session.data.by.user.id <- function(server.base.url,
                                                user.id, session.id, server.port=NULL,
                                                api.key=NULL) {
  status.url <- construct.url(server.base.url, server.port,
                              c(DATA.ACCESS.EXTENSION, user.id, session.id))
  if(!is.null(api.key)) {
    auth.string <- paste0(MASTER.API.FIELD, "=", api.key)
    r <- GET(status.url, add_headers(Authorization=auth.string))
  } else {
    r <- GET(status.url)
  }
  if(status_code(r)==404) {
    ## If we have a 404, this means either the server base URL is
    ## not correct, the status path is not correct, or both.
    return(list(status="Not found"))
  }
  if(status_code(r)==403) {
    ## If we have a 403, it means the server API path is not
    ## correct.
    return(list(status="Forbidden"))
  }
  if(status_code(r)!=200) {
    return(list(status="Unknown error"))
  }
  return(content(r))
}
