## These will be moved back to caps.R in rcloud.web
.html.in <- function(x){
  if (inherits(x, "javascript_function") || (is.character(x) && length(x) == 1)){ x }
  else {paste(as.character(x), collapse='\n')}
} 

#' rcw.append
#' @export
rcw.append <- function(element, what) {
  if (grepl("^rcloud-params-", what)) {
    input.caps$appendElement(element, what)
  } else{
    input.caps$appendDiv(element, .html.in(what))
  }
  
}

#' rcw.prepend
#' @export
rcw.prepend <- function(element, what) {
  if (grepl("^rcloud-params-", what)) {
    input.caps$prependElement(element, what)
  } else{
    input.caps$prependDiv(element, .html.in(what))
  }
} 

#' rcw.set
#' @export
rcw.set <- function(element, what) {
  if (grepl("^rcloud-params-", what)) {
    input.caps$setElement(element, what)
  } else{
    input.caps$setDiv(element, .html.in(what))
  }
} 

#' rcw.on
#' @export
rcw.on <- function(element, events, callback, data=element, ...)
  if (length(list(...))) {
    l <- list(...)
    if (!length(names(l)))
      stop("callbacks must be named when passed via ...")
    if (!missing(events) || !missing(callback))
      stop("events/callback and using named events in ... are mutually exclusive")
    for (n in names(l))
      rcw.on(element, n, l[[n]], data)
    invisible(names(l))
  } else {
    events <- paste(events, collapse=' ')
    if (!inherits(callback, "OCref")) {
      if (is.function(callback))
        callback <- ocap(callback)
      else
        stop("callback must be a function or ocap")
    }
    input.caps$on(element, events, callback, data)
  }

