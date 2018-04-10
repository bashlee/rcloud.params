nullOrEmpty <- function(x) {
  length(x) == 0
}

# Given a vector or list, drop all the NULL or length-0 items in it
dropNullsOrEmpty <- function(x) {
  x[!vapply(x, nullOrEmpty, FUN.VALUE=logical(1))]
}

getType <- function(`_tag_name`, varArgs){
  if(`_tag_name` == "select")
    "character"
  else if(`_tag_name` == "input")
    switch(varArgs$type,
           'text' = "character",
           'number' = "numeric",
           'date' = "Date",
           'range' = "numeric",
           'checkbox' = "logical",
           'character')
}

mytags <- list(
  input = function(var, val = NA, ...) ptag("input", substitute(var), val, list(...)), ## generic input function
  select = function(var, val = NA,...) ptag("select",  substitute(var), val, list(...))
)

#' ptag
#'
#' @export
ptag <- function(`_tag_name`, var, value = NA, varArgs){

  name <- deparse(var)

  if(exists(name)) value <- eval(var)

  varArgsNames <- names(varArgs)
  if (is.null(varArgsNames))
    varArgsNames <- character(length = length(varArgs))
  named_idx <- nzchar(varArgsNames)
  attribs <- dropNullsOrEmpty(varArgs[named_idx])
  attribs$id <- paste0("rcloud-params-", name)
  
  children <- unname(varArgs[!named_idx])

  inputTag <-toString(structure(list(name = `_tag_name`, attribs = attribs, children = children),
                                class = "shiny.tag"))

  varClass <- getType(`_tag_name`, varArgs)

  param(inputTag, name, varClass = varClass, inputVal = value)
}


#' Create text input
#'
#' Creates HTML for a textbox input to pass to the param function
#' @export

textInput <- function(var, value = NA){
  name <- deparse(substitute(var))
  if(exists(name)) value <- var

  #inputTag <- paste0("<input type= 'text' id='", paste0("rcloud-params-", name), "'></input>") 
  inputTag <- toString(tags$input(type = 'text', id = paste0("rcloud-params-", name)))
  
  param(inputTag, name, varClass = "character", inputVal = value)
}

#' Create numeric input
#'
#' Creates HTML for a numeric value input to pass to the param function
#' @export

numericInput <- function(var, value = NA, min = NA, max = NA, step = NA){
  name <- deparse(substitute(var))
  if(exists(name)) value <- var  # If variable defined value argument is over-ridden. 
  
  # inputTag <- paste0("<input type= 'number' id='", paste0("rcloud-params-", name),  
  #                    "' min = '", min, "' max = '", max, "' step = '", step,"'></input>") 
  inputTag <- toString(tags$input(type = "number", id = paste0("rcloud-params-", name), 
                         min = min, max = max, step = step))
  param(inputTag, name, varClass = "numeric", inputVal = value)
}


#' Create select input
#'
#' Creates HTML for a select dropdown input to pass to the param function
#' @export

selectInput <- function(var, choices, selected = '', multiple = FALSE){
  name <- deparse(substitute(var))
  
  optionTag <- mapply(choices, choices, FUN = function(choice, label){
    sprintf('<option value="%s"%s>%s</option>',
            htmlEscape(choice, TRUE), 
            if (choice %in% selected) ' selected' else '', 
            htmlEscape(label))
  })
  
  multipleTag <- ifelse(multiple, "multiple = 'multiple'", "")
  
  optionTag <- HTML(paste(optionTag, collapse = '\n'))
  inputTag <- paste0("<select id='", paste0("rcloud-params-", name, "' "), multipleTag, 
                     " >", optionTag, "</select>")
  param(inputTag, name, varClass = "character")
  
}

#' Create date input
#'
#' Creates HTML for a clickable calender input that is passed to the param function.
#' If var is already defined, calander will be prepoulated with existing date (value ignored) 
#' 
#' @param var varible name defined or undefined
#' @param value Either a date object or a string in yyyy-mm-dd format
#' @export

dateInput <- function(var, value = NA){
  name <- deparse(substitute(var))

  if(exists(name)) value <- var
  if (inherits(value, "Date"))  value <- format(value, "%Y-%m-%d")
  
  inputTag <- paste0("<input type='date' id='", paste0("rcloud-params-", name),
                     "' data-date-format='yyyy-mm-dd' >")
 
  param(inputTag, name, varClass = "Date", inputVal = as.character(value))
}


#' Create slider input
#'
#' Creates HTML for a numeric value input to pass to the param function
#' @export

sliderInput <- function(var, value = NA, min = NA, max = NA){
  name <- deparse(substitute(var))

  
  if(exists(name)) value <- var  # If variable defined value argument is over-ridden. 
  if(!exists(name) & is.na(value)) stop(paste(name, " not defined, value arg required"))
  
  if(is.na(min) | is.na(max) ) stop("Please supply min & max ") 
  if(any(lapply(c(min, max, value), class) != "numeric")) stop("All values must be numeric")

  inputTag <- paste0("<span>", min, "</span>", 
                     "<input type= 'range'  id='", paste0("rcloud-params-", name),  
                          "' min ='", min, "' max = '", max, "'></input>", 
                     "<span>", max, "</span>")
  
  param(inputTag, name, varClass = "numeric", inputVal = value)
}

#' Create checkbox input
#'
#' Creates HTML for a checkbox input to pass to the param function
#' @export

checkBoxInput <- function(var, value = FALSE){
  name <- deparse(substitute(var))
  if(exists(name)) value <- var  # If variable defined value argument is over-ridden. 
  
  status <- ifelse(value, "checked", "")
  
  inputTag <- paste0("<input type = 'checkbox' id='", paste0("rcloud-params-", name), "'",   status," >")
  
  param(inputTag, name, varClass = "logical", inputVal = value)
}
