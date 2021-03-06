#' Create div that will include parms widgets
#' @description  Allows users to enter param widgets with html tools all in one call
#' @param byRow alter widget display (T/F)
#' @examples 
#'    div(h1("Start"),h1("Start"),
#'        x = list(label = "Minimum",  min = 1, input = "numeric"),       h1("End"))
#'    submit()
#' @export

##  This function uses the rcw.* functions from rcloud.web. 
## These functions have been copied to rcloud.params.js but will need to be updated 
## when the package is transfered.


div <- function(..., byRow = FALSE){

    listNames <- as.character(match.call())[-1]  # remove call 
  
  if(!missing(byRow))
    listNames <- listNames[-length(listNames)] # remove byRow arg

  myDiv <- vector(length = length(listNames))

  if(byRow){
    rcloud.html.out(htmltools::div(id = "param"))
    for(i in seq_len(length(listNames)))
      rcw.append("#param", eval(parse(text = listNames[i])))
    
  } else{
    for(i in seq_len(length(listNames)))
      myDiv[i] <- as.character(htmltools::div(id = paste0("param", i)))
    
    rcloud.html.out(htmltools::div(HTML(myDiv)))
    
    for(i in seq_len(length(listNames)))
      rcw.set(paste0("#param", i), eval(parse(text = listNames[i])))
  }
}
