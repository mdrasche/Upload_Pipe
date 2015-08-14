optioncreator <- function(options) {
  #Take a list of the options and turn it into a list that is compatible with 
  #the choices param in checkboxGroupInput
  options <- sort(options)
  output <- list()
  for (opt in options) {
    output[as.character(opt)] <- opt
  }
  return(output)
}



