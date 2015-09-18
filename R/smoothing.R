#'@title Switch between global and local smoothing
#'
#'@description We use a lot of smoothing in our reactive graphs,
#'and tend to offer both global (entire dashboard) and local (tab-specific) smoothing options.
#'\code{smooth_switch} is a simple function to abstract away the logic behind determining whether
#'the global or local option should be relied on.
#'
#'@param global the input variable for global smoothing.
#'
#'@param local the input variable for local smoothing.
#'
#'@export
smooth_switch <- function(global, local){
  if(local == "global"){
    return(global)
  }
  return(local)
}