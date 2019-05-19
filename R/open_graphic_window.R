

open_graphic_window <- function(){
  
  
  if (.Platform$OS.type == "windows"){
    windows()
  } else {
  
    user_system <-  Sys.info()['sysname']
    
    if(user_system == "Linux"){
      X11()
    }
    
    if(user_system == "Darwin"){
      quartz()
    } 
    
}
  
  


get_os <- function() {
  if (.Platform$OS.type == "windows") { 
    "win"
  } else if (Sys.info()["sysname"] == "Darwin") {
    "mac" 
  } else if (.Platform$OS.type == "unix") { 
    "unix"
  } else {
    stop("Unknown OS")
  }
}
  # This gives time for the window to actually pop
  Sys.sleep(0.2)
  
}