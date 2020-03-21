.onLoad <- function(libname, pkgname) {
  # set package options
  options(emuR.emuWebApp.dir = file.path(tempdir(), "EMU-webApp"))
  # options(emuR.emuWebApp.dir = file.path("~/Developer/EMU-webApp/dist/")) # for devel
  
  invisible()
}