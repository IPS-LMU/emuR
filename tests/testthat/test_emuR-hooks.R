context("testing that hooks like .onLoad are doing the right stuff")

test_that(".onLoad options are set correctly", {
  expect_equal(getOption("emuR.emuWebApp.dir"), 
               file.path(tempdir(), "EMU-webApp"))
  
})