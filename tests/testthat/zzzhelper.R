#cleanup

#delete the tempdir() to force the dataPath back to the orig if it existed
unlink(file.path(tempdir(), ".Rnightlights"), recursive = T, force = T)