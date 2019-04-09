#init path

dvpt_path = file.path(Sys.getenv("HOME"),'dvpt/R/rfonc/R')

sapply(dir(dvpt_path, pattern="\\.[Rr]$", full.names = TRUE), source)

