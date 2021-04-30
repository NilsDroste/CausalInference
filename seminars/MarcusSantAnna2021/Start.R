############################################
# REPLICATION Seminar: Marcus and Sant'Anna
############################################

# the raw data is too large to updload to github.
# in case we want to have the fully replicable set
# we will have to download and unpack the raw data.

# either by hand from: https://doi.org/10.7910/DVN/TXB0ZO
# or through R: check for all other than Windows:
# https://stackoverflow.com/questions/46628844/how-to-read-file-rar-directly-from-website-in-r

# Here is how this would work in R on Windows

library(here)

temp <- tempfile()

download.file(paste0("https://dvn-cloud.s3.amazonaws.com/10.7910/DVN/TXB0ZO/1745654c2a0-8792208fb16d?response-content-disposition=attachment%3B%20filename%2A%3DUTF-8%27%27replication_files.rar&response-content-type=application%2Fx-rar-compressed&X-Amz-Algorithm=AWS4-HMAC-SHA256&X-Amz-Date=20210429T134307Z&X-Amz-SignedHeaders=host&X-Amz-Expires=3600&X-Amz-Credential=AKIAIEJ3NV7UYCSRJC7A%2F20210429%2Fus-east-1%2Fs3%2Faws4_request&X-Amz-Signature=40575c122ba081bf4a0babf545b781e99ad428e5d6f30f2fd38d6622f1946992"), temp)

#extract RAR to current working directory using p7zip (given you have the 7z.exe path in your PATH)
system(paste("7z x", temp, paste0("-o", getwd())))

# but also this may cause a cryptic error message.