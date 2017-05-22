library(rhdf5)
library(data.table)
create_h5file <- function() {
  library(rhdf5)
  library(data.table)
 # fdir <- "/Volumes/MacintoshHD2/Users/haroonr/Detailed_datasets/Dataport/wiki-energy/"
  fdir2 <-  "/home/hrashid/Dataport_data/"
  f5storepath <- "/home/hrashid/Dataport_data/"
  filename <- paste0(f5storepath,"dataport_minutely.h5")
 # h5createFile(filename)
 handle <- h5createFile(filename)
  files = list.files(fdir2,pattern="*.csv")
 # files2 = files[1:3]
  lapply(files,function(x) {
    df = fread(paste0(fdir2,x))
    h5write(df, filename, x)
  })
  H5close(fhandle)
}

read_h5files <- function() {
  readhandle = H5Fopen(filename)
  #list of files stored in object: h5ls(readhanlde)$name
  h5read(readhandle,"101.csv")
}
