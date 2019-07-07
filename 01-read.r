source("libraries.r")
source("functions.r")


# create local data dir
dir.create("data/", showWarnings=F)
dir.create("data/working/", showWarnings=F)
dir.create("data/raw/", showWarnings=F)

# ONLY ONCE: DOWNLOAD ICIO2018_2015.zip in data/raw/ from:
# http://stats.oecd.org/wbos/fileview2.aspx?IDFile=9f579ef3-4685-45e4-a0ba-d1acbd9755a6
# (using download.file() failed)

# input file
zip_file = "data/raw/ICIO2018_2015.zip"

# unzip file
unzip(zip_file, exdir=dirname(zip_file), overwrite=T)
csv_file = "data/raw/" %+% unzip(zip_file, list=T)$Name

# read matrix
mat_raw = read.delim(csv_file, header=T, sep=",", row.names=1) %>% as.matrix()

# save matrix as rds
saveRDS(mat_raw, "data/working/matrix_raw.rds")



