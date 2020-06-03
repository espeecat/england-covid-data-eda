# Run once for set-up
# create folders and initial set-up

#
# jason bailey
# 20200602 first commit

#install.packages('tidyverse')
#install.packages('xts')

# Store images here
if (!file.exists("media")){
  dir.create("media")
  print("Creating media folder")
} else {
  print("media folder exists")
}

# England LA data here
if (!file.exists("indata")){
  dir.create("indata")
  print("Creating indata folder")
  # Local_Authority_Districts.csv
} else {
  # LA data here: http://geoportal.statistics.gov.uk/datasets/local-authority-districts-april-2019-names-and-codes-in-the-united-kingdom
  print("indata folder exists")
}

