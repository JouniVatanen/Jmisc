# Checkpoint installs packages
if (!require("checkpoint")) install.packages("checkpoint")
# use a date few weeks after R.version was released
checkpoint::checkpoint("2019-12-30", R.version = "3.6.2",
                       checkpointLocation = Sys.getenv("USERPROFILE"))

# Document and install package
if (!require(devtools)) install.packages("devtools")
devtools::document()
devtools::install()

# Commit changes and push the files to the github
# 1. Commit changes shell "git add .;git commit -m 'comment'" OR Rstudio UI
# 2. Push changes either shell "git push" OR Rstudio UI
# 3. Install from github with devtools::install_github(JouniVatanen/stools)
