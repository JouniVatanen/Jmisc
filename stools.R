# Load required packages
if (!require("checkpoint")) install.packages("checkpoint"); library(checkpoint)

# Use the date few weeks after R-3.6.1. was released
checkpoint("2019-07-28", R.version = "3.6.1",
           checkpointLocation = Sys.getenv("USERPROFILE"))

# Create package documentation
devtools::document()

# Install package
setwd("..")
devtools::install("stools")
setwd("./stools")

# Commit changes and push the files to the github
# 1. Commit changes shell "git add .;git commit -m 'comment'" OR Rstudio UI
# 2. Push changes either shell "git push" OR Rstudio UI
# 3. Install from github with devtools::install_github(JouniVatanen/stools)
