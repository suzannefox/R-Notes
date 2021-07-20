# make a list of r packages I already have installed by querying my R environment
# make a list of r packages I want
# check each package I want to see if it already installed
# if not install it

# currently installed
df.installed.packages <- data.frame(installed.packages())

# packages I want
my.packages <- c('tidyverse',
                 'janitor',
                 'glue',
                 'odbc',
                 'googledrive',
                 'googlesheets4',
                 'palmerpenguins',
                 'datapasta')

# install anything that's not already installed
for (package in my.packages) {
  
  got <- package %in% df.installed.packages$Package
  print(paste(' ... checking for package', package, ': status', got))

  if (!got) {
    # install it
    install.packages(package)
    # update installed package list
    df.installed.packages <- data.frame(installed.packages())
  }
}
