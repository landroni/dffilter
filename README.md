gWidgets2-filter
================

gWidgets2 GUI to view, search, subset and edit a data frame in R

The GUI allows the user to search and filter rows in a data frame, select only the desired columns and display the resulting subset. Optionally the user can edit the subset and merge the changes into the original data frame (experimental). 

At the moment the software is alpha, and the editing feature is ***experimental***. Testers welcome. 

Installation instructions
================

You will need to install [gWidgets2](http://cran.r-project.org/web/packages/gWidgets2) and [gWidgets2RGtk2](http://cran.r-project.org/web/packages/gWidgets2RGtk2). The data frame viewer may work with [gWidgets2tcltk](http://cran.r-project.org/web/packages/gWidgets2tcltk), but this wasn't thoroughly tested.

    # on Ubuntu, you may need to install the package libgtk2.0-dev
    install.packages(c("gWidgets2", "gWidgets2RGtk2"))

Then download and source `gWidgets2-filter.R`. (Proper R packaging is on the way.)

NOTE: Since the code is still alpha and moving a lot, most of the times you would need to install the development versions of gWidgets2:

    library(devtools)
    install_github('gWidgets2RGtk2', 'jverzani')
    install_github('gWidgets2', 'jverzani')
