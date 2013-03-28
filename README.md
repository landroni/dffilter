gWidgets2-filter
================

gWidgets2 GUI to search, filter selection and edit a data frame in R

The GUI allows the user to search and filter rows in a data frame, select only the desired columns and display the resulting subset. Optionally the user can edit the subset and reliably merge the changes into the original data frame. 

At the moment the software is pre-alpha, ***experimental***. Testers needed. 

Installation intructions
================

You will need development versions of [gWidgets2](https://github.com/jverzani/gWidgets2) and [gWidgets2RGtk2](https://github.com/jverzani/gWidgets2RGtk2). It may work with [gWidgets2tcltk](https://github.com/jverzani/gWidgets2tcltk) but this wasn't yet tested.

    require(devtools) 
    install_github("gWidgets2", "jverzani")
    install_github("gWidgets2RGtk2", "jverzani")

Then download and source `gWidgets2-filter.R`. 
