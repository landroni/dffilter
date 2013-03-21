## edit a really large data set *after* it has been filtered

require(MASS)
data_set <- Cars93 ## this will be in a function... replace with your won

##!!set up a function
DF <- NULL                                  # global gdf instance
idxs <- NULL                                # global set of indices that are being edited
cnms <- NULL                                # global column names

options(guiToolkit="RGtk2")
library(gWidgets2) ## on github not CRAN. (require(devtools); install_github("gWidgets2", "jverzani")

##??data frame selector
w <- gwindow("edit me", visible=FALSE)
pg <- gpanedgroup(w, horizontal=TRUE)

f_side <- gvbox(cont=pg)
df_side <- gvbox(cont = pg, expand=TRUE)

df_box <- ggroup(cont=df_side, expand=TRUE) ## holds df instance
glabel("Select columns to be displayed \nand define appropriate row filters,\nthen click the 'Display selection' button. \nIf you make changes to your data, you \ncan merge them into the original dataset.", cont=df_box)

btn_gp <- ggroup(cont = df_side)
##!!close button (or discard/save&close)
do_btn <- gbutton("Merge changes...", cont=btn_gp)
visible(do_btn) <- FALSE
gs_df <- gstatusbar('', cont=df_side)

## set up filters.
## Select columns
##??increase hight of selection box; use size()
##!!reload data.frame
c_gp <- gframe("Select columns:", cont=f_side, horizontal=FALSE)
c_names <- gcheckboxgroup(names(data_set), cont=c_gp, use.table=TRUE, expand=TRUE)
s_gp <- ggroup(cont=c_gp, horizontal=TRUE)
gbutton("Invert", cont=ggroup(cont=s_gp), handler = function(h,...) {
    svalue(c_names, index=TRUE) <- setdiff(1:length(names(data_set)), 
                                           svalue(c_names, index=TRUE))
})
gbutton("Select all", cont=ggroup(cont=s_gp), handler = function(h,...) {
    svalue(c_names, index=TRUE) <- 1:length(names(data_set))
})
gbutton("Clear", cont=ggroup(cont=s_gp), handler = function(h,...) {
    svalue(c_names, index=TRUE) <- integer()
})


## Filter rows by logical
r_gp <- gframe("Filter rows:", cont=f_side, horizontal=FALSE)
##!!inspect the code (logical vector selection, use spinners for 'range', have 'select all', rename radio/choice to single/multiple, head/tail/some, )
row_filter <- gfilter(data_set, cont=r_gp, expand=TRUE)
##??show dim() of potential selection
# addHandlerChanged(row_filter, function(h,...) {
#     rows <- svalue(row_filter)
#     cnms <<- svalue(c_names)
#     idxs <<- which(rows)                  # move to global variable
#     data_set_dim <<- dim(data_set[idxs, cnms])
#     svalue(b_disp, append=T) <- data_set_dim[1]
# })
##!!add actual 'grepl' search


##!!editable checkbox
##!!automatic update checkbox
b_disp <- gbutton("Display selection", expand=TRUE, cont=ggroup(cont=f_side), handler=function(h,...) {
    visible(do_btn) <- TRUE
    cnms <<- svalue(c_names)
    rows <- svalue(row_filter)
    
    
    idxs <<- which(rows)                  # move to global variable
    ## now add a data frame
    delete(df_box, df_box[1])             # remove child
    DF <<- gdf(data_set[rows, cnms], cont=df_box, expand=TRUE)
    DF$set_selectmode("multiple")
    data_set_dim <- dim(data_set[idxs, cnms])
    svalue(gs_df) <- paste('Displaying a', data_set_dim[1], 'x', data_set_dim[2], 'selection.')
})
font(b_disp) <- list(weight = "bold")

size(w) <- c(600, 500)
visible(w) <- TRUE
svalue(pg) <- 0.33


##!!detect changes in gdf() and activate button only then
##??undo/redo
## What to do when you do ...
addHandlerClicked(do_btn, function(h,...) {
    ## change me to your liking
##??fix confirmation
#    gconfirm('Merge changes into the original data frame?', 'Confirm merge...', 
#             icon='question')
    data_set[idxs, cnms] <<- DF[]
    galert("The data frame has been modified.", parent=w)
})
