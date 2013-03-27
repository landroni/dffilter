## edit a really large data set *after* it has been filtered

# DF <- NULL                                  # global gdf instance
# idxs <- NULL                                # global set of indices that are being edited
# cnms <- NULL                                # global column names

##!!inspect the gfilter code (logical vector selection, use spinners for 'range', have 'select all' and redefine 'clear', rename radio/choice to single/multiple, head/tail/some, use combo evern for 3 radio choices, automatically update filter items to reflect available choices, as an option?, what happens to 'Date' or 'other' classes, )
##!!hack 'grepl' search into gfilter()
##!!bug when modifying a level it doesn't update the filters
##??optimize return() beahviour, confirmation, on-the-fly, discard/save&close button, undo/redo, what 5gb dataset, etc.
##??reload data.frame
##??sorting
##??diff (papertrail) before save/ask confirmation
##??data frame selector (use data frame browser)
##??waht happens when alter 'other' variables (& robustness of editor)
##??f4 to hide left pane
dffilter <- function(data_set, DF = NULL, idxs = NULL, cnms = NULL){
    require(gWidgets2) ## on github not CRAN. (require(devtools); install_github("gWidgets2", "jverzani")
    options(guiToolkit="RGtk2")
    #require(RGtk2)
    
    ## ensure we have a data frame of 1x2 dimensions
    stopifnot(is.data.frame(data_set))
    stopifnot(all(dim(data_set) >= c(1,2)))
    data_set_name <- deparse(substitute(data_set))
    
    data_set_dim_orig <- dim(data_set)
    w <- gwindow(paste(data_set_name, " (", data_set_dim_orig[1], ' x ', 
                       data_set_dim_orig[2], ')', sep=''), visible=FALSE, 
                 handler=function(h,...){
        #if(identical)
        #return(data_set)
    })
    pg <- gpanedgroup(w, horizontal=TRUE)
    
    f_side <- gvbox(cont=pg, use.scrollwindow=TRUE)
    df_side <- gvbox(cont = pg, expand=TRUE)
    
    df_box <- ggroup(cont=df_side, expand=TRUE) ## holds df instance
    glabel("Select columns to be displayed \nand define appropriate row filters,\nthen click the 'Display selection' button. \nIf you make changes to your data, you \ncan merge them into the original dataset.", cont=df_box)
    
    btn_gp <- ggroup(cont = df_side)
    do_btn <- gbutton("Merge changes...", cont=btn_gp)
    do_btn$set_icon("ok")
    enabled(do_btn) <- FALSE
    ##!!editable checkbox 
#     cb_do_btn <- gcheckbox('Prevent merges', checked=TRUE, cont=btn_gp, 
#                            handler=function(h,...){
#                                if((grepl('*', svalue(w), fixed=T) & 
#                                        svalue(cb_do_btn))){
#                                    enabled(do_btn) <- TRUE
#                                }
#     })
#     tooltip(obj=cb_do_btn) <- "Uncheck to allow merging changes into the original data frame."
    addSpring(btn_gp)
    close_btn <- gbutton("Close", cont=btn_gp, handler=function(h,...){
        dispose(w)
    })
    gs_df <- gstatusbar('', cont=df_side)
    
    ## set up filters.
    ## Select columns
    c_gp <- gframe("<b> Select columns: </b>", markup=TRUE, cont=f_side, 
                   horizontal=FALSE)
    c_names <- gcheckboxgroup(names(data_set), checked=TRUE, cont=c_gp, 
                              use.table=TRUE, expand=TRUE)
    s_gp <- ggroup(cont=c_gp, horizontal=TRUE)
    # gbutton("Invert", cont=ggroup(cont=s_gp), handler = function(h,...) {
    #     svalue(c_names, index=TRUE) <- setdiff(1:length(names(data_set)), 
    #                                            svalue(c_names, index=TRUE))
    # })
    b_selall <- gbutton("Select all", cont=ggroup(cont=s_gp), handler = function(h,...) {
        svalue(c_names, index=TRUE) <- 1:length(names(data_set))
    })
    b_selall$set_icon("select-all")
    b_clear <- gbutton("Clear", cont=ggroup(cont=s_gp), handler = function(h,...) {
        svalue(c_names, index=TRUE) <- integer()
    })
    
    ## centralized handler helper fun for display button
    h_disp <- function(h, ...) {
        rows <- svalue(row_filter)
        cnms <<- svalue(c_names)
        idxs <<- which(rows)                  # move to global variable
        
        ## detect size of data frame to be displayed
        if(length(cnms)==1){
            data_set_dim <- c(length(idxs), 1)
        } else data_set_dim <- dim(data_set[idxs, cnms])
        if(any(data_set_dim < c(1,2))){
            enabled(b_disp) <- FALSE
        } else enabled(b_disp) <- TRUE
        
        blockHandler(b_disp)
        ## dynamically update 'display' button label given current selection
        svalue(b_disp, append=T) <- paste('Display selection (', data_set_dim[1], 
                                          ' x ', data_set_dim[2], ')', sep='')
        b_disp$set_icon("execute")
        #font(b_disp) <- list(weight = "bold")
        unblockHandler(b_disp)
        
        ## autoupdate when option checked and button enabled
        if( svalue(cb_autoupdate) & enabled(b_disp) ) b_disp$invoke_change_handler()
    }
    addHandlerChanged(c_names, h_disp)
    addHandlerChanged(b_selall, h_disp)
    addHandlerChanged(b_clear, h_disp)
    
    ## Filter rows by logical
    r_gp <- gframe("<b>Filter rows:</b>", markup=TRUE, cont=f_side, horizontal=FALSE)
    row_filter <- gfilter(data_set, cont=r_gp, expand=TRUE)
    addHandlerChanged(row_filter, h_disp)
    
    b_disp <- gbutton(paste("Display selection (", data_set_dim_orig[1], ' x ', 
                            data_set_dim_orig[2], ')', sep=''), expand=TRUE, 
                      cont=ggroup(cont=f_side), handler=function(h,...) {
        cnms <<- svalue(c_names)
        rows <- svalue(row_filter)
        
        idxs <<- which(rows)                  # move to global variable
        ## now add a data frame
        delete(df_box, df_box[1])             # remove child
        DF <<- gdf(data_set[rows, cnms], cont=df_box, expand=TRUE)
        DF$set_selectmode("multiple")
        
        ## use "edited" dirty flag
        addHandlerChanged(DF, handler=function(h,...){
            if(!grepl('*', svalue(w), fixed=T)){
                enabled(do_btn) <- TRUE
                svalue(w) <- paste( svalue(w), '*', sep='')
            }
        })
        if(grepl('*', svalue(w), fixed=T)){
            enabled(do_btn) <- FALSE
            svalue(w) <- paste( substr(svalue(w), 1, (nchar(svalue(w))-1)), sep='')
        }
        data_set_dim <- dim(data_set[idxs, cnms])
        
        ##!!editable checkbox 
        #sapply(1:data_set_dim[2], function(j) editable(DF, j) <- FALSE)
        
        ## custom message when displaying full database.
        if(all(data_set_dim == data_set_dim_orig)){
            svalue(gs_df) <- paste("Currently displaying the full data set.", sep='')
        } else {
            svalue(gs_df) <- paste('Currently displaying a ', data_set_dim[1], ' x ', 
                               data_set_dim[2], " subset.", sep='')
        }
    })
    enabled(b_disp) <- TRUE
    b_disp$set_icon("execute")
    #font(b_disp) <- list(weight = "bold")
    ## allow to automatically update the viewed subset
    cb_autoupdate <- gcheckbox('Update automatically', cont=ggroup(cont=f_side))
    tooltip(cb_autoupdate) <- "Check to refresh the displayed dataset \nas soon as the column or row selections change."
    
    size(w) <- c(600, 500)
    visible(w) <- TRUE
    svalue(pg) <- 0.38
    #pg$widget$setPosition(290)
    
    ## What to do when you do ...
    addHandlerClicked(do_btn, function(h,...) {
        ## change me to your liking
       if(gconfirm('Merge changes into the original data frame?', 'Confirm merge...', 
                icon='question')) {
           ##!!graciously reintegrate when row/col deletion
           ##!!disallow row/col delete when not all columns/rows are displayed
            data_set[idxs, cnms] <<- DF[]
            assign(data_set_name, data_set, .GlobalEnv)
            enabled(do_btn) <- FALSE
            svalue(w) <- paste( substr(svalue(w), 1, (nchar(svalue(w))-1)), sep='')
            galert("The original data frame has been modified.", parent=w)
       } else {
           #galert("Modifications cancelled.", parent=w)
       }
    }) 

    ## use 4 lines as hight of selection box
    size(c_names)[2] <- 4*25
    #print(size(pg))
    #print(size(c_names))
    
}

require(MASS)
Xa <- Cars93 ## this will be in a function... replace with your won
dffilter(Xa)
