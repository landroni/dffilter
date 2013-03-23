## edit a really large data set *after* it has been filtered

# DF <- NULL                                  # global gdf instance
# idxs <- NULL                                # global set of indices that are being edited
# cnms <- NULL                                # global column names

##??optimize return() beahviour, confirmation, on-the-fly, etc.
df_filter <- function(data_set, DF = NULL, idxs = NULL, cnms = NULL){
    library(gWidgets2) ## on github not CRAN. (require(devtools); install_github("gWidgets2", "jverzani")
    options(guiToolkit="RGtk2")
    ##!!test for data.frmae or matrix
    data_set_name <- deparse(substitute(data_set))
    
    ##??data frame selector (use data frame browser)
    data_set_dim_orig <- dim(data_set)
    w <- gwindow(paste(data_set_name, " (", data_set_dim_orig[1], ' x ', 
                       data_set_dim_orig[2], ')', sep=''), visible=FALSE, 
                 handler=function(h,...){
        #if(identical)
        #return(data_set)
    })
    pg <- gpanedgroup(w, horizontal=TRUE)
    
    ##??minimal scrollwindow width
    ##??f4 to hide left pane
    f_side <- gvbox(cont=pg, use.scrollwindow=TRUE)
    df_side <- gvbox(cont = pg, expand=TRUE)
    
    df_box <- ggroup(cont=df_side, expand=TRUE) ## holds df instance
    glabel("Select columns to be displayed \nand define appropriate row filters,\nthen click the 'Display selection' button. \nIf you make changes to your data, you \ncan merge them into the original dataset.", cont=df_box)
    
    btn_gp <- ggroup(cont = df_side)
    ##!!close button (or discard/save&close)
    do_btn <- gbutton("Merge changes...", cont=btn_gp)
    visible(do_btn) <- FALSE
    enabled(do_btn) <- FALSE
    gs_df <- gstatusbar('', cont=df_side)
    
    ## set up filters.
    ## Select columns
    ##!!increase hight of selection box; use size()
    ##??reload data.frame
    c_gp <- gframe("<b> Select columns: </b>", markup=TRUE, cont=f_side, horizontal=FALSE)
    c_names <- gcheckboxgroup(names(data_set), cont=c_gp, use.table=TRUE, expand=TRUE)
    s_gp <- ggroup(cont=c_gp, horizontal=TRUE)
    # gbutton("Invert", cont=ggroup(cont=s_gp), handler = function(h,...) {
    #     svalue(c_names, index=TRUE) <- setdiff(1:length(names(data_set)), 
    #                                            svalue(c_names, index=TRUE))
    # })
    b_selall <- gbutton("Select all", cont=ggroup(cont=s_gp), handler = function(h,...) {
        svalue(c_names, index=TRUE) <- 1:length(names(data_set))
    })
    b_clear <- gbutton("Clear", cont=ggroup(cont=s_gp), handler = function(h,...) {
        svalue(c_names, index=TRUE) <- integer()
    })
    
    ##!!check dim() bug when 'atomic'
    ## centralized handler helper fun for display button
    b_disp <- function(h, ...) {
        rows <- svalue(row_filter)
        cnms <<- svalue(c_names)
        idxs <<- which(rows)                  # move to global variable
        data_set_dim <- dim(data_set[idxs, cnms])
        blockHandler(b_disp)
        svalue(b_disp, append=T) <- paste('Display selection (', data_set_dim[1], 
                                          ' x ', data_set_dim[2], ')', sep='')
        #font(b_disp) <- list(weight = "bold")
        unblockHandler(b_disp)
    }
    addHandlerChanged(c_names, b_disp)
    addHandlerChanged(b_selall, b_disp)
    addHandlerChanged(b_clear, b_disp)
    
    
    ## Filter rows by logical
    r_gp <- gframe("<b>Filter rows:</b>", markup=TRUE, cont=f_side, horizontal=FALSE)
    ##!!inspect the code (logical vector selection, use spinners for 'range', have 'select all', rename radio/choice to single/multiple, head/tail/some, use combo evern for 3 radio choices, )
    row_filter <- gfilter(data_set, cont=r_gp, expand=TRUE)
    addHandlerChanged(row_filter, b_disp)
    ##!!add actual 'grepl' search
    
    ##!!editable checkbox (for(j in 1:ncol(DF)) set_editable=function(j, value=FALSE))
    ##!!automatic update checkbox
    b_disp <- gbutton("Display selection", expand=TRUE, cont=ggroup(cont=f_side), 
                      handler=function(h,...) {
        visible(do_btn) <- TRUE
        cnms <<- svalue(c_names)
        rows <- svalue(row_filter)
        
        
        idxs <<- which(rows)                  # move to global variable
        ## now add a data frame
        delete(df_box, df_box[1])             # remove child
        DF <<- gdf(data_set[rows, cnms], cont=df_box, expand=TRUE)
        DF$set_selectmode("multiple")
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
        svalue(gs_df) <- paste('Currently displaying a ', data_set_dim[1], ' x ', 
                               data_set_dim[2], ' subset of ', data_set_name, '.', sep='')
    })
    #font(b_disp) <- list(weight = "bold")
    
    size(w) <- c(600, 500)
    visible(w) <- TRUE
    svalue(pg) <- 0.33
    
    
    ##??undo/redo
    ## What to do when you do ...
    addHandlerClicked(do_btn, function(h,...) {
        ## change me to your liking
       if(gconfirm('Merge changes into the original data frame?', 'Confirm merge...', 
                icon='question')) {
            data_set[idxs, cnms] <<- DF[]
            assign(data_set_name, data_set, .GlobalEnv)
            enabled(do_btn) <- FALSE
            svalue(w) <- paste( substr(svalue(w), 1, (nchar(svalue(w))-1)), sep='')
            galert("The original data frame has been modified.", parent=w)
       } else {
           #galert("Modifications cancelled.", parent=w)
       }
    }) 
}

#require(MASS)
Xa <- Cars93 ## this will be in a function... replace with your won
df_filter(Xa)
