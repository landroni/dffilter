## edit a really large data set *after* it has been filtered

# DF <- NULL                                  # global gdf instance
# idxs <- NULL                                # global set of indices that are being edited
# cnms <- NULL                                # global column names


##!!experiment with hide left pane layout
##!!reload data.frame (automatically poll for changes?); bug when modifying a level it doesn't update the filters
##!!hack 'grepl' search into gfilter(); regex?; 
##??on display button, log subsetting operation in the console
##??optimize return() beahviour, confirmation, on-the-fly, discard/save&close button, undo/redo, what 5gb dataset, display diff before confirm merge, etc.
##??waht happens when alter 'other' variables (Date & robustness of editor)

##!!fix arguments (mv inside fun def)
dffilter <- function(data_set, DF = NULL, idxs = NULL, cnms = NULL){
    require(gWidgets2) ## on github not CRAN. (require(devtools); install_github("gWidgets2", "jverzani")
    options(guiToolkit="RGtk2")
    require(RGtk2)
    
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
#     addHandlerChanged(w, "size-allocate", function(h,...) {
#         print(sapply(f_side0$children, function(u) size(u)))
#     })
    pg <- gpanedgroup(cont=w, horizontal=TRUE)
    #pg <- ggroup(cont=w, horizontal=TRUE)
    
    f_side0 <- gvbox(cont=pg, use.scrollwindow=FALSE)
    f_side0g <- ggroup(cont=f_side0)
    b_hide <- gbutton("Hide", cont=ggroup(cont=f_side0g))
    addHandlerClicked(b_hide, handler=function(h, ...) {
        val <- svalue(h$obj)
        if(val == "Hide") {
            delete(f_side0, f_side1)
            #svalue(pg) <- as.integer(size(b_hide)[1])
            tooltip(b_hide) <- "Show panel"
            print(sapply(f_side0$children, function(u) size(u)))
        } else {
            add(f_side0, f_side1, expand=T)
            svalue(pg) <- as.integer(size(c_names)[1] + 0)
            tooltip(b_hide) <- "Hide panel"
            print(sapply(f_side0$children, function(u) size(u)))
        }
        blockHandlers(h$obj)
        svalue(h$obj) = ifelse(val == "Hide", "Show", "Hide")
        if(svalue(h$obj) == "Hide") {
            b_hide$set_icon("go-back")
        } else {
            b_hide$set_icon("go-forward")
            svalue(pg) <- as.integer(size(b_hide)[1])
        }
        unblockHandler(h$obj)
        #size
    })
    b_hide$set_icon("go-back")
    tooltip(b_hide) <- "Hide panel"
    #b_reload <- gbutton("Reload", cont=ggroup(cont=f_side0))
    #b_reload$set_icon("refresh")
    
    f_side1 <- gvbox(cont=f_side0, use.scrollwindow=TRUE, expand=T)
    f_side1$block$setPolicy("GTK_POLICY_NEVER", "GTK_POLICY_AUTOMATIC")
    
    df_side <- gvbox(cont = pg, expand=TRUE)
    
    df_box <- ggroup(cont=df_side, expand=TRUE) ## holds df instance
    glabel("Select columns to be displayed \nand define appropriate row filters,\nthen click the 'Display selection' button. \nIf you make changes to your data, you \ncan merge them into the original dataset.", cont=df_box)
    
    btn_gp <- ggroup(cont = df_side)
    do_btn <- gbutton("Merge changes...", cont=btn_gp)
    do_btn$set_icon("ok")
    enabled(do_btn) <- FALSE
    addSpring(btn_gp)
    close_btn <- gbutton("Close", cont=btn_gp, handler=function(h,...){
        dispose(w)
    })
    gs_df <- gstatusbar('', cont=df_side)
    
    ## set up filters.
    ## Select columns
    c_gp <- gframe("<b> Select columns: </b>", markup=TRUE, cont=f_side1, 
                   horizontal=FALSE)
    c_names <- gcheckboxgroup(names(data_set), checked=TRUE, cont=c_gp, 
                              use.table=TRUE, expand=TRUE)
    s_gp <- ggroup(cont=c_gp, horizontal=TRUE)
    
    ## Invert selection, select all and select none are all useful in different cases
    b_invert <- gbutton("Invert", cont=ggroup(cont=s_gp), handler = function(h,...) {
        svalue(c_names, index=TRUE) <- setdiff(1:length(names(data_set)), 
                                               svalue(c_names, index=TRUE))
    })
    tooltip(b_invert) <- 'Invert selection'
    b_selall <- gbutton("Select all", cont=ggroup(cont=s_gp), handler = function(h,...) {
        svalue(c_names, index=TRUE) <- 1:length(names(data_set))
    })
    tooltip(b_selall) <- 'Select all'
    b_selall$set_icon("select-all")
    b_clear <- gbutton("Clear", cont=ggroup(cont=s_gp), handler = function(h,...) {
        svalue(c_names, index=TRUE) <- integer()
    })
    tooltip(b_clear) <- 'Select none'
    
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
    addHandlerChanged(b_invert, h_disp)
    addHandlerChanged(b_clear, h_disp)
    
    ## Filter rows by logical
    r_gp <- gframe("<b>Filter rows:</b>", markup=TRUE, cont=f_side1, horizontal=FALSE)
    row_filter <- gfilter(data_set, cont=r_gp, expand=TRUE)
    addHandlerChanged(row_filter, h_disp)
    
    b_disp <- gbutton(paste("Display selection (", data_set_dim_orig[1], ' x ', 
                            data_set_dim_orig[2], ')', sep=''), expand=TRUE, 
                      cont=ggroup(cont=f_side1), handler=function(h,...) {
        cnms <<- svalue(c_names)
        rows <- svalue(row_filter)
        
        idxs <<- which(rows)                  # move to global variable
        
        ## now add a data frame
        delete(df_box, df_box[1])             # remove child
        
        ##!!editable checkbox "disable editing"
        #sapply(1:data_set_dim[2], function(j) editable(DF, j) <- FALSE)
        
        ## disallow row/col c-menu when not all columns/rows are displayed (freeze_attributes=TRUE)
        if( all(c(length(idxs), length(cnms)) == data_set_dim_orig) ){
            ## display full data set
            DF <<- gdf(data_set[rows, cnms], cont=df_box, expand=TRUE)
        } else if( all(c(length(idxs), length(cnms)) != data_set_dim_orig) ){
            ## display subset (fewer rows/columns)
            DF <<- gdf(data_set[rows, cnms], cont=df_box, expand=TRUE, 
                       freeze_attributes=TRUE)
        } else if( all(c(length(idxs) != data_set_dim_orig[1], 
                         length(cnms) == data_set_dim_orig[2])) ){
            ## display subset (fewer rows / all columns)
            DF <<- gdf(data_set[rows, cnms], cont=df_box, expand=TRUE, 
                       freeze_attributes="column")
        } else if( all(c(length(idxs) == data_set_dim_orig[1], 
                         length(cnms) != data_set_dim_orig[2])) ){
            ## display subset (all rows / fewer columns)
            DF <<- gdf(data_set[rows, cnms], cont=df_box, expand=TRUE, 
                       freeze_attributes="row")
        }
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
    cb_autoupdate <- gcheckbox('Update automatically', cont=ggroup(cont=f_side1))
    tooltip(cb_autoupdate) <- "If checked refresh the displayed dataset \nas soon as the column or row selections change."
    
    ##!!editable checkbox (freeze_attributes=TRUE)
#         cb_do_btn <- gcheckbox('Allow editing', checked=FALSE, cont=ggroup(cont=f_side1), 
#                                handler=function(h,...){
#                                    if((grepl('*', svalue(w), fixed=T) & 
#                                            svalue(cb_do_btn))){
#                                        enabled(do_btn) <- TRUE
#                                    }
#         })
#         tooltip(obj=cb_do_btn) <- "If checked allow editing of displayed subsets \nin a spreadsheet-like environment."
    
    size(w) <- c(600, 500)
    visible(w) <- TRUE
    svalue(pg) <- as.integer(size(b_disp)[1] + 20)
    #svalue(pg) <- 0.42
    #svalue(pg) <- 250L
    
    ## What to do when you do ...
    addHandlerClicked(do_btn, function(h,...) {
        ## change me to your liking
       if(gconfirm('Merge changes into the original data frame?', 'Confirm merge...', 
                icon='question')) {
            ##!!graciously reintegrate when row/col deletion/insertion or NA vals present
            ##!!bug when editing factor levels (not reintegrated)
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
    #print(size(s_gp))
    #print(sapply(f_side0g$children, function(u) size(u)))
}

require(MASS)
Xa <- Cars93 ## this will be in a function... replace with your won
#Xa[3:7,1] <- NA
# Xa$Model1 <- as.character(Xa$Model)
# Xa[2,'Model1'] <- paste(rep(letters, 26), collapse='')
dffilter(Xa)

## gfilter TODO items
##!!inspect the gfilter code (logical vector selection, use spinners for 'range', head/tail/some/!is.na/na.omit, manually update filter items to reflect available choices as a 'update filters' button or 'freeze selection' checkbox?, what happens to 'Date' or 'other' classes, use rgtk2editdf, 'enable/disable filter' toggle button next to 'remove', )
##!!have 'select all/reset' (done) and 'clear' buttons
##!!bug when NA in variables (detect gracefully, )
##Error in which(sapply(widgets, function(i) i$getActive())) : 
##    argument to 'which' is not logical
##!!adding new filter should consider merged changes (reload data frame button?)
##??type-ahead search in combobox gfilter and in cehckbox table? (as in LyX?)
##!!central RStudio-like search and dynamically filter GUI elements
##??sort variable components?
##??gfilterpreset (use c("==", "%in%", "!=", ">", ">=", "<", "<=") combo, logical / manual subset, )

## tangential TODO items
##??data frame selector (use data frame browser; what about matrix objs?) & label(..., self=T) in tooltip and make  label editable & describe() & class() & nchar()
##??sort  & re-order columns (in gdf()? or separately w/o loading the full data frame?; have distinct window)
##??diff (papertrail; setdiff2 {prob} using github green/red & darkgreen/darkred colouring approach; 'dataview'; split-window with side-by-side display as in diffPDF; what happens when the two df have different nr of rows/columns?; also diff two variables within data.frame?)
##??replace2 (gui for replacement tables)
