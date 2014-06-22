## edit a really large data set *after* it has been filtered

dffilter <- function(data_set, display=TRUE, maximize=TRUE, editable=FALSE){
    require(gWidgets2) ## on github not CRAN. (require(devtools); install_github("gWidgets2", "jverzani")
    options(guiToolkit="RGtk2")
    require(RGtk2)

    DF <- NULL                      # global gdf instance
    idxs <- NULL                    # global set of indices that are being edited
    cnms <- NULL                    # global column names
    c_names <- NULL                 # global column names widget
    old_selection <- NULL           # global old selection storage
    
    ## ensure we have a data frame of 1x2 dimensions
    stopifnot(is.data.frame(data_set))
    data_set_name <- deparse(substitute(data_set))
    data_set_nms <- names(data_set)
    ##FIXME find most efficient way to determine size of df
    data_set_dim_orig <- dim(data_set)
    stopifnot(all(data_set_dim_orig >= c(1,2)))
    
    w <- gwindow(paste(data_set_name, " (", data_set_dim_orig[1], ' x ', 
                       data_set_dim_orig[2], ')', sep=''), visible=FALSE, 
                 handler=function(h,...){
                     #if(identical)
                     #return(data_set)
                 })
                 
    ##maximize window on load
    if(maximize) getToolkitWidget(w)$maximize()
    
    pg <- gpanedgroup(cont=w, horizontal=TRUE)
    #pg <- ggroup(cont=w, horizontal=TRUE)
    
    ## have a hide/show button
    f_side0 <- gvbox(cont=pg, use.scrollwindow=FALSE, 
                     resize=FALSE, shrink=FALSE)
    f_side0g <- ggroup(cont=f_side0)
    b_hide <- gbutton("Hide", cont=ggroup(cont=f_side0g))
    addHandlerClicked(b_hide, handler=function(h, ...) {
        val <- svalue(h$obj)
        if(val == "Hide") {
            delete(f_side0, f_side1)
        } else {
            add(f_side0, f_side1, expand=T)
        }
        blockHandlers(h$obj)
        svalue(h$obj) = ifelse(val == "Hide", "Show", "Hide")
        if(svalue(h$obj) == "Hide") {
            b_hide$set_icon("go-back")
            #print(sapply(f_side0$children, function(u) size(u)))
            svalue(pg) <- as.integer(size(c_names)[1] + 10)
            tooltip(b_hide) <- "Hide panel"
        } else {
            b_hide$set_icon("go-forward")
            #print(sapply(f_side0$children, function(u) size(u)))
            svalue(pg) <- as.integer(size(b_hide)[1])
            tooltip(b_hide) <- "Show panel"
        }
        unblockHandler(h$obj)
    })
    b_hide$set_icon("go-back")
    tooltip(b_hide) <- "Hide panel"

    ## have a reload button
    #b_reload <- gbutton("Reload", cont=ggroup(cont=f_side0))
    #b_reload$set_icon("refresh")
    
    f_side1 <- gvbox(cont=f_side0, use.scrollwindow=TRUE, expand=TRUE)
    
    df_side <- gvbox(cont = pg, expand=TRUE)
    
    df_box <- ggroup(cont=df_side, expand=TRUE) ## holds df instance
    glabel("Select columns to be displayed \nand define appropriate row filters,\nthen click the 'Display selection' button. \nIf you make changes to your data, you \ncan merge them into the original dataset.", cont=df_box)
    
    btn_gp <- ggroup(cont = df_side)
    if(editable) {
        do_btn <- gbutton("Merge changes...", cont=btn_gp)
        do_btn$set_icon("ok")
        enabled(do_btn) <- FALSE
    }
    addSpring(btn_gp)
    close_btn <- gbutton("Close", cont=btn_gp, handler=function(h,...){
        dispose(w)
    })
    gs_df <- gstatusbar('', cont=df_side)
    
    ## set up filters.
    ## Select columns
    c_gp <- gframe("<b> Select columns: </b>", markup=TRUE, cont=f_side1, 
                   horizontal=FALSE)

    ##fancy search for selecting columns
    ##prepare the search input box & handler
     vb <- gvbox(container=c_gp)
     search_type <-  list(ignore.case=TRUE, perl=FALSE, fixed=FALSE)  ##init global instance
       gp <- ggroup(cont=vb)
       ed <- gedit("", initial.msg="Filter values by...", expand=TRUE, container=gp)
       ed$set_icon("ed-search", "start")
       ed$set_icon("ed-remove", "end")
       ed$set_icon_handler(function(h,...) {
         svalue(ed) <- ""
       }, where="end")
       
       search_handler <- function(h,..., do_old=TRUE) {
         ## we keep track of old selection here
         ## that updates only when user changes selection, not when filter does
         cur_sel <- old_selection
         blockHandlers(c_names)
         on.exit(unblockHandlers(c_names))
         val <- svalue(ed)

         if(val == "") {
           c_names[] <<- data_set_nms
         } else {
           l <- c(list(pattern=val, x=data_set_nms), search_type)
           new_vals = data_set_nms[do.call(grepl, l)]
           if (length(new_vals)) {
             c_names[] <<- new_vals
           } else {
             c_names[] <<- character(0) 
             return()
           }
         }
         svalue(c_names) <<- cur_sel
       }

       b <- gbutton("opts", cont=gp)
       cbs <- list(gcheckbox("Ignore case", checked=TRUE, handler=function(h,...) {
                             search_type[["ignore.case"]] <<- svalue(h$obj)
                             search_handler(do_old=FALSE)
                             }),
                   gcheckbox("Regex", checked=TRUE, handler=function(h,...) {
                     search_type[["fixed"]] <<- !svalue(h$obj)
                     search_handler(do_old=FALSE)                                                     
                   }),
                   gcheckbox("Perl compatible", checked=FALSE, handler=function(h,...) {
                     search_type[["perl"]] <<- svalue(h$obj)
                     search_handler(do_old=FALSE)                                                     
                   })
                   )
       
       addPopupMenu(b, gmenu(cbs, popup=TRUE))

       addHandlerKeystroke(ed, search_handler)
       addHandlerChanged(ed, search_handler)


    c_names <- gcheckboxgroup(data_set_nms, checked=TRUE, cont=c_gp, 
                              use.table=TRUE, expand=TRUE, fill=TRUE)
    
    ##continue fancy search functionality
    ##initialize old_selection which will be the output value of c_names
     old_selection <- svalue(c_names)
     #svalue(c_names, index=TRUE) <<- TRUE

                              
    s_gp <- ggroup(cont=c_gp, horizontal=TRUE)
    
    ## Invert selection, select all and select none are all useful in different cases
    ##FIXME are the h_disp() calls redundant?
    b_invert <- gbutton("", cont=ggroup(cont=s_gp), handler = function(h,...) {
        svalue(c_names, index=TRUE) <<- setdiff(1:length(data_set_nms), 
                                               svalue(c_names, index=TRUE))
        h_disp()
    })
    tooltip(b_invert) <- 'Invert selection'
    b_invert$set_icon("jump-to")
 
    b_selall <- gbutton("Select all", cont=ggroup(cont=s_gp), handler = function(h,...) {
        #svalue(c_names, index=TRUE) <- 1:length(data_set_nms)
        svalue(c_names, index=TRUE) <<- TRUE
        #c_names$invoke_change_handler()
        h_disp()
    })
    tooltip(b_selall) <- 'Select all'
    b_selall$set_icon("select-all")

    b_clear <- gbutton("Clear", cont=ggroup(cont=s_gp), handler = function(h,...) {
        #svalue(c_names, index=TRUE) <- integer()
        svalue(c_names, index=TRUE) <<- FALSE
        h_disp()
    })
    tooltip(b_clear) <- 'Select none'
    
    ## Filter rows by logical
    r_gp <- gframe("<b>Filter rows:</b>", markup=TRUE, cont=f_side1, horizontal=FALSE)
    row_filter <- gfilter(data_set, initial.vars=data.frame(data_set_nms[1], "preset", 
                                                            "preset", stringsAsFactors=FALSE), 
                          cont=r_gp, expand=TRUE)

    ## centralized handler helper fun to update size in 'display' button
    h_disp <- function(h, ...) {
        rows <- svalue(row_filter)
        #cnms <<- svalue(c_names)
        cnms <<- old_selection
        idxs <<- which(rows)                  # move to global variable
        
        ## detect size of data frame to be displayed
        if(length(cnms)==1){
            data_set_dim <- c(length(idxs), 1)
        #} else data_set_dim <- dim(data_set[idxs, cnms])
        } else data_set_dim <- c(length(idxs), length(cnms))
        if(any(data_set_dim < c(1,2))){
            enabled(b_disp) <- FALSE
        } else enabled(b_disp) <- TRUE
        
        blockHandler(b_disp)
        ## dynamically update 'display' button label given current selection
        svalue(b_disp, append=T) <- paste('Display selection (', data_set_dim[1], 
                                          ' x ', data_set_dim[2], ')', sep='')
        b_disp$set_icon("execute")
        font(b_disp) <- list(weight = "bold")
        unblockHandler(b_disp)
        
        ## autoupdate when option checked and button enabled
        if( svalue(cb_autoupdate) & enabled(b_disp) ) b_disp$invoke_change_handler()
    }
    #addHandlerChanged(c_names, h_disp)
    addHandlerChanged(b_selall, h_disp)
    addHandlerChanged(b_invert, h_disp)
    addHandlerChanged(b_clear, h_disp)
    addHandlerChanged(row_filter, h_disp)
    
    ##handler for fancy search functionality
    ##needs to be after h_disp() is defined
     addHandlerChanged(c_names, function(h,...) {
       ### XXX selection
       ## have to be careful, as items may be narrowed
       visible_items = c_names[]
       new <- svalue(h$obj)
       old <- intersect(visible_items, old_selection)

       
       added <- setdiff(new, old)
       removed <- setdiff(old, new)

       ## This is sort of tricky, not sure it is correct
       if(length(added) > 0) {
         old_selection <<- unique(c(old_selection, added))
       }
       if(length(removed) > 0) {
         old_selection <<- setdiff(old_selection, removed)
       }
       old_selection <<- data_set_nms[data_set_nms %in% old_selection]
       h_disp()
     })                              
    
    ##handler to execute on click of 'display' button
    hb_disp <- function(h,...) {
        rows <- svalue(row_filter)
        #cnms <<- svalue(c_names)
        cnms <<- old_selection
        idxs <<- which(rows)                  # move to global variable
        
        ## now add a data frame
        delete(df_box, df_box[1])             # remove child
        
        ## disable editing if so requested
        #data_set_dim <- dim(data_set[idxs, cnms])
        ##FIXME mv this to global var
        data_set_dim <- c(length(idxs), length(cnms))
        if(!editable) {
            DF <<- gdf(data_set[rows, cnms], cont=df_box, expand=TRUE, 
                       freeze_attributes=TRUE)
            sapply(1:data_set_dim[2], function(j) editable(DF, j) <- FALSE)
            
            ## if(editable), disallow row/col c-menu when not all columns/rows are displayed (freeze_attributes=TRUE)
        } else if( all(data_set_dim == data_set_dim_orig) ){
            ## display full data set
            DF <<- gdf(data_set[rows, cnms], cont=df_box, expand=TRUE)
        } else if( all(data_set_dim != data_set_dim_orig) ){
            ## display subset (fewer rows/columns)
            DF <<- gdf(data_set[rows, cnms], cont=df_box, expand=TRUE, 
                       freeze_attributes=TRUE)
        } else if( all(data_set_dim[1] != data_set_dim_orig[1], 
                       data_set_dim[2] == data_set_dim_orig[2])) {
            ## display subset (fewer rows / all columns)
            DF <<- gdf(data_set[rows, cnms], cont=df_box, expand=TRUE, 
                       freeze_attributes="column")
        } else if( all(data_set_dim[1] == data_set_dim_orig[1], 
                       data_set_dim[2] != data_set_dim_orig[2])) {
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
        
        ## custom message when displaying full database.
        if(all(data_set_dim == data_set_dim_orig)){
            svalue(gs_df) <- paste("Displaying the full data set.", sep='')
        } else {
            svalue(gs_df) <- paste('Displaying a ', data_set_dim[1], ' x ', 
                                   data_set_dim[2], " subset.", sep='')
        }
        font(b_disp) <- list(weight = "normal")

    }
    
    b_disp <- gbutton(paste("Display selection (", data_set_dim_orig[1], ' x ', 
                            data_set_dim_orig[2], ')', sep=''), expand=TRUE, 
                      cont=ggroup(cont=f_side1), handler=hb_disp)
    enabled(b_disp) <- TRUE
    b_disp$set_icon("execute")
    #font(b_disp) <- list(weight = "bold")
    ## allow to automatically update the viewed subset
    cb_autoupdate <- gcheckbox('Update automatically', cont=ggroup(cont=f_side1))
    tooltip(cb_autoupdate) <- "If checked refresh the displayed dataset \nas soon as the column or row selections change."
    
    ##??editable checkbox (freeze_attributes=TRUE)
    #         cb_do_btn <- gcheckbox('Allow editing', checked=FALSE, cont=ggroup(cont=f_side1), 
    #                                handler=function(h,...){
    #                                    if((grepl('*', svalue(w), fixed=T) & 
    #                                            svalue(cb_do_btn))){
    #                                        enabled(do_btn) <- TRUE
    #                                    }
    #         })
    #         tooltip(obj=cb_do_btn) <- "If checked allow editing of displayed subsets \nin a spreadsheet-like environment."
    
    h_disp()  ##update display button size given 'preset' filter
    size(w) <- c(750, 600)
    visible(w) <- TRUE
    svalue(pg) <- as.integer(size(b_disp)[1] + 20)
    #if(display) hb_disp()
    if(display) b_disp$invoke_change_handler()
    #svalue(pg) <- 0.42
    #svalue(pg) <- 250L
    
    ## What to do when you do ...
    if(editable){
        addHandlerClicked(do_btn, function(h,...) {
            ## change me to your liking
            if(gconfirm('Merge changes into the original data frame?', 'Confirm merge...', 
                        icon='question')) {
                ##!!graciously reintegrate when row/col deletion/insertion or NA vals present
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
    ## use 5 lines as hight of selection box (less claustrophobic)
    size(c_names)[2] <- 5*25
    #print(size(pg))
    #print(size(c_names))
    #print(size(s_gp))
    #print(sapply(f_side0g$children, function(u) size(u)))
}

# require(MASS)
# Xa <- Cars93 ## this will be in a function... replace with your won
# Xa[3:7,1] <- NA
# Xa[3:7,"Price"] <- NA
# Xa$Model1 <- as.character(Xa$Model)
# Xa[2,'Model1'] <- paste(rep(letters, 26), collapse='')
# Xa$Man.trans.avail1 <- as.logical(Xa$Man.trans.avail)
# Xa$Man.trans.avail1 <- ifelse(Xa$Man.trans.avail=='Yes', TRUE, FALSE)
View <- dffilter
#View(Xa)
