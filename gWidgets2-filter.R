## edit a really large data set *after* it has been filtered

dffilter <- function(data_set, display=TRUE, maximize=TRUE, editable=FALSE, 
                     data_set_name=NULL, sel.col=NULL, sel.row=NULL, esc=FALSE, 
                     def.col=100, details=TRUE, details.on.tab.sel=TRUE, 
                     confirm.big.df=TRUE, 
                     initial.vars=data.frame(data_set_nms[1], "preset", "preset", 
                        stringsAsFactors=FALSE)
                     ){
    require(gWidgets2) ## on github not CRAN. (require(devtools); install_github("gWidgets2", "jverzani")
    options(guiToolkit="RGtk2")
    require(RGtk2)
    ##FIXME put this in the handler
    if(details) require(Hmisc)
    

    DF <- NULL                      # global gdf instance
    rows <- NULL                    # global rows index
    rows.disp <- NULL               # global rows index (subset currently displayed)
    rows.disp_old <- list()               # global rows index (subset previously displayed)
    idxs <- NULL                    # global set of indices that are being edited
    cnms <- NULL                    # global column names
    cnms.disp <- NULL               # global column names (subset currently displayed)
    cnms.disp_old <- list()               # global column names (subset currently displayed)
    len_idxs <- NULL                # global set of indices that are being edited (length)
    len_cnms <- NULL                # global column names (length)
    data_set_dim <- NULL            # global df dim
    c_names <- NULL                 # global column names widget
    old_selection <- NULL           # global old selection storage
    radio.inst <- NULL
    radio.sel <- NULL
    DF_deb <- NULL
    details.out <- list()
    new.disp <- FALSE
    filter.types <- c("single"="RadioItem", "multiple"="ChoiceItem", 
        "range"="RangeItem", "preset"="PresetItem")
    
     #print(data_set_name)
     #print(class(sel.row))
     #print(length(sel.row))
     #for(i in sel.row) print(class(i)[1])
    #for(i in 1:length(sel.row)) print(svalue(sel.row[[i]]))
    
    ## ensure we have a data frame of 1x2 dimensions
    stopifnot(is.data.frame(data_set))
    if(is.null(data_set_name)) data_set_name <- deparse(substitute(data_set))
    if(!is.character(data_set_name)) data_set_name <- as.character(data_set_name)
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
    
    ##set WM icon (gtk-only)
    w_img <- gdkPixbufNewFromFile("gtk-logo-rgb.gif")
    getToolkitWidget(w)$setIcon(w_img$retval)
                 
    ##maximize window on load
    if(maximize) getToolkitWidget(w)$maximize()
    
    ##Filter tab
    ntbk <- gnotebook(3, cont=w)
    pg <- gpanedgroup(cont=ntbk, horizontal=TRUE, label=" Filter")
    ntbk$add_tab_icon(1, "find")
    ntbk$add_tab_tooltip(1, "Filter data frame and display subset")
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
    addSpring(f_side0g)
    b_reload <- gbutton("Reload", cont=ggroup(cont=f_side0g))
    addHandlerClicked(b_reload, handler=function(h, ...) {
        #break.point()
        #print(data_set_name)
        #print(class(row_filter))
        #for(i in sel.row) print(class(i)[1])
        #for(i in 1:length(sel.row)) print(svalue(sel.row[[i]]))
        
        dispose(w)
        dffilter_reload(data_set=get(data_set_name), display=display, maximize=maximize, 
                        editable=editable, data_set_name=data_set_name, 
                        sel.col=old_selection, sel.row=row_filter)
    })
    b_reload$set_icon("refresh")
    tooltip(b_reload) <- "Reload data frame"
    
    f_side1 <- gvbox(cont=f_side0, use.scrollwindow=TRUE, expand=TRUE)
    
    df_side <- gvbox(cont = pg, expand=TRUE)
    
    df_box <- ggroup(cont=df_side, expand=TRUE) ## holds DF instance
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
    #gsb_df <- gstatusbar('', cont=df_side)  ##uncomment to enable gtkSpinner() functionality
    gsb_dfl <- gstatusbar('', cont=df_side)
    ##set statusbar for spinner
    #gsb_dff <- gsb_df$block[[1]]

    #gsb_dff$remove(gsb_dff[[1]])                        #remove old
    #gsb_dfg <- ggroup()
    #gsb_dff$add(gsb_dfg$block)                          # add a group

    #gsb_dfl <- glabel("")
    #add(gsb_dfg, gsb_dfl)
    #addSpring(gsb_dfg)
    #gsb_dfsp <- gtkSpinner()
    
    ##inital spin
    #add(gsb_dfg, gsb_dfsp)
    #gsb_dfsp$start()
    #gsb_dfsp$stop()
    #gsb_dfg$widget$remove(gsb_dfsp)
    
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
         focus(ed) <- TRUE
       }, where="end")
       ed$widget$setIconActivatable("primary", FALSE)
       
       search_handler <- function(h,..., do_old=TRUE) {
         ## we keep track of old selection here
         ## that updates only when user changes selection, not when filter does
         cur_sel <- old_selection
         blockHandlers(c_names)
         on.exit(unblockHandlers(c_names))
         val <- svalue(ed)

         if(val == "") {
           c_names[] <<- data_set_nms
           ed$widget$modifyBase(GtkStateType["normal"], NULL)
           ed$widget$modifyText(GtkStateType["normal"], NULL) 
         } else {
           l <- c(list(pattern=val, x=data_set_nms), search_type)
           new_vals = data_set_nms[do.call(grepl, l)]
           if (length(new_vals)) {
             c_names[] <<- new_vals
             ed$widget$modifyBase(GtkStateType["normal"], NULL)
             ed$widget$modifyText(GtkStateType["normal"], NULL) 
           } else {
             c_names[] <<- character(0) 
             ed$widget$modifyBase(GtkStateType["normal"], "#FF6666")
             ed$widget$modifyText(GtkStateType["normal"], "white") 
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
                              
    
    ##if sel.col is supplied (e.g. for reload) check structure to see if all 
    ##selected variables are still present in reloaded data frame
    if(def.col!=0){
        svalue(c_names, index=TRUE) <- 1:min(def.col, data_set_dim_orig[2])
    }
    if(!is.null(sel.col)){
        ##FIXME message to inform user when selection couldn't be restored
        if(all(sel.col %in% data_set_nms)) svalue(c_names) <- sel.col
    } 

    ##continue fancy search functionality
    ##initialize old_selection which will be the output value of c_names
    old_selection <- svalue(c_names)
    #svalue(c_names, index=TRUE) <<- TRUE

                              
    s_gp <- ggroup(cont=c_gp, horizontal=TRUE)
    
    ## Invert selection, select all and select none are all useful in different cases
    b_invert <- gbutton("", cont=ggroup(cont=s_gp), handler = function(h,...) {
        svalue(c_names, index=TRUE) <<- setdiff(1:data_set_dim_orig[2], 
                                               svalue(c_names, index=TRUE))
        #len_cnms_update()
        #h_disp()
    })
    tooltip(b_invert) <- 'Invert selection'
    b_invert$set_icon("jump-to")
 
    b_selall <- gbutton("Select all", cont=ggroup(cont=s_gp), handler = function(h,...) {
        #svalue(c_names, index=TRUE) <- 1:length(data_set_nms)
        svalue(c_names, index=TRUE) <<- TRUE
        #c_names$invoke_change_handler()
        #len_cnms_update()
        #h_disp()
    })
    tooltip(b_selall) <- 'Select all'
    b_selall$set_icon("select-all")

    b_clear <- gbutton("Clear", cont=ggroup(cont=s_gp), handler = function(h,...) {
        #svalue(c_names, index=TRUE) <- integer()
        svalue(c_names, index=TRUE) <<- FALSE
        #len_cnms_update()
        #h_disp()
    })
    tooltip(b_clear) <- 'Select none'
    
    ## Filter rows by logical
    if(!is.null(sel.row)){
       #sel.row[[1]]$make_ui(visible=TRUE)
        #print(length(sel.row$l))
        #print(sel.row$l[[1]]$name)
        #print(sel.row$l[[1]]$type)
        #print(class(sel.row$l[[1]])[1])
        filter.type <- c()
        filter.var <- c()
        for(i in sel.row$l){ 
            filter.type <- c(filter.type , class(i)[1])
            filter.var <- c(filter.var , i$name)
        }
        for(i in 1:length(filter.type)) names(filter.type)[i] <- names(
            filter.types)[filter.types == filter.type[i]]
        initial.vars <- data.frame(vars=filter.var, names=filter.var, 
            filter=names(filter.type), stringsAsFactors=FALSE)
        initial.vars[ initial.vars$filter=="preset", "vars"] <- data_set_nms[1]
        print(initial.vars)
       ##FIXME this should work, but ends up in an error
       #sel.row$l[[1]]$initialize_item()
       #sel.row[[1]]$initialize_item()
       #row_filter <- sel.row
       #row_filter$l[[1]]$initialize_item()
    }
    r_gp <- gframe("<b>Filter rows:</b>", markup=TRUE, cont=f_side1, horizontal=FALSE)
    row_filter <- gfilter(data_set, initial.vars=initial.vars, 
                          cont=r_gp, expand=TRUE, head.def=500)


    ## centralized handler helper fun to update size of row/col selection
    len_idxs_update <- function(h, ...) {
        rows <<- svalue(row_filter)
        ##catch if row_filter outputs all FALSE or empty selection
        if(!any(rows)){
            idxs <<- integer()
            len_idxs <<- 0L
        } else {
            idxs <<- which(rows)                  # move to global variable
            len_idxs <<- length(idxs)                  # move to global variable
        }
        data_set_dim <<- c(len_idxs, len_cnms)
    }
    len_cnms_update <- function(h, ...) {
        cnms <<- old_selection
        len_cnms <<- length(cnms)                  # move to global variable
        data_set_dim <<- c(len_idxs, len_cnms)
    }

    ## centralized handler helper fun to update size in 'display' button
    h_disp <- function(h, ...) {
        #rows <<- svalue(row_filter)
        #idxs <<- which(rows)                  # move to global variable
        #len_idxs <<- length(idxs)                  # move to global variable
        #cnms <<- svalue(c_names)
        #cnms <<- old_selection
        #len_cnms <<- length(cnms)                  # move to global variable
        
        ## detect size of data frame to be displayed
        #print(len_idxs)
        #print(len_cnms)
        #data_set_dim <<- c(len_idxs, len_cnms)
        #data_set_dim <- dim(data_set[idxs, cnms])
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
    # addHandlerChanged(c_names, h_disp)
    # addHandlerChanged(b_selall, h_disp)
    # addHandlerChanged(b_invert, h_disp)
    # addHandlerChanged(b_clear, h_disp)

    ##pick-up changes to row selection
     addHandlerChanged(row_filter, function(h,...) {
        ##update display button
        len_idxs_update()
        h_disp()
     })
    
    ##pick-up changes to col selection
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
       ##update display button
       len_cnms_update()
       h_disp()
     })                              
    
    ##init dummy handler funs to avoid "not found" error
    h_descr <- function() invisible(NULL)
    h_lev <- function() invisible(NULL)
    h_var <- function() invisible(NULL)
    h_summ <- function() invisible(NULL)
    h_deb <- function() invisible(NULL)
    h_details <- function() invisible(NULL)
    h_details.ins <- function() invisible(NULL)
    
    ##handler to execute on click of 'display' button
    hb_disp <- function(h,...) {
        #rows <- svalue(row_filter)
        #cnms <<- svalue(c_names)
        #cnms <<- old_selection
        #idxs <<- which(rows)                  # move to global variable
        
        ##store rows/cnms currently being displayed for use in describe()
        rows.disp <<- rows
        cnms.disp <<- cnms
        
        ##check if loading a huge data frame and warn user
        if(all( (sum(rows) * length(cnms)) >= 500000, confirm.big.df)){
            bd_huge <- gconfirm('You are about to load a large data frame, 
which may take a long time to display (in 
some cases up to several minutes). 
                   
Do you want to proceed?', title="Warning", icon="warning")
            if(!bd_huge) return()
        }
        
        ##gtkSpinner() functionality
        #add(gsb_dfg, gsb_dfsp)
        #gsb_dfsp$start()
        
        ## now add a data frame
        delete(df_box, df_box[1])             # remove child
        
        ## disable editing if so requested
        #data_set_dim <- dim(data_set[idxs, cnms])
        #data_set_dim <- c(len_idxs, len_cnms)
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
            svalue(gsb_dfl) <- paste("Displaying the full data set.", sep='')
        } else {
            svalue(gsb_dfl) <- paste('Displaying a ', data_set_dim[1], ' x ', 
                                   data_set_dim[2], " subset.", sep='')
        }
        #gsb_dfsp$stop()
        #gsb_dfg$widget$remove(gsb_dfsp)
        font(b_disp) <- list(weight = "normal")
        
        ##update details tab
        ##FIXME speed-up: mv this to handler on tab selection
        if(details){
			h_descr()
			h_lev()
			h_var()
			h_summ()
			h_deb()
            if(!details.on.tab.sel){
                h_details()
                h_details.ins()
            }
            new.disp <<- TRUE
        }
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
    
    ##init display button label
    len_idxs_update()
    len_cnms_update()
    #print(data_set_dim)
    h_disp()  ##update display button size given 'preset' filter

    ##activate auto-display of preset filter
    #if(display) hb_disp()
    if(display) b_disp$invoke_change_handler()
    
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
    #print(size(pg))
    #print(size(c_names))
    #print(size(s_gp))
    #print(sapply(f_side0g$children, function(u) size(u)))
    
    
    ############################
    ##Details tab
    if(details){
    dgg <- ggroup(cont=ntbk, horizontal=TRUE, label=" Details")
    ntbk$add_tab_icon(2, "info")
    ntbk$add_tab_tooltip(2, "Display data frame details")
    
    #svalue(ntbk) <- 1
    dntbk <- gnotebook(2, cont=dgg, expand=TRUE, fill=TRUE)


    ##helper fun to list label in a dataframe
    list_lab <- function(data=data_set){
        lab <- label(data, self=TRUE)
        if(lab=="") return(NULL)
        return(lab)
    }
    
    #####
    ##Describe sub-tab
    dlgg <- ggroup(cont=dntbk, horizontal=FALSE, label="Describe", expand=TRUE, 
                   use.scrollwindow = TRUE)
    #tooltip(dlgg) <- "Describe the data set that is currently displayed"
    ##radio buttons
    dlgg1 <- ggroup(cont=dlgg, expand=FALSE)
    details_choices <- c("full"="Full data set", "col"="Column selection", 
                        "sel"="Displayed subset", "row"="Row selection")
    r_descr <- gradio(details_choices, 2, horizontal=TRUE, cont=dlgg1
                      #, label="Describe data set"
                      )
    tooltip(dlgg1) <- "Describe the full data set, a column selection (all rows), the currently displayed subset, a row selection (all columns)"
    ##hide-able label
    ##FIXME will need to update label when reloading df
    dlgg2 <- gexpandgroup("Label:", cont=dlgg, horizontal=FALSE,expand=F, fill=T)
    tooltip(dlgg2) <- "Label stored in `label(data, self=TRUE)`"
    t_lab <- gtext(cont=dlgg2, font.attr=list(family="monospace"), 
                     width=300, height=25*6, 
                     expand=T, fill=T)
    editable(t_lab) <- FALSE
    lab.out <- list_lab()
    lab.out.ins <- if(!is.null(lab.out)) capture.output(cat(lab.out)) else 
        capture.output(lab.out)
    insert(t_lab, lab.out.ins, font.attr=list(family="monospace"))
    if(is.null(lab.out)) visible(dlgg2) <- FALSE
    
    ##handler to update/init describe() output
    h_descr <- function(h,...) {
        radio.sel <<- svalue(r_descr, index=TRUE)
        radio.inst <<- "r_descr"
        r_sync()
    }
    addHandlerChanged(r_descr, h_descr)
    t_descr <- gtext(cont=dlgg, font.attr=list(family="monospace"), 
                     #width=500, height=1000, 
                     expand=TRUE)
    editable(t_descr) <- FALSE

    #####
    ##Summary sub-tab
    dsgg <- ggroup(cont=dntbk, horizontal=FALSE, label="Summary", expand=TRUE, 
                   use.scrollwindow = TRUE)
    #tooltip(dsgg) <- "Describe the data set that is currently displayed"
    ##radio buttons
    dsgg1 <- ggroup(cont=dsgg, expand=FALSE)
    r_summ <- gradio(details_choices, 2, horizontal=TRUE, cont=dsgg1
                      #, label="Describe data set"
                      )
    tooltip(dsgg1) <- "Summarise the full data set, a column selection (all rows), the currently displayed subset, a row selection (all columns)"
    
    ##handler to update/init describe() output
    h_summ <- function(h,...) {
        radio.sel <<- svalue(r_summ, index=TRUE)
        radio.inst <<- "r_summ"
        r_sync()
    }
    addHandlerChanged(r_summ, h_summ)
    t_summ <- gtext(cont=dsgg, font.attr=list(family="monospace"), 
                     #width=500, height=1000, 
                     expand=TRUE)
    editable(t_summ) <- FALSE


    #####
    ##Levels sub-tab
    dlevgg <- ggroup(cont=dntbk, horizontal=FALSE, label="Levels", expand=TRUE, 
                   use.scrollwindow = TRUE)
    dlevgg2 <- ggroup(cont=dlevgg, expand=FALSE)
    r_lev <- gradio(details_choices, 2, horizontal=TRUE, cont=dlevgg2
                      #, label="Describe data set"
    )
    tooltip(dlevgg2) <- "Display levels of factors for the full data set, a column selection (all rows), the currently displayed subset, a row selection (all columns)"
    
    ##helper fun to list levels in a dataframe
    list_levs <- function(data=data_set, vars=NULL){
        if(is.null(vars)) vars <- names(data)
        lev_nms <- vars[sapply(data, class)=="factor"]
        if(length(lev_nms)==0) return(NULL)
        levs <- lapply(lev_nms, function(x) levels(data[ , x]))
        names(levs) <- lev_nms
        return(levs)
    }

    ##handler to update/init describe() output
    h_lev <- function(h,...) {
        radio.sel <<- svalue(r_lev, index=TRUE)
        radio.inst <<- "r_lev"
        r_sync()
    }
    addHandlerChanged(r_lev, h_lev)
    t_lev <- gtext(cont=dlevgg, font.attr=list(family="monospace"), 
                     #width=500, height=1000, 
                     expand=TRUE)
    editable(t_lev) <- FALSE
    
    #####
    ##Variables sub-tab
    dvargg <- ggroup(cont=dntbk, horizontal=FALSE, label="Variables", expand=TRUE, 
                   use.scrollwindow = TRUE)
    dvargg2 <- ggroup(cont=dvargg, expand=FALSE)
    r_var <- gradio(details_choices, 2, horizontal=TRUE, cont=dvargg2
                      #, label="Describe data set"
    )
    tooltip(dvargg2) <- "Display levels of factors for the full data set, a column selection (all rows), the currently displayed subset, a row selection (all columns)"
    
    ##handler to update/init describe() output
    h_var <- function(h,...) {
        radio.sel <<- svalue(r_var, index=TRUE)
        radio.inst <<- "r_var"
        r_sync()
    }
    addHandlerChanged(r_var, h_var)
    t_var <- gtext(cont=dvargg, font.attr=list(family="monospace"), 
                     #width=500, height=1000, 
                     expand=TRUE)
    editable(t_var) <- FALSE
    
    #####
    ##Debugging sub-tab
    ddebgg <- ggroup(cont=dntbk, horizontal=FALSE, label="Debugging", expand=TRUE
                    #, use.scrollwindow = TRUE
                   )
    #ddebgg <- gvbox(cont = dntbk, expand=TRUE)
    #tooltip(dsgg) <- "Describe the data set that is currently displayed"
    ##radio buttons
    ddebgg1 <- ggroup(cont=ddebgg, expand=FALSE)
    r_deb <- gradio(details_choices, 2, horizontal=TRUE, cont=ddebgg1
                      #, label="Describe data set"
                      )
    tooltip(ddebgg1) <- "Display debugging info for the full data set, a column selection (all rows), the currently displayed subset, a row selection (all columns)"
    
    ##handler to update/init describe() output
    h_deb <- function(h,...) {
        radio.sel <<- svalue(r_deb, index=TRUE)
        radio.inst <<- "r_deb"
        r_sync()
    }
    addHandlerChanged(r_deb, h_deb)
    df_deb_box <- ggroup(cont=ddebgg, expand=TRUE) ## holds DF_deb instance
    ## create a place-holder that can later be deleted
    DF_deb <- glabel("", cont=df_deb_box, expand=TRUE)
    

    ##focus Describe sub-tab
    svalue(dntbk) <- 1

    ##handler to keep Details radios in sync
    r_sync <- function(h, ...){
        ##FIXME isn't there an infinite loop here in this sync? 
        if(radio.inst!="r_descr") svalue(r_descr, index=TRUE) <- radio.sel
        if(radio.inst!="r_summ") svalue(r_summ, index=TRUE) <- radio.sel
        if(radio.inst!="r_lev") svalue(r_lev, index=TRUE) <- radio.sel
        if(radio.inst!="r_var") svalue(r_var, index=TRUE) <- radio.sel
        if(radio.inst!="r_deb") svalue(r_deb, index=TRUE) <- radio.sel
    }
    
    f_details <- function(x=data_set, nm=data_set_name, nms=data_set_nms){
        out <- list()
        out[["descr"]] <- describe(x, descript=nm)
        out[["summ"]] <- capture.output(summary(x))
        out[["lev"]] <- capture.output(list_levs(x, NULL))
        out[["var"]] <- capture.output(dput(names(x)))
        out[["deb"]] <- debug_data.frame(x)
        return(out)
    }
    
    ##use one handler to rule them all (since subsetting is the bitch)
    h_details <- function(h, choice=radio.sel, ...) {
        choice <- names(details_choices)[choice]
        #print(choice)
        if(choice=='full'){
            ##avoid re-computing if output already exists
            if(is.null(details.out[[choice]])) details.out[[choice]] <<- f_details()
        } else if(choice=='col'){
            ##compute if output does NOT exist
            if(is.null(details.out[[choice]])){
                details.out[[choice]] <<- f_details(droplevels(data_set[ , cnms.disp]))
            ##avoid re-computing if output already exists & selection same
            } else if(!isTRUE(all.equal(cnms.disp, cnms.disp_old[[choice]]))){
                details.out[[choice]] <<- f_details(droplevels(data_set[ , cnms.disp]))
            }
            ##store selection of displayed details
            cnms.disp_old[[choice]] <<- cnms.disp
        } else if(choice=='sel'){
            if(is.null(details.out[[choice]])){
                details.out[[choice]] <<- f_details(droplevels(DF[]))
            } else if(any(!isTRUE(all.equal(cnms.disp, cnms.disp_old[[choice]])), 
                !isTRUE(all.equal(rows.disp, rows.disp_old[[choice]])))){
                details.out[[choice]] <<- f_details(droplevels(DF[]))
            }
            cnms.disp_old[[choice]] <<- cnms.disp
            rows.disp_old[[choice]] <<- rows.disp
        } else if(choice=='row'){
            if(is.null(details.out[[choice]])){
                details.out[[choice]] <<- f_details(droplevels(data_set[rows.disp, ]))
            } else if(!isTRUE(all.equal(rows.disp, rows.disp_old[[choice]]))){
                details.out[[choice]] <<- f_details(droplevels(data_set[rows.disp, ]))
            }
            rows.disp_old[[choice]] <<- rows.disp
        }
        new.disp <<- FALSE
    }

    h_details.ins <- function(h, choice=radio.sel, ins=details.out,...) {
        choice <- names(details_choices)[choice]
        svalue(t_descr) <- ""
        insert(t_descr, capture.output(ins[[choice]][["descr"]]), 
            font.attr=list(family="monospace"))
        svalue(t_summ) <- ""
        insert(t_summ, ins[[choice]][["summ"]], font.attr=list(family="monospace"))
        svalue(t_lev) <- ""
        insert(t_lev, ins[[choice]][["lev"]], font.attr=list(family="monospace"))
        svalue(t_var) <- ""
        insert(t_var, ins[[choice]][["var"]], font.attr=list(family="monospace"))
        delete(df_deb_box, df_deb_box[1])             # remove child
        DF_deb <- gdf(ins[[choice]][["deb"]], cont=df_deb_box, expand=TRUE, 
                      freeze_attributes=TRUE)
        sapply(1:data_set_dim[2], function(j) editable(DF_deb, j) <- FALSE)
        DF_deb$set_selectmode("multiple")
    }
    
    addHandlerChanged(r_descr, function(h, ...){
        h_details()
        h_details.ins()
    })
    
    ##init Details tab
    radio.sel <- 2
    radio.inst <- ""
    r_sync()
    ##by default update Details only when the tab is selected
    if(!details.on.tab.sel){
        h_details()
        h_details.ins()
    }

    ##focus Filter tab
    svalue(ntbk) <- 1
    if(details.on.tab.sel){
        ##by default update Details only when the tab is selected
        ##and if there is a newly displayed data frame
        addHandlerChanged(ntbk, function(h, ...){
                if(all(h$page.no==2, new.disp)){
                    h_details()
                    h_details.ins()
                }
            })
    }
	}

    ##set GUI window parameters
    ##set sizes
    size(w) <- c(750, 600)
    ##FIXME if mv visible call down, then pg doesn't resize correctly
    visible(w) <- TRUE
    svalue(pg) <- as.integer(size(b_disp)[1] + 20)
    #svalue(pg) <- 0.42
    #svalue(pg) <- 250L
    ## use 5 lines as hight of selection box (less claustrophobic)
    size(c_names)[2] <- 5*25
    
    ##set some key-bindings
    if(esc){
        addHandlerKeystroke(w, function(h, ...){
            if(h$key=="\033") dispose(w)
        })
    }
}

# require(MASS)
# Xa <- Cars93 ## this will be in a function... replace with your won
# Xa[3:7,1] <- NA
# Xa[3:7,"Price"] <- NA
# Xa$Model1 <- as.character(Xa$Model)
# Xa[2,'Model1'] <- paste(rep(letters, 26), collapse='')
# Xa$Man.trans.avail1 <- as.logical(Xa$Man.trans.avail)
# Xa$Man.trans.avail1 <- ifelse(Xa$Man.trans.avail=='Yes', TRUE, FALSE)
# x <- mtcars
# for(i in 1:400) x <- rbind(x, mtcars)
# for(i in 1:5) x <- cbind(x, x)
View <- dffilter

dffilter_reload <- function(...){
    #dffilter(data_set=.data_set, display, maximize, editable)
    dffilter(...)
}

##FIXME more efficient way of doing this? 
debug_data.frame <- function(data, funs.def=c("class"=class, "mode"=mode, 
                             "complete.cases"=function(x) sum(complete.cases(x)), 
                             "is.na"=function(x) sum(is.na(x)), 
                             "is.nan"=function(x) sum(is.nan(x)),
                             "is.finite"=function(x) sum(is.finite(x)),
                             "is.infinite"=function(x) sum(is.infinite(x)),
                             "length(unique(nchar(x)))"=function(x) 
                                 length(unique(nchar(as.character(x)))),
                             "unique(nchar(x))"=function(x) 
                                 paste(sort(unique(nchar(as.character(x)))), 
                                       collapse=" "), 
                             "is_alnum"=function(x) sum(is_alnum(x)),
                             "is_alpha"=function(x) sum(is_alpha(x)),
                             "is_digit"=function(x) sum(is_digit(x)),
                             "is_punct"=function(x) sum(is_punct(x)),
                             "is_notalnum"=function(x) sum(is_notalnum(x))
                             ), 
                             funs.add=NULL){
    funs <- c(funs.def, funs.add)
    #out <- data[ FALSE , ]
    out <- as.data.frame(lapply(data[ FALSE , ], as.character), stringsAsFactors=FALSE)
    for(i in 1:length(funs)){
        #if(names(funs[i])=="unique(nchar())") break.point()
        ##FIXME put checks on what fun outputs
        out[i, ] <- sapply(data, funs[[i]])
        row.names(out)[i] <- names(funs[i])
    }
    return(out)
}
#debug_data.frame(iris)

##FIXME add is_notalpha, is_notdigit, etc.?
is_alnum <- function(x) {grepl("[[:alnum:]]", x)}  ##Alphanumeric characters
is_alpha <- function(x) {grepl("[[:alpha:]]", x)}  ##Alphabetic characters
is_digit <- function(x) {grepl("[[:digit:]]", x)}  ##Digits
is_punct <- function(x) {grepl("[[:punct:]]", x)}  ##Punctuation characters
is_notalnum <- function(x) {grepl("[^[:alnum:]]", x)}  ##Non-Alphanumeric characters
