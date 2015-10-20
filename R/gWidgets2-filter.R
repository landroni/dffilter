## edit a really large data set *after* it has been filtered

dffilter <- function(data_set, display=TRUE, maximize=TRUE, editable=FALSE, 
                     data_set_name=NULL, sel.col=NULL, sel.row=NULL, esc=FALSE, 
                     def.col=100, details=TRUE, details.on.tab.sel=TRUE, 
                     confirm.big.df=TRUE, 
                     initial.vars=data.frame(data_set_nms[1], "preset", "preset", 
                        stringsAsFactors=FALSE), filter.on.tab.sel=TRUE, 
                     hide=FALSE
                     , crosstab=TRUE, crosstab.on.tab.sel=TRUE
                     ){
    require(gWidgets2) ## on github not CRAN. (require(devtools); install_github("gWidgets2", "jverzani")
    options(guiToolkit="RGtk2")
    require(RGtk2)
    ##FIXME !!proper CRAN packaging
    ##FIXME !!put package require() in appropriate handlers
    if(details) require(Hmisc)
    

    DF <- NULL                      # global gdf instance
    rows <- NULL                    # global rows index
    rows.disp <- NULL               # global rows index (subset currently displayed)
    rows.descr_old <- list()               # global rows index (subset previously displayed)
    idxs <- NULL                    # global set of indices that are being edited
    cnms <- NULL                    # global column names
    cnms.disp <- NULL               # global column names (subset currently displayed)
    cnms.descr_old <- list()               # global column names (subset currently displayed)
    len_idxs <- NULL                # global set of indices that are being edited (length)
    len_cnms <- NULL                # global column names (length)
    data_set_dim <- NULL            # global df dim
    c_names <- NULL                 # global column names widget
    old_selection <- NULL           # global old selection storage
    radio.inst <- NULL
    radio.sel <- NULL
    DF_deb <- NULL
    details.out <- list()
    new.disp <- TRUE
    new.descr <- TRUE
    new.ctab <- TRUE
    rows.df.deb <- NULL               # global rows index (subset currently displayed)
    filter.types <- c("single"="RadioItem", "multiple"="ChoiceItem", 
        "range"="RangeItem", "preset"="PresetItem")
    h_disp_lab <- NULL
    hidden.panel <- hide
    b_disp_font <- list(weight = "normal")
    tb_ctab <- NULL
    ctab.dropped <- c()
    ctab.sel <- list()
    DF_ctab <- NULL
    has_b_melt_var <- FALSE
    b_melt_var.ctab <- NULL
    ctab.vars.init <- list()
    rows.ctab_old <- list()               # global rows index (subset previously displayed)
    cnms.ctab_old <- list()               # global column names (subset currently displayed)
    tb_ctab.tmp.sel <- NULL
    new.descr.sync <- TRUE
    new.ctab.sync <- TRUE
    l_lyt_ctab <- list()
    f_lyt_ctab <- list()
    g_variable2...fixed <- NULL     ## global instance of special 'variable' obj in ctab
    
    ##if there is no Details tab, we always want to display subset
    if(!details) filter.on.tab.sel <- FALSE
    
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
                     ##attempt to free memory
                     gc(TRUE)
                     #if(identical)
                     #return(data_set)
                 })
    
    ##set WM icon (gtk-only)
    w_img <- gdkPixbufNewFromFile("gtk-logo-rgb.gif")
    getToolkitWidget(w)$setIcon(w_img$retval)
                 
    ##maximize window on load
    if(maximize) getToolkitWidget(w)$maximize()
    
    #pg <- gpanedgroup(cont=ntbk, horizontal=TRUE, label=" Filter")
    pg <- gpanedgroup(cont=w, horizontal=TRUE)
    #pg <- ggroup(cont=w, horizontal=TRUE)
    f_side0 <- gvbox(cont=pg, use.scrollwindow=FALSE, 
                     resize=FALSE, shrink=FALSE)  ##1st side of paned grp
    
    ## have a hide/show button
    f_side0g <- ggroup(cont=f_side0)
    f_side0g1 <- ggroup(cont=f_side0g)
    b_hide <- gbutton("Hide", cont=f_side0g1)
    h_hide <- function(h, ...){
        val <- svalue(h$obj)
        if(val == "Hide") {
            delete(f_side0, f_side1)
            add(f_side0g, f_side0g2)
            delete(f_side0g, f_side0g1)
            svalue(pg) <- as.integer(size(b_show)[1])
            hidden.panel <<- TRUE
        } else {
            add(f_side0, f_side1, expand=T)
            #print(sapply(f_side0$children, function(u) size(u)))
            add(f_side0g, f_side0g1)
            delete(f_side0g, f_side0g2)
            ##FIXME this slowly enlargens panel size after multiple clicks
            svalue(pg) <- as.integer(size(c_names)[1] + 9)
            hidden.panel <<- FALSE
        }
        #blockHandlers(h$obj)
        #unblockHandler(h$obj)
    }
    addHandlerClicked(b_hide, h_hide)
    b_hide$set_icon("go-back")
    tooltip(b_hide) <- "Hide panel"

    ## have a reload button
    ##FIXME !!restore the sel of row filters
    #addSpring(f_side0g)
    b_reload <- gbutton("Reload", cont=f_side0g1)
    h_reload <- function(h, ...) {
        #print(data_set_name)
        #print(class(row_filter))
        #for(i in sel.row) print(class(i)[1])
        #for(i in 1:length(sel.row)) print(svalue(sel.row[[i]]))
        
        dispose(w)
        dffilter_reload(data_set=get(data_set_name), display=display, maximize=maximize, 
                        editable=editable, data_set_name=data_set_name, 
                        sel.col=old_selection, sel.row=row_filter, 
                        hide=hidden.panel)
    }
    addHandlerClicked(b_reload, h_reload)
    b_reload$set_icon("refresh")
    tooltip(b_reload) <- "Reload data frame"
    
    ##vertically aligned buttons for when panel is hidden
    f_side0g2 <- ggroup(horizontal = FALSE, cont=f_side0g)
    b_show <- gbutton("", cont=f_side0g2)
    addHandlerClicked(b_show, h_hide)
    b_show$set_icon("go-forward")
    tooltip(b_show) <- "Show panel"
    #add(f_side0g2, b_reload)
    b_reload2 <- gbutton("", cont=f_side0g2)
    addHandlerClicked(b_reload2, h_reload)
    b_reload2$set_icon("refresh")
    tooltip(b_reload2) <- "Reload data frame"
    delete(f_side0g, f_side0g2)  ##init the vertical buttons


    f_side1 <- gvbox(cont=f_side0, use.scrollwindow=TRUE, expand=TRUE)

    ##FIXME mv this to appropriate location in code
    ############################
    ##Filter *tab*
    #df_side <- gvbox(cont=pg, expand=TRUE)
    ntbk <- gnotebook(3, cont=pg)  ##2nd side of paned grp
    df_side <- gvbox(cont=ntbk, expand=TRUE, label=" Filter")
    ntbk$add_tab_icon(1, "find")
    ntbk$add_tab_tooltip(1, "Display data frame")
    
    df_box <- ggroup(cont=df_side, expand=TRUE) ## holds DF instance
    glabel("Select columns to be displayed \nand define appropriate row filters,\nthen click the 'Display selection' button. \nIf you make changes to your data, you \ncan merge them into the original dataset.", cont=df_box)
    
    btn_gp <- ggroup(cont=df_side)
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
       ed <- gedit("", initial.msg="Filter column names by...", expand=TRUE, container=gp)
       ed$set_icon("ed-search", "start")
       ed$set_icon("ed-remove", "end")
       ed$set_icon_handler(function(h,...) {
         svalue(ed) <- ""
         focus(ed) <- TRUE
       }, where="end")
       ed$widget$setIconActivatable("primary", FALSE)
       
       search_handler <- function(h,..., do_old=TRUE){
         ## we keep track of old selection here
         ## that updates only when user changes selection, not when filter does
         cur_sel <- old_selection
         blockHandlers(c_names)
         on.exit(unblockHandlers(c_names))
         val <- svalue(ed)
         #print(val)

         if(val == ""){
           c_names[] <<- data_set_nms
           ed$widget$modifyBase(GtkStateType["normal"], NULL)
           ed$widget$modifyText(GtkStateType["normal"], NULL) 
         } else {
           l <- c(list(pattern=val, x=data_set_nms), search_type)
           new_vals <- data_set_nms[do.call(grepl, l)]
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

       b <- gbutton("", cont=gp)
       tooltip(b) <- "Search options"
       b$set_icon("properties")

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
    
    
    ###############################
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
        #print(initial.vars)
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
        ##FIXME sometimes this catch is buggy
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
    h_disp <- function(h, ...){
        #rows <<- svalue(row_filter)
        #idxs <<- which(rows)                  # move to global variable
        #len_idxs <<- length(idxs)                  # move to global variable
        #cnms <<- svalue(c_names)
        #cnms <<- old_selection
        #len_cnms <<- length(cnms)                  # move to global variable
        
        ## detect size of data frame to be displayed
        if(any(data_set_dim < c(1,2))){
            enabled(b_disp) <- FALSE
        } else enabled(b_disp) <- TRUE
        
        blockHandlers(b_disp)
        ## dynamically update 'display' button label given current selection
        h_disp_lab <<- paste(' selection (', data_set_dim[1], 
                                          ' x ', data_set_dim[2], ')', sep='')
        svalue(b_disp, append=T) <- paste(if(svalue(ntbk)==1) 'Display' else 
            if(svalue(ntbk)==2) 'Describe' else 
                if(svalue(ntbk)==3) 'Define', h_disp_lab, sep='')
        b_disp$set_icon("execute")
        b_disp_font <<- list(weight = "bold")
        font(b_disp) <- b_disp_font
        unblockHandlers(b_disp)
        
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
        
        ##FIXME need to make this message tab specific
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
        
        ##by default Display data frame only when the tab is selected
        if(any(!filter.on.tab.sel, svalue(ntbk)==1)){
            h_filter()
            ##signal new display event via button
            #new.disp <<- TRUE
            new.descr <<- TRUE
            new.ctab <<- TRUE
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
        b_disp_font <<- list(weight = "normal")
        font(b_disp) <- b_disp_font
        
        ##update details tab
        ##FIXME ??speed-up: mv this to handler on tab selection
        if(details){
            #print(svalue(ntbk))
            if(any(!details.on.tab.sel, svalue(ntbk)==2)){
                h_details()
                h_details.ins()
                ##signal new describe event via button
                #new.descr <<- TRUE
                new.disp <<- TRUE
                new.ctab <<- TRUE
            }
        }
        
        if(crosstab){
            if(any(!crosstab.on.tab.sel, svalue(ntbk)==3)){
                h_ctab_vars()
                h_ctab_vars.ins(drop.vars=TRUE)
                #h_ctab_clear()

                ##signal new ctab event via button
                #new.ctab <<- TRUE
                new.disp <<- TRUE
                new.descr <<- TRUE
            }
        }

        #print("end-of-button handler")
        #print(paste("new.disp:", new.disp))
        #print(paste("new.descr:", new.descr))
        #print(paste("new.ctab:", new.ctab))
    }
    
    h_filter <- function(h, ...){
        ## now add a data frame
        delete(df_box, df_box[1])             # remove child
        
        ## disable editing if so requested
        if(!editable){
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
        
        ##FIXME move this outside handler? 
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
        
        ##signal that no new display is necessary
        #new.descr <<- FALSE
        #new.ctab <<- FALSE
        new.disp <<- FALSE
        #print("display event")
        #print(paste("new.disp:", new.disp))
        #print(paste("new.descr:", new.descr))
        #print(paste("new.ctab:", new.ctab))
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
    
    ##FIXME full editing support for some other time
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
    ##FIXME !!details=F is completely broken (as it will affect crosstab too)
    if(details){
    dgg <- ggroup(cont=ntbk, horizontal=TRUE, label=" Details")
    ntbk$add_tab_icon(2, "info")
    ntbk$add_tab_tooltip(2, "Describe data frame")
    
    #svalue(ntbk) <- 1
    dntbk <- gnotebook(2, cont=dgg, expand=TRUE, fill=TRUE)


    #####
    ##Describe sub-tab
    ##FIXME !!add numSummary tab (RcmdrMisc)
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
    r_summ <- gradio(details_choices, 2, horizontal=TRUE, cont=dsgg1)
    tooltip(dsgg1) <- "Summarise the full data set, a column selection (all rows), the currently displayed subset, a row selection (all columns)"
    
    ##handler to update/init summary() output
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
    ##Labels sub-tab
    dlabgg <- ggroup(cont=dntbk, horizontal=FALSE, label="Labels", expand=TRUE, 
                   use.scrollwindow = TRUE)
    dlabgg2 <- ggroup(cont=dlabgg, expand=FALSE)
    r_lab <- gradio(details_choices, 2, horizontal=TRUE, cont=dlabgg2)
    tooltip(dlabgg2) <- "Display labels for the full data set, a column selection (all rows), the currently displayed subset, a row selection (all columns)"
    
    ##hideable label
    dlabgg3 <- gexpandgroup("Data frame:", cont=dlabgg, horizontal=FALSE, 
        expand=F, fill=T)
    tooltip(dlabgg3) <- "Label stored in `label(data, self=TRUE)`"
    t_lab.df <- gtext(cont=dlabgg3, font.attr=list(family="monospace"), 
                     width=300, height=25*6, 
                     expand=T, fill=T)
    editable(t_lab.df) <- FALSE
    
    ##helper fun to list label in a dataframe
    list_lab <- function(data=data_set){
        lab <- label(data, self=TRUE)
        if(lab=="") return(NULL)
        cat(lab)
    }

    #lab.out <- list_lab()
    #lab.out.ins <- if(!is.null(lab.out)) capture.output(cat(lab.out)) else 
    #    capture.output(lab.out)
    #lab.out.ins <- capture.output(list_lab())
    #insert(t_lab.df, lab.out.ins, font.attr=list(family="monospace"))
    #if(is.null(lab.out.ins)) visible(dlabgg3) <- FALSE
    #if(all(lab.out.ins=="NULL", 
    #    label(data_set, self=TRUE)!="NULL")) visible(dlabgg3) <- FALSE

    ##handler to update/init label() output
    h_lab <- function(h,...){
        radio.sel <<- svalue(r_lab, index=TRUE)
        radio.inst <<- "h_lab"
        r_sync()
    }
    addHandlerChanged(r_lab, h_lab)

    dlabgg4 <- gexpandgroup("Variables:", cont=dlabgg, horizontal=FALSE, 
        expand=T, fill=T)
    tooltip(dlabgg4) <- "Labels stored in `label(data)`"
    t_lab.var <- gtext(cont=dlabgg4, font.attr=list(family="monospace"), 
                     #width=500, height=1000, 
                     expand=TRUE)
    editable(t_lab.var) <- FALSE
    

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
    tooltip(dvargg2) <- "Display variable names for the full data set, a column selection (all rows), the currently displayed subset, a row selection (all columns)"
    
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
    
    cb_deb <- gcheckbox("Extended debugging details", checked=FALSE, cont=ddebgg)
    tooltip(cb_deb) <- "Check to see additional debugging information"

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
    
    ##hide extended debugging details when requested
    h_cb_deb <- function(h, ...){
        #print(svalue(cb_deb))
        #print(rows.df.deb)
        #if(svalue(cb_deb)) print(rep(TRUE, rows.df.deb)) else
        #    print(1:rows.df.deb %in% 1:8)
        if(svalue(cb_deb)) visible(DF_deb) <<- rep(TRUE, rows.df.deb) else
            ##FIXME need to generalize this
            visible(DF_deb) <<- 1:rows.df.deb %in% 1:8
    }
    addHandlerChanged(cb_deb, h_cb_deb)


    ##focus Describe sub-tab
    svalue(dntbk) <- 1

    ##handler to keep Details radios in sync
    r_sync <- function(h, ...){
        ##details
        if(radio.inst!="r_descr") svalue(r_descr, index=TRUE) <- radio.sel
        if(radio.inst!="r_summ") svalue(r_summ, index=TRUE) <- radio.sel
        if(radio.inst!="r_lab") svalue(r_lab, index=TRUE) <- radio.sel
        if(radio.inst!="r_lev") svalue(r_lev, index=TRUE) <- radio.sel
        if(radio.inst!="r_var") svalue(r_var, index=TRUE) <- radio.sel
        if(radio.inst!="r_deb") svalue(r_deb, index=TRUE) <- radio.sel
        ##crosstab
        if(radio.inst!="r_ctab") svalue(r_ctab, index=TRUE) <- radio.sel
    }
    
    f_details <- function(x=data_set, nm=data_set_name, nms=data_set_nms){
        out <- list()
        out[["descr"]] <- describe(x, descript=nm)
        out[["summ"]] <- capture.output(summary(x))
        out[["lab.df"]] <- capture.output(list_lab()) ##use only data_set
        ##FIXME need to check if this works after subset
        out[["lab.var"]] <- capture.output(label(x))
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
            } else if(!isTRUE(all.equal(cnms.disp, cnms.descr_old[[choice]]))){
                details.out[[choice]] <<- f_details(droplevels(data_set[ , cnms.disp]))
            }
            ##store selection of displayed details
            cnms.descr_old[[choice]] <<- cnms.disp
        } else if(choice=='sel'){
            if(is.null(details.out[[choice]])){
                ##FIXME if possible use DF[] conditionally
                #details.out[[choice]] <<- f_details(droplevels(DF[]))
                details.out[[choice]] <<- f_details(droplevels(data_set[rows.disp, cnms.disp]))
            } else if(any(!isTRUE(all.equal(cnms.disp, cnms.descr_old[[choice]])), 
                !isTRUE(all.equal(rows.disp, rows.descr_old[[choice]])))){
                #details.out[[choice]] <<- f_details(droplevels(DF[]))
                details.out[[choice]] <<- f_details(droplevels(data_set[rows.disp, cnms.disp]))
            }
            cnms.descr_old[[choice]] <<- cnms.disp
            rows.descr_old[[choice]] <<- rows.disp
        } else if(choice=='row'){
            if(is.null(details.out[[choice]])){
                details.out[[choice]] <<- f_details(droplevels(data_set[rows.disp, ]))
            } else if(!isTRUE(all.equal(rows.disp, rows.descr_old[[choice]]))){
                details.out[[choice]] <<- f_details(droplevels(data_set[rows.disp, ]))
            }
            rows.descr_old[[choice]] <<- rows.disp
        }
        
        ##signal that no new describe is necessary
        #new.disp <<- FALSE
        #new.ctab <<- FALSE
        new.descr <<- FALSE
        #print("describe event")
        #print(paste("new.disp:", new.disp))
        #print(paste("new.descr:", new.descr))
        #print(paste("new.ctab:", new.ctab))

        ##signal that no new describe after sync is necessary
        new.descr.sync <<- FALSE
        #print(paste("new.descr.sync:", new.descr.sync))
        #print(paste("new.ctab.sync:", new.ctab.sync))
    }

    h_details.ins <- function(h, choice=radio.sel, ins=details.out, ...){
        choice <- names(details_choices)[choice]
        svalue(t_descr) <- ""
        insert(t_descr, capture.output(ins[[choice]][["descr"]]), 
            font.attr=list(family="monospace"))
        svalue(t_summ) <- ""
        insert(t_summ, ins[[choice]][["summ"]], font.attr=list(family="monospace"))
        ##FIXME speed-up: check if you can do this only when reloading df
        svalue(t_lab.df) <- ""
        insert(t_lab.df, ins[[choice]][["lab.df"]], font.attr=list(family="monospace"))
        #if(all(ins[[choice]][["lab.df"]]=="NULL", 
        #    label(data_set, self=TRUE)!="NULL")) visible(dlabgg3) <- FALSE
        svalue(t_lab.var) <- ""
        insert(t_lab.var, ins[[choice]][["lab.var"]], font.attr=list(family="monospace"))
        svalue(t_lev) <- ""
        insert(t_lev, ins[[choice]][["lev"]], font.attr=list(family="monospace"))
        svalue(t_var) <- ""
        insert(t_var, ins[[choice]][["var"]], font.attr=list(family="monospace"))
        delete(df_deb_box, df_deb_box[1])             # remove child
        DF_deb <<- gdf(ins[[choice]][["deb"]], cont=df_deb_box, expand=TRUE, 
                      freeze_attributes=TRUE)
        if(is.null(rows.df.deb)) rows.df.deb <<- nrow(DF_deb)
        h_cb_deb()
        sapply(1:data_set_dim[2], function(j) editable(DF_deb, j) <- FALSE)
        DF_deb$set_selectmode("multiple")
    }
    
    ##handle change of radio choice in Details tab
    addHandlerChanged(r_descr, function(h, ...){
        if(details.on.tab.sel){
        ##by default update Details only when the tab is selected
            if(svalue(ntbk)==2){
                h_details()
                h_details.ins()
                new.ctab.sync <<- TRUE
                #print("describe sync event")
                #print(paste("new.descr.sync:", new.descr.sync))
                #print(paste("new.ctab.sync:", new.ctab.sync))
            }
        }
    })
    
    
    ############################
    ##Pivot table tab (cross tabulation)
    if(crosstab){
        require(reshape2)
        require(formula.tools)
    cgg <- ggroup(cont=ntbk, horizontal=TRUE, label=" Pivot Table")
    ntbk$add_tab_icon(3, "jump-to")
    ntbk$add_tab_tooltip(3, "Explore data frame using pivot tables (cross tabulations)")
    
    cntbk <- gnotebook(2, cont=cgg, expand=TRUE, fill=TRUE)


    #####
    ##Reshape sub-tab
    clgg <- ggroup(cont=cntbk, horizontal=FALSE, label="Reshape", expand=TRUE, 
                   use.scrollwindow = TRUE)
    #tooltip(clgg) <- "Describe the data set that is currently displayed"
    ##radio buttons
    clgg1 <- ggroup(cont=clgg, expand=FALSE)
    r_ctab <- gradio(details_choices, 2, horizontal=TRUE, cont=clgg1)
    tooltip(clgg1) <- "Generate pivot tables from the full data set, a column selection (all rows), the currently displayed subset, a row selection (all columns)"
    
    ##FIXME !!on radio sync, gracefully redo ctab
    h_rshp <- function(h,...){
        radio.sel <<- svalue(r_ctab, index=TRUE)
        radio.inst <<- "r_ctab"
        r_sync()
    }
    addHandlerChanged(r_ctab, h_rshp)

    clgg2 <- ggroup(cont=clgg)
    cb_ctab <- gcheckbox("Pivot table layout", checked=TRUE, cont=clgg2)
    tooltip(cb_ctab) <- "Uncheck to hide the pivot table layout editor"
    
    h_ctab_hide <- function(h, ...){
        hide_layout.ctab <- !(svalue(cb_ctab))
        if(hide_layout.ctab){
            delete(gg_tb_ctab0, gg_tb_ctab1)
            delete(gg_ctab1, gg_ctab2)
            svalue(pg_ctab) <- 0
            enabled(b_ctab_clear) <- FALSE
        } else {
            add(gg_tb_ctab0, gg_tb_ctab1, expand=T)
            add(gg_ctab1, gg_ctab2)
            svalue(pg_ctab) <- as.integer(size(tb_ctab)[1] + 0)
            enabled(b_ctab_clear) <- TRUE
        }
    }
    addHandlerChanged(cb_ctab, h_ctab_hide)
    
    h_ctab_clear <- function(h, all.fields=TRUE, field.nr=NULL, ...){
        #print(ctab.sel)
        #print(all.fields)
        #print(field.nr)

        ##reset all fields or one field in particular
        idx <- if(all.fields) 1:3 else field.nr
        stopifnot(!is.null(idx))
        #print(idx)
        lapply(idx, function(x){
              ##REQ !!how to speedy delete all children of container
              lapply(lyt_ctab[1,x]$children, function(y) delete(lyt_ctab[1,x], y))
        })
        
        ##reinit global instances
        if(all.fields){
            ##reset all fields
            ctab.dropped <<- c()
            ctab.sel <<- list()
            ##reinit search box
            svalue(ed_search_ctab) <- ""
            ##update gtable variables
            h_ctab_vars.ins()
        } else {
            ##reset one field in particular
            stopifnot(!is.null(field.nr), length(field.nr)==1)
            x <- field.nr
            #print(x)
            ctab.sel_tmp <- ctab.sel[[as.character(x)]]
            ctab.dropped <<- ctab.dropped[!(ctab.dropped %in% ctab.sel_tmp)]
            ctab.sel[[as.character(x)]] <<- character(0)
            #ctab.sel[[as.character(x)]] <<- sapply(lyt_ctab[1,x]$children, 
            #       function(y) svalue(y$children[[1]]))
            #tb_ctab[] <- old_selection[!(old_selection %in% ctab.dropped)]
            ##update gtable variables
            ##active search
            h_ctab_vars.ins(drop.vars=TRUE, tmp.sel=tb_ctab.tmp.sel)
        }

        if(length(ctab.sel[["3"]])!=0){
            h_ctab_reshape()
        } else {
            ##rm displayed ctab
            delete(g_df_ctab_box, g_df_ctab_box[1])             # remove child
            DF_ctab <<- glabel("", cont=g_df_ctab_box, expand=TRUE)
        }
        
        ##take care of Values field
        if(any(all.fields, field.nr == 3)){
            b_melt_var.ctab <<- NULL
            has_b_melt_var <<- FALSE
            ##delete special 'variable` button
            ##FIXME if(field.nr == 3), do NOT rm  g_variable2...fixed
            ##FIXME some related (R:21444): Gtk-CRITICAL **: 
            ##'IA__gtk_container_remove: assertion 'GTK_IS_TOOLBAR (container) || widget->parent == GTK_WIDGET (container)' failed
            #g_dnd_name <- paste("g_", "variable", 2, "...fixed", sep="")
            #try(delete(lyt_ctab[1,2], get(g_dnd_name)), silent=F)
            try(delete(lyt_ctab[1,2], g_variable2...fixed), silent=T)
            g_variable2...fixed <<- NULL
        }
    }
    b_ctab_clear <- gbutton("Clear", cont=clgg2, handler=function(h, ...){ 
                            h_ctab_clear(all.fields=TRUE)
                            })
    tooltip(b_ctab_clear) <- "Clear pivot table and its layout"
    
    pg_ctab <- gpanedgroup(cont=clgg, expand=T, fill=T)
    #svalue(pg_ctab) <- 0.20

    ##FIXME on sync need to check if col sel is incompatible with already added vars
    gg_tb_ctab0 <- gvbox(cont=pg_ctab, expand=T)
    gg_tb_ctab1 <- ggroup(cont=gg_tb_ctab0, expand=T)
    gg_tb_ctab1bis <- gvbox(cont=gg_tb_ctab1, expand=T)

    ##fancy search for selecting ctab variables
    ##prepare the search input box & handler
     vb_search_ctab <- gvbox(container=gg_tb_ctab1bis)
     search_type_ctab <-  list(ignore.case=TRUE, perl=FALSE, fixed=FALSE)  ##init global instance
     gp_search_ctab <- ggroup(cont=vb_search_ctab)
       
       ed_search_ctab <- gedit("", initial.msg="Filter variables by...", expand=TRUE, 
                    container=gp_search_ctab)
       ed_search_ctab$set_icon("ed-search", "start")
       ed_search_ctab$set_icon("ed-remove", "end")
       ed_search_ctab$set_icon_handler(function(h,...) {
         svalue(ed_search_ctab) <- ""
         focus(ed_search_ctab) <- TRUE
       }, where="end")
       ed_search_ctab$widget$setIconActivatable("primary", FALSE)
       
       search_handler_ctab <- function(h,..., do_old=TRUE, 
            choice=radio.sel, ins=ctab.vars.init, dropped.vars=ctab.dropped){
         choice <- names(details_choices)[choice]
         ## we keep track of old selection here
         ## that updates only when user changes selection, not when filter does
         #cur_sel <- old_selection_search_ctab
         blockHandlers(tb_ctab)
         on.exit(unblockHandlers(tb_ctab))
         val <- svalue(ed_search_ctab)

         if(val == "") {
           ##revert to default `gtable` behavior
           tb_ctab.tmp.sel <<- NULL
           h_ctab_vars.ins(drop.vars=TRUE)
           ed_search_ctab$widget$modifyBase(GtkStateType["normal"], NULL)
           ed_search_ctab$widget$modifyText(GtkStateType["normal"], NULL) 
         } else {
           l <- c(list(pattern=val, x=cnms.disp), search_type_ctab)
           #avail_vals <- h_ctab_vars.ins(drop.vars=TRUE, ret=TRUE)
           ##initially do for all vars in gtable..
           avail_vals <- h_ctab_vars.ins(ret=TRUE)
           new_vals <- avail_vals[do.call(grepl, l)]
           #new_vals <- new_vals[!is.na(new_vals)]
           #print(avail_vals)
           #print(new_vals)
           if (length(new_vals)) {
             tb_ctab.tmp.sel <<- new_vals
             h_ctab_vars.ins(drop.vars=TRUE, tmp.sel=tb_ctab.tmp.sel)
             #tb_ctab[] <<- new_vals
             #tb_ctab[] <<- na.omit(new_vals)
             ed_search_ctab$widget$modifyBase(GtkStateType["normal"], NULL)
             ed_search_ctab$widget$modifyText(GtkStateType["normal"], NULL) 
           } else {
             tb_ctab.tmp.sel <<- character(0)
             h_ctab_vars.ins(null.sel=TRUE)
             ed_search_ctab$widget$modifyBase(GtkStateType["normal"], "#FF6666")
             ed_search_ctab$widget$modifyText(GtkStateType["normal"], "white") 
             return()
           }
         }
         #svalue(tb_ctab) <<- cur_sel
       }

       b_search_ctab <- gbutton("", cont=gp_search_ctab)
       tooltip(b_search_ctab) <- "Search options"
       b_search_ctab$set_icon("properties")
       cbs_search_ctab <- list(gcheckbox("Ignore case", checked=TRUE, handler=function(h,...) {
                             search_type_ctab[["ignore.case"]] <<- svalue(h$obj)
                             search_handler_ctab(do_old=FALSE)
                             }),
                   gcheckbox("Regex", checked=TRUE, handler=function(h,...) {
                     search_type_ctab[["fixed"]] <<- !svalue(h$obj)
                     search_handler_ctab(do_old=FALSE)                                                     
                   }),
                   gcheckbox("Perl compatible", checked=FALSE, handler=function(h,...) {
                     search_type_ctab[["perl"]] <<- svalue(h$obj)
                     search_handler_ctab(do_old=FALSE)                                                     
                   })
                   )
       
       addPopupMenu(b_search_ctab, gmenu(cbs_search_ctab, popup=TRUE))

       addHandlerKeystroke(ed_search_ctab, search_handler_ctab)
       addHandlerChanged(ed_search_ctab, search_handler_ctab)


    ##REQ programmatically resize gtable/gdf?
    ##REQ disable c-menu rename column
    ##REQ label variables by factor/char & numeric
    tb_ctab <- gtable(cnms.disp, cont=gg_tb_ctab1bis)
    names(tb_ctab) <- "Variables"
    ##REQ tb_ctab$set_selectmode("multiple")
    #tb_ctab$set_selectmode("multiple")
    #size(tbl) <- c(100, 300)

    ##continue fancy search functionality
    ##initialize old_selection which will be the output value of tb_ctab
    ##!!may rm this as not  used
    #old_selection_search_ctab <- svalue(tb_ctab)


    gg_ctab0 <- gvbox(cont=pg_ctab)
    gg_ctab1 <- ggroup(cont=gg_ctab0)
    gg_ctab2 <- ggroup(cont=gg_ctab1)

    ##FIXME !!implement multiple DnD
    addDropSource(tb_ctab, handler=function(h,...){
        svalue(tb_ctab)
    })
    
    ##FIXME rename handler if don't save copy of subset, and rm all unnecessary checks??
    h_ctab_vars <- function(h, choice=radio.sel, ...) {
        choice <- names(details_choices)[choice]
        #print(choice)
        
        ##FIXME !!on 'define sel' button, when sel is different..
        ##..check what vars are already in fields and drop them with a msg:
        #tb_ctab[] <- old_selection[!(old_selection %in% ctab.dropped)]
        ##FIXME can this check be put below? 
        if(!isTRUE(all.equal(cnms.disp, cnms.ctab_old[[choice]]))){
            h_ctab_clear(all.fields=TRUE)
        } else {
            ##FIXME check if this can be avoided
            svalue(ed_search_ctab) <- ""
        }
        
        if(choice=='full'){
            ##avoid re-computing if output already exists
            if(is.null(ctab.vars.init[[choice]])) ctab.vars.init[[choice]] <<- data_set_nms
        } else if(choice=='col'){
            ##compute if output does NOT exist
            if(is.null(ctab.vars.init[[choice]])){
                ctab.vars.init[[choice]] <<- cnms.disp
                #ctab.vars.init[[choice]] <<- f_details(droplevels(data_set[ , cnms.disp]))
            ##avoid re-computing if output already exists & selection same
            } else if(!isTRUE(all.equal(cnms.disp, cnms.ctab_old[[choice]]))){
                ctab.vars.init[[choice]] <<- cnms.disp
                #ctab.vars.init[[choice]] <<- f_details(droplevels(data_set[ , cnms.disp]))
            }
            ##store selection of displayed details
            cnms.ctab_old[[choice]] <<- cnms.disp
        } else if(choice=='sel'){
            if(is.null(ctab.vars.init[[choice]])){
                ##FIXME if possible use DF[] conditionally
                ctab.vars.init[[choice]] <<- cnms.disp
                #ctab.vars.init[[choice]] <<- f_details(droplevels(DF[]))
                #ctab.vars.init[[choice]] <<- f_details(droplevels(data_set[rows.disp, cnms.disp]))
            } else if(any(!isTRUE(all.equal(cnms.disp, cnms.ctab_old[[choice]])), 
                !isTRUE(all.equal(rows.disp, rows.ctab_old[[choice]])))){
                ctab.vars.init[[choice]] <<- cnms.disp
                #ctab.vars.init[[choice]] <<- f_details(droplevels(DF[]))
                #ctab.vars.init[[choice]] <<- f_details(droplevels(data_set[rows.disp, cnms.disp]))
            }
            cnms.ctab_old[[choice]] <<- cnms.disp
            rows.ctab_old[[choice]] <<- rows.disp
        } else if(choice=='row'){
            if(is.null(ctab.vars.init[[choice]])){
                ctab.vars.init[[choice]] <<- data_set_nms
                #ctab.vars.init[[choice]] <<- f_details(droplevels(data_set[rows.disp, ]))
            } else if(!isTRUE(all.equal(rows.disp, rows.ctab_old[[choice]]))){
                ctab.vars.init[[choice]] <<- data_set_nms
                #ctab.vars.init[[choice]] <<- f_details(droplevels(data_set[rows.disp, ]))
            }
            rows.ctab_old[[choice]] <<- rows.disp
        }
        
        {
            lyt_val <- 3
            lyt_has_child <- try(is.null(lyt_ctab[1,lyt_val]$children[[1]]), silent=T)
            if(!(class(lyt_has_child)=="try-error"))
                h_ctab_reshape()
        }
        
        ##signal that no new ctab is necessary
        #new.disp <<- FALSE
        #new.descr <<- FALSE
        new.ctab <<- FALSE
        #print("ctab event")
        #print(paste("new.disp:", new.disp))
        #print(paste("new.descr:", new.descr))
        #print(paste("new.ctab:", new.ctab))

        ##signal that no new ctab after sync is necessary
        new.ctab.sync <<- FALSE
        #print(paste("new.descr.sync:", new.descr.sync))
        #print(paste("new.ctab.sync:", new.ctab.sync))
    }
    
    ##handler to update variables in gtable instance
    h_ctab_vars.ins <- function(h, choice=radio.sel, ins=ctab.vars.init, 
        drop.vars=FALSE, dropped.vars=ctab.dropped, ret=FALSE, 
        null.sel=FALSE, tmp.sel=NULL, ...){
        #print("go-h_ctab_vars.ins")
        #print(ctab.vars.init)
        #restore.point('f', F)
        
        ##displayed sel should be NULL
        if(null.sel){
            tb_ctab[] <<- data.frame("Variables"=character(0))
            return()
        }

        ##displayed sel is !NULL
        choice <- names(details_choices)[choice]
        selection <- ins[[choice]]
        if(!is.null(tmp.sel)) selection <- tmp.sel
        #if(!is.null(tmp.sel)){
        #    tb_ctab[] <<- data.frame("Variables"=
        #        tmp.sel[!(tmp.sel %in% dropped.vars)])
        #    return()
        #}
        if(!drop.vars){
            out <- selection
            if(ret) return(out) else 
                tb_ctab[] <<- data.frame("Variables"=out)
        } else {
            out <- selection[!(selection %in% dropped.vars)]
            if(ret) return(out) else 
                tb_ctab[] <<- data.frame("Variables"=out)
        }
    }
    
    lyt_ctab <- glayout(homogeneous=F, cont=gg_ctab2, expand=TRUE, fill=T)
    ##FIXME ??add clear button and DnD area to gframe (render 'clear field' more visible UI)
    ##FIXME disable c-menu clear when not needed
    field.nms <- c("Row Fields", "Column Fields", "Values")
    for(i in 1:3){
        lyt_ctab[1,i, expand=TRUE, fill=T] <- 
                f_lyt_ctab[[i]] <- gframe("", horizontal=FALSE,
                              container=lyt_ctab, expand=TRUE, fill=T)
        ##have gframe with custom label (and context menu)
        l_lyt_ctab[[i]] <- glabel(field.nms[i])
        tooltip(l_lyt_ctab[[i]]) <- paste(
            "Right-click on", field.nms[i], "to clear field variables")
        #print(i)
        #print(field.nms[i])
        h_ctab_clear.force <- function(i){
            force(i)
            #print(i)
            function(h, ...){
                h_ctab_clear(all.fields=FALSE, field.nr=i)
            }
        }
        addRightclickPopupMenu(l_lyt_ctab[[i]], 
        #addPopupMenu(l_lyt_ctab[[i]], 
            list(a=gaction("Clear field", icon="clear", 
                    handler=h_ctab_clear.force(i)
                        #function(h, ...){
                        #    h_ctab_clear(all.fields=FALSE, field.nr=i)
                        #}
                        )))
        f_lyt_ctab[[i]]$block$setLabelWidget(l_lyt_ctab[[i]]$block)         # the voodoo
        l_lyt_ctab[[i]]$widget$setSelectable(FALSE)           # may not be needed
    }
    #lyt_ctab[1,2, expand=TRUE, fill=T] <- gframe("Column Fields", horizontal=FALSE,
    #                      container=lyt_ctab, expand=TRUE, fill=T)
    #lyt_ctab[1,3, expand=TRUE, fill=T] <- gframe("Values", horizontal=FALSE,
    #                      container=lyt_ctab, expand=TRUE, fill=T)
    lyt_ctab[1,4, expand=TRUE, fill=T] <- gframe("Options", horizontal=FALSE,
                      container=lyt_ctab, expand=TRUE, fill=T)
    g_cmb.fun_ctab <- ggroup(cont=lyt_ctab[1,4])
    ##FIXME !!add margins cb
    cmb.fun_ctab <- gcombobox(c("length", "length2", "mean", "median", "sd", "var", "sum"), editable=TRUE, 
              use_completion=TRUE, cont=g_cmb.fun_ctab)
    #size(cmb.fun_ctab) <- c(140,25)
    cmb.fun.args_ctab <- gcombobox(c("", "na.rm=TRUE"), selected=1, editable=TRUE, 
              use_completion=TRUE, cont=g_cmb.fun_ctab)
    
    g_df_ctab_box <- ggroup(cont=gg_ctab0, expand=T)
    DF_ctab <- glabel("", cont=g_df_ctab_box, expand=TRUE)
    
    h_ctab_reshape <- function(h, choice=radio.sel, ...){
        ##FIXME !!strange issue with varname_varname when:
        ##'    adding two value vars
        ##'    adding grp on col field
        ##'    rm grp in col field
        ##'    rm 1 value var
        ##'    Error in eval(expr, envir, enclos) : object 'variable' not found
        ##FIXME !!error when row selection is zero
        ##Error in dim(ordered) <- ns : 
		##  dims [product 1] do not match the length of object [0]
		##Error in names(details_choices)[choice] : 
		##  invalid subscript type 'externalptr'
        #print(ctab.sel[["1"]])
        #print(ctab.sel[["2"]])
        choice <- names(details_choices)[choice]
        ##use melt if two or more Value vars
        use.melt <- length(ctab.sel[["3"]]) >= 2
        form.ctab <- formula(paste(
            if(any(is.null(ctab.sel[["1"]]), length(ctab.sel[["1"]])==0)) "." else 
                paste(unlist(ctab.sel[["1"]]), collapse=" + "), 
            if(any(is.null(ctab.sel[["2"]]), length(ctab.sel[["2"]])==0)) 
                ifelse(!use.melt, ".", "variable") else 
                    paste(c(unlist(ctab.sel[["2"]]), if(use.melt) "variable"), collapse=" + "), 
            sep=" ~ "))
        if(use.melt){
            form.vars.ctab <- all.vars(form.ctab)
            form.vars.ctab <- form.vars.ctab[!(form.vars.ctab %in% ".")]
            form.vars.ctab <- form.vars.ctab[!grepl("variable", form.vars.ctab, fixed=T)]
            m.data_set <- melt(
                            if(choice=='full') data_set else 
                            if(choice=='col') data_set[ , cnms.disp] else 
                            if(choice=='sel') data_set[rows.disp , cnms.disp] else 
                            if(choice=='row') data_set[rows.disp , ], 
                id.vars=form.vars.ctab, 
                measure.vars=unlist(ctab.sel[["3"]]))
            #form.ctab <- update(form.ctab, ~ . + variable)
        }
        #print(form.ctab)
        #print(svalue(cmb.fun_ctab))
        fun.args_ctab <- as.list(parse(text=paste0("f(", svalue(cmb.fun.args_ctab) , ")"))[[1]])[-1]
        #print(fun.args_ctab)
        ##assume df is already molten (in long-format)
        ##FIXME use try to catch incorrect arguments
        df.ctab <- do.call(dcast, 
                         c(list(if(use.melt) m.data_set else {
                         ##FIXME ??speed-up: mv this into h_ctab_vars and have copy of subset (doc consider memory usage, though)
                            if(choice=='full') data_set else 
                            if(choice=='col') data_set[ , cnms.disp] else 
                            if(choice=='sel') data_set[rows.disp , cnms.disp] else 
                            if(choice=='row') data_set[rows.disp , ]
                         }, 
                         form.ctab, fun.aggregate=get(svalue(cmb.fun_ctab))), 
                         fun.args_ctab, 
                         list(value.var=if(!use.melt) unlist(ctab.sel[["3"]]) else "value")))
        ##FIXME fix this wehn melt involved
        if(!use.melt){
            cat(paste("dcast(", data_set_name, ", ", form.ctab, ", fun.aggregate=", 
                    svalue(cmb.fun_ctab), ", ", if(length(fun.args_ctab)!=0) paste(
                        svalue(cmb.fun.args_ctab), ", ", sep=""),
                    "value.var='", unlist(ctab.sel[["3"]]), "')", sep=""), "\n\n")
        }
        ##update GUI
        if(use.melt){
            ##FIXME check that no clash with "variable" var in data_set_nms
            ##FIXME make it possible to move buttons between row/col fields
            #b_dnd <- "variable"
            #x <- 2
            #g_dnd_name <- paste("g_", b_dnd, x, "...fixed", sep="")
            if(!has_b_melt_var){
                #print(g_dnd_name)
                #g_variable2...fixed <<- ggroup(cont=lyt_ctab[1,x])
                g_variable2...fixed <<- ggroup(cont=lyt_ctab[1,2])
                #assign(g_dnd_name, ggroup(cont=lyt_ctab[1,x]))
                #b_melt_var.ctab <<- gbutton(b_dnd, cont=get(g_dnd_name), expand=F, fill=F, 
                b_melt_var.ctab <<- gbutton("variable", cont=g_variable2...fixed, expand=F, fill=F, 
                    handler=function(h, ...){
                    #delete(lyt_ctab[1,x], get(g_dnd_name))
                    delete(lyt_ctab[1,2], g_variable2...fixed)
                    g_variable2...fixed <<- NULL
                    #ctab.dropped <<- ctab.dropped[!(ctab.dropped %in% b_dnd)]
                    #tb_ctab[] <- old_selection[!(old_selection %in% ctab.dropped)]
                    #ctab.sel_tmp <- ctab.sel[[as.character(x)]]
                    #ctab.sel[[as.character(x)]] <<- ctab.sel_tmp[!(ctab.sel_tmp %in% b_dnd)]
                    #delete(g_df_ctab_box, g_df_ctab_box[1])             # remove child
                    #DF_ctab <<- glabel("", cont=g_df_ctab_box, expand=TRUE)
                    #lyt_has_child <- try(is.null(lyt_ctab[1,lyt_val]$children[[1]]), silent=T)
                    ##FIXME there is an error here
                    #if(names(lyt_ctab[1,x]) != "Values"){
                    #	if(class(lyt_has_child)=="try-error") return()
                    })
                blockHandlers(b_melt_var.ctab)
                font(b_melt_var.ctab) <- list(color = "darkblue")
				has_b_melt_var <<- TRUE
            }
        }
        ##insert resulting cross tab in the gui
        delete(g_df_ctab_box, g_df_ctab_box[1])             # remove child
        DF_ctab <<- gdf(df.ctab, cont=g_df_ctab_box, expand=TRUE, 
                       freeze_attributes=TRUE)
        sapply(1:ncol(DF_ctab), function(j) editable(DF_ctab, j) <- FALSE) 
		DF_ctab$set_selectmode("multiple")
    }
    
    ##FIXME Error in get(svalue(cmb.fun_ctab)) : object 'ma' not found
    ##FIXME !!be more selective when activate h_ctab_reshape: put check inside fun
    ##(e.g. it activates even if no Value var)
	#    Error in if (!(value.var %in% names(data))) { : 
	#  argument is of length zero
    addHandlerChanged(cmb.fun_ctab, function(h, ...){
		#svalue(cmb.fun.args_ctab) <- ""
		h_ctab_reshape()
	})
	##FIXME need to think of a way to request confirmatoin from user when finished inputting args
    addHandlerChanged(cmb.fun.args_ctab, h_ctab_reshape)
    
    ##handle change of radio choice in Crosstab tab
    addHandlerChanged(r_ctab, function(h, ...){
        if(crosstab.on.tab.sel){
        ##by default update Crosstab only when the tab is selected
            if(svalue(ntbk)==3){
                h_ctab_vars()
                h_ctab_vars.ins(drop.vars=TRUE)
                #h_ctab_clear()
                new.descr.sync <<- TRUE
                #print("ctab sync event")
                #print(paste("new.descr.sync:", new.descr.sync))
                #print(paste("new.ctab.sync:", new.ctab.sync))
            }
        }
    })


    ##FIXME ??allow duplication of vars from one target to another (using ctrl+DnD)
    ##FIXME allow reverse DnD, to allow rm of buttons from target area
    ##FIXME ??add rm button in front (or have buttons with close buttons)
    ##REQ can gdf() handle arrays (acast)
    lapply(1:3, function(x){
        addDropTarget(lyt_ctab[1,x], handler=function(h,...) {
            lyt_val <- 3
            b_dnd <- h$dropdata
            field.nm <- svalue(l_lyt_ctab[[x]])
            #print(b_dnd)
            #print(field.nm)
            ##REQ how to disable DnD notification when button is already present
            ##refuse spurious drops
            ##FIXME !!radio sync: respect radio sel on drop: old_selection | data_set_nms
            ##'use a r_sync.sel global var
            if(!(b_dnd %in% old_selection)) return()
            #print(length(lyt_ctab[1,x]$children))
            ctab.sel[[as.character(x)]] <<- sapply(lyt_ctab[1,x]$children, 
                               function(y) svalue(y$children[[1]]))
            #print(unlist(ctab.sel))
            if(any(unlist(ctab.sel) %in% b_dnd)) return()
            #gbutton(h$dropdata, cont=lyt_ctab[1,x])
            #break.point()
            lyt_has_child <- try(is.null(lyt_ctab[1,lyt_val]$children[[1]]), silent=T)
            lyt_has_child_2nd <- try(is.null(lyt_ctab[1,lyt_val]$children[[2]]), silent=T)
            ##FIXME add message to inform users of failed DnD, or tooltip
            if(field.nm == "Values"){
                ##if only 1 Value exists, rm fixed 'variable' button
                if(class(lyt_has_child)!="try-error"){
            if(F){
                    lyt_val_tmp <- svalue(lyt_ctab[1,x]$children[[1]]$children[[1]])
                    #print(lyt_val_tmp)
                    ctab.dropped <<- ctab.dropped[!(ctab.dropped %in% lyt_val_tmp)]
                    tb_ctab[] <- old_selection[!(old_selection %in% ctab.dropped)]
                    ctab.sel_tmp <- ctab.sel[[as.character(x)]]
                    ctab.sel[[as.character(x)]] <<- ctab.sel_tmp[!(ctab.sel_tmp %in% 
                        lyt_val_tmp)]
                    #ctab.sel[[as.character(x)]] <<- list()
                    #print(ctab.sel[["3"]])
                    delete(lyt_ctab[1,x], lyt_ctab[1,x]$children[[1]])
            }
                }
            }
            g_dnd_name <- paste("g_", b_dnd, x, sep="")
            assign(g_dnd_name, ggroup(cont=lyt_ctab[1,x]))
            gbutton(b_dnd, cont=get(g_dnd_name), expand=F, fill=F, 
				handler=function(h, ...){
				#print("asdf")
                #print(x)
                #print(ctab.sel[["1"]])
                #print(ctab.sel[["2"]])
                delete(lyt_ctab[1,x], get(g_dnd_name))
				ctab.dropped <<- ctab.dropped[!(ctab.dropped %in% b_dnd)]
				#tb_ctab[] <- old_selection[!(old_selection %in% ctab.dropped)]
                #restore.point('f', F)
                ##take into account if there is an active search
                if(is.null(tb_ctab.tmp.sel)){
                    h_ctab_vars.ins(drop.vars=TRUE)
                } else {
                    ##active search
                    h_ctab_vars.ins(drop.vars=TRUE, tmp.sel=tb_ctab.tmp.sel)
                }
				ctab.sel_tmp <- ctab.sel[[as.character(x)]]
				ctab.sel[[as.character(x)]] <<- ctab.sel_tmp[!(ctab.sel_tmp %in% b_dnd)]
				delete(g_df_ctab_box, g_df_ctab_box[1])             # remove child
				DF_ctab <<- glabel("", cont=g_df_ctab_box, expand=TRUE)
				lyt_has_child <- try(is.null(lyt_ctab[1,lyt_val]$children[[1]]), silent=T)
				if(class(lyt_has_child)=="try-error") return()
                ##FIXME !!some error still lurking with many changes to layout: click 1st Value button and then 'var' doesn't get rmed
                ##Error in eval(expr, envir, enclos) : object 'variable' not found
                if(field.nm == "Values"){
                    if(all(class(lyt_has_child_2nd)=="try-error", has_b_melt_var)){
                        #print('go')
                        #restore.point('f', F)
                        #lyt_ctab[1,2]$children[[1]]
                        #for(i in 1:2)
                        #    try(delete(lyt_ctab[1,i], 
                        #               get(paste("g_", "variable", 2, "...fixed", sep=""))))
                        unblockHandlers(b_melt_var.ctab)
                        b_melt_var.ctab$invoke_change_handler()
                        b_melt_var.ctab <<- NULL
                        has_b_melt_var <<- FALSE
                    }
                }
                #print("go2")
				#print(ctab.sel[["1"]])
				#print(ctab.sel[["2"]])
                #break.point()
				h_ctab_reshape()
            })
#             b_dnd_name <- paste("b_", b_dnd, sep="")
#             assign(b_dnd_name, gbutton(b_dnd, cont=get(g_dnd_name), expand=F, fill=F))
#             addHandlerRightclick(get(b_dnd_name), handler=function(h, ...){
# 				print("asdf")
# 				delete(lyt_ctab[1,x], h$parent)
#             })
            #gg_val <- ggroup(cont=lyt_ctab[1,x])
            #gbutton(h$dropdata, cont=gg_val)
            #addSpring(gg_val)
            ctab.dropped <<- c(ctab.dropped, b_dnd)
            #tb_ctab[] <- old_selection[!(old_selection %in% ctab.dropped)]
            #print("here")
            #restore.point('f', F)
            ##take into account if there is an active search
            if(is.null(tb_ctab.tmp.sel)){
                h_ctab_vars.ins(drop.vars=TRUE)
            } else {
                ##active search
                h_ctab_vars.ins(drop.vars=TRUE, tmp.sel=tb_ctab.tmp.sel)
            }
            ctab.sel[[as.character(x)]] <<- c(ctab.sel[[as.character(x)]], b_dnd)
            #print(ctab.sel)
            ##do not initiate cross-tab if no value.var selected
            if(field.nm != "Values"){
                if(class(lyt_has_child)=="try-error") return()
            }
            h_ctab_reshape()
        })
    })
    
    }
    
    ##init Details tab on start-up
    ##default to column selection or subset selection ??
    radio.sel <- 3
    radio.inst <- ""
    r_sync()
    ##by default update Details only when the tab is selected
    if(!details.on.tab.sel){
        h_details()
        h_details.ins()
    }
    ##by default update Crosstab only when the tab is selected
    if(!crosstab.on.tab.sel){
        h_ctab_vars()
        h_ctab_vars.ins()
    }

    ##focus Filter tab
    svalue(ntbk) <- 1
    ##FIXME thoroughly test !details.on.tab.sel
    if(details.on.tab.sel){
        ##by default update Details only when the tab is selected
        ##and if there is a newly displayed data frame
        addHandlerChanged(ntbk, function(h, ...){
                if(h$page.no==2){
                    if(any(new.descr, new.descr.sync)){
                        h_details()
                        h_details.ins()
                    }
                    blockHandlers(b_disp)
                    svalue(b_disp, append=T) <- paste('Describe', h_disp_lab, sep='')
                    b_disp$set_icon("execute")
                    font(b_disp) <- b_disp_font
                    unblockHandlers(b_disp)
                    #print("switch-to-tab-2 event")
                    #print(paste("new.disp:", new.disp))
                    #print(paste("new.descr:", new.descr))
                    #print(paste("new.ctab:", new.ctab))
                }
            })
    }
	}
    if(filter.on.tab.sel){
        ##by default update Filter only when the tab is selected
        ##and if there is a newly displayed data frame
        addHandlerChanged(ntbk, function(h, ...){
                if(h$page.no==1){
                    if(new.disp){
                        h_filter()
                    }
                    blockHandlers(b_disp)
                    svalue(b_disp, append=T) <- paste('Display', h_disp_lab, sep='')
                    b_disp$set_icon("execute")
                    font(b_disp) <- b_disp_font
                    unblockHandlers(b_disp)
                    #print("switch-to-tab-1 event")
                    #print(paste("new.disp:", new.disp))
                    #print(paste("new.descr:", new.descr))
                    #print(paste("new.ctab:", new.ctab))
                }
            })
    }

    if(crosstab.on.tab.sel){
        ##by default update Crosstab only when the tab is selected
        ##and if there is a newly displayed data frame
        addHandlerChanged(ntbk, function(h, ...){
                if(h$page.no==3){
                    if(any(new.ctab, new.ctab.sync)){
                        h_ctab_vars()
                        h_ctab_vars.ins(drop.vars=TRUE)
                        #h_ctab_clear()
                    }
                    blockHandlers(b_disp)
                    svalue(b_disp, append=T) <- paste('Define', h_disp_lab, sep='')
                    b_disp$set_icon("execute")
                    font(b_disp) <- b_disp_font
                    unblockHandlers(b_disp)
                    #print("switch-to-tab-3 event")
                    #print(paste("new.disp:", new.disp))
                    #print(paste("new.descr:", new.descr))
                    #print(paste("new.ctab:", new.ctab))
                }
            })
    }


    ##update button label given tab selection
    ##FIXME something doesn't work as expected
#     addHandlerChanged(ntbk, function(h, ...){
#         svalue(b_disp, append=T) <- paste(if(h$page.no==1) 'Display' else 
#             if(h$page.no==2) 'Describe', h_disp_lab, sep='')
#         b_disp$set_icon("execute")
#     })

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
    ##FIXME it works here, but not above
    svalue(pg_ctab) <- 0.15
    #size(gg_ctab2) <- c(1000, 300)
    ##REQ setting one dim makes the other stuck (prove with DnD)
    #size(lyt_ctab)[2] <- 100
    #size(lyt_ctab[1,1])[1] <- c(250)
    #size(lyt_ctab[1,4]) <- c(100, 100)
    ##REQ/FIXME generates assertion
    ##(R:18255): Gtk-CRITICAL **: IA__gtk_table_attach: assertion 'child->parent == NULL' failed
    size(lyt_ctab[1,4])[2] <- c(100)
    #print(size(gg_ctab2))
    #print(size(lyt_ctab))
    #print(size(lyt_ctab[1,1]))
    #print(size(lyt_ctab[1,2]))

    
    ##initially focus the c_names search box
    ##FIXME not sure if you want this
    focus(ed) <- TRUE

    ##activate hidden panel
    if(hide) b_hide$invoke_change_handler()
    
    ##set some key-bindings
#     if(esc){
#         h_esc <- addHandlerKeystroke(w, function(h, ...){
#             if(h$key=="\033") dispose(w)
#         })
#         #print(h_esc)
#         #break.point()
#         addHandlerBlur(w, function(h, ...){
#             #print(h_signal <- addHandler(w, "key-release-event"))
#             #print(addHandler(h_esc))
#             blockHandler(w, h_esc)
#             #on.exit(unblockHandler(w, h_esc))
#         })
#     }
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
