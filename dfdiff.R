##GUI diff two data frames
dfdiff <- function(x, y, ...){
    require(RGtk2)
    require(prob)
    library(gWidgets2)
    options(guiToolkit="RGtk2")
    
    ##async data frame diff 
    Lp_diff <- setdiff(x, y, ...)
    Rp_diff <- setdiff(y, x, ...)
    
    ##add col info
    ##!!use nicer approach (vector, etc.)
    Lp_diff$aa__colour <- "red"
    Rp_diff$aa__colour <- "green"
    
    ##prepare containing widgets
    lp_frame <- rGtkDataFrame(Lp_diff)
    rp_frame <- rGtkDataFrame(Rp_diff)
    
    lp_view <- gtkTreeView(lp_frame)
    rp_view <- gtkTreeView(rp_frame)
    
    ##prepare df names
    nm_index <- function(nm, a) match(nm, names(a))
    lp_nms <- names(Lp_diff)
    rp_nms <- names(Rp_diff)
    
    ##apply colour to each column
    for(nm in lp_nms){
        lp_column <- gtkTreeViewColumn()
        lp_column$setTitle(nm)
        lp_cr <- gtkCellRendererText()
        lp_column$packStart(lp_cr)
        lp_idx <- nm_index(nm, Lp_diff)
        lp_column$addAttribute(lp_cr, "text", lp_idx - 1L)
        lp_col_idx <- nm_index(paste("__color", sep=""), Lp_diff)
        if(!is.na(lp_col_idx))
            lp_column$addAttribute(lp_cr, "background", lp_col_idx )
        lp_view$appendColumn(lp_column)
    }
    
    for(nm in rp_nms){
        rp_column <- gtkTreeViewColumn()
        rp_column$setTitle(nm)
        rp_cr <- gtkCellRendererText()
        rp_column$packStart(rp_cr)
        rp_idx <- nm_index(nm, Rp_diff)
        rp_column$addAttribute(rp_cr, "text", rp_idx - 1L)
        rp_col_idx <- nm_index(paste("__color", sep=""), Rp_diff)
        if(!is.na(rp_col_idx))
            rp_column$addAttribute(rp_cr, "background", rp_col_idx - 1L)
        rp_view$appendColumn(rp_column)
    }
    
    ##put all in gWidgets instance
    ##prepare containers
    w <- gwindow(visible=FALSE)
    pg <- gpanedgroup(cont=w)
    lp_g <- gvbox(cont=pg, expand=TRUE)
    rp_g <- gvbox(cont=pg, expand=TRUE)
    lp_l <- glabel("Left", cont=lp_g)
    pp_l <- glabel("Right", cont=rp_g)
    
    ##prepare rendered cells
    lp_scr <- gtkScrolledWindowNew()
    lp_scr$setPolicy("GTK_POLICY_AUTOMATIC","GTK_POLICY_AUTOMATIC")
    lp_scr$add(lp_view)
    add(lp_g, lp_scr)
    
    rp_scr <- gtkScrolledWindowNew()
    rp_scr$setPolicy("GTK_POLICY_AUTOMATIC","GTK_POLICY_AUTOMATIC")
    rp_scr$add(rp_view)
    add(rp_g, rp_scr)
    
    size(w) <- c(1000, 550)
    svalue(pg) <- .5
    visible(w) <- TRUE
    
}
Xe <- head(mtcars)
Xf <- head(mtcars)
Xf[2:4,3:5] <- 55
# all.equal(Xe, Xf)
# setdiff(Xe, Xf)
# setdiff2(Xe, Xf)
dfdiff(Xe, Xf)
