#' Extract Future Fs
#'
#' @param M list object created by read_admb function
#' @return dataframe of spawning biomass
#' @export
#' 
.get_fut_fs <- function(M)
{
    n <- length(M)
    mdf <- NULL
    for (i in 1:n)
    {
        A <- M[[i]]
        df <- data.frame(year = A$Future_F[,1])
        df$Model <- names(M)[i]
        df$year <- A$Future_F[,1]
        df$ssb  <- A$Future_F[,2]
        df$lb   <- A$Future_F[,4]
        df$ub   <- A$Future_F[,5]
        mdf     <- rbind(mdf, df)
    }
    return(mdf)
}


#' Plot Future Fs given future catches
#'
#' Spawning biomass may be defined as all males or some combination of males and
#' females
#'
#' @param M List object(s) created by read_admb function
#' @param xlab the x-label of the figure
#' @param ylab the y-label of the figure
#' @param ylim is the upper limit of the figure
#' @param alpha the opacity of the ribbon
#' @return Plot of model estimates of spawning stock biomass 
#' @export
#' 
plot_fut_Fs <- function(M, xlab = "Year", ylab = "Fishing mortality", ylim = NULL, xlim=NULL, alpha = 0.1)
{
    xlab <- paste0("\n", xlab)
    ylab <- paste0(ylab, "\n")
    
    mdf <- .get_fut_fs(M)
    
    p <- ggplot(mdf) + labs(x = xlab, y = ylab)
    
    if (!is.null(xlim))
        p <- p + xlim(xlim[1], xlim[2])        

    if (is.null(ylim))
    {
        p <- p + expand_limits(y = 0)
    } else {
        p <- p + ylim(ylim[1], ylim[2])        
    }
    
    if (length(M) == 1)
    {
        p <- p + geom_line(aes(x = year, y = ssb)) +
            geom_ribbon(aes(x = year, ymax = ub, ymin = lb), alpha = alpha,fill="salmon")
    } else {
        p <- p + geom_line(aes(x = year, y = ssb, col = Model),size=1.2) +
            geom_ribbon(aes(x = year, ymax = ub, ymin = lb, fill = Model), alpha = alpha)
    }
    
    if(!.OVERLAY) p <- p + facet_wrap(~Model)
    print(p + .THEME)
}
