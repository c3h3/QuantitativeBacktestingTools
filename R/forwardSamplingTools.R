
####################################################
# Pt2Rt
####################################################

#' Pt2Rt
#'
#' function which can help you transform your price xts to an accumulate return xts
#'
#' @param Xt a xts object
#' @param XtColumnSeletor Xt's single column selector
#' @examples
#' Xt = getSymbols('2330.TW', auto.assign = F)
#' Xt %>% Pt2Rt %>% head
#' @export
Pt2Rt <- function(Xt, XtColumnSeletor = Cl) {
  Rt = Xt %>% XtColumnSeletor %>% PerformanceAnalytics::CalculateReturns()
  Rt[1] = 0
  accRt <- cumprod(1+Rt)
  accRt
}


####################################################
# ForwardSlidingWindow
####################################################

#' ForwardSlidingWindow
#'
#' a funtion which an help you do forward sampling easier
#'
#' @param accRt an accumulate return xts object
#' @param win_size the size of forward sampling slide window
#' @examples
#' Xt = getSymbols('2330.TW', auto.assign = F)
#' Xt %>% Pt2Rt %>% ForwardSlidingWindow(20) %>% head
#' @export
ForwardSlidingWindow <- function(accRt, win_size) {
  fswXt = cbind(accRt/accRt,lag.xts(accRt,-1)/accRt)
  for (i in 2:(win_size-1)){
    fswXt = cbind(fswXt,lag.xts(accRt,-i)/accRt)
  }
  colnames(fswXt) <- paste0("t",1:win_size)
  return(fswXt)
}

####################################################
# FswXt2Df
####################################################

#' FswXt2Df
#'
#' a function which can help you transform your forward sampling xts to data.frame
#'
#' @param fswXt a forward sampling xts
#' @param filterDatetimes filtered fswXt by datetimes
#' @examples
#' Xt = getSymbols('2330.TW', auto.assign = F)
#' Xt %>% Pt2Rt %>% ForwardSlidingWindow(20) %>% FswXt2Df(index(Xt)[c(1,3,5,7,9)]) %>% head
#' @export
FswXt2Df = function(fswXt, filterDatetimes=NULL){
  if (is.null(filterDatetimes)){
    filteredXt = fswXt
  }else{
    filteredXt = fswXt[filterDatetimes]
  }
  retDf = data.frame(datetime=index(filteredXt),filteredXt)
  rownames(retDf) <- NULL

  return(retDf)
}
