
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
#' @param longFormat logical variable which defined your output format is long format or wide format
#' @examples
#' Xt = getSymbols('2330.TW', auto.assign = F)
#' Xt %>% Pt2Rt %>% ForwardSlidingWindow(20) %>% FswXt2Df(index(Xt)[c(1,3,5,7,9)]) %>% head
#' @export
FswXt2Df = function(fswXt, filterDatetimes=NULL, longFormat=F){
  if (is.null(filterDatetimes)){
    filteredXt = fswXt
  }else{
    filteredXt = fswXt[filterDatetimes]
  }
  retDf = data.frame(datetime=index(filteredXt),filteredXt)
  rownames(retDf) <- NULL

  retDf %<>% na.omit

  if (longFormat){
    retDf %<>% gather(key = "t",value = "Rt",-datetime) %>% arrange(datetime)
    retDf$t %<>% sub("t","",.) %>% as.numeric()
  }

  return(retDf)
}


####################################################
# simulateStopLimitPrices
####################################################

#' simulateStopLimitPrices
#'
#' a function which can help you transform your forward sampling xts to data.frame
#'
#' @param longFswXtDf a forward sampling xts with long format
#' @param limitRatio limit return ratio
#' @param stopRatio stop return ratio
#' @param NonHitIdxValue when the price not hit stop or limit, assign this value
#' @examples
#' Xt = getSymbols('2330.TW', auto.assign = F)
#' Xt %>% Pt2Rt %>% ForwardSlidingWindow(20) %>% FswXt2Df(index(Xt)[c(1,3,5,7,9)],longFormat=T) %>% simulateStopLimitPrices(0.06,0.06) %>% head
#' @export
simulateStopLimitPrices <- function(longFswXtDf, limitRatio, stopRatio, NonHitIdxValue = 1e6) {
  hitLimitDf =
    long_gcfswXtDf %>%
    filter(Rt >= (1+limitRatio)) %>%
    group_by(datetime) %>%
    summarise(hitLimitIdx = min(t)) %>%
    inner_join(long_gcfswXtDf,by=c("hitLimitIdx"="t","datetime"="datetime")) %>%
    mutate(hitLimitPrice=Rt) %>% select(-Rt)

  hitStopDf =
    long_gcfswXtDf %>%
    filter(Rt <= (1-stopRatio)) %>%
    group_by(datetime) %>%
    summarise(hitStopIdx = min(t)) %>%
    inner_join(long_gcfswXtDf,by=c("hitStopIdx"="t","datetime"="datetime")) %>%
    mutate(hitStopPrice=Rt) %>% select(-Rt)


  hitStopLimitDf =
    dplyr::full_join(hitLimitDf,hitStopDf,by="datetime") %>%
    mutate(hitLimitIdx = ifelse(is.na(hitLimitIdx),NonHitIdxValue,hitLimitIdx),
           hitStopIdx = ifelse(is.na(hitStopIdx),NonHitIdxValue,hitStopIdx)) %>%
    mutate(hitType = sign(hitStopIdx - hitLimitIdx),
           hitIdx = pmin(hitStopIdx,hitLimitIdx),
           hitPrice = 0) %>%
    mutate(hitPrice = ifelse(hitType == -1, hitStopPrice, hitPrice)) %>%
    mutate(hitPrice = ifelse(hitType == 1, hitLimitPrice, hitPrice)) %>%
    select(datetime,hitType,hitIdx,hitPrice)

  return(hitStopLimitDf)
}

####################################################
# doSeriesOfSimulateStopLimitPrices
####################################################

#' doSeriesOfSimulateStopLimitPrices
#'
#' a function which can help you transform your forward sampling xts to data.frame
#'
#' @param longFswXtDf a forward sampling xts with long format
#' @param limitRatioSeq series of limit return ratio
#' @param stopRatioSeq series of stop return ratio
#' @param NonHitIdxValue when the price not hit stop or limit, assign this value
#' @examples
#' Xt = getSymbols('2330.TW', auto.assign = F)
#' Xt %>% Pt2Rt %>% ForwardSlidingWindow(20) %>% FswXt2Df(index(Xt)[c(1,3,5,7,9)],longFormat=T) %>% simulateStopLimitPrices(0.06,0.06) %>% head
#' @export
doSeriesOfSimulateStopLimitPrices = function(longFswXtDf,
                                             limitRatioSeq = seq(0.01,0.1,0.01),
                                             stopRatioSeq=seq(0.01,0.1,0.01),
                                             NonHitIdxValue = 1e6){
  expsArgs = expand.grid(limitRatio = limitRatioSeq,
                         stopRatio = stopRatioSeq)

  expsArgs %<>% split(seq(nrow(expsArgs)))

  expsResults= lapply(expsArgs, function(args){
    args = as.list(args)
    args$longFswXtDf = long_gcfswXtDf
    retDf = do.call(simulateStopLimitPrices, args)
    retDf$limitRatio = args$limitRatio
    retDf$stopRatio = args$stopRatio
    return(retDf)
  })

  expsResultsDf = do.call(rbind,expsResults)
  return(expsResultsDf)
}

