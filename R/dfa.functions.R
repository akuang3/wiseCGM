#' DFA Random Walk (Step 1)
#'
#' Convert a vector into a random walk
#' @param glucose A vector of glucose values from CGM data
#' @export
#'
#'


convert.to.random.walk <- function(glucose){

  stopifnot(is.vector(glucose))

  temp.data <- data.frame(glucose=glucose,
                          sample=1:length(glucose),
                          stringsAsFactors=FALSE)

  ## given a bounded time series x_t of length N, where t E N, integration or
  ## summation first converts this into an unbounded process X_t:
  ## X_t = sumattion of i=[1, t] of x_i - (x), where (x) denotes the mean value of the time series
  ## X_t is called cumulative sum or profile. This process converts, for example
  ## an i.i.d. white noise process into a random walk
  ts.mean <- mean(temp.data[,'glucose'], na.rm=TRUE)

  temp.data$Bi_meanBi <- temp.data[,'glucose']-ts.mean

  temp.data$Y_k <- cumsum(temp.data$Bi_meanBi)

  return(cgm.rw=temp.data)
}


#' Monofractual Detrended Fluctuation Analysis (Step 2)
#'
#' Performed monofractual DFA for a givien dataframe of random walk values and a vector of scales
#' @param cgm.rw A dataframe of random walk values
#' @param scale.vect A vector of scales to determine bin sizes for each analyses
#' @export
#'

mono.dfa <- function(cgm.rw, scale.vect){

  ## Next, X_t is divided into time windows of length n samples each,
  ## and a local least square straight-line fit (the local trend) is calculated by
  ## minimising the squared errors within each time window.
  ## Let Y_t indicate the resulting piecewise sequence of straight-line fits
  ## Then, the root-mean-square deviation from the trend, the fluctuation, is calculated:
  ## F(n) = sqrt of 1/N times summation of t=[1, N] of (X_t - Y_t)^2

  dfa.results <- lapply(scale.vect, function(n){
    n_in_box <- rep(1:n, floor(dim(cgm.rw)[1]/n))
    box_n <- rep(1:floor(dim(cgm.rw)[1]/n), each=n)

    cgm.rw.n <- data.table::setDF(cgm.rw)[1:length(n_in_box), ]

    cgm.rw.n$n_in_box <- n_in_box
    cgm.rw.n$box_n <- box_n

    ## fit linear regressions (take n=[1:n] as x, and y_k as y)
    cgm.rw.box <- split(cgm.rw.n, f=cgm.rw.n$box_n)
    cgm.rw.box  <- lapply(cgm.rw.box, function(box){
      temp.linear.fit <- stats::lm(Y_k~Samples, data=box)
      box$Slope <- temp.linear.fit$coefficients[2]
      box$Intercept <- temp.linear.fit$coefficients[1]
      box$Y_nk <- (box$Samples*box$Slope) + box$Intercept
      box$Y_k_Y_nk <- (box$Y_k-box$Y_nk)^2
      box$RMS <- sqrt(mean(box$Y_k_Y_nk))
      return(box)
    })

    cgm.rw.n <- data.table::rbindlist(cgm.rw.box)
    F_n <- sqrt(sum(cgm.rw.n$Y_k_Y_nk)/dim(cgm.rw.n)[1])

    F_ns.df <- data.frame(n=n, F_n=F_n, stringsAsFactors=FALSE)

    return(list(F_ns.df=F_ns.df, cgm.rw.box=cgm.rw.box))
  })

  cgm.rw.box <- lapply(dfa.results, function(x) x$cgm.rw.box)
  f.ns <- lapply(dfa.results, function(x) x$F_ns.df)


  ## compute power law relation between overall RMS, indicated by the slope of regression between scales and fluctuation
  fluctuation.scale.df <- data.frame(scale=unlist(sapply(f.ns, function(x) x$n)),
                                     fluctuation=unlist(sapply(f.ns, function(x) x$F_n)),
                                     stringsAsFactors=FALSE)

  C <- stats::glm(log(fluctuation.scale.df$fluctuation)~log(fluctuation.scale.df$scale), family='gaussian')



  return(list(dfa.results=cgm.rw.box,
              fluctuation.scale.df=fluctuation.scale.df,
              Hurst_Exponent=as.numeric(C$coefficients[2])))
}






#' Multifractual Detrended Fluctuation Analysis (Step 3)
#'
#' Performed monofractual DFA for a givien dataframe of random walk values and a vector of scales
#' @param mono.dfa.test A monofractual DFA analysis output
#' @param q.vect A vector of scales for the q-th order statistics
#' @param scale.vect A vector of scales to determine bin sizes for each analyses
#' @export


multi.dfa <- function(mono.dfa.test, q.vect, scale.vect){

  ## extract the RMS from the dfa.object for all scales
  rms.list <- lapply(mono.dfa.test$dfa.results, function(dfa.scale){
    lapply(dfa.scale, function(dfa.box){
      dfa.box$RMS[1]
    })
  })

  rms.list <- lapply(rms.list, unlist)

  q.rms.list <- lapply(rms.list, function(rms){
    Fq.list <- lapply(q.vect, function(q){
      qrms <- rms^q
      if(q!=0){
        Fq <- mean(qrms)^(1/q)
      }else{
        Fq <- exp(0.5*mean(log(rms^2)))
      }
      return(data.frame(Fq=Fq, q=q))
    })
    Fq.df <- do.call(rbind, Fq.list)
    return(Fq.df)
  })

  q.rms.list <- mapply(function(qrms, scale.n){
    qrms$scale <- scale.n
    return(qrms)
  }, qrms=q.rms.list, scale.n=scale.vect, SIMPLIFY=FALSE)

  Fq <- do.call(rbind, q.rms.list)


  ### fit regression model between scale and Fq
  Fq.list <- split(Fq, f=Fq$q)
  Fq.reg.list <- lapply(Fq.list, function(x){

    ### q-order of Hurst exponent can be defined as the slopes of regression lines
    ### for each q-order RSMS
    q.fq.fit <- stats::glm(log2(x$Fq)~log2(x$scale), family='gaussian')

    return(q.fq.fit)

  })

  ### Hurst exponent
  Hq.list <- lapply(Fq.reg.list, function(x){
    as.numeric(x$coefficients[2])
  })
  Hq.list <- unlist(Hq.list)

  fitted.list <- lapply(Fq.reg.list, function(x){
    stats::fitted(x)
  })


  ### convert Hq to the q-order mass exponent (tq)
  tq.list <- lapply(names(Hq.list), function(q){
    Hq.temp <- Hq.list[[q]]
    q.temp <- as.numeric(q)
    (q.temp*Hq.temp)-1
  })
  tq.list <- unlist(tq.list)

  ### convert q-order mass exponent to q-order singularity exponent
  ### ## subtract t_q(i+1) - t_q(i)
  singularity.exp.q.list.top <- tq.list[2:length(tq.list)]-tq.list[1:(length(tq.list)-1)]
  singularity.exp.q.list.bottom <- q.vect[2:length(q.vect)]-q.vect[1:(length(q.vect)-1)]
  singularity.exp.q.list <- singularity.exp.q.list.top/singularity.exp.q.list.bottom

  ### q-order singularity dimension
  Dq <- (q.vect[1:(length(q.vect)-1)]*singularity.exp.q.list)-(tq.list[1:(length(q.vect)-1)])


  return(list(Fq=Fq,
              Fq.reg.list=Fq.reg.list,
              HurstExponent=Hq.list,
              MassExponent=tq.list,
              SingularityExponent=singularity.exp.q.list,
              SingularityDimension=Dq))

}




#' Local Fluctuation (Step 4)
#'
#' Performed monofractual DFA for a givien dataframe of random walk values and a vector of scales
#' @param cgm.rw A dataframe of random walk values
#' @param multi.dfa.test A multifractual DFA analysis output
#' @param scale_small A vector of "small" scales to determine bin sizes for each analyses
#' @export


local.fluctuation.hurst <- function(cgm.rw, multi.dfa.test, scale_small){

  halfmax <- floor(max(scale_small)/2)

  time_index <- (halfmax+1):(dim(cgm.rw)[1]-halfmax)


  ### fit regression using small scales to compute local fluctuation
  local.fluctuation.list <- lapply(scale_small, function(n){

    halfseg <- floor(n/2)
    fit.list <- lapply(time_index, function(v){
      T_index <- (v-halfseg):(v+halfseg)
      C <- stats::glm(cgm.rw$Y_k[T_index]~T_index, family='gaussian')
      C.fitted <- stats::fitted(C)
      RMS <- sqrt(mean((cgm.rw$Y_k[T_index]-C.fitted)^2))
      return(RMS)
    })
    rms <- unlist(fit.list)
    return(rms)
  })
  names(local.fluctuation.list) <- scale_small

  ### compute local Hurst exponent (Ht) from local fluctuation
  multi.fq.0 <- multi.dfa.test$Fq
  multi.fq.0 <- multi.fq.0[which(multi.fq.0$q==0), ]
  c.fit <- stats::glm(log2(Fq)~log2(scale), data=multi.fq.0, family='gaussian')

  predict.scale.small <- c.fit$coefficients[1]+c.fit$coefficients[2]*log2(scale_small)
  names(predict.scale.small) <- scale_small

  maxL <- dim(cgm.rw)[1]

  local.Hurst <- lapply(names(local.fluctuation.list), function(n){

    RMSt <- local.fluctuation.list[[n]]
    resRMS <- predict.scale.small[n] - log2(RMSt)
    logscale <- log2(maxL)-log2(as.numeric(n))
    Hurst_t <- resRMS/(logscale+multi.dfa.test$HurstExponent[names(multi.dfa.test$HurstExponent)==0])

  })


}




