generate.placebos <- function(dataprep.out,
                              synth.out,
                              Sigf.ipop = 5) {
  
  tr <- as.numeric(dataprep.out$tag$treatment.identifier)
  names.and.numbers <-
    subset(dataprep.out$names.and.numbers, unit.numbers != tr)
  n <- length(dataprep.out$tag$controls.identifier)
  
  b <-
    data.frame(matrix(
      0,
      ncol = n,
      nrow = length(dataprep.out$tag$time.plot)
    ))
  mspe.placs <- data.frame(matrix(0, ncol = 1, nrow = n))
  for (i in 1:n) {
    temp <- syn.plac(i, dataprep.out, Sigf.ipop)
    b[, i] <- temp$a
    colnames(b)[i] <-
      paste('synthetic', as.character(names.and.numbers[i, 2]), sep = '.')
    mspe.placs[i, ] <- temp$s.mspe
  }
  df <-
    cbind(
      b,
      dataprep.out$Y0,
      dataprep.out$Y1,
      dataprep.out$Y0plot %*% synth.out$solution.w,
      dataprep.out$tag$time.plot
    )
  colnames(df)[(ncol(df) - 2):ncol(df)] <-
    c('Y1', 'synthetic.Y1', 'year')
  t0 <- as.numeric(dataprep.out$tag$time.optimize.ssr[1])
  t1 <-
    as.numeric(dataprep.out$tag$time.optimize.ssr[length(dataprep.out$tag$time.optimize.ssr)]) +
    1
  treated.name <-
    as.character(dataprep.out$names.and.numbers$unit.names[dataprep.out$names.and.numbers[, 2] %in% dataprep.out$tag$treatment.identifier])
  loss.v <- synth.out$loss.v
  res2 <-
    list(
      df = df,
      mspe.placs = mspe.placs,
      t0 = t0,
      t1 = t1,
      tr = tr,
      names.and.numbers = names.and.numbers,
      n = n,
      treated.name = treated.name,
      loss.v = loss.v
    )
  return(res2)
}
