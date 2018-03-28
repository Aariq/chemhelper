#' Calculate Van Den Dool and Kratz Retention Indicies
#'
#' @param rt The retention time of the compound
#' @param alkanesRT A vector of retention times of alkanes, in descending order
#' @param C_num A vector of the numbers of carbons for each of the alkanes
#'
#' @return A retention index
#' @export
#'
#' @examples
#' alkanes <- data.frame(RT = c(1.88, 2.23, 5.51, 8.05, 10.99,
#'                              14.10, 17.20, 20.20, 22.90, 25.60,
#'                              28.10, 30.50, 32.81, 35.22, 37.30),
#'                       C_num = 6:20)
#' calc_RI(11.237, alkanes$RT, alkanes$C_num)
#'
calc_RI <-
  function(rt, alkanesRT, C_num){
    if(length(alkanesRT) != length(C_num)){
      stop("Supplied alkaneRT and C_num must be equal length")
    }else if(rt == 0){
      return(NA)
    }else{
      leng = length(alkanesRT)
      n_int = findInterval(rt, alkanesRT)
      if(n_int == 1){
        warning("Compound RT is earlier than that of smallest alkane.")
        n_pos = n_int
      }else if(n_int == leng){
        warning("Compound RT is later than that of largest alkane.")
        n_pos = leng - 1
      }else{
        n_pos = n_int
      }
      t_n = alkanesRT[n_pos]
      n = C_num[n_pos]
      t_N = alkanesRT[n_pos + 1]
      RI = 100 * (n + (rt - t_n)/(t_N - t_n))
      return(RI)
    }
  }
