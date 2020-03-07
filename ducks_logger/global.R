player_list <- sort(c("Hao", "Ambrose", "Josh", "Matt", "Fei", "Sudip D",
                      "Sudip L", "Ben", "Zhong", "Rimon", "Sebastian", "Bal",
                      "Ni", "Yulin", "Gaojie", "Raul", "Nicola"))


update_table <- function(df, mat) {
  
  for (i in 1:nrow(mat)) {
    
    irow <- mat[i, 1]
    icol <- mat[i, 2]
    val <- mat[i, 3]
    
    df[irow, icol] <- val
    
  }
  
  df
}