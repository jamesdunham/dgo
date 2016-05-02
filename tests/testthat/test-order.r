dgirt_in <- suppressMessages(shape(opinion, item_names = "Q_cces2006_abortion",
                  time_name = "year", geo_name = "state", group_names = c("race", "female"),
                  time_filter = 2006:2008, geo_filter = c("MA", "NY"),
                  survey_name = "source", weight_name = "weight"))
dgirt_out <- suppressMessages(suppressWarnings({
  dgirt(dgirt_in, iter = 20, chains = 2, cores = 2, open_progress = FALSE)
}))

test_that("group_grid and MMM are in matching order", {

  tb <- t(as.matrix(dgirt_out, pars = 'theta_bar'))[, 1:2]
  tb <- expand_rownames(as.data.frame(tb), c(dgirt_in$control@geo_name, 
                                             dgirt_in$control@group_names,  
                                             dgirt_in$control@time_name))

  col_order <- c(dgirt_in$control@group_names, dgirt_in$control@geo_name,
                 dgirt_in$control@time_name)
  tb <- tb[, col_order, with = FALSE]

  mmm_order <- dimnames(dgirt_in$MMM)[[3]]
  mmm_order <- as.data.frame(t(as.data.frame(strsplit(mmm_order, "_")))[, -3], stringsAsFactors = FALSE)
  names(mmm_order) <- col_order[-length(col_order)]

  for (y in dgirt_in$control@time_filter) {
    for (varname in c(dgirt_in$control@group_names, dgirt_in$control@geo_name)) {
             expect_identical(tb[year == y, get(k)], mmm_order[[k]])
    }
  }
})

test_that("trial/success vectors and MMM are in matching order", {

  nm <- names(dgirt_in$n_vec)
  nm <- as.data.frame(t(as.data.frame(strsplit(nm, "__"))))
  colnames(nm) <- c(dgirt_in$control@time_name, dgirt_in$control@geo_name,
                 dgirt_in$control@group_names, "item") 
  pos <- 0
  for (t_ in seq.int(dim(dgirt_in$MMM)[1])) {
    for (q_ in seq.int(dim(dgirt_in$MMM)[2])) {
      for (g in seq.int(dim(dgirt_in$MMM)[3])) {
        if (!dgirt_in$MMM[t_, q_, g]) {
          pos <- pos + 1
          pos_nm <- unlist(strsplit(names(dgirt_in$n_vec[pos]), "__"))
          mmm_grp <- unlist(strsplit(dimnames(dgirt_in$MMM)[[3]][g], "_"))
          expect_equal(pos_nm[1], dimnames(dgirt_in$MMM)[[1]][t_])
          expect_equal(pos_nm[2], mmm_grp[length(mmm_grp)])
          expect_equal(pos_nm[-c(1:2, length(pos_nm))],
                       mmm_grp[seq_along(dgirt_in$control@group_names)])
        }
      }
    }
  }
  expect_equal(pos, nrow(nm))
})
