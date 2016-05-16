suppressMessages({

context("test output against legacy output")

test_that("shape output matches wrangle output", {

    load("integration_env1.Rdata")
    suppressWarnings({
      d_int1 <- shape(item_data = item_data,
                      modifier_data = state_covariates,
                      aggregate_data = gss,
                      aggregate_item_names = unique(gss$item),
                      item_names = econ_items,
                      group_names = "black_x_urban",
                      time_name = "year",
                      time_filter = 1936:2014,
                      geo_name = "state",
                      survey_name = "D_source",
                      weight_name = "D_weight",
                      modifier_names = "nopooling",
                      t1_modifier_names = c("percent_evangelicals",
                                            "percent_hispanic",
                                            "percent_urban"),
                      constant_item = FALSE)
    })

  for (x in c("G", "Q", "T", "P", "H", "S"))
    expect_true(all.equal(d_int1[[x]], wrangle_out[[x]]))
  expect_true(all.equal(d_int1$N_observed, wrangle_out$N))

  sn <- as.data.table(tstrsplit(names(d_int1$n_vec), "__", fixed = TRUE))
  id_cols = c(d_int1$control@time_name, d_int1$control@geo_name,
              d_int1$control@group_names, "item")
  setnames(sn, id_cols)
  sn[, n := d_int1$n_vec]
  sn <- sn[n > 0]
  
  wn <- as.data.table(tstrsplit(names(wrangle_out$n_vec), " | ", fixed = TRUE))
  setnames(wn, c("V1", "V2"), c(wrangle_out$vars$time_id, "item"))
  wn[, c(wrangle_out$vars$geo_id, wrangle_out$vars$groups) := tstrsplit(V3, "__")]
  wn[, V3 := NULL]
  wn[, n := wrangle_out$n_vec]

  joined <- merge(wn, sn, by = id_cols, all = TRUE)
  expect_equal(sum(joined$n.x != joined$n.y), 0)

})

})
