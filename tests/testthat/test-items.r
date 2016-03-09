context("items selector")

expect_silent({
  res = items(state_opinion, ids(state, year, "source"), items = starts_with("Q_"), controls(groups = 'race'))
})

expect_is(res$tbl, "data.frame")
expect_is(res$geo, "ItemVar")
expect_is(res$time, "ItemVar")
expect_is(res$survey, "ItemVar")
expect_is(res$items, "ItemVar")

expect_true(identical(res$tbl, state_opinion))
expect_identical(as.character(res$geo), "state")
expect_identical(as.character(res$time), "year")
expect_identical(as.character(res$survey), "source")
item_vars = c("Q_cces2006_minimumwage", "Q_cces2006_gaymarriageamendment", "Q_cces2006_abortion",
  "Q_cces2006_socialsecurity", "Q_cces2006_stemcell", "Q_naes2004_guncontrol", "Q_naes2000_gaysmilitary",
  "Q_naes2000_jobdiscgays", "Q_naes2004_estatetax", "Q_naes2004_stategaymarriage", "Q_cces2010_recoveryact",
  "Q_cces2010_cleanenergy", "Q_cces2010_healthreform", "Q_anwr_drilling", "Q_death_penalty", "Q_assault_weapons",
  "Q_universal_healthcare", "Q_health_children", "Q_partial_birth")
expect_true(identical(as.character(res$items), item_vars))

expect_silent({
  res = items(state_opinion, ids(state, year, "source"), items = starts_with("Q_"),
    hierarchical = hierarchical(state_opinion, pid3), controls(groups = 'race'))
})

expect_silent({
  res = items(state_opinion, ids(state, year, "source"), items = starts_with("Q_"),
    hierarchical = hierarchical(state_opinion, pid3), item_filter = item_filter(t = 2002:2008),
    controls(groups = 'race'))
})
