context("hierarchical selector")

res = hierarchical(state_opinion, pid3, pid7, year, state)
expect_identical(state_opinion, res$tbl)
expect_is(res$tbl, "data.frame") 
expect_is(res$modifiers, "ItemVar") 
expect_is(res$t1_modifiers, "ItemVar") 
expect_is(res$time, "ItemVar") 
expect_is(res$geo, "ItemVar") 
expect_identical(as.character(res$modifiers), "pid3") 
expect_identical(as.character(res$t1_modifiers), "pid7") 
expect_identical(as.character(res$time), "year") 
expect_identical(as.character(res$geo), "state") 

res = hierarchical(state_opinion, pid3, time = year, geo = state)
expect_identical(res$t1_modifiers, res$modifiers)
expect_error(hierarchical(state_opinion), "'modifiers' not found")
expect_identical(res$t1_modifiers, res$modifiers)

res = hierarchical(state_opinion, "pid3")
expect_identical(as.character(res$modifiers), "pid3")

