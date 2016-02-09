context("assertions")

expect_silent(assert(TRUE))
expect_error(assert(FALSE), "is not TRUE")

expect_silent(assert(is_flag(TRUE)))
expect_error(assert(is_flag("foo")), "not a length-one logical vector")

expect_error(assert(all_equal(1, 2)), "not all equal")
expect_silent(assert(all_equal(1, 1)))

expect_silent(assert(is_string("foo")))
expect_error(assert(is_string(1)), "not a length-one character vector")
expect_error(assert(is_string(c("foo", "bar"))), "not a length-one character vector")

expect_silent(assert(has_var(data.frame(foo = 1), "foo")))
expect_error(assert(has_var(data.frame(foo = 1), "bar")), "is not a variable in")

expect_silent(assert(is_count(1)))
expect_error(assert(is_count(-1)), "not a positive integer")
expect_error(assert(is_count("bar")), "not a positive integer")

expect_silent(assert(all_counts(c(1, 2))))
expect_error(assert(all_counts(c(1, "a"))), "not all positive integers")

expect_silent(assert(equal_length(1, 2)))
expect_error(assert(equal_length(1, 1:2)), "have different lengths")

expect_error(assert(is_subset(1, 2)), "is not a subset")
expect_silent(assert(is_subset(1, 1:2)))

expect_silent(assert(is_positive(1)))
expect_error(assert(is_positive(-1), "not a positive number"))

expect_error(assert(none_empty(list(c(), 1:2))), "empty dimension")
expect_silent(assert(none_empty(c(1:2, 3:4))))

expect_silent(assert(all_strings(list("foo", "bar"))))
expect_error(assert(all_strings(list("foo", 1))), "not all positive-length strings")

expect_silent(assert(has_all_names(data.frame(a = 1), "a")))
expect_error(assert(has_all_names(data.frame(a = 1), "b"), "not all .* are names in"))

expect_error(assert(is_numeric("a")), "not a positive-length numeric")
expect_silent(assert(is_numeric(1)))

expect_silent(assert(names_match(data.frame(a=1), data.frame(a=2))))
expect_error(assert(names_match(data.frame(a=1), data.frame(b=2))), "names differ")

expect_silent(assert(names_subset(c(a=1), c(b=1, a=2))))
expect_error(assert(names_subset(c(a=1), c(b=1, c=2))), "names .* are not a subset of those in")

expect_silent(assert(all_in(1, c(2,1))))
expect_error(assert(all_in(1, c(2,3))), "does not include all")

expect_error(assert(none_in(1, 1)), "contains")
expect_silent(assert(none_in(1, 2)))

expect_error(assert(has_length(1:2, 1L), "does not have length"))
expect_silent(assert(has_length(1:2, 2)))
expect_silent(assert(has_length(1:2, 2L)))
