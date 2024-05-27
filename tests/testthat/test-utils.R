test_that("chunk_labels() creates suitable labels", {
  expect_equal(chunk_labels(c("Summer is hot", "", NA, " ", "Winter is cold  ")),
               c("Summer_is_hot", "", NA, " ", "Winter_is_cold"))
})
