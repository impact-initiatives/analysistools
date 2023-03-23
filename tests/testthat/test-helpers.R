test_that("char_to_vector returns the correct outputs", {
  expected_results <- list(result1 = c("groupa", "groupb", "groupc"),
                           result2 = c("group_a", "group_b", "group_c"),
                           result3 = c("groupa", "groupb", "groupc"),
                           result4 = c("groupa"),
                           result5 = c("group_a")
  )

  expect_equal(char_to_vector("groupa, groupb, groupc"), expected_results$result1)
  expect_equal(char_to_vector("group_a,group_b,group_c"), expected_results$result2)
  expect_equal(char_to_vector("groupa , groupb , groupc "), expected_results$result3)
  expect_error(char_to_vector("groupa, "), "The group_var seems to have empty value, please check the inputs values")
  expect_error(char_to_vector("   ,group_a "), "The group_var seems to have empty value, please check the inputs values")
  expect_error(char_to_vector(c("group_a ", "group_b")) , "The group_var to be turned into a vector is already a vector.")
})
