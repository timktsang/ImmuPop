test_that("geo_mean computes geometric mean correctly", {
  # geo_mean of c(10, 100) = exp(mean(log(c(10,100)))) = sqrt(1000) ~ 31.623
  expect_equal(geo_mean(c(10, 100)), sqrt(1000), tolerance = 1e-6)

  # geo_mean of identical values returns that value
  expect_equal(geo_mean(c(40, 40, 40)), 40)

  # geo_mean of single value returns that value
  expect_equal(geo_mean(5), 5)
})

test_that("weighted_gmt computes weighted geometric mean", {
  # With equal proportions and one group, should equal geo_mean
  df_single <- data.frame(raw_titer = c(10, 20, 40), agegp1 = factor("[0,100)"))
  result <- weighted_gmt(df_single, age_prop = c(1.0))
  expect_equal(result, geo_mean(c(10, 20, 40)), tolerance = 1e-6)

  # With two groups and equal proportions
  df_two <- data.frame(
    raw_titer = c(10, 10, 40, 40),
    agegp1 = factor(c("[0,18)", "[0,18)", "[18,100)", "[18,100)"))
  )
  result2 <- weighted_gmt(df_two, age_prop = c(0.5, 0.5))
  # weighted GMT = exp(0.5 * log(geo_mean_group1) + 0.5 * log(geo_mean_group2))
  # = exp(0.5 * log(10) + 0.5 * log(40)) = sqrt(10 * 40) = 20
  expect_equal(result2, 20, tolerance = 1e-6)
})

test_that("weighted_prop_HImorethan5 computes seropositive proportion", {
  # All titers >= 10 -> proportion = 1
  df_all_pos <- data.frame(raw_titer = c(10, 20, 40), agegp1 = factor("[0,100)"))
  expect_equal(weighted_prop_HImorethan5(df_all_pos, c(1.0)), 1.0)

  # All titers = 5 (below 10) -> proportion = 0
  df_all_neg <- data.frame(raw_titer = c(5, 5, 5), agegp1 = factor("[0,100)"))
  expect_equal(weighted_prop_HImorethan5(df_all_neg, c(1.0)), 0.0)

  # Mixed: 1 of 2 positive in each group, equal weights
  df_mixed <- data.frame(
    raw_titer = c(5, 10, 5, 20),
    agegp1 = factor(c("[0,18)", "[0,18)", "[18,100)", "[18,100)"))
  )
  # Each group has prop 0.5, weighted by 0.5 each -> 0.5 * 0.5 + 0.5 * 0.5 = 0.5
  expect_equal(weighted_prop_HImorethan5(df_mixed, c(0.5, 0.5)), 0.5)
})

test_that("weighted_prop_HImorethan5 uses >= 10 threshold (not > 5)", {
  # Titer of exactly 10 should count as seropositive
  df_exact10 <- data.frame(raw_titer = c(10), agegp1 = factor("[0,100)"))
  expect_equal(weighted_prop_HImorethan5(df_exact10, c(1.0)), 1.0)

  # Titer of 9 should NOT count
  df_nine <- data.frame(raw_titer = c(9), agegp1 = factor("[0,100)"))
  expect_equal(weighted_prop_HImorethan5(df_nine, c(1.0)), 0.0)
})
