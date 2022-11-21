test_that("multiplication works", {
  library(mylr)
  x <-  seq(1,10)
  y = 5*x+rnorm(10)
  data = data.frame(x,y)
  lm.model = mylr::mylm.basic(y~x,data)
  expect_equal(predict(lm(y~x,data),data), predict(mylm.basic(y~x),data))
})
