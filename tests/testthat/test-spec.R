test_that("pk_spec has sensible defaults and parameter names", {
  s <- pk_spec()
  expect_equal(pk_params(s), c("ka", "cl", "v2", "q", "v3"))
  expect_equal(s$iiv, c("cl", "v2"))
  expect_equal(pk_params(pk_spec(ncmt = 1, lag = TRUE)), c("ka", "cl", "v", "tlag"))
  expect_equal(pk_params(pk_spec(ncmt = 1, route = "iv")), c("cl", "v"))
})


test_that("pk_spec rejects invalid structures", {
  expect_error(pk_spec(ncmt = 3), "1- and 2-compartment")
  expect_error(pk_spec(ncmt = 2, iiv = c("cl", "tlag")), "not in the model")
  expect_error(pk_spec(ncmt = 2, iiv = c("cl", "v2"), blocks = list(c("cl", "q"))), "without IIV")
})


test_that("pk_code generates the expected nlmixr2 model code", {
  s <- pk_spec(ncmt = 2, lag = TRUE, iiv = c("ka", "cl", "v2"), blocks = list(c("cl", "v2")))
  code <- pk_code(s)
  expect_match(code, "lka <- log(1)", fixed = TRUE)
  expect_match(code, "eta.cl + eta.v2 ~ c(", fixed = TRUE)
  expect_match(code, "eta.ka ~ 0.1", fixed = TRUE)
  expect_match(code, "alag(depot) <- exp(ltlag)", fixed = TRUE)
  expect_match(code, "linCmt() ~ prop(prop.sd)", fixed = TRUE)
  expect_true(is.function(pk_compile(s)))
})


test_that("error models are compiled correctly", {
  expect_match(pk_code(pk_spec(error = "add")), "linCmt() ~ add(add.sd)", fixed = TRUE)
  expect_no_match(pk_code(pk_spec(error = "add")), "prop.sd", fixed = TRUE)
  comb <- pk_code(pk_spec(error = "comb"))
  expect_match(comb, "linCmt() ~ prop(prop.sd) + add(add.sd)", fixed = TRUE)
  expect_match(comb, "add.sd <- 0.1", fixed = TRUE)
})


test_that("editing verbs return candidates and keep the parent", {
  base <- pk_spec(ncmt = 2, iiv = c("cl", "v2"))
  cand <- base |>
    pk_add_iiv("ka") |>
    pk_set_block(c("cl", "v2")) |>
    pk_set_error("comb")
  expect_s3_class(cand, "pk_candidate")
  expect_true(is.na(cand$parent_id))
  expect_equal(cand$spec$iiv, c("cl", "v2", "ka"))
  expect_equal(cand$spec$blocks, list(c("cl", "v2")))
  expect_equal(cand$spec$error, "comb")
  # the original is unchanged
  expect_equal(base$iiv, c("cl", "v2"))
})


test_that("covariates are added with the right term and can be dropped", {
  s <- pk_spec(ncmt = 2) |>
    pk_add_cov("cl", "WT", form = "power", ref = 70, init = 0.75) |>
    pk_add_cov("cl", "AGE", form = "linear", ref = 50) |>
    pk_add_cov("v2", "SEXF", form = "cat")
  cv <- s$spec$covariates
  expect_equal(cv$id, c("CL~WT", "CL~AGE", "V2~SEXF"))
  expect_equal(cv$term, c("cl_wt * log(WT/70)", "cl_age * (AGE - 50)", "v2_sexf * SEXF"))
  code <- pk_code(s)
  expect_match(code, "cl <- exp(lcl + cl_wt * log(WT/70) + cl_age * (AGE - 50) + eta.cl)", fixed = TRUE)
  expect_match(code, "cl_wt <- 0.75", fixed = TRUE)

  s2 <- pk_drop_cov(s, "CL~AGE")
  expect_equal(s2$spec$covariates$id, c("CL~WT", "V2~SEXF"))
})


test_that("fixed covariate parameters are written with fix()", {
  s <- pk_spec(ncmt = 2) |>
    pk_add_cov("cl", "WT", form = "power", ref = 70, init = 0.75, fix = TRUE)
  expect_match(pk_code(s), "cl_wt <- fix(0.75)", fixed = TRUE)
})


test_that("a covariate needs a reference value", {
  expect_error(pk_add_cov(pk_spec(), "cl", "WT", form = "power"), "reference value")
})


test_that("IOV is compiled into eta terms", {
  s <- pk_spec(ncmt = 2, iiv = c("cl", "v2", "ka")) |>
    pk_add_iov("ka", occasion = "OCC", levels = c(1, 2))
  code <- pk_code(s)
  expect_match(code, "iov.ka1 ~ 0.01", fixed = TRUE)
  expect_match(code, "iov.ka2 ~ 0.01", fixed = TRUE)
  expect_match(code, "iov.ka1 * (OCC == 1)", fixed = TRUE)
})


test_that("the spec hash follows the generated code", {
  a <- pk_spec(ncmt = 2)
  b <- pk_spec(ncmt = 2)
  c <- pk_spec(ncmt = 2, inits = list(cl = 12))
  expect_equal(spec_hash(a), spec_hash(b))
  expect_false(spec_hash(a) == spec_hash(c))
})


test_that("changing the structure drops IIV, blocks and covariates of removed parameters", {
  s <- pk_spec(ncmt = 2, lag = TRUE, iiv = c("cl", "v2", "tlag")) |>
    pk_add_cov("tlag", "WT", form = "power", ref = 70)
  s2 <- pk_set_structure(s, lag = FALSE)
  expect_equal(s2$spec$iiv, c("cl", "v2"))
  expect_equal(nrow(s2$spec$covariates), 0L)
  s3 <- pk_set_structure(s2, ncmt = 1)
  expect_equal(pk_params(s3$spec), c("ka", "cl", "v"))
  expect_equal(s3$spec$iiv, "cl")
})


test_that("verbs refuse to edit a custom model function", {
  f <- function() {
    ini({
      lcl <- log(1)
      prop.sd <- 0.1
    })
    model({
      cl <- exp(lcl)
      linCmt() ~ prop(prop.sd)
    })
  }
  s <- pk_spec(custom_fn = f)
  expect_error(pk_add_iiv(s, "cl"), "custom model function")
  expect_match(pk_code(s), "linCmt", fixed = TRUE)
})
