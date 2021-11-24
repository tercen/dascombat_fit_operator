library(tercen)
library(tim) # tercen/tim
library(dplyr, warn.conflicts = FALSE)
source("R/core.R")

# tercenCtx
ctx = tercenCtx()

# obtain data
data = ctx %>% select(.y, .ri, .ci) %>%
  mutate(.color = ctx$select(ctx$colors[[1]]) %>% pull())

# cast data and fit model
y = acast(data, .ri ~ .ci, value.var = ".y")
bx = factor(acast(data, .ri ~ .ci, value.var = ".color")[1, ])

model = fit(Y, bx, mean.only = TRUE)

# serialize data and return back
res <- get_serialized_result(
  df = data,
  object = model,
  object_name = "dascombat_model",
  ctx = ctx
)

ctx$save(res)