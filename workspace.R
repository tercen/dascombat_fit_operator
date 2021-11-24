library(tercen)
library(tim) # tercen/tim
library(reshape2)
library(dplyr, warn.conflicts = FALSE)
source("R/core.R")

# http://localhost:5402/admin/w/55b87c4ed069d42cc8d1e1efde0155af/ds/26609041-bed5-4e61-8a4d-7e7def76999a

options("tercen.workflowId" = "55b87c4ed069d42cc8d1e1efde0155af")
options("tercen.stepId"     = "26609041-bed5-4e61-8a4d-7e7def76999a")

getOption("tercen.workflowId")
getOption("tercen.stepId")

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