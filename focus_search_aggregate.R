library(mlr3misc)
library(data.table)

file_paths = list.files(
    path = "/gscratch/mbecke16/mbo_config/focus_search", 
    pattern = "focus_search_average",
    full.names = TRUE)

files = imap(file_paths, function(file, i) {
    message(i)
    readRDS(file)
})

tab = rbindlist(files)

fwrite(tab, "/gscratch/mbecke16/mbo_config/focus_search/focus_search_average.gz")

file_paths = list.files(
    path = "/gscratch/mbecke16/mbo_config/focus_search", 
    pattern = "focus_search_extrapolation_.*rds",
    full.names = TRUE)

files = imap(file_paths, function(file, i) {
    message(i)
    readRDS(file)
})

x = map(files, function(file) {
    file[["coefs"]] = list(file[["coefs"]])
    file
})

res = rbindlist(x)

res[, intercept := map_dbl(coefs, function(x) x[1])]
res[, iter := map_dbl(coefs, function(x) x[2])]

fwrite(res, "/gscratch/mbecke16/mbo_config/focus_search/focus_search_extrapolation.gz")
