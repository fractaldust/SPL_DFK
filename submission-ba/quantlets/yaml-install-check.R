#instructions from https://github.com/lborke/yamldebugger/blob/master/README.md

# needed only for package installation or update
library(devtools)
devtools::install_github("lborke/yamldebugger")

# load the package every time you want to use 'yamldebugger'
library(yamldebugger)


allKeywords
"plot" %in% allKeywords
"tuning" %in% allKeywords

workdir = getwd()  # folder that contains the folders of the quantlets
d_init = yaml.debugger.init(workdir, show_keywords = TRUE)
qnames = yaml.debugger.get.qnames(d_init$RootPath)
d_results = yaml.debugger.run(qnames, d_init)
OverView = yaml.debugger.summary(qnames, d_results, summaryType = "mini")

