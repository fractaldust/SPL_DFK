library(yamldebugger)

allKeywords
"plot" %in% allKeywords
"tuning" %in% allKeywords

help(yaml.debugger.init)
d_init = yaml.debugger.init(getwd(), show_keywords = TRUE)


workdir = getwd() # folder that contains the folders of the quantlets
d_init = yaml.debugger.init(workdir, show_keywords = TRUE)
qnames = yaml.debugger.get.qnames(d_init$RootPath)
d_results = yaml.debugger.run(qnames, d_init)
OverView = yaml.debugger.summary(qnames, d_results, summaryType = "mini")


# 00-2-rep_cv.R does not work
