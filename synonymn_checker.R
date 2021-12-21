#synonymn checker function
library(syn)

any(syn("plastic") %in% "paper")

any(syn("plastic") %in% syn("paper"))

syn("plastic") %in% syn("nylon")
