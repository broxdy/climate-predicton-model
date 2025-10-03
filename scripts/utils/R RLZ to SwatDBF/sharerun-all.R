dir_name = "sharerun OptSCN "
rlz.n = 30

rlz.f.pcp = "conclude rain all-rlz all-sta1971-2096SCNopt.csv"
source("rlztodbfV3-pcp-sharerun.R")

rlz.f = "conclude temp all-rlz all-sta.csv"
source("rlztodbfV3-tmp-sharerun.R")

rlz.f = "HMD1971-2096optSCN.csv"
source("rlztodbfV3-hmd-sharerun.R")

rlz.f = "SLR1971-2096optSCN.csv"
source("rlztodbfV3-slr-sharerun.R")

