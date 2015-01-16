source('COACT_v1.R')
source('CCOACT_v1.R')
source('SPREPORT_v1.R')
source('NIGHTACT_v1.R')
source('VOTEACT_v1.R')
source('REVOTEACT_v1.R')
source('JINROU_v1.R')

start <- function() {
  RAND <- sample(2147483647, 1)
  JINROU(seed=RAND, open=1)
}
