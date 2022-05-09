
pCov <- function(x, ...)(sd(x, ...) / mean(x,...))*100
pCov1 <- function(x, ...)(mad(x, ...) / median(x,...))*100

blrstTrainDF %>% 
  filter(DATA_TYPE == "FIELD") %>% 
  group_by(SCN_CODE) %>% 
  dplyr::select(-DATA_TYPE, -MOSAIC_ID, -SCN_INDEX, -layer, -ID, -pr) %>% 
  summarise_all(.funs=list(avg=mean), na.rm=TRUE) %>% 
  dplyr::select(-SCN_CODE) %>% 
  summarise_all(.funs = list(cov=pCov)) %>% t %>% 
  as.data.frame %>% mutate(cn=rownames(.)) %>% 
  dplyr::select(2,1) %>% 
  arrange(desc(V1))

  View


blrstTrainDF_init$percCov
