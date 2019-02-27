
shrimp.data.fcn <- function(shrimp.data.raw){
  shrimp.data.raw %>%
    mutate(SIZE_GROUP_f = as.factor(SIZE_GROUP)) %>%
    mutate(SIZE_GROUP2 = case_when(SIZE_GROUP == "Extra Small" ~ "Tiny", 
                                   TRUE ~ SIZE_GROUP)) %>%
    mutate(SIZE_GROUP2 = factor(SIZE_GROUP2, 
                                levels = c("Tiny", "Small",
                                           "Medium", "Large", "Extra Large",
                                           "Jumbo", "Extra Jumbo",
                                           "Colossal", "Extra Colossal"))) %>%
    mutate(SIZE_GROUP_NUM = case_when(SIZE_GROUP2 == "Tiny" ~ 1,
                                      SIZE_GROUP2 == "Small" ~ 2,
                                      SIZE_GROUP2 == "Medium" ~ 3,
                                      SIZE_GROUP2 == "Large" ~ 4,
                                      SIZE_GROUP2 == "Extra Large" ~ 5,
                                      SIZE_GROUP2 == "Jumbo" ~ 6,
                                      SIZE_GROUP2 == "Extra Jumbo" ~ 7,
                                      SIZE_GROUP2 == "Colossal" ~ 8,
                                      SIZE_GROUP2 == "Extra Colossal" ~ 9,
                                      TRUE ~ NA_real_)) %>%
    mutate(SP_GROUP = case_when(grepl("penaeus", 
                                      tolower(SPECIES)) |
                                  grepl("penaeid", 
                                        tolower(SPECIES)) ~ "Penaeid",
                                grepl("pandalus", 
                                      tolower(SPECIES)) ~ "Pandelus",
                                TRUE ~ "Other")) %>%
    mutate(SP_GROUP_NUM = case_when(SP_GROUP == "Penaeid" ~ 1,
                                    SP_GROUP == "Pandelus" ~ 2,
                                    SP_GROUP == "Other" ~ 3,
                                    TRUE ~ NA_real_)) %>%
    mutate(ECOLABEL2 = case_when(is.na(ECOLABEL) ~ "NONE",
                                 TRUE ~ ECOLABEL)) %>%
    filter(ECOLABEL2 == "BAP" | 
             ECOLABEL2 == "MSC" | 
             ECOLABEL2 == "FAIR TRADE" | 
             ECOLABEL2 == "MBAG" | 
             ECOLABEL2 == "MBAY" | 
             ECOLABEL2 == "NONE") %>%
    mutate(SHRIMP_PROCESSING2 = case_when(grepl("FZ BRD", SHRIMP_PROCESSING) ~ "BREADED",
                                          SHRIMP_PROCESSING == "CKD" ~ "COOKED", 
                                          SHRIMP_PROCESSING == "CKD HL" ~ "COOKED",
                                          SHRIMP_PROCESSING == "CKD HO" ~ "COOKED", 
                                          SHRIMP_PROCESSING == "CKD TOFF" ~ "COOKED",
                                          SHRIMP_PROCESSING == "CKD TON" ~ "COOKED", 
                                          SHRIMP_PROCESSING == "FZ CKD HL" ~ "COOKED",
                                          SHRIMP_PROCESSING == "FZ CKD HO" ~ "COOKED",
                                          SHRIMP_PROCESSING == "FZ BLK HL" ~ "HEADLESS_RAW", 
                                          SHRIMP_PROCESSING == "FZ HL" ~ "HEADLESS_RAW",
                                          SHRIMP_PROCESSING == "HL" ~ "HEADLESS_RAW",
                                          SHRIMP_PROCESSING == "FZ BLK TON" ~ "PEELED_RAW", 
                                          SHRIMP_PROCESSING == "FZ BLK TOFF" ~ "PEELED_RAW",
                                          SHRIMP_PROCESSING == "FZ CKD TON" ~ "PEELED_RAW",
                                          SHRIMP_PROCESSING == "FZ CKD TOFF" ~ "PEELED_RAW",
                                          SHRIMP_PROCESSING == "FZ TON" ~ "PEELED_RAW", 
                                          SHRIMP_PROCESSING == "FZ TOFF" ~ "PEELED_RAW",
                                          SHRIMP_PROCESSING == "TOFF" ~ "PEELED_RAW", 
                                          SHRIMP_PROCESSING == "TON" ~ "PEELED_RAW",
                                          SHRIMP_PROCESSING == "FZ BLK HO" ~ "WHOLE_RAW", 
                                          SHRIMP_PROCESSING == "FZ HO" ~ "WHOLE_RAW",
                                          SHRIMP_PROCESSING == "HO" ~ "WHOLE_RAW", 
                                          SHRIMP_PROCESSING == "LIVE" ~ "WHOLE_RAW",
                                          TRUE ~ SHRIMP_PROCESSING)) %>%
    mutate(SHRIMP_PROCESSING_NUM = case_when(SHRIMP_PROCESSING2 == "BREADED" ~ 1,
                                             SHRIMP_PROCESSING2 == "COOKED" ~ 2,
                                             SHRIMP_PROCESSING2 == "HEADLESS_RAW" ~ 3,
                                             SHRIMP_PROCESSING2 == "PEELED_RAW" ~ 4,
                                             SHRIMP_PROCESSING2 == "WHOLE_RAW" ~ 5,
                                             TRUE ~ NA_real_)) %>%
    filter(PRICE < 95) -> shrimp.data
  
  return(shrimp.data)
}

# function to extract necessary data for RF analysis
RF.shrimp.data <- function(shrimp.data){
  shrimp.data %>% select(SP_GROUP, SP_GROUP_NUM,
                         SIZE_GROUP, SIZE_GROUP_NUM,
                         SHRIMP_PROCESSING, SHRIMP_PROCESSING_NUM,
                         YEAR, ZIP, MARKETTYPE, PRICE,
                         ECOLABEL2, PROD_METHOD) %>% 
    mutate(ECOLABEL_f = as.factor(ECOLABEL2),
           SP_GROUP_f = as.factor(SP_GROUP_NUM),
           SIZE_GROUP_f = as.factor(SIZE_GROUP_NUM),
           MARKETTYPE_f = as.factor(MARKETTYPE),
           SHRIMP_PROCESSING_f = as.factor(SHRIMP_PROCESSING_NUM),
           ZIP_f = as.factor(ZIP),
           #YEAR_f = as.factor(YEAR),
           PROD_METHOD_f = as.factor(PROD_METHOD),
           RV = runif(nrow(shrimp.data), 
                      min = 0, 
                      max = 100)) %>%
    select(PRICE, ECOLABEL_f, SP_GROUP_f, SIZE_GROUP_f,
           MARKETTYPE_f, SHRIMP_PROCESSING_f,
           ZIP_f, YEAR, PROD_METHOD_f, RV) %>%
    na.omit()-> shrimp.data.RF
  return(shrimp.data.RF)  
}

# function to run prediction on cRF. A vector is returned.
predict.cRF <- function(cRF.out = NULL, outfilename = NULL){
  if (!file.exists(outfilename)){
    pred <- predict(cRF.out, type="response", OOB=TRUE)
    saveRDS(pred, file = outfilename)
  } else {
    pred <- readRDS(outfilename)
  }
  
  
  return(as.vector(pred, mode = "numeric"))
}

# function to compute MSE and variance explained (pseudo R2). A list is returned.
fit.cRF <- function(predicted = NULL,
                    df = NULL){
  df$PREDICT <- predicted
  
  df %>% 
    mutate(residual = PRICE - PREDICT) -> df
  
  mse <- sum(df$residual^2)/nrow(df)
  pseudo.R2 <- 1-mse/var(df$PRICE)
  return(list(MSE = mse,
              R2 = pseudo.R2,
              df = df))  
}

# function to extract variable importance metric
var.imp.shrimp <- function(cRF.out = NULL,
                           outfile = NULL){
  if (!file.exists(outfile)){
    var.imp <- sort(varimp(cRF.out), 
                    decreasing = FALSE)
    saveRDS(var.imp, file = outfile)
  } else {
    var.imp <- readRDS(outfile)
  }
  
  var.imp <- data.frame(var.imp)
  var.imp$Variable <- rownames(var.imp)
  return(var.imp)
}
