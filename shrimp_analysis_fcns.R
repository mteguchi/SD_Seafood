
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
                                             SHRIMP_PROCESSING2 == "WHOLE_RAW" ~ 4,
                                             TRUE ~ NA_real_)) %>%
    filter(PRICE < 95) -> shrimp.data
  
  return(shrimp.data)
}

