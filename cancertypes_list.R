cancer types studied in TCGA project:

'Adrenal', 'Paraganglioma', 'Pheochromocytoma', 'Adrenocortical', 'Adenoid'
'Breast'
'Bladder', 'urothelial'
'Blood', 'Myeloid', 'Leukemia', 'Lymphoma'
'Brain', 'Glioblastoma', 'Medulloblastoma', 'Oligodendroglioma'
'Colorectal', 'Rectal'
'Rsophagael','Esophagogastric'
'Gallbladder'
'Head'
'Histiocytosis'
'Kidney', 'Renal'
'Liver', 'Hepatocellular'
'Lung', 'Pan-lung'
'Mesothelioma'
'Pancreatic'
'Prostate'
'Sarcoma'
'Skin', 'Melanoma'
'Stomach'
'Thyroid', 'Thymoma'
'Testicular'
'Urothelial'
'Uterine', 'Cervical', 'Ovarian', 'Endometrial'
"Wilms'"

# Prepare ifelse argument

ifelse(any(str_detect(study, c('Adrenal', 'Paraganglioma', 'Pheochromocytoma', 'Adrenocortical', 'Adenoid')),  TRUE), "Adrenal gland",
             ifelse(str_detect(study, 'Breast'), "Breast", 
            ifelse(any(str_detect(study, c('Bladder', 'Urothelial')), TRUE), "Urethary",
            ifelse(any(str_detect(study, c('Blood', 'Myeloid', 'Leukemia', 'Lymphoma')), TRUE), 'Blood',
            ifelse(any(str_detect(study, c('Brain', 'Glioblastoma', 'Medulloblastoma', 'Oligodendroglioma')), TRUE), "Nervous",
            ifelse(any(str_detect(study, c('Colorectal', 'Rectal')), TRUE),'Colon',
            ifelse(any(str_detect(study, c('Esophagael','Esophagogastric')), TRUE), 'Esophagael',
            ifelse(str_detect(study, 'Gallbladder'), 'Gallbladder',
            ifelse(str_detect(study, 'Head'), 'Head and Neck',
            ifelse(str_detect(study, 'Histiocytosis'),'Histiocytosis',
              ifelse(any(str_detect(study, c('Kidney', 'Renal')), TRUE), 'Kidney',
                ifelse(any(str_detect(study, c('Liver', 'Hepatocellular')), TRUE), 'Liver',
                  ifelse(any(str_detect(study, c('Lung', 'Pan-lung')), TRUE), 'Lung', 
                    ifelse(str_detect(study, 'Mesothelioma'), 'Mesothelioma',
                      ifelse(str_detect(study, 'Pancreatic'), 'Pancreatic',
                        ifelse(str_detect(study, 'Sarcoma'),'Sarcoma',
                      ifelse(any(str_detect(study, c('Skin', 'Melanoma')), TRUE), 'Skin',
                     ifelse(str_detect(study, 'Stomach'), 'Stomach',
                    ifelse(any(str_detect(study, c('Thyroid', 'Thymoma')), TRUE), 'Thyroid',
                  ifelse(str_detect(study, 'Testicular'),'Testicular',
              ifelse(any(str_detect(study, c('Uterine', 'Cervical', 'Ovarian', 'Endometrial')), TRUE), 'Reproductory',
            ifelse(str_detect(study, "Wilms'"), "Wilms'", "Other") ))))))))))))))))))))))
 
            
            
mutate(type = ifelse(str_detect(study, 'Adrenal'), "Adrenal gland",
ifelse(str_detect(study,'Breast'), "Breast", 0))

ifelse(str_detect(study, 'Pancreatic'), 'Pancreatic', ifelse(str_detect(study, 'Sarcoma'),'Sarcoma')

# Working 

mycall5 <- mycall4 %>% mutate(type =ifelse(str_detect(study, c('Adrenal', 'Lung')),1, 
                                           ifelse(str_detect(study, "Liver"), 2, 
                                           ifelse(str_detect(study, 'Prostate'),3,1)   )))

# Also Working 

mycall5 <- mycall4 %>% mutate(type = ifelse(str_detect(study, c('Adrenal', 'Paraganglioma', 'Pheochromocytoma', 'Adrenocortical', 'Adenoid')), "Adrenal gland",
                                            ifelse(str_detect(study, 'Breast'), "Breast", 
                                                   ifelse(str_detect(study, c('Bladder', 'Urothelial')), "Urethary",
                                                          ifelse(str_detect(study, c('Blood', 'Myeloid', 'Leukemia', 'Lymphoma')), 'Blood',
                                                                 ifelse(str_detect(study, c('Brain', 'Glioblastoma', 'Medulloblastoma', 'Oligodendroglioma')), "Nervous",
                                                                        ifelse(str_detect(study, c('Colorectal', 'Rectal')), 'Colon',
                                                                               ifelse(str_detect(study, c('Esophagael','Esophagogastric')), 'Esophagael',
                                                                                      ifelse(str_detect(study, 'Gallbladder'), 'Gallbladder',
                                                                                             ifelse(str_detect(study, 'Head'), 'Head and Neck',
                                                                                                    ifelse(str_detect(study, 'Histiocytosis'),'Histiocytosis',
                                                                                                           ifelse(str_detect(study, c('Kidney', 'Renal')), 'Kidney',
                                                                                                                  ifelse(str_detect(study, c('Liver', 'Hepatocellular')), 'Liver',
                                                                                                                         ifelse(str_detect(study, c('Lung', 'Pan-lung')), 'Lung',
                                                                                                                                ifelse(str_detect(study, 'Mesothelioma'), 'Mesothelioma',
                                                                                                                                       ifelse(str_detect(study, 'Pancreatic'), 'Pancreatic',
                                                                                                                                              ifelse(str_detect(study, 'Sarcoma'),'Sarcoma',
                                                                                                                                                     ifelse(str_detect(study, c('Skin', 'Melanoma')), 'Skin',
                                                                                                                                                            ifelse(str_detect(study, 'Stomach'), 'Stomach',
                                                                                                                                                                   ifelse(str_detect(study, c('Thyroid', 'Thymoma')), 'Thyroid',
                                                                                                                                                                          ifelse(str_detect(study, 'Testicular'),'Testicular',
                                                                                                                                                                                 ifelse(str_detect(study, c('Uterine', 'Cervical', 'Ovarian', 'Endometrial')), 'Reproductory',
                                                                                                                                                                                        ifelse(str_detect(study, "Wilms'"), "Wilms'", "Other") ))))))))))))))))))))))
# Other

Pediatric Preclinical Testing Consortium
Pediatric Pan-Cancer
Pediatric Acute Lymphoid Leukemia
Metastatic Solid Cancers
MSS Mixed Solid Tumors (Broad/Dana-Farber
MSK-IMPACT Clinical Sequencing Cohort (MS
Cancer Cell Line Encyclopedia (Novartis/Broad, Nature 2012)                                    
                                       
                                       