
dep=c("rsconnect" ,"shiny","mlogit" ,"MASS" ,"AER" ,         
       "bayesm","MCMCpack",
       "abind","bayesboot","bibtex" ,      
       "BMA","car","carData","cellranger" ,"clipr",        
       "coda","DT","Formula","ivbma",    
       "Matrix" ,"matrixcalc","RcppEigen",    
       "Rdpack","rhandsontable" ,"statmod", "truncnorm"  ,  
       "stringr","renv")

needed=dep[!dep%in%installed.packages()]

if (length(needed)>0){
  for (p in needed){
    if (p=="ivbma"){
      install.packages("ivbma_1.05.tar.gz", repos = NULL, type = "source")
    }else if (p=="MCMCpack"){
      install.packages("MCMCpack_1.4-4.tar.gz", repos = NULL, type = "source")
    }else{
      install.packages(p)
    }
  }
}
