# This script should be called by server to load actual scripts
# based on the modules user selected

LoadScripts <- function(module.nm = "stat"){
    file.sources <- "";
    if(module.nm == "stat"){
        file.sources <- list.files(c("../../rscripts/general", "../../rscripts/stats"),full.names=TRUE, pattern=".Rc$");
    }else if(module.nm == "ts"){
        file.sources <- list.files(c("../../rscripts/general", "../../rscripts/time"),full.names=TRUE, pattern=".Rc$");
    }else if(substring(module.nm, 1,4) == "path" | substring(module.nm, 1,4) == "mset"| module.nm == "inmex"){
        file.sources <- list.files(c("../../rscripts/general", "../../rscripts/enrich_path"),full.names=TRUE, pattern=".Rc$");
    }else if(module.nm == "roc" | module.nm == "power"){
        file.sources <- list.files(c("../../rscripts/general", "../../rscripts/power_roc"),full.names=TRUE, pattern=".Rc$");
    }else if(module.nm == "utils"){
        file.sources <- list.files(c("../../rscripts/general", "../../rscripts/utils"),full.names=TRUE, pattern=".Rc$");
        file.sources <- c(file.sources, "../../rscripts/enrich_path/name_match.Rc");
    }else{
        print(paste("Unknown module code: ", module.nm));
    }
    #sapply(file.sources,source,.GlobalEnv);
    library(compiler);
    sapply(file.sources,loadcmp,.GlobalEnv);
}

CompileScripts <- function(){
    library(compiler);
    #print("compiling R codes .... ");
    all.folders <- c("general", "stats", "time", "enrich_path", "power_roc", "utils");
    for(i in 1:length(all.folders)){
        # Yang: change "../rscripts/" to "../../rscripts/"
        files <- list.files(paste("../../rscripts/", all.folders[i], sep=""),full.names=TRUE, pattern=".R$");
        print(paste(all.folders[i], " : ", files));
        for(f in files){
            cmpfile(f, paste(f, "c", sep=""), options=list(suppressAll=TRUE));
        }
    }
    return("TRUE");
}

LoadScriptsNoncompiled <- function(module.nm = "stat", generalPath = '', statsPath = ''){
  file.sources <- "";
  if(module.nm == "stat"){
    #file.sources <- list.files(c("../../rscripts/general", "../../rscripts/stats"),full.names=TRUE, pattern=".R$");
      #file.sources <- list.files(c("C:/apache-tomcat-7.0.47/webapps/MetaboAnalyst/resources/rscripts/general", "C:/apache-tomcat-7.0.47/webapps/MetaboAnalyst/resources/rscripts/stats"), full.names = TRUE, pattern = ".R$");
    file.sources <- list.files(c(generalPath, statsPath), full.names = TRUE, pattern = ".R$");
  }else if(module.nm == "ts"){
    file.sources <- list.files(c("../../rscripts/general", "../../rscripts/time"),full.names=TRUE, pattern=".R$");
  }else if(substring(module.nm, 1,4) == "path" | substring(module.nm, 1,4) == "mset"| module.nm == "inmex"){
    file.sources <- list.files(c("../../rscripts/general", "../../rscripts/enrich_path"),full.names=TRUE, pattern=".R$");
  }else if(module.nm == "roc" | module.nm == "power"){
    file.sources <- list.files(c("../../rscripts/general", "../../rscripts/power_roc"),full.names=TRUE, pattern=".R$");
  }else if(module.nm == "utils"){
    file.sources <- list.files(c("../../rscripts/general", "../../rscripts/utils"),full.names=TRUE, pattern=".R$");
    file.sources <- c(file.sources, "../../rscripts/enrich_path/name_match.Rc");
  }else{
    print(paste("Unknown module code: ", module.nm));
  }
  
  sapplyResult <- sapply(file.sources, source, simplify=T);
}