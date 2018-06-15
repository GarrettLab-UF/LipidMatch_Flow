#rm(list=ls(all=TRUE)) 

#posneg = "POS";
#posneg = "NEG";

#workdir <- "C:/aatemp_test/R_workdir/pos";
#workdir <- "C:/aatemp_test/R_workdir/neg";
#setwd(workdir);

#source("C:/apache-tomcat-7.0.47/webapps/MetaboAnalyst/resources/rscripts/_script_loader.R");

#CompileScripts();
#LoadScripts(module.nm = "stat");
#LoadScriptsNoncompiled(module.nm = "stat");

#################################################################################
# step Upload
#################################################################################
# Data Type: Peak intensity table => csvDataType = "pktable"
# Format: Samples in Columns (unpaired) => csvFormat = "colu"
csvDataType = "pktable";
csvFormat = "colu";

#csvFile = "C:/aatemp_test/Metaboanalyst/csv_files/Metaboanalyst_FOSTER_POS_exported2_cleaned.csv";  # 3 groups
#csvFile = "C:/aatemp_test/Metaboanalyst/csv_files/Metaboanalyst_Foster-neg-TG_cleaned.csv";  # 3 groups

#csvFile = "C:/aatemp_test/Metaboanalyst/csv_files/Metaboanalyst_Voils_POS.csv"; # 2 groups
#csvFile = "C:/aatemp_test/Metaboanalyst/csv_files/Metaboanalyst_Voils_NEG.csv"; # 2 groups

# java statLoader.handleCsvFileUpload => R : Read.TextData()

# install required libraries: Cairo, genefilter, ellipse, RJSONIO, pls, caret, e1071, siggenes, pheatmap
if ("Cairo" %in% rownames(installed.packages()) == FALSE) { install.packages("Cairo") }
if ("genefilter" %in% rownames(installed.packages()) == FALSE) {
    source("https://bioconductor.org/biocLite.R");
    biocLite("genefilter", suppressUpdates = TRUE);
}
if ("ellipse" %in% rownames(installed.packages()) == FALSE) { install.packages("ellipse") }
if ("RJSONIO" %in% rownames(installed.packages()) == FALSE) { install.packages("RJSONIO") }
if ("pls" %in% rownames(installed.packages()) == FALSE) { install.packages("pls") }
if ("caret" %in% rownames(installed.packages()) == FALSE) { install.packages("caret") }
if ("e1071" %in% rownames(installed.packages()) == FALSE) { install.packages("e1071") }
if ("siggenes" %in% rownames(installed.packages()) == FALSE) {
    source("https://bioconductor.org/biocLite.R");
    biocLite("siggenes", suppressUpdates = TRUE);
}
if ("pheatmap" %in% rownames(installed.packages()) == FALSE) { install.packages("pheatmap") }


InitDataObjects(dataType = "pktable", analType = "stat", paired = "FALSE");

#source("../general/misce_utils.R");
readTextDataResult <- Read.TextData(filePath = csvFile, format = "colu", lbl.type = "disc");
## readTextDataResult == 1 : upload successful
## readTextDataResult == 0 : upload failed

#errMsg <- GetErrMsg();
#print(errMsg);

#dataSet;

#################################################################################
# step Processing
#################################################################################

#########################################
# Step Processing : Pre-process
sanityResult <- SanityCheckData()

# must be 1
## sanityResult == 1 : successful;
## sanityResult == 0 : failed;

# display all message
dataSet$check.msg;

#########################################
# Step Processing : Data Check
# Need to click "Missing value imputation"
  # java procBean : imputeButton_action()
    # R : GetGroupNumber()
groupNum <- GetGroupNumber();
isMultipleGroup <- FALSE;
isMultipleGroup <- (groupNum > 2);
# If "Skip" button clicked, 
# java procBean : skipButton_action() => run R: ReplaceMin();
#####ReplaceMin();

#########################################
# step Processing : Missing Value
# In Step 1, At this time always using 80%, otber choices keep defaults
# Click "Process" button
  # java procBean : performMissingImpute() => R : RemoveMissingPercent(percent=0.8);
percentValue <- 0.8;
RemoveMissingPercent(percent=percentValue);
# In Step 2, always using "Replace by a small value (half of the minimum positive value in the original data)" 
# which value is "min". 
# R : ImputeVar(method="min");
methodValue <- "min";
rCommand <- ImputeVar(method = methodValue);
integChecked = TRUE;
dataNormed = FALSE;
isSmallSampleSizeValue <- (IsSmallSmplSize() == 1);
numProcFeature <- ncol(dataSet$proc);
nextStep <- NULL;
if (numProcFeature > 250) {
  nextStep <- "Data filter";
} else if (dataSet$type != "conc") {
  nextStep <- "Data filter";
} else {
  nextStep <- "Normalization";
}
nextStep;

#########################################
# step Processing : Data filter
# Should select "Relative standard deviation (RSD = SD/mean)" which filterOpt = "rsd"
# java procBean : filterButton_action() => R : FilterVariable(filter = "rsd")
filterOpt <- "rsd";
rCommand <- FilterVariable(filter = filterOpt);
# AFter this step, go to "Normalization"

#################################################################################
# step Normalization
#################################################################################

# Select the fillowing values
  # Sample normalization: Normalization by sum => normBean.rowNormOpt = "SumNorm"
  # Data transformation: Log transformation => normBean.transNormOpt = "LogNorm"
  # Data scaling: Auto scaling => normBean.scaleNormOpt = "AutoNorm"
# Click "Normalize" button
# java normBean : performDataNormalization() 
rowNorm <- "SumNorm";
transNorm <- "LogNorm";
scaleNorm <- "AutoNorm";
ref <-  "<Not set>";
includeRatio <- FALSE;
ratioNum <- 20;
ratioCMD <- "ratio=FALSE, ratioNum=20";
# must run the following 3 lines before run Normalization(), otherwise it will raise errors.
rCommand <- GetPrenormSmplNms(); 
rCommand <- GetPrenormClsNms();
rCommand <- GetPrenormFeatureNms();
rCommand <- Normalization(rowNorm = rowNorm, transNorm = transNorm, scaleNorm = scaleNorm, ref = ref, ratio = FALSE, ratioNum = 20);
#rCommand <- Normalization("SumNorm", "LogNorm", "AutoNorm", NULL, FALSE, 20);
# rCommand == 1 : successful; 
if (rCommand > 0) {
  imgName <- "norm";
  format <- "png";
  dpi <- 72;
  rCommand <- PlotNormSummary(imgName = imgName, format = format, dpi = dpi, width = NA);
  
  imgName <- "snorm";
  rCommand <- PlotSampleNormSummary(imgName = imgName, format = format, dpi = dpi, width = NA);
  
  dataNormed <- TRUE;
  normPerformed <- TRUE;
}

#################################################################################
# step Statistics
#################################################################################

#########################################
# Univariate Analysis

###### if not multiple group (2 groups only): Fold Change Analysis		T-tests		Volcano plot
if (!isMultipleGroup) {
  # === Fold Change Analysis
  # UniVarTests.InitUnpairedFC(this.sb, 2.0D, 0);
  rCommand <- FC.Anal.unpaired(fc.thresh = 2.0, cmp.type = 0);
  
  # UniVarTests.PlotFC(this.sb, this.sb.getCurrentImage("fc"), "png", 72);
  rCommand <- PlotFC(imgName = "fc", format = "png", dpi = 72, width = NA);
  
  # === T-tests
  # UniVarTests.performTtests(this.sb, "F", 0.05D, "FALSE", "TRUE");
    # Need to install R package: genefilter
    ## try http:// if https:// URLs are not supported
    # source("https://bioconductor.org/biocLite.R")
    # biocLite("genefilter")
  rCommand <- Ttests.Anal(nonpar = F, threshp = 0.05, paired = FALSE, equal.var = TRUE);
  
  # UniVarTests.PlotT(this.sb, this.sb.getCurrentImage("tt"), "png", 72);
  rCommand <- PlotTT(imgName = "tt", format = "png", dpi = 72, width = NA);
  
  # === Volcano plot
  # UniVarTests.performVolcano(this.sb, "FALSE", 2.0D, 0, 0.75D, "F", 0.1D, "TRUE");
  rCommand <- Volcano.Anal(paired = FALSE, fcthresh = 2.0, cmpType = 0, percent.thresh = 0.75, nonpar = F, threshp = 0.1, equal.var = TRUE);
  
  # UniVarTests.PlotVolcano(this.sb, this.sb.getCurrentImage("volcano"), "png", 72);
  rCommand <- PlotVolcano(imgName = "volcano", format = "png", dpi = 72, width = NA);
  
} else {
###### if muliple group: One-way Analysis of Variance (ANOVA)
    # UniVarTests.performANOVA(this.sb, "F", 0.05D, "fisher");
  rCommand <- ANOVA.Anal(nonpar = "F", thresh = 0.05, post.hoc = "fisher");
  ## it will take a while to run anova analysis
  # rCommand == 1, successful
  
  # UniVarTests.PlotAOV(this.sb, this.sb.getCurrentImage("aov"), "png", 72);
  rCommand <- PlotANOVA(imgName = "aov", format = "png", dpi = 72, width = NA);
  
}

#########################################
# Chemometrics Analysis

###### Principal Component Analysis (PCA)
# ChemoMetrics.InitPCA(this.sb);
rCommand <- PCA.Anal(); 

# ChemoMetrics.PlotPCAPairSummary(this.sb, this.sb.getCurrentImage("pca_pair"), "png", 72, 5);
rCommand <- PlotPCAPairSummary(imgName = "pca_pair", format = "png", dpi = 72, pc.num = 5);

# ChemoMetrics.PlotPCAScree(this.sb, this.sb.getCurrentImage("pca_scree"), "png", 72, 5);
rCommand <- PlotPCAScree(imgName = "pca_scree", format = "png", dpi = 72, width = NA, scree.num = 5);

# ChemoMetrics.PlotPCA2DScore(this.sb, this.sb.getCurrentImage("pca_score2d"), "png", 72, 1, 2, 0.95D, 1, 0);
rCommand <- PlotPCA2DScore(imgName = "pca_score2d", format = "png", dpi = 72, width = NA, pcx = 1, pcy = 2, reg = 0.95, show = 0, grey.scale = 0, posneg = posneg);

# ChemoMetrics.PlotPCALoading(this.sb, this.sb.getCurrentImage("pca_loading"), "png", 72, 1, 2, "scatter", 1);
rCommand <- PlotPCALoading(imgName = "pca_loading", format = "png", dpi = 72, width = NA, inx1 = 1, inx2 = 2, plotType = "scatter", lbl.feat = 1);

# ChemoMetrics.PlotPCABiplot(this.sb, this.sb.getCurrentImage("pca_biplot"), "png", 72, 1, 2);
rCommand <- PlotPCABiplot(imgName = "pca_biplot", format = "png", dpi = 72, width = NA, inx1 = 1, inx2 = 2);
  
# ChemoMetrics.PlotPCA3DScore(this.sb, this.sb.getCurrentImage("pca_score3d"), "json", 1, 2, 3);
rCommand <- PlotPCA3DScore(imgName = "pca_score3d", format = "json", inx1 = 1, inx2 = 2, inx3 = 3);

###### Partial Least Squares - Discriminant Analysis (PLS-DA)

# ChemoMetrics.InitPLS(this.sb);
rCommand <-  PLSR.Anal();

# ChemoMetrics.PlotPLSPairSummary(this.sb, this.sb.getCurrentImage("pls_pair"), "png", 72, ChemoMetrics.GetDefaultPLSPairNumber(this.sb));
pcNum <- GetDefaultPLSPairComp();
rCommand <- PlotPLSPairSummary(imgName = "pls_pair", format = "png", dpi = 72, width = NA, pc.num = pcNum);

# ChemoMetrics.PlotPLS2DScore(this.sb, this.sb.getCurrentImage("pls_score2d"), "png", 72, 1, 2, 0.95D, 1, 0);
### NOT "Display sample names" : show = 0
rCommand <- PlotPLS2DScore(imgName = "pls_score2d", format = "png", dpi = 72, width = NA, inx1 = 1, inx2 = 2, reg = 0.95, show = 0, grey.scale = 0, use.sparse = FALSE, posneg = posneg);

# ChemoMetrics.PlotPLS3DScore(this.sb, this.sb.getCurrentImage("pls_score3d"), "json", 1, 2, 3);
rCommand <- PlotPLS3DScore(imgName = "pls_score3d", format = "json", inx1 = 1, inx2 = 2, inx3 = 3);

# ChemoMetrics.PlotPLSLoading(this.sb, this.sb.getCurrentImage("pls_loading"), "png", 72, 1, 2, "scatter", 1);
rCommand <- PlotPLSLoading(imgName = "pls_loading", format = "png", dpi = 72, width = NA, inx1 = 1, inx2 = 2, plotType = "scatter", lbl.feat = 1);

cvMethod <- "T";
# int minSize = RDataUtils.getMinGroupSize(this.sb.getRConnection());
minSize <- GetMinGroupSize();
if (minSize < 11) {
  cvMethod <-  "L";
}
#ChemoMetrics.TrainPLSClassifier(this.sb, cvMethod, ChemoMetrics.GetDefaultPLSCVNumber(this.sb), "Q2");
compNum <- GetDefaultPLSCVComp();
rCommand <- PLSDA.CV(methodName = cvMethod, compNum = compNum, choice = "Q2");

#ChemoMetrics.PlotPLSClassification(this.sb, this.sb.getCurrentImage("pls_cv"), "png", 72);
rCommand <- PlotPLS.Classification(imgName = "pls_cv", format = "png", dpi = 72, width = NA);

#ChemoMetrics.PlotPLSImp(this.sb, this.sb.getCurrentImage("pls_imp"), "png", 72, "vip", "Comp. 1", 15, "FALSE");
rCommand <- PlotPLS.Imp(imgName = "pls_imp", format = "png", dpi = 72, width = NA, type = "vip", feat.nm = "Comp. 1", feat.num = 15, color.BW = FALSE, posneg = posneg);

# === Permutation: permStat = bw , permNum = 100
permMsg <- PLSDA.Permut(num = 100, type = "bw"); # There were 50 or more warnings (use warnings() to see the first 50)
#ChemoMetrics.PlotPLSPermutation(this.sb, this.sb.getNewImage("pls_perm"), "png", 72);
rCommand <- PlotPLS.Permutation(imgName = "pls_perm", format = "png", dpi = 72, width = NA, posneg = posneg);

# === 2D Scores Plot : uncheck "Display sample names" checkbox
# ChemoMetrics.PlotPLS2DScore(this.sb, this.sb.getNewImage("pls_score2d"), "png", 72, this.plsScore2dX, this.plsScore2dY, conf, this.displayNames ? 1 : 0, useGreyScale);
#plsScore2dX <- 1;
#plsScore2dY <- 2;
#conf <- 0.95;
#displayNames <- 0; # NOT display sample names
#useGreyScale <- 0;
#rCommand <- PlotPLS2DScore();

###### Orthogonal Partial Least Squares - Discriminant Analysis (orthoPLS-DA)

#ChemoMetrics.InitOPLS(this.sb);
rCommand <- OPLSR.Anal();

# ChemoMetrics.PlotOPLS2DScore(this.sb, this.sb.getCurrentImage("opls_score2d"), "png", 72, 1, 2, 0.95D, 1, 0);
# NOT display sample names: show = 0
rCommand <- PlotOPLS2DScore(imgName = "opls_score2d", format = "png", dpi = 72, width = NA, inx1 = 1, inx2 = 2, reg = 0.95, show = 0, grey.scale = 0);
  
#ChemoMetrics.PlotOplsSplot(this.sb, this.sb.getCurrentImage("opls_splot"), "png", 72, "all");
rCommand <- PlotOPLS.Splot(imgName = "opls_splot", format = "png", dpi = 72, width = NA, plotType = "all");

#ChemoMetrics.PlotOplsMdlView(this.sb, this.sb.getCurrentImage("opls_mdl"), "png", 72);
rCommand <- PlotOPLS.MDL(imgName = "opls_mdl", format = "png", dpi = 72, width = NA);

# ==== Permutation : permNum = 100
#this.permMsg = ChemoMetrics.PlotOPLSPermutation(this.sb, this.sb.getNewImage("opls_perm"), "png", 72, this.permNum);
permMsg <- PlotOPLS.Permutation(imgName = "opls_perm", format = "png", dpi = 72, num = 100, width = NA);

#########################################
# Feature Identification

###### Significance Analysis of Microarray (and Metabolites) (SAM)

# SigVarSelect.InitSAM(this.sb, "d.stat", "FALSE", "TRUE");
  ## Need to install package "siggenes". Run the following command lines
  ## > source("https://bioconductor.org/biocLite.R")
  ## > biocLite("siggenes")
rCommand <- SAM.Anal(method = "d.stat", paired = FALSE, varequal = TRUE);

# double delta = SigVarSelect.GetSAMSuggestedDelta(this.sb);
delta <- GetSuggestedSAMDelta();

# SigVarSelect.PlotSAM_FDR(this.sb, delta, this.sb.getCurrentImage("sam_view"), "png", 72);
rCommand <- PlotSAM.FDR(delta = delta, imgName = "sam_view", format = "png", dpi = 72, width = NA);

# SigVarSelect.PlotSAM_Cmpd(this.sb, this.sb.getCurrentImage("sam_imp"), "png", 72, delta);
rCommand <- SetSAMSigMat(delta = delta);
rCommand2 <- PlotSAM.Cmpd(imgName = "sam_imp", format = "png", dpi = 72, width = NA);

#########################################
# Cluster Analysis

#### Hierarchical Clustering:		Dendrogram		Heatmaps
# === Dendrogram
# Clustering.PlotClustTree(this.sb, this.sb.getCurrentImage("tree"), "png", 72, "euclidean", "ward.D");
rCommand  <- PlotHCTree(imgName = "tree", format = "png", dpi = 72, width = NA, smplDist = "euclidean", clstDist = "ward.D");

# === Heatmaps
#if (this.useTopFeature) {
#  Clustering.PlotSubHeatMap(this.sb, this.sb.getNewImage("heatmap"), "png", 72, this.dataOpt, this.scaleOpt, this.hmDistOpt, this.hmMethodOpt, this.hmColorOpt, this.selectMethodOpt, this.topThresh, this.viewOpt, rowV, colV, this.drawBorders ? "T" : "F");
#  Clustering.PlotSubHeatMap(this.sb, this.sb.getNewImage("heatmap"), "png", 72, this.dataOpt, this.scaleOpt, this.hmDistOpt, this.hmMethodOpt, this.hmColorOpt, this.selectMethodOpt, this.topThresh, this.viewOpt, rowV, colV, this.drawBorders ? "T" : "F");

if (isMultipleGroup) {
    rCommand <- PlotSubHeatMap(imgName = "heatmap", format="png", dpi=72, width=NA, dataOpt="norm", scaleOpt="row", smplDist="euclidean", clstDist="ward.D", palette="bwm", method.nm="tanova", top.num=50, viewOpt="overview",  rowV=T, colV=T, border=T, posneg = posneg);
} else {
    rCommand <- PlotSubHeatMap(imgName = "heatmap", format="png", dpi=72, width=NA, dataOpt="norm", scaleOpt="row", smplDist="euclidean", clstDist="ward.D", palette="bwm", method.nm="vip", top.num=50, viewOpt="overview",  rowV=T, colV=T, border=T, posneg = posneg);
}

#} else {
#  Clustering.PlotHeatMap(this.sb, this.sb.getNewImage("heatmap"), "png", 72, this.dataOpt, this.scaleOpt, this.hmDistOpt, this.hmMethodOpt, this.hmColorOpt, this.viewOpt, rowV, colV, this.drawBorders ? "T" : "F");
#   Clustering.PlotHeatMap(this.sb, this.sb.getCurrentImage("heatmap"), "png", 72, "norm", "row", "euclidean", "ward.D", "bwm", "overview", "T", "T", "T");
  #rCommand <- PlotHeatMap(imgName = "heatmap", format="png", dpi=72, width=NA, dataOpt="norm", scaleOpt="row", smplDist="euclidean", clstDist="ward.D", palette="bwm", viewOpt="overview", rowV=T, colV=T, var.inx=NA, border=T)
#}


