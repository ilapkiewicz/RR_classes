install.packages("xts")   # for working with time series
install.packages("quantmod") # for getting Yahoo finance data automatically
library(xts)
library(quantmod)

get_tickers <- function(x) {
  #first ticker group
  if(x==1){
    Healthcare_tickers <- getSymbols(c("BHP",	"LIN",	"BBL",	"VALE",	"RIO",	
                                       "CTA-PB",	"CTA-PA",	"APD",	"ECL",	"NEM",	
                                       "FCX",	"SCCO",	"DOW",	"DD",	"GOLD",	"PPG",	
                                       "CRH",	"LYB",	"IFF",	"NTR",	"CTVA",	"MT",	
                                       "NUE",	"FNV",	"PKX",	"SHW",	"VMC",	"MLM",	
                                       "WPM",	"CE",	"ALB",	"AEM",	"AVTR",	"SUZ",	"EMN",	
                                       "AVTR-PA",	"FMC",	"ACH",	"JHX",	"VEDL",	"MOS",	
                                       "SBSW",	"STLD",	"TECK",	"WLK",	"SID",	"CX",	"RPM",	
                                       "SMG",	"KL",	"SQM",	"CF",	"AU",	"GGB",	"GFI",	"RS",	
                                       "KGC",	"CLF",	"WFG",	"ICL",	"RGLD",	"AXTA",	"BAK",	
                                       "TX",	"OLN",	"PAAS",	"AA",	"MDU",	"X",	"HUN",	"EXP",
                                       "CC",	"ESI",	"ASH",	"AUY",	"UFPI",	"AVNT",	"MP",	"HL",	
                                       "GRA",	"UNVR",	"BCPC",	"KWR",	"SSRM",	"AG",	"CMC",	"NEU",	
                                       "SUM",	"NGVT",	"SXT",	"AGI",	"ZY",	"TRQ",	"CBT",	"FUL",	
                                       "WDFC",	"TROX",	"AMRS",	"HMY",	"UFS",	"SCL",	"BVN",	"MEOH",	
                                       "MTX",	"LTHM",	"CDE",	"BCC",	"TSE",	"IOSP",	"CSTM",	"CMP",	"OR",	
                                       "CGAU",	"PVG",	"PQG",	"EGO",	"HBM",	"KALU",	"EVA",	"KRO",	
                                       "GCP",	"FOE",	"DNMR",	"SAND",	"LAC",	"MTRN",	"NEXA",	"SCHN",	
                                       "SWM",	"SA",	"LOMA",	"CENX",	"FSM",	"OEC",	"RFP",	"MGPI",	
                                       "KRA",	"EXK",	"DRD",	"GPRE",	"MERC",	"NP",	"WLKP",	"USCR",	
                                       "LGO",	"PLL",	"HCC",	"ASIX",	"GATO",	"USLM",	"KOP",	"VHI",	
                                       "FF",	"TMST",	"CPAC",	"GLT",	"BIOX",	"UAN",	"SXC",	"VRS",	
                                       "AVD",	"TG",	"MSB",	"CLW",	"RYAM",	"IPI",	"ZEUS",	"HWKN",	
                                       "ODC",	"AMR",	"CINR",	"TREC",	"LXU",	"SPPP",	"USAP",	"CGA",	
                                       "GURE",	"IFFT"),
                                     from = "2020-01-01")
    
    Healthcare_tickers_xts <- BHP$BHP.Close
    counter <- length(Healthcare_tickers)
    for (ticker in Healthcare_tickers) {
      Object = get(paste0(ticker))
      Object = Object[, paste0(ticker,'.Close')]
      Healthcare_tickers_xts <- cbind(Healthcare_tickers_xts,Object)
      
    }
    Healthcare_tickers_xts$BHP.Close.1 <- NULL
    names <- colnames(Healthcare_tickers_xts)
    new_names <- 0
    for (i in names){
      name <- unlist(strsplit(i[1], split='.', fixed=TRUE))[1]
      new_names <- c(new_names,name)
    }
    new_names <- new_names[-(1:1)]
    colnames(Healthcare_tickers_xts) <- new_names
    return(Healthcare_tickers_xts)
    #second ticker group
  } else if (x==2) {
    BasicMaterials_tickers <- getSymbols(c("UFS",	"BVN",	"MEOH",	"MTX",	"BCC",	"LTHM",	"TSE",	"CDE",
                                           "IOSP",	"CSTM",	"CMP",	"OR",	"CGAU",	"PQG",	"PVG",	"KALU",
                                           "EGO",	"HBM",	"EVA",	"KRO",	"GCP",	"FOE",	"DNMR",	"SAND",
                                           "LAC",	"MTRN",	"SCHN",	"NEXA",	"SWM",	"SA",	"LOMA",	"RFP",
                                           "CENX",	"OEC",	"FSM",	"MGPI",	"KRA",	"EXK",	"HL-PB",	"DRD",
                                           "GPRE",	"MERC",	"WLKP",	"NP",	"USCR",	"LGO",	"ASIX",	"HCC",
                                           "GATO",	"USLM",	"KOP",	"VHI",	"FF",	"TMST",	"CPAC",	"GLT",
                                           "UAN",	"BIOX",	"SXC",	"VRS",	"AVD",	"TG",	"MSB",	"CLW",
                                           "RYAM",	"ZEUS",	"IPI",	"HWKN",	"ODC",	"AMR",	"CINR",
                                           "TREC",	"LXU",	"SPPP",	"USAP",	"CGA",	"GURE",	"IFFT",
                                           "BHP",	"LIN",	"BBL",	"VALE",	"RIO",	"CTA-PB",	"CTA-PA",
                                           "APD",	"ECL",	"FCX",	"NEM",	"SCCO",	"DOW",	"DD",	"GOLD",
                                           "PPG",	"CRH",	"LYB",	"IFF",	"NTR",	"CTVA",	"MT",	"NUE",
                                           "FNV",	"SHW",	"PKX",	"VMC",	"MLM",	"WPM",	"CE",	"ALB",
                                           "AVTR",	"AEM",	"EMN",	"SUZ",	"AVTR-PA",	"FMC",	"ACH",
                                           "JHX",	"VEDL",	"MOS",	"SBSW",	"STLD",	"WLK",	"TECK",	"SID",
                                           "CX",	"SMG",	"RPM",	"KL",	"CF",	"SQM",	"GGB",	"AU",	"RS",
                                           "GFI",	"KGC",	"CLF",	"WFG",	"ICL",	"RGLD",	"AXTA",	"TX",
                                           "BAK",	"OLN",	"PAAS",	"AA",	"MDU",	"X",	"HUN",	"EXP",
                                           "CC",	"ESI",	"ASH",	"AUY",	"UFPI",	"AVNT",	"MP",	"HL",
                                           "UNVR",	"GRA",	"BCPC",	"KWR",	"SSRM",	"AG",	"CMC",	"NEU",
                                           "SUM",	"SXT",	"NGVT",	"AGI",	"ZY",	"CBT",	"FUL",	"TRQ",
                                           "WDFC",	"AMRS",	"TROX",	"HMY",	"SCL"),
                                         from = "2020-01-01")
    
    BasicMaterials_tickers_xts <- UFS$UFS.Close
    counter <- length(BasicMaterials_tickers)
    for (ticker in BasicMaterials_tickers) {
      Object = get(paste0(ticker))
      Object = Object[, paste0(ticker,'.Close')]
      BasicMaterials_tickers_xts <- cbind(BasicMaterials_tickers_xts,Object)
      
    }
    BasicMaterials_tickers_xts$UFS.Close.1 <- NULL
    names <- colnames(BasicMaterials_tickers_xts)
    new_names <- 0
    for (i in names){
      name <- unlist(strsplit(i[1], split='.', fixed=TRUE))[1]
      new_names <- c(new_names,name)
    }
    new_names <- new_names[-(1:1)]
    colnames(BasicMaterials_tickers_xts) <- new_names
    return(BasicMaterials_tickers_xts)
  } else if (x==3) {
    
  }
}

tickers <- get_tickers(2)


install.packages("treemap")
library("treemap")
install.packages("RColorBrewer")
library(RColorBrewer)


group <- colnames(tickers)
value <- 0
value_for_vSize <- 0
weight <- 0
for (ticker in group ){
  #vector of percentage change for display
  number <- as.numeric(last(tickers[, paste0(ticker)])) / as.numeric(first(tickers[, paste0(ticker)]))
  number <- number * 100
  number <- round(number,digits=2)
  value_for_vSize <- c(value_for_vSize,number)
  number <- number -100
  value <- c(value,number)
  #vector of last value for size weight
  number <- as.numeric(last(tickers[, paste0(ticker)]))
  weight <- c(weight,number)
}
value_for_vSize <- value_for_vSize[-(1:1)]
value <- value[-(1:1)]
weight <- weight[-(1:1)]

data <- data.frame(group,value, weight, value_for_vSize)

data$label <- paste(data$group, data$value, sep =", %")
treemap(data,
        index=c("label"),
        vSize="value_for_vSize",
        vColor="value_for_vSize",
        type="value",
        palette = brewer.pal(n=10, "RdYlGn"),
        mapping = c(80, 100, 150)
)
