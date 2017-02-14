source("dftFUNs.R")
source("corFUNs.R")

# Load the dataset, adjust, and convert to monthly returns
set.seed(42)
library(quantmod)
getSymbols('^GSPC', from='1990-01-01')
GSPC_0 <- adjustOHLC(GSPC, symbol.name='^GSPC')

# Daily
Target <- ClCl(GSPC_0)

tgtDFT <- dft(as.numeric(Target)[-1])
tgtSPEC <- dftPower(as.numeric(Target)[-1], make_percent = TRUE)
plot(tgtSPEC)
plot(tgtSPEC, xtype = "period")

ord_tgtSPEC <- dftPower(as.numeric(Target)[-1], 
                        make_percent = TRUE,
                        make_order = TRUE)
plot(tgtSPEC, thr = 0.15)
plot(tgtSPEC, xtype = "period", thr = 0.15)


sum(ord_tgtSPEC$spectrum[ord_tgtSPEC$dft$T_ks >= 0 & ord_tgtSPEC$dft$T_ks < 1])
sum(ord_tgtSPEC$spectrum[ord_tgtSPEC$dft$T_ks >= 1 & ord_tgtSPEC$dft$T_ks < 2])
sum(ord_tgtSPEC$spectrum[ord_tgtSPEC$dft$T_ks >= 2 & ord_tgtSPEC$dft$T_ks < 3])
sum(ord_tgtSPEC$spectrum[ord_tgtSPEC$dft$T_ks >= 3 & ord_tgtSPEC$dft$T_ks < 4])
sum(ord_tgtSPEC$spectrum[ord_tgtSPEC$dft$T_ks >= 4 & ord_tgtSPEC$dft$T_ks < 5])
sum(ord_tgtSPEC$spectrum[ord_tgtSPEC$dft$T_ks >= 5 & ord_tgtSPEC$dft$T_ks < 6])
sum(ord_tgtSPEC$spectrum[ord_tgtSPEC$dft$T_ks >= 6 & ord_tgtSPEC$dft$T_ks < 7])
sum(ord_tgtSPEC$spectrum[ord_tgtSPEC$dft$T_ks >= 7 & ord_tgtSPEC$dft$T_ks < 8])

# Weekly
GSPC <- to.weekly(GSPC_0, indexAt='lastof')
Target <- ClCl(GSPC)

tgtDFT <- dft(as.numeric(Target)[-1])
tgtSPEC <- dftPower(as.numeric(Target)[-1], make_percent = TRUE)
plot(tgtSPEC)
plot(tgtSPEC, xtype = "period")

ord_tgtSPEC <- dftPower(as.numeric(Target)[-1], 
                        make_percent = TRUE,
                        make_order = TRUE)
plot(tgtSPEC, thr = 0.6)
plot(tgtSPEC, xtype = "period", thr = 0.6)

ord_tgtSPEC$spectrum[ord_tgtSPEC$spectrum > 0.6]
ord_tgtSPEC$dft$T_ks[ord_tgtSPEC$spectrum > 0.6]

sum(ord_tgtSPEC$spectrum[ord_tgtSPEC$dft$T_ks >= 0 & ord_tgtSPEC$dft$T_ks < 1])
sum(ord_tgtSPEC$spectrum[ord_tgtSPEC$dft$T_ks >= 1 & ord_tgtSPEC$dft$T_ks < 2])
sum(ord_tgtSPEC$spectrum[ord_tgtSPEC$dft$T_ks >= 2 & ord_tgtSPEC$dft$T_ks < 3])
sum(ord_tgtSPEC$spectrum[ord_tgtSPEC$dft$T_ks >= 3 & ord_tgtSPEC$dft$T_ks < 4])
sum(ord_tgtSPEC$spectrum[ord_tgtSPEC$dft$T_ks >= 4 & ord_tgtSPEC$dft$T_ks < 5])
sum(ord_tgtSPEC$spectrum[ord_tgtSPEC$dft$T_ks >= 5 & ord_tgtSPEC$dft$T_ks < 6])
sum(ord_tgtSPEC$spectrum[ord_tgtSPEC$dft$T_ks >= 6 & ord_tgtSPEC$dft$T_ks < 7])
sum(ord_tgtSPEC$spectrum[ord_tgtSPEC$dft$T_ks >= 7 & ord_tgtSPEC$dft$T_ks < 8])
# Monthly
GSPC <- to.monthly(GSPC_0, indexAt='lastof')
Target <- ClCl(GSPC)


tgtDFT <- dft(as.numeric(Target)[-1])
tgtSPEC <- dftPower(as.numeric(Target)[-1], make_percent = TRUE)
plot(tgtSPEC)
plot(tgtSPEC, xtype = "period")

ord_tgtSPEC <- dftPower(as.numeric(Target)[-1], 
                        make_percent = TRUE,
                        make_order = TRUE)
plot(tgtSPEC, thr = 1.5)
plot(tgtSPEC, xtype = "period", thr = 1.5)

ord_tgtSPEC$spectrum[ord_tgtSPEC$spectrum > 1.5]
ord_tgtSPEC$dft$T_ks[ord_tgtSPEC$spectrum > 1.5]

sum(ord_tgtSPEC$spectrum[ord_tgtSPEC$dft$T_ks >= 0 & ord_tgtSPEC$dft$T_ks < 1])
sum(ord_tgtSPEC$spectrum[ord_tgtSPEC$dft$T_ks >= 1 & ord_tgtSPEC$dft$T_ks < 2])
sum(ord_tgtSPEC$spectrum[ord_tgtSPEC$dft$T_ks >= 2 & ord_tgtSPEC$dft$T_ks < 3])
sum(ord_tgtSPEC$spectrum[ord_tgtSPEC$dft$T_ks >= 3 & ord_tgtSPEC$dft$T_ks < 4])
sum(ord_tgtSPEC$spectrum[ord_tgtSPEC$dft$T_ks >= 4 & ord_tgtSPEC$dft$T_ks < 5])
sum(ord_tgtSPEC$spectrum[ord_tgtSPEC$dft$T_ks >= 5 & ord_tgtSPEC$dft$T_ks < 6])
sum(ord_tgtSPEC$spectrum[ord_tgtSPEC$dft$T_ks >= 6 & ord_tgtSPEC$dft$T_ks < 7])
sum(ord_tgtSPEC$spectrum[ord_tgtSPEC$dft$T_ks >= 7 & ord_tgtSPEC$dft$T_ks < 8])

