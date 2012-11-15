.catalog <- function(df) {
	catalog <- c(CCS_MID="LIBOR", DESC_IRS="TIIE", FIX="Fija",CD.CC="Directo Cuenta Corriente",
		CD.REFAC="Directo Refaccionario",CD.AVIO="Directo Avío",COM.REFAC="Comisionista Refaccionario",
		COM.AVIO="Comisionista Avío",REPORTO="Reporto",TOTAL="Total")
	factor <- which(sapply(df,is.factor))
	if(length(factor)>0) for(j in 1:length(factor)) df[,j] <- as.character(df[,j])	
	dfDim <- dim(df)
	for(i in 1:length(catalog)) {
		if (sum(df==names(catalog[i])) > 0) {
			index <- which(df==names(catalog[i]))
			for(k in index) {
				row <- k %% dfDim[1]
				if (row == 0) {
					row <- dfDim[1]
				}
				column <- floor(k / dfDim[1]) + 1
				if (k %% dfDim[1] == 0) {
					column <- column - 1
				}
				df[row, column] <- catalog[i]
			}
		}
	}
	
	if(length(factor)>0) for(j in 1:length(factor)) df[,j] <- as.factor(df[,j])	
	df
}

.select <- function(object, ...) {
# Funcion que selecciona un flujo (o varios) del portafolio.
# IN:   1- object. objeto de clase "rateCurveList" o de alguna clase que herede de esta
#       2- ... nombre y valor de los atributos con los que se quiere hacer la seleccion
# OUT: objeto de clase "xxxList" o "xxxCurve"
    args <- list(...)
    ret <- object
    names_args <- names(args)
    len_names_args <- length(names_args)
    len_list <- length(object)
    i <- 1
    while(i <= len_names_args & len_list >= 1) {
        a <- names_args[i]
        selection <- unlist(args[a])
        if (a == "date"){
            selection <- as.Date(selection, origin="1970-01-01")	

        }
        # Obtiene un arreglo con los nombres de las distintas curvas de la lista:
        ids <- getAttr(ret, a)
        index <- which(!is.na(match(ids, selection)))
        # Regresa un objeto que corresponde a una curva individual o a una lista de curvas.
        len_list <- length(index)
        if (len_list >= 1) {
            ret <- new(class(ret), ret[index])
        }else {
            ret <- list()
        }
        i <- i + 1
    }
    ret
}

.buckets <- function(x, y, y_breaks, ...) {
	if ( length(x)!=length(y) ) 
		stop("differing x and y lengths")
	len_y_breaks <- length(y_breaks)
	intervals_index <- findInterval(y, y_breaks, ...)
	intervals <- tapply(x, intervals_index, sum, na.rm=TRUE)
	intervals <- intervals[names(intervals) %in% (1:(len_y_breaks-1))]
	buckets <- rep(0, len_y_breaks - 1)
	buckets[as.numeric(names(intervals))] <- intervals
	buckets
}

#
setGeneric("select", function(object, date, ...) standardGeneric("select"))
setGeneric("buckets", function(object, breaks, ...) standardGeneric("buckets"))
setGeneric("getAttr",function(object, attr, ...) standardGeneric("getAttr"))
setGeneric("makeCashFlow", function(object, curve, ...) standardGeneric("makeCashFlow"))
setGeneric("makeCashFlowPortfolio", function(object, curve, ...) standardGeneric("makeCashFlowPortfolio"))


# cashFlow
setGeneric("as.cashFlow",function(object, curve, ...) standardGeneric("as.cashFlow"))
setGeneric("as.cashFlowPortfolio",function(object, curve, ...) standardGeneric("as.cashFlowPortfolio"))
setGeneric("presentValue", function(object, curves, valDate, exRate=1, ...) standardGeneric("presentValue"))
setGeneric("presentValueMultiple", function(object, curves, valDate, exRate=1, ...) standardGeneric("presentValueMultiple"))

# rates
setGeneric("fwdRate", function(object, ...) standardGeneric("fwdRate"))
setGeneric("as.spotRate", function(object, ...) standardGeneric("as.spotRate"))
setGeneric("as.spotRateList", function(object, ...) standardGeneric("as.spotRateList"))
setGeneric("shockCurveHist", function(object, baseCurve, ...) standardGeneric("shockCurveHist"))
setGeneric("matriz", function(object, ...) standardGeneric("matriz"))
setGeneric("yield", function(object, ...) standardGeneric("yield"))
setGeneric("fwdExRate", function(domesticCurve, foreignCurve, exRate, ...) standardGeneric("fwdExRate"))

# RiskMeasures
#
setGeneric("joinExposure", function(x, y, ...) standardGeneric("joinExposure"))
setGeneric("PLs", function(object, curves, valDate, fromDate, toDate, exRates, ...) standardGeneric("PLs"))

#
setGeneric("VaR", function(object, ...) standardGeneric("VaR"))
#
setGeneric("KRD", function(object, curves, valDate, exRate=1, ...) standardGeneric("KRD"))
setGeneric("linearApproximation", function(object, ...) standardGeneric("linearApproximation"))
setGeneric("sensitivity", function(object, ...) standardGeneric("sensitivity"))

#
setGeneric("exchangeRateKRD", function(object, curves, valDate, exRate=1, ...) standardGeneric("exchangeRateKRD"))
#
setGeneric("exposure", function(object, curves, valDate, exRate=1, breaks=c(7, 28, 91, 182, 364, 728, 1092, 1820, 3640, 5460), ...) standardGeneric("exposure"))
#
setGeneric("potentialLoss", function(object, curves, exRate, ...)  standardGeneric("potentialLoss"))
setGeneric("liquidityGap", function(valDate, object, breaks=c(7, 28, 91, 182, 364, 728, 1092, 1820, 3640, 5460), ...) standardGeneric("liquidityGap"))
setGeneric("liquidityGapFWD", function(valDate, object, curves, exRate, breaks=c(7, 28, 91, 182, 364, 728, 1092, 1820, 3640, 5460), ...) standardGeneric("liquidityGapFWD"))
setGeneric("liquidityGapFWDSensitivity", function(valDate, object, curves, exRate, breaks=c(7, 28, 91, 182, 364, 728, 1092, 1820, 3640, 5460), ...) standardGeneric("liquidityGapFWDSensitivity"))
setGeneric("cumLiquidityGap", function(object, ...) standardGeneric("cumLiquidityGap"))
setGeneric("diversification", function(object, ...) standardGeneric("diversification"))

# reporte
setGeneric("saveTex", function(x, ...) standardGeneric("saveTex"))






