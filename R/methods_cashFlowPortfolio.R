# TODO Aqui va validacion
.as.cashFlow.cashFlowPortfolio <- function(object) {
# Convierte un objeto de clase cashFlowPortfolio en uno de clase cashFlow.
# IN: 	object.- objecto de la clase cashFlowPortfolio.
# OUT: 	Objeto de clase cashFlow
	rate_type <- unique(getAttr(object, "rate_type"))
	currency <- unique(getAttr(object, "currency"))
	factor <- unique(getAttr(object, "factor"))
#	date <- unique(getAttr(object, "date"))
	asset <- unique(getAttr(object,"asset"))	
	
	if (length(rate_type) > 1) {
	 	stop("Object elements must have same rate_types")
	}
	if (length(currency) > 1) {
		stop("Object elements must have same currencies.")
	}
	if (length(factor) > 1) {
		stop("Object elements must have same factors.")
	}
	if (length(asset) > 1) {
		stop("Object elements must be assets or liabilities.")
	}
	
	# Determina la longitud de cada uno de los flujos y excluye aquellos cuya 
	# longitud == 0
	
	rates <- unlist(lapply(object, function(x) {
		if (length(x@rate) != 1) {
			ret <- x@rate
		} else {
			ret <- rep(x@rate, length(x))
		}
		ret
		}
	))
	
	spreads <- unlist(lapply(object, function(x) {
		if (length(x@spread) != 1) {
			ret <- x@spread
		} else {
			ret <- rep(x@spread, length(x))
		}
		ret
		}
	))
		
	cashFlowLen <- unlist(lapply(object, length))
	object <- new(class(object), object[which(cashFlowLen > 0)])
	cash_flow <- new("cashFlow",
		getAttr(object, ".Data"),
		interest_fix=getAttr(object, "interest_fix"),
		interest_variable=getAttr(object, "interest_variable"),
		asset=asset,
		period_ini_date=getAttr(object, "period_ini_date"),
		period_fin_date=getAttr(object, "period_fin_date"),
		pay_date=getAttr(object, "pay_date"),
		rate_type=rate_type,
		rate=rates,
		spread=spreads,
		currency=currency,
		capital=getAttr(object, "capital"),
		amortization=getAttr(object, "amortization"),
		factor=factor
	)
	cash_flow
}

.buckets.cashFlowPortfolio <- function(object, period_fin_date_breaks, ...) {
	add.bucketsList <- function(bucketsList) {
		sums <- function(x) {
			if (is.matrix(x)) {
				rowSums(x)
			} else {
				sum(x)
			}
		}
		len <- unique(sapply(bucketsList, function(x) length(x)))
		if ( length(len) > 1)
			stop("Not possible to add buckets of differing lengths")
		ret <- lapply(1:len, function(i) sums(sapply(bucketsList, function(x) {x[[i]]})))
		ret
	}
	
	bucketsList <- lapply(object, function(x) {buckets(x, period_fin_date_breaks, ...)})
	buckets <- add.bucketsList(bucketsList)
	names(buckets) <- c("cashFlow", "interest_fix", "interest_variable", "amortization")
  	buckets
}


setMethod("makeCashFlowPortfolio", signature(object="cashFlowPortfolio"),
	function(object, curve, valDate, fixedRateDate=NULL) {
    	cash_flows <- lapply(object, function(object) {makeCashFlow(object, curve, valDate, fixedRateDate) })
    	new("cashFlowPortfolio",cash_flows, name=object@name)
	}
)

setMethod("getAttr",signature(object="cashFlowPortfolio"), function(object, attr) {
	ret <- unlist(lapply(object, function(x, y) slot(x, y), y=attr))
	if (attr == "date"| attr == "period_ini_date" | attr == "period_fin_date" | attr == "pay_date") {
		ret <- as.Date(ret, origin="1970-01-01")
	}
	ret
})

setMethod("select", signature(object="cashFlowPortfolio", date="missing"), 
	function(object, ...) {
		subPort <- .select(object, ...)
		args <- list(...)
		subnames <- paste(args, collapse=".")
		if(length(subPort) > 0) {
			subPort@name <- paste(object@name, subnames, sep="_")
		}
		subPort
	}
)

setMethod("presentValue", signature(object="cashFlowPortfolio"),
	function(object, curves, valDate, exRate, byFlow=FALSE, ...) {
	# Calcula el valor presente de un objeto de clase cashFlowPortfolio.
	# IN: 	x.- objecto de la clase cashFlowPortfolio.
	# 		curves.- objeto de la clase discountRate o discountRateList.
	#		valDate.- fecha a la que se quiere obtener el valor presente.
	#		exRate.- objeto de la clase exchangeRate o exchangeRateList
	# OUT: 	valor presente del objeto cashFlow.
		res <- lapply(object, function(x) presentValue(x, curves, valDate, exRate, byFlow=byFlow, ...))
		res <- unlist(res)
		if (!byFlow){
			res <- sum(res)
		}
	#	ids <- getAttr(object, "id")
		res
	}
)

setMethod("as.cashFlow", signature(object="cashFlowPortfolio"), 
	function(object){
		.as.cashFlow.cashFlowPortfolio(object)
	}
)

setMethod("buckets", signature(object="cashFlowPortfolio"), 
	function(object, breaks, ...) {
		.buckets.cashFlowPortfolio(object, breaks, ...)
	}
)

setMethod("aggregate", signature(x="cashFlowPortfolio"), 
	function (x, ...) {
    	attr <- unique(data.frame(getAttr(x, "rate_type"), getAttr(x, "factor"), 
       	getAttr(x, "currency"), getAttr(x, "asset"), stringsAsFactors=FALSE))
    	names(attr) <- c("rate_type", "factor", "currency", "asset")
    	ret <- lapply(1:dim(attr)[1], function(i) as.cashFlow(select(x, rate_type=attr[i,1], 
			factor=attr[i,2], currency=attr[i,3], asset=attr[i,4])))
  		ret <- new("cashFlowPortfolio", ret, name=x@name)
    	ret
	}
)

# Risk Measures
setMethod("PLs", signature(object="cashFlowPortfolio", curves="list"),
	function(object, curves, valDate, fromDate, toDate, exRates=1, ...) {
		# Lo agrega por moneda, factor, asset
		object <- aggregate(object)
		curvesLen <- length(curves)
		# Variable donde se guardan los escenarios
		scenarios <- 0
		presentValue <- 0
		# Ciclo por cada uno de los factores en curves
		for(i in 1:curvesLen) {
			factor <- unique(getAttr(curves[[i]], "name"))
			subPortfolioFactor <- select(object, factor=factor)

   		# Si hay cashFlows con ese factor entonces ahora lo divide por moneda
   			if (length(subPortfolioFactor) > 0) {
					
					scenariosCurrency <- lapply(subPortfolioFactor, function(x) PLs(x, curves[[i]], valDate, fromDate, toDate, exRates, ...))
					scenarios <- scenarios + Reduce("+", scenariosCurrency)
					
					presentValue <- presentValue + sum(unlist(lapply(scenariosCurrency, function(x) x@presentValue)))
				
			}
		}
		scenarios@portfolio <- object@name
		scenarios@presentValue <- presentValue
		scenarios@factor <- unique(getAttr(object, "factor"))
		scenarios@currency <- unique(getAttr(object, "currency"))
		scenarios
	}
)

setMethod("PLs", signature(object="cashFlowPortfolio", curves="list", exRates="exchangeRateList"),
	function(object, curves, valDate, fromDate, toDate, exRates, ...) {
		# Lo agrega por moneda, factor, asset
		object <- aggregate(object)
		curvesLen <- length(curves)
		# Variable donde se guardan los escenarios
		presentValue <- 0
		scenarios <- 0
		# Ciclo por cada uno de los factores en curves
		for(i in 1:curvesLen) {
			factor <- unique(getAttr(curves[[i]], "name"))
			subPortfolioFactor <- select(object, factor=factor)

   			# Si hay cashFlows con ese factor entonces ahora lo divide por moneda
   			if (length(subPortfolioFactor) > 0) {
	   			# Para cada una de las distintas monedas del subportafolio calcula los PLs
	   			subCurrencies <- unique(getAttr(subPortfolioFactor, "currency"))
	   			for(k in 1:length(subCurrencies)) {
	   				subPortfolio <- select(subPortfolioFactor, currency=subCurrencies[k])					
					scenariosCurrency <- lapply(subPortfolio, function(x) PLs(x, curves[[i]], valDate, fromDate, toDate, exRates, ...))
						
					scenarios <- scenarios + Reduce("+", scenariosCurrency)
					
					presentValue <- presentValue + sum(unlist(lapply(scenariosCurrency, function(x) x@presentValue)))
				}
			}
		}
		scenarios@portfolio <- object@name
		scenarios@presentValue <- presentValue
		scenarios@factor <- unique(getAttr(object, "factor"))
		scenarios@currency <- unique(getAttr(object, "currency"))
		scenarios
	}
)

setMethod("KRD", signature(object="cashFlowPortfolio", curves="BNSpotRateList", exRate="exchangeRateList"), 
	function(object, curves, valDate, exRate, bps=1, delta=1/1000000, ...) {
		# Selecciona los factores de riesgo de object 
		riskFactors <- unique(getAttr(object, "factor"))
		
		# Obtiene el KRD para cada uno de los factores de riesgo
		KRDList <- list()
		KRD <- c()
		pvs <- c()
		for(j in 1:length(riskFactors)) {
			# Selecciona el subportafolio con el factor de riesgo j
			subPortfolio <- select(object, factor=riskFactors[j])
			# Selecciona la curva
			curve <- select(curves, name=riskFactors[j])[[1]]
			curvesLen <- length(curve)
			# Obtiene las derivadas parciales
			for(i in 1:curvesLen) {
 	  			curvesDelta <- curve
				# Convierte la curva a tasas equivalentes anuales, altera la tasa por delta y la convierte en spot
				curvesDelta[i] <- (((1 + curve[i] * curve@nodes[i] / curve@period / 100) ^ 
					(curve@period / curve@nodes[i]) + delta) ^ (curve@nodes[i] / curve@period) - 1) *  
					(curve@period / curve@nodes[i] * 100) 
				
#	   			curvesDelta[i] <- curvesDelta[i] + delta
	   			curvesSpot <- as.spotRate(curvesDelta)
				
	   			pvRight <- presentValue(subPortfolio, curvesSpot, valDate, exRate, ...)
				curvesDelta[i] <- (((1 + curve[i] * curve@nodes[i] / curve@period / 100) ^ 
					(curve@period / curve@nodes[i]) - delta) ^ (curve@nodes[i] / curve@period) - 1) *  
					(curve@period / curve@nodes[i] * 100)
					
#	   			curvesDelta[i] <- curvesDelta[i] - 2 * delta
	   			curvesSpot <- as.spotRate(curvesDelta)
	   			pvLeft <- presentValue(subPortfolio, curvesSpot, valDate, exRate, ...)
	   			KRD[i] <- (pvRight - pvLeft) / (2 * delta )
			}
			
			KRDList[[j]] <- KRD * bps / 10000
			names(KRDList[[j]]) <- curve@nodes
			# Obtiene el valor presente del portafolio con el factor de riesgo correspondiente
			pvs <- c(pvs, presentValue(subPortfolio, as.spotRate(curve), valDate, exRate, ...))
		}
		names <- riskFactors
		new("KRD", KRDList, portfolio=object@name, names=names, pv=pvs, bps=bps)
	}
)

#
setMethod("exchangeRateKRD", signature(object="cashFlowPortfolio", curves="spotRateList", exRate="exchangeRateList"), 
	function(object, curves, valDate, exRate, cents=1, delta=1/1000000, ...) {
		# Selecciona los factores de riesgo de object 
		riskFactorsCurrency <- unique(getAttr(object, "currency"))
		riskFactorsCurrency <- riskFactorsCurrency[riskFactorsCurrency!="MXN"]
		
		# Obtiene el KRD para cada uno de los factores de riesgo
		KRD <- c()
		pvs <- c()	
		# Si no hay ningun cashFlow con moneda distinta a MXN entonces no hay sensibilidad
		if (length(riskFactorsCurrency) == 0) {
			pvs <- presentValue(object, curves, valDate, exRate, ...)
			ret <- new("KRD", 0, portfolio=object@name, names="MXN", pv=pvs, bps=0)
		} else {
			for(i in 1:length(riskFactorsCurrency)) {
				subPortfolio <- select(object, currency=riskFactorsCurrency[i])
				pvSub <- presentValue(subPortfolio, curves, valDate, 1, ...)
				KRD[i] <- pvSub * cents / 100
				# Obtiene el valor presente de object
				pvs <- c(pvs, presentValue(subPortfolio, curves, valDate, exRate, ...))
			}
			ret <- new("KRD", list(KRD), portfolio=object@name, names=riskFactorsCurrency, pv=pvs, bps=cents)
		}
		ret
	}
)
#

setMethod("exposure", signature(object="cashFlowPortfolio", curves="spotRateList", exRate="exchangeRateList"), 
	function(object, curves, valDate, exRate, breaks, ...) {
		# Selecciona los factores de riesgo de object 
		aggregatedPortfolio <- aggregate(object)
		exposureObjects <- lapply(aggregatedPortfolio, function(x) exposure(x, curves, valDate, exRate, breaks, ...))
		exposures <- lapply(exposureObjects, function(x) x@.Data[[1]])
#		exposuresIntFix <- lapply(exposureObjects, function(x) x@interest_fix[[1]])
#		exposuresIntVar <- lapply(exposureObjects, function(x) x@interest_variable[[1]])
		
		names <- unlist(lapply(exposureObjects, function(x) x@names))
		pvs <- unlist(lapply(exposureObjects, function(x) x@pv))
		breaks <- exposureObjects[[1]]@breaks
		
		newExposures <- list()
#		newExposuresIntFix <- list()
#		newExposuresIntVar <- list()
		
		newNames <- c()
		newPvs <- c()
		newNames <- unique(names)
		for(i in newNames) {
			index <- which(names == i)
			aggExposures <- exposures[index]
#			aggExposures <- exposuresIntFix[index]
#			aggExposures <- exposuresIntVar[index]
					
			newExposures <- c(newExposures, list(as.numeric(unlist(Reduce("+", aggExposures)))))
#			newExposuresIntFix <- c(newExposuresIntFix, list(as.numeric(unlist(Reduce("+", aggExposures)))))
#			newExposuresIntVar <- c(newExposuresIntVar, list(as.numeric(unlist(Reduce("+", aggExposures)))))
			newPvs <- c(newPvs, sum(pvs[index]))
		}
		totalSum <- list(as.numeric(unlist(Reduce("+", exposures))))
		cumulative <- cumsum(totalSum[[1]])
#		totalSumIntFix <- list(as.numeric(unlist(Reduce("+", exposuresIntFix))))
#		totalSumIntVar <- list(as.numeric(unlist(Reduce("+", exposuresIntVar))))		
		# Obtiene el valor presente de object
		pv <- presentValue(object, curves, valDate, exRate, ...)
		new("exposure", c(totalSum, newExposures), cumulative=list(cumulative), portfolio=object@name, names=c("TOTAL", newNames), 
			pv=c(pv, newPvs), breaks=breaks)	
	}
)

#
setMethod("diversification", signature(object="cashFlowPortfolio"),
	function(object, curves, valDate, exRate, ...) {
		tipos <- unique(getAttr(object, "tipo"))
		pvs <- c()
		for(i in tipos) {
			subPort <- aggregate(select(object, tipo=i))
			pvs <- c(pvs, presentValue(subPort, curves, valDate, exRate, ... ))
		}
		totalPV <- sum(pvs)
		df <- data.frame(c("TOTAL", tipos), c(totalPV, pvs), round(abs(c(totalPV, pvs) / totalPV) * 100, 2))
		names(df) <- c("kind", "pv", "perc")
		df
	}
)

setMethod("potentialLoss", signature(object="cashFlowPortfolio", curves="spotRateList", exRate="exchangeRateList"),
	function(object, curves, exRate, valDate, bps=1, cents=1, type="parallel") {
		.potentialLoss(object, curves, exRate, valDate, bps=bps, cents=cents, type=type)
	}
)

setMethod("liquidityGap", signature(valDate="Date", object="cashFlowPortfolio"), 
	function(valDate, object, breaks, ...) {
		# Selecciona los factores de riesgo de object 
		aggregatedPortfolio <- aggregate(object)
		exposureObjects <- lapply(aggregatedPortfolio, function(x) liquidityGap(valDate, x, breaks=breaks, ...))
		liqGap <- lapply(exposureObjects, function(x) x@.Data[[1]])
		liqGapCum <- lapply(exposureObjects, function(x) x@cumulative[[1]])
		
		names <- unlist(lapply(exposureObjects, function(x) x@names))
		pvs <- unlist(lapply(exposureObjects, function(x) x@pv))
		breaks <- exposureObjects[[1]]@breaks
		
		cashFlowsCur <- list()
		cashFlowsCurCum <- list()
		
		currencies <- c()
		pvsCur <- c()
		namesCur <- unique(names)
		
		for(cur in namesCur) {
			index <- which(names == cur)
			# 
			gap <- liqGap[index]
			gapCum <- liqGapCum[index]
			cashFlowsCur <- c(cashFlowsCur, list(as.numeric(unlist(Reduce("+", gap)))))
			cashFlowsCurCum <- c(cashFlowsCurCum, list(as.numeric(unlist(Reduce("+", gapCum)))))
			currencies <- c(currencies, cur)
			pvsCur <- c(pvsCur, sum(pvs[index]))
		}
		new("exposure", c(cashFlowsCur), cumulative=c(cashFlowsCurCum), portfolio=object@name, names=namesCur, pv=pvsCur, breaks=breaks)	
	}
)

setMethod("liquidityGapFWD", signature(valDate="Date", object="cashFlowPortfolio", curves="spotRateList", exRate="exchangeRateList"), 
	function(valDate, object, curves, exRate, breaks, ...) {
		# Selecciona los factores de riesgo de object 
		aggregatedPortfolio <- aggregate(object)
		exposureObjects <- lapply(aggregatedPortfolio, function(x) liquidityGapFWD(valDate, x, curves, exRate, breaks=breaks, ...))
		liqGap <- lapply(exposureObjects, function(x) x@.Data[[1]])
		
#		liqGapIntFix <- lapply(exposureObjects, function(x) x@interest_fix[[1]])
#		liqGapIntVar <- lapply(exposureObjects, function(x) x@interest_variable[[1]])
		
		names <- unlist(lapply(exposureObjects, function(x) x@names))
		pvs <- unlist(lapply(exposureObjects, function(x) x@pv))
		breaks <- exposureObjects[[1]]@breaks
		
		cashFlowsCur <- list()
#		cashFlowsCurIntFix <- list()
#		cashFlowsCurIntVar <- list()
		
		currencies <- c()
		pvsCur <- c()
		namesCur <- unique(names)
		
		for(cur in namesCur) {
			index <- which(names == cur)
			gap <- liqGap[index]
#			gapIntFix <- liqGapIntFix[index]
#			gapIntVar <- liqGapIntVar[index]
			cashFlowsCur <- c(cashFlowsCur, list(a=as.numeric(unlist(Reduce("+", gap)))))
#			cashFlowsCurIntFix <- c(cashFlowsCurIntFix, list(a=as.numeric(unlist(Reduce("+", gapIntFix)))))
#			cashFlowsCurIntVar <- c(cashFlowsCurIntVar, list(a=as.numeric(unlist(Reduce("+", gapIntVar)))))
			currencies <- c(currencies, cur)
			pvsCur <- c(pvsCur, sum(pvs[index]))
		}
		totalSum <- list(as.numeric(unlist(Reduce("+", cashFlowsCur))))
		cumulative <- cumsum(totalSum[[1]])
		
		new("exposure", c(totalSum, cashFlowsCur), cumulative=list(cumulative), portfolio=object@name, names=c("TOTAL", namesCur), pv=c(sum(pvsCur), pvsCur), breaks=breaks)	
	}
)

setMethod("liquidityGapFWDSensitivity", signature(valDate="Date", object="cashFlowPortfolio", curves="spotRateList", exRate="exchangeRateList"), 
	function(valDate, object, curves, exRate, breaks, bps=1, fixedRateDate=NULL) {
		# Selecciona los factores de riesgo de object 
		aggregatedPortfolio <- aggregate(object)
		exposureObjects <- lapply(aggregatedPortfolio, function(x) liquidityGapFWDSensitivity(valDate, x, curves, exRate, breaks=breaks, bps=bps, fixedRateDate=fixedRateDate))
		liqGap <- lapply(exposureObjects, function(x) x@.Data[[1]])
		
#		liqGapIntFix <- lapply(exposureObjects, function(x) x@interest_fix[[1]])
#		liqGapIntVar <- lapply(exposureObjects, function(x) x@interest_variable[[1]])
		
		names <- unlist(lapply(exposureObjects, function(x) x@names))
		pvs <- unlist(lapply(exposureObjects, function(x) x@pv))
		breaks <- exposureObjects[[1]]@breaks
		
		cashFlowsCur <- list()
#		cashFlowsCurIntFix <- list()
#		cashFlowsCurIntVar <- list()
		
		currencies <- c()
		pvsCur <- c()
		namesCur <- unique(names)
		
		for(cur in namesCur) {
			index <- which(names == cur)
			gap <- liqGap[index]
#			gapIntFix <- liqGapIntFix[index]
#			gapIntVar <- liqGapIntVar[index]
			cashFlowsCur <- c(cashFlowsCur, list(a=as.numeric(unlist(Reduce("+", gap)))))
#			cashFlowsCurIntFix <- c(cashFlowsCurIntFix, list(a=as.numeric(unlist(Reduce("+", gapIntFix)))))
#			cashFlowsCurIntVar <- c(cashFlowsCurIntVar, list(a=as.numeric(unlist(Reduce("+", gapIntVar)))))
			currencies <- c(currencies, cur)
			pvsCur <- c(pvsCur, sum(pvs[index]))
		}
		totalSum <- list(as.numeric(unlist(Reduce("+", cashFlowsCur))))
		cumulative <- cumsum(totalSum[[1]])
		
		new("exposure", c(totalSum, cashFlowsCur), cumulative=list(cumulative), portfolio=object@name, names=c("TOTAL", namesCur), pv=c(sum(pvsCur), pvsCur), breaks=breaks)	
	}
)

setMethod("cumLiquidityGap", signature(object="cashFlowPortfolio"),
	function(object, fillDates=TRUE, plot=TRUE) {
		# Crea una lista con dataDFsFrames del acumulado por cada cashFlow
		dfList <- lapply(object, function(x) cumLiquidityGap(x, fillDates, plot=FALSE))
		dfListLen <- length(dfList)
		# Crea un solo DF con los elementos de la lista
		df <- data.frame()
		for (i in 1:dfListLen) {
			df <- rbind(df, dfList[[i]])
		}
		dfAll <- data.frame()
		for (i in unique(df$currency)) {
			dfCur <- df[df$currency == i,]
			if(fillDates) {
				minDate <- min(dfCur$date)
				maxDate <- max(dfCur$date)
				dates <- seq(minDate, maxDate, by=1)
				values <- rep(0, length(dates))
				dfCur <- rbind(dfCur, data.frame(date=dates, value=values, cumValue=0, currency=i))
			}
			dfCur <- aggregate(dfCur$value, by=list(dfCur$date), sum)
			names(dfCur) <- c("date", "value")
			dfCur <- dfCur[order(dfCur$date),]
			dfCur$cumValue <- cumsum(dfCur$value)
			dfCur$currency <- i
			dfAll <- rbind(dfAll, dfCur)
		}
		if (plot) {
			ret <- ggplot(dfAll, aes(x=date, y=cumValue)) + geom_step() + facet_wrap( ~currency, scales = "free_y", nrow=2)  + theme(axis.text.x=element_text(size=6), axis.text.y=element_text(size=6))
		} else {
			ret <- dfAll
		}
		ret
	}
)

setMethod("plot", signature(x="cashFlowPortfolio"), 
	function(x, y, ...) {
		argv <- list(...)
		lenX <- length(x)
		valDate <- as.Date(argv$valDate)
		if (length(argv) > 1) {
			intervals <- argv$breaks
		} else{
			intervals <- c(7, 28, 91, 182, 364, 728, 1092, 1820, 3640, 5460)
		}
		intervals <- valDate + c(0, intervals)
		intervalsLen <- length(intervals)
		
		dataF <- cumLiquidityGap(x, fillDates=FALSE, plot=FALSE)
		maxDate <- max(as.Date(dataF$date))

    	if(dim(dataF)[1] == 0) {
    		stop("There is nothing to plot")
    	} 
    	
    	dataF$ymin <- 0
    	dataF$ymax <- 0
    	dataF$ymin[dataF$value < 0] <- dataF$value[dataF$value < 0]
    	dataF$ymax[dataF$value >= 0] <- dataF$value[dataF$value >= 0]
    	
    	dataF <- dataF[dataF$date >= intervals[1] & dataF$date <= intervals[length(intervals)], ]
    	dataF <- dataF[dataF$date <= maxDate, ]
    	# Crea un data frame con los valores de los intereses fix, intereses variables 
    	# y amortizacion agregados
    	cur <- unique(getAttr(x, "currency"))
    	dataFBuckets <- data.frame()
		
    	for(i in cur) {
    		buckets <- buckets(new("cashFlowPortfolio", select(x,currency=i)), intervals)
			# Elimina los valores para las monedas que no tienen flujos
			if(sum(buckets$cashFlow) != 0) {
				dataFBuckets <- rbind(dataFBuckets, data.frame(value=buckets$cashFlow, f_ini=intervals[1:intervalsLen-1],f_fin=intervals[2:intervalsLen], type="Amortization", currency=i))
				# Hay que quitar los comentarios para permitir que se grafiquen las
				# partes correspondientes a intereses
#	    		dataFBuckets <- rbind(dataFBuckets, data.frame(value=buckets$interest_fix, f_ini=intervals[1:intervalsLen-1],f_fin=intervals[2:intervalsLen], type="Interest Fix", currency=i))
#	    		dataFBuckets <- rbind(dataFBuckets, data.frame(value=buckets$interest_variable, f_ini=intervals[1:intervalsLen-1],f_fin=intervals[2:intervalsLen], type="Interest Variable", currency=i))
			}
    	}
		
		
		dataFBuckets <- dataFBuckets[dataFBuckets$f_ini <= maxDate,]
    	# Graficacion
		
		dataF$panel <- "Bucket"
		dataFBuckets$panel <- "Bucket"
		dataF$currency <- as.character(dataF$currency)
		dataFBuckets$currency <- as.character(dataFBuckets$currency)
		
	
		# Completa el data frame agregando un acumulado de 0 para la fecha anterior al
		# dia de valuacion
		for(i in unique(dataF$currency)){
			dataF <- rbind(data.frame(date=valDate-1, value=0, cumValue=0, currency=i, ymin=0,ymax=0, panel="Bucket"),dataF)
		}
		
		dfAux <- data.frame(currency=(c(as.character(dataFBuckets$currency), as.character(dataF$currency), as.character(dataF$currency))),
			panel=c(rep("Bucket",length(dataFBuckets[,1])), rep("Bucket", length(dataF[,1])), rep("Acumulado", length(dataF[,1]))))
		
		# Escala d elos ejes (lo ideal es que esto se hiciera en alguna otra parte)
		dataF[,c("value","cumValue","ymin","ymax")] <- dataF[,c("value","cumValue","ymin","ymax")] / 1000
		dataFBuckets[,c("value")] <- dataFBuckets[,c("value")] / 1000
		
		# Graficacion
		# TickMarks: Ajusta para que no se amontonen
		tickMarks <- sort(c(unique(dataFBuckets$f_ini), max(dataFBuckets$f_fin)))
		i <- 2
		while(i < length(tickMarks)) {
			if (as.numeric(tickMarks[i] - tickMarks[i-1]) < 15 ) {
				tickMarks <- tickMarks[-i]
			}
			i <- i + 1
		}
		# Color condicional ifelse(x == 0, "red", "black")
	
		p <- ggplot(dfAux) + facet_wrap(panel~currency, scales = "free_y", nrow=2) 
		p <- p + geom_rect(aes(xmin=f_ini,xmax=f_fin, ymin=0,ymax=value), data=dataFBuckets, fill="steelblue", alpha=.75) #+ scale_fill_brewer(palette="Blues")
		p <- p + geom_linerange(aes(x=date, ymin=ymin, ymax=ymax), data=dataF)
		dataF$panel <- "Acumulado"
		p2 <- p + geom_step(aes(x=date, y=cumValue), data=dataF)
		p2 <- p2 + geom_hline(aes(yintercept=0), colour="red", data=dataF) 
		p2 <- p2 + scale_y_continuous("monto", labels=comma)
		p2 <- p2 + scale_x_date(breaks=tickMarks) 
#		p2 <- p2 + scale_fill_discrete("tipo")
		p2 <- p2 + theme(axis.text.x=element_text(size=6, angle=90), axis.text.y=element_text(size=6)) 
		p2 <- p2 + theme(axis.title.x=element_blank(), axis.title.y = element_text(size=9, angle=90))
		p2 + theme(legend.text=element_text(size=6), legend.title=element_text(size=9))
	}
)



