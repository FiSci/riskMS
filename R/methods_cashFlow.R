############################################################################
# Clase cashFlow.
############################################################################

.makeCashFlow.cashFlow <- function(object, curve, valDate, fixedRateDate) {
# Funcion que calcula los intereses fijos, intereses variables y flujos de un instrumento.
# INPUT:    object.- objeto de clase amortizationSchedule
#           curves.- curva o lista de curvas con que se calculan los intereses
#           valDate.- fecha de la valuacion
# OUTPUT:   objecto de clase cashFlow
    object_len <- length(object@period_ini_date)

    date <- valDate

	if (!is.null(fixedRateDate)) {
		if (fixedRateDate < valDate) {
			error("fixedRateDate < valDate")
		} else {
			date <- fixedRateDate
		}
	}

    # Indice de los flujos futuros:
    index_fwd <- which(object@period_ini_date > date)
    #Indice de los flujos pasados, es decir, los que ya son fijos:
    index_fix <- which(object@period_ini_date <= date)
    
    # Nodos necesarios para calcular las tasas forward:
    nodes_1 <- as.numeric(object@period_ini_date - as.Date(valDate))
    nodes_2 <- as.numeric(object@period_fin_date - as.Date(valDate))
    
    # Dias entre nodos:
    days_period <- as.numeric(object@period_fin_date - object@period_ini_date)
    
    capital <- object@capital
    
    # Calculo de los intereses para los instrumentos con tasa fija:
    if (object@rate_type == "FIX"){
        interest_fix <- object@capital * (object@rate + object@spread) / 100 * days_period / curve@period
        interest_variable <- rep(0, object_len)
#        new_object_rate <- object@rate
        # Calculo de los intereses para instrumentos con tasa variable:
    } else {
        # La parte fija. Incluye los flujos con fecha de inicio anterior al dia de valuacion
        # y el spread:
        rates_fix <- rep(0,object_len)
		
		# Estas condiciones discriminan entre flujos agregados, donde rate es un vector, y
		# flujos sin agregar, donde es un numero
		if (length(object@rate) == length(object@capital)) {
			rates_fix[index_fix] <- object@rate[index_fix]
		} else {
			rates_fix[index_fix] <- object@rate[1]
		}
        
        interest_fix <- capital * (rates_fix + object@spread) / 100 * days_period / curve@period
        
        # La parte variable, calculo con las tasas forward: 
        interest_variable <- rep(0,object_len)
        rate <- fwdRate(curve, nodes_1[index_fwd], nodes_2[index_fwd])
		if (sum(nodes_1[index_fwd] == nodes_2[index_fwd]) > 0 ) {
#			warning(paste("period_ini_date == period_fin_date in ID: ", object@id_credito))
		}
        interest_variable[index_fwd] <- capital[index_fwd] * rate * days_period[index_fwd] / curve@period
    }


	interest_fix[is.na(interest_fix)] <- 0
	interest_variable[is.na(interest_variable)] <- 0
	object@amortization[is.na(object@amortization)] <- 0
	
    cash_flow <- interest_fix + interest_variable + object@amortization
    object@interest_fix <- interest_fix
    object@interest_variable <- interest_variable
    object@.Data <- cash_flow
	object
}

.presentValueFast.cashFlow <- function(object, curve, valDate, exRate, spread=0, byFlow=FALSE) {
# Funcion que calcula el valor presente de un conjunto de flujos
# INPUT:	object.- objeto de clase cashFlow
#			curve.- curva o lista de curvas con que se valuara
#			valDate.- dia de valuacion
#			exRate.- tipo de cambio
# OUTPUT: valor presente del instrumento a la fecha dada
    discountFactor <- function(x) {
        d_factor <- 1 / (1 + (x * x@nodes) / x@period / 100)
        d_factor
    }
	# Esta seccion hace comprobaciones
 	discount_factor <- discountFactor(curve)

	# Calculo del valor presente:
    span_ini <- as.numeric(object@period_ini_date - as.Date(valDate, format="%Y-%m-%d"))
    span_fin <- as.numeric(object@pay_date - as.Date(valDate, format="%Y-%m-%d"))
    
    fwd_ef <- object@interest_variable/object@capital
    capital <- object@interest_variable/fwd_ef

#    capital <- object@capital
	
	fixPart <- (object@interest_fix + object@amortization) * discount_factor[match(span_fin, curve@nodes)]
	
    variablePart1 <- capital * discount_factor[match(span_ini, curve@nodes)]

	# Si la primera parte de interes variable corresponde a la fecha de inicio,
	# es como si fuera el nodo 0 de la curva y entonces debe ser igual al monto,
	# i.e. no se descuenta.
	if (is.na(variablePart1[1])) {
		variablePart1[1] <- capital[1]
	}
    variablePart2 <- -capital *discount_factor[match(span_fin, curve@nodes)]

    pv <- sum(c(variablePart1, variablePart2, fixPart), na.rm=TRUE)
    if (!object@asset) {
        pv <- -pv
    }
    
	# Conversion por el tipo de cambio
	pv * exRate	
}

.presentValue.cashFlow <- function(object, curve, valDate, exRate, spread=0, byFlow=FALSE, pastFlows=FALSE) {
# Funcion que calcula el valor presente de un conjunto de flujos
# INPUT:	object.- objeto de clase cashFlow
#			curve.- curva o lista de curvas con que se valuara
#			valDate.- dia de valuacion
#			exRate.- tipo de cambio
# OUTPUT: valor presente del instrumento a la fecha dada
    discountFactor <- function(x) {
        d_factor <- 1 / (1 + (x * x@nodes) / x@period / 100)
        d_factor
    }
	getCurveRate <- function(x) {
		newCurve <- curve[x]
		newCurve[is.na(newCurve)] <- 0
		newCurve
	}
	getDiscountFactor <- function(x, y) {
		newDFactor <- x[y]
		newDFactor[is.na(newDFactor)] <- 0
		newDFactor
	}
	
	# Esta seccion hace comprobaciones
 	discount_factor <- discountFactor(curve)
	discount_factorSpread <- discountFactor(curve + spread)

	# Calculo del valor presente:
    span_ini <- as.numeric(object@period_ini_date - as.Date(valDate, format="%Y-%m-%d"))
    span_fin <- as.numeric(object@pay_date - as.Date(valDate, format="%Y-%m-%d"))

#    fwd_ef <- object@interest_variable/object@capital
#   capital <- object@interest_variable/fwd_ef
	capital <- object@capital * object@interest_variable / object@interest_variable
	if (object@rate_type == "FIX") {
		capital <- 0
	}
	
	mIni <- match(span_ini, curve@nodes)
	mFin <- match(span_fin, curve@nodes)

	fixPart <- (object@interest_fix + object@amortization) * getDiscountFactor(discount_factorSpread, mFin)
	
    variablePart <- capital * (getCurveRate(mFin) * span_fin - getCurveRate(mIni) * span_ini) / 36000 *
				getDiscountFactor(discount_factorSpread, mFin) * getDiscountFactor(discount_factor, mIni)

	if (pastFlows) {
		ind <- which(span_fin <= 0)
		fixPart[ind] <- (object@interest_fix + object@amortization)[ind]
		variablePart[ind] <- (capital * (getCurveRate(mFin) * span_fin - getCurveRate(mIni) * span_ini) / 36000)[ind]
	}

	fixPart[is.na(fixPart)] <- 0
	variablePart[is.na(variablePart)] <- 0
	pv <- fixPart + variablePart
	
		
    if (!object@asset) {
        pv <- -pv
    }

	# Conversion por el tipo de cambio
	pv <- pv * exRate	

	if (!byFlow) {
		pv <- sum(pv)
	}
	pv
	
}

.presentValueFULL.cashFlow <- function(object, curve, valDate, exRate, byFlow=FALSE) {
	discountFactor <- function(x) {
        d_factor <- 1 / (1 + (x * x@nodes) / x@period / 100)
        d_factor
    }
	# Esta seccion hace comprobaciones

 	discount_factor <- discountFactor(curve)

	# Calculo del valor presente:
 #   span_ini <- as.numeric(object@period_ini_date - as.Date(valDate, format="%Y-%m-%d"))
    span_fin <- as.numeric(object@pay_date - as.Date(valDate, format="%Y-%m-%d"))
	pv <- object * discount_factor[match(span_fin, curve@nodes)]
	pv <- pv * exRate
	pv
}

.buckets.cashFlow <- function(object, period_fin_date_breaks, ...) {
	index <- which(!is.na(object@.Data))
	O <- list(object@.Data[index], object@interest_fix[index], object@interest_variable[index], object@amortization[index])
	if (!object@asset) {
		O <- lapply(O, function(x) -x)
	}
	buckets <- lapply(O, function(x) .buckets(x, as.numeric(object@pay_date[index]), as.numeric(period_fin_date_breaks), ...))
	names(buckets) <- c("cashFlow", "interest_fix", "interest_variable", "amortization")
	buckets
}

.pls <- function(object, baseCurve, shockedCurves, valDate, baseExRate=1, shockedExRates=1, ...) {
	# Esta parte de la funcion calcula el cambio en el valor presente 
	# de un cashFlow a cambios en la tasa
	presenValues <- unlist(lapply(shockedCurves, function(x) presentValue(object, x, valDate, 1, ...)))					
	presentValue <- presentValue(object, baseCurve, valDate, 1, ...)
	
	# Calcula los shocks en el tipo de cambio
	presenValues <- presenValues * shockedExRates
	presentValue <- presentValue * baseExRate
    
	PLs <- presentValue - presenValues
	PLs
}

.potentialLoss <- function(object, curves, exRate, valDate, bps=1, cents=1, type="parallel") {
	# Valor presente base
	pv <- presentValue(object, curves, valDate, exRate)
	# Calcula el valor presente con sensibilidades
	sensCurve <- sensitivity(curves, bps, type=type)
	sensExRate <- sensitivity(exRate, cents)
	senspv <- presentValue(object, sensCurve, valDate, sensExRate)
	
#		# Perdida en monto
	loss <- pv - senspv
	lossPerc <- loss / pv 
	if (class(object) == "cashFlow") {
		nam <- ""
	} else {
		nam <- object@name
	}
	df <- data.frame(nam, pv, loss, round(abs(lossPerc) * 100, 2))
	df
#	new("KRD", list(c(loss, lossPerc)), portfolio=nam, names=".", pv=pv, bps=bps)
}


setMethod("getAttr", signature(object="cashFlow"), 
	function(object, attr) {
		slot(object, attr)
	}
)

# makeCashflow
setMethod("makeCashFlow", signature(object="cashFlow", curve="spotRate"), 
	function(object, curve, valDate, fixedRateDate=NULL) {
		.makeCashFlow.cashFlow(object, curve, valDate, fixedRateDate)
	}
)

setMethod("makeCashFlow", signature(object="cashFlow", curve="spotRateList"), 
	function(object, curve, valDate, fixedRateDate=NULL) {
		curve <- select(curve, name=object@factor)
		if (length(curve) == 0) {
			stop(paste("Couldn't find", object@factor, "curve."))
		} 
		curve <- curve[[1]]
		makeCashFlow(object, curve, valDate, fixedRateDate)
	}
)

# presentValue
setMethod("presentValue", signature(object="cashFlow", curves="spotRate" ), 
	function(object, curves, valDate, exRate, ...) {
		.presentValue.cashFlow(object, curves, valDate, exRate, ...)
	}
)

setMethod("presentValue", signature(object="cashFlow", curves="spotRate", exRate="exchangeRate"), 
	function(object, curves, valDate, exRate, ...) {
		exr <- select(exRate, as.Date(valDate))[[1]]
		presentValue(object, curves, valDate, exr, ...)
	}
)

setMethod("presentValue", signature(object="cashFlow", curves="spotRate", exRate="exchangeRateList"), 
	function(object, curves, valDate, exRate, ...) {
		if (object@currency == "MXN") {
			currency <- 1
		} else{
			currency <- select(exRate, currency=object@currency)[[1]]
		}
		presentValue(object, curves, valDate, currency, ...)
	}
)

setMethod("presentValue", signature(object="cashFlow", curves="spotRateList"), 
	function(object, curves, valDate, exRate, ...) {
		curve <- select(curves, name=object@factor)
        curve <- curve[[1]]
#		print(curve)
		presentValue(object, curve, valDate, exRate, ...)
	}
)

setMethod("presentValueMultiple", signature(object="cashFlow", curves="spotRateList"),
	function(object, curves, valDate, exRate, ...) {
		unlist(lapply(curves, function(x) presentValue(object, x, valDate, exRate, ...)))
	}
)

#
setMethod("buckets", signature(object="cashFlow"), 
	function(object, breaks, ...) {
		.buckets.cashFlow(object, breaks, ...)
	}
)

# Risk measures
setMethod("PLs", signature(object="cashFlow", curves="rateCurveList"),
	function(object, curves, valDate, fromDate, toDate, exRates, ...) {
		# Selecciona la curva base
		baseCurve <- select(curves, as.Date(valDate))[[1]]
		
		# Selecciona las curvas para los escenarios
		curvesScenarios <- select(curves, as.Date(toDate), fromDate=as.Date(fromDate))	
		
		# Hace los shocks a las curvas
		shockedCurves <- as.spotRate(shockCurveHist(curvesScenarios, baseCurve))
		baseCurve <- as.spotRate(baseCurve)
		
		# Obtiene los shocks
		pls <- .pls(object, baseCurve, shockedCurves, valDate, 1, 1, ...)	
		
		# Calcula el valor presente
		presentValue <- presentValue(object, baseCurve, valDate, 1, ...)
			
		# Crea el objeto PLs
		new("PLs", pls,
			date=as.Date(valDate), 
			factor=object@factor, 
			currency=object@currency,
			type="historic",
			presentValue=presentValue	
		)
	}
)

setMethod("PLs", signature(object="cashFlow", curves="rateCurveList", exRates="exchangeRate"),
	function(object, curves, valDate, fromDate, toDate, exRates, ...) {
		# Selecciona la curva base
		baseCurve <- select(curves, as.Date(valDate))[[1]]
		
		# Selecciona las curvas y tipos de cambio para los escenarios
		curvesScenarios <- select(curves, as.Date(toDate), fromDate=as.Date(fromDate))
		exRatesScenarios <- select(exRates, as.Date(toDate), fromDate=as.Date(fromDate)) 

		if (object@currency != "MXN") {
			# Verifica que coincidan las monedas del cashflow con las de exrate
			if (exRates@currency != object@currency) {
				warning("object's currency doesn't match exRates'es")
			}
			# Selecciona el tipo de cambio base
			baseExRate <- select(exRates, as.Date(valDate))
			# Toma los tipos de cambio con la misma fecha que en las curvas
   			selExRates <- exRatesScenarios[!is.na(match(exRatesScenarios@date, getAttr(curvesScenarios, "date")))]
			# Hace los shocks
			lenExRates <- length(selExRates)
			shockedExRates <- baseExRate * c(exRates[2:lenExRates] / exRates[1:(lenExRates-1)])
   		} else {
			baseExRate <- 1
			shockedExRates <- 1
		}
		# Hace los shocks a las curvas
		shockedCurves <- as.spotRate(shockCurveHist(curvesScenarios, baseCurve))
		baseCurve <- as.spotRate(baseCurve)
		
		# Obtiene el vector de PLs
		pls <- .pls(object, baseCurve, shockedCurves, valDate, baseExRate, shockedExRates, ...)
		# Valor presente
		presentValue <- presentValue(object, baseCurve, valDate, baseExRate, ...)
		
		# Crea el objeto PLs
		new("PLs", pls,
			date=as.Date(valDate), 
			factor=object@factor, 
			currency=object@currency,
			type="historic",
			presentValue=presentValue	
		)
			
	}
)

setMethod("PLs", signature(object="cashFlow", curves="rateCurveList", exRates="exchangeRateList"),
	function(object, curves, valDate, fromDate, toDate, exRates, ...) {
		objectCur <- object@currency
		if(objectCur != "MXN") {
			exRate <- select(exRates, currency=object@currency)[[1]]
		} else {
			exRate <- 1
		}
		PLs(object, curves, valDate, fromDate, toDate, exRate, ...)
	}
)

setMethod("KRD", signature(object="cashFlow", curves="BNSpotRate"), 
	function(object, curves, valDate, exRate, bps=1, delta=1/1000000, ...) {
		curvesLen <- length(curves)
		
		KRD <- c()	
		# Derivada en  pv respecto a los nodos de la curva
		for(i in 1:curvesLen) {
			curvesDelta <- curves
			# Convierte la curva a tasas equivalentes anuales, altera la tasa por delta y la convierte en spot
			curvesDelta[i] <- (((1 + curves[i] * curves@nodes[i] / curves@period / 100) ^ 
				(curves@period / curves@nodes[i]) + delta) ^ (curves@nodes[i] / curves@period) - 1) *  
				(curves@period / curves@nodes[i] * 100)
			
			curvesSpot <- as.spotRate(curvesDelta)
			pvRight <- presentValue(object, curvesSpot, valDate, exRate, ...)
			
			curvesDelta <- curves
			curvesDelta[i] <- (((1 + curves[i] * curves@nodes[i] / curves@period / 100) ^ 
				(curves@period / curves@nodes[i]) - delta) ^ (curves@nodes[i] / curves@period) - 1) *  
				(curves@period / curves@nodes[i] * 100)
				
#			curvesDelta[i] <- curvesDelta[i] - 2 * delta
			curvesSpot <- as.spotRate(curvesDelta)
			pvLeft <- presentValue(object, curvesSpot, valDate, exRate, ...)
			KRD[i] <- (pvRight - pvLeft) / (2 * delta )
		}
		pv <- presentValue(object, as.spotRate(curves), valDate, exRate, ...)
		KRD <- KRD * bps / 10000
		names(KRD) <- curves@nodes
    	# Derivada de pv con respecto al tipo de cambio
		new("KRD", list(KRD), names=object@factor, pv=pv, bps=bps)
	}
)

setMethod("KRD", signature(object="cashFlow", curves="BNSpotRateList"), 
	function(object, curves, valDate, exRate, bps=1, delta=1/1000000) {
		curve <- select(curves, name=object@factor, date=valDate)[[1]]	
		KRD(object, curve, valDate, exRate, ...)
	}
)

#
setMethod("exchangeRateKRD", signature(object="cashFlow", exRate="exchangeRate"),
	function(object, curves, valDate, exRate, cents=1, delta=1/1000000, ...) {
		curvesLen <- length(curves)
		KRD <- c()	
		# PResent value
		pv <- presentValue(object, curves, valDate, 1, ...)
    	# Derivada de pv con respecto al tipo de cambio
    	if (!object@currency=="MXN") {
    		valDateExRate <- select(exRate, as.Date(valDate))[[1]]
			KRD <- pv / valDateExRate / 100
    	} else {
			KRD <- 0
		}
		new("KRD", list(KRD), names=object@currency, pv=pv, bps=cents)
	}
)

setMethod("exchangeRateKRD", signature(object="cashFlow", exRate="exchangeRateList"),
	function(object, curves, valDate, exRate, cents=1, delta=1/1000000, ...) {
		if(object@currency == "MXN") {
			pv <- presentValue(object, curves, valDate, exRate, ...)
			KRD <- 0
			names(KRD) <- "MXN"
			ret <- new("KRD", list(KRD), names=object@currency, pv=pv)
		} else {
			selectedExRate <- select(exRate, currency=object@currency)[[1]]
			ret <- exchangeRateKRD(object, curves, valDate, selectedExRate, cents, delta, ...)
		}	
		ret
	}
)

#
setMethod("exposure", signature(object="cashFlow", curves="spotRate"),
	function(object, curves, valDate, exRate, breaks, ...) {
# Funcion que calcula el valor presente de un conjunto de flujos
# INPUT:	object.- objeto de clase cashFlow
#			curve.- curva o lista de curvas con que se valuara
#			valDate.- dia de valuacion
#			exRate.- tipo de cambio
# OUTPUT: valor presente del instrumento a la fecha dada

	pv <- presentValue(object, curves, valDate, exRate, byFlow=TRUE, ...)
	
	# El siguiente bloque calcula los buckets 
	breaks <- valDate + sort(c(0, breaks))
	numericBreaks <- as.numeric(breaks)
	pvBucket <- tapply(pv, cut(as.numeric(object@pay_date), numericBreaks), sum, na.rm=TRUE)
	pvBucket[is.na(pvBucket)] <- 0
	
	cumulative <- cumsum(pvBucket)
	
	presentVal <- sum(pvBucket)
	new("exposure", list(pvBucket), cumulative=list(cumulative), names=paste(object@currency, object@rate_type, sep="."), 
		breaks=breaks, pv=presentVal)
		
	}
)

setMethod("exposure", signature(object="cashFlow", curves="spotRate", exRate="exchangeRate"), 
	function(object, curves, valDate, exRate, breaks, ...) {
		exr <- select(exRate, as.Date(valDate))[[1]]
		exposure(object, curves, valDate, exr, breaks, ...)
	}
)

setMethod("exposure", signature(object="cashFlow", curves="spotRate", exRate="exchangeRateList"), 
	function(object, curves, valDate, exRate, breaks, ...) {
		if (object@currency == "MXN") {
			currency <- 1
		} else{
			currency <- select(exRate, currency=object@currency)[[1]]
		}
		exposure(object, curves, valDate, currency, breaks, ...)
	}
)

setMethod("exposure", signature(object="cashFlow", curves="spotRateList"), 
	function(object, curves, valDate, exRate, breaks, ...) {
		curve <- select(curves, name=object@factor)
        curve <- curve[[1]]
		exposure(object, curve, valDate, exRate, breaks, ...)
	}
)

setMethod("potentialLoss", signature(object="cashFlow", curves="spotRateList", exRate="exchangeRateList"),
	function(object, curves, exRate, valDate, bps=1, cents=1, type="parallel") {
		.potentialLoss(object, curves, exRate, valDate, bps=bps, cents=cents, type=type)
	}
)

setMethod("liquidityGap", signature(valDate="Date", object="cashFlow"),
	function(valDate, object, breaks, fwdExRates=1, ...) {
		# Breaks
		breaks <- valDate + c(0, breaks)
		numericBreaks <- as.numeric(breaks)

		# Multiplica por el tipo de cambio fwd
		if (class(fwdExRates) == "spotRate") {
			span_fin <- as.numeric(object@pay_date - valDate)
			object@.Data <- object@.Data * fwdExRates[match(span_fin, fwdExRates@nodes)]
		} else {
			object@.Data <- object@.Data * fwdExRates
		}		
		
		# Agrupa por intervalo
		bucket <- tapply(object@.Data , cut(as.numeric(object@pay_date), numericBreaks), sum , na.rm=TRUE)
		bucket[is.na(bucket)] <- 0
		
		if (!object@asset) {
			bucket <- (-1) * bucket 
		}
		totalSum <- sum(bucket)
		cumulative <- cumsum(bucket)
		
		new("exposure", list(bucket), cumulative=list(cumulative), names=object@currency, breaks=breaks, pv=totalSum)
	}
)

setMethod("liquidityGapFWD", signature(valDate="Date", object="cashFlow", curves="spotRateList", exRate="exchangeRateList"),
	function(valDate, object, curves, exRate, breaks, ...) {
		fwd <- 1
		if (object@currency != "MXN") {
			domesticCurve <- select(curves, name="DESCUENTO_IRS")[[1]]

			# Selecciona la curva con esa moneda
			foreignCurve <- select(curves, name=getAttr(object, "factor")[[1]])[[1]]
			# Selecciona el tipo de cambio con esa moneda
			exR <- select(exRate, currency=getAttr(object, "currency")[[1]])[[1]]

			if (length(foreignCurve) == 0 | length(exR) == 0) {
				warning(paste("Couldn't find either", getAttr(object, "factor")[[1]], "or", getAttr(object, "currency")[[1]],"for this object"))
			} else {
				fwd <- fwdExRate(domesticCurve, foreignCurve, exR)
			}	
		} 
		liquidityGap(valDate, object, breaks=breaks, fwdExRates=fwd, ...)	
	}
)

setMethod("liquidityGapFWDSensitivity", signature(valDate="Date", object="cashFlow", curves="spotRateList", exRate="exchangeRateList"),
	function(valDate, object, curves, exRate, breaks, bps=1, fixedRateDate=NULL, ...) {
		delta <- 1 / 10000000 
		curveParallel <- sensitivity(curves, (-1) * delta, type="parallel")
		leftCF <- makeCashFlow(object, curveParallel, valDate, fixedRateDate)
		leftGap <- liquidityGapFWD(valDate, leftCF, curveParallel, exRate, breaks=breaks)
			
		curveParallel <- sensitivity(curves, delta, type="parallel")
		rightCF <- makeCashFlow(object, curveParallel, valDate, fixedRateDate)
		rightGap <- liquidityGapFWD(valDate, rightCF, curveParallel, exRate, breaks=breaks)	
		
		sens <- (rightGap[[1]] - leftGap[[1]]) / (2 * delta ) * bps
		breaks <- valDate + c(0, breaks)
		new("exposure", list(sens), cumulative=list(cumsum(sens)), pv=sum(sens), names=object@currency, breaks=breaks)
	}
)

setMethod("cumLiquidityGap", signature(object="cashFlow"),
	function(object, fillDates=FALSE, plot=FALSE) {
		# DataFrame con las fechas de pago y los montos
		df <- data.frame(date=object@pay_date, value=object@.Data)
		# Incluye todas las fechas en el rango
		if(fillDates) {
			minDate <- min(df$date)
			maxDate <- max(df$date)
			dates <- seq(minDate, maxDate, by=1)
			values <- rep(0, length(dates))
			df <- rbind(df,data.frame(date=dates, value=values))
		}
		df <- aggregate(df$value, by=list(df$date), sum)
		names(df) <- c("date", "value")
		if (!object@asset) {
			df$value <- (-1) * df$value
		}
		df <- df[order(df$date),]
		df$cumValue <- cumsum(df$value)
		df$currency <- object@currency
		if (plot) {
			ret <- ggplot(df, aes(x=date, y=cumValue)) + geom_step() + theme(axis.text.x=element_text(size=6), axis.text.y=element_text(size=6))
		} else {
			ret <- df
		}
		ret
	}
)

# Plot & show
# INPUT: x - cashFlow, valDate, intervals 
setMethod("plot", signature(x="cashFlow"), 
	function(x, y, ...) {
		argv <- list(...)
		valDate <- as.Date(argv$valDate)
		if (length(argv) > 1) {
			intervals <- argv$breaks
		} else{
			intervals <- c(7, 28, 91, 182, 364, 728, 1092, 1820, 3640, 5460)
		}
		intervals <- valDate + c(0, intervals)

		dataF <- cumLiquidityGap(x, plot=FALSE, fillDates=FALSE)
		maxDate <- max(as.Date(dataF$date))
	
		intervalsLen <- length(intervals)
		dataF$ymin <- 0
		dataF$ymax <- 0
		dataF$ymin[dataF$value < 0] <- dataF$value[dataF$value < 0]
		dataF$ymax[dataF$value >= 0] <- dataF$value[dataF$value >= 0]
		
		dataF <- dataF[dataF$date >= intervals[1] & dataF$date <= intervals[length(intervals)], ]
    	dataF <- dataF[dataF$date <= maxDate, ]
		if(dim(dataF)[1] == 0) {
	    	stop("There is nothing to plot")
	    }	
		# Crea un data frame con los valores de los intereses fix, intereses variables 
		# y amortizacion agregados
		buckets <- buckets(x, intervals)
		dataFBuckets <- data.frame(value=buckets$cashFlow, f_ini=intervals[1:intervalsLen-1],f_fin=intervals[2:intervalsLen], type="Amortization")
		dataFBuckets <- rbind(dataFBuckets, data.frame(value=buckets$interest_fix, f_ini=intervals[1:intervalsLen-1],f_fin=intervals[2:intervalsLen], type="Interest Fix"))
#		dataFBuckets <- rbind(dataFBuckets, data.frame(value=buckets$interest_variable, f_ini=intervals[1:intervalsLen-1],f_fin=intervals[2:intervalsLen], type="Interest Variable"))
#		dataFBuckets <- dataFBuckets[dataFBuckets$f_ini <= maxDate,]
		
		dfAux <- data.frame(panel=c(rep("Bucket",length(dataFBuckets[,1])), rep("Bucket", length(dataF[,1])), rep("Acumulado", length(dataF[,1]))))

		dataF$panel <- "Bucket"
		dataFBuckets$panel <- "Bucket"
		
		tickMarks <- sort(c(unique(dataFBuckets$f_ini), max(dataFBuckets$f_fin)))
		i <- 2
		while(i < length(tickMarks)) {
#			print(as.numeric(tickMarks[i] - tickMarks[i-1]) < 15)
			if (as.numeric(tickMarks[i] - tickMarks[i-1]) < 15 ) {
				tickMarks <- tickMarks[-i]
			}
			i <- i + 1
		}

		p <- ggplot(dfAux) + facet_grid(panel ~., scales = "free") 
		p <- p + geom_rect(aes(xmin=f_ini,xmax=f_fin, ymin=0,ymax=value), fill="steelblue", alpha=.75, data=dataFBuckets) #+ scale_fill_brewer(palette="Blues")
		p <- p + geom_linerange(aes(x=date,ymin=ymin, ymax=ymax), data=dataF)
		dataF$panel <- "Acumulado"
		p2 <- p + geom_step(aes(x=date, y=cumValue), data=dataF)
		p2 <- p2 + scale_y_continuous("monto", labels=comma)
		p2 <- p2 + scale_x_date(breaks=tickMarks) 
#		p2 <- p2 + scale_fill_discrete("tipo")
		p2 <- p2 + theme(axis.text.x=element_text(size=6, angle=90), axis.text.y=element_text(size=6)) 
		p2 <- p2 + theme(axis.title.x=element_blank(), axis.title.y = element_text(size=9))
		p2 + theme(legend.text=element_text(size=6), legend.title=element_text(size=9))
	}
)


