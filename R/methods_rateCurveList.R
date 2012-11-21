############################################################################
# Clase de lista de curvas de tasas de interes. 
# Clase ABSTRACTA qe hereda sus atributos y rutinas a otras.
############################################################################

.as.matrix.rateCurveList <- function(object) {
	lenObject <- length(object)
	# Obtiene los valores de la tasa
	rates <- getAttr(object, ".Data")

	# Comprueba que todas las entradas tengan el mimso numero de tasas
	elementLen <- unlist(lapply(rates, length))
	if(length(unique(elementLen)) != 1) {
		stop("rateCurveList elements must have the same number of elements.")
	}

    nodes <- object[[1]]@nodes
    rateMatrix <- matrix(rates, nrow=lenObject, byrow=TRUE)   
	colnames(rateMatrix) <- nodes
	rateMatrix
}

.shockCurveHist <- function(object, baseCurve) {
# Funcion que crea un objeto de clase "xxxList" con las curvas obtenidas
# de aplicar shocks historicos a la curva con la fecha mas reciente.
# INPUT:	object.- objecto de clase rateCurveList o de alguna clase que herede de esta.
# OUTPUT: 	lista de curvas con los shocks historicos

	# Ordena la lista de curvas por fecha ya que los shocks se hacen en este orden
	object <- new(class(object), object[order(getAttr(object, "date"))])
	len_object <- length(object)
	
    len_nodes <- unique(unlist(lapply(object, length)))
	
	if (length(len_nodes) != 1) {
        stop("Curves must have the same length.")
	} 

	if (object@name == "MIXED") {
		warning("Applying shocks to distinct name curves.")
	}
    
	term <- baseCurve@nodes / baseCurve@period / 100
    
	sim <- list()
	for (i in 1:(len_object-1)) {
		sim[[i]] <- (1 / (1 + object[[i]] * term) * (1 + object[[i + 1]] * term) * (1 + baseCurve * term) - 1) * 1/term
	}
    new(class(object), sim)
	
}

setMethod("getAttr", signature(object="rateCurveList"), 
	function(object, attr, ...)	{
		ret <- unlist(lapply(object, function(x, y) slot(x, y), y=attr))
		if (attr == "date"){
			ret <- as.Date(ret, origin="1970-01-01")
		}
		ret
	}
)

setMethod("select", signature(object="rateCurveList", date="missing"), .select)

setMethod("select", signature(object="rateCurveList", date="Date"), 
	function(object, date, fromDate=date) {
		curveNames <- getAttr(object, "name")
		if (length(unique(curveNames)) > 1 ) {
			warning("Different curve names in object")
		}
		curveDates <- getAttr(object, "date")
		fromDate <- as.Date(fromDate)
		toDate <- as.Date(date)
		dates <- seq(fromDate, toDate, by="1 day")
		
		index <- which(curveDates %in% dates)
		object@.Data <- object[index]
		object
		
	}
)

setAs("rateCurveList","rateCurve", 
	function(from) {
    	if (length(from) == 0) {
	        stop("Null list.")
	    } else if (length(from) > 1) {
	        warning("More than one element in rateCurveList. Returning first element.")
	    }
	    from[[1]]
	}
)

setMethod("shockCurveHist", signature(object="rateCurveList", baseCurve="rateCurve"), .shockCurveHist)

setMethod("cov", "rateCurveList", 
	function (x, y = NULL, use = "everything", method = c("pearson", 
	    "kendall", "spearman")) {
		objectMat <- .as.matrix.rateCurveList(x)
		cov(objectMat, y , use , method )
	}
)

setMethod("var", "rateCurveList", 
	function (x, y = NULL, na.rm = FALSE, use)  {
		objectMat <- .as.matrix.rateCurveList(x)
		var(objectMat, y, na.rm, use)
	}
)

setMethod("mean", "rateCurveList", 
	function (x, ...) {
		objectMat <- .as.matrix.rateCurveList(x)
		colMeans(objectMat, ...)
	}
)

setMethod("summary", signature("rateCurveList"), 
	function (object, ...) {
		objectMat <- .as.matrix.rateCurveList(object)
		summary(objectMat, ...)
	}
)

setMethod("yield", signature(object="rateCurveList"),
	function (object, ...) {
		new(class(object), lapply(object, yield))
	}
)

setMethod("as.data.frame", signature("rateCurveList"),
	function(x, row.names = NULL, optional = FALSE, ...) {
		df <- data.frame(x)
		if (x@name == "MIXED") {
			nam <- unlist(lapply(x, function(x) x@name))
		} else {
			nam <- unlist(lapply(x, function(x) as.character(x@date)))
		}
		df <- cbind(data.frame(x[[1]]@nodes), df)
		names(df) <- c("NODES", nam)
		df
	}
)

setMethod("sensitivity", signature(object="rateCurveList"),
	function(object, bps, type=c("parallel", "twist", "bend"), ...) {
		type <- match.arg(type)
		newCurves <- lapply(object, function(x) sensitivity(x, bps, type=type))
		name <- paste(object@name, " (", type, ")", sep="")
		new(class(object), newCurves)
	}
)


## rep
setMethod("saveTex", signature("rateCurveList"),
	function(x, file="exRates.tex", ...) {
		x <- yield(x)
		df <- as.data.frame(x)
		df <- cbind(c("1","7","28","91","182","364", "2Y", "3Y", "5Y", "10Y", "15Y","20Y", "30Y"), df[,-1])
		colnames(df) <- c("Plazo", names(df)[-1])
		print(xtable(df[1:length(df[,1]),]), floating=FALSE, only.contents=FALSE, include.rownames=FALSE, file=file)
	}
)


# TODO: VaR montecarlo
