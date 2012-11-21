.as.matrix.exchangeRateList <- function(object) {
	lenObject <- length(object)
	# Obtiene los valores de la tasa
	rates <- getAttr(object, ".Data")
	
	# Comprueba que todas las entradas tengan el mimso numero de tasas
	elementLen <- unlist(lapply(rates, length))
	if(length(unique(elementLen)) != 1) {
		stop("rateCurveList elements must have the same number of elements.")
	}
	
    rateMatrix <- matrix(rates, ncol=lenObject, byrow=FALSE)   
	colnames(rateMatrix) <- names(object)
	rateMatrix
}

setMethod("getAttr", signature(object="exchangeRateList"), 
	function(object, attr, ...)	{
		ret <- unlist(lapply(object, function(x, y) slot(x, y), y=attr))
		if (attr == "date"){
			ret <- as.Date(ret, origin="1970-01-01")
		}
		ret
}
)

setMethod("select",signature(object="exchangeRateList", date="missing"), .select)

setMethod("select", signature(object="exchangeRateList", date="Date"), 
	function(object, date, fromDate=date, ...) {
		names <- getAttr(object, "currency")
		selDates <- lapply(object, function(x) select(x, date))
		new("exchangeRateList", selDates, names=names)
	}
)

setMethod("sensitivity", signature(object="exchangeRateList"),
	function(object, bps) {
		object@.Data <- lapply(object, function(x) sensitivity(x, bps))
		object
	}
)


setMethod("cov", "exchangeRateList", 
	function (x, y = NULL, use = "everything", method = c("pearson", "kendall", "spearman")) {
		objectMat <- .as.matrix.exchangeRateList(x)
		cov(objectMat, y, use, method)
	}
)

setMethod("var", "exchangeRateList", 
	function (x, y = NULL, na.rm = FALSE, use)  {
		objectMat <- .as.matrix.exchangeRateList(x)
		var(objectMat, y, na.rm, use)
	}
)

setMethod("mean", "exchangeRateList",
	function (x, ...) {
		objectMat <- .as.matrix.exchangeRateList(x)
		colMeans(objectMat, ...)
	}
)

setMethod("summary", signature("exchangeRateList"),
	function(object, ...) {
		objectMat <- .as.matrix.exchangeRateList(object)
		summary(objectMat, ...)
	}
)

setMethod("plot", signature(x="exchangeRateList"), 
	function(x, y, ...) {
		args <- list(...)
		dataF <- melt(as.data.frame(x), id=c("date"))
		if (length(args) > 0) {
			dataFSplit <- dataF[dataF$date > max(dataF$date) - args$split,]
			dataF$split <- "historic"
			dataFSplit$split <- paste("last", args$split ,"days")
			dataF <- rbind(dataF, dataFSplit)
#			p <- p + geom_rect(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax, fill = "grey", alpha=.05) 

			p <- ggplot(dataF, aes(x=date, y=value, colour=variable)) + geom_line() + facet_wrap(~split, scales="free") 
		} else {
			p <- ggplot(dataF, aes(x=date, y=value, colour=currency)) + geom_line()
		}
		p <- p + scale_fill_discrete("moneda")
		p <- p + theme(legend.text=element_text(size=6), legend.title=element_text(size=9))
		p + theme(axis.title.x=element_blank(), axis.title.y = element_blank(), axis.text.x=element_text(size=6), axis.text.y=element_text(size=6)) 
	}
)

setMethod("as.data.frame", signature("exchangeRateList"),
	function(x, row.names = NULL, optional = FALSE, ...) {
		lenX <- length(x)
		lenExRates <- length(x[[1]])
		nam <- names(x)
		df <- data.frame(date=x[[1]]@date)
		
		for (i in 1:lenX) {
			df[nam[i]] <- x[[i]]@.Data
		}
		df
	}
)

##### Reporte
# Recibe como argumentos un objeto de clase exchangeRateList y el parametro
# lastN que son los ultimos N tipos de cambio que se quieren guardar
setMethod("saveTex", signature("exchangeRateList"),
	function(x, file="exRates.tex", ...) {
		args <- list(...)
		df <- as.data.frame(x)
		if (!is.null(args$lastN)) {
			df <- df[length(df[,1]) - seq(0,args$lastN - 1),]
		}
		df$date <- as.character(df$date)
		print(xtable(df), floating=FALSE, only.contents=FALSE, include.rownames=FALSE, file=file)
	}
)
