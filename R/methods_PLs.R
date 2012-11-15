setMethod("VaR", signature(object="PLs"), 
	function(object, probs=c(.95, .99, .995)) {
		quantiles <- data.frame(quantile=paste(qs*100,"%",sep=""),value=quantile(x, probs=probs))
		quantiles
	}
)

setMethod("plot", signature(x="PLs"), 
	function(x, y, ...) {
		argv <- list(...)
		probs <- .995
		binwidth <- 10
		div <- 1
		if (!is.null(argv$binwidth)) {
			binwidth <- argv$binwidth
		} 
		if (!is.null(argv$prob)) {
			probs <- argv$probs
		} 
		if (!is.null(argv$div)) {
			div <- argv$div
		}
	
		dataF <- data.frame(PLs=x@.Data)
		quantiles <- data.frame(quantile=paste(probs*100,"%",sep=""),value=quantile(x, probs=probs) / div)
		dataF$PLs <- dataF$PLs / div
		dataF$div <- div
		print(quantiles)
		p <- ggplot(dataF)
		p <- p + geom_histogram(aes(x=PLs, y=..density..), colour="steelblue", binwidth=binwidth, fill="steelblue") 
		p <- p + geom_vline(aes(xintercept=value), colour="red", data=quantiles) 
		p <- p + geom_density(aes(x=PLs),size=.5, kernel="gaussian", fill="steelblue", alpha=.25, data=dataF)
		p <- p + scale_x_continuous("pérdida", labels=comma)
		p <- p + scale_y_continuous(labels=NULL)
		p <- p + opts(axis.title.x=theme_text(size=9), axis.title.y=theme_blank())
		p + opts(axis.text.x=theme_text(size=8))
	}
)

##### Reporte
# Recibe como argumentos un objeto de clase exchangeRateList y el parametro
# lastN que son los ultimos N tipos de cambio que se quieren guardar
setMethod("saveTex", signature("PLs"),
	function(x, file="var.tex", probs=c(.95, .99, .995), rowIdentifier=TRUE, divisor=1, ...) {
		qt <- data.frame(x@portfolio, x@presentValue) 
		for(i in as.numeric(quantile(x, probs=probs))) {
			qt <- cbind(qt, i)
		}
		qtLen <- length(qt[1,])
		for(i in 2:qtLen) {
			qt[,i] <- format(round(as.numeric(qt[,i])/divisor, digits=0), big.mark=",")
		}
		if(!rowIdentifier) {
			qt <- qt[ , c(-1,-2)]
		} 
		if (!is.data.frame(qt)) {
			qt <- data.frame(t(qt))
		}
		print(xtable(qt), floating=FALSE, only.contents=TRUE, include.rownames=FALSE, 
			include.colnames=FALSE, hline.after=NULL, file=file)
	}
)

