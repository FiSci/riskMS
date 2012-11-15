setMethod("plot", signature(x="KRD"), 
	function(x, y, ...) {
		lenx <- length(x)
		ids <- names(x)
		KRDdf <- as.data.frame(x)
		KRDdf$risk_factor <- rownames(KRDdf)
		KRDdf <- melt(KRDdf, id=c("risk_factor"))
		p <- ggplot(KRDdf, aes(x=risk_factor, y=value/1000, fill=variable))
		p <- p +  geom_bar(stat="identity", position="dodge") #+ xlim(as.character(unique(KRDdf$risk_factor))) #+ scale_fill_brewer(palette="Blues")
		p <- p + scale_y_continuous("cambio (en miles de pesos)", labels=comma)
		p <- p + scale_x_discrete("plazo", limits=as.character(unique(KRDdf$risk_factor)))
		p <- p + scale_fill_discrete("factor")
		p <- p + opts(axis.text.x=theme_text(size=6, angle=90), axis.text.y=theme_text(size=6)) 
		p <- p + opts(axis.title.x=theme_text(size=9), axis.title.y = theme_text(size=9, angle=90))
		p + opts(legend.text=theme_text(size=6), legend.title=theme_text(size=9))
	}
)

##### Reporte
# Recibe como argumentos un objeto de clase exchangeRateList y el parametro
# lastN que son los ultimos N tipos de cambio que se quieren guardar
setMethod("saveTex", signature("KRD"),
	function(x, file="KRD.tex", divisor=1,...) {
		args <- list(...)
		df <- format(round(t(as.data.frame(x)/divisor), 0), big.mark=",")
		lenDF <- length(x)
		df <- cbind(data.frame(c(x@portfolio, rep("", lenDF-1)), 
			c(pv=format(round(x@pv / divisor, 0), big.mark=",")), row.names(df)), df)
		df[,3] <- as.character(df[,3])
		df <- .catalog(df)
		print(xtable(df), floating=FALSE, only.contents=TRUE, include.rownames=FALSE, 
			hline.after=NULL, include.colnames=FALSE, file=file)
	}
)

setMethod("linearApproximation", signature(object="KRD"),
	function(object, vector, ...){
		if (length(object[[1]]) != length(vector)) {
			stop("vector must have the same length as object")
		}
		vector <- vector / 10000
		newKRD <- list()
		for(i in 1:length(object)) {
			# para obtener la derivada y no la sensibilidad
			gradient <- object[[i]] * 10000 / object@bps
			newKRD[[i]] <- sum(gradient * vector)
		}
		object@.Data <- newKRD	
		object
	}
)

# Aplica sensibilidades de tipi twist, paralelos y bends con base en los KRDs
# twist: incrementa en bps el primer nodo y en -bps el ultimo, 
#		obtiene la recta que une a estos dos puntos para hacer los cambios
#		en los nodos intermedios
# 
setMethod("sensitivity", signature(object="KRD"),
	function(object, bps, type=c("parallel", "twist", "bend"), ...){
		type <- match.arg(type, several.ok = TRUE)
		lenObject <- length(object)
		lenType <- length(type)
		
		sensList <- list()
		for(i in 1:lenType) {
			if (type[i]=="parallel") {
				approxVector <- rep(bps, length(object[[1]]))
			}
			if (type[i]=="twist" | type[i]=="bend") {
				nodes <- as.numeric(names(object[[1]]))
				xlen <- nodes[length(nodes)]

				# suma bps al primer nodo y lo resta al ultimo
				firstNode <- bps
				lastNode <- -bps

				# obtiene la recta que une a los dos puntos anteriores
				slope <- (lastNode - firstNode) / (max(nodes) - min(nodes))
				translation <- (min(nodes) + max(nodes)) / 2
				approxVector <- slope * (nodes - translation)	
			}
			if (type[i]=="bend") {
				approxVector <- abs(approxVector)
			}
			sensList[[i]] <- unlist(linearApproximation(object, approxVector))
		}
		sensDF <- as.data.frame(sensList)
		sensList <- list()
		for(i in 1:lenObject) {
			vector <- as.numeric(sensDF[i,])
			names(vector) <- type
			sensList[[i]] <- vector
		}
		object@.Data <- sensList
		object
	}
)
