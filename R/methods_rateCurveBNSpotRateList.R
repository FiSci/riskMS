# Clase de lista de curvas de tasas en nodos basicos. 
# Lista de objetos de clase "BNSpotRate".
# 
# Author: humberto
###############################################################################

setMethod("initialize", "BNSpotRateList", function(.Object, x) {
# Inicializa el objeto con un nombre adecuado.
# Esta funcion no se utiliza directamente, se llama automaticamente
# cuando se crea un objeto.
            cls <- unique((lapply(x, class)))
            len_cls <- length(cls)
            
            name <- unique((lapply(x, function(x, y) slot(x, y), y="name")))
            len_name <- length(name)
            # Verifica que todos los elementos de la lista sean de clase BNSpotRate.
            if(len_cls != 1 | cls[[1]] != "BNSpotRate") {
                stop("All rateCurves in list must be a BNSpotRate object.")
            } 
            # Los nombres de los elementos de la lista son distintos, 
            # asigna MIXED al nombre del objeto de clase BNSpotRateList:
            if (len_name != 1) {
                name <- "MIXED"
            } else {
                # Si todos los elementos de la lista se llaman igual, 
                # asigna el mismo nombre al objeto de clase BNSpotRateList:
                name <- name[[1]]
            }
            callNextMethod(.Object, x, name=name)    
        }
)

setMethod("as.spotRate", signature(object="BNSpotRateList"), 
	function(object, type="linear") {
	# Funcion que convierte un objeto de la clase BNSpotRateList a uno
	# de la clase spotRateList mediante interpolacion 
	# INPUT:    object.- objeto de clase BNSpotRateList
	#           type.- tipo de interpolacion
	# OUTPUT:    objeto de clase spotRateList
		new_list <- lapply(object, function(x) as.spotRate(x, type))
	  new("spotRateList", new_list)
	}
)

setMethod("plot", signature(x="BNSpotRateList"), 
	function(x, y, ...) {
		x <- yield(x)
		dataF <- melt(as.data.frame(x), id=c("NODES"))
		current <- max(as.Date(dataF$variable))
		
		weeklyCurves <- seq(max(as.Date(dataF$variable)), min(as.Date(dataF$variable)), by=-7)
		# Escoge una curva por semana
		dataF <- dataF[!is.na(match(as.Date(dataF$variable), weeklyCurves, )),]
		
		# La convierte en yield
		
		dataFLine <- dataF[as.Date(dataF$variable) == current,]
		
		
		dataFLine$NODES2 <- c(dataFLine$NODES[2:length(dataFLine$NODES)],0)
		dataFLine$RATES2 <- c(dataFLine$value[2:length(dataFLine$NODES)],0)
		dataFLine <- dataFLine[-length(dataFLine$NODES),]
		dataF$variable <- as.Date(dataF$variable)
		dataFLine$variable <- as.Date(dataFLine$variable)
		min_date <- min(dataF$variable)
		max_date <- max(dataF$variable)
		min_date_numeric <- as.numeric(min_date)
		max_date_numeric <- as.numeric(max_date)
		
		
#		p <- ggplot(dataF, aes(x=factor(node), y=log(10+rate), group=as.numeric(date),colour=as.numeric(date))) + geom_jitter(alpha=.3)
#		p <- p + scale_colour_gradient("Dates",low="red", high="blue", breaks=c(min_date_numeric, max_date_numeric), labels=c(min_date, max_date))
#		p <- p + facet_wrap(~node, nrow=1, scale="free")
#		p <- p + geom_point(aes(x=factor(node), y=rate), colour="red",data=dataFLine)
	
		# Tickmarks
		tickMarks <- unique(dataF$NODES)
		i <- 2
		while(i < length(tickMarks)) {
			if (as.numeric(tickMarks[i] - tickMarks[i-1]) < 180 ) {
				tickMarks <- tickMarks[-i]
			}
			i <- i + 1
		}
		
		# Plot
		p <- ggplot(dataF, aes(x=NODES, y=value, group=as.numeric(variable),colour=as.numeric(variable))) + geom_line(alpha=.4)
		p <- p + scale_colour_gradient("Dates",low="red", high="blue", breaks=c(min_date_numeric, max_date_numeric), labels=c(min_date, max_date))
		p <- p + geom_segment(aes(x=NODES, y=value, xend=NODES2, yend=RATES2), colour="black",data=dataFLine, size=.7) + geom_point(aes(x=NODES, y=value), colour="black",size=1.2, data=dataFLine)
		p <- p + theme(axis.title.x=element_blank(), axis.title.y = element_blank(),	axis.text.x=element_text(size=6), axis.text.y=element_text(size=6)) 
		p <- p + scale_fill_discrete("fecha")
		p <- p + scale_x_continuous("plazo", breaks=tickMarks[-1]) 
		p <- p + theme(legend.text=element_text(size=6), legend.title=element_text(size=9))
		p <- p + theme(axis.text.x=element_text(size=6, angle=90), axis.title.x=element_text(size=9))
		p
	}
)


