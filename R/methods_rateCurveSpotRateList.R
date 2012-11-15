# Clase de lista de curvas de tasas en TODOS los nodos.
# Lista de objetos de clase "spotRateList".
# 
# Author: humberto
###############################################################################

setMethod("initialize", "spotRateList", function(.Object, x) {
            cls <- unique((lapply(x, class)))
            len_cls <- length(cls)
            name <- unique((lapply(x, function(x, y) slot(x, y), y="name")))
            len_name <- length(name)
            
            if (len_cls != 1 | cls[[1]] != "spotRate") {
                stop("All rateCurves in list must be a spotRate object.")
            } 
            if(len_name != 1) {
                name <- "MIXED"
            } else {
                name <- name[[1]]
            }
            callNextMethod(.Object, x, name=name)
        })

setMethod("as.spotRateList", signature(object="list"),
	function(object) {
		new("spotRateList", object)
	}
)

setMethod("plot", signature(x="spotRateList"), 
	function(x, y, ...) {
		dataF <- data.frame(node=x[[1]]@nodes, rate=x[[1]]@.Data, curve=x[[1]]@name, date=x[[1]]@date)
		for(i in 2:length(x)) {
			dataF <- rbind(dataF,data.frame(node=x[[i]]@nodes, rate=x[[i]]@.Data, curve=x[[i]]@name, date=x[[1]]@date))
		}
		p <- ggplot(dataF, aes(x=node, y=rate, group=curve, colour=curve))
		p + geom_line(alpha=1)
	}
)
