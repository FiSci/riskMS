# Clase de curvas de tasas de interes. Tiene TODOS los nodos de la curva.
# 
# Author: humberto
###############################################################################


.validSpotRate <- function(object) {
# Valida que el objeto sea de tipo spotRate cuando se crea. 
# Esta funcion no se utiliza directamente, se llama automaticamente
# cuando se crea un objeto.
    # Valida que el numero de nodos sea igual que el numero de tasas
    if (length(object@nodes) != length(object)) {
        stop("The number of rates must be equal to the number of nodes.")
    }
    # Valida que los nodos sean completos y consecutivos ej. 1,...,10000
    len_nodes <- length(object@nodes)
    if (len_nodes != object@nodes[len_nodes]) {
        stop("Must introduce a curve with consecutive nodes.")
    }
    TRUE
}
setValidity("spotRate", .validSpotRate)

setMethod("fwdRate", signature(object="spotRate"), 
	function(object, node_1, node_2) {
		object <- object / 100
		rate <- object[match(node_2, object@nodes)] * (node_2) / object@period - 
	            object[match(node_1, object@nodes)] * node_1 / object@period
	    rate <- rate / (1 + object[match(node_1, object@nodes)] * node_1 / object@period) * object@period / (node_2 - node_1)
	    rate
	}
)

setMethod("plot", signature(x="spotRate"), 
	function(x, y, ...) {
		dataF <- data.frame(node=x@nodes, rate=x@.Data)
		p <- ggplot(dataF, aes(x=node, y=rate )) 
		p + geom_line() 
	}
)

setMethod("fwdExRate", signature(domesticCurve="spotRate", foreignCurve="spotRate", exRate="exchangeRate"),
	function(domesticCurve, foreignCurve, exRate) {
		minLen <- min(length(domesticCurve), length(foreignCurve))
		index <- 1:minLen
		
		fwdExRate <- exRate * (1 + domesticCurve[index] * domesticCurve@nodes[index] / 36000) / 
			(1 + foreignCurve[index] * foreignCurve@nodes[index] / 36000)
			
		new("spotRate", fwdExRate, nodes=foreignCurve@nodes[index], origin="INTERNAL", 
			date=domesticCurve@date, name=paste("FWD", exRate@name, sep=""))
	}
)
