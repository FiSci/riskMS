############################################################################
# Clase con los atributos de una curva de tasas de interes.
# Esta clase es ABSTRACTA por lo que no se puede instanciar un objeto de ella ya que
# NO CONTIENE EL VALOR DE LAS TASAS.
############################################################################


setMethod("getAttr", signature(object="rateCurve"), function(object, attr, ...)	slot(object, attr))

setMethod("yield", signature(object="rateCurve"),
	function(object, ...) {
		object@.Data <- ((1 + object@.Data * object@nodes / object@period / 100) ^ (object@period / object@nodes) - 1) * 100 
		object
	}
)

setMethod("sensitivity", signature(object="rateCurve"),
	function(object, bps, type=c("parallel", "twist", "bend"), ...) {
		type <- match.arg(type)
		bps <- bps / 10000
		nodes <- object@nodes
		xlen <- nodes[length(nodes)]
		if (type=="parallel") {
			approxVector <- bps
		}
		if (type=="twist" | type=="bend") {
			firstNode <- bps
			lastNode <- -bps

			slope <- (lastNode - firstNode) / (max(nodes) - min(nodes))
			translation <- (min(nodes) + max(nodes)) / 2
			approxVector <- slope * (nodes - translation)
		}
		if (type=="bend") {
			approxVector <- abs(approxVector)
		}
		object@.Data <- (((1 + object * object@nodes / object@period / 100) ^ 
			(object@period / object@nodes) + approxVector) ^ (object@nodes / object@period) - 1) *  
			(object@period / object@nodes * 100)
#		object@name <- paste(object@name, " (", type, ")", sep="")
		object
	}
)
