# Clase de curvas de tasas de interes. Tiene UNICAMENTE los nodos basicos de la curva.
# 
# Author: humberto
###############################################################################


.validBNSpotRate <- function(object) {
# Valida que el objeto sea de tipo spotRate cuando se crea. 
# Esta funcion no se utiliza directamente, se llama automaticamente
# cuando se crea un objeto.
    # Valida que exista una tasa por cada nodo
    if (length(object@nodes) != length(object)) {
        stop("The number of rates must be equal to the number of nodes.")
    }
    # Valida que los nodos NO sean consecutivos
    len_nodes <- length(object@nodes)
    if (len_nodes == object@nodes[len_nodes]) {
        stop("Introduced curve appears to be a spotRate curve.")
    }
    TRUE
}

.as.spotRateLin <- function(object) {
# Funcion que convierte un objeto de clase BNSpotRate en uno de clase spotRate
# mediante interpolación lineal en ln(1+spot).
# INPUT:    object.- objeto BNSpotRate
# OUTPUT:   objeto spotRate
    int_x <- seq(1, max(object@nodes), 1)
    interpolation <- approx(object@nodes, log(1 + object * object@nodes / object@period / 100), int_x)
    y <- new("spotRate", (exp(interpolation$y) - 1) * 36000 / int_x,
            nodes=interpolation$x,
            name=object@name,
            date=object@date,
            period=object@period,
            origin=object@origin)
	y
}

.as.spotRateQuad <- function(object) {
# Funcion que convierte un objeto de clase BNSpotRate en uno de clase spotRate
# mediante interpolación cuadrática en ln(1+spot).
# INPUT:    object.- objeto BNSpotRate
# OUTPUT:   objeto spotRate 
    len <- length(object)
    ##
    intervalo <- object@nodes[2:len] - object@nodes[1:(len-1)]
    ## Logaritmo de Tasa Spot efectiva
    spot <- log(1 + object@.Data * object@nodes / 36000)
    ## Encontrar primera delta
    delta <- 2 * (spot[2]-(intervalo[1] + 1) * spot[1]) / ((intervalo[1] + 1) * (intervalo[1]))
    rho <- spot[1] + (intervalo[1]) * delta
    # 
    for(i in 2:(len - 1)) {
        delta <- c(delta, 2 * (spot[i+1] - spot[i] - (intervalo[i]) * rho[i - 1]) / 
                        ((intervalo[i] + 1) * (intervalo[i])))
        rho <- c(rho, rho[i - 1] + (intervalo[i]) * delta[i])
    }
    rho <- c(spot[1], rho)
    ## Interpolacion lineal entre fwds
    int_x <- seq(1, max(object@nodes), 1)
    interpolacion_fwd <- approx(object@nodes, rho, int_x)
    interpolacion_spot <- (exp(cumsum(interpolacion_fwd$y)) - 1) * 36000 / (interpolacion_fwd$x)
    y <- new("spotRate", interpolacion_spot,
            nodes=int_x,
            name=object@name,
            date=object@date,
            period=object@period,
            origin=object@origin)
	y
}

setValidity("BNSpotRate", .validBNSpotRate)

setMethod("as.spotRate", "BNSpotRate", 
	function(object, type="linear") {
		if(type=="linear") {
			ret <- .as.spotRateLin(object)
		}
		if(type=="quadratic") {
			ret <- .as.spotRateQuad(object)
		}
		ret
	}
)

setMethod("plot", signature(x="BNSpotRate"), 
	function(x, y, ...) {
		x <- yield(x)
		dataF <- data.frame(node=x@nodes, rate=x@.Data)
		p <- ggplot(dataF, aes(x=node, y=rate )) 
		graf <- p + geom_line(size=.7, alpha=.25) + geom_point(size=1.2)
		graf
	}
)