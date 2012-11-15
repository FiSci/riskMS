# divisor = list(c(2,1000), (3, 10))
setMethod("saveTex", signature("data.frame"),
	function(x, file="tex.tex", levels=2, divisor=list()) {
		x[,1] <- as.character(x[,1])
		colLen <- length(x[,1])
		rowLen <- length(x[1,])
		
		# Divide
		if (length(divisor) > 0) {
			for(i in 1:length(divisor)) {
				numCol <- divisor[[i]][1]
				div <- divisor[[i]][2]
				x[, numCol] <- round(x[, numCol] / div, 0)
				x[, numCol] <- format(x[, numCol], big.mark=",")
			}
		}

		# Primer nivel de la tabla (de izquierda a derecha)
		if (levels == 2) {
			dfNames <- data.frame(c(x[1,1], rep("", colLen-1 )), c("", x[2:colLen, 1]))
			names(dfNames) <- c("level1", "level2")
			# Une los nombres y el df
			x <- cbind(dfNames, x[,2:rowLen])
		}
		print(xtable(x), floating=FALSE, only.contents=TRUE, include.rownames=FALSE, 
			hline.after=NULL,include.colnames=FALSE, file=file)
	}
)

