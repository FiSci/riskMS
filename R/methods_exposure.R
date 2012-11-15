setMethod("as.data.frame", signature("exposure"),
	function(x, row.names = NULL, optional = FALSE, ...) {
		df <- as.data.frame.list(x)
		
		lenBreaks <- length(x@breaks)
		rownames(df) <- paste("(",x@breaks[1:lenBreaks-1], ",", x@breaks[2:lenBreaks], "]", sep="")
		
		# Transpone el DF
		df <- t(df)
		df <- cbind(PV=x@pv, df)
		# Ordena los renglones del DF
		rowNames <- sort(row.names(df))
		# Determina si el objeto pertenece a liquidez o a exposicion
		# Exposicion:
		if (sum(rowNames=="TOTAL") > 0) {
			rowNames <- rowNames[rowNames!="TOTAL"]
			df <- df[c("TOTAL", rowNames),]
		} else { # Liquidez:
			if (length(rowNames) > 1) {
				df <- df[rowNames,] 
			} 
		}
		df
	}
)

setMethod("plot", signature(x="exposure"), 
	function(x, y, ...) {
		exposure <- as.data.frame.list(x)
		lenBreaks <- length(x@breaks)
		rownames(exposure) <- paste("(",x@breaks[1:lenBreaks-1], ",", x@breaks[2:lenBreaks], "]", sep="")
#		exposure$bin <- rownames(exposure)
		exposure$bin <- as.factor(x@breaks[2:length(x@breaks)] - x@breaks[1])
		exposure <- melt(exposure, id=c("bin"))
		exposure <- exposure[exposure$variable != "TOTAL", ]
		names(exposure) <- c("plazo", "moneda.factor", "monto")
		
		p <- ggplot(exposure, aes(x=plazo, y=monto / 1000, fill=moneda.factor))
		p <- p +  geom_bar(stat="identity", position="dodge")
		p <- p + scale_y_continuous("monto", labels=comma)
		p <- p + scale_x_discrete("plazo") 
		p <- p + scale_fill_discrete("moneda.factor")
		p <- p + opts(axis.text.x=theme_text(size=6), axis.text.y=theme_text(size=6)) 
		p <- p + opts(axis.title.x=theme_text(size=9), axis.title.y = theme_text(size=9, angle=90))
		p + opts(legend.text=theme_text(size=6), legend.title=theme_text(size=9))
		
		#+ xlim(as.character(unique(KRDdf$risk_factor))) #+ scale_fill_brewer(palette="Blues")
	}
)

##### Reporte
# Recibe como argumentos un objeto de clase exchangeRateList y el parametro
# lastN que son los ultimos N tipos de cambio que se quieren guardar
setMethod("saveTex", signature("exposure"),
	function(x, file="exposure.tex", divisor=1) {
		# 
		df <- as.data.frame(x)
		df <- format(round(df/divisor,0), big.mark=",")

		# Formato de los nombres de los factores
		nam <- row.names(df)
		splittedNam <- strsplit(nam, ".", fixed=TRUE)
		
		# Primer nivel de la tabla (de izquierda a derecha)
		level0 <- c(x@portfolio, rep("", length(nam) - 1))
		
		# Segundo nivel de la tabla (de izquierda a derecha)
		level1 <- unlist(lapply(splittedNam, function(x) x[1]))
		level1[level1 == "TOTAL"] <- ""
		level1[duplicated(level1)] <- ""
		
		# Tercer nivel de la tabla (de izquierda a derecha)
		level2 <- unlist(lapply(splittedNam, function(x) x[2]))
		level2[is.na(level2)] <- ""
		
		levelsDF <- data.frame(nivel=level0, moneda=level1, factor=level2)
		levelsDF[,1] <- as.character(levelsDF[,1])
		levelsDF[,2] <- as.character(levelsDF[,2])
		levelsDF[,3] <- as.character(levelsDF[,3])
		df <- cbind(levelsDF, df)
		df <- .catalog(df)
		print(xtable(df), floating=FALSE, only.contents=TRUE, include.rownames=FALSE, 
			hline.after=NULL,include.colnames=FALSE, file=file)
	}
)

setMethod("joinExposure", signature(x="exposure", y="exposure"),
	function(x, y, portfolioName="") {
		# 
		if (sum(x@breaks != y@breaks) != 0) {
			error("object and object2 must have the same breaks")
		}

		nams <- unique(c(names(x), names(y)))
		
		newList <- list()
		pv <- c()
		k <- 1
		for(i in nams) {
			indx <- which(names(x) == i)
			indy <- which(names(y) == i)
			if(length(indx) == 0) {
				xySum <- y@.Data[[indy]]
				xypv <- y@pv[indy]
			} else if(length(indy) == 0) {
				xySum <- x@.Data[[indx]]
				xypv <- x@pv[indx]
			} else{
				xySum <- x@.Data[[indx]] + y@.Data[[indy]]
				xypv <- x@pv[indx] + y@pv[indy]
			}
			newList[[k]] <- xySum
			pv <- c(pv, xypv)
			
			k <- k + 1	
		}
		
		new("exposure", newList, names=nams, portfolio=portfolioName, breaks=x@breaks, cumulative=list(), pv=pv)
	}
)
