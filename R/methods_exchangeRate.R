setMethod("getAttr", signature(object="exchangeRate"), 
	function(object, attr, ...)	{
		slot(object, attr)
	}
)

setMethod("select", signature(object="exchangeRate", date="missing"), 
	function(object, ...)	{
		args <-  unlist(list(...))
		if (is.numeric(args)) {
			args <- as.Date(args, origin="1970-01-01")
		}
		if (is.character(args)) {
			args <- as.Date(args)
		}
		
		exrates <- object@.Data[match(args, object@date)]
		names(exrates) <- args
		exrates
	}
)

setMethod("sensitivity", signature(object="exchangeRate"),
	function(object, bps) {
		object@.Data <- object@.Data + bps / 100
		object
	}
)

setMethod("select", signature(object="exchangeRate", date="Date"), 
	function(object, date, fromDate=date, ...) {
		objectDates <- getAttr(object, "date")
		fromDate <- as.Date(fromDate)
		toDate <- as.Date(date)
		dates <- seq(fromDate, toDate, by="1 day")
		
		index <- which(objectDates %in% dates)
		new("exchangeRate", object@.Data[index], date=object@date[index], currency=object@currency, origin=object@origin)
	}
)

setMethod("plot", signature(x="exchangeRate"), 
	function(x, y, ...) {
		args <- list(...)
		dataF <- data.frame(date=x@date, MXN=x@.Data)
	
		if (length(args) > 0) {
			xmax <- as.numeric(max(dataF$date))
			xmin <- as.numeric(max(dataF$date) - args$split)  
			ymin <- min(dataF$MXN[dataF$date > max(dataF$date) - args$split])
			ymax <- max(dataF$MXN[dataF$date > max(dataF$date) - args$split])
			dataFSplit <- dataF[dataF$date > max(dataF$date) - args$split,]
			dataF$split <- "historic"
			dataFSplit$split <- paste("last", args$split ,"days")
			dataF <- rbind(dataF, dataFSplit)
#			p <- p + geom_rect(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax, fill = "grey", alpha=.05) 

			p <- ggplot(dataF, aes(x=date, y=MXN)) + geom_line() + facet_wrap(~split, scales="free") 
		} else {
			p <- ggplot(dataF, aes(x=date, y=MXN)) + geom_line() 
		} 		
		p + opts(axis.title.x=theme_blank(), axis.title.y = theme_blank()) 
	}
)

setMethod("hist", signature(x="exchangeRate"),
	function(x, ...) {
		dataF <- data.frame(date=x@date, MXN=x@.Data)
		p <- ggplot(dataF, aes(x=exchange_rate)) + geom_histogram(binwidth=.1)
		p
	}
)