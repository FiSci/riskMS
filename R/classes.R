setClass("cashFlow",
        representation(
        		asset="logical",
                factor="character",         # Nombre de la curva con que se valua
                period_ini_date="Date",     # Periodo de calculo de intereses. Es el periodo de revision de tasa.
                period_fin_date="Date",
				pay_date="Date",
                rate_type="character",      # FIX, TIIE, ETC
                rate="numeric",             # Valor de la tasa
                spread="numeric",           # Valor del spread
                currency="character",       
                capital="numeric",          # Capital sobre el que se calculan intereses
                amortization="numeric",      # Pagos a capital
				interest_fix="numeric",
				interest_variable="numeric"
        ),
		contains="numeric",			#amortizacion + intereses 
		prototype=prototype(
			period_ini_date=Sys.Date(),
			period_fin_date=Sys.Date(),
			pay_date=Sys.Date()
		)
);

setClass("cashFlowPortfolio", representation(name="character"), contains="list");

#setClass("interestRates", representation(date="Date", name="character", period="numeric"), contains="numeric");

setClass("exchangeRate",
	representation(
		currency="character",
		date="Date",
		origin="character"
		),
	contains="numeric",
	prototype=prototype(name="USD", date=Sys.Date(), origin="BANXICO")
);

setClass("exchangeRateList",
	representation(
		names="character"
		),
	contains="list",
	prototype=prototype(name="MIXED")
);

setClass("rateCurve",
	representation(
		nodes="numeric",	# Nodos que conforman la curva
		name="character",	# Nombre de la curva: ej. TIIE28, LIBOR, Descuento IRS, etc...
		date="Date",		# Fecha de generacion de la curva
		period="numeric",	# Periodo al que se encuentran las tasas (en dias), 360 (para las anualizadas)
		origin="character"	# Origen de la curva: PIP, Quantit, etc...
		),
		# DEFAULTS: name = "TIIE",
		#	date = fecha del dia en que se crea el objeto,
		#	period = 360,
		#	origin = "PIP"
	prototype=prototype(name="TIIE", date=Sys.Date(), period=360, origin="PIP")
);

setClass("rateCurveList",
	representation(
		name="character",	# Nombre de la LISTA DE TASAS
		period="numeric"	
	)
);

setClass("spotRate", representation("rateCurve"), contains="numeric");

setClass("spotRateList", representation("rateCurveList"), contains="list");

setClass("BNSpotRate", representation("rateCurve"), contains="numeric");

setClass("BNSpotRateList", representation("rateCurveList"), contains="list");

# Risk Measures
setClass("exposure",
	representation(
		portfolio="character",
		names="character",
		breaks="Date",
		cumulative="list",
		pv="numeric"
	),
	contains="list"
)

setClass("PLs", 
	representation(
		portfolio="character",
		date="Date",
		factor="character",
		currency="character",
		type="character",
		presentValue="numeric"
	),
	contains="numeric"
)

setClass("KRD", 
	representation(portfolio="character", names="character", pv="numeric", bps="numeric"), 
	contains="list"
)
