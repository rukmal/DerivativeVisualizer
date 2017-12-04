#' @title Profit/loss profile of Equity
#'
#' @description
#' \code{equity} returns an object of values,
#' with the strike price of the underlying asset,
#' and the corresponding profit
#'
#' @param price Price of the equity.
#' @param commission Commission charged for purchasing the equity. Defaults to $0.
#' @param shares Number of shares purchased. Deefaults to 1.
#' @param short Set to TRUE if a short position is opened. Defaults to FALSE.
#' @param strikeRange Range of strike prices for which profit/loss is calculated. Defualts to 90\% to 110\%.
#' @param granularity Granularity of range of prices generated. Defaults to $0.01.
#'
#' @export
#' @examples
#' callOption(strike = 100, premium = 4, short = TRUE)

equity <- function (price, commission = NULL, shares = NULL, short = NULL, strikeRange = NULL, granularity = NULL) {
	if (is.null(strikeRange)) {
		strikeRange <- get("range", envir = defaults)
	}

	lower <- price * strikeRange[1]
	upper <- price * strikeRange[2]

	if (is.null(granularity)) {
		granularity <- get("granularity", envir = defaults)
	}

	strikePrices <- seq(from = lower, to = upper, by = granularity)

	if (is.null(commission)) {
		commission <- get("commission", envir = defaults)
	}

	profitLoss <- strikePrices - commission

	if (is.null(short)) {
		short <- get("short", envir = defaults)
	}

	if (short) {
		profitLoss <- profitLoss * (-1)
	}

	if (is.null(shares)) {
		shares <- get("shares", envir = defaults)
	}

	profitLoss <- profitLoss * shares

	# Creating output object
	output <- list("strikePrices" = strikePrices)
	output$profitLoss <- profitLoss

	output
}
