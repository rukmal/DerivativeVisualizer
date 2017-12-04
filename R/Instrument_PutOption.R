#' @title Profit/loss profile of Put Option
#'
#' @description
#' \code{putOption} returns an object of values,
#' with the x coordinate denoting the strike price
#' of the underlying asset, and the y coordinate denoting the profit
#' were the underlying to be at a price level of $x.
#'
#' @param strike Strike price of the put option.
#' @param premium Premium of the put option.
#' @param size Contract size of the option. Defaults to 100.
#' @param short Set to TRUE if a short position is opened. Defaults to FALSE.
#' @param strikeRange Range of strike prices for which profit/loss is calculated. Defualts to 90\% to 110\%.
#' @param granularity Granularity of range of prices generated. Defaults to $0.01.
#'
#' @export
#' @examples
#' putOption(strike = 100, premium = 4, short = TRUE)

putOption <- function (strike, premium, size = NULL, short = NULL, strikeRange = NULL, granularity = NULL) {
	if (is.null(strikeRange)) {
		strikeRange <- get("range", envir = defaults)
	}

	lower <- strike * strikeRange[1]
	upper <- strike * strikeRange[2]

	if (is.null(granularity)) {
		granularity <- get("granularity", envir = defaults)
	}

	strikePrices <- seq(from = lower, to = upper, by = granularity)

	profitLoss <- c()

	for (price in strikePrices) {
		if (price > strike) {
			profitLoss <- c(profitLoss, (-1 * premium))
		} else {
			profitLoss <- c(profitLoss, (strike - price) - premium)
		}
	}

	if (is.null(short)) {
		short <- get("short", envir = defaults)
	}

	if (short) {
		profitLoss <- profitLoss * (-1)
	}

	if (is.null(size)) {
		size <- get("size", envir = defaults)
	}

	profitLoss <- profitLoss * size

	# Creating output object
	output <- list("strikePrices" = strikePrices)
	output$profitLoss <- profitLoss

	output
}
