#' @title Profit/loss profile of Put Option
#'
#' @description
#' \code{putOption} returns a tuple of values,
#' with the x coordinate denoting the strike price
#' of the underlying asset, and the y coordinate denoting the profit
#' were the underlying to be at a price level of $x.
#'
#' @param strike Strike price of the put option.
#' @param premium Premium of the put option.
#' @param size Contract size of the option. Defaults to 100.
#' @param short Set to TRUE if a short position is opened. Defaults to FALSE.
#' @param lower Lower bound of the range of strike prices. Defaults to 90\% of strike price.
#' @param upper Upper bound of the range of strike prices. Defaults to 110\% of strike price.
#' @param granularity Granularity of range of prices generated. Defaults to $0.01.
#'
#' @export
#' @examples
#' putOption(strike = 100, premium = 4, short = TRUE)

putOption <- function (strike, premium, size = 100, short = FALSE, lower = NULL, upper = NULL, granularity = 0.01) {
	if (is.null(lower)) {
		lower <- strike * 0.9 # Lower defaults to 90% of strike price
	}
	if (is.null(upper)) {
		upper <- strike * 1.1 # Upper defaults to 110% of strike price
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

	if (short) {
		profitLoss <- profitLoss * (-1)
	}

	profitLoss <- profitLoss * size

	t(rbind(strikePrices, profitLoss))
}
