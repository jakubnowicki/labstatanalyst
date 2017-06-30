#' Cross table
#'
#' Tworzy tabelę częstości dla danych typu Tak/Nie z zadanymi przecięciami
#'
#' @param data dane w formie data frame
#' @param data.character lista kolumn do analizy (jeżeli zmienna data zawiera cały zbiór danych)
#' @param factor.list zmienne do przecięcia danych
#' @param weights wagi
#'
#' @export
#' @import dplyr
#' @import questionr
#' @import haven
#'

cross.table <- function(data, data.character = NULL, factor.list, weights = NULL) {
    if(!is.null(data.character)) {
        data.2 <- dplyr::select(data, data.character)
    } else {
        data.2 <- data
    }
    if(class(factor.list) != "list") {
        factor.list <- select(data, factor.list) %>% as.list()
    }
    n.factors <- length(factor.list)
    tmp.output <- list()
    for (i in 1:n.factors) {
        tmp <- questionr::cross.multi.table(
            df = data.2,
            crossvar = haven::as_factor(factor.list[[i]]),
            weights = weights,
            freq = T,
            n = T,
            tfreq = 'col'
        ) %>% as.data.frame.matrix() %>% t()
        tmp.output[[i]] <- tmp
    }
    output <- do.call(rbind, tmp.output)
    return(output)
}
