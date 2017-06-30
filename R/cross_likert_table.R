#' Cross likert table
#'
#' Tworzy dane częstości występowania odpowiedzi w pytaniach ze skali Likerta
#'
#' @param data dane (pełen zakres)
#' @param data.character wybrane kolumny (nazwy kolumn lub początek nazw)
#' @param factor.list przecięcia (uwaga! zmienne muszą mieć atrybut "labels"!)
#' @param weights wagi
#' @param exact jeżeli w data.character podano jedną kolumnę, to exact = TRUE
#' @param output.format 1 - lista, 2 - data frame
#' @param na.rm czy usunąć dane opisane jako na.string
#' @param na.string dane do usunięcia (domyślnie "Trudno powiedzieć")
#'
#' @export
#' @import dplyr
#' @import haven
#' @import questionr
#' @import stringr
#'
cross.likert.table <-
    function(data,
             data.character,
             factor.list,
             weights,
             exact = FALSE,
             output.format = 2,
             na.rm = FALSE,
             na.string = "Trudno powiedzieć") {
        # sprawdzenie zakresu i wybór danych - jedna zmienna, wiele zmiennych, wiele zmiennych wypranych poprzez dplyr::starts_with
        if (length(data.character) == 1 & exact == FALSE) {
            data.2 <- dplyr::select(data, starts_with(data.character))
        } else {
            data.2 <- data[, data.character]
        }
        # zebranie etykiet dla wierszy i kolumn tabel
        r.nms <- lapply(
            data.2,
            FUN = function(x) {
                attributes(x)$label
            }
        )
        etykiety <- lapply(data.2, attr, which = "labels")
        c.nms <- attributes(etykiety[[1]])$names
        # przygotowanie danych
        data.2 <- transmute_all(.tbl = data.2, .funs = as_factor)
        if (na.rm == TRUE) {
            data.2 <- as.data.frame(
                lapply(data.2,
                       FUN = function(x) recode.na(x,
                                                   na.string,
                                                   verbose = TRUE)
                )
            )
            c.nms <- c.nms[c.nms!=na.string]
        }
        n.factors <- length(factor.list)
        tmp.output <- list()
        for (i in 1:n.factors) {
            # poziomy zmiennej przecinającej
            poziomy <- as_factor(factor.list[[i]]) %>% droplevels() %>% levels
            n.levels <- length(poziomy)
            tmp.output[[i]] <- list()
            # etykiety
            names(tmp.output)[i] <- attributes(factor.list[[i]])$label
            for (j in 1:n.levels) {
                # filtrowanie danych i wag
                tmp.data <- data.2 %>%
                    dplyr::filter(as_factor(factor.list[[i]]) == poziomy[j])
                tmp.weights <-
                    weights[as_factor(factor.list[[i]]) == poziomy[j]]
                # tworzenie tabeli dla danych
                tmp <- lapply(
                    tmp.data,
                    FUN = function(x) {
                        questionr::freq(wtd.table(x, weights = tmp.weights,
                                                  na.rm = TRUE),
                                        digits = 3)[, 3]
                    }
                )
                tmp <- t(as.data.frame(tmp))
                # etykiety
                colnames(tmp) <- c.nms
                rownames(tmp) <- r.nms
                tmp.output[[i]][[j]] <- tmp
                names(tmp.output[[i]])[j] <- poziomy[j]
            }
        }
        if (output.format == 1) {
            return(tmp.output)
        } else {
            if (output.format == 2) {
                # przekształcenie danych do formatu data.frame
                tmp.output.2 <- unlist(tmp.output, recursive = F)
                variables <- colnames(data.2)
                n.variables <- length(data.2)
                output <- NULL
                for (i in 1:n.variables) {
                    tmp <- sapply(tmp.output.2, '[', i,1:length(c.nms))
                    tmp <- t(tmp)
                    tmp <- as.data.frame(tmp)
                    tmp.names <- stringr::str_split(string = rownames(tmp), pattern = "\\.", n = 3, simplify = T)
                    tmp.names[,1] <- paste(tmp.names[,1], tmp.names[,2],sep = ".")
                    pytanie <- variables[i]
                    tmp <- cbind(r.nms[i], tmp.names[,1],tmp.names[,3],tmp)
                    colnames(tmp)[1:3] <- c("zmienna","metryczka","wartosc")
                    output <- rbind(output, tmp)
                }
                return(output)
            } else {
                stop("unknown output format")
            }
        }
    }
