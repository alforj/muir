# Helper functions that allow string arguments for  dplyr's data modification functions like arrange, select etc.
# Author: Sebastian Kranz

# Examples are below

#' Modified version of dplyr's filter that uses string arguments
#' @param .data TBD
#' @param ... TBD
#' @export
s_filter = function(.data, ...) {
        eval.string.dplyr(.data,"filter", ...)
}

#' Modified version of dplyr's select that uses string arguments
#' @param .data TBD
#' @param ... TBD
#' @export
s_select = function(.data, ...) {
        eval.string.dplyr(.data,"select", ...)
}

#' Modified version of dplyr's arrange that uses string arguments
#' @param .data TBD
#' @param ... TBD
#' @export
s_arrange = function(.data, ...) {
        eval.string.dplyr(.data,"arrange", ...)
}

#' Modified version of dplyr's arrange that uses string arguments
#' @param .data TBD
#' @param ... TBD
#' @export
s_mutate = function(.data, ...) {
        eval.string.dplyr(.data,"mutate", ...)
}

#' Modified version of dplyr's summarise that uses string arguments
#' @param .data TBD
#' @param ... TBD
#' @export
s_summarise = function(.data, ...) {
        eval.string.dplyr(.data,"summarise", ...)
}

#' Modified version of dplyr's group_by that uses string arguments
#' @param .data TBD
#' @param ... TBD
#' @export
s_group_by = function(.data, ...) {
        eval.string.dplyr(.data,"group_by", ...)
}

#' Internal function used by s_filter, s_select etc.
#' @param .data TBD
#' @param ... TBD
#' @param .fun.name TBD
eval.string.dplyr = function(.data, .fun.name, ...) {
        args = list(...)
        args = unlist(args)
        code = paste0(.fun.name,"(.data,", paste0(args, collapse=","), ")")
        df = eval(parse(text=code,srcfile=NULL))
        df
}

# # Examples
# library(dplyr)
# 
# # Original usage of dplyr
# mtcars %>%
#         filter(gear == 3,cyl == 8) %>%
#         select(mpg, cyl, hp:vs)
# 
# # Select user specified cols.
# # Note that you can have a vector of strings
# # or a single string separated by ',' or a mixture of both
# cols = c("mpg","cyl, hp:vs")
# mtcars %>%
#         filter(gear == 3,cyl == 8) %>%
#         s_select(cols)
# 
# # Filter using a string
# col = "gear"
# mtcars %>%
#         s_filter(paste0(col,"==3"), "cyl==8" ) %>%
#         select(mpg, cyl, hp:vs)
# 
# # Arrange without using %.%
# s_arrange(mtcars, "-mpg, gear, carb")
# 
# # group_by and summarise with strings
# mtcars %>%
#         s_group_by("cyl") %>%
#         s_summarise("mean(disp), max(disp)")
