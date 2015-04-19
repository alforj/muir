#' Explore Datasets with Trees
#'
#' This function allows users to easily and dynamically explore or document a
#' dataset using a tree structure.
#' @param data A data frame to be explored using trees
#' @param node.levels The columns from \code{data} that will be used to construct the
#'  tree provides as a character vector in the order that they should appear in the tree levels.
#'  Values can be provided just as "colname", "colname!", "colname+", or "colname!+".
#'  \enumerate{
#'  \item Providing just the column name itself (e.g, "hp") will only return results
#'  based on the operators and values provided in the \code{level.criteria} parameter
#'  for that column name.
#'  \item Providing the column name ending with an "*" (e.g., "hp*") will return a node for
#'  all distinct values for that column up to the limit imposed by the \code{node.limit} value.
#'  \item Providing the column name ending with with a "+" (e.g., "hp+") will return all the
#'  values provided in the \code{level.criteria} parameter for that column plus an extra node
#'  titled "Other" for that column that aggregates all the remaining values not included
#'  in the \code{level.criteria} df for that column.
#'  \item Providing a column name ending with both symbols (e.g., "hp*+") will return a node for
#'  all distinct values for that column up to the limit imposed by the \code{node.limit} value
#'  plus an additional "Other" node aggregating any remaining values beyond the \code{node.limit},
#'  if applicable. If the number of distinct values is <= the \code{node.limit} then the "Other"
#'  node will not be created.
#'  }
#'  See the node.levels vignette for more details.
#' @param level.criteria A data frame consisting of 4 character columns containing a
#' column name (from \code{node.levels} without any "*" or "+" symbols), an operator or
#' function (e.g., "==",">", "is.na"), a value, and a corresponding node title for that criteria.
#' @param node.limit When providing a colum in \code{node.levels} that ends with a "*"
#' the \code{node.limit} will limit how many distinct values to actually process to prevent
#' run-away queries and unreadable trees. The limit defaults to 3 (not including an additional
#' 4th if requesting to provide an "Other" node as well with a "*+" suffix). If the
#' number of distinct values for the column is greater than the \code{node.limit}, the tree
#' will include the Top "X" values based on count, where "X" = \code{node.limit}. If the
#' \code{node.limit} is greater than the number of distinct values for the column, it will
#' be ignored.
#' @param label.vals Additional values to include in the node provided as a
#' character vector. The values must take the form of dplyr \code{summarise} functions
#' (as characters) and include the columns the functions should be run against (e.g.,
#' "min(hp)", "mean(hp)", etc.)
#' @param tree.dir The direction the tree graph should be rendered. Defaults to "LR"
#' \enumerate{
#' \item Use "LR" for left-to-right
#' \item Use "RL" for right-to left
#' \item Use "TB" for top-to-bottom
#' \item User "BT" for bottom-to-top
#' }
#' @param show.percent Should nodes show the percent of records represented by
#' that node compared to the total number of records in \code{data.} Defaults to TRUE
#' @param num.precision Number of digits to print numeric label values out to
#' @param show.empty.child Show a balanced tree with children nodes that are all
#' empty or stop expanding the tree once there is a parent node that is empty.
#' Defaults to FALSE -- don't show empty children nodes
#' @param tree.height Control tree height to zoom in/out on nodes. Passed to DiagrammeR
#' as \code{height} param. Defaults to NULL
#' @param tree.width Control tree width to zoom in/out on nodes. Passed to DiagrammeR
#' as \code{width} param. Defaults to NULL
#' @param ... Additional named arguments. E.g., \code{digits} to control precision of
#' the percent value it \code{show.percent} = TRUE.
#' @return An object with classes \code{DiagrammeR} and \code{htmlwidget} that will
#' print itself as HTML.
#' @import dplyr stringr
#' @export
#' @rdname muir

muir <- function(data, node.levels, level.criteria, node.limit = 3, label.vals = NULL, tree.dir = "LR", show.percent = TRUE,
                 num.precision = 2, show.empty.child = FALSE, tree.height = NULL, tree.width = NULL, ...) {

  dots <- list(...)

  # Hack for CRAN R CMD check
  parent <- NULL; rm("parent")
  index <- NULL; rm("index")
  node <- NULL; rm("node")

  #         # validate function parameters
  #         stopifnot(inherits(data,"data.frame"))
  #         TBD Add more validation checks on params

  ## Parse node.level columns to separate colnames from the requested criteria
  node.criteria <- data.frame(index = 1:length(node.levels),
                              do.call(rbind, str_split(node.levels, ":")),
                              stringsAsFactors = FALSE)
  colnames(node.criteria) <- c("index", "col", "criteria")

  #make sure cols with no node.level criteria are marked as NA (they will be repeated in df)
  node.criteria$criteria[node.criteria$column == node.criteria$criteria] <- NA
  node.levels <- as.vector(unlist(node.criteria$col))


#   node.criteria <- node.levels
#   node.levels <- str_replace(node.levels, "[+$]", "")
#   node.levels <- unlist(str_replace(node.levels, "[*$]", ""))

  ## Initialize data frame based on number of levels (data columns) provided
#   colindex <- dplyr::data_frame(index=1:length(node.levels), col = node.levels,
#                                 criteria = node.criteria, stringsAsFactors = FALSE)

  ## Ensure level.criteria has expected number of cols and set names
  ## TBD - current only works if level.criteria is provided as df. Accepting list as potential enhancement
  ## TBD need to handle when level.criteria is NULL
  ## TBD Don't need to check ncols if NULL -- and will break anyway
  ## TBD if not NULL, need to make sure that all the cols in node.levels that are not (*|n) are included
  if(!(ncol(level.criteria) == 4)) {
    stop("The criteria input does not contain the required number of columns.
         See help(muir) for instructions.")
  }

  colnames(level.criteria) <- c("col", "oper", "val", "title")

  ## Remove factor levels from level.criteria if present by forcing character
  #i <- sapply(level.criteria, is.factor)
  #level.criteria[i] <- lapply(level.criteria[i], as.character)


  level.criteria <- data.frame(lapply(level.criteria, as.character), stringsAsFactors=FALSE)

  ## Get data values for columns where the user wants nodes for each value and update criteria
  ## to use those values instead of any values passed in via level.criteria. Constrain number
  ## of values to the node.limit in descending order by total count of occurrences in the data df
  for(i in 1:nrow(node.criteria)) {
    if (grepl("\\*|\\d", node.criteria$criteria[i])) {

      col.values <- s_group_by(data, node.criteria$col[i])
      col.values <- dplyr::summarize(col.values, cnt = n())
      col.values <- dplyr::arrange(col.values, desc(cnt))

      if (grepl("\\*", node.criteria$criteria[i])) {
        col.values <- dplyr::slice(col.values, 1:node.limit)
      } else {
        col.values <- dplyr::slice(col.values, 1:str_extract(node.criteria$criteria[i], "\\d"))
      }

      col.values <- unlist(s_select(col.values, node.criteria$col[i]))

      new.criteria <- data.frame(col = as.character(node.criteria$col[i]),
                                 oper = "==",
                                 val = as.character(col.values),
                                 title = paste0(as.character(node.criteria$col[i]), " = ",
                                                as.character(col.values)),
                                 stringsAsFactors = FALSE)

      ## TBD need to handle when level.criteria starts as NULL
      level.criteria <- filter(level.criteria, col != node.criteria$col[i])
      level.criteria <- dplyr::bind_rows(level.criteria, new.criteria)
    }
  }


  colcounts <- dplyr::group_by(level.criteria, col)
  colcounts <- dplyr::summarise(colcounts, cnt = n())
  colcounts <- dplyr::inner_join(colcounts, node.criteria, by = "col")
  colcounts <- dplyr::arrange(colcounts, index)

  ## Determine number of df columns used based on node.levels
  numcols <- length(node.levels)

  #Add 1 to the count to account for the remaining "other" nodes where requested

  for(i in 1:nrow(colcounts)) {
    if (grepl("\\+", colcounts$criteria[i])) {
      colcounts$cnt[i] <- colcounts$cnt[i] + 1
    }
  }
 ###TBD make sure rest of DB is populated correctly

  numnodes <- 1 + max(cumsum(cumprod(colcounts$cnt)))

  nodedf_cols <- c("node", "colindex", "parent", "filter", "leaf_filter", "title", "v_n")

    # If label vals are provided, add them as columns in the node df
    add.labels = NULL
    if (!is.null(label.vals)) {
      add.labels <- as.vector(sapply(label.vals,
                                     function(x) {
                                       init <- x
                                       x1 <- str_replace(init, "\\(", "_")
                                       x2 <- str_replace(x1, "\\)","")
                                       paste0("v_", x2)
                                       }))
      nodedf_cols <- c(nodedf_cols,add.labels)
    }


  nodedf <- data.frame(matrix(ncol = length(nodedf_cols), nrow = numnodes))
  colnames(nodedf) <- nodedf_cols

  ## Set parent node
  nodedf$node[1] <- 1
  nodedf$colindex[1] <- 0
  nodedf$parent[1] <- "None"
  nodedf$filter[1] <- "None"
  nodedf$leaf_filter[1] <- "None"
  nodedf$title[1] <- "All"
  nodedf$v_n[1] <- as.integer(dplyr::summarise(data, n()))

  ## Add values for additional lables if provided
  if (!is.null(add.labels)) {
    for (l in 1:length(add.labels)) {
      if (nodedf$v_n[1] > 0) {
      nodedf[, add.labels[l]][1] <- as.character(format(round(s_summarise(data, label.vals[l]),
                                                 digits = num.precision),
                                           nsmall = num.precision))
      } else {
        nodedf[, add.labels[l]][1] <- NA
      }
    }
  }



  ### Build out binary tree table nodedf
  cur_node <- 2

  for (r in 1:numcols) { # loop through the columns provided from the dataset

    parnodes <- dplyr::filter(nodedf, colindex == (r - 1))
    leaves <- dplyr::filter(level.criteria, col == node.levels[r])

    for (pn in 1:nrow(parnodes)) { # Loop through the parent nodes at the current level

      root <- parnodes$node[pn]
      leaf_filter <- NULL

      if (!show.empty.child & parnodes$v_n[pn] == 0) {

        ## if parent node has no values, set the child leaves as NA/0
        nodedf$node[cur_node] <- cur_node
        nodedf$colindex[cur_node] <- r
        nodedf$parent[cur_node] <- root
        nodedf$filter[cur_node] <- NA
        nodedf$leaf_filter[cur_node] <- NA
        nodedf$title[cur_node] <- NA
        nodedf$v_n[cur_node] <- 0

        if (!is.null(add.labels)) {
          for (l in 1:length(add.labels)) {

            nodedf[, add.labels[l]][cur_node] <- NA
          }
        }

        cur_node <- cur_node + 1

      } else {

        if (parnodes$filter[pn] == "None") {
          pfltr <- NULL
        } else {
          pfltr <- parnodes$filter[pn]
        }

        for (n in 1:nrow(leaves)) { # Add leaves for each parent node at this level


          nodedf$node[cur_node] <- cur_node
          nodedf$colindex[cur_node] <- r
          nodedf$title[cur_node] <- leaves[[4]][n]
          nodedf$parent[cur_node] <- root
          if (leaves[[3]][n] == "" | is.na(leaves[[3]][n])) {
            nodedf$leaf_filter[cur_node] <- ff(leaves[[2]][n], leaves[[1]][n])
          } else {
            nodedf$leaf_filter[cur_node] <- paste0(leaves[[1]][n], leaves[[2]][n], leaves[[3]][n])
          }

          if (!is.null(pfltr)) {
            cur_filter <- paste0(pfltr, ",", nodedf$leaf_filter[cur_node])
          } else {
            cur_filter <- nodedf$leaf_filter[cur_node]
          }

          nodedf$filter[cur_node] <- cur_filter
          cur_filter_df <- s_filter(data, cur_filter)
          dfcount <- as.integer(dplyr::summarise(cur_filter_df, n()))
          nodedf$v_n[cur_node] <- dfcount

          if (!is.null(add.labels)) {
            for (l in 1:length(add.labels)) {
              if (nodedf$v_n[cur_node] > 0) {
                nodedf[, add.labels[l]][cur_node] <- as.character(format(round(s_summarise(cur_filter_df,
                                                                              label.vals[l]),
                                                                  digits = num.precision),
                                                            nsmall = num.precision))
              } else {
                nodedf[, add.labels[l]][cur_node] <- NA
              }
            }
          }

          cur_node <- cur_node + 1

        }

        ## Add "Other" Node to capture remaining data
        if(grepl("\\+",node.criteria$criteria[node.criteria$col == node.levels[r]])) {
          nodedf$node[cur_node] <- cur_node
          nodedf$colindex[cur_node] <- r
          nodedf$title[cur_node] <- "Other"

          leafdf<- filter(nodedf, parent == root)
          leafdf <- select(leafdf, leaf_filter)
          prev_leaf_filters <- leafdf
          nodedf$leaf_filter[cur_node] <- paste0("!", paste0(leafdf[[1]], collapse = ",!"))

          if (!is.null(pfltr)) {
            cur_filter <- paste0(pfltr, ",", nodedf$leaf_filter[cur_node])
          } else {
            cur_filter <- nodedf$leaf_filter[cur_node]
          }

          nodedf$filter[cur_node] <- cur_filter
          cur_filter_df <- s_filter(data, cur_filter)
          dfcount <- as.integer(dplyr::summarise(cur_filter_df, n()))
          nodedf$v_n[cur_node] <- dfcount
          nodedf$parent[cur_node] <- root

          if (!is.null(add.labels)) {
            for (l in 1:length(add.labels)) {
              if (nodedf$v_n[cur_node] > 0) {
                nodedf[, add.labels[l]][cur_node] <- as.character(format(round(s_summarise(cur_filter_df,
                                                                              label.vals[l]),
                                                                  digits = num.precision),
                                                            nsmall = num.precision))
              } else {
                nodedf[, add.labels[l]][cur_node] <- NA
              }
            }
          }
          cur_node <- cur_node + 1
        }

      }

    }
  }

  # add percent of total n() value to the df for inclusion as a node label later
  if (show.percent) {
    v_pct <- format(round((as.numeric(nodedf$v_n)/as.numeric(nodedf$v_n[1])) * 100,
                          digits = num.precision), nsmall = num.precision)
    nodedf <- dplyr::mutate(nodedf, v_pct = v_pct)
  }

  nodedf <- filter(nodedf, !is.na(node))
  #return(nodedf)
  tree <- build_tree(nodedf, tree.dir, tree.height, tree.width)

  # return and render tree
  tree

}
