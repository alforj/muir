#' Explore Datasets with Trees
#'
#' This function allows users to easily and dynamically explore or document a
#' dataset using a tree structure.
#' @param data A data frame to be explored using trees
#' @param node.levels The columns from data that will be used to construct the
#'  tree in the order that they should appear in the tree levels
#' @param level.criteria A data frame consisting of 4 character columns containing a
#' column name (from node.levels), an operator or function (e.g., "==",">", "is.na"),
#' a value, and a label
#' @param tree.dir Direction of tree graph. Use either "LR" for left-to-right,
#' "RL" for right-to left, "TB" for top-to-bottom, or "BT" for bottom-to-top.
#' @param show.percent Should nodes show the percent of records represented by
#' that node compared to the total number of records in data. Defaults to TRUE
#' @param show.empty.child Show a balanced tree with children nodes that are all
#' empty or stop expanding the tree once there is a parent node that empty.
#' Defaults to FALSE -- don't show empty children nodes
#' @param tree.height Control tree height to zoom in/out on nodes. Defaults to NULL
#' @param tree.width Control tree width to zoom in/out on nodes. Defaults to NULL
#' @param ... Placeholder for future params to support GraphViz attributes
#' @return An object of class \code{htmlwidget} that will print itself as HTML
#' @import dplyr
#' @export
#' @rdname muir

muir <- function(data, node.levels, level.criteria, tree.dir = "LR", show.percent = TRUE,
                 show.empty.child = FALSE, tree.height = NULL, tree.width = NULL, ...) {

  dfname <<- args()[1]
  # Hack for CRAN R CMD check
  parent <- NULL; rm("parent")
  index <- NULL; rm("index")
  node <- NULL; rm("node")

  #         # validate function parameters
  #         stopifnot(inherits(data,"data.frame"))
  #         TBD Add more validation checks on params

  node.criteria <- node.levels
  node.levels <- unlist(strsplit(node.levels, "[+*]"))

  ## Initialize data frame based on number of levels (data columns) provided
  colindex <- dplyr::data_frame(index=1:length(node.levels), col = node.levels,
                                criteria = node.criteria, stringsAsFactors = FALSE)

  ## Ensure level.criteria has expected number of cols and set names
  ## TBD - current only works if level.criteria is provided as df. Accepting list as potential enhancement
  if(!(ncol(level.criteria) == 4)) {
    stop("The criteria input does not contain the required number of columns.
         See help file for instructions.")
  }

  colnames(level.criteria) <- c("col", "oper", "val", "label")

  ## Remove factor levels from level.criteria if present by forcing character
  #i <- sapply(level.criteria, is.factor)
  #level.criteria[i] <- lapply(level.criteria[i], as.character)
  level.criteria <- data.frame(lapply(level.criteria, as.character), stringsAsFactors=FALSE)

  ## Get data values for columns where the user wants nodes for each value and update criteria
  for(i in 1:nrow(colindex)) {
    if (grepl("[*]", colindex$criteria[i])) {

      col.values <- s_group_by(data, colindex$col[i])
      col.values <- unlist(dplyr::summarise(col.values))

      new.criteria <- data.frame(col = as.character(colindex$col[i]),
                                 oper = "==",
                                 val = as.character(col.values),
                                 label = paste0(as.character(colindex$col[i]), " = ",
                                                as.character(col.values)),
                                 stringsAsFactors = FALSE)
      level.criteria <- filter(level.criteria, col != colindex$col[i])
      level.criteria <- dplyr::bind_rows(level.criteria, new.criteria)
    }
  }

  colcounts <- dplyr::group_by(level.criteria, col)
  colcounts <- dplyr::summarize(colcounts, cnt = n())
  colcounts <- dplyr::inner_join(colcounts, colindex, by = "col")
  colcounts <- dplyr::arrange(colcounts, index)

  ## Determine number of df columns used based on node.levels
  numcols <- length(node.levels)

  #Add 1 to account for the remaining "other" nodes where requested
  for(i in 1:nrow(colcounts)) {
    if (grepl("[+]", colcounts$criteria[i])) {
      colcounts$cnt[i] <- colcounts$cnt[i] + 1
    }
  }

  numnodes <- 1 + max(cumsum(cumprod(colcounts$cnt)))
  nodedf_cols <- c("node", "colindex", "parent", "filter", "leaf_filter", "label", "v_n")
  nodedf <- data.frame(matrix(ncol = length(nodedf_cols), nrow = numnodes))
  colnames(nodedf) <- nodedf_cols

  ## Set parent node
  nodedf$node[1] <- 1
  nodedf$colindex[1] <- 0
  nodedf$parent[1] <- "None"
  nodedf$filter[1] <- "None"
  nodedf$leaf_filter[1] <- "None"
  nodedf$label[1] <- "All"
  nodedf$v_n[1] <- as.integer(dplyr::summarize(data, n()))

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
        nodedf$label[cur_node] <- NA
        nodedf$v_n[cur_node] <- 0
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
          nodedf$label[cur_node] <- leaves[[4]][n]
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
          dfcount <- s_filter(data, cur_filter)
          dfcount <- as.integer(dplyr::summarize(dfcount, n()))
          nodedf$v_n[cur_node] <- dfcount
          cur_node <- cur_node + 1

        }

        ## Add "Other" Node to capture remaining data
        if(grepl("[+]",colindex$criteria[colindex$col == node.levels[r]])) {
          nodedf$node[cur_node] <- cur_node
          nodedf$colindex[cur_node] <- r
          nodedf$label[cur_node] <- "Other"

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
          dfcount <- s_filter(data, cur_filter)
          dfcount <- as.integer(dplyr::summarize(dfcount, n()))
          nodedf$v_n[cur_node] <- dfcount
          nodedf$parent[cur_node] <- root
          cur_node <- cur_node + 1
        }

      }

    }
  }

  # add percent of total n() value to the df for inclusion as a node label later
  if (show.percent) {
    v_pct <- round((as.numeric(nodedf$v_n)/as.numeric(nodedf$v_n[1])) * 100, ...)
    nodedf <- dplyr::mutate(nodedf, v_pct = v_pct)
  }

  nodedf <- filter(nodedf, !is.na(node))
  tree <- build_tree(nodedf, tree.dir, tree.height, tree.width)

  # return and render tree
  tree

}
