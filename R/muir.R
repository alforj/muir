#' Explore Datasets with Trees
#'
#' This function allows users to easily and dynamically explore or document a dataset using a tree structure.
#' @param data A data frame to be explored
#' @param node.levels The columns from data that will be used to construct the tree in the order that
#' they should appear in the tree levels
#' @param criteria A data frame consisting of 4 character columns containing a column name (from node.levels),
#' an operator or function (e.g., "==",">", "is.na"), a value, and a label
#' @param show.percent Should nodes show the percent of records represented by that node compared to the total
#' number of records in data. Defaults to TRUE
#' @param percent.digits Remove and put digits in ...
#' @param show.empty.child Show a balanced tree with children nodes that are all empty or stop expanding
#' the tree once there is a parent node that empty. Defaults to FALSE -- don't show empty children nodes
#' @param ... TBD OTHER PARAMS
#' @return An object of class \code{htmlwidget} that will print itself as HTML and be saved as tree.html in wd()
#' @export
#' @rdname muir

muir <- function(data, node.levels, criteria, show.percent = TRUE, percent.digits = 2, show.empty.child = FALSE, ...) {

        'select' <- NULL; rm("select")
        'starts_with' <- NULL; rm("starts_with")
       'parent' <- NULL; rm("parent")
       'index' <- NULL; rm("index")
         'node' <- NULL; rm("node")

#         # validate function parameters
#         stopifnot(inherits(data,"data.frame"))
#         TBD Add more validation checks on params

#         source("R/ff.R")
#         source("R/pct.R")
#         source("R/build_tree.R")

        node.criteria <- node.levels
        node.levels <- unlist(strsplit(node.levels, "[+*]"))

        ## Initialize data frame based on number of levels (data columns) provided
        colindex <- dplyr::data_frame(index=1:length(node.levels), col = node.levels, criteria = node.criteria, stringsAsFactors = FALSE)

        ## Ensure criteria has expected number of cols and set names
        ## TBD - does something like this work if provided as a list, may need to also force convert list to df first
        if(!(ncol(criteria) == 4)) {
                stop("The criteria input does not contain the required number of columns. See help file for instructions.")
        }

        colnames(criteria) <- c("col", "oper", "val", "label")

        ## Remove factor levels from criteria if present
        #i <- sapply(criteria, is.factor)
        #criteria[i] <- lapply(criteria[i], as.character)
        criteria <- data.frame(lapply(criteria, as.character), stringsAsFactors=FALSE)

        ## Get data values for columns where the user wants nodes for each value and update criteria
        for(i in 1:nrow(colindex)) {
                if (grepl("[*]", colindex$criteria[i])) {

                        col.values <- s_group_by(data, colindex$col[i])
                        col.values <- unlist(dplyr::summarise(col.values))

                        new.criteria <- data.frame(col = as.character(colindex$col[i]),
                                                   oper = "==",
                                                   val = as.character(col.values),
                                                   label = paste0(as.character(colindex$col[i]), " = ", as.character(col.values)),
                                                   stringsAsFactors = FALSE)
                        criteria <- filter(criteria, col != colindex$col[i])
                        criteria <- dplyr::bind_rows(criteria, new.criteria)


                }
        }

        colcounts <- dplyr::group_by(criteria, col)
        colcounts <- dplyr::summarize(colcounts, cnt = n())
        colcounts <- dplyr::inner_join(colcounts, colindex, by = "col")
        colcounts <- dplyr::arrange(colcounts, index)

        ## Reset criteria based on node.levels
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
                leaves <- dplyr::filter(criteria, col == node.levels[r])

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
                                                                #nodedf$v_n[cur_node] <- as.integer(data %>% s_filter(cur_filter) %>% dplyr::summarize(n()))
                                                                nodedf$parent[cur_node] <- root
                                                                cur_node <- cur_node + 1
                                                        }

                                                }

                        }
        }

        if (show.percent) {
                nodedf <- dplyr::mutate(nodedf,v_pct = round((as.numeric(nodedf$v_n)/as.numeric(nodedf$v_n[1])) * 100,percent.digits))
        }

        nodedf <- filter(nodedf, !is.na(node))
        #return(nodedf)
        tree <- build_tree(nodedf)
        htmlwidgets::saveWidget(tree, "tree.html")
        tree

}
