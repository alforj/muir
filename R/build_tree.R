#' Build dynamic tree using DiagrammeR and muir-generated data frame
#'
#' This function allows users to easily and dynamically explore or document a
#'  dataset using a tree structure.
#' @param data A muir-generated data frame to be processed into a tree using DiagrammeR
#' @param tree.dir Direction of tree graph. Use either "LR" for left-to-right,
#' "RL" for right-to left, "TB" for top-to-bottom, or "BT" for bottom-to-top.
#' @param tree.height Control tree height to zoom in/out on nodes. Defaults to NULL
#' @param tree.width Control tree width to zoom in/out on nodes. Defaults to NULL
#' @return An object of class \code{htmlwidget} that will print itself as HTML and
#' be saved as tree.html in wd()
#' @import dplyr stringr DiagrammeR
#' @rdname buiid_tree
build_tree <- function(data, tree.dir = "LR", tree.height = NULL, tree.width = NULL) {

  # Hack for CRAN R CMD check
  select <- NULL; rm("select")
  starts_with <- NULL; rm("starts_with")
  parent <- NULL; rm("parent")


  nodelist <- paste0("graph ", tree.dir, ";")
  nodevals <- names(select(data, starts_with("v_")))


  ## Build parent (head) node

  headnode <- filter(data, parent == "None")
  if (nrow(headnode) != 1) stop("There is either no head node or more than one head node.")

  nodelist <- paste0(nodelist, headnode$node, "(", headnode$title, "<br/>", sep = "")
  for (n in 1:length(nodevals)) {

    nodelist <- paste0(nodelist, paste0(stringr::str_replace(nodevals[n], "v_", ""), ": "),
                       headnode[,nodevals[n]], "<br/>", sep = "")
  }

  nodelist <- paste0(nodelist, ");", sep = "")

  ## Build non-Zero leaf nodes (for when user does not children of parents with no values)
  nodes <- filter(data, parent != "None")

  ## if no data, spit back an emtpy diagram
  if (nrow(nodes) < 1) return(DiagrammeR::DiagrammeR(nodelist))

  edge.num <- 0
  for (i in 1:nrow(nodes)) {

    if(is.na(nodes$title[i])) {

      nodelist <- paste0(nodelist, nodes$parent[i], "-->", nodes$node[i],
                         "( );", sep = "")

      if(exists("hide.nodes")) {
        hide.nodes <- c(hide.nodes, nodes$node[i])
      } else {
        hide.nodes <- nodes$node[i]
      }

      if(exists("hide.edges")) {
        hide.edges <- c(hide.edges, edge.num)
      } else {
        hide.edges <- edge.num
      }

      edge.num = edge.num + 1

    } else {

      nodelist <- paste0(nodelist, nodes$parent[i], "-->", nodes$node[i],
                         "(", nodes$title[i], "<br/>", sep = "")


      for (n in 1:length(nodevals)) {

        nodelist <- paste0(nodelist, paste0(stringr::str_replace(nodevals[n], "v_", ""), ": "),
                           nodes[i,nodevals[n]], "<br/>", sep = "")
      }


      nodelist <- paste0(nodelist, ");", sep = "")

      edge.num = edge.num + 1
    }

  }


  nodelist <- paste0(nodelist, "linkStyle default stroke-width:2px, fill:none;")
  nodelist <- paste0(nodelist, "classDef default fill:white,stroke:#333,stroke-width:2px;")
  nodelist <- paste0(nodelist, "classDef invisible fill:white,stroke:white,stroke-width:0px;")

  if(exists("hide.nodes")) {
    if(length(hide.nodes) > 0) {
      nodelist <- paste0(nodelist,
                         "class ", paste0(hide.nodes, collapse = ","), " invisible;")
    }
  }

  if(exists("hide.edges")) {
    if(length(hide.edges) > 0) {

      for(e in 1:length(hide.edges)) {
        nodelist <- paste0(nodelist, "linkStyle ", hide.edges[e], " fill:none, stroke-width:0px;")
      }
    }
  }

  ## TBD - make height/width either a parameter or auto-adjusting to size, or both
  DiagrammeR::DiagrammeR(nodelist, height = tree.height, width = tree.width)
}
