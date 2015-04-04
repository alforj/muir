build_tree <- function(data) {


  'select' <- NULL; rm("select")
  'starts_with' <- NULL; rm("starts_with")
  'parent' <- NULL; rm("parent")


        nodelist <- "graph LR;"
        nodevals <- names(select(data, starts_with("v_")))


        ## Build parent (head) node

                headnode <- filter(data, parent == "None")
                if (nrow(headnode) != 1) stop("There is either no head node or more than one head node.")

                nodelist <- paste0(nodelist, headnode$node, "(", headnode$label, "<br/>", sep = "")
                for (n in 1:length(nodevals)) {

                        nodelist <- paste0(nodelist, paste0(stringr::str_replace(nodevals[n], "v_", ""), ": "), headnode[,nodevals[n]], "<br/>", sep = "")
                }

                nodelist <- paste0(nodelist, ");", sep = "")

        ## Build non-Zero leaf nodes (for when user does not want to show children of parents with no values)
                nodes <- filter(data, parent != "None")


                if (nrow(nodes) < 1) return(DiagrammeR::DiagrammeR(nodelist)) ## if no data, spit back an emtpy diagram

                edge.num <- 0
                for (i in 1:nrow(nodes)) {

                        if(is.na(nodes$label[i])) {

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
                                                   "(", nodes$label[i], "<br/>", sep = "")


                                        for (n in 1:length(nodevals)) {

                                                nodelist <- paste0(nodelist, paste0(stringr::str_replace(nodevals[n], "v_", ""), ": "), nodes[i,nodevals[n]], "<br/>", sep = "")
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

                DiagrammeR::DiagrammeR(nodelist) ## TBD - make height/width either a parameter or auto-adjusting to size, or both
}
