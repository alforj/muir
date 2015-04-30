#muir

The **muir** package allows users to explore a data.frame using the tree data structure with minimal effort by simply providing the data.frame columns to be explored. In addition, the **muir** package allows for more targeted tree data structures to be created with specific column criteria as a method for documenting and communicating the data structure and relationships within a data set. These data tree structures can be viewed within the *RStudio* console, standard browers, or saved as HTML for sharing using the **htmlwidgets** package.

The package legerages the infrastructure provided by [**DiagrammeR**](http://rich-iannone.github.io/DiagrammeR/).

## Installation

Install the development version of **muir** from GitHub using the **devtools** package.

```r
devtools::install_github('alforj/muir')
```

## Using muir

### Basic example
A basic example using **muir** to explore the **mtcars** data set showing the 'cyl' and 'carb' columns
and the relationship between them. Providing the "\*" qualifier (after the ":" separator) will
show the top occuring values for those columns up to the limit indicated in the *node.limit* value. By
default, the *node.limit* is set to 3 to curb run-away queries and unreadable trees. The value can be set
explicitly when calling the **muir** function. 

The resulting tree will be rendered starting with a level 0 node counting all rows in the data set. Each resulting level will be based on the columns provided by the user and will include nodes for each distinct value (up to the limit provided, in ascending order based on occurrences). Subsequent levels will carry the filters from previous
parent nodes forward. Percentages will be provided for each node (compared to the level 0 count) by default and can 
be turned off if not desired.

*tree.height* and *tree.width* values control how the tree is rendered and can be adjusted to best fit trees of
various depths and widths.


```r
library(muir)
data(mtcars)
mtTree <- muir(data = mtcars, node.levels = c("cyl:*", "carb:*"), 
               tree.height = 1200, tree.width = 800)
```

<!--html_preserve--><div id="htmlwidget-5761" style="width:800px;height:1200px;" class="DiagrammeR"></div>
<script type="application/json" data-for="htmlwidget-5761">{ "x": {
 "diagram": "graph LR;1(All<br/>n: 32<br/>%: 100.00<br/>);1-->2(cyl = 8<br/>n: 14<br/>%:  43.75<br/>);1-->3(cyl = 4<br/>n: 11<br/>%:  34.38<br/>);1-->4(cyl = 6<br/>n: 7<br/>%:  21.88<br/>);2-->5(carb = 2<br/>n: 4<br/>%:  12.50<br/>);2-->6(carb = 4<br/>n: 6<br/>%:  18.75<br/>);2-->7(carb = 1<br/>n: 0<br/>%:   0.00<br/>);3-->8(carb = 2<br/>n: 6<br/>%:  18.75<br/>);3-->9(carb = 4<br/>n: 0<br/>%:   0.00<br/>);3-->10(carb = 1<br/>n: 5<br/>%:  15.62<br/>);4-->11(carb = 2<br/>n: 0<br/>%:   0.00<br/>);4-->12(carb = 4<br/>n: 4<br/>%:  12.50<br/>);4-->13(carb = 1<br/>n: 2<br/>%:   6.25<br/>);linkStyle default stroke-width:2px, fill:none;classDef default fill:white,stroke:#333,stroke-width:2px;classDef invisible fill:white,stroke:white,stroke-width:0px;" 
},"evals": [  ] }</script><!--/html_preserve-->

### More complicated example
Instead of just returning top counts for columns provided in *node.levels*,
provide custom filter criteria and custom node titles in *level.criteria*
(*level.criteria* could also be read in from a stored file (e.g., a crtieria.csv) as a data.frame)

The **criteria** data.frame below includes the column names, operators, and associated values 
(e.g., "cyl" <= 4), and a node title to accompany each node generated for that filter criteria. 
Adding a "+" suffix after the column name in the *node.levels* parameter will add an extra 
"Other" node that will aggregate all values node already provided in the *level.criteria* value 
or for values below the *node.limit* provided.

Additional label values can be provided with the *label.vals* parameter using 
[**dplyr**](https://github.com/hadley/dplyr) summary functions. Custom labels for each value 
can be provided by adding custom text after the ":" separator. 

Lastly, the direction the tree is drawn can be changed from the default left-to-right to a 
top-to-bottom ("TB") rendering by providing a new value for *tree.dir*.


```r
criteria <- data.frame(col = c("cyl", "cyl", "carb"),
                       oper = c("<=", ">", "=="),
                       val = c(4, 4, 2),
                       title = c("Up to 4 Cylinders", "More than 4 Cylinders", "2 Carburetors"))

mtTree <- muir(data = mtcars, node.levels = c("cyl", "carb:+"),
               level.criteria = criteria,
               label.vals = c("min(wt):Min Weight", "max(wt):Max Weight"),
               tree.dir = "TB",
               tree.height = 400, tree.width = 800)
```

<!--html_preserve--><div id="htmlwidget-9097" style="width:800px;height:400px;" class="DiagrammeR"></div>
<script type="application/json" data-for="htmlwidget-9097">{ "x": {
 "diagram": "graph TB;1(All<br/>n: 32<br/>Min Weight: 1.51<br/>Max Weight: 5.42<br/>%: 100.00<br/>);1-->2(Up to 4 Cylinders<br/>n: 11<br/>Min Weight: 1.51<br/>Max Weight: 3.19<br/>%:  34.38<br/>);1-->3(More than 4 Cylinders<br/>n: 21<br/>Min Weight: 2.62<br/>Max Weight: 5.42<br/>%:  65.62<br/>);2-->4(2 Carburetors<br/>n: 6<br/>Min Weight: 1.51<br/>Max Weight: 3.19<br/>%:  18.75<br/>);2-->5(Other<br/>n: 5<br/>Min Weight: 1.84<br/>Max Weight: 2.46<br/>%:  15.62<br/>);3-->6(2 Carburetors<br/>n: 4<br/>Min Weight: 3.44<br/>Max Weight: 3.85<br/>%:  12.50<br/>);3-->7(Other<br/>n: 17<br/>Min Weight: 2.62<br/>Max Weight: 5.42<br/>%:  53.12<br/>);linkStyle default stroke-width:2px, fill:none;classDef default fill:white,stroke:#333,stroke-width:2px;classDef invisible fill:white,stroke:white,stroke-width:0px;" 
},"evals": [  ] }</script><!--/html_preserve-->

### More examples

More examples can be found in the *examples* in the **muir** function


```r
help(muir)
```
