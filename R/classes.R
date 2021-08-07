#' nodes S4 class
#' @slot data nodes dataframe
#' @slot required_fields required fields for a nodes s4 class object
#' @slot attribute_fields fields denoting node attributes

nodes <-
  setClass(
    Class = "nodes",
    slots = c(data = "data.frame",
              required_fields = "character",
              attribute_fields = "character"),
    prototype = list(data = tibble::tibble(),
                     required_fields = c("id", "type", "label"),
                     attribute_fields =   c(
                         'shape',
                         'style',
                         'penwidth',
                         'color',
                         'fillcolor',
                         'image',
                         'fontname',
                         'fontsize',
                         'fontcolor',
                         'peripheries',
                         'height',
                         'width',
                         'x',
                         'y',
                         'group',
                         'tooltip',
                         'xlabel',
                         'URL',
                         'sides',
                         'orientation',
                         'skew',
                         'distortion',
                         'gradientangle',
                         'fixedsize',
                         'labelloc',
                         'margin'
                       )
                     ))

#' edges S4 class
#' @slot data edges dataframe
#' @slot required_fields required fields for a edges s4 class object
#' @slot attribute_fields fields denoting edge attributes

edges <-
setClass(
  Class = "edges",
  slots = c(data = "data.frame",
            required_fields = "character",
            attribute_fields = "character"),
  prototype = list(data = tibble::tibble(),
                   required_fields = c("from", "to", "label", "rel"),
                   attribute_fields =   c(
                     'style',
                     'penwidth',
                     'color',
                     'arrowsize',
                     'arrowhead',
                     'arrowtail',
                     'fontname',
                     'fontsize',
                     'fontcolor',
                     'len',
                     'tooltip',
                     'URL',
                     'label',
                     'labelfontname',
                     'labelfontsize',
                     'labelfontcolor',
                     'labeltooltip',
                     'labelURL',
                     'edgetooltip',
                     'edgeURL',
                     'dir',
                     'headtooltip',
                     'headURL',
                     'headclip',
                     'headlabel',
                     'headport',
                     'tailtooltip',
                     'tailURL',
                     'tailclip',
                     'taillabel',
                     'tailport',
                     'decorate'
                   ))
)

#' nodes.and.edges S4 class

nodes.and.edges <-
  setClass("nodes.and.edges",
           list(nodes = "nodes",
                edges = "edges"))


#' raw.omop.data S4 class

omop.data <-
  setClass("omop.data",
           list(data = "data.frame"))

#' @importClassesFrom DiagrammeR
omop.graph <-
  setClass("omop.graph")





setMethod("print",
          signature(x = "nodes"),
          function(x,...) print(x@data,...))

setMethod("print",
          signature(x = "nodes"),
          function(x,...) print(x@data,...))

setMethod("print",
          signature(x = "nodes.and.edges"),
          function(x, ...) print(list(x@Nodes,x@Edges)))
