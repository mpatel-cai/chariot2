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


nodes.and.edges <-
  setClass("nodes.and.edges",
           list(nodes = "nodes",
                edges = "edges"))

setMethod("print",
          signature(x = "nodes"),
          function(x,...) print(x@data,...))

setMethod("print",
          signature(x = "nodes"),
          function(x,...) print(x@data,...))

setMethod("print",
          signature(x = "nodes.and.edges"),
          function(x, ...) print(list(x@Nodes,x@Edges)))
