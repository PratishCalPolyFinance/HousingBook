library(DiagrammeR)
library(DiagrammeRsvg)
library(rsvg)

graph <- grViz("
digraph permit_process {
  graph [layout = dot, rankdir = TB, nodesep = 1.5, ranksep = 2.0]

  node [shape = box, style = filled, fontname = Helvetica, fontsize = 16]

  Start [label = 'Start:\\nSubmit Permit Application', shape = ellipse, fillcolor = lightblue, fontsize = 18]
  A [label = 'Application Submission', fillcolor = '#CCCCCC']
  B [label = 'San Francisco\\nPlanning Department\\nMedian: 289 days', fillcolor = '#E69F00']
  C [label = 'Department of\\nBuilding Inspection (DBI)\\nMedian: 259 days', fillcolor = '#56B4E9']
  D [label = 'Department of\\nPublic Works (DPW)\\nMedian: 137 days', fillcolor = '#009E73']
  E [label = 'Public Utilities\\nCommission (PUC)\\nMedian: 43 days', fillcolor = '#F0E442']
  F [label = 'San Francisco\\nFire Department\\n(if applicable)\\nMedian: 127 days', fillcolor = '#0072B2']
  G [label = 'Final Permit Issuance\\nTotal: >620 days', fillcolor = '#56B4E9']
  Note [label = 'Note:\\nCoordination issues may delay the process', shape = note, fillcolor = '#CC79A7', fontcolor = white, fontsize = 16]

  Start -> A
  A -> B
  B -> C
  C -> D
  D -> E
  E -> F
  E -> G [label = 'If no Fire Dept.']
  F -> G
  G -> Note

  B -> A [style = dashed, label = 'Revisions']
  C -> A [style = dashed]
  D -> A [style = dashed]
  E -> A [style = dashed]
  F -> A [style = dashed]
}
")

# Ensure output folder exists
if (!dir.exists("img")) dir.create("img")

# Export as PNG (fixed layout, no clipping)
graph_svg <- export_svg(graph)
rsvg_png(charToRaw(graph_svg), file = "img/process.png")
