ExpKeywords <- list(

"ControlKeywords" = c(
"not",
"PBS",
"non",
"DMSO",
"untreated",
"before",
"prior",
"control",
"medium",
"responder",
"vehicle",
"sensitive",
"inphase",
"placebo",
"treatment[[:punct:]]none",
"no[[:punct:]]treatment"
),

"TreatmentKeywords" = c(
"treat",
"after",
"agent"),

"TitrationKeywords" = c(
"[[:digit:]]+\\s*nm",
"[[:digit:]]+\\s*um",
"[[:digit:]]+\\s*mm",
"[[:digit:]]+\\s*ug",
"[[:digit:]]+\\s*ng"
),

"TimeUnits" = c(
"hour",
"min",
"minute",
"sec",
"second",
"day",
"week",
"month",
"mon",
"year",
"yr"
),

"TimeStudyKeywords" = c(
"time\\s*points*",
"time\\s*series*"),


"SexKeywords"  = c(
"male","female",
"M","F"),


"SampleColumns"  = c(
"rep\\s*[[:digit:]]",
"biological\\s*replicate[[:digit:]]",
"biological\\s*rep[[:digit:]]",
"donor\\s*[[0-9]",
"patient no\\s*[[:digit:]]",
"patient\\s*[[:digit:]]"),

"StudySkip" = c(
"siCONTROL",
"loss\\s*of\\s*[[0-9a-z]]\\s*function",
"LOF",
"siRNA", 
"CRISPR",
"cas9")

)


