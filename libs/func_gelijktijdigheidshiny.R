GelijktijdigheidShiny <- function(df, van_prio, van_gebied, van_voertuig, naar_prio, naar_gebied, naar_voertuig) {
  # Aangepaste versie van de Gelijktijdigheid functie voor gebruik in een Shiny dashboard.
  #
  # LET OP DIT IS EEN ERG QUICK & DIRTY TOEPASSING/DEMO: NIET GEREED VOOR PRODUCTIE
  # ER IS WEINIG TOT GEEN AANDACHT BESTEED AAN VALIDATIE VAN INPUT PARAMETERS
  #
  # Guido Legemaate
  # Brandweer Amsterdam-Amstelland
  # Gereviseerde versie d.d. 12-2-2021
  # g.legemaate@brandweeraa.nl
  #
  # Copyright (c) 2021 G.A.G. Legemaate, MIT License
  #
  # Args:
  #   df: input dataframe (please note that as of now it is in a AA custom data vault format only)
  #   van_prio: incidenten met welke prio gelden als basis (veroorzaken gelijktijdigheid)
  #   van_gebied: incidenten in welk verzorgingsgebied gelden als basis (veroorzaken gelijktijdigheid)
  #   van_voertuig: incidenten met voertuigen van welk type gelden als basis (veroorzaken gelijktijdigheid)
  #   naar_prio: incidenten met welke prio gelden als doel (zijn gelijktijdig)
  #   naar_gebied: incidenten in welk verzorgingsgebied gelden als doel (zijn gelijktijdig)
  #   naar_voertuig: incidenten met voertuigen van welk type gelden als doel (zijn gelijktijdig)
  #
  # Returns:
  #   Dataframe met gelijktijdige incidenten. Let op dat zowel de incidenten die veroorzaken als óók 
  #   de veroorzaakte incidenten in deze dataset zitten opgenomen (zie de extra kolommen aan het eind).
  #   Afhankelijk van het perspectief kan een incident ook zowel veroorzaker als veroorzaakte zijn, wat
  #   het complex maakt bij het bepalen van een bepaalde waarde.

  # progress bar
  withProgress(message = "Aan het rekenen...", value = 0.1, {
  
  # dataset van_incidenten prepareren
	van_incidenten <- subset(df, dim_prioriteit_prio %in% van_prio & kazerne_groep %in% van_gebied)
	van_incidenten <- van_incidenten[ grep(van_voertuig, van_incidenten$voertuig_groep_uniek), ]
	van_incidenten <- van_incidenten[ order(van_incidenten$inzet_start_inzet_datumtijd), ]
	rownames(van_incidenten) <- NULL
	
  # dataset naar_incidenten prepareren
	naar_incidenten <- subset(df, dim_prioriteit_prio %in% naar_prio & kazerne_groep %in% naar_gebied)
	naar_incidenten <- naar_incidenten[ grep(naar_voertuig, naar_incidenten$voertuig_groep_uniek), ]
  naar_incidenten <- naar_incidenten[ order(naar_incidenten$inzet_start_inzet_datumtijd), ]
  rownames(naar_incidenten) <- NULL
  
  # gelijktijdigheid - boolean
  incProgress(0.7, detail = "op zoek naar gelijktijdigheid")
  van_incidenten$gelijktijdigheid <- sapply(van_incidenten$interval_inzet,
                                            function(x) {
                                              any(
                                                int_overlaps(x,
                                                             naar_incidenten$interval_inzet[ naar_incidenten$interval_inzet != x ]))
                                            })
  
  van_incidenten <- van_incidenten[ van_incidenten$gelijktijdigheid == TRUE, ]
  
  # gelijktijdigheid - counter
  incProgress(0.8, detail = "gelijktijdigheid tellertje toevoegen")
  van_incidenten$gelijk_counter <- sapply(van_incidenten$interval_inzet,
                                          function(x) {
                                            length(
                                               which(
                                                 int_overlaps(x,
                                                              naar_incidenten$interval_inzet[ naar_incidenten$interval_inzet != x ])))
                                          })
  
  # gelijktijdigheid - overlap
  incProgress(0.9, detail = "overlappende incident id's erbij zoeken")
  van_incidenten$gelijk_overlap <- sapply(van_incidenten$hub_incident_id,
                                          function(x) {
                                            naar_incidenten$hub_incident_id[ naar_incidenten$hub_incident_id != x ][which(int_overlaps(van_incidenten$interval_inzet[ van_incidenten$hub_incident_id == x ], naar_incidenten$interval_inzet[ naar_incidenten$hub_incident_id != x ]))]
                                          })
  
  # OUD! FOUT! Ter leering ende vermaeck only
  ###########################################
  # van_incidenten$gelijk_overlap <- sapply(van_incidenten$interval_inzet,
  #                                          function(x) {
  #                                            naar_incidenten$hub_incident_id[which(
  #                                              int_overlaps(x,
  #                                                           naar_incidenten$interval_inzet[ naar_incidenten$interval_inzet != x ])) ]
  #                                         })
  ###########################################
  
  incProgress(1, detail = "klaar!")
  return(van_incidenten)
  })
}
