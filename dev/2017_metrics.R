bio_fam_metrics <- function(x) {
  metrics.key <- x %>%
    select(unique_id) %>%
    distinct()
  
  metrics.key %>%
    dplyr::mutate(
      aspt_mod = taxa_tol_index(
        long.df = x,
        unique.id.col = unique_id,
        count.col = reporting_value,
        tol.col = aspt
      ),
      pct_gastro_oligo = taxa_pct(
        long.df = x,
        unique.id.col = unique_id,
        count.col = reporting_value,
        taxon.col = class,
        taxon = c("gastropoda", "oligochatea")
      ),
      pct_diptera = taxa_pct(
        long.df = x,
        unique.id.col = unique_id,
        count.col = reporting_value,
        taxon.col = order,
        taxon = "diptera"
      ),
      gold = 1 - (pct_gastro_oligo + pct_diptera),
      hbi = taxa_tol_index(
        long.df = x,
        unique.id.col = unique_id,
        count.col = reporting_value,
        tol.col = tol_val
      ),
      pct_arthropoda = taxa_pct(
        long.df = x,
        unique.id.col = unique_id,
        count.col = reporting_value,
        taxon.col = phylum,
        taxon = "arthropoda"
      ),
      pct_burrow = taxa_pct(
        long.df = x,
        unique.id.col = unique_id,
        count.col = reporting_value,
        taxon.col = habit,
        taxon = "burrow"
      ),
      pct_chironomidae = taxa_pct(
        long.df = x,
        unique.id.col = unique_id,
        count.col = reporting_value,
        taxon.col = family,
        taxon = "chironomidae"
      ),
      pct_cling = taxa_pct(
        long.df = x,
        unique.id.col = unique_id,
        count.col = reporting_value,
        taxon.col = habit,
        taxon = "cling"
      ),
      pct_burrow = taxa_pct(
        long.df = x,
        unique.id.col = unique_id,
        count.col = reporting_value,
        taxon.col = habit,
        taxon = "burrow"
      ),
      pct_collect = taxa_pct(
        long.df = x,
        unique.id.col = unique_id,
        count.col = reporting_value,
        taxon.col = ffg,
        taxon = c("filter", "gather")
      ),
      pct_cote = taxa_pct(
        long.df = x,
        unique.id.col = unique_id,
        count.col = reporting_value,
        taxon.col = order,
        taxon = c("coleoptera",
                  "odonata",
                  "trichoptera",
                  "ephemeroptera")
      ),
      pct_ephemeroptera = taxa_pct(
        long.df = x,
        unique.id.col = unique_id,
        count.col = reporting_value,
        taxon.col = order,
        taxon = "ephemeroptera"
      ),
      pct_baetidae = taxa_pct(
        long.df = x,
        unique.id.col = unique_id,
        count.col = reporting_value,
        taxon.col = family,
        taxon = "baetidae"
      ),
      pct_ephemeroptera_no_baetid = pct_ephemeroptera - pct_baetidae,
      pct_ept = taxa_pct(
        long.df = x,
        unique.id.col = unique_id,
        count.col = reporting_value,
        taxon.col = order,
        taxon = c("ephemeroptera",
                  "plecoptera",
                  "trichoptera")
      ),
      pct_hydropsychidae = taxa_pct(
        long.df = x,
        unique.id.col = unique_id,
        count.col = reporting_value,
        taxon.col = family,
        taxon = "hydropsychidae"
      ),
      pct_ept_no_hydro = pct_ept - pct_hydropsychidae,
      pct_euholognatha = taxa_pct(
        long.df = x,
        unique.id.col = unique_id,
        count.col = reporting_value,
        taxon.col = suborder,
        taxon = "euholognatha"
      ),
      pct_filter = taxa_pct(
        long.df = x,
        unique.id.col = unique_id,
        count.col = reporting_value,
        taxon.col = ffg,
        taxon = "filter"
      ),
      pct_heptageniidae = taxa_pct(
        long.df = x,
        unique.id.col = unique_id,
        count.col = reporting_value,
        taxon.col = family,
        taxon = "heptageniidae"
      ),
      pct_hexapoda = taxa_pct(
        long.df = x,
        unique.id.col = unique_id,
        count.col = reporting_value,
        taxon.col = subphylum,
        taxon = "hexapoda"
      ),
      pct_hydro_ept = ifelse(pct_ept == 0,
                             0,
                             (pct_hydropsychidae / pct_ept) * 100),
      pct_intol_0_3 = taxa_pct(
        long.df = x,
        unique.id.col = unique_id,
        count.col = reporting_value,
        taxon.col = tol_val,
        taxon = 0:3
      ),
      pct_intol_0_4 = taxa_pct(
        long.df = x,
        unique.id.col = unique_id,
        count.col = reporting_value,
        taxon.col = tol_val,
        taxon = 0:4
      ),
      pct_amph_iso = taxa_pct(
        long.df = x,
        unique.id.col = unique_id,
        count.col = reporting_value,
        taxon.col = order,
        taxon = c("amphipoda", "isopoda")
      ),
      pct_ephemerellidae = taxa_pct(
        long.df = x,
        unique.id.col = unique_id,
        count.col = reporting_value,
        taxon.col = family,
        taxon = "ephemerellidae"
      ),
      pct_limestone = pct_amph_iso + pct_ephemerellidae,
      pct_mod_tol_4_6 = taxa_pct(
        long.df = x,
        unique.id.col = unique_id,
        count.col = reporting_value,
        taxon.col = tol_val,
        taxon = 4:6
      ),
      pct_trichoptera = taxa_pct(
        long.df = x,
        unique.id.col = unique_id,
        count.col = reporting_value,
        taxon.col = order,
        taxon = "trichoptera"
      ),
      pct_non_hydrop_trichoptera = pct_trichoptera - pct_hydropsychidae,
      pct_odonata = taxa_pct(
        long.df = x,
        unique.id.col = unique_id,
        count.col = reporting_value,
        taxon.col = order,
        taxon = "odonata"
      ),
      pct_pisciforma = taxa_pct(
        long.df = x,
        unique.id.col = unique_id,
        count.col = reporting_value,
        taxon.col = suborder,
        taxon = "pisciforma"
      ),
      pct_predator = taxa_pct(
        long.df = x,
        unique.id.col = unique_id,
        count.col = reporting_value,
        taxon.col = ffg,
        taxon = "predator"
      ),
      pct_pterygota = taxa_pct(
        long.df = x,
        unique.id.col = unique_id,
        count.col = reporting_value,
        taxon.col = subclass,
        taxon = "pterygota"
      ),
      pct_retreat_caddisfly = taxa_pct(
        long.df = x,
        unique.id.col = unique_id,
        count.col = reporting_value,
        taxon.col = suborder,
        taxon = "annulipalpia"
      ),
      pct_scrape = taxa_pct(
        long.df = x,
        unique.id.col = unique_id,
        count.col = reporting_value,
        taxon.col = ffg,
        taxon = "scrape"
      ),
      pct_sprawl = taxa_pct(
        long.df = x,
        unique.id.col = unique_id,
        count.col = reporting_value,
        taxon.col = habit,
        taxon = "sprawl"
      ),
      pct_systellognatha = taxa_pct(
        long.df = x,
        unique.id.col = unique_id,
        count.col = reporting_value,
        taxon.col = suborder,
        taxon = "systellognatha"
      ),
      pct_tolerant_7_10 = taxa_pct(
        long.df = x,
        unique.id.col = unique_id,
        count.col = reporting_value,
        taxon.col = tol_val,
        taxon = 7:10
      ),
      pct_urban_intol = taxa_pct(
        long.df = x,
        unique.id.col = unique_id,
        count.col = reporting_value,
        taxon.col = suborder,
        taxon = c(
          "aeshnidae",
          "ameletidae",
          "asellidae",
          "athericidae",
          "baetidae",
          "brachycentridae",
          "caenidae",
          "calamoceratidae",
          "cambaridae",
          "capniidae",
          "ceratopogonidae",
          "chironomidae",
          "chloroperlidae",
          "cordulegastridae",
          "corduliidae",
          "corydalidae",
          "crangonyctidae",
          "elmidae",
          "ephemerellidae",
          "ephemeridae",
          "glossosomatidae",
          "gomphidae",
          "heptageniidae",
          "hydropsychidae",
          "hydroptilidae",
          "isonychiidae",
          "lepidostomatidae",
          "leptophlebiidae",
          "leuctridae",
          "libellulidae",
          "metretopodidae",
          "nemouridae",
          "odontoceridae",
          "peltoperlidae",
          "perlidae",
          "perlodidae",
          "philopotamidae",
          "phryganeidae",
          "polycentropodidae",
          "polymitarcyidae",
          "potamanthidae",
          "psephenidae",
          "pteronarcyidae",
          "rhyacophilidae",
          "sericostomatidae",
          "sialidae",
          "simuliidae",
          "stratiomyidae",
          "tabanidae",
          "taeniopterygidae",
          "tipulidae",
          "uenoidae",
          "viviparidae"
        )
      ),
      # Richness/Diversity metrics require rarefied data.
      margalefs = taxa_div(
        long.df = x,
        unique.id.col = unique_id,
        count.col = reporting_value,
        # low.taxa.col = NULL,
        high.taxa.col = family,
        job = "margalef"
      ),
      # pct_ept_rich = taxa_pct_rich(
      #   long.df = x,
      #   unique.id.col = unique_id,
      #   count.col = reporting_value,
      #   taxon.col = order,
      #   taxon = c("ephemeroptera", "plecoptera", "trichotpera"),
      #   high.res.taxa.col = family,
      #   exclusion.col = NULL,
      #   exclusion.vec = NULL
      # ),
      # pct_ept_rich_no_tol = taxa_pct_rich(
      #   long.df = x,
      #   unique.id.col = unique_id,
      #   count.col = reporting_value,
      #   taxon.col = order,
      #   taxon = c("ephemeroptera", "plecoptera", "trichotpera"),
      #   high.res.taxa.col = family,
      #   exclusion.col = tol_val,
      #   exclusion.vec = 7:10
      # ),
      pielou = taxa_div(
        long.df = x,
        unique.id.col = unique_id,
        count.col = reporting_value,
        # low.taxa.col = NULL,
        high.taxa.col = family,
        job = "pielou"
      ),
      rich_burrow = taxa_rich(
        long.df = x,
        unique.id.col = unique_id,
        low.taxa.col = habit,
        high.taxa.col = family,
        taxon = "burrow"
      ),
      rich_climb = taxa_rich(
        long.df = x,
        unique.id.col = unique_id,
        low.taxa.col = habit,
        high.taxa.col = family,
        taxon = "climb"
      ),
      rich_cling = taxa_rich(
        long.df = x,
        unique.id.col = unique_id,
        low.taxa.col = habit,
        high.taxa.col = family,
        taxon = "cling"
      ),
      rich_collect = taxa_rich(
        long.df = x,
        unique.id.col = unique_id,
        low.taxa.col = ffg,
        high.taxa.col = family,
        taxon = c("filter", "gather")
      ),
      rich_ephemeroptera = taxa_rich(
        long.df = x,
        unique.id.col = unique_id,
        low.taxa.col = order,
        high.taxa.col = family,
        taxon = "ephemeroptera"
      ),
      rich_ept = taxa_rich(
        long.df = x,
        unique.id.col = unique_id,
        low.taxa.col = order,
        high.taxa.col = family,
        taxon = c("ephemeroptera",
                  "plecoptera",
                  "trichoptera")
      ),
      rich_filter = taxa_rich(
        long.df = x,
        unique.id.col = unique_id,
        low.taxa.col = ffg,
        high.taxa.col = family,
        taxon = "filter"
      ),
      rich_gather = taxa_rich(
        long.df = x,
        unique.id.col = unique_id,
        low.taxa.col = ffg,
        high.taxa.col = family,
        taxon = "gather"
      ),
      rich_intol = taxa_rich(
        long.df = x,
        unique.id.col = unique_id,
        low.taxa.col = tol_val,
        high.taxa.col = family,
        taxon = 0:3
      ),
      rich_modtol = taxa_rich(
        long.df = x,
        unique.id.col = unique_id,
        low.taxa.col = tol_val,
        high.taxa.col = family,
        taxon = 4:6
      ),
      rich_plecoptera = taxa_rich(
        long.df = x,
        unique.id.col = unique_id,
        low.taxa.col = order,
        high.taxa.col = family,
        taxon = "plecoptera"
      ),
      rich_predator = taxa_rich(
        long.df = x,
        unique.id.col = unique_id,
        low.taxa.col = ffg,
        high.taxa.col = family,
        taxon = "predator"
      ),
      rich_shred = taxa_rich(
        long.df = x,
        unique.id.col = unique_id,
        low.taxa.col = ffg,
        high.taxa.col = family,
        taxon = "shred"
      ),
      rich_sprawl = taxa_rich(
        long.df = x,
        unique.id.col = unique_id,
        low.taxa.col = habit,
        high.taxa.col = family,
        taxon = "sprawl"
      ),
      rich_tol = taxa_rich(
        long.df = x,
        unique.id.col = unique_id,
        low.taxa.col = tol_val,
        high.taxa.col = family,
        taxon = 7:10
      ),
      rich_trichoptera = taxa_rich(
        long.df = x,
        unique.id.col = unique_id,
        low.taxa.col = order,
        high.taxa.col = family,
        taxon = "trichoptera"
      )
    )
}
