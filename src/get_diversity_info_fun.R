#get_diversity_function
#function (
neon_div_object; scale = "plot"; trace_cover = 0.5; fix_unks = FALSE; families = NULL;spp = NULL 

    
    full_on_cover <- get_longform_cover(neon_div_object, scale = scale, 
        fix_unks = fix_unks)
    n_i <- full_on_cover %>% dplyr::group_by(plotID, subplotID, 
        year) %>% dplyr::mutate(total_cover = sum(cover)) %>% 
        dplyr::ungroup() %>% dplyr::group_by(plotID, subplotID, 
        year, nativeStatusCode) %>% dplyr::summarise(cover = sum(cover), 
        total_cover = first(total_cover)) %>% dplyr::ungroup() %>% 
        dplyr::mutate(rel_cover = cover/total_cover) %>% dplyr::ungroup() %>% 
        dplyr::filter(nativeStatusCode != "")
    n_i_cover <- n_i %>% dplyr::filter(nativeStatusCode != "" & 
        nativeStatusCode != "A" & nativeStatusCode != "NI") %>% 
        dplyr::select(plotID, subplotID, year, nativeStatusCode, 
            cover) %>% tidyr::pivot_wider(names_from = nativeStatusCode, 
        values_from = cover, values_fill = list(cover = 0)) %>% 
        dplyr::rename(cover_native = N, cover_exotic = I, cover_unk = UNK)
    n_i_rel_cover <- n_i %>% dplyr::filter(nativeStatusCode != 
        "" & nativeStatusCode != "A" & nativeStatusCode != "NI") %>% 
        dplyr::select(plotID, subplotID, year, nativeStatusCode, 
            rel_cover) %>% tidyr::pivot_wider(names_from = nativeStatusCode, 
        values_from = rel_cover, values_fill = list(rel_cover = 0)) %>% 
        dplyr::rename(rel_cover_native = N, rel_cover_exotic = I, 
            rel_cover_unk = UNK)
    byfam <- full_on_cover %>% dplyr::group_by(plotID, subplotID, 
        year) %>% dplyr::mutate(total_cover = sum(cover)) %>% 
        dplyr::ungroup() %>% dplyr::group_by(plotID, subplotID, 
        year, family) %>% dplyr::summarise(cover = sum(cover), 
        total_cover = first(total_cover)) %>% dplyr::ungroup() %>% 
        dplyr::mutate(rel_cover = cover/total_cover) %>% dplyr::ungroup() %>% 
        dplyr::filter(family %in% families)
    rcf <- byfam %>% dplyr::select(plotID, subplotID, year, family, 
        rel_cover) %>% tidyr::pivot_wider(names_from = family, 
        names_prefix = "rc_", values_from = (rel_cover), values_fill = list(rel_cover = 0))
    cf <- byfam %>% dplyr::select(plotID, subplotID, year, family, 
        cover) %>% tidyr::pivot_wider(names_from = family, names_prefix = "cover_", 
        values_from = (cover), values_fill = list(cover = 0))
    nspp_byfam <- full_on_cover %>% dplyr::group_by(plotID, subplotID, 
        year) %>% dplyr::mutate(total_cover = sum(cover)) %>% 
        dplyr::ungroup() %>% dplyr::group_by(plotID, subplotID, 
        year, family, nativeStatusCode) %>% dplyr::summarise(nspp = length(unique(scientificName))) %>% 
        dplyr::ungroup() %>% dplyr::filter(nativeStatusCode != 
        "UNK", family %in% families) %>% tidyr::pivot_wider(names_from = c(family, 
        nativeStatusCode), names_prefix = "nspp_", values_from = (nspp), 
        values_fill = list(nspp = 0))
    bysp <- full_on_cover %>% dplyr::group_by(plotID, subplotID, 
        year) %>% dplyr::mutate(total_cover = sum(cover)) %>% 
        dplyr::ungroup() %>% dplyr::group_by(plotID, subplotID, 
        year, scientificName) %>% dplyr::summarise(cover = sum(cover), 
        total_cover = first(total_cover)) %>% dplyr::ungroup() %>% 
        dplyr::mutate(rel_cover = cover/total_cover) %>% dplyr::ungroup() %>% 
        dplyr::mutate(gen = str_split(scientificName, pattern = " ", 
            simplify = TRUE)[, 1], sp = str_split(scientificName, 
            pattern = " ", simplify = TRUE)[, 2], gen_sp = str_c(gen, 
            " ", sp)) %>% dplyr::mutate(genus = str_split(scientificName, 
        pattern = " ", simplify = TRUE)[, 1], species = str_split(scientificName, 
        pattern = " ", simplify = TRUE)[, 2], gen_sp = str_c(genus, 
        " ", species)) %>% dplyr::filter(gen_sp %in% spp)
    rc_sp <- bysp %>% dplyr::select(plotID, subplotID, year, 
        gen_sp, rel_cover) %>% dplyr::mutate(gen_sp = str_replace(gen_sp, 
        " ", "_")) %>% dplyr::group_by(plotID, subplotID, year, 
        gen_sp) %>% dplyr::summarise(rel_cover = mean(rel_cover)) %>% 
        dplyr::ungroup() %>% tidyr::pivot_wider(names_from = gen_sp, 
        names_prefix = "rc_", values_from = (rel_cover), values_fill = list(rel_cover = 0))
    c_sp <- bysp %>% dplyr::select(plotID, subplotID, year, gen_sp, 
        cover) %>% dplyr::mutate(gen_sp = str_replace(gen_sp, 
        " ", "_")) %>% dplyr::group_by(plotID, subplotID, year, 
        gen_sp) %>% dplyr::summarise(cover = mean(cover)) %>% 
        dplyr::ungroup() %>% tidyr::pivot_wider(names_from = gen_sp, 
        names_prefix = "cover_", values_from = (cover), values_fill = list(cover = 0))
    exotic_grass <- full_on_cover %>% dplyr::group_by(plotID, 
        subplotID, year) %>% dplyr::mutate(total_cover = sum(cover)) %>% 
        dplyr::ungroup() %>% dplyr::group_by(plotID, subplotID, 
        year, family, nativeStatusCode) %>% dplyr::summarise(cover = sum(cover), 
        total_cover = first(total_cover)) %>% dplyr::ungroup() %>% 
        dplyr::mutate(rel_cover = cover/total_cover) %>% dplyr::ungroup() %>% 
        dplyr::filter(family %in% families)
    rc_ig <- exotic_grass %>% dplyr::select(plotID, subplotID, 
        year, family, nativeStatusCode, rel_cover) %>% dplyr::filter(nativeStatusCode == 
        "I") %>% tidyr::pivot_wider(names_from = family, names_prefix = "rc_exotic_", 
        values_from = (rel_cover), values_fill = list(rel_cover = 0)) %>% 
        dplyr::select(-nativeStatusCode)
    rc_ng <- exotic_grass %>% dplyr::select(plotID, subplotID, 
        year, family, nativeStatusCode, rel_cover) %>% dplyr::filter(nativeStatusCode == 
        "N") %>% tidyr::pivot_wider(names_from = family, names_prefix = "rc_native_", 
        values_from = (rel_cover), values_fill = list(rel_cover = 0)) %>% 
        dplyr::select(-nativeStatusCode)
    c_ig <- exotic_grass %>% dplyr::select(plotID, subplotID, 
        year, family, nativeStatusCode, cover) %>% dplyr::filter(nativeStatusCode == 
        "I") %>% tidyr::pivot_wider(names_from = family, names_prefix = "cover_exotic_", 
        values_from = (cover), values_fill = list(cover = 0)) %>% 
        dplyr::select(-nativeStatusCode)
    c_ng <- exotic_grass %>% dplyr::select(plotID, subplotID, 
        year, family, nativeStatusCode, cover) %>% dplyr::filter(nativeStatusCode == 
        "N") %>% tidyr::pivot_wider(names_from = family, names_prefix = "cover_native_", 
        values_from = (cover), values_fill = list(cover = 0)) %>% 
        dplyr::select(-nativeStatusCode)
    vegan_friendly_div <- full_on_cover %>% dplyr::group_by(plotID, 
        subplotID, taxonID, year, nativeStatusCode) %>% dplyr::summarise(cover = sum(cover, 
        na.rm = TRUE)) %>% dplyr::ungroup() %>% dplyr::mutate(taxonID = as.character(taxonID), 
        plotID = as.character(plotID), nativeStatusCode = as.character(nativeStatusCode)) %>% 
        dplyr::filter(nchar(as.character(taxonID)) > 0, nativeStatusCode != 
            "", nativeStatusCode != "A", nativeStatusCode != 
            "NI") %>% dplyr::group_by(plotID, subplotID, year, 
        nativeStatusCode) %>% tidyr::spread(taxonID, cover, fill = 0) %>% 
        dplyr::ungroup()
    vegan_friendly_div$shannon = vegan::diversity(vegan_friendly_div[5:ncol(vegan_friendly_div)])
    vegan_friendly_div$nspp = vegan::specnumber(vegan_friendly_div[5:ncol(vegan_friendly_div)])
    nspp <- vegan_friendly_div %>% dplyr::select(plotID, subplotID, 
        year, nativeStatusCode, nspp) %>% tidyr::pivot_wider(names_from = nativeStatusCode, 
        values_from = nspp, values_fill = list(nspp = 0)) %>% 
        dplyr::rename(nspp_native = N, nspp_exotic = I, nspp_unk = UNK)
    shannon <- vegan_friendly_div %>% dplyr::select(plotID, subplotID, 
        year, nativeStatusCode, shannon) %>% tidyr::pivot_wider(names_from = nativeStatusCode, 
        values_from = shannon, values_fill = list(shannon = 0)) %>% 
        dplyr::rename(shannon_native = N, shannon_exotic = I, 
            shannon_unk = UNK)
    vegan_friendly_div_total <- full_on_cover %>% dplyr::group_by(plotID, 
        subplotID, taxonID, year) %>% dplyr::summarise(cover = sum(cover)) %>% 
        dplyr::ungroup() %>% dplyr::mutate(taxonID = as.character(taxonID), 
        plotID = as.character(plotID)) %>% dplyr::filter(nchar(as.character(taxonID)) > 
        0) %>% dplyr::group_by(plotID, subplotID, year) %>% tidyr::spread(taxonID, 
        cover, fill = 0) %>% dplyr::ungroup()
    div_total <- dplyr::select(vegan_friendly_div_total, plotID, 
        subplotID, year)
    div_total$shannon_total = vegan::diversity(vegan_friendly_div_total[4:ncol(vegan_friendly_div_total)])
    div_total$nspp_total = vegan::specnumber(vegan_friendly_div_total[4:ncol(vegan_friendly_div_total)])
    final_table <- dplyr::left_join(nspp, shannon, by = c("plotID", 
        "subplotID", "year")) %>% dplyr::left_join(n_i_cover, 
        by = c("plotID", "subplotID", "year")) %>% dplyr::left_join(n_i_rel_cover, 
        by = c("plotID", "subplotID", "year")) %>% dplyr::left_join(div_total, 
        by = c("plotID", "subplotID", "year")) %>% dplyr::left_join(rcf, 
        by = c("plotID", "subplotID", "year")) %>% dplyr::left_join(rc_ig, 
        by = c("plotID", "subplotID", "year")) %>% dplyr::left_join(c_ig, 
        by = c("plotID", "subplotID", "year")) %>% dplyr::left_join(cf, 
        by = c("plotID", "subplotID", "year")) %>% dplyr::left_join(rc_ng, 
        by = c("plotID", "subplotID", "year")) %>% dplyr::left_join(c_ng, 
        by = c("plotID", "subplotID", "year")) %>% dplyr::left_join(rc_sp, 
        by = c("plotID", "subplotID", "year")) %>% dplyr::left_join(c_sp, 
        by = c("plotID", "subplotID", "year")) %>% dplyr::left_join(nspp_byfam, 
        by = c("plotID", "subplotID", "year")) %>% dplyr::mutate(site = str_sub(plotID, 
        1, 4), scale = scale, invaded = if_else(cover_exotic > 
        0, "invaded", "not_invaded")) %>% dplyr::mutate(scale = factor(scale, 
        levels = c("1m", "10m", "100m", "plot")))
    final_table[is.na(final_table)] <- 0
    return(final_table)
#}
