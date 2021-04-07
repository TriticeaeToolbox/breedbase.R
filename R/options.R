
# Get value of breedbase option
# @param key The name of the breedbase option (without the breedbase prefix)
# @returns The value of the option from the global options, if set, or the default value
getBBOption <- function(key) {
    return(
        getOption(
            paste("breedbase", key, sep="."), 
            DEFAULT_BB_OPTIONS[[key]]
        )
    )
}

# SET OF DEFAULT BREEDBASE OPTIONS
DEFAULT_BB_OPTIONS = list(
    standard_stock_props = c("population_name", "organization_name", "synonym", "PUI"),
    editable_stock_props = c("variety", "released_variety_name", "donor", "donor institute", "donor PUI", "country of origin", "state", "institute code", "institute name", "biological status of accession code", "notes", "accession number", "seed source", "type of germplasm storage code", "acquisition date", "location_code", "ploidy_level", "genome_structure", "ncbi_taxonomy_id", "transgenic", "introgression_parent", "introgression_backcross_parent", "introgression_map_version", "introgression_chromosome", "introgression_start_position_bp", "introgression_end_position_bp", "purdy_pedigree", "filial_generation"),
    cross_types = c("biparental", "self", "open", "sib"),
    country_codes = c('AFG','ALA','ALB','DZA','ASM','AND','AGO','AIA','ATA','ATG','ARG','ARM','ABW','AUS','AUT','AZE','BHS','BHR','BGD','BRB','BLR','BEL','BLZ','BEN','BMU','BTN','BOL','BES','BIH','BWA','BVT','BRA','IOT','VGB','BRN','BGR','BFA','BDI','CPV','KHM','CMR','CAN','CYM','CAF','TCD','CHL','CHN','HKG','MAC','CXR','CCK','COL','COM','COG','COK','CRI','CIV','HRV','CUB','CUW','CYP','CZE','PRK','COD','DNK','DJI','DMA','DOM','ECU','EGY','SLV','GNQ','ERI','EST','SWZ','ETH','FLK','FRO','FJI','FIN','FRA','GUF','PYF','ATF','GAB','GMB','GEO','DEU','GHA','GIB','GRC','GRL','GRD','GLP','GUM','GTM','GGY','GIN','GNB','GUY','HTI','HMD','VAT','HND','HUN','ISL','IND','IDN','IRN','IRQ','IRL','IMN','ISR','ITA','JAM','JPN','JEY','JOR','KAZ','KEN','KIR','KWT','KGZ','LAO','LVA','LBN','LSO','LBR','LBY','LIE','LTU','LUX','MDG','MWI','MYS','MDV','MLI','MLT','MHL','MTQ','MRT','MUS','MYT','MEX','FSM','MCO','MNG','MNE','MSR','MAR','MOZ','MMR','NAM','NRU','NPL','NLD','NCL','NZL','NIC','NER','NGA','NIU','NFK','MKD','MNP','NOR','OMN','PAK','PLW','PAN','PNG','PRY','PER','PHL','PCN','POL','PRT','PRI','QAT','KOR','MDA','REU','ROU','RUS','RWA','BLM','SHN','KNA','LCA','MAF','SPM','VCT','WSM','SMR','STP','SAU','SEN','SRB','SYC','SLE','SGP','SXM','SVK','SVN','SLB','SOM','ZAF','SGS','SSD','ESP','LKA','PSE','SDN','SUR','SJM','SWE','CHE','SYR','TJK','THA','TLS','TGO','TKL','TON','TTO','TUN','TUR','TKM','TCA','TUV','UGA','UKR','ARE','GBR','TZA','UMI','USA','VIR','URY','UZB','VUT','VEN','VNM','WLF','ESH','YEM','ZMB','ZWE'),
    location_types = c('Farm', 'Field', 'Greenhouse', 'Screenhouse', 'Lab', 'Storage', 'Other'),
    design_types = c('CRD', 'RCBD', 'Alpha', 'Augmented', 'MAD', 'Westcott', 'Lattice'),
    trial_types = c('Seedling Nursery', 'phenotyping_trial', 'Advanced Yield Trial', 'Preliminary Yield Trial', 'Uniform Yield Trial', 'Variety Release Trial', 'Clonal Evaluation', 'genetic_gain_trial', 'storage_trial', 'heterosis_trial', 'health_status_trial', 'grafting_trial', 'Screen House', 'Seed Multiplication', 'crossing_block_trial', 'Specialty Trial')
)