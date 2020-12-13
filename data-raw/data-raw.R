rmtd_guide_shortcuts <- tibble::tribble(
  ~shortcut, ~name,
  "nam", "name_file"
)
rmtd_supported_codes <- c("MT3D-USGS", "MT3DMS")
rmtd_supported_packages <- tibble::tribble(
  ~ftype, ~rmt,
  'BTN',  'btn',
  'ADV',  'adv',
  'DSP',  'dsp',
  'GCG',  'gcg', 
  'RCT',  'rct',
  'SSM',  'ssm'
)
rmtd_internal_nunit <- tibble::tribble(
  ~ftype, ~nunit,
  'BTN',      1,
  'FTL',     10,
  'FT6',     21, # actually 21-23 but MODFLOW-6 not supported
  'ADV',      2, 
  'DSP',      3,
  'SSM',      4,
  'CTS',      6,
  'UZT',      7,
  'RCT',      8,
  'GCG',      9,
  'TOB',     12,
  'HSS',     13,
  'TSO',     14,
  'LKT',     18,
  'SFT',     19,
  'LIST',    16,
  'CNF',     17)
usethis::use_data(
  rmtd_supported_codes,
  rmtd_supported_packages,
  rmtd_guide_shortcuts,
  rmtd_internal_nunit,
  internal = TRUE,
  overwrite = TRUE
)
