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
  'CNF',     17
)
rmtd_itype <- tibble::tribble(
 ~itype,                 ~names,
   1,          "constant-head",
   2,                    "wel",
   3,                    "drn",
   4,                    "riv",
   5,                    "ghb",
  15,           "mass-loading",
  -1, "constant-concentration",
  21,                    "str",
  22,                    "res",
  23,                    "fhd",
  26,                    "lak",
  27,                    "mnw",
  28,                    "drt",
  30,                    "sfr"
)
usethis::use_data(
  rmtd_supported_codes,
  rmtd_supported_packages,
  rmtd_guide_shortcuts,
  rmtd_internal_nunit,
  rmtd_itype,
  internal = TRUE,
  overwrite = TRUE
)
