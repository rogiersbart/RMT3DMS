rmtd_guide_shortcuts <- tibble::tribble(
  ~shortcut, ~name,
  "nam", "name_file"
)
rmtd_supported_codes <- c("MT3D-USGS", "MT3DMS")
rmtd_supported_packages <- tibble::tribble(
  ~ftype,   ~rmt,
  'BTN',  'btn',
  'ADV',  'adv',
  'DSP',  'dsp',
  'GCG',  'gcg', 
  'RCT',  'rct',
  'SSM',  'ssm'
)
usethis::use_data(
  rmtd_supported_codes,
  rmtd_supported_packages,
  rmtd_guide_shortcuts,
  internal = TRUE,
  overwrite = TRUE
)