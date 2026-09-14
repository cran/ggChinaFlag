# ggChinaFlag 0.4.0

* Added `plot_CYLC()` for the flag of the Communist Youth League of China,
  based on the national standard GB/T 40055-2021.
* Added `plot_PLA()` for the flag of the People's Liberation Army and its
  service branches, including the Army, Navy, Air Force, Rocket Force,
  Aerospace Force, Cyberspace Force, Information Support Force, and Joint
  Logistics Support Force.
* `FlagStorage()` now includes two new categories: `Organizations` (currently
  the CYLC flag) and `Military` (the general PLA flag and its service branch
  flags), in both Chinese and English.
* `plotCNFlag()` now recognises the CYLC flag and all PLA service branch
  flags, and dispatches to `plot_CYLC()` and `plot_PLA()` accordingly.

# ggChinaFlag 0.3.0

* Added `plot_HK_SAR_flag()` and `plot_Macao_SAR_flag()` for the regional
  flags of the Hong Kong and Macao Special Administrative Regions.
* `FlagStorage()` and `plotCNFlag()` now recognise the two SAR regional flags.

# ggChinaFlag 0.2.0

* Initial CRAN release.