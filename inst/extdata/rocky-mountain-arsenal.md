Note the MODFLOW files were deleted after the file export in ModelMuse, and the following changes were made for more uniform file names:

``` r
file.rename("rocky_mountain_arsenal_Cl._mas", "rocky-mountain-arsenal.mas")
file.rename("rocky_mountain_arsenal_Cl.ucn", "rocky-mountain-arsenal.ucn")
file.rename("rocky-mountain-arsenal.mt_nam", "rocky-mountain-arsenal.nam")
x <- readLines("rocky-mountain-arsenal.nam")
y <- gsub("rocky_mountain_arsenal_Cl._mas", "rocky-mountain-arsenal.mas", x, fixed = TRUE)
y <- gsub("rocky_mountain_arsenal_Cl.ucn", "rocky-mountain-arsenal.ucn", y, fixed = TRUE)
cat(y, file = "rocky-mountain-arsenal.nam", sep = "\n")
```
