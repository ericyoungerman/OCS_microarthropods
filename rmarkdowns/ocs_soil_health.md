OCS soil health analysis
================

# Load libraries

``` r
#Load packages 
library(tidyverse) ##install.packages("tidyverse")
library(knitr)
library(patchwork) ##install.packages("patchwork")
library(skimr)     ##install.packages("skimr")
library(readxl)
library(janitor) ##install.packages("janitor")

library(kableExtra) ##install.packages("kableExtra")
library(webshot) ##install.packages("webshot")
webshot::install_phantomjs()
library(viridis) ##install.packages("viridis")
library(lme4) ##install.packages("lme4")
library(lmerTest) ##install.packages("lmerTest")
library(emmeans) ##install.packages("emmeans")
library(rstatix) ##install.packages("rstatix")
#library(Matrix) ##install.packages("Matrix")
library(multcomp) ##install.packages("multcomp")
library(multcompView) ##install.packages("multcompView")
library(ggResidpanel) ##install.packages("ggResidpanel")
#library(car)
#library(TMB)  ##install.packages("TMB")
library(glmmTMB)  ##install.packages("glmmTMB")
library(DHARMa)  ##install.packages("DHARMa")
library(performance) ##install.packages("performance")
library(WrensBookshelf)##install.packages("WrensBookshelf")
#Load Functions
MeanPlusSe<-function(x) mean(x)+plotrix::std.error(x)

find_logw0=function(x){c=trunc(log(min(x[x>0],na.rm=T)))
d=exp(c)
return(d)}
```

<br>

# Load and clean data

## Load data

``` r
ocs_soil_health_raw <- read_excel("~/Github/OCS-microinvertebrates/ocs_soil_health_2022.xlsx")
ocs_soil_health_raw$block <- as.factor(ocs_soil_health_raw$block)
ocs_soil_health_raw$entry <- as.factor(ocs_soil_health_raw$entry)
ocs_soil_health_raw$treatments <- as.factor(ocs_soil_health_raw$treatments)

kable(head(ocs_soil_health_raw))
```

| sample | field | block | entry | treatments | DateSampled | Tillage_1to4 | soil_texture_sand | soil_texture_silt | soil_texture_clay | pred_water_capacity | pred_water_capacity_rating | surface_hardness | surface_hardness_rating | subsurface_hardness | subsurface_hardness_rating | aggregate_stability | aggregate_stability_rating | organic_matter | organic_matter_rating | soc | total_c | total_n | pred_soil_protein | pred_soil_protein_rating | respiration | respiration_rating | active_carbon | active_carbon_rating | ph | ph_rating | p | p_rating | k | k_rating | mg | fe | mn | zn | al | ca | cu | s | minor_elements_rating | overall_score |
|:---|---:|:---|:---|:---|:---|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|
| 1.1.A | 1 | 1 | A | HF | 2022-04-26 | 3 | 43.78894 | 38.94759 | 17.26347 | 0.2151171 | 80.17685 | 163.750 | 50.11709 | 363.750 | 28.11106 | 11.013698 | 13.34241 | 3.279532 | 62.19444 | 2.475162 | 3.355 | 0.226 | 7.284740 | 59.39819 | 0.7348661 | 67.34839 | 675.9906 | 82.92740 | 7.859 | 39.427968 | 15.411281 | 100.00000 | 55.20132 | 80.35098 | 134.3313 | 0.4385637 | 5.997551 | 1.0348564 | 1.488808 | 4981.545 | 0.0769410 | 4.258684 | 100 | 63.61623 |
| 1.1.B | 1 | 1 | B | HF | 2022-04-26 | 3 | 31.65381 | 42.95357 | 25.39262 | 0.2406547 | 89.26677 | 137.500 | 62.21399 | 334.375 | 37.73303 | 16.448171 | 20.94201 | 3.958228 | 85.64936 | 2.599392 | 3.613 | 0.248 | 7.785285 | 65.15395 | 1.1179044 | 95.78576 | 765.0517 | 92.40299 | 7.981 | 20.533155 | 26.736384 | 40.17995 | 67.36930 | 92.74042 | 155.1208 | 0.6199197 | 6.889562 | 1.7188682 | 1.559192 | 5574.299 | 0.1080163 | 4.625916 | 100 | 66.88345 |
| 1.2.A | 1 | 1 | A | LF | 2022-04-26 | 3 | 35.72908 | 42.71480 | 21.55612 | 0.2237636 | 83.66984 | 103.125 | 76.25914 | 308.750 | 46.82994 | 12.552440 | 15.27136 | 4.056622 | 87.98074 | 2.636123 | 2.688 | 0.253 | 7.080389 | 56.98041 | 0.7632393 | 70.68244 | 571.7617 | 65.09554 | 8.028 | 15.001626 | 1.168519 | 35.26775 | 37.35276 | 53.99341 | 168.7945 | 0.4083176 | 4.703952 | 0.1759417 | 1.556919 | 1534.697 | 0.0663931 | 2.891420 | 88 | 57.91935 |
| 1.2.B | 1 | 1 | B | LF | 2022-04-26 | 3 | 40.51895 | 42.54852 | 16.93253 | 0.2133034 | 79.39070 | 132.500 | 64.42361 | 343.750 | 34.54156 | 22.965539 | 32.76690 | 3.698215 | 78.10647 | 2.352474 | 2.397 | 0.237 | 6.540035 | 50.48398 | 0.5859068 | 48.12656 | 530.9501 | 56.64322 | 7.913 | 30.375313 | 1.238093 | 37.10041 | 28.77912 | 37.31419 | 155.5995 | 0.4077104 | 1.544811 | 0.1383971 | 1.395193 | 1415.862 | 0.0523665 | 2.113361 | 77 | 52.18941 |
| 1.3.A | 1 | 1 | A | EWM | 2022-04-26 | 3 | 41.62047 | 40.60886 | 17.77067 | 0.2173521 | 81.12033 | 100.625 | 77.15656 | 311.875 | 45.70159 | 9.497207 | 11.61217 | 3.250630 | 60.96772 | 2.074495 | 2.224 | 0.205 | 5.562383 | 38.81567 | 0.7632393 | 70.68244 | 549.5008 | 60.54857 | 8.089 | 9.448430 | 1.320055 | 39.25936 | 29.92011 | 39.37146 | 156.6386 | 0.3936410 | 3.244145 | 0.1696729 | 1.398104 | 1689.141 | 0.0678691 | 3.135554 | 88 | 51.89036 |
| 1.3.B | 1 | 1 | B | EWM | 2022-04-26 | 3 | 40.32626 | 43.57837 | 16.09537 | 0.2160877 | 80.59005 | 123.750 | 68.17253 | 330.000 | 39.25314 | 15.249652 | 19.07817 | 3.350125 | 65.13723 | 2.092920 | 2.282 | 0.208 | 5.720484 | 40.66322 | 0.7774259 | 72.28804 | 549.5008 | 60.54857 | 8.114 | 7.675065 | 1.856948 | 53.99454 | 29.86921 | 39.27967 | 149.0407 | 0.5315828 | 3.581000 | 0.5459499 | 1.411568 | 1785.242 | 0.0790191 | 2.600446 | 100 | 53.89002 |

\##Clean data

``` r
 #Filter relevant data (we can't use data from alley and corn)
ocs_soil_health_clean <- ocs_soil_health_raw |>  
  filter(treatments %in% c("HF", "LF", "EWM", "RT"))

# Display cleaned dataset
kable(head(ocs_soil_health_clean))
```

| sample | field | block | entry | treatments | DateSampled | Tillage_1to4 | soil_texture_sand | soil_texture_silt | soil_texture_clay | pred_water_capacity | pred_water_capacity_rating | surface_hardness | surface_hardness_rating | subsurface_hardness | subsurface_hardness_rating | aggregate_stability | aggregate_stability_rating | organic_matter | organic_matter_rating | soc | total_c | total_n | pred_soil_protein | pred_soil_protein_rating | respiration | respiration_rating | active_carbon | active_carbon_rating | ph | ph_rating | p | p_rating | k | k_rating | mg | fe | mn | zn | al | ca | cu | s | minor_elements_rating | overall_score |
|:---|---:|:---|:---|:---|:---|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|
| 1.1.A | 1 | 1 | A | HF | 2022-04-26 | 3 | 43.78894 | 38.94759 | 17.26347 | 0.2151171 | 80.17685 | 163.750 | 50.11709 | 363.750 | 28.11106 | 11.013698 | 13.34241 | 3.279532 | 62.19444 | 2.475162 | 3.355 | 0.226 | 7.284740 | 59.39819 | 0.7348661 | 67.34839 | 675.9906 | 82.92740 | 7.859 | 39.427968 | 15.411281 | 100.00000 | 55.20132 | 80.35098 | 134.3313 | 0.4385637 | 5.997551 | 1.0348564 | 1.488808 | 4981.545 | 0.0769410 | 4.258684 | 100 | 63.61623 |
| 1.1.B | 1 | 1 | B | HF | 2022-04-26 | 3 | 31.65381 | 42.95357 | 25.39262 | 0.2406547 | 89.26677 | 137.500 | 62.21399 | 334.375 | 37.73303 | 16.448171 | 20.94201 | 3.958228 | 85.64936 | 2.599392 | 3.613 | 0.248 | 7.785285 | 65.15395 | 1.1179044 | 95.78576 | 765.0517 | 92.40299 | 7.981 | 20.533155 | 26.736384 | 40.17995 | 67.36930 | 92.74042 | 155.1208 | 0.6199197 | 6.889562 | 1.7188682 | 1.559192 | 5574.299 | 0.1080163 | 4.625916 | 100 | 66.88345 |
| 1.2.A | 1 | 1 | A | LF | 2022-04-26 | 3 | 35.72908 | 42.71480 | 21.55612 | 0.2237636 | 83.66984 | 103.125 | 76.25914 | 308.750 | 46.82994 | 12.552440 | 15.27136 | 4.056622 | 87.98074 | 2.636123 | 2.688 | 0.253 | 7.080389 | 56.98041 | 0.7632393 | 70.68244 | 571.7617 | 65.09554 | 8.028 | 15.001626 | 1.168519 | 35.26775 | 37.35276 | 53.99341 | 168.7945 | 0.4083176 | 4.703952 | 0.1759417 | 1.556919 | 1534.697 | 0.0663931 | 2.891420 | 88 | 57.91935 |
| 1.2.B | 1 | 1 | B | LF | 2022-04-26 | 3 | 40.51895 | 42.54852 | 16.93253 | 0.2133034 | 79.39070 | 132.500 | 64.42361 | 343.750 | 34.54156 | 22.965539 | 32.76690 | 3.698215 | 78.10647 | 2.352474 | 2.397 | 0.237 | 6.540035 | 50.48398 | 0.5859068 | 48.12656 | 530.9501 | 56.64322 | 7.913 | 30.375313 | 1.238093 | 37.10041 | 28.77912 | 37.31419 | 155.5995 | 0.4077104 | 1.544811 | 0.1383971 | 1.395193 | 1415.862 | 0.0523665 | 2.113361 | 77 | 52.18941 |
| 1.3.A | 1 | 1 | A | EWM | 2022-04-26 | 3 | 41.62047 | 40.60886 | 17.77067 | 0.2173521 | 81.12033 | 100.625 | 77.15656 | 311.875 | 45.70159 | 9.497207 | 11.61217 | 3.250630 | 60.96772 | 2.074495 | 2.224 | 0.205 | 5.562383 | 38.81567 | 0.7632393 | 70.68244 | 549.5008 | 60.54857 | 8.089 | 9.448430 | 1.320055 | 39.25936 | 29.92011 | 39.37146 | 156.6386 | 0.3936410 | 3.244145 | 0.1696729 | 1.398104 | 1689.141 | 0.0678691 | 3.135554 | 88 | 51.89036 |
| 1.3.B | 1 | 1 | B | EWM | 2022-04-26 | 3 | 40.32626 | 43.57837 | 16.09537 | 0.2160877 | 80.59005 | 123.750 | 68.17253 | 330.000 | 39.25314 | 15.249652 | 19.07817 | 3.350125 | 65.13723 | 2.092920 | 2.282 | 0.208 | 5.720484 | 40.66322 | 0.7774259 | 72.28804 | 549.5008 | 60.54857 | 8.114 | 7.675065 | 1.856948 | 53.99454 | 29.86921 | 39.27967 | 149.0407 | 0.5315828 | 3.581000 | 0.5459499 | 1.411568 | 1785.242 | 0.0790191 | 2.600446 | 100 | 53.89002 |

``` r
summary(ocs_soil_health_clean
        )
```

    ##     sample              field   block entry  treatments  DateSampled        
    ##  Length:32          Min.   :1   1:8   A:16   Alley:0    Min.   :2022-04-26  
    ##  Class :character   1st Qu.:1   2:8   B:16   Corn :0    1st Qu.:2022-04-26  
    ##  Mode  :character   Median :1   3:8   C: 0   EWM  :8    Median :2022-04-26  
    ##                     Mean   :1   4:8          HF   :8    Mean   :2022-04-26  
    ##                     3rd Qu.:1   5:0          LF   :8    3rd Qu.:2022-04-26  
    ##                     Max.   :1                RT   :8    Max.   :2022-04-26  
    ##   Tillage_1to4  soil_texture_sand soil_texture_silt soil_texture_clay
    ##  Min.   :2.00   Min.   :31.65     Min.   :36.49     Min.   :15.25    
    ##  1st Qu.:2.75   1st Qu.:37.90     1st Qu.:40.43     1st Qu.:17.96    
    ##  Median :3.00   Median :39.17     Median :41.92     Median :19.28    
    ##  Mean   :2.75   Mean   :39.14     Mean   :41.60     Mean   :19.26    
    ##  3rd Qu.:3.00   3rd Qu.:40.52     3rd Qu.:42.96     3rd Qu.:20.37    
    ##  Max.   :3.00   Max.   :43.79     Max.   :45.00     Max.   :25.39    
    ##  pred_water_capacity pred_water_capacity_rating surface_hardness
    ##  Min.   :0.1944      Min.   :70.17              Min.   : 93.75  
    ##  1st Qu.:0.2055      1st Qu.:75.82              1st Qu.:106.41  
    ##  Median :0.2139      Median :79.64              Median :140.62  
    ##  Mean   :0.2120      Mean   :78.62              Mean   :136.52  
    ##  3rd Qu.:0.2173      3rd Qu.:81.12              3rd Qu.:156.25  
    ##  Max.   :0.2407      Max.   :89.27              Max.   :208.12  
    ##  surface_hardness_rating subsurface_hardness subsurface_hardness_rating
    ##  Min.   :30.22           Min.   :296.2       Min.   :19.55             
    ##  1st Qu.:53.62           1st Qu.:322.2       1st Qu.:31.66             
    ##  Median :60.80           Median :337.5       Median :36.66             
    ##  Mean   :62.09           Mean   :336.4       Mean   :37.30             
    ##  3rd Qu.:75.05           3rd Qu.:352.5       3rd Qu.:42.01             
    ##  Max.   :79.52           Max.   :394.4       Max.   :51.36             
    ##  aggregate_stability aggregate_stability_rating organic_matter 
    ##  Min.   : 9.497      Min.   :11.61              Min.   :2.600  
    ##  1st Qu.:13.306      1st Qu.:16.28              1st Qu.:2.935  
    ##  Median :15.257      Median :19.09              Median :3.146  
    ##  Mean   :15.897      Mean   :20.67              Mean   :3.167  
    ##  3rd Qu.:17.278      3rd Qu.:22.30              3rd Qu.:3.280  
    ##  Max.   :30.368      Max.   :48.60              Max.   :4.057  
    ##  organic_matter_rating      soc           total_c         total_n      
    ##  Min.   :32.85         Min.   :1.497   Min.   :1.687   Min.   :0.1440  
    ##  1st Qu.:47.11         1st Qu.:1.735   1st Qu.:2.058   1st Qu.:0.1648  
    ##  Median :56.45         Median :1.859   Median :2.221   Median :0.1770  
    ##  Mean   :56.65         Mean   :1.935   Mean   :2.277   Mean   :0.1878  
    ##  3rd Qu.:62.21         3rd Qu.:2.079   3rd Qu.:2.410   3rd Qu.:0.2035  
    ##  Max.   :87.98         Max.   :2.636   Max.   :3.613   Max.   :0.2530  
    ##  pred_soil_protein pred_soil_protein_rating  respiration     respiration_rating
    ##  Min.   :4.403     Min.   :26.26            Min.   :0.4228   Min.   :27.73     
    ##  1st Qu.:4.906     1st Qu.:31.45            1st Qu.:0.6966   1st Qu.:62.62     
    ##  Median :5.302     Median :35.83            Median :0.7512   Median :69.28     
    ##  Mean   :5.454     Mean   :37.81            Mean   :0.7498   Mean   :67.89     
    ##  3rd Qu.:5.664     3rd Qu.:40.00            3rd Qu.:0.7881   3rd Qu.:73.42     
    ##  Max.   :7.785     Max.   :65.15            Max.   :1.1179   Max.   :95.79     
    ##  active_carbon   active_carbon_rating       ph          ph_rating     
    ##  Min.   :404.8   Min.   :30.34        Min.   :7.859   Min.   : 0.000  
    ##  1st Qu.:488.7   1st Qu.:47.58        1st Qu.:8.111   1st Qu.: 0.000  
    ##  Median :522.6   Median :54.86        Median :8.267   Median : 1.689  
    ##  Mean   :529.1   Mean   :55.60        Mean   :8.227   Mean   : 5.680  
    ##  3rd Qu.:550.4   3rd Qu.:60.74        3rd Qu.:8.328   3rd Qu.: 7.879  
    ##  Max.   :765.1   Max.   :92.40        Max.   :8.441   Max.   :39.428  
    ##        p             p_rating            k            k_rating    
    ##  Min.   : 1.118   Min.   : 33.94   Min.   :28.32   Min.   :36.48  
    ##  1st Qu.: 1.330   1st Qu.: 39.53   1st Qu.:29.91   1st Qu.:39.35  
    ##  Median : 1.609   Median : 46.75   Median :33.48   Median :46.76  
    ##  Mean   : 3.112   Mean   : 55.33   Mean   :35.69   Mean   :49.81  
    ##  3rd Qu.: 2.490   3rd Qu.: 69.53   3rd Qu.:37.76   3rd Qu.:54.76  
    ##  Max.   :26.736   Max.   :100.00   Max.   :67.37   Max.   :92.74  
    ##        mg              fe               mn              zn        
    ##  Min.   :134.3   Min.   :0.2903   Min.   :1.545   Min.   :0.1199  
    ##  1st Qu.:150.1   1st Qu.:0.4041   1st Qu.:2.942   1st Qu.:0.1553  
    ##  Median :156.7   Median :0.4677   Median :3.538   Median :0.1863  
    ##  Mean   :163.7   Mean   :0.5302   Mean   :4.054   Mean   :0.2919  
    ##  3rd Qu.:180.9   3rd Qu.:0.5609   3rd Qu.:5.047   3rd Qu.:0.2844  
    ##  Max.   :198.3   Max.   :1.1954   Max.   :9.167   Max.   :1.7189  
    ##        al              ca             cu                s        
    ##  Min.   :1.277   Min.   :1338   Min.   :0.05208   Min.   :1.935  
    ##  1st Qu.:1.467   1st Qu.:1530   1st Qu.:0.06132   1st Qu.:2.588  
    ##  Median :1.637   Median :1863   Median :0.07280   Median :3.083  
    ##  Mean   :1.745   Mean   :2326   Mean   :0.07448   Mean   :3.077  
    ##  3rd Qu.:1.905   3rd Qu.:2608   3rd Qu.:0.07922   3rd Qu.:3.309  
    ##  Max.   :2.612   Max.   :5574   Max.   :0.16309   Max.   :4.626  
    ##  minor_elements_rating overall_score  
    ##  Min.   : 77.00        Min.   :42.11  
    ##  1st Qu.: 88.00        1st Qu.:47.38  
    ##  Median : 88.00        Median :49.81  
    ##  Mean   : 89.69        Mean   :51.43  
    ##  3rd Qu.:100.00        3rd Qu.:53.98  
    ##  Max.   :100.00        Max.   :66.88

# ashley LMER

``` r
lmer_aj <- lmer(overall_score ~ treatments*entry  + (1|block:treatments), data = ocs_soil_health_clean)

resid_panel(lmer_aj)
```

![](ocs_soil_health_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->

``` r
simulateResiduals(lmer_aj,plot = TRUE) 
```

![](ocs_soil_health_files/figure-gfm/unnamed-chunk-4-2.png)<!-- -->

    ## Object of Class DHARMa with simulated residuals based on 250 simulations with refit = FALSE . See ?DHARMa::simulateResiduals for help. 
    ##  
    ## Scaled residual values: 0.952 0.972 0.892 0.672 0.752 0.76 0.936 0.9 0.512 0.56 0.344 0.66 0.6 0.64 0.616 0.836 0.188 0.088 0.324 0.532 ...

``` r
check_model(lmer_aj)
```

![](ocs_soil_health_files/figure-gfm/unnamed-chunk-4-3.png)<!-- --> \###
Joint test (anova)

``` r
lmer_aj |> 
  joint_tests() |> 
  kable()  
```

|     | model term       | df1 | df2 | F.ratio |   p.value |
|:----|:-----------------|----:|----:|--------:|----------:|
| 1   | treatments       |   3 |  12 |   0.559 | 0.6521036 |
| 3   | entry            |   1 |  12 |   1.519 | 0.2414378 |
| 2   | treatments:entry |   3 |  12 |   1.278 | 0.3262792 |

### Fisher compact letter display

#### entry (significant)

``` r
cld_entry_fisher <-cld(emmeans(lmer_aj, ~  entry , type = "response"), Letters = letters, adjust = "none",sort = TRUE, reversed=TRUE)
```

    ## NOTE: Results may be misleading due to involvement in interactions

``` r
cld_entry_fisher
```

    ##  entry emmean   SE   df lower.CL upper.CL .group
    ##  B       51.9 1.56 13.6     48.6     55.3  a    
    ##  A       50.9 1.56 13.6     47.6     54.3  a    
    ## 
    ## Results are averaged over the levels of: treatments 
    ## Degrees-of-freedom method: kenward-roger 
    ## Confidence level used: 0.95 
    ## significance level used: alpha = 0.05 
    ## NOTE: If two or more means share the same grouping symbol,
    ##       then we cannot show them to be different.
    ##       But we also did not show them to be the same.

\#Jake LMER

``` r
lmer_ja <- lmer(overall_score ~ treatments*entry + block  + (1|block:treatments), data = ocs_soil_health_clean)

resid_panel(lmer_ja)
```

![](ocs_soil_health_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->

``` r
simulateResiduals(lmer_ja,plot = TRUE) 
```

![](ocs_soil_health_files/figure-gfm/unnamed-chunk-7-2.png)<!-- -->

    ## Object of Class DHARMa with simulated residuals based on 250 simulations with refit = FALSE . See ?DHARMa::simulateResiduals for help. 
    ##  
    ## Scaled residual values: 0.8 0.916 0.556 0.1 0.136 0.18 0.748 0.524 0.308 0.42 0.08 0.528 0.504 0.572 0.556 0.92 0.324 0.152 0.764 0.92 ...

``` r
check_model(lmer_ja)
```

![](ocs_soil_health_files/figure-gfm/unnamed-chunk-7-3.png)<!-- -->

### Joint test (anova)

``` r
lmer_ja |> 
  joint_tests() |> 
  kable()  
```

|     | model term       | df1 | df2 | F.ratio |   p.value |
|:----|:-----------------|----:|----:|--------:|----------:|
| 1   | treatments       |   3 |   9 |   2.352 | 0.1402490 |
| 3   | entry            |   1 |  12 |   1.519 | 0.2414378 |
| 4   | block            |   3 |   9 |  13.832 | 0.0010184 |
| 2   | treatments:entry |   3 |  12 |   1.278 | 0.3262792 |

### Fisher compact letter display

#### entry (significant)

``` r
cld_entry_fisher_ja <-cld(emmeans(lmer_ja, ~  entry , type = "response"), Letters = letters, adjust = "none",sort = TRUE, reversed=TRUE)
```

    ## NOTE: Results may be misleading due to involvement in interactions

``` r
cld_entry_fisher_ja
```

    ##  entry emmean    SE df lower.CL upper.CL .group
    ##  B       51.9 0.834 14     50.1     53.7  a    
    ##  A       50.9 0.834 14     49.2     52.7  a    
    ## 
    ## Results are averaged over the levels of: treatments, block 
    ## Degrees-of-freedom method: kenward-roger 
    ## Confidence level used: 0.95 
    ## significance level used: alpha = 0.05 
    ## NOTE: If two or more means share the same grouping symbol,
    ##       then we cannot show them to be different.
    ##       But we also did not show them to be the same.

\####block (significant)

``` r
cld_block_fisher_ja <-cld(emmeans(lmer_ja, ~  block , type = "response"), Letters = letters, adjust = "none",sort = TRUE, reversed=TRUE)
cld_block_fisher_ja
```

    ##  block emmean   SE df lower.CL upper.CL .group
    ##  1       58.4 1.47  9     55.1     61.7  a    
    ##  2       53.2 1.47  9     49.8     56.5   b   
    ##  3       47.4 1.47  9     44.1     50.7    c  
    ##  4       46.8 1.47  9     43.4     50.1    c  
    ## 
    ## Results are averaged over the levels of: treatments, entry 
    ## Degrees-of-freedom method: kenward-roger 
    ## Confidence level used: 0.95 
    ## significance level used: alpha = 0.05 
    ## NOTE: If two or more means share the same grouping symbol,
    ##       then we cannot show them to be different.
    ##       But we also did not show them to be the same.

# ashley GLMM

``` r
glmm_aj <- glmmTMB(overall_score ~ treatments * entry +  (1 | block:treatments),
                      data = ocs_soil_health_clean,
                      family = gaussian())


simulateResiduals(glmm_aj ,plot = TRUE) 
```

![](ocs_soil_health_files/figure-gfm/unnamed-chunk-11-1.png)<!-- -->

    ## Object of Class DHARMa with simulated residuals based on 250 simulations with refit = FALSE . See ?DHARMa::simulateResiduals for help. 
    ##  
    ## Scaled residual values: 0.968 0.992 0.924 0.672 0.74 0.788 0.956 0.924 0.404 0.508 0.32 0.656 0.652 0.64 0.652 0.848 0.164 0.072 0.324 0.544 ...

``` r
check_model(glmm_aj )
```

    ## `check_outliers()` does not yet support models of class `glmmTMB`.

![](ocs_soil_health_files/figure-gfm/unnamed-chunk-11-2.png)<!-- -->

### Joint test (anova)

``` r
glmm_aj |> 
  joint_tests() |> 
  kable()  
```

|     | model term       | df1 | df2 | F.ratio |   p.value |
|:----|:-----------------|----:|----:|--------:|----------:|
| 1   | treatments       |   3 |  22 |   0.745 | 0.5365582 |
| 3   | entry            |   1 |  22 |   2.025 | 0.1687808 |
| 2   | treatments:entry |   3 |  22 |   1.704 | 0.1953680 |

### Fisher compact letter display

#### entry (significant)

``` r
cld_entry_fisher_a <-cld(emmeans(glmm_aj, ~  entry , type = "response"), Letters = letters, adjust = "none",sort = TRUE, reversed=TRUE)
```

    ## NOTE: Results may be misleading due to involvement in interactions

``` r
cld_entry_fisher_a
```

    ##  entry emmean   SE df lower.CL upper.CL .group
    ##  B       51.9 1.35 22     49.1     54.7  a    
    ##  A       50.9 1.35 22     48.1     53.7  a    
    ## 
    ## Results are averaged over the levels of: treatments 
    ## Confidence level used: 0.95 
    ## significance level used: alpha = 0.05 
    ## NOTE: If two or more means share the same grouping symbol,
    ##       then we cannot show them to be different.
    ##       But we also did not show them to be the same.

\#Jake Glmm

``` r
glmm_ja <- glmmTMB(overall_score ~ treatments * entry + block + (1 | block:treatments),
                      data = ocs_soil_health_clean,
                      family = gaussian())


simulateResiduals(glmm_ja ,plot = TRUE) 
```

    ## qu = 0.25, log(sigma) = -3.242394 : outer Newton did not converge fully.

    ## qu = 0.25, log(sigma) = -3.286125 : outer Newton did not converge fully.

    ## qu = 0.25, log(sigma) = -3.316035 : outer Newton did not converge fully.

![](ocs_soil_health_files/figure-gfm/unnamed-chunk-14-1.png)<!-- -->

    ## Object of Class DHARMa with simulated residuals based on 250 simulations with refit = FALSE . See ?DHARMa::simulateResiduals for help. 
    ##  
    ## Scaled residual values: 0.864 0.984 0.56 0.044 0.096 0.128 0.764 0.6 0.244 0.392 0.052 0.544 0.508 0.572 0.632 0.948 0.268 0.048 0.808 0.968 ...

``` r
check_model(glmm_ja )
```

    ## `check_outliers()` does not yet support models of class `glmmTMB`.

![](ocs_soil_health_files/figure-gfm/unnamed-chunk-14-2.png)<!-- -->

### Joint test (anova)

``` r
glmm_ja |> 
  joint_tests() |> 
  kable()  
```

|     | model term       | df1 | df2 | F.ratio |   p.value |
|:----|:-----------------|----:|----:|--------:|----------:|
| 1   | treatments       |   3 |  19 |   4.182 | 0.0197009 |
| 3   | entry            |   1 |  19 |   2.025 | 0.1709623 |
| 4   | block            |   3 |  19 |  24.591 | 0.0000009 |
| 2   | treatments:entry |   3 |  19 |   1.704 | 0.2000241 |

### Fisher compact letter display

#### treatments (significant)

``` r
cld_treatments_fisher <-cld(emmeans(glmm_ja, ~  treatments , type = "response"), Letters = letters, adjust = "none",sort = TRUE, reversed=TRUE)
```

    ## NOTE: Results may be misleading due to involvement in interactions

``` r
cld_treatments_fisher 
```

    ##  treatments emmean  SE df lower.CL upper.CL .group
    ##  HF           54.3 1.1 19     52.0     56.6  a    
    ##  RT           52.2 1.1 19     49.9     54.5  ab   
    ##  LF           50.0 1.1 19     47.7     52.3   b   
    ##  EWM          49.3 1.1 19     47.0     51.6   b   
    ## 
    ## Results are averaged over the levels of: entry, block 
    ## Confidence level used: 0.95 
    ## significance level used: alpha = 0.05 
    ## NOTE: If two or more means share the same grouping symbol,
    ##       then we cannot show them to be different.
    ##       But we also did not show them to be the same.
