
# 1. Main analysis -----------------------------------------------------------


# 1.1. Null models --------------------------------------------------------

data <- rio::import(here("01_data", "clean_data.Rdata"))

sub_data <- data %>% 
  filter(study == 3) 

null_model <- lmer(fgcp ~ (1|strata), data = sub_data)

null_model_wemix <-
  mix(fgcp ~ (1 | strata), data = sub_data, weights = c("zwgt",
                                                      "weightL1"))

summary(null_model)

summary(null_model_wemix)


# 1.2. Interaction models -------------------------------------------------

int_model_wemix <-
  mix(
    fgcp ~ female + rural + age_bin + education_3cat + (1 |strata),
    weights = c("zwgt","weightL1"),
    data = sub_data
  )

summary(int_model_wemix)

int_model <-
  lmer(fgcp ~ female + rural + age_bin + education_3cat +
         (1 | strata),
       data = sub_data)

summary(int_model)
