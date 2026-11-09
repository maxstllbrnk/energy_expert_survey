################################################################################
# questionnaire.R — the questionnaire in the order it is shown, and the response
#                   times per question
# Inputs : none
# Outputs: none
# Checks : none
################################################################################
# Used by the cleaning (the last question an expert answered) and by the summary
# statistics (the completion curve, durations).

# One row per question (page). `item` is the name of the question's response-time
# column, with two exceptions that question_times() builds: each staircase decision
# is one item (time_tp_2a and time_tp_2b are both "time_tp_2", as every expert
# sees exactly one of them), and vignette_1 to vignette_6 and vignette_cost come
# from the vignettes. Questions hidden by routing stay in the list: experts who did
# not see them have no time for them.
QUESTIONNAIRE <- tribble(
  ~item,                    ~label,                                 ~section,
  "time_firm_zip",          "Q0 Postcode",                          "Firm",
  "time_resp_is_owner",     "Q0a Owner or managing director",       "Firm",
  "time_resp_role",         "Q0b Role in the firm",                 "Firm",
  "time_firm_cat",          "Q2 Category of the firm",              "Firm",
  "time_firm_employees",    "Q3 Employees",                         "Firm",
  "time_firm_revenue",      "Q4 Revenue",                           "Firm",
  "time_firm_radius",       "Q6 Service area",                      "Firm",
  "time_firm_dh_in_area",   "Q6a District heating in the area",     "Firm",
  "vignette_1",             "Vignette 1",                           "Vignettes",
  "vignette_2",             "Vignette 2",                           "Vignettes",
  "vignette_3",             "Vignette 3",                           "Vignettes",
  "vignette_4",             "Vignette 4",                           "Vignettes",
  "vignette_5",             "Vignette 5",                           "Vignettes",
  "vignette_6",             "Vignette 6",                           "Vignettes",
  "vignette_cost",          "Cost question of the last vignette",   "Vignettes",
  "time_price",             "Q15 Energy prices 2026",               "Energy and carbon prices",
  "time_pexp",              "Q17 Expected price change",            "Energy and carbon prices",
  "time_co2_heard",         "Q18 Carbon price known",               "Energy and carbon prices",
  "time_co2_est",           "Q18b Carbon price estimated",          "Energy and carbon prices",
  "time_mix25",             "Q11b Heating systems installed 2025",  "Heating technologies and manufacturers",
  "time_brand_top1",        "Q11c Top manufacturer",                "Heating technologies and manufacturers",
  "time_tp_1",              "Time preference, decision 1",          "Time preferences",
  "time_tp_2",              "Time preference, decision 2",          "Time preferences",
  "time_tp_3",              "Time preference, decision 3",          "Time preferences",
  "time_tp_4",              "Time preference, decision 4",          "Time preferences",
  "time_tp_5",              "Time preference, decision 5",          "Time preferences",
  "time_risk_1",            "Risk preference, decision 1",          "Risk preferences",
  "time_risk_2",            "Risk preference, decision 2",          "Risk preferences",
  "time_risk_3",            "Risk preference, decision 3",          "Risk preferences",
  "time_risk_4",            "Risk preference, decision 4",          "Risk preferences",
  "time_risk_5",            "Risk preference, decision 5",          "Risk preferences",
  "time_svc",               "Q8 Services offered",                  "Services, market and customers",
  "time_adv_households",    "Q10a Households advised",              "Services, market and customers",
  "time_adv_share",         "Q10b Shares of the recommendations",   "Services, market and customers",
  "time_barr_ren",          "Q19a Barriers, renewable heating",     "Services, market and customers",
  "time_barr_dh",           "Q19b Barriers, district heating",      "Services, market and customers",
  "time_barr_conv",         "Q19c Barriers, conventional heating",  "Services, market and customers",
  "time_cust_rank",         "Q7 Main customer groups",              "Services, market and customers",
  "time_crit_rank",         "Q28 Customers' main criteria",         "Services, market and customers",
  "time_dem_birthyear",     "Q21 Year of birth",                    "Personal information",
  "time_dem_education",     "Q22 Highest degree",                   "Personal information",
  "time_dem_training_year", "Q23 Year training was completed",      "Personal information",
  "time_cpd",               "Q24 Continuing education",             "Personal information",
  "time_cpd_other_text",    "Q24a Other continuing education",      "Personal information",
  "time_dem_own_heating",   "Q20 Heating at home",                  "Personal information",
  "time_fb_comment",        "Feedback",                             "Feedback"
) %>%
  mutate(position = row_number(),
         section = fct_inorder(section))

#' Response times per question: one row per expert x question they submitted
#'
#' A question has a time exactly when the expert submitted its page.
#'
#' @param main      one row per expert, with the time_* columns
#' @param vignettes one row per expert x vignette, with vig_num, vig_time and
#'                  vig_cost_time
#' @return tibble: resp_uid, item, seconds, label, section, position
question_times <- function(main, vignettes) {
  from_main <- main %>%
    select(resp_uid, starts_with("time_"), -any_of("time_interview")) %>%
    pivot_longer(-resp_uid, names_to = "item", values_to = "seconds",
                 values_drop_na = TRUE) %>%
    mutate(item = str_replace(item, "^time_(tp|risk)_([1-5]).*$", "time_\\1_\\2"))

  vignette_times <- vignettes %>%
    filter(!is.na(vig_time)) %>%
    transmute(resp_uid, item = paste0("vignette_", vig_num), seconds = vig_time)
  cost_times <- vignettes %>%
    filter(!is.na(vig_cost_time)) %>%
    transmute(resp_uid, item = "vignette_cost", seconds = vig_cost_time)

  bind_rows(from_main, vignette_times, cost_times) %>%
    inner_join(QUESTIONNAIRE, by = "item")
}
