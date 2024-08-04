*****PROPENSITY SCORE MATCHING: COMPARING COSTS & UTILZATION FOR REFERRALS VS CONTROLS OF SERIOUSLY ILL PATIENTS***************

/*SETTING MEMORY*/
set more off
set scrollbufsize 2048000

/*WORKING DIRECTORY*/
cd
cd "H:\My Drive\Analytics\StatisticalModelings\" 
cd

/*DATASET - PROPENSITY SCORE - BEFORE MATCHING*/
use "H:\My Drive\Analytics\StatisticalModelings\ps_beforematch.dta", clear
save "H:\My Drive\Analytics\StatisticalModelings\ps_beforematch.dta", replace /*N=15,456*/

/*ANALYTIC SAMPLE: REFERRED PATIENTS VS CONTROLS */
tab referIM, missing /*N(referred patients)=459 vs N(control patients)=14,997 ---> N(total)=15,456*/

/*DESCRIPTIVE STATISTICS BEFORE MATCHING*/
use "H:\My Drive\Analytics\StatisticalModelings\ps_beforematch.dta", clear
save "H:\My Drive\Analytics\StatisticalModelings\ps_beforematch.dta", replace

global beforematch ///
age_group gender raceethnicity maritalstatus disability medicaid rurality ///
drivedistance_pc nextkin fraility /// 
copd med_nonadhere asthma ptsd substanceuse anxiety bipolar stroke renalfailure ischemicheartdisease congestiveheartfailure hypertension ///
diabetes dementia peripvasculardisease liverdisease arrhythmia fluidelectrodisorder anemia coagulopathy pulmonarycircdisorder depression psychosis ///
pcvisit ervisit hospitalization 

/*Chi Square Tests*/
foreach var of global beforematch {
 tab referIM `var' , cell row col chi2
 } 
 
/*********************************PROPENSITY MODELING-MATCHING**********************/
/*STEP 1. CHOOSING VARIABLES TO PREDICT LIKELIHOOD OF SERIOUSLY ILL PATIENTS BEING REFERRED TO INTENSIVE MANAGMENT*/

/*drop _mypscore _myblock*/

use "H:\My Drive\Analytics\StatisticalModelings\ps_beforematch.dta", clear

/*1.1. Use SIGNIFICANTLY DIFFERENT covariates to predict likelihood of being in PIM group*/
pscore referIM ///
pcvisit ervisit hospitalization ///
age_group gender raceethnicity maritalstatus disability medicaid rurality drivedistance_pc nextkin fraility ///
copd med_nonadhere asthma ptsd substanceuse anxiety bipolar stroke renalfailure ischemicheartdisease congestiveheartfailure hypertension diabetes ///
dementia peripvasculardisease liverdisease arrhythmia fluidelectrodisorder anemia coagulopathy pulmonarycircdisorder depression psychosis, ///
pscore(_mypscore) blockid(_myblock)logit /*detail*/
s
/*STEP 2 & 3. BALANCE PROPENSITY SCORE & COVARIATES ACROSS TREATMENT & COMPARISON GROUPS */
drop _mypscore _myblock

pscore referIM ///
pcvisit ervisit hospitalization ///
age_group gender raceethnicity maritalstatus disability medicaid rurality drivedistance_pc nextkin fraility ///
copd med_nonadhere asthma ptsd substanceuse anxiety bipolar stroke renalfailure ischemicheartdisease congestiveheartfailure hypertension diabetes ///
dementia peripvasculardisease liverdisease arrhythmia fluidelectrodisorder anemia coagulopathy pulmonarycircdisorder depression psychosis, ///
pscore(_mypscore) blockid(_myblock)logit detail

/*GRAPH OF PROPENSITY SCORES ACROSS TREATMENT & COMPARISON-Used to check for balance of Propensity Score Across Treatment Group*/
psgraph, treated (referIM) pscore(_mypscore) bin(10) title("Whole Sample: PS Distrb By Referred Patients vs Controls")

/*STANDARDIZED DIFFERENCES BEFORE PS MATCHING*/
/*NOTE: recommended maximum standardized difference is <25%.
the goal is to have no more than 3 variables (10% of the 33 variables) with standardized difference >25%*/

/*size of standardized differences*/
xi: pbalchk referIM ///
pcvisit ervisit hospitalization ///
age_group gender raceethnicity maritalstatus disability medicaid rurality drivedistance_pc nextkin fraility ///
copd med_nonadhere asthma ptsd substanceuse anxiety bipolar stroke renalfailure ischemicheartdisease congestiveheartfailure hypertension diabetes ///
dementia peripvasculardisease liverdisease arrhythmia fluidelectrodisorder anemia coagulopathy pulmonarycircdisorder depression psychosis

/*p-values of standardized differences between referred patients & controls*/
xi: pbalchk referIM ///
pcvisit ervisit hospitalization ///
age_group gender raceethnicity maritalstatus disability medicaid rurality drivedistance_pc nextkin fraility ///
copd med_nonadhere asthma ptsd substanceuse anxiety bipolar stroke renalfailure ischemicheartdisease congestiveheartfailure hypertension diabetes ///
dementia peripvasculardisease liverdisease arrhythmia fluidelectrodisorder anemia coagulopathy pulmonarycircdisorder depression psychosis

/*3 things to look for in a BALANCED propensity score
1. common suppport-look at the figure from the psgraph-overlap in the range of propensity scores across treatment and comparison groups
2a. Balance of propensity scores-from t-tests in STATA>pscore..., detail-mean of propensity score in each block (quintiles or deciles) is similar (not significantly signficant) 
2b. Balance of propensity scores-from standardized differences, their p-values, and the graph listing size of standardized differences
*/

/*STEP 4. MATCHING STRATEGY: NEAREST NEIGHBOR MATCHING*/
/*sort random order before psmatch2*/
sort _ranorder
psmatch2 referIM, outcome (n_va_op_post12) pscore (_mypscore) 

/*STEP 5a.1 EVALUATE NEAREST NEIGBOR 1-TO-1 MATCHING WITH NO REPLACEMENT*/
pstest pcvisit ervisit hospitalization ///
age_group gender raceethnicity maritalstatus disability medicaid rurality drivedistance_pc nextkin fraility ///
copd med_nonadhere asthma ptsd substanceuse anxiety bipolar stroke renalfailure ischemicheartdisease congestiveheartfailure hypertension diabetes ///
dementia peripvasculardisease liverdisease arrhythmia fluidelectrodisorder anemia coagulopathy pulmonarycircdisorder depression psychosis, treated(referIM) both hist

/*generating & keepings obs that matched with PIM obs */
sort _id
tab _support, missing
tab _treated, missing
tab _support _treated, missing

keep if _support==1 /*(160 observations deleted)*/

/*LIST OUT IDs of referred patients (_treated==1) & each of their nearest neighter in NEAREST NEIGBOR 1-TO-1 MATCHING */
sort _id
list _id _treated _n1 if _treated==1

egen _id_1to1nn_rep=anymatch(_id), values ///
(705 1414 1424 1430 1536 1582 1593 1805 1883 2291 2334 2363 2425 2440 2828 2913 3296 3489 3500 3556 3626 3734 ///
4263 4293 4559 4947 4998 5065 5149 5155 5254 5268 5274 5356 5371 5477 5702 5956 6368 6500 6723 6741 6881 6939 ///
6945 6993 7073 7139 7485 7526 7565 7634 7635 7671 7801 7818 7883 7992 8027 8031 8043 8107 8144 8163 8189 8208 ///
8251 8301 8350 8368 8382 8446 8672 8673 8707 8726 8734 8745 8851 8935 9035 9056 9065 9087 9180 9199 9210 9244 ///
9263 9463 9466 9528 9539 9601 9634 9692 9804 9889 9904 9930 9993 10018 10035 10051 10169 10197 10231 10348 10368 10425 ///
10595 10598 10612 10641 10661 10708 10747 10808 10895 10982 10997 11009 11081 11125 11153 11195 11233 11274 11305 11364 11403 11415 ///
11417 11434 11445 11456 11478 11479 11496 11508 11528 11535 11548 11672 11677 11683 11692 11727 11733 11820 11849 11859 11862 11916 ///
11958 11967 11974 11977 12098 12105 12151 12179 12191 12254 12256 12271 12354 12404 12419 12436 12463 12482 12505 12583 12646 12672 ///
12699 12722 12775 12852 12858 12892 12899 12913 12914 12941 12975 12993 13000 13029 13032 13053 13065 13071 13103 13109 13127 13186 ///
13197 13206 13260 13264 13281 13311 13319 13323 13329 13335 13342 13348 13354 13370 13383 13385 13401 13426 13449 13460 13481 13495 ///
13509 13519 13540 13545 13563 13575 13579 13580 13582 13605 13606 13636 13644 13646 13667 13676 13699 13709 13725 13729 13735 13741 ///
13743 13752 13757 13762 13763 13798 13802 13811 13814 13820 13833 13835 13839 13846 13904 13926 13927 13951 13953 13955 13962 13965 ///
13972 13980 13982 14012 14033 14059 14080 14083 14088 14121 14124 14147 14154 14167 14170 14171 14173 14176 14185 14188 14196 14200 ///
14209 14220 14235 14256 14275 14277 14280 14284 14294 14297 14318 14324 14333 14335 14338 14345 14361 14373 14377 14379 14392 14404 ///
14409 14426 14443 14449 14454 14455 14464 14467 14468 14480 14481 14492 14499 14500 14503 14504 14505 14506 14511 14515 14521 14526 ///
14527 14531 14542 14553 14557 14558 14561 14565 14571 14584 14590 14594 14599 14600 14608 14627 14628 14630 14634 14645 14649 14656 ///
14661 14663 14674 14679 14680 14681 14683 14684 14687 14688 14689 14707 14708 14710 14711 14720 14722 14734 14736 14737 14738 14739 ///
14740 14744 14746 14747 14753 14755 14758 14759 14761 14763 14764 14765 14768 14769 14770 14772 14773 14779 14788 14795 14797 14799 ///
14801 14802 14804 14805 14809 14810 14817 14819 14822 14826 14827 14830 14831 14832 14833 14835 14836 14837 14840 14841 14842 14843 ///
14844 14845 14846 14847 14848 14849 14850 14851 14852 14853 14854 14855 14856 14857 14858 14859 14860 14861 14862 14863 14864 14865 ///
14866 14867 14868 14869 14870 14871 14872 14873 14874 14875 14876 14877 14878 14879 14880 14881 14882 14883 14884 14885 14886 14887 ///
14888 14889 14890 14891 14892 14893 14894 14895 14896 14897 14898 14899 14900 14901 14902 14903 14904 14905 14906 14907 14908 14909 ///
14910 14911 14912 14913 14914 14915 14916 14917 14918 14919 14920 14921 14922 14923 14924 14925 14926 14927 14928 14929 14930 14931 ///
14932 14933 14934 14935 14936 14937 14938 14939 14940 14941 14942 14943 14944 14945 14946 14947 14948 14949 14950 14951 14952 14953 ///
14954 14955 14956 14957 14958 14959 14960 14961 14962 14963 14964 14965 14966 14967 14968 14969 14970 14971 14972 14973 14974 14975 ///
14976 14977 14978 14979 14980 14981 14982 14983 14984 14985 14986 14987 14988 14989 14990 14991 14992 14993 14994 14995 14996 14997 ///
14998 14999 15000 15001 15002 15003 15004 15005 15006 15007 15008 15009 15010 15011 15012 15013 15014 15015 15016 15017 15018 15019 ///
15020 15021 15022 15023 15024 15025 15026 15027 15028 15029 15030 15031 15032 15033 15034 15035 15036 15037 15038 15039 15040 15041 ///
15042 15043 15044 15045 15046 15047 15048 15049 15050 15051 15052 15053 15054 15055 15056 15057 15058 15059 15060 15061 15062 15063 ///
15064 15065 15066 15067 15068 15069 15070 15071 15072 15073 15074 15075 15076 15077 15078 15079 15080 15081 15082 15083 15084 15085 ///
15086 15087 15088 15089 15090 15091 15092 15093 15094 15095 15096 15097 15098 15099 15100 15101 15102 15103 15104 15105 15106 15107 ///
15108 15109 15110 15111 15112 15113 15114 15115 15116 15117 15118 15119 15120 15121 15122 15123 15124 15125 15126 15127 15128 15129 ///
15130 15131 15132 15133 15134 15135 15136 15137 15138 15139 15140 15141 15142 15143 15144 15145 15146 15147 15148 15149 15150 15151 ///
15152 15153 15154 15155 15156 15157 15158 15159 15160 15161 15162 15163 15164 15165 15166 15167 15168 15169 15170 15171 15172 15173 ///
15174 15175 15176 15177 15178 15179 15180 15181 15182 15183 15184 15185 15186 15187 15188 15189 15190 15191 15192 15193 15194 15195 ///
15196 15197 15198 15199 15200 15201 15202 15203 15204 15205 15206 15207 15208 15209 15210 15211 15212 15213 15214 15215 15216 15217 ///
15218 15219 15220 15221 15222 15223 15224 15225 15226 15227 15228 15229 15230 15231 15232 15233 15234 15235 15236 15237 15238 15239 ///
15240 15241 15242 15243 15244 15245 15246 15247 15248 15249 15250 15251 15252 15253 15254 15255 15256 15257 15258 15259 15260 15261 ///
15262 15263 15264 15265 15266 15267 15268 15269 15270 15271 15272 15273 15274 15275 15276 15277 15278 15279 15280 15281 15282 15283 ///
15284 15285 15286 15287 15288 15289 15290 15291 15292 15293 15294 15295 15296)

keep if _id_1to1nn_rep==1 /* (14,425 observations deleted)---> n=871=*/

/* DATA CHECK
tab1 referIM, missing
-> tabulation of referIM 

       referIM |   Freq.   Percent    Cum.
-----------------------------+-----------------------------------
not referred controls |    415    47.65    47.65
IM referred patients |    456    52.35   100.00
-----------------------------+-----------------------------------
        Total |    871   100.00

*/

/*Data with only on support (_support==1) or 1-to-1 nearest neighbor matched with replacment for PIM & SHEP controls */
use "H:\My Drive\Analytics\StatisticalModelings\ps_aftermatched.dta", clear 

save "H:\My Drive\Analytics\StatisticalModelings\ps_aftermatched.dta", replace

 
/*DESCRIPTIVE STATISTICS AFTER MATCHING-NEAREST NEIGBOR 1-TO-1 MATCHING WTIH REPLACMENT*/
use "H:\My Drive\Analytics\StatisticalModelings\ps_aftermatched.dta", clear /*n=871 (456 vs 415*/

global aftermatch_1to1 ///
age_group gender raceethnicity maritalstatus disability medicaid rurality ///
drivedistance_pc nextkin fraility /// 
copd med_nonadhere asthma ptsd substanceuse anxiety bipolar stroke renalfailure ischemicheartdisease congestiveheartfailure hypertension ///
diabetes dementia peripvasculardisease liverdisease arrhythmia fluidelectrodisorder anemia coagulopathy pulmonarycircdisorder depression psychosis ///
pcvisit ervisit hospitalization

foreach var of global aftermatch_1to1 {
 tab referIM `var' , cell row col chi2
 }
 
/*STEP 6 - PROPENSITY MATCHED (1-TO-1 NEAREST NEIGHBOR WITH REPLACEMENT) ON COST & UTILIZATION*/
/*OUTPATIENT COSTS & UTILIZATION*/
use "H:\My Drive\Analytics\StatisticalModelings\ps_aftermatched.dta", clear

/*PIM outpatient utillzation */
global outpatient_visits ///
n_er n_observation n_triage n_urgent_care n_primary_care n_primarycare_phone n_womenshealth n_geriatric n_geriatric_phone n_mhicm n_mental_substanceabuse ///
n_mental_phone n_specialty n_specialty_phone n_social_work n_clinical_pharmacy n_care_mgmt n_care_mgmt_phone n_homeless n_homeless_phone ///
n_palliativehospice n_telehealth n_home_care n_rehabilitation n_hbpc n_long_term_care n_other ntotal_outpatient

/*ATET for outcome n_va_op_post12*/
foreach var of global outpatient_visits {
teffects psmatch (`var') (referIM ///
pcvisit ervisit hospitalization ///
age_group gender raceethnicity maritalstatus disability medicaid rurality drivedistance_pc nextkin fraility ///
copd med_nonadhere asthma ptsd substanceuse anxiety bipolar stroke renalfailure ischemicheartdisease congestiveheartfailure hypertension diabetes ///
dementia peripvasculardisease liverdisease arrhythmia fluidelectrodisorder anemia coagulopathy pulmonarycircdisorder depression psychosis), ///
nneighbor (1) atet 

/*MACRO: PROPENSITY SCORE MATCHING TREATMENT EFFECTS FOR OUTPATIENT COST OUTCOMES*/  
global outpatient_costs ///
m_er m_observation m_triage m_urgent_care m_primary_care m_primarycare_phone m_womens_health m_geriatric m_geriatric_phone m_mhicm m_mental_substanceabuse ///
m_mental_phone m_specialty m_specialty_phone m_social_work m_clinical_pharmacy m_care_mgmt m_care_mgmt_phone m_homeless m_homeless_phone ///
m_palliativehospice m_telehealth m_home_care m_rehabilitation m_hbpc m_long_term_care m_other mtotal_op

 
/*ATET FOR OUTPATIENT COST OUTCOMES*/
foreach var of global outpatient_costs {
teffects psmatch (`var') (referIM ///
pcvisit ervisit hospitalization ///
age_group gender raceethnicity maritalstatus disability medicaid rurality drivedistance_pc nextkin fraility ///
copd med_nonadhere asthma ptsd substanceuse anxiety bipolar stroke renalfailure ischemicheartdisease congestiveheartfailure hypertension diabetes ///
dementia peripvasculardisease liverdisease arrhythmia fluidelectrodisorder anemia coagulopathy pulmonarycircdisorder depression psychosis), ///
 nneighbor (1) atet 
}

 
/*MACRO: PROPENSITY SCORE MATCHING TREATMENT EFFECTS FOR INPATIENT UTILIZATION, LENGTH OF STAY & COSTS OUTCOMES*/ 
global inpatient_vists_los_costs ///
n_inpatient0 n_inpatient1 n_inpatient2 n_inpatient3 n_inpatient5 n_inpatient6 n_inpatient7 n_inpatient8 n_inpatient9 ntotal_inpatient ///
los_inpatient0 los_inpatient1 los_inpatient2 los_inpatient3 los_inpatient5 los_inpatient6 los_inpatient7 los_inpatient8 los_inpatient9 ///
m_inpatient0 m_inpatient1 m_inpatient2 m_inpatient3 m_inpatient5 m_inpatient6 m_inpatient7 m_inpatient8 m_inpatient9 ntotal_inpatient 
 
/*ATET FOR INPATIENT UTILIZATION, LENGTH OF STAY & COSTS OUTCOMES*/
foreach var of global inpatient_vists_los_costs {
teffects psmatch (`var') (referIM ///
pcvisit ervisit hospitalization ///
age_group gender raceethnicity maritalstatus disability medicaid rurality drivedistance_pc nextkin fraility ///
copd med_nonadhere asthma ptsd substanceuse anxiety bipolar stroke renalfailure ischemicheartdisease congestiveheartfailure hypertension diabetes ///
dementia peripvasculardisease liverdisease arrhythmia fluidelectrodisorder anemia coagulopathy pulmonarycircdisorder depression psychosis), ///
nneighbor (1) atet 
}
 

/*MACRO: PROPENSITY SCORE MATCHING TREATMENT EFFECTS FOR COMMUNITY CARE OUTPATIENT & INPATIENT COST & UTILIZATION*/
global communitycare n_comm_outpatient n_comm_contract n_comm_nursing n_comm_inpatient n_comm_er m_comm_outpatient m_comm_contract m_comm_nursing m_comm_inpatient m_comm_er

/*ATET for outcome nonVA_costutil_post12 */
foreach var of global communitycare { 
teffects psmatch (`var') (referIM ///
pcvisit ervisit hospitalization ///
age_group gender raceethnicity maritalstatus disability medicaid rurality drivedistance_pc nextkin fraility ///
copd med_nonadhere asthma ptsd substanceuse anxiety bipolar stroke renalfailure ischemicheartdisease congestiveheartfailure hypertension diabetes ///
dementia peripvasculardisease liverdisease arrhythmia fluidelectrodisorder anemia coagulopathy pulmonarycircdisorder depression psychosis), ///
nneighbor (1) atet
 }
 
/*TOTAL HEALTH CARE SYSTEM COSTS & UTLIZATION*/
global total_healthsystem n_totalhcs m_totalhcs 
 
/*ATET for outcome TotalVA*/
foreach var of global total_healthsystem {
teffects psmatch (`var') (referIM ///
pcvisit ervisit hospitalization ///
age_group gender raceethnicity maritalstatus disability medicaid rurality drivedistance_pc nextkin fraility ///
copd med_nonadhere asthma ptsd substanceuse anxiety bipolar stroke renalfailure ischemicheartdisease congestiveheartfailure hypertension diabetes ///
dementia peripvasculardisease liverdisease arrhythmia fluidelectrodisorder anemia coagulopathy pulmonarycircdisorder depression psychosis), ///
nneighbor (1) atet 
}
 
/*MORTALITY DATA*/
global mortality1year_2year mortality_1year mortality_2year
 
/*ATET for outcome mortality_1_2year*/
foreach var of global mortality1year_2year {
teffects psmatch (`var') (referIM ///
pcvisit ervisit hospitalization ///
age_group gender raceethnicity maritalstatus disability medicaid rurality drivedistance_pc nextkin fraility ///
copd med_nonadhere asthma ptsd substanceuse anxiety bipolar stroke renalfailure ischemicheartdisease congestiveheartfailure hypertension diabetes ///
dementia peripvasculardisease liverdisease arrhythmia fluidelectrodisorder anemia coagulopathy pulmonarycircdisorder depression psychosis), ///
 nneighbor (1) atet 
}

/*DIFFERENCE IN DIFFERENCES ANALYSIS*/
sort _id
generate dnd_id=_n

save "H:\My Drive\Analytics\StatisticalModelings\ps_aftermatched.dta", replace
use "H:\My Drive\Analytics\StatisticalModelings\ps_aftermatched.dta", clear /*n=871*/


/*DnD data sets*/
save "H:\My Drive\Analytics\StatisticalModelings\ps_aftermatched_DnDwide.dta", replace
use "H:\My Drive\Analytics\StatisticalModelings\ps_aftermatched_DnDwide.dta", clear


/*RESHAPE PS DATA FROM WIDE TO LONG*/
reshape long 
n_er_ n_observation_ n_triage_ n_urgent_care_ n_primary_care_ n_primarycare_phone_ n_womens_health_ n_geriatric_ n_geriatric_phone_ n_mhicm_ n_mental_substanceabuse_ n_mental_phone_ ///
n_specialty_ n_specialty_phone_ n_social_work_ n_clinical_pharmacy_ n_care_mgmt_ n_care_mgmt_phone_ n_homeless_ n_homeless_phone_ n_palliativehospice_ n_telehealth_ n_home_care_ ///
n_rehabilitation_ n_hbpc_ n_long_tern_care_ n_other_ mtotal_op_ ///
m_er_ m_observation_ m_triage_ m_urgent_care_ m_primary_care_ m_primarycare_phone_ m_womens_health_ m_geriatric_ m_geriatric_phone_ m_mhicm_ m_mental_substanceabuse_ m_mental_phone_ ///
m_specialty_ m_specialty_phone_ m_social_work_ m_clinical_pharmacy_ m_care_mgmt_ m_care_mgmt_phone_ m_homeless_ m_homeless_phone_ m_palliativehospice_ m_telehealth_ m_home_care_ m_rehabilitation_ ///
m_hbpc_ m_long_term_care_ m_other_ mtotal_op_ ///
n_inpatient0_ n_inpatient1_ n_inpatient2_ n_inpatient3_ n_inpatient5_ n_inpatient6_ n_inpatient7_ n_inpatient8_ n_inpatient9_ ntotal_inpatient_ ///
los_inpatient0_ los_inpatient1_ los_inpatient2_ los_inpatient3_ los_inpatient5_ los_inpatient6_ los_inpatient7_ los_inpatient8_ los_inpatient9_ ///
m_inpatient0_ m_inpatient1_ m_inpatient2_ m_inpatient3_ m_inpatient5_ m_inpatient6_ m_inpatient7_ m_inpatient8_ m_inpatient9_ ntotal_inpatient_ ///
n_comm_outpatient_ n_comm_contract_ n_comm_nursing_ n_comm_inpatient_ n_comm_er_ m_comm_outpatient_ m_comm_contract_ m_comm_nursing_ m_comm_inpatient_ m_comm_er_ ///
n_totalhcs _ m_totalhcs_ ///
mortality_1year _ mortality_2year_, i(dnd_id) j(post)

save "H:\My Drive\Analytics\StatisticalModelings\ps_aftermatched_DnDlong.dta", replace 
use "H:\My Drive\Analytics\StatisticalModelingsps_aftermatched_DnDlong.dta", clear

/*DIFFERENCE IN DIFFERENCE ANALYSIS*/
global DinD_ana ///
n_er_ n_observation_ n_triage_ n_urgent_care_ n_primary_care_ n_primarycare_phone_ n_womens_health_ n_geriatric_ n_geriatric_phone_ n_mhicm_ n_mental_substanceabuse_ n_mental_phone_ ///
n_specialty_ n_specialty_phone_ n_social_work_ n_clinical_pharmacy_ n_care_mgmt_ n_care_mgmt_phone_ n_homeless_ n_homeless_phone_ n_palliativehospice_ n_telehealth_ n_home_care_ ///
n_rehabilitation_ n_hbpc_ n_long_tern_care_ n_other_ mtotal_op_ ///
m_er_ m_observation_ m_triage_ m_urgent_care_ m_primary_care_ m_primarycare_phone_ m_womens_health_ m_geriatric_ m_geriatric_phone_ m_mhicm_ m_mental_substanceabuse_ m_mental_phone_ ///
m_specialty_ m_specialty_phone_ m_social_work_ m_clinical_pharmacy_ m_care_mgmt_ m_care_mgmt_phone_ m_homeless_ m_homeless_phone_ m_palliativehospice_ m_telehealth_ m_home_care_ m_rehabilitation_ ///
m_hbpc_ m_long_term_care_ m_other_ mtotal_op_ ///
n_inpatient0_ n_inpatient1_ n_inpatient2_ n_inpatient3_ n_inpatient5_ n_inpatient6_ n_inpatient7_ n_inpatient8_ n_inpatient9_ ntotal_inpatient_ ///
los_inpatient0_ los_inpatient1_ los_inpatient2_ los_inpatient3_ los_inpatient5_ los_inpatient6_ los_inpatient7_ los_inpatient8_ los_inpatient9_ ///
m_inpatient0_ m_inpatient1_ m_inpatient2_ m_inpatient3_ m_inpatient5_ m_inpatient6_ m_inpatient7_ m_inpatient8_ m_inpatient9_ ntotal_inpatient_ ///
n_comm_outpatient_ n_comm_contract_ n_comm_nursing_ n_comm_inpatient_ n_comm_er_ m_comm_outpatient_ m_comm_contract_ m_comm_nursing_ m_comm_inpatient_ m_comm_er_ ///
n_totalhcs _ m_totalhcs_ 

/*DinD - unadjusted*/
foreach var of global DinD_ana {
regress `var' post##_treated, r 
}


/*DinD controlling for demographics, social vulnerabilities & comorbidities*/
foreach var of global DinD_ana {
regress `var' post##_treated ///
pcvisit ervisit hospitalization ///
age_group gender raceethnicity maritalstatus disability medicaid rurality drivedistance_pc nextkin fraility ///
copd med_nonadhere asthma ptsd substanceuse anxiety bipolar stroke renalfailure ischemicheartdisease congestiveheartfailure hypertension diabetes ///
dementia peripvasculardisease liverdisease arrhythmia fluidelectrodisorder anemia coagulopathy pulmonarycircdisorder depression psychosis, r 
}


/*MACRO: PROPENSITY SCORE MATCHING TREATMENT EFFECTS FOR DELTA YS*/ 
global delta_Ys ///
n_er_tdelta n_observation_tdelta n_triage_tdelta n_urgent_care_tdelta n_primary_care_tdelta n_primarycare_phone_tdelta n_womens_health_tdelta n_geriatric_tdelta n_geriatric_phone_tdelta ///
n_mhicm_tdelta n_mental_substanceabuse_tdelta n_mental_phone_tdelta n_specialty_tdelta n_specialty_phone_tdelta n_social_work_tdelta n_clinical_pharmacy_tdelta n_care_mgmt_tdelta n_care_mgmt_phone_tdelta ///
n_homeless_tdelta n_homeless_phone_tdelta n_palliativehospice_tdelta n_telehealth_tdelta n_home_care_tdelta n_rehabilitation_tdelta n_hbpc_tdelta n_long_tern_care_tdelta n_other_tdelta ntotal_op_tdelta ///
m_er_tdelta m_observation_tdelta m_triage_tdelta m_urgent_care_tdelta m_primary_care_tdelta m_primarycare_phone_tdelta m_womens_health_tdelta m_geriatric_tdelta m_geriatric_phone_tdelta m_mhicm_tdelta /// 
m_mental_substanceabuse_tdelta m_mental_phone_tdelta m_specialty_tdelta m_specialty_phone_tdelta m_social_work_tdelta m_clinical_pharmacy_tdelta m_care_mgmt_tdelta m_care_mgmt_phone_tdelta ///
m_homeless_tdelta m_homeless_phone_tdelta m_palliativehospice_tdelta m_telehealth_tdelta m_home_care_tdelta m_rehabilitation_tdelta m_hbpc_tdelta m_long_term_care_tdelta m_other_tdelta mtotal_op_tdelta ///
n_inpatient0_tdelta n_inpatient1_tdelta n_inpatient2_tdelta n_inpatient3_tdelta n_inpatient5_tdelta n_inpatient6_tdelta n_inpatient7_tdelta n_inpatient8_tdelta n_inpatient9_tdelta ntotal_inpatient_tdelta ///
los_inpatient0_tdelta los_inpatient1_tdelta los_inpatient2_tdelta los_inpatient3_tdelta los_inpatient5_tdelta los_inpatient6_tdelta los_inpatient7_tdelta los_inpatient8_tdelta los_inpatient9_tdelta ///
m_inpatient0_tdelta m_inpatient1_tdelta m_inpatient2_tdelta m_inpatient3_tdelta m_inpatient5_tdelta m_inpatient6_tdelta m_inpatient7_tdelta m_inpatient8_tdelta m_inpatient9_tdelta ntotal_inpatient_tdelta ///
n_comm_outpatient_tdelta n_comm_contract_tdelta n_comm_nursing_tdelta n_comm_inpatient_tdelta n_comm_er_tdelta ///
m_comm_outpatient_tdelta m_comm_contract_tdelta m_comm_nursing_tdelta m_comm_inpatient_tdelta m_comm_er_tdelta ///
n_totalhcs _tdelta m_totalhcs_tdelta


/*ATET for delta Ys*/
 foreach var of global delta_Ys {
 teffects psmatch (`var') (referIM ///
 pcvisit ervisit hospitalization ///
age_group gender raceethnicity maritalstatus disability medicaid rurality drivedistance_pc nextkin fraility ///
copd med_nonadhere asthma ptsd substanceuse anxiety bipolar stroke renalfailure ischemicheartdisease congestiveheartfailure hypertension diabetes ///
dementia peripvasculardisease liverdisease arrhythmia fluidelectrodisorder anemia coagulopathy pulmonarycircdisorder depression psychosis), ///
 nneighbor (1) atet 
}

/*SUBGROUP ANALYSIS-PROPENSITY SCORE MATCHING FOR DELTA YS* */

use "H:\My Drive\Analytics\StatisticalModelings\ps_numeric_ids.dta", clear 

/*Dataset after matching*/
use "H:\My Drive\Analytics\StatisticalModelings\ps_aftermatched.dta", clear 
save "H:\My Drive\Analytics\StatisticalModelings\ps_aftermatched.dta", replace

/*STEPS IN SUBGROUP ANALYSIS
1. Use the dataset (\ps_numeric_ids.dta) to get IDs of referred patietns with 1 or more visits
2. Using the analytic dataset (ps_aftermatched.dta), I get the IDs of these referred respondents & their nearest neighbor controls
3. Keep them (i.e. drop everyone else)
4. Perform the analysis of the difference in utilzation & cost as before
*/

use "O:\PCS_PIMprog\Caroline\Propensity\Util_cost\Data\ps_numeric_ids.dta", clear

use "H:\My Drive\Analytics\StatisticalModelings\ps_aftermatched.dta", clear 

/*Patients with referrals PLUS with 1 or more visits*/
egen refer_plusvisit=anymatch(_patient_numids), values(1000687772 1000730380 1000740091 1000741393 1000756975 1000760512 1000799074 1000862703 1000878012 1000932528 1000995462 1001034486 1001035931 1001036057 ///
1001036462 1001038294 1001038387 1001041526 1001043327 1001044645 1001050315 1001050804 1001057456 1001061140 1001062095 1001062171 1001062349 1001064397 1001065943 1001070142 1001077695 1001079513 1001088417 ///
1001091864 1001093224 1001230662 1001236306 1001252029 1001252367 1001252583 1001252820 1001253006 1001253024 1001253156 1001253339 1001253880 1001254038 1001254257 1001254526 1001254978 1001255206 1001255355 ///
1001255769 1001256593 1001256718 1001257737 1001258061 1001258068 1001258207 1001258569 1001260087 1001260544 1001261594 1001261736 1001262668 1001262728 1001262915 1001262917 1001262943 1001263668 1001264785 ///
1001265615 1001266239 1001266584 1001266771 1001267401 1001267465 1001268950 1001271960 1001277456 1001285584 1001286722 1001288557 1001297295 1001297510 1001303616 1001304206 1001308007 1001308939 1001311515 ///
1001410203 1001507884 1001715034 1001743203 1001775906 1001842489 1001984364 1002033816 1002251641 1002279695 1002281205 1002295029 1002339417 1002385094 1002417134 1002452894 1002718314 1002723571 1002782154 ///
1002887574 1003154712 1003156915 1003446556 1003485182 1003496330 1003509766 1003587745 1003588260 1003592751 1003594701 1003604036 1003606319 1003608207 1003611160 1003616740 1003618281 1003619703 1003620329 ///
1003624836 1003624844 1003626782 1003697202 1003845304 1003927927 1004002299 1004230320 1004232085 1004321674 1004446170 1004479402 1004483180 1004640306 1004718496 1004777914 1004778312 1004778816 1004783235 ///
1004789973 1004797666 1004969107 1004990261 1005230939 1005326864 1005345293 1005346476 1005350548 1005352193 1005353198 1005354314 1005355036 1005355310 1005356101 1005357138 1005360104 1005361922 1005364698 ///
1005366024 1005374056 1005376076 1005376923 1005383023 1005387314 1005403203 1005406325 1005410069 1005412655 1005417188 1005521591 1005549808 1005550146 1005550706 1005550890 1005551168 1005551322 1005551540 ///
1005551651 1005551699 1005552778 1005552912 1005554665 1005556674 1005558667 1005560421 1005560991 1005562170 1005562850 1005570353 1005571642 1005572130 1005579152 1005588446 1005590582 1005613128 1005895823 ///
1005896184 1005963902 1006261492 1006484134 1006781290 1006928908 1007102804 1007122976 1007144183 1007209343 1007216151 1007220447 1007576760 1007624775 1007712083 1007861473 1007872018 1008215943 1008272059 ///
1008283836 1008424289 1008464895 1008524600 1008897863 1008912217 1009135378 1009213262 1009247746 1009259819 1009282855 1009322405 1009339298 1009437740 1009583432 1009597576 1009639450 1009653405 1009697915 ///
1009715343 1009730101 1009738622 1009768234 1009771202 1009830860 1009898174 1009958469 1010016157 1010067567 1010209400 1010273482 1010298851 1010346524 1010408690 1010415029 1010443372 1010498041 1010533468 ///
1010605940 1010626158 1010670748 1010722218 1010735073 1010819457 1010824373 1010831082 1010875755 1010949971 1011057456 1011058779 1011176254 1011279882 1011280531 1011297757 1011310991 1011328028 1011539691 ///
1011556364 1011591489 1011670409 1011693744 1011706888 1011744849 1011759768 1011864177 1011907830 1011911107 1011918282 1011950685 1011985113 1012058129 1012211525 1012279220 1012296592 1012358423 1012425959 ///
1012426256 1012436587 1012459018 1012530917 1012536029 1012789512 1012814763 1012899374 1013210581 1013230259 1013684202 1013695846 1014104340 1014303830 1014395932 1014427021 1014461026 1014473294 1014623639 ///
1014926317 1014962487 1015189527 1015313519 1015378931 1015477044 1015687809 1015734529 1015950826 1015966795 1016052307 1016113208 1016258334 1016408031 1016462146 1016492301 1016534630 1016650122 1016705439 ///
1016758149 1016821499 1016822092 1017008738 1017316603 1017386932 1017463342 1017502296 1017503961 1017693997 1017972674 1017994281 1018429753 1018512468 1018530764 1018544073 1018702734 1018824227 1018856531 ///
1018873779 1018902665 1019095279 1019175208 1019281169 1019723584 1020099027 1020115808 1020144475 1023646916 1023873177 1024396209 1024843128 1026502071 1031458976 1031910178 1034189850 1034940252 1039865620 ///
1042442977 1042917873 1043683680 1043729080)

/*list of referred with 1+ visits and their nearet neighbor so I know how to subset the group for analysis*/
list _id _n1 referIM _treated if refer_plusvisit==1

/*nearest neighbor to referred patients with 1 or more visits */
egen neighbor=anymatch(_id), values(705 1424 1430 1536 1593 1805 1883 2334 2363 2425 2440 2828 2913 3296 3489 3500 3556 4263 4293 4559 4947 4998 5065 5149 5155 5254 5268 5274 5356 5371 5477 5702 5956 6741 6881 ///
6939 6945 6993 7073 7139 7526 7565 7634 7635 7671 7801 7818 7883 8027 8043 8107 8163 8189 8208 8251 8301 8350 8446 8672 8673 8707 8726 8734 8745 8851 9035 9065 9087 9180 9199 9210 9244 9263 9463 9466 9528 9539 9601 ///
9692 9804 9889 9904 9930 9993 10018 10051 10169 10197 10231 10348 10368 10425 10598 10612 10641 10661 10708 10747 10808 10895 10982 10997 11081 11125 11153 11195 11233 11305 11364 11403 11415 11417 11434 11445 11456 //
11478 11479 11496 11508 11528 11535 11548 11672 11677 11683 11692 11727 11733 11820 11859 11862 11916 11958 11967 11974 11977 12098 12105 12151 12179 12191 12254 12256 12271 12354 12404 12419 12463 12482 12505 12646 ///
12699 12722 12775 12852 12892 12914 12941 12975 12993 13029 13032 13053 13065 13071 13103 13109 13127 13186 13197 13206 13260 13264 13281 13319 13323 13329 13335 13342 13348 13370 13385 13401 13426 13449 13460 13495 ///
13519 13540 13545 13563 13579 13580 13582 13605 13606 13644 13646 13667 13676 13699 13709 13725 13729 13735 13741 13743 13752 13757 13762 13763 13798 13802 13811 13814 13814 13820 13820 13833 13846 13926 13951 13953 ///
13962 13965 13972 13980 13982 14012 14059 14080 14083 14088 14121 14154 14167 14170 14171 14176 14185 14188 14196 14200 14209 14220 14256 14275 14280 14284 14294 14294 14297 14318 14324 14324 14333 14335 14338 14345 ///
14373 14377 14379 14379 14392 14404 14443 14449 14454 14455 14464 14468 14481 14492 14499 14503 14504 14505 14506 14515 14515 14521 14521 14526 14542 14553 14557 14558 14561 14561 14565 14571 14584 14590 14590 14599 ///
14600 14634 14645 14649 14656 14661 14661 14674 14679 14680 14681 14683 14684 14687 14688 14688 14689 14707 14707 14708 14710 14710 14711 14720 14722 14737 14738 14738 14739 14740 14746 14747 14753 14755 14758 14759 ///
14761 14763 14764 14764 14764 14764 14765 14768 14769 14772 14773 14773 14795 14795 14795 14797 14797 14799 14801 14802 14804 14804 14809 14809 14810 14817 14819 14819 14819 14822 14826 14827 14830 14831 14832 14833 ///
14833 14835 14835 14837 14840 14840 14840 14840 14840)

/*DATA CHECKS*/
tab _treated if neighbor==1
tab referIM if neighbor==1

tab referIM refer_plusvisit, missing
tab referIM neighbor, missing

/*SUBGROUP ANALYIS OF REFERRED PATIENTS WITH 1+ VISITS & THEIR NEAREST NEIGBOR*/
keep if PIMvisit1plus==1 | neighbor==1 /*(150 observations deleted) --> n=721*/


/*DEMOGRAPHICS*/
global aftermatch_1to1 ///
age_group gender raceethnicity maritalstatus disability medicaid rurality drivedistance_pc nextkin fraility ///
copd med_nonadhere asthma ptsd substanceuse anxiety bipolar stroke renalfailure ischemicheartdisease congestiveheartfailure hypertension diabetes ///
dementia peripvasculardisease liverdisease arrhythmia fluidelectrodisorder anemia coagulopathy pulmonarycircdisorder depression psychosis
pcvisit ervisit hospitalization

foreach var of global aftermatch_1to1 {
tab refer_plusvisit `var' , cell row col chi2
}


/*MACRO: SUBGROUP ANALYIS OF REFERRED PATIENTS WITH 1+ VISITS  - PROPENSITY SCORE MATCHING TREATMENT EFFECTS FOR DELTA YS */ 
global delta_Ys n_va_er_delta n_OBSERVATION_delta n_TRIAGE_delta n_URGENT_CARE_delta n_PRIMARY_CARE_delta n_PRIMARYCARE_PHN_delta n_WOMENs_HEALTH_delta n_GERIATRIC_delta n_GERIATRIC_PHONE_delta n_MHICM_delta n_MENTAL_SUBABU_delta n_MENTAL_PHONE_delta n_SPECIALTY_delta n_SPECIALTY_PHONE_delta n_SOCIAL_WORK_delta n_CLINICAL_PHARMACY_delta n_CARE_MGMT_delta n_CARE_MGMT_PHONE_delta n_HOMELESS_delta n_HOMELESS_PHONE_delta n_PALLIATIVEHOSPICE_delta n_TELEHEALTH_delta n_HOME_CARE_delta n_REHAB_delta n_HBPC_delta n_LONG_TERM_CARE_delta n_OTHER_delta n_va_op_delta1 n_va_op_delta2 m_va_er_delta m_OBSERVATIOm_delta m_TRIAGE_delta m_URGENT_CARE_delta m_PRIMARY_CARE_delta m_PRIMARYCARE_PHm_delta m_WOMENs_HEALTH_delta m_GERIATRIC_delta m_GERIATRIC_PHONE_delta m_MHICM_delta m_MENTAL_SUBABU_delta m_MENTAL_PHONE_delta m_SPECIALTY_delta m_SPECIALTY_PHONE_delta m_SOCIAL_WORK_delta m_CLINICAL_PHARMACY_delta m_CARE_MGMT_delta m_CARE_MGMT_PHONE_delta m_HOMELESS_delta m_HOMELESS_PHONE_delta m_PALLIATIVEHOSPICE_delta m_TELEHEALTH_delta m_HOME_CARE_delta m_REHAB_delta m_HBPC_delta m_LONG_TERM_CARE_delta m_OTHER_delta m_va_op_delta1 m_va_op_delta2 n_ip_0_delta n_ip_1_delta n_ip_2_delta n_ip_3_delta n_ip_5_delta n_ip_6_delta n_ip_7_delta n_ip_8_delta n_ip_9_delta n_ip_va_delta los_ip_0_delta los_ip_1_delta los_ip_2_delta los_ip_3_delta los_ip_5_delta los_ip_6_delta los_ip_7_delta los_ip_8_delta los_ip_9_delta m_ip_0_delta m_ip_1_delta m_ip_2_delta m_ip_3_delta m_ip_5_delta m_ip_6_delta m_ip_7_delta m_ip_8_delta m_ip_9_delta m_ip_va_delta n_nva_op_delta n_nva_C_delta n_nva_N_delta n_IP_nva_delta n_nva_er_delta m_nva_op_delta m_nva_C_delta m_nva_N_delta m_IP_nva_delta m_nva_er_delta n_va_delta m_va_delta n_nva_delta m_nva_delta
 
/*ATET for delta Ys*/
 foreach var of global delta_Ys {
 teffects psmatch (`var') (referIM ///
pcvisit ervisit hospitalization///
age_group gender raceethnicity maritalstatus disability medicaid rurality drivedistance_pc nextkin fraility ///
copd med_nonadhere asthma ptsd substanceuse anxiety bipolar stroke renalfailure ischemicheartdisease congestiveheartfailure hypertension diabetes ///
dementia peripvasculardisease liverdisease arrhythmia fluidelectrodisorder anemia coagulopathy pulmonarycircdisorder depression psychosis), ///
nneighbor (1) atet
}




































