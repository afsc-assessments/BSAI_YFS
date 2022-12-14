////////////////////////////////////////////////////////////////////////
/// Flatfish Model "fm"
//  To update in 2007: Srec model (check for SigmaR approach, update document to reflect approach)
//  Added in wt-age variability and use that instead of const wt_pop matrix for statistics (similar to how selectivity variability is modeled)
//  Need to add: sigmaR prior code (if estimated) and change SR Likelihood to proper form 
//  (with added bit) 
//  check to ensure variances of mean wts are correct
// 
//  To do
//    --add MSE utility...
// 
//   Add sex ratio in survey selectivity, output on SR in fishery and survey, 
//   eff N using 3 methods, 
//   Weight calculation method: estimated wt in first phase, use between phases to set "actual" wts to value of estimates 
// 
///////////////////////////////////////////////////////////////////////
DATA_SECTION


  int oper_mod
  int mcmcmode
  int mcflag
 LOCAL_CALCS
  oper_mod = 0;
  mcmcmode = 0;
  mcflag   = 1;
  tmpstring=adprogram_name + adstring(".dat");
  if (argc > 1)
  {
    int on=0;
    if ( (on=option_match(argc,argv,"-ind"))>-1)
    {
      if (on>argc-2 | argv[on+1][0] == '-') 
      { 
        cerr << "Invalid input data command line option"
           " -- ignored" << endl;  
      }
      else
      {
        cntrlfile_name = adstring(argv[on+1]);
      }
    }
    if ( (on=option_match(argc,argv,"-om"))>-1)
    {
      oper_mod  = 1;
      cout<<"Got to operating model option "<<oper_mod<<endl;
    }
    if ( (on=option_match(argc,argv,"-mcmc"))>-1)
    {
      mcmcmode = 1;
    }
  }
  global_datafile= new cifstream(cntrlfile_name);
  if (!global_datafile)
  {
  }
  else
  {
    if (!(*global_datafile))
    {
      delete global_datafile;
      global_datafile=NULL;
    }
  }
 END_CALCS
  // Start reading control file......................................................
  !!ad_comm::change_datafile_name("mod.ctl");  // GRID
  !! *(ad_comm::global_datafile) >>  model_name; 
  !! *(ad_comm::global_datafile) >>  datafile; 
  init_int Growth_Option
  int phase_grwth
  int phase_wt
  int phase_sst
  int do_wt_only              //Trial flag for model using wt only in estimation
  !! phase_grwth=-2;
  !! phase_wt = -1;
  !! phase_sst=-1;
  !! if(Growth_Option==2) {phase_wt = 1;phase_sst=-1;} else if (Growth_Option==3) {phase_wt= -1; phase_sst=1;}
  init_int ABC_age_lb
  init_int phase_age_incr
  init_int phase_init_age_comp
  init_int phase_mn_rec
  // If phase for mean recruitment is less than zero, then that means it's a do-wt estimation only...
  // for options 2 and 3
  !! if ( phase_mn_rec<0) do_wt_only=1; else do_wt_only=0;
  init_int phase_rec_dev
  init_int phase_mn_f
  init_int phase_F40           //Phase to begin spawning biomass estimation
  init_int phase_fmort         //Phase to begin mortality estimation
  init_int phase_proj          //Phase to begin future projections
  init_int phase_logist_sel    //Phase to begin logistic selectivity estimation
  init_int phase_male_sel      //Phase to begin logistic selectivity estimation
  init_int phase_q
  init_number alpha_prior
  init_int phase_alpha
  init_number beta_prior
  init_int phase_beta
  init_int phase_m_f
  init_int phase_m_m
  init_int phase_sr
  init_int phase_bottom_temps  //
  init_int phase_sigmar        //
  init_int phase_wtfmsy        //
  init_number pf_sigma         // penalty cv for selectivity used in Fmsy calcs
  init_number a50_sigma        // 
  init_number slp_sigma        //
  // catchability values for rock sole from herding experiment
  init_number q_exp
  init_number q_sigma
  init_number m_exp
  init_number m_sigma
  init_number sigmar_exp
  init_number sigmar_sigma
  init_int nselages            //Number of selected ages

 LOCAL_CALCS
    log_input(Growth_Option);
    log_input(phase_grwth);
    log_input(phase_wt);
    log_input(phase_age_incr);
    log_input(phase_sst);
    log_input(ABC_age_lb);
    log_input(phase_F40);           //Phase to begin spawning biomass estimation
    log_input(phase_fmort);         //Phase to begin mortality estimation
    log_input(phase_proj);          //Phase to begin future projections
    log_input(phase_logist_sel);    //Phase to begin logistic selectivity estimation
    log_input(phase_male_sel);      //Phase to begin logistic selectivity estimation
    log_input(phase_q);
    log_input(phase_m_f);
    log_input(phase_m_m);
    log_input(phase_sr);
    log_input(phase_bottom_temps);  //
    log_input(phase_sigmar);        //
    log_input(phase_wtfmsy);        //
    log_input(pf_sigma);         // penalty cv for selectivity used in Fmsy calcs
    log_input(a50_sigma);        // 
    log_input(slp_sigma);        //
    log_input(q_exp);
    log_input(q_sigma);
    log_input(m_exp);
    log_input(m_sigma);
    log_input(sigmar_exp);
    log_input(sigmar_sigma);
    log_input(nselages);
    log_input(alpha_prior);
    log_input(phase_alpha);
    log_input(beta_prior);
    log_input(phase_beta);
 END_CALCS

  init_vector lambda(1,10)     //Vector of emphasis factors
  
  init_int styr_sr             // year to start spawner-recruit fit
  init_int endyr_sr            // year to end   spawner-recruit fit
  init_int styr_wt             // year to start variance mean wt at age for Fmsy etc
  init_int endyr_wt            // year to end   variance mean wt at age for Fmsy etc
  init_number yr1_futcat       // catch specified in endyr+1 (for 2-year projection for Tier 1)
  init_number yr2_futcat       // catch specified in endyr+2 (for 2-year projection for Tier 1) used for SSB_2 calc
  !! log_input(lambda); 
  !! log_input(styr_sr); log_input(endyr_sr);
  !! log_input(styr_wt); log_input(endyr_wt);
  !! log_input(yr1_futcat); log_input(yr1_futcat);
  // End reading control file......................................................

  // Open datafile specified........
  !!ad_comm::change_datafile_name(datafile);  
  init_int styr
  init_int endyr_in
	int endyr
	!! endyr = endyr_in;
  init_int nages
  // Read in allometric parameters
  init_number a_lw_f
  init_number b_lw_f
  init_number a_lw_m
  init_number b_lw_m

//!!cout <<styr<<endyr<<nages<<endl;

// Fishery specifics

  init_int nfsh                                        //Number of fisheries
  init_matrix catch_bio(1,nfsh,styr,endyr)             //Catch (t)
  !!log_input(catch_bio);

  // Read in fishery age data-------------------------------------
  init_ivector nyrs_fsh_age_c(1,nfsh)                    //Number of fishery age compositions (combined sex)
  init_ivector nyrs_fsh_age_s(1,nfsh)                    //Number of fishery age compositions (split sex)
  !!log_input(nyrs_fsh_age_c); log_input(nyrs_fsh_age_s);
  init_imatrix yrs_fsh_age_c(1,nfsh,1,nyrs_fsh_age_c)      //Years of fishery index value (annual)
  init_imatrix yrs_fsh_age_s(1,nfsh,1,nyrs_fsh_age_s)      //Years of fishery index value (annual)
  !!log_input(yrs_fsh_age_c);
  !!log_input(yrs_fsh_age_s);
  init_3darray oac_fsh_c(1,nfsh,1,nyrs_fsh_age_c,1,nages)  //Fishery age compositions
  // !!cout <<oac_fsh_c<< endl<<"Finished reading Fishery Age data..."<<endl;
  init_3darray oac_fsh_s(1,nfsh,1,nyrs_fsh_age_s,1,2*nages)  //Fishery age compositions
  !!log_input(oac_fsh_s); 
  init_3darray wt_fsh_in(1,nfsh,styr,endyr,1,2*nages)       //Values of fishery weight at age (g)
  3darray   wt_fsh_b_in(1,nfsh,styr,endyr,1,nages)       //Values of fishery weight at age (g)
  3darray wt_fsh_f_in(1,nfsh,styr,endyr,1,nages)       //Values of fishery weight at age (g)
  3darray wt_fsh_m_in(1,nfsh,styr,endyr,1,nages)       //Values of fishery weight at age (g)
 LOCAL_CALCS
  for (int k=1;k<=nfsh;k++) 
    for (int i=styr;i<=endyr;i++) 
      for (int j=1;j<=nages;j++) 
      {
        wt_fsh_f_in(k,i,j) = wt_fsh_in(k,i,j);
        wt_fsh_m_in(k,i,j) = wt_fsh_in(k,i,nages+j);
      }
  wt_fsh_b_in = (wt_fsh_m_in + wt_fsh_f_in)/2.;
  log_input(wt_fsh_f_in(1,endyr));
  log_input(wt_fsh_m_in(1,endyr));
 END_CALCS

  // NOTE only done for fishery 1, needs to be modified for multiple fisheries...
  matrix wt_fsh_tmp_f(1,nages,styr_wt,endyr_wt)
  matrix wt_fsh_tmp_m(1,nages,styr_wt,endyr_wt)
  matrix wt_pop_tmp_f(1,nages,styr_wt,endyr_wt)
  matrix wt_pop_tmp_m(1,nages,styr_wt,endyr_wt)

  vector wt_fsh_mn_f(1,nages)
  vector wt_fsh_mn_m(1,nages)
  vector wt_pop_mn_f(1,nages)
  vector wt_pop_mn_m(1,nages)

  vector wt_fsh_sigma_f(1,nages)
  vector wt_fsh_sigma_m(1,nages)
  vector wt_pop_sigma_f(1,nages)
  vector wt_pop_sigma_m(1,nages)

  init_matrix nsmpl_fsh_c(1,nfsh,1,nyrs_fsh_age_c)         //Number of age structures read per collection
  init_matrix nsmpl_fsh_s(1,nfsh,1,nyrs_fsh_age_s)         //Number of age structures read per collection
  !!log_input(nsmpl_fsh_c);
  !!log_input(nsmpl_fsh_s);

// Define survey indices
  init_int nsrv                             //Number of surveys
  init_ivector nyrs_srv(1,nsrv)             //Number of years of annual survey index values
  !!log_input(nsrv);
  init_imatrix yrs_srv(1,nsrv,1,nyrs_srv)   //Years of the survey index values
  init_vector mo_srv(1,nsrv)                //Month the survey occurs
  init_matrix obs_srv(1,nsrv,1,nyrs_srv)    //Survey values (biomass or CPUE)
  init_matrix obs_se_srv(1,nsrv,1,nyrs_srv) //Survey standard errors
  matrix     obs_lse_srv(1,nsrv,1,nyrs_srv) //Survey standard errors (for lognormal)
  !!log_input(obs_se_srv);
  !!log_input(obs_srv);
  !! obs_lse_srv = elem_div(obs_se_srv,obs_srv);
  !! obs_lse_srv = sqrt(log(square(obs_lse_srv) + 1.));

  init_ivector nyrs_srv_age_c(1,nsrv)         //Number of survey age compositions
  init_ivector nyrs_srv_age_s(1,nsrv)         //Number of survey age compositions
  init_matrix yrs_srv_age_c(1,nsrv,1,nyrs_srv_age_c)  //Years of survey age compositions
  init_matrix yrs_srv_age_s(1,nsrv,1,nyrs_srv_age_s)  //Years of survey age compositions
  !!log_input( nyrs_srv_age_c);
  !!log_input(  yrs_srv_age_s);
  init_matrix nsmpl_srv_c(1,nsrv,1,nyrs_srv_age_c)  //Number of age structures read per age collection
  init_matrix nsmpl_srv_s(1,nsrv,1,nyrs_srv_age_s)  //Number of age structures read per age collection
  !!log_input(nsmpl_srv_s);
  init_3darray oac_srv_c(1,nsrv,1,nyrs_srv_age_c,1,nages)  //Survey age compositions
  // !! cout <<oac_srv_c<<endl;
  init_3darray oac_srv_s(1,nsrv,1,nyrs_srv_age_s,1,2*nages)  //Survey age compositions

  init_3darray wt_srv_f_in(1,nsrv,styr,endyr,1,nages)           //Survey weight_at_age (g)
  init_3darray wt_srv_m_in(1,nsrv,styr,endyr,1,nages)           //Survey weight_at_age (g)
  matrix          wt_obs_f(styr_wt,endyr_wt,1,nages);
  matrix          wt_obs_m(styr_wt,endyr_wt,1,nages);
  !! for (i=styr_wt;i<=endyr_wt;i++) {wt_obs_f(i) = wt_srv_f_in(1,i); wt_obs_m(i) = wt_srv_m_in(1,i); }

  init_matrix  wt_pop_f_in(styr,endyr,1,nages)                 //Population weight-at-age 
  init_matrix  wt_pop_m_in(styr,endyr,1,nages)                 //Population weight-at-age 
  !!log_input(oac_srv_s);
  
  vector age_vector(1,2*nages);
    !! for (int j=1; j<=nages;j++)
       !!  age_vector(j) = double(j);
    !! for (int j=nages+1; j<=2*nages;j++)
       !!  age_vector(j) = double(j-nages);
//  init_vector wt_pop(1,nages)                 //Population weight-at-age as a vector for yellowfin sole

  init_matrix maturity(styr,endyr,1,nages)               //Maturity-at-age
  !!log_input(maturity);

  init_number init_age_comp          //Flag whether initial age comp independent or not (0 = NOT)

//  Index values

  int styr_rec
  int k         //Index for survey or fishery
  int i         //Index for year
  int j         //Index for age
  int rec_lag


// Survey bottom temperatures for q analysis
  init_matrix bottom_temps(1,nsrv,1,nyrs_srv)
  !!log_input(bottom_temps);

   number    adj_1;
   number    adj_2
   number    SSB_1
   number    SSB_2
 LOCAL_CALCS
  rec_lag=1;    //Lag between year-class and recruitment

  if (phase_init_age_comp>0&&init_age_comp > 0)
  {
    styr_rec            = styr ;  // Change in two places (see below)
    phase_init_age_comp = 2;
  }
  else
  {
    styr_rec            = styr - nages+1; // Extend recruitment devs back in time to estimate init_age from rec_dev
    phase_init_age_comp = -2; // Don't estimate initial age comp parameters
  }
 END_CALCS

//Spawning month
  init_number spawnmo          //Mean spawning month
  number spmo_frac             //Spawning month occurance as a fraction of the year
  !!spmo_frac    =(spawnmo-1)/12.;
  !!log_input(spawnmo);

//Survey month
  init_vector srv_mo(1,nsrv)  //Mean month of survey 
  vector srv_mo_frac(1,nsrv)  //Month survey as a fraction of the year
  !! srv_mo_frac =(srv_mo - 1.) /12.;
  !!log_input(srv_mo);

//Sample size for wts at age 1982-2009, 1-20
  init_matrix n_wts(styr_wt,endyr_wt,1,nages);
  !! log_input(n_wts);
  // !! n_wts/=max(n_wts);
  !! log_input(n_wts);
  init_vector sst_anom(styr_wt,endyr);
  !! log_input(sst_anom);

//Projection indices
  int styr_fut                //Start year for projections
  int endyr_fut               //End year for projections
  int num_proj_Fs             // Number of F's to evaluate in future
  !! num_proj_Fs = 4;

  vector offset_srv(1,nsrv)
  vector offset_fsh(1,nfsh)

	number n_retro;

  number R_guess;
 LOCAL_CALCS                     //Offset calculations for later age comp fitting
  offset_fsh.initialize();
  offset_srv.initialize();
  for (int k=1;k<=nfsh;k++)
  {
    for (i=1;i<=nyrs_fsh_age_c(k);i++)
    {
      oac_fsh_c(k,i) /= sum(oac_fsh_c(k,i));
      offset_fsh(k)  -= nsmpl_fsh_c(k,i)*(oac_fsh_c(k,i) + 0.001) * log(oac_fsh_c(k,i) + 0.001); 
    }
    for (i=1;i<=nyrs_fsh_age_s(k);i++)
    {
      oac_fsh_s(k,i) /= sum(oac_fsh_s(k,i));
      offset_fsh(k)  -= nsmpl_fsh_s(k,i)*(oac_fsh_s(k,i) + 0.001) * log(oac_fsh_s(k,i) + 0.001); 
    }
  }

  for (int k=1;k<=nsrv;k++)
  {
    for (i=1;i<=nyrs_srv_age_c(k);i++)
    {
      oac_srv_c(k,i) /= sum(oac_srv_c(k,i));
      offset_srv(k)-= nsmpl_srv_c(k,i)*(oac_srv_c(k,i) + 0.001) * log(oac_srv_c(k,i) + 0.001);  
    }
    for (int i=1;i<=nyrs_srv_age_s(k);i++)
    {
      oac_srv_s(k,i) /= sum(oac_srv_s(k,i));
      offset_srv(k)-= nsmpl_srv_s(k,i)*(oac_srv_s(k,i) + 0.001) * log(oac_srv_s(k,i) + 0.001); 
    }
  }
  log_input(offset_fsh);
  log_input(offset_srv);


// Compute an initial Rzero value based on exploitation 
   double btmp=0.;
   double ctmp=0.;
   dvector ntmp(1,nages);
   ntmp(1) = 1.;
   for (int a=2;a<=nages;a++)
     ntmp(a) = ntmp(a-1)*exp(-m_exp-.05);
   btmp = wt_pop_f_in(endyr)(1,nages) * ntmp;
   ctmp = mean(catch_bio);
   log_input( ctmp );
   R_guess = log((ctmp/.02 )/btmp) ;
   log_input( R_guess );
	 // Now that everything is read in, can change the "endyr" to reflect retrospective year
	 n_retro = 3;
	 endyr = endyr - n_retro;
	 // Lag between weights and years
	 if (endyr <= endyr_wt) endyr_wt = endyr - (endyr_in-endyr_wt );
	 if (endyr <= endyr_wt) endyr_wt = endyr - (endyr_in-endyr_wt );
	 endyr_sr = endyr - (endyr_in-endyr_sr );
   styr_fut=endyr+1;
   endyr_fut=styr_fut+10;
 END_CALCS

PARAMETER_SECTION

  init_bounded_vector q_srv(1,nsrv,.01,3,phase_q)
  init_bounded_number natmort_f(.02,.30,phase_m_f)
  init_bounded_number natmort_m(.02,.30,phase_m_m)
  init_bounded_number Linf_f(25.,50.,phase_grwth)
  init_bounded_number    K_f(.05,1.0,phase_grwth)
  init_bounded_number   t0_f(-5.,5.0,phase_grwth)
  init_bounded_number Linf_m(25.,50.,phase_grwth)
  init_bounded_number    K_m(.05,1.0,phase_grwth)
  init_bounded_number   t0_m(-5.,5.0,phase_grwth)

  // Baseline incremental growth (to be computed from vbg)
  vector base_incr_f(2,nages);
  vector base_incr_m(2,nages);
  // Incremental growth matrix as deviations from baseline  
  matrix incr_dev(styr+1,endyr,2,nages)
  // Age-effect (added growth curve goof)
  init_bounded_dev_vector age_incr(5,nages-5,-10,10,phase_age_incr)
  // Year-effect (not related to temperature...)
  init_bounded_dev_vector yr_incr(styr+1,endyr,-10,10,phase_wt)
  // Temperature coeficient (multiplier to temperature time series)
  init_number sst_alpha(phase_sst)

  //   linear model paramters to fit temperature to survey catchability
  init_number alpha(phase_alpha)
  init_number beta(phase_beta)

  //Recruitment parameters
  init_number mean_log_rec(phase_mn_rec);
  init_bounded_vector rec_dev(styr_rec,endyr,-15,15,phase_rec_dev)

  // Added to make initial age comp inconsistent with recruitment estimates
  init_number mean_log_init(phase_init_age_comp); 
  init_bounded_vector init_dev_m(2,nages,-15,15,phase_init_age_comp)
  init_bounded_vector init_dev_f(2,nages,-15,15,phase_init_age_comp)
  number sigmarsq
  number avg_rec_dev
  number sigma_rec
  number var_rec

  //Fishing mortality parameters
  init_vector log_avg_fmort(1,nfsh,phase_mn_f)
  init_bounded_matrix fmort_dev(1,nfsh,styr,endyr,-15,15,phase_fmort)
  vector Fmort(styr,endyr);    //Annual total fmort
  number hrate
  number kobs_tot_catch
  number Fnew

  //Selectivity parameters

  // time invariant selectivity parameters for fishery
  //  init_vector sel_slope_fsh(1,nfsh,phase_logist_sel)
  //  init_vector sel50_fsh(1,nfsh,phase_logist_sel)
  //  init_vector sel_slope_fsh_m(1,nfsh,phase_male_sel)
  //  init_vector     sel50_fsh_m(1,nfsh,phase_male_sel)

  //  time-varying selectivity

  // fishery females  
  init_vector sel_slope_fsh_f(1,nfsh,phase_logist_sel)
  init_vector sel50_fsh_f(1,nfsh,phase_logist_sel)
  init_bounded_matrix sel_slope_fsh_devs_f(1,nfsh,styr,endyr,-10,10,phase_logist_sel)
  init_bounded_matrix sel50_fsh_devs_f(1,nfsh,styr,endyr,-10,10,phase_logist_sel)
  number slope_tmp
  number sel50_tmp
  // fishery males

  init_vector sel_slope_fsh_m(1,nfsh,phase_logist_sel)
  init_bounded_matrix sel_slope_fsh_devs_m(1,nfsh,styr,endyr,-10,10,phase_logist_sel)
  init_vector sel50_fsh_m(1,nfsh,phase_logist_sel)
  init_bounded_matrix sel50_fsh_devs_m(1,nfsh,styr,endyr,-10,10,phase_logist_sel)

  // survey selectivity (time invariant)

  init_vector sel_slope_srv(1,nsrv,phase_logist_sel)
  init_vector sel50_srv(1,nsrv,phase_logist_sel)
  init_vector sel_slope_srv_m(1,nsrv,phase_male_sel)
  init_vector     sel50_srv_m(1,nsrv,phase_male_sel)

  //SPR computation parameters
  init_bounded_vector F40(1,nfsh,0.01,10.,phase_F40)
  init_bounded_vector F35(1,nfsh,0.01,10.,phase_F40)
  init_bounded_vector F30(1,nfsh,0.01,10.,phase_F40)

  // Spawner-recruit parameters and associated definitions
  init_number R_logalpha(phase_sr)      // log of Ricker alpha
  init_number R_logbeta(phase_sr)      // log of Ricker beta
  vector SRC_recruits(styr_sr,endyr_sr)     // predicted recruits
  vector SAM_recruits(styr_sr,endyr_sr)     // model estimate of recruitment (from nage matrix)
  init_number sigma_R(phase_sigmar)                   // variability of R for objective function distributional assumption
  number R_alpha                            // Ricker alpha

  // Weights-at-age definitions (formerly in Data section)
  3darray   wt_fsh(1,nfsh,styr,endyr,1,nages)       //Values of fishery weight at age (g)
  3darray wt_fsh_f(1,nfsh,styr,endyr,1,nages)       //Values of fishery weight at age (g)
  3darray wt_fsh_m(1,nfsh,styr,endyr,1,nages)       //Values of fishery weight at age (g)
  3darray wt_srv_f(1,nsrv,styr,endyr,1,nages)         //Survey weight_at_age (g)
  3darray wt_srv_m(1,nsrv,styr,endyr,1,nages)         //Survey weight_at_age (g)
  matrix  wt_pop_f(styr,endyr,1,nages)                //Population weight-at-age 
  matrix  wt_pop_m(styr,endyr,1,nages)                //Population weight-at-age 
  vector wt_vbg_f(1,nages);
  vector wt_vbg_m(1,nages);
  matrix wt_pred_f(styr_wt,endyr_wt,1,nages);
  matrix wt_pred_m(styr_wt,endyr_wt,1,nages);
  // Future weights-at-age definitions
  init_vector wt_fsh_fut_f(1,nages,phase_wtfmsy)
  init_vector wt_fsh_fut_m(1,nages,phase_wtfmsy)
  init_vector wt_pop_fut_f(1,nages,phase_wtfmsy)
  init_vector wt_pop_fut_m(1,nages,phase_wtfmsy)
  number R_beta                             // Ricker beta 
  number phizero                            // SPR at F=0

  //Defining more matrices and vectors
  3darray log_sel_fsh_f(1,nfsh,styr,endyr,1,nages)
  3darray log_sel_fsh_m(1,nfsh,styr,endyr,1,nages)
  3darray sel_fsh_f(1,nfsh,styr,endyr,1,nages)
  3darray sel_fsh_m(1,nfsh,styr,endyr,1,nages)

  matrix log_sel_srv_f(1,nsrv,1,nages)
  matrix log_sel_srv_m(1,nsrv,1,nages)

  init_vector log_msy_sel_f(1,nages,phase_proj) // new partial fishing mortality (==selectivity) for use in Fmsy calcs
  init_vector log_msy_sel_m(1,nages,phase_proj) // new partial fishing mortality (==selectivity) for use in Fmsy calcs
  vector      partial_F_f(1,nages)      // new partial fishing mortality (==selectivity) for use in Fmsy calcs
  vector      partial_F_m(1,nages)      // new partial fishing mortality (==selectivity) for use in Fmsy calcs
  matrix sel_srv_f(1,nsrv,1,nages)
  matrix sel_srv_m(1,nsrv,1,nages)
  matrix pred_srv(1,nsrv,styr,endyr)

  3darray eac_fsh_c(1,nfsh,1,nyrs_fsh_age_c,1,nages)
  3darray eac_srv_c(1,nsrv,1,nyrs_srv_age_c,1,nages)

  3darray eac_fsh_s(1,nfsh,1,nyrs_fsh_age_s,1,2*nages)
  3darray eac_srv_s(1,nsrv,1,nyrs_srv_age_s,1,2*nages)

  matrix pred_catch(1,nfsh,styr,endyr)

  matrix natage_f(styr,endyr,1,nages)
  matrix natage_m(styr,endyr,1,nages)

  3darray catage_f(1,nfsh,styr,endyr,1,nages)
  3darray catage_m(1,nfsh,styr,endyr,1,nages)

  matrix Z_m(styr,endyr,1,nages)
  matrix Z_f(styr,endyr,1,nages)
  3darray F_m(1,nfsh,styr,endyr,1,nages)
  3darray F_f(1,nfsh,styr,endyr,1,nages)
  matrix expl_biom(1,nfsh,styr,endyr)
  matrix S_m(styr,endyr,1,nages)
  matrix S_f(styr,endyr,1,nages)
  number surv_f
  number surv_m

  sdreport_vector SSB(styr,endyr)
  vector pred_rec(styr,endyr)
  //Likelihood value names
  number sigma
  vector rec_like(1,4)
  number catch_like
  vector q_like(1,nsrv)
  number m_like
  vector age_like_fsh(1,nfsh)
  vector age_like_srv(1,nsrv)
  vector wt_like(1,3)
  vector sel_like(1,3)
  number fpen
  objective_function_value obj_fun
  vector srv_like(1,nsrv)

  //Definitions for SPR and future yield projections
  number SPR_ABC // (0.05,2.,phase_F40)
  number SPR_OFL // (0.05,2.,phase_F40)
  number sigmar_fut
  vector ftmp(1,nfsh)
  number SB0
  number SBF40
  number SBF35
  number SBF30
  number sprpen
  vector Fratio(1,nfsh)

  matrix Nspr(1,4,1,nages)

  matrix nage_future_f(styr_fut,endyr_fut,1,nages)
  matrix nage_future_m(styr_fut,endyr_fut,1,nages)
  init_vector rec_dev_future(styr_fut,endyr_fut,phase_proj)
  vector SSB_future(styr_fut,endyr_fut)
  vector TotBiom_future(styr_fut,endyr_fut)

  3darray F_future_f(1,nfsh,styr_fut,endyr_fut,1,nages)
  3darray F_future_m(1,nfsh,styr_fut,endyr_fut,1,nages)
  matrix Z_future_f(styr_fut,endyr_fut,1,nages)
  matrix Z_future_m(styr_fut,endyr_fut,1,nages)
  matrix S_future_f(styr_fut,endyr_fut,1,nages)
  matrix S_future_m(styr_fut,endyr_fut,1,nages)

  matrix catage_future_f(styr_fut,endyr_fut,1,nages)
  matrix catage_future_m(styr_fut,endyr_fut,1,nages)
  number avg_rec_dev_future
  vector avg_F_future(1,5)
 
  // matrix nage_future(styr_fut,endyr_fut,1,nages)
  // init_vector rec_dev_future(styr_fut,endyr_fut,phase_proj);
  // vector Sp_Biom_future(styr_fut-rec_age,endyr_fut);

  // 3darray F_future(1,nfsh,styr_fut,endyr_fut,1,nages);
  // matrix Z_future(styr_fut,endyr_fut,1,nages);
  // matrix S_future(styr_fut,endyr_fut,1,nages);
  // matrix catage_future(styr_fut,endyr_fut,1,nages);
  // number avg_rec_dev_future
  // vector avg_F_future(1,5)


  sdreport_vector C_age(1,nages)
  sdreport_number Fmsy
  sdreport_number logFmsy
  sdreport_number Fmsyr
  sdreport_number logFmsyr

  sdreport_number ABC_biom1;
  sdreport_number ABC_biom2;
  sdreport_number msy
  sdreport_number Bmsy
  sdreport_number Bmsyr
  sdreport_vector TotBiom(styr,endyr)
  sdreport_number endbiom
  sdreport_number depletion
  sdreport_matrix catch_future(1,num_proj_Fs-1,styr_fut,endyr_fut)
  sdreport_matrix future_SSB(1,num_proj_Fs,styr_fut,endyr_fut)
  sdreport_matrix future_TotBiom(1,num_proj_Fs,styr_fut,endyr_fut)

PRELIMINARY_CALCS_SECTION
  // Initialize matrices used in the model
  // exit(1);
  for (i=styr_wt;i<=endyr_wt;i++) 
  {
    wt_fsh_f(i) = wt_fsh_f_in(i);
    wt_fsh_m(i) = wt_fsh_m_in(i);
    wt_srv_f(i) = wt_srv_f_in(i);
    wt_srv_m(i) = wt_srv_m_in(i);
    wt_pop_f(i) = wt_pop_f_in(i);
    wt_pop_m(i) = wt_pop_m_in(i);
    // cout<<"wt fsh"<<endl; cout<<i<<" "<<wt_fsh_f(1,i)(5,9)<<endl;
    // cout <<wt_fsh_f(1,endyr)<<endl<<wt_fsh_m(1,endyr)<< endl<<"Finished reading Fishery wt_Age data..."<<endl;

    for (j=1;j<=nages;j++) 
    {
      //note, only uses fishery 1 for this... needs to be updated for more than 1 fishery
      wt_fsh_tmp_f(j,i) = wt_fsh_f_in(1,i,j);
      wt_fsh_tmp_m(j,i) = wt_fsh_m_in(1,i,j);

      wt_pop_tmp_f(j,i) = value(wt_pop_f(i,j));//note, only uses fishery 1 for this... needs to be updated for more than 1 fishery
      wt_pop_tmp_m(j,i) = value(wt_pop_m(i,j));//note, only uses fishery 1 for this... needs to be updated for more than 1 fishery
    }
  }
  // cout<<"tmp wt fsh"<<endl;
  // cout<<wt_fsh_tmp_f<<endl;
  for (j=1;j<=nages;j++)
  {
    wt_fsh_mn_f(j) = mean(wt_fsh_tmp_f(j));
    wt_fsh_mn_m(j) = mean(wt_fsh_tmp_m(j));
    wt_pop_mn_f(j) = mean(wt_pop_tmp_f(j));
    wt_pop_mn_m(j) = mean(wt_pop_tmp_m(j));
    wt_fsh_sigma_f(j) = sqrt(norm2(wt_fsh_tmp_f(j)-wt_fsh_mn_f(j))/(endyr_wt-styr_wt+1) );
    wt_fsh_sigma_m(j) = sqrt(norm2(wt_fsh_tmp_m(j)-wt_fsh_mn_m(j))/(endyr_wt-styr_wt+1) );
    wt_pop_sigma_f(j) = sqrt(norm2(wt_pop_tmp_f(j)-wt_pop_mn_f(j))/(endyr_wt-styr_wt+1) );
    wt_pop_sigma_m(j) = sqrt(norm2(wt_pop_tmp_m(j)-wt_pop_mn_m(j))/(endyr_wt-styr_wt+1) );

    //check for zero values or no variance 
    if (wt_pop_mn_f(j)==0.)
    {
      wt_pop_mn_f(j)=.03;
      wt_pop_mn_m(j)=.03;
    }
    if (wt_fsh_mn_m(j)==0.)
    {
      wt_fsh_mn_f(j)=.03;
      wt_fsh_mn_m(j)=.03;
    }
    if (wt_pop_sigma_f(j)<0.03)
    {
      wt_pop_sigma_f(j)=.03*wt_pop_mn_f(j);
      wt_pop_sigma_m(j)=.03*wt_pop_mn_m(j);
    }
    if (wt_fsh_sigma_f(j)<0.03)
    {
      wt_fsh_sigma_f(j)=.03*wt_fsh_mn_f(j);
      wt_fsh_sigma_m(j)=.03*wt_fsh_mn_m(j);
    }
  }

  adj_1=1.0;
  adj_2=1.0; 
  SSB_1=1.0;
  SSB_2=1.0; 
  if(phase_wtfmsy>0)
  {
    wt_fsh_fut_f= wt_fsh_mn_f; // initializes estimates to correct values...
    wt_fsh_fut_m= wt_fsh_mn_m; // initializes estimates to correct values...
    wt_pop_fut_f= wt_pop_mn_f; // initializes estimates to correct values...
    wt_pop_fut_m= wt_pop_mn_m; // initializes estimates to correct values...
  }
  else
  {
    wt_fsh_fut_f = wt_fsh_f_in(1,endyr) ; // initializes estimates to correct values...
    wt_fsh_fut_m = wt_fsh_m_in(1,endyr) ;
    wt_pop_fut_f = wt_pop_f(endyr)   ; // initializes estimates to correct values...
    wt_pop_fut_m = wt_pop_m(endyr)   ; // initializes estimates to correct values...
  }
  for (int j=1;j<=nages;j++)
  {
      wt_vbg_f(j) = value(a_lw_f * pow(Linf_f*(1.-exp(-K_f*(double(j)-t0_f))),b_lw_f));
      wt_vbg_m(j) = value(a_lw_m * pow(Linf_m*(1.-exp(-K_m*(double(j)-t0_m))),b_lw_m));
  }
  base_incr_f(2,nages) = wt_vbg_f(2,nages) - ++wt_vbg_f(1,nages-1);
  base_incr_m(2,nages) = wt_vbg_m(2,nages) - ++wt_vbg_m(1,nages-1);
  // cout <<base_incr_f<<endl<<base_incr_m<<endl;exit(1);
  switch (Growth_Option)
  {
    case 0 : // Use constant time-invariant growth (from base growth parameters)
    {
      for (k=1;k<=nfsh;k++)
        for (i=styr;i<=endyr;i++)
        {
          wt_fsh_f(k,i) = wt_vbg_f;
          wt_fsh_m(k,i) = wt_vbg_m;
        }
      for (k=1;k<=nsrv;k++)
        for (i=styr;i<=endyr;i++)
        {
          wt_srv_f(k,i) = wt_vbg_f;
          wt_srv_m(k,i) = wt_vbg_m;
        }
        for (i=styr;i<=endyr;i++)
        {
          wt_pop_f(i) = wt_vbg_f;
          wt_pop_m(i) = wt_vbg_m;
        }
    }
    // cout<<wt_srv_f<<endl;exit(1);
    break;
    case 1 : // Use empirical (values in data file) mean wts at age
    {
      // No need to do anything since they are initialized at these values...
    }
    break;
    case 2 : // start out with base growth values and deviations decomposed by year and age
    {
      for (k=1;k<=nfsh;k++)
        for (i=styr;i<=endyr;i++)
        {
          wt_fsh_f(k,i) = wt_vbg_f;
          wt_fsh_m(k,i) = wt_vbg_m;
        }
      for (k=1;k<=nsrv;k++)
        for (i=styr;i<=endyr;i++)
        {
          wt_srv_f(k,i) = wt_vbg_f;
          wt_srv_m(k,i) = wt_vbg_m;
        }
        for (i=styr;i<=endyr;i++)
        {
          wt_pop_f(i) = wt_vbg_f;
          wt_pop_m(i) = wt_vbg_m;
        }
    }
    break;
    case 3 : // Use base growth values (not estimated) and deviations as fn of temperature and
             // decomposed by year and age
    {
    }
    break;
  }
  log_input(wt_fsh_f);
  log_input(wt_fsh_m);
  log_input(wt_srv_f);
  log_input(wt_srv_m);
  log_input(wt_pop_f);
  log_input(wt_pop_m);


INITIALIZATION_SECTION
  // yellowfin sole males  n=656 name value std dev
  Linf_m 34.030 // 0.149
  K_m 0.161     // 0.004
  t0_m 0.515    // 0.096

  // yellowfin sole females  n=709 
  Linf_f 38.034 // 0.152
  K_f 0.137     // 0.003
  t0_f 0.297    // 0.009

  wt_fsh_fut_f .8;
  wt_fsh_fut_m .8;
  wt_pop_fut_f .8;
  wt_pop_fut_m .8;
  alpha alpha_prior ;
  beta beta_prior ;
  R_logalpha    -4.18844741303
  R_logbeta     -6.15913355283
  sigma_R  sigmar_exp
  natmort_f  m_exp
  natmort_m  m_exp
  mean_log_rec 0.8
  mean_log_init -.8
  log_avg_fmort -2.
  q_srv  q_exp
  sel_slope_fsh_f  .8
  sel_slope_fsh_m  .8
  sel_slope_srv  .8
  sel50_fsh_f      5.
  sel50_fsh_m      5.
  sel50_srv      3.
  F40 .11
  F35 .14
  F30 .16
  sst_alpha .0
  
PROCEDURE_SECTION
  // if (Growth_Option>1&&(current_phase()<=1))
  if (Growth_Option>1 && (last_phase()||current_phase()<=phase_wt))
    Get_wt_age();
  if(!do_wt_only)
  {
    get_selectivity();
    get_mortality();
    get_numbers_at_age();
    partial_F_f = mfexp(log_msy_sel_f);
    partial_F_m = mfexp(log_msy_sel_m);
  
    if(active(R_logalpha))
     compute_sr_fit();
  
    if (active(sst_alpha))
    {
      dvar_vector incr_dev_tmp(2,nages);
      // two degree increase in temperature
      incr_dev_tmp(5,nages-5)     = sst_alpha * 2.0 + age_incr;
      incr_dev_tmp(nages-4,nages) = incr_dev_tmp(nages-5) ;
      incr_dev_tmp(2,4)           = incr_dev_tmp(5) ;

      wt_pop_fut_f(1)        = wt_vbg_f(1);
      wt_pop_fut_m(1)        = wt_vbg_m(1);
      wt_pop_fut_f(2,nages)  = ++wt_vbg_f(1,nages-1) + elem_prod(base_incr_f,mfexp(incr_dev_tmp)) ; // initializes estimates to correct values...
      wt_pop_fut_m(2,nages)  = ++wt_vbg_m(1,nages-1) + elem_prod(base_incr_m,mfexp(incr_dev_tmp)) ; // initializes estimates to correct values...
    }
    if (active(F40))
      compute_spr_rates();

    if (last_phase())
      Future_projections();
    if (sd_phase() || mceval_phase())
    {
      get_msy();
    }
    catch_at_age();
  }
  evaluate_the_objective_function();

  // next part returns the Byesian posterior for the listed parameters
  if (mceval_phase())
  {
    // if (oper_mod)
      // Oper_Model();
    // else
    {
     // compute_spr_rates();
      // write_mceval();
    }
    evalout <<obj_fun<<" "<<q_srv(1)<<" "<<natmort_f<<" "<< Fmsyr <<" "<<ABC_biom1<<" "<<ABC_biom2<<" "<<Bmsy<<" "<<msy<<" "<< TotBiom 
                     << " "<<SSB <<" "<< mean_log_rec <<" "<<partial_F_f(7,12)<<
                        " "<<sel_slope_srv<<" "<<sel50_srv<<
                        " "<<sel_slope_srv_m<<" "<<sel50_srv_m<<
                        endl;
  } 

FUNCTION get_selectivity
  //Logistic selectivity is modeled for the fishery and survey, by age
  if (active(sel_slope_fsh_f))
  {
  //   time invariant selectivy calculations
  //  for (k=1;k<=nfsh;k++)
  //  {
  //    log_sel_fsh_f(k)(1,nselages) = -log( 1.0 + mfexp(-sel_slope_fsh(k) * (age_vector(1,nselages) - sel50_fsh(k)) ));
  //    log_sel_fsh_f(k)(nselages,nages) = log_sel_fsh_f(k,nselages);
  //
  //    dvariable slope_tmp = sel_slope_fsh(k) * mfexp(sel_slope_fsh_m(k));
  //    dvariable sel50_tmp = sel50_fsh(k)     * mfexp(sel50_fsh_m(k));
  //   log_sel_fsh_m(k)(1,nselages) = -log( 1.0 + mfexp(-slope_tmp * (age_vector(1,nselages) - sel50_tmp) ));
  //    log_sel_fsh_m(k)(nselages,nages) = log_sel_fsh_m(k,nselages);
  //  }
  //  time-varying selectivy calculations, by sex for the fishery, females first
    for (k=1;k<=nfsh;k++)
    {
      for (i=styr;i<=endyr;i++)
      {
        slope_tmp = sel_slope_fsh_f(k)*mfexp(sel_slope_fsh_devs_f(k,i));
        sel50_tmp = sel50_fsh_f(k)*mfexp(sel50_fsh_devs_f(k,i));
        for (j=1;j<=nselages;j++)
        {
          log_sel_fsh_f(k,i,j) = -log(1.0 + mfexp(-slope_tmp*((j)-sel50_tmp)));
        }
     //  males
        slope_tmp = sel_slope_fsh_m(k)*mfexp(sel_slope_fsh_devs_m(k,i));
        sel50_tmp = sel50_fsh_m(k)*mfexp(sel50_fsh_devs_m(k,i));
        for (j=1;j<=nselages;j++)
        {
          log_sel_fsh_m(k,i,j) = -log(1.0+mfexp(-slope_tmp*((j)-sel50_tmp)));
        }
        for (j=nselages+1;j<=nages;j++)
        {
          log_sel_fsh_f(k,i,j)=log_sel_fsh_f(k,i,j-1);
          log_sel_fsh_m(k,i,j)=log_sel_fsh_m(k,i,j-1);
        }
      }
    }

    for (k=1;k<=nsrv;k++)
    {
      log_sel_srv_f(k)(1,nselages) = -log( 1.0 + mfexp(-1.*sel_slope_srv(k) * (age_vector(1,nselages) - sel50_srv(k)) ));
      log_sel_srv_f(k)(nselages,nages) = log_sel_srv_f(k,nselages);

      dvariable slope_tmp = sel_slope_srv(k) * mfexp(sel_slope_srv_m(k));
      dvariable sel50_tmp = sel50_srv(k)     * mfexp(sel50_srv_m(k));
      log_sel_srv_m(k)(1,nselages) = -log( 1.0 + mfexp(-slope_tmp * (age_vector(1,nselages) - sel50_tmp) ));
      log_sel_srv_m(k)(nselages,nages) = log_sel_srv_m(k,nselages);
    }
  }
  sel_srv_f=mfexp(log_sel_srv_f);
  sel_srv_m=mfexp(log_sel_srv_m);
  sel_fsh_f = mfexp(log_sel_fsh_f);
  sel_fsh_m = mfexp(log_sel_fsh_m);

FUNCTION get_mortality
  Z_f.initialize();
  Z_m.initialize();
  F_f.initialize();
  F_m.initialize();
  S_f.initialize();
  S_m.initialize();
  surv_f.initialize();
  surv_m.initialize();
  Fmort.initialize();
  surv_f    = mfexp(-natmort_f);
  surv_m    = mfexp(-natmort_m);
  Z_f = natmort_f;
  Z_m = natmort_m;
  Fmort = 0.;
  for (k=1;k<=nfsh;k++)
  {
    Fmort  += mfexp(log_avg_fmort(k) + fmort_dev(k));
    for (i=styr;i<=endyr;i++)
    {
      F_f(k,i)  = (mfexp(log_avg_fmort(k) + fmort_dev(k,i))* sel_fsh_f(k,i)) ;
      F_m(k,i)  = (mfexp(log_avg_fmort(k) + fmort_dev(k,i))* sel_fsh_m(k,i)) ;
    }
    Z_f += F_f(k);
    Z_m += F_m(k);
  }
  S_f = mfexp(-1.*Z_f);
  S_m = mfexp(-1.*Z_m);

FUNCTION get_numbers_at_age
  //Initial age composition
  // Initial age comp independent from recruitment...
  if (styr_rec == styr)
  {
    for (j=2;j<=nages;j++)
    {
      natage_m(styr,j) = .5*mfexp(mean_log_init + init_dev_m(j) );
      natage_f(styr,j) = .5*mfexp(mean_log_init + init_dev_f(j) );
    }
  }
  else  // Initial age comp consistent with estimated recruitment levels
  {
    int itmp;
    for (j=1;j<nages;j++)
    {
      itmp=styr+1-j;
      natage_f(styr,j) = 0.5*mfexp(mean_log_rec-natmort_f*double(j-1)+rec_dev(itmp));
      natage_m(styr,j) = 0.5*mfexp(mean_log_rec-natmort_m*double(j-1)+rec_dev(itmp));
    }
    //Plus group in the first year
    itmp=styr+1-nages;
    natage_f(styr,nages) = 0.5*mfexp(mean_log_rec+rec_dev(itmp) - natmort_f*(nages-1))/(1.-surv_f);
    natage_m(styr,nages) = 0.5*mfexp(mean_log_rec+rec_dev(itmp) - natmort_m*(nages-1))/(1.-surv_m);
  }

  //Recruitment in subsequent years
  for (i=styr;i<=endyr;i++)
  {
    natage_m(i,1) = 0.5*mfexp(mean_log_rec+rec_dev(i));
    natage_f(i,1) = natage_m(i,1);
  }

  pred_rec(styr) = 2.*natage_m(styr,1);

  //Fill in the rest of the matrix of numbers at age by applying F's
  for (i=styr;i<endyr;i++)
  {
    natage_m(i+1)(2,nages)= ++elem_prod(natage_m(i)(1,nages-1),S_m(i)(1,nages-1));
    natage_m(i+1,nages)  +=  natage_m(i,nages)*S_m(i,nages);
    natage_f(i+1)(2,nages)= ++elem_prod(natage_f(i)(1,nages-1),S_f(i)(1,nages-1));
    natage_f(i+1,nages)  +=  natage_f(i,nages)*S_f(i,nages);

    TotBiom(i)=natage_f(i)*wt_pop_f(i) + natage_m(i)*wt_pop_m(i);

    SSB(i)  =  elem_prod(natage_f(i),pow(S_f(i),spmo_frac)) * elem_prod(wt_pop_f(i),maturity(i));  //need to add recruitment lag
		if (SSB(i)<0) cout <<i<<" "<<wt_pop_f(i)(1,5)<<" "<< maturity(i)(4,7) <<" "<< spmo_frac << endl;

    // SSB_1         = value(elem_prod(elem_prod(Ntmp,pow(Stmp,spmo_frac)),maturity)*wt_pop_fut);
    pred_rec(i+1) = 2.*natage_f(i+1,1);
  }
 //  SSB(endyr) = (natage(endyr)/2) * elem_prod(wt_pop(endyr),maturity);
  SSB(endyr)  =  elem_prod(natage_f(endyr),pow(S_f(endyr),spmo_frac)) * elem_prod(wt_pop_f(endyr),maturity(endyr));  //need to add recruitment lag
  TotBiom(endyr)   =  natage_f(endyr)*wt_pop_f(endyr) + natage_m(endyr)*wt_pop_m(endyr);
  if (sd_phase())
  {
    depletion = TotBiom(endyr)/TotBiom(styr);
    endbiom=TotBiom(endyr);
  }

  //Survey computations
  for (k=1;k<=nsrv;k++)
  {
    for (i=1;i<=nyrs_srv(k);i++)
    {
      int srvyrtmp = yrs_srv(k,i);
      dvariable b1tmp = elem_prod(natage_f(srvyrtmp),exp( -Z_f(srvyrtmp) * srv_mo_frac(k) )) * elem_prod(sel_srv_f(k),wt_srv_f(k,srvyrtmp));
      b1tmp          += elem_prod(natage_m(srvyrtmp),exp( -Z_m(srvyrtmp) * srv_mo_frac(k) )) * elem_prod(sel_srv_m(k),wt_srv_m(k,srvyrtmp));

      if (phase_bottom_temps>=1)
      {
     // pred_srv(k,yrs_srv(k,i)) =       (alpha+beta*bottom_temps(k,i)) * elem_prod(natage(yrs_srv(k,i)),exp( -Z(yrs_srv(k,i)) * srv_mo_frac(k) )) * elem_prod(sel_srv(k),wt_srv(k,yrs_srv(k,i)));
        pred_srv(k,srvyrtmp)     = mfexp(-alpha+beta*bottom_temps(k,i)) * b1tmp;
      }
      else
      {
        pred_srv(k,srvyrtmp)     = q_srv(k) * b1tmp;
      }
    }
    for (i=1;i<=nyrs_srv_age_c(k);i++)
    {
      int yrtmp = yrs_srv_age_c(k,i); 
      eac_srv_c(k,i) = elem_prod(sel_srv_f(k),natage_f(yrtmp)) + elem_prod(sel_srv_m(k),natage_m(yrtmp)) ; 
      eac_srv_c(k,i)/= sum(eac_srv_c(k,i));
    }
    for (i=1;i<=nyrs_srv_age_s(k);i++)
    {
      int yrtmp = yrs_srv_age_s(k,i); 
      eac_srv_s(k,i)(1,nages) = elem_prod(sel_srv_f(k),natage_f(yrtmp));
      // eac_srv_s(k,i)(nages+1,2*nages)  = elem_prod(sel_srv(k),natage_m(yrtmp)).shift(nages+1);
      for (j=1;j<=nages;j++)
        eac_srv_s(k,i,j+nages) = sel_srv_m(k,j) * natage_m(yrtmp,j);

      eac_srv_s(k,i)/= sum(eac_srv_s(k,i));
     }
   }

FUNCTION catch_at_age
  catage_f.initialize();
  catage_m.initialize();
  pred_catch.initialize();
  for (k=1;k<=nfsh;k++)
  {
    for (i=styr;i<=endyr;i++)
    {
      catage_f(k,i) = elem_prod(elem_div(F_f(k,i),Z_f(i)),elem_prod(1. - S_f(i),natage_f(i)));
      catage_m(k,i) = elem_prod(elem_div(F_m(k,i),Z_m(i)),elem_prod(1. - S_m(i),natage_m(i)));
      pred_catch(k,i) = catage_f(k,i)*wt_fsh_f(k,i) ;
      pred_catch(k,i)+= catage_m(k,i)*wt_fsh_m(k,i);
    }
    if (sd_phase()) 
      C_age = catage_f(1,endyr-2);
 
    for (i=1;i<=nyrs_fsh_age_c(k);i++)
    {
      int yrtmp = yrs_fsh_age_c(k,i);
      eac_fsh_c(k,i) = catage_f(k,yrtmp) + catage_m(k,yrtmp);
      eac_fsh_c(k,i)/= sum(eac_fsh_c(k,i));
    }
    for (i=1;i<=nyrs_fsh_age_s(k);i++)
    {
      int yrtmp = yrs_fsh_age_s(k,i);
      eac_fsh_s(k,i)(1,nages) = catage_f(k,yrtmp);
      // eac_fsh_s(k,i)(nages+1,2*nages) = catage_f(k,yrtmp).shift(nages+1);
      for (j=1;j<=nages;j++)
        eac_fsh_s(k,i,j+nages) = catage_m(k,yrtmp,j);
      eac_fsh_s(k,i) /= sum(eac_fsh_s(k,i));
    }
  }

FUNCTION evaluate_the_objective_function
  fpen.initialize();
  wt_like.initialize();
  q_like.initialize();
  if(active(yr_incr)||active(age_incr)||active(sst_alpha))
  {
    // cout <<wt_pred_f(1988)(3,7)<<endl;
    for (i=styr_wt;i<=endyr_wt;i++)
    {
      for (j=3;j<nages;j++)
      {
         // matrices for the period of wt-age data (modify to use sample size) divided by 2 to account for males and females...
         wt_like(1) += 0.5*log(.1+ .5*n_wts(i,j)*square(log(wt_pred_f(i,j) + .1) - log(wt_obs_f(i,j) + .1)) ); 
         wt_like(1) += 0.5*log(.1+ .5*n_wts(i,j)*square(log(wt_pred_m(i,j) + .1) - log(wt_obs_m(i,j) + .1)) ); // matrices for the period of wt-age data (modify to use sample size)
         // wt_like(1) += 50.*(n_wts(i,j)*square(log(wt_pred_f(i,j) + .1) - log(wt_obs_f(i,j) + .1)) ); // matrices for the period of wt-age data (modify to use sample size)
         // wt_like(1) += 50.*(n_wts(i,j)*square(log(wt_pred_m(i,j) + .1) - log(wt_obs_m(i,j) + .1)) ); // matrices for the period of wt-age data (modify to use sample size)
      }
    }
    // wt_like(1) += 50.*norm2(log(wt_pred_f + .01) - log(wt_obs_f + .01)); // matrices for the period of wt-age data (modify to use sample size)
    // wt_like(1) += 50.*norm2(log(wt_pred_m + .01) - log(wt_obs_m + .01)); // matrices for the period of wt-age data (modify to use sample size)

    // obj_fun += 50.*norm2(incr_dev);
    if(active(age_incr))
      wt_like(2) += 12.5 * norm2(first_difference(first_difference(age_incr)));
      // wt_like(2) += 12.5 * norm2(age_incr);

    if(active(yr_incr))
      wt_like(3) += 12.5 * norm2(yr_incr);
    // cout<<"ObjFunWt: "<< norm2(log(wt_pred_m) - log(wt_obs_m))<< endl;; // matrices for the period of wt-age data (modify to use sample size)
    obj_fun += sum(wt_like);
  }
  if (!do_wt_only)
  {
  if (active(rec_dev))
  {
    rec_like(1) = norm2(rec_dev);
    sigma_rec = norm2(rec_dev);
    var_rec = (sigma_rec/((endyr-styr)+1))+.0001;      //variance of Rbar from styr to endyr

    rec_like(2)  = norm2(init_dev_m);
    rec_like(2) += norm2(init_dev_f);
    rec_like(4) = (1/(2*var_rec))*norm2(rec_dev_future);
  }
  if(active(R_logalpha))
    rec_like(3) = (0.5*norm2(log(SAM_recruits)-log(SRC_recruits+1.0e-3)))/(sigma_R*sigma_R);

  obj_fun += lambda(1)* sum(rec_like);

  if (active(q_srv))
  {
     q_like(1) = .5* square(log(q_srv(1))-log(q_exp))/(q_sigma*q_sigma);
     obj_fun += q_like(1);
   }
  if (active(sel_slope_fsh_devs_f))
  {
    sel_like.initialize();
    // Implies a CV of 0.5 on time-varying selectivity parameter
    sel_like(1) += .5*norm2(sel_slope_fsh_devs_f)/(slp_sigma*slp_sigma);
    sel_like(1) += .5*norm2(sel_slope_fsh_devs_m)/(slp_sigma*slp_sigma);
    sel_like(2) += .5*norm2(sel50_fsh_devs_f)/(a50_sigma*a50_sigma);
    sel_like(2) += .5*norm2(sel50_fsh_devs_m)/(a50_sigma*a50_sigma);
    obj_fun += sum(sel_like);
  }

  if (active(natmort_f))
  {
     m_like = .5* square(log(natmort_f)-log(m_exp))/(m_sigma*m_sigma);
     obj_fun += m_like;
  }

  if (current_phase()>phase_wt)
  {
    Age_Like();
    Srv_Like();
    if (phase_fmort>0)
    {
       catch_like = norm2(log(catch_bio+.000001)-log(pred_catch+.000001));
       // obj_fun += 1. * catch_like;
       obj_fun += lambda(3) * catch_like;
    }
    Fmort_Pen();
    obj_fun +=fpen;
  }

  if(active(wt_fsh_fut_f)) 
    for (int j=1;j<=nages;j++)
    {
      dvariable res = wt_fsh_mn_f(j)-wt_fsh_fut_f(j);
      obj_fun += res*res / (2.*wt_fsh_sigma_f(j)*wt_fsh_sigma_f(j));

      res           = wt_fsh_mn_m(j)-wt_fsh_fut_m(j);
      obj_fun += res*res / (2.*wt_fsh_sigma_m(j)*wt_fsh_sigma_m(j));

      res = wt_pop_mn_f(j)-wt_pop_fut_f(j);
      obj_fun += res*res / (2.*wt_pop_sigma_f(j)*wt_pop_sigma_f(j));

      res = wt_pop_mn_m(j)-wt_pop_fut_m(j);
      obj_fun += res*res / (2.*wt_pop_sigma_m(j)*wt_pop_sigma_m(j));
    }

  if(active(log_msy_sel_f)) 
  {
    // dvar_vector pf_tmp = log(F(1,endyr)/F(1,endyr,nages)); // NOTE: For one fishery only!!!
    // obj_fun += 0.5*norm2(pf_tmp - log_sel_coff)/(pf_sigma*pf_sigma);
    dvar_vector log_sel_mean(1,nages);
    log_sel_mean  = log_sel_fsh_f(1,endyr) ;
    log_sel_mean += log_sel_fsh_f(1,endyr-1) ;
    log_sel_mean += log_sel_fsh_f(1,endyr-2) ; 
    log_sel_mean /= 3. ; 
    obj_fun += 0.5*norm2(log_sel_mean - log_msy_sel_f)/(pf_sigma*pf_sigma);
    log_sel_mean  = log_sel_fsh_m(1,endyr) ;
    log_sel_mean += log_sel_fsh_m(1,endyr-1) ;
    log_sel_mean += log_sel_fsh_m(1,endyr-2) ; 
    log_sel_mean /= 3. ; 
    obj_fun += 0.5*norm2(log_sel_mean - log_msy_sel_m)/(pf_sigma*pf_sigma);
  }

  // small penalty to initialize stock to have similar values (unless data suggest otherwise)
  if(active(init_dev_m))
    obj_fun += norm2(init_dev_m-init_dev_f);
  }

FUNCTION Fmort_Pen
//Phases less than 3, penalize high F values
  if (current_phase()<3)
     fpen += 10.*norm2(Fmort - .2);
  else
     fpen += .01*norm2(Fmort - .2);
  fpen += 100.*square(mean(fmort_dev));

FUNCTION Srv_Like
//Fit to trawl survey assuming the lognormal distribution
  srv_like.initialize();
  //cout<<pred_srv(1,yrs_srv(1,22)) <<" "<<pred_srv(1,yrs_srv(1,21)) <<endl;
  for (k=1;k<=nsrv;k++){
    for (i=1;i<=nyrs_srv(k);i++){
     // Log-normal
      srv_like(k) += lambda(2) * square(log(obs_srv(k,i) / pred_srv(k,yrs_srv(k,i)) ))/ (2.* obs_lse_srv(k,i) * obs_lse_srv(k,i)); 
     // Normal
      //srv_like(k) += lambda(2) * square(obs_srv(k,i) - pred_srv(k,yrs_srv(k,i)) )/ (2.* obs_se_srv(k,i) * obs_se_srv(k,i));
    }
  }
  obj_fun += sum(srv_like);

FUNCTION Age_Like
//Fit to fishery and survey age compositions assuming the multinomial distribution
  age_like_fsh.initialize();
  for (k=1;k<=nfsh;k++)
  {
      for (i=1;i<=nyrs_fsh_age_c(k);i++)
         age_like_fsh(k) -= nsmpl_fsh_c(k,i)*(oac_fsh_c(k,i) + 0.001) * log(eac_fsh_c(k,i) + 0.001);

      for (i=1;i<=nyrs_fsh_age_s(k);i++)
         age_like_fsh(k) -= nsmpl_fsh_s(k,i)*(oac_fsh_s(k,i) + 0.001) * log(eac_fsh_s(k,i) + 0.001);
  }
  age_like_fsh-=offset_fsh;
  obj_fun += sum(age_like_fsh);

  age_like_srv.initialize();
  for (k=1;k<=nsrv;k++)
  {
    for (i=1;i<=nyrs_srv_age_c(k);i++)
      age_like_srv(k) -= nsmpl_srv_c(k,i)*(oac_srv_c(k,i) + 0.001) * log(eac_srv_c(k,i) + 0.001);
    for (i=1;i<=nyrs_srv_age_s(k);i++)
      age_like_srv(k) -= nsmpl_srv_s(k,i)*(oac_srv_s(k,i) + 0.001) * log(eac_srv_s(k,i) + 0.001);
  }
  age_like_srv-=offset_srv;
  obj_fun += sum(age_like_srv);

FUNCTION dvariable spr_ratio(dvariable trial_F,dvar_vector& sel)
  dvariable SBtmp;
  dvar_vector Ntmp(1,nages);
  dvar_vector srvtmp(1,nages);
  SBtmp.initialize();
  Ntmp.initialize();
  srvtmp.initialize();
  dvar_vector Ftmp(1,nages);
  Ftmp = sel*trial_F;
  srvtmp  = exp(-(Ftmp + natmort_f) );
  dvar_vector wttmp = wt_pop_f(endyr);
  Ntmp(1)=1.;
  j=1;
  // Sp_Biom_future(i) = elem_prod(wt_pop ,maturity) * elem_prod(nage_future(i),pow(S_future(i),yrfrac)) ;
	// Note using end-year maturity for spr
  SBtmp  += Ntmp(j)*maturity(endyr,j)*wttmp(j)*pow(srvtmp(j),spmo_frac);
  for (j=2;j<nages;j++)
  {
    Ntmp(j) = Ntmp(j-1)*srvtmp(j-1);
    SBtmp  += Ntmp(j)*maturity(endyr,j)*wttmp(j)*pow(srvtmp(j),spmo_frac);
  }
  Ntmp(nages)=Ntmp(nages-1)*srvtmp(nages-1)/(1.-srvtmp(nages));

  SBtmp  += Ntmp(nages)*maturity(endyr,nages)*wttmp(nages)*pow(srvtmp(nages),spmo_frac);
  // cout<<sel_tmp<<endl<< " Trial: "<<trial_F<<" "<<SBtmp<<" Phizero "<<phizero<<endl;
  phizero=get_spr(0.0);
  return(get_spr(trial_F)/phizero);

FUNCTION compute_spr_rates
//Compute SPR rates and add them to the likelihood for females
  SB0=0.;
  SBF40=0.; 
  SBF35=0.;
  SBF30=0.;
//Fill in the number of SSB matrix
  for (i=1;i<=4;i++)
    Nspr(i,1)=1.;

//A temporary transposed matrix is made for selectivity for matrix multiplication reasons
  // dvar_matrix sel_tmp(sel_fsh_f(1).colmin(),sel_fsh_f(1).colmax(),sel_fsh_f(1).rowmin(),sel_fsh_f(1).rowmax());
  // sel_tmp = trans(sel_fsh_f(1));
  dvar_matrix sel_tmpu(1,nfsh,1,nages);
  dvar_matrix sel_tmp(1,nages,1,nfsh);
  for (k=1;k<=nfsh;k++)
    sel_tmpu(k) = sel_fsh_f(k,endyr);
  sel_tmp = trans(sel_tmpu);

  for (j=2;j<nages;j++)
  {
    Nspr(1,j) = Nspr(1,j-1)*exp(-natmort_f);
    Nspr(2,j) = Nspr(2,j-1)*exp(-(natmort_f+F40*sel_tmp(j-1)));
    Nspr(3,j) = Nspr(3,j-1)*exp(-(natmort_f+F35*sel_tmp(j-1)));
    Nspr(4,j) = Nspr(4,j-1)*exp(-(natmort_f+F30*sel_tmp(j-1)));
  }
//Fill in the plus group
  Nspr(1,nages)=Nspr(1,nages-1)*exp(-natmort_f)/(1.-exp(-natmort_f));
  Nspr(2,nages)=Nspr(2,nages-1)*exp(-(natmort_f+F40*sel_tmp(nages-1)))/(1.-exp(-(natmort_f+F40*sel_tmp(nages))));
  Nspr(3,nages)=Nspr(3,nages-1)*exp(-(natmort_f+F35*sel_tmp(nages-1)))/(1.-exp(-(natmort_f+F35*sel_tmp(nages))));
  Nspr(4,nages)=Nspr(4,nages-1)*exp(-(natmort_f+F30*sel_tmp(nages-1)))/(1.-exp(-(natmort_f+F30*sel_tmp(nages))));

  for (j=1;j<=nages;j++)
  {
//Kill them off until they spawn (spmo_frac), 0.5*Nspr is for females only
    SB0    += 0.5 * Nspr(1,j) * maturity(endyr,j) * wt_pop_fut_f(j) * exp(-spmo_frac*natmort_f);
    SBF40  += 0.5 * Nspr(2,j) * maturity(endyr,j) * wt_pop_fut_f(j) * exp(-spmo_frac*(natmort_f+F40*sel_tmp(j)));
    SBF35  += 0.5 * Nspr(3,j) * maturity(endyr,j) * wt_pop_fut_f(j) * exp(-spmo_frac*(natmort_f+F35*sel_tmp(j)));
    SBF30  += 0.5 * Nspr(4,j) * maturity(endyr,j) * wt_pop_fut_f(j) * exp(-spmo_frac*(natmort_f+F30*sel_tmp(j)));
  }
  sprpen   =10.*square(SBF40-0.4*SB0);
  sprpen  +=10.*square(SBF35-0.35*SB0);
  sprpen  +=10.*square(SBF30-0.3*SB0);

  obj_fun += sprpen;

FUNCTION compute_sr_fit
  // sigma_R = 0.6;
  R_alpha=mfexp(R_logalpha);
  R_beta=mfexp(R_logbeta);
  for (int i=styr_sr; i <= endyr_sr; i++)
  {
    SAM_recruits(i)= 2.*natage_f(i,1);
    SRC_recruits(i) = R_alpha*SSB(i-rec_lag)*(mfexp(-1.*R_beta*SSB(i-rec_lag)));  // Ricker model
  }

FUNCTION dvariable get_yield(const dvariable& Ftmp)      // calculates yield for any value of F 
  dvariable phi;  
  dvariable ypr;
  dvariable yield;
  dvariable R_eq;
  phi  = get_spr(Ftmp);
  R_eq = -(log(1/(R_alpha*phi)))/(phi*R_beta);   // Ricker formulation of equilibrium recruitment at each F
  ypr  = get_ypr(Ftmp);
  yield=R_eq*ypr;
  return(yield);

FUNCTION dvariable get_spr(dvariable Ftemp)    // calculation of equilibrium SPR for Fmsy determination
  dvariable phi;
  dvar_vector sel_tmp(1,nages);
  sel_tmp = partial_F_f; // Set selectivity to 
  dvar_vector Ntmp(1,nages);
  Ntmp(1)=1.;
  for (j=2;j<=nages;j++)
  {
     Ntmp(j)=Ntmp(j-1)*exp(-(natmort_f+Ftemp*sel_tmp(j-1)));  // fills in matrix for ages 2 through nages-1
           
   }   
//Fill in the plus group
   Ntmp(nages)=Ntmp(nages-1)*exp(-(natmort_f+Ftemp*sel_tmp(nages-1)))/(1.-exp(-(natmort_f+Ftemp*sel_tmp(nages))));
           
//  kill them off till they spawn and convert from numbers to biomass
   phi = 0.5*elem_prod(Ntmp,maturity(endyr))*elem_prod(wt_pop_fut_f,exp(-spmo_frac*(natmort_f+Ftemp*sel_tmp)));
   return(phi);
  
FUNCTION dvariable get_ypr(dvariable Ftemp)    // calculation of equilibrium YPR for Fmsy determination
//A temporary transposed matrix is made for selectivity for matrix multiplication reasons
  dvariable ypr;
  dvar_vector sel_tmp_f(1,nages);
  dvar_vector sel_tmp_m(1,nages);
  sel_tmp_f = partial_F_f; // Set selectivity to 
  sel_tmp_m = partial_F_m; // Set selectivity to 
 
  dvar_vector Ntmp_f(1,nages);
  dvar_vector Ntmp_m(1,nages);
  Ntmp_f(1)=1.;
  Ntmp_m(1)=1.;
  for (j=2;j<=nages;j++)
  {
     Ntmp_f(j)=Ntmp_f(j-1)*exp(-(natmort_f+Ftemp*sel_tmp_f(j-1)));  // fills in matrix for ages 2 through nages-1
     Ntmp_m(j)=Ntmp_m(j-1)*exp(-(natmort_m+Ftemp*sel_tmp_m(j-1)));  // fills in matrix for ages 2 through nages-1
  }   
//Fill in the plus group
   Ntmp_f(nages)=Ntmp_f(nages-1)*exp(-(natmort_f+Ftemp*sel_tmp_f(nages-1)))/(1.-exp(-(natmort_f+Ftemp*sel_tmp_f(nages))));
   Ntmp_m(nages)=Ntmp_m(nages-1)*exp(-(natmort_m+Ftemp*sel_tmp_m(nages-1)))/(1.-exp(-(natmort_m+Ftemp*sel_tmp_m(nages))));
           
//  calculate yield per recruit
   // ypr = elem_div(Ftemp*sel_tmp,Ftemp*sel_tmp+natmort)*elem_prod(elem_prod(Ntmp,wt_pop(endyr)),(1-exp(-(natmort+Ftemp*sel_tmp))));
   ypr  = elem_div(Ftemp*sel_tmp_f,Ftemp*sel_tmp_f+natmort_f)*elem_prod(elem_prod(Ntmp_f,wt_fsh_fut_f),(1-exp(-(natmort_f+Ftemp*sel_tmp_f))));
   ypr += elem_div(Ftemp*sel_tmp_m,Ftemp*sel_tmp_m+natmort_m)*elem_prod(elem_prod(Ntmp_m,wt_fsh_fut_m),(1-exp(-(natmort_m+Ftemp*sel_tmp_m))));
   return(ypr);

FUNCTION dvariable get_biom_per_rec(dvariable Ftemp)    
 // calculation of equilibrium biomass for geometric mean calc determination
  dvariable bpr;
  dvar_vector sel_tmp_f(1,nages);
  dvar_vector sel_tmp_m(1,nages);
  dvar_vector Ntmp_f(1,nages);
  dvar_vector Ntmp_m(1,nages);
  sel_tmp_f = partial_F_f; // Set selectivity to 
  sel_tmp_m = partial_F_m; // Set selectivity to 
  Ntmp_f(1)=1.;
  Ntmp_m(1)=1.;
  for (j=2;j<=nages;j++)
  {
     Ntmp_f(j)=Ntmp_f(j-1)*exp(-(natmort_f+Ftemp*sel_tmp_f(j-1)));  // fills in matrix for ages 2 through nages-1
     Ntmp_m(j)=Ntmp_m(j-1)*exp(-(natmort_m+Ftemp*sel_tmp_m(j-1)));  // fills in matrix for ages 2 through nages-1
  }   
//  calculate biomass per recruit
   // bpr = Ntmp(6,nages)*wt_pop(endyr)(6,nages); //JNI? need to change here to wt_fmsy?
   bpr  = Ntmp_f(ABC_age_lb,nages)*wt_pop_fut_f(ABC_age_lb,nages); 
   bpr += Ntmp_m(ABC_age_lb,nages)*wt_pop_fut_m(ABC_age_lb,nages); 
   return(bpr);

FUNCTION get_msy
//  loop which finds Fmsy by trying different values on the yield curve
    double d;
    dvariable F1;
    dvariable F2;
    dvariable F3;
    dvariable yld1;
    dvariable yld2;
    dvariable yld3;
    dvariable Fprime;
    dvariable F2prime;
    dvariable R_eq;
    dvariable phi;
    F1 = 1.1* natmort_f;          //start with F=M
    d=0.00001;
    for (i=1;i<=6;i++)
    {
      F2      = F1+d;
      F3      = F1-d;
      yld1    = get_yield(F1);
      yld2    = get_yield(F2);
      yld3    = get_yield(F3);
      Fprime  = (yld2-yld3)/(2*d);   //  Newton-Raphson approximation for first derivitive
      F2prime = (yld3-(2*yld1)+yld2)/(d*d);  // Newton-Raphson approximation for second derivitive
      F1     -= Fprime/F2prime;              // value to increment F for next time through the loop
      // cout <<"F1= "<<F1<<" Fprime= "<<Fprime<<endl;
    }
    Fmsy    = F1;
    phi     = get_spr(Fmsy);
    R_eq    = -(log(1/(R_alpha*phi)))/(phi*R_beta);   // Ricker formulation of equilibrium recruitment at each F
    Bmsy    = R_eq*phi;
    msy     = get_yield(Fmsy);
    logFmsy = log(Fmsy);
    Bmsy    = R_eq*phi;

    SPR_OFL = spr_ratio(Fmsy,partial_F_f);
    Bmsyr   = R_eq*get_biom_per_rec(Fmsy)    ;
    Fmsyr   = msy/Bmsyr;
    logFmsyr= log(Fmsyr);
    get_2yr_proj();

FUNCTION get_2yr_proj
    dvar_vector Ntmp_f(1,nages);
    dvar_vector Ntmp_m(1,nages);
    dvar_vector Ntmp2_f(1,nages);
    dvar_vector Ntmp2_m(1,nages);
    dvar_vector Stmp_f(1,nages);
    dvar_vector Stmp_m(1,nages);
    dvariable   Ftmp;
    Ntmp_f.initialize();
    Ntmp_m.initialize();
    Ntmp2_f.initialize();
    Ntmp2_m.initialize();
    Stmp_f.initialize();
    Stmp_m.initialize();

  // Begin year N at age in endyr + 1 (start of future)
    Ntmp_f(2,nages) = ++elem_prod(natage_f(endyr)(1,nages-1), S_f(endyr)(1,nages-1));  
    Ntmp_f(nages)  += natage_f(endyr,nages)*S_f(endyr,nages);
    Ntmp_m(2,nages) = ++elem_prod(natage_m(endyr)(1,nages-1), S_m(endyr)(1,nages-1));  
    Ntmp_m(nages)  += natage_m(endyr,nages)*S_m(endyr,nages);

  // three year mean for future
    Ntmp_f(1)   = .5*mean(pred_rec(endyr-3,endyr));
    Ntmp_m(1)   = Ntmp_f(1);
    ABC_biom1   = Ntmp_f(ABC_age_lb,nages) * wt_pop_f(endyr)(ABC_age_lb,nages);
    ABC_biom1  += Ntmp_m(ABC_age_lb,nages) * wt_pop_m(endyr)(ABC_age_lb,nages);
    Ftmp        = SolveF2(Ntmp_f,Ntmp_m,yr1_futcat);
    Stmp_f      = mfexp(-(Ftmp*partial_F_f + natmort_f));
    Stmp_m      = mfexp(-(Ftmp*partial_F_m + natmort_m));

    SSB_1       = value(elem_prod(elem_prod(Ntmp_f,pow(Stmp_f,spmo_frac)),maturity(endyr))*wt_pop_fut_f);

  // Begin year N at age in endyr + 2 (second year of future)
    Ntmp2_f(2,nages) = ++elem_prod(Ntmp_f(1,nages-1), Stmp_f(1,nages-1));  
    Ntmp2_f(nages)  += Ntmp_f(nages)*Stmp_f(nages);
    Ntmp2_m(2,nages) = ++elem_prod(Ntmp_m(1,nages-1), Stmp_m(1,nages-1));  
    Ntmp2_m(nages)  += Ntmp_m(nages)*Stmp_m(nages);

    ABC_biom2        = Ntmp2_f(ABC_age_lb,nages) * wt_pop_f(endyr)(ABC_age_lb,nages); 
    ABC_biom2       += Ntmp2_m(ABC_age_lb,nages) * wt_pop_m(endyr)(ABC_age_lb,nages); 

    Ftmp             = SolveF2(Ntmp2_f,Ntmp2_m,yr2_futcat);
    Stmp_f           = mfexp(-(Ftmp*partial_F_f + natmort_f));
    SSB_2            = value(elem_prod(elem_prod(Ntmp2_f,pow(Stmp_f,spmo_frac)),maturity(endyr))*wt_pop_fut_f);
    // cout <<"Fmsy= "<<Fmsy<<" MSY= "<<msy<<" Bmsy= "<<Bmsy<<endl;

FUNCTION Future_projections
//Future SSB set equal to estimated SSB with a recruitment lag
//SSB_future(styr_fut-rec_lag,styr_fut-1) = SSB(endyr-rec_lag+1,endyr);

//Start calculation for first year of numbers at age matrix in the projection
  nage_future_f(styr_fut)(2,nages) = ++elem_prod(natage_f(endyr)(1,nages-1),S_f(endyr)(1,nages-1));
  nage_future_f(styr_fut,nages)   += natage_f(endyr,nages)*S_f(endyr,nages);
  nage_future_m(styr_fut)(2,nages) = ++elem_prod(natage_m(endyr)(1,nages-1),S_m(endyr)(1,nages-1));
  nage_future_m(styr_fut,nages)   += natage_m(endyr,nages)*S_m(endyr,nages);
  future_SSB=0.;
  catch_future=0.;
  
//Loop to cycle different fishing mortality values through the projections
  for (int m=1;m<=num_proj_Fs;m++)
  {
     switch (m)
      {
         case 1:
          ftmp = F40;
          break;
         case 2:
          ftmp = F35;
          break;
         case 3:
          ftmp = F30;
          break;
         case 4:
          ftmp = 0.0;
          break;
  }
//Calculation of future F's, Z and survival (S)
  Z_future_f = natmort_f;
  Z_future_m = natmort_m;
  for (i=endyr+1;i<=endyr_fut;i++)
  {
     for (k=1;k<=nfsh;k++)
     {
       F_future_f(k,i) = sel_fsh_f(k,endyr) * ftmp(k);
       Z_future_f(i)  += F_future_f(k,i);
       F_future_m(k,i) = sel_fsh_m(k,endyr) * ftmp(k);
       Z_future_m(i)  += F_future_m(k,i);
     }
   S_future_f(i) = exp(-Z_future_f(i));
   S_future_m(i) = exp(-Z_future_m(i));
  }
  
  //Future recruitment and SSB
  //Mean average recruitment of the time-series is used for the projection
    //NOTE spawningbiomass is beginyear, NOT spawnmonth
  dvariable Rectmp=mfexp(mean_log_rec);
  for (i=styr_fut;i<endyr_fut;i++)
  {
    nage_future_f(i,1) = .5*Rectmp*mfexp(rec_dev_future(i));
    nage_future_m(i,1) = nage_future_f(i,1);
    SSB_future(i) = elem_prod(nage_future_f(i)/2,pow(S_future_f(i),spmo_frac)) * elem_prod(wt_pop_fut_f,maturity(endyr));  //need to add recruitment lag
    TotBiom_future(i)  = nage_future_f(i)*wt_pop_fut_f; 
    TotBiom_future(i) += nage_future_m(i)*wt_pop_fut_m; 
  //Now graduate for the next year.....
    nage_future_f(i+1)(2,nages) = ++elem_prod(nage_future_f(i)(1,nages-1),S_future_f(i)(1,nages-1));
    nage_future_f(i+1,nages)   += nage_future_f(i,nages) * S_future_f(i,nages);
    nage_future_m(i+1)(2,nages) = ++elem_prod(nage_future_m(i)(1,nages-1),S_future_m(i)(1,nages-1));
    nage_future_m(i+1,nages)   += nage_future_m(i,nages) * S_future_m(i,nages);
  }
  nage_future_f(endyr_fut,1) = .5*Rectmp*mfexp(rec_dev_future(i));
  nage_future_m(endyr_fut,1) = nage_future_f(endyr_fut,1) ;

  SSB_future(endyr_fut) = elem_prod(nage_future_f(endyr_fut)/2,pow(S_future_f(endyr_fut),spmo_frac)) * elem_prod(wt_pop_fut_f,maturity(endyr));  //need to add recruitment lag
  TotBiom_future(endyr_fut)  = nage_future_f(endyr_fut)*wt_pop_fut_f;
  TotBiom_future(endyr_fut) += nage_future_m(endyr_fut)*wt_pop_fut_m;

  //Calculations of catch at predicted future age composition
  for (i=styr_fut;i<=endyr_fut;i++)
  {
    catage_future_f(i) = 0.;
    catage_future_m(i) = 0.;

    for (k=1;k<=nfsh;k++)
    {
      catage_future_f(i) += elem_prod(nage_future_f(i), elem_prod(F_future_f(k,i), elem_div( (1.-S_future_f(i) ),Z_future_f(i))));
      catage_future_m(i) += elem_prod(nage_future_m(i), elem_prod(F_future_m(k,i), elem_div( (1.-S_future_m(i) ),Z_future_m(i))));
    }

    if (m!=num_proj_Fs)  // catch biomass in future
      catch_future(m,i)  += catage_future_f(i)*wt_fsh_fut_f + catage_future_m(i)*wt_fsh_fut_m      ;

    future_SSB(m,i) = SSB_future(i);
    future_TotBiom(m,i) = TotBiom_future(i);
   }
  }   //End of loop over F's

FUNCTION write_srec
  dvariable phi;  
  phizero=get_spr(0.0);
  // cout<<phizero<<endl;
  dvariable Rzero = -(log(1/(R_alpha*phizero)))/(phizero*R_beta);   // Ricker formulation of equilibrium recruitment at each F
  dvariable Bzero = Rzero*phizero;
  dvariable Btmp  =  .8*Bzero;
  srecpar << "# Bzero,  PhiZero,  Alpha, sigmar"<<endl;
  srecpar     << Bzero          <<
         " "  << phizero/1000   <<
         " "  << (log(R_alpha)-R_beta*Btmp+log(phizero))/(1.-Btmp/Bzero) <<
         " "  << sigma_R         << endl;
  R_report << "$Bzero" <<endl;
  R_report << Bzero    <<endl;
  R_report << "$phizero" <<endl;
  R_report << phizero/1000<<endl;
  R_report << "$alpha" <<endl;
  R_report << (log(R_alpha)-R_beta*Btmp+log(phizero))/(1.-Btmp/Bzero) <<endl;
  R_report << "$sigmar" <<endl;
  R_report << sigma_R    <<endl;


REPORT_SECTION
  save_gradients(gradients);
  dvar_vector wt_pop_tmp_f(1,nages);
  for (int i=0;i<=3;i++)
    {
      dvar_vector incr_dev_tmp(2,nages);
      incr_dev_tmp(5,nages-5)     = sst_alpha * double(i) ;
      incr_dev_tmp(nages-4,nages) = incr_dev_tmp(nages-5) ;
      incr_dev_tmp(2,4)           = incr_dev_tmp(5) ;
      wt_pop_tmp_f(1)        = wt_vbg_f(1);
      wt_pop_tmp_f(2,nages)  = ++wt_vbg_f(1,nages-1) + elem_prod(base_incr_f,mfexp(incr_dev_tmp)) ; // initializes estimates to correct values...
      report <<i<<" "<<double(i)*sst_alpha<<" "<<wt_pop_tmp_f<<endl;
    }
    report << "wt_pop_f"<<endl;
    report << wt_pop_f<<endl;
    report << "wt_pop_m"<<endl;
    report << wt_pop_m<<endl;
  // This section to write s-rec parameters to input file for projection model
  // need Rzero, phizero calculated then use alpha and beta
  if (active(R_logbeta))
    write_srec();
  cout<<"Done with phase "<<current_phase()<<" --in report file"<<endl;
  cout<<wt_pop_mn_f(4,10)<<endl;
  cout<<wt_fsh_mn_f(4,10)<<endl;
  report << model_name<<endl;
  if (last_phase()&&!do_wt_only)
  {
  dvariable Ftemp;
  Ftemp=0.01;
  dvariable ypr;
  dvariable yield;
  dvariable phi;  
  dvariable R_eq;
   for (i=1;i<=60;i++) // range of F values to evaluate for Fmsy (0.02 to 0.31-30 steps)
    {
     Ftemp=Ftemp+0.01;            //increments F by 0.01 each pass through the loop
     phi=get_spr(Ftemp);
     R_eq = -(log(1/(R_alpha*phi)))/(phi*R_beta);   // Ricker formulation of equilibrium recruitment at each F
     ypr=get_ypr(Ftemp);
     yield=R_eq*ypr;
     cout <<" Ftemp= "<<Ftemp<<" r= "<<yield/(R_eq*phi)<<" phi= "<<phi<<" R_eq= "<<R_eq<<" ypr= "<<ypr<<" yield= "<<yield<< endl;
    }
  }
  report << "Estimated_numbers_of_fish year "<<age_vector << endl;
   for (i=styr;i<=endyr;i++)
     report << " "<<  i << ", "<< natage_f(i) << natage_m(i)<<endl;

  report << endl<< "Estimated_F_mortality Fishery Year " << age_vector<< endl;
   for (k=1;k<=nfsh;k++)
     for (i=styr;i<=endyr;i++)
       report << " "<< k <<" "<< i << " "<<F_f(k,i) << " " << F_m(k,i)<<endl;

   report << endl << "Observed survey values " << endl;
   for (k=1;k<=nsrv;k++)
   {
     int ii=1;
     report <<endl<< "ObsSurvey "<< k <<"  " <<"predSurvey "<< endl;
     for (i=styr;i<=endyr;i++)
     {
        if (yrs_srv(k,ii)==i)
        {
          report << i << ", " << obs_srv(k,ii) << ", " << pred_srv(k,i)<< endl;
          ii++;
        }
        else
         report << i<< ",NA, "<<pred_srv(k,i)<<endl;
     }
  }

  report << endl<< "Observed_proportion "<<endl;
  for (k=1;k<=nfsh;k++)
  {
    report << "Observed_fishery_age_comp "<< k <<"  "<<endl ;
    for (i=1;i<=nyrs_fsh_age_c(k);i++)
      report << yrs_fsh_age_c(k,i)<< ", " << oac_fsh_c(k,i) << " "<<Eff_N(oac_fsh_c(k,i),eac_fsh_c(k,i))<< endl; 
    for (i=1;i<=nyrs_fsh_age_s(k);i++)
    {
      report << yrs_fsh_age_s(k,i)<< ", " << oac_fsh_s(k,i) << ", Female/Total: "<< sum(oac_fsh_s(k,i)(1,nages)) 
             << " "<<Eff_N(oac_fsh_s(k,i),eac_fsh_s(k,i)) 
             << " "<<mn_age(oac_fsh_s(k,i))
             << " "<<mn_age(eac_fsh_s(k,i))
             << " "<<Sd_age(oac_fsh_s(k,i),eac_fsh_s(k,i))
             << " "<<Eff_N2(oac_fsh_s(k,i),eac_fsh_s(k,i))
             <<endl;
    }
  }

  report << endl << "Predicted_prop_Fishery "<<endl;
  for (k=1;k<=nfsh;k++)
  {
    report << "Pred_fishery_age_comp "<< k <<"  "<<endl ;
    for (i=1;i<=nyrs_fsh_age_c(k);i++)
      report << yrs_fsh_age_c(k,i)<< ", " << eac_fsh_c(k,i) << endl;
    for (i=1;i<=nyrs_fsh_age_s(k);i++)
      report << yrs_fsh_age_s(k,i)<< ", " << eac_fsh_s(k,i) << ", Female/Total: "<< sum(eac_fsh_s(k,i)(1,nages)) << endl; 
  }

  report << endl<< "Observed_prop_Survey "<<endl;
  for (k=1;k<=nsrv;k++)
  {
    report << "Obs_Survey_age_comp "<< k <<"  "<<endl ;
    for (i=1;i<=nyrs_srv_age_c(k);i++)
      report << yrs_srv_age_c(k,i)<< ", " << oac_srv_c(k,i)  << " "<<Eff_N(oac_srv_c(k,i),eac_srv_c(k,i))<< endl; 
    for (i=1;i<=nyrs_srv_age_s(k);i++)
      report << yrs_srv_age_s(k,i)<< ", " << oac_srv_s(k,i) << ", Female/Total: "<< sum(oac_srv_s(k,i)(1,nages))  << " "<<Eff_N(oac_srv_s(k,i),eac_srv_s(k,i))<< endl; 
  }

  report << endl << "Predicted_prop_Survey "<<endl;
  for (k=1;k<=nsrv;k++)
  {
    report << "Pred_Survey_age_comp "<< k <<"  "<<endl ;
    for (i=1;i<=nyrs_srv_age_c(k);i++)
      report << yrs_srv_age_c(k,i)<< ", " << eac_srv_c(k,i) << endl;
    for (i=1;i<=nyrs_srv_age_s(k);i++)
      report << yrs_srv_age_s(k,i)<< ", " << eac_srv_s(k,i) << ", Female/Total: "<< sum(eac_srv_s(k,i)(1,nages)) << endl; 
  }
      
  report << endl<< "Observed_catch_biomass " << endl;
  report << catch_bio << endl;
  report << "predicted_catch_bomass" << endl;
  report << pred_catch << endl;
  report << endl << "Estimated_annual_Fishing_mortality "<< endl;
  for (i=styr; i<endyr;i++)
  {
    report << i << " ";
    for (k=1;k<=nfsh;k++)
      report<< mean(F_f(k,i)) << " ";
    report <<Fmort(i)<<endl;
  }
  report << endl<<"Selectivity sex fshry_age " << age_vector(1,nages)<<endl;
  for (k=1;k<=nfsh;k++)
  {
  for (i=styr; i<=endyr;i++)
    report << "Fishery female " << k <<" "<<i<<" "<< sel_fsh_f(k,i) << endl;
    report<<endl;
  for (i=styr; i<=endyr;i++)
    report << "Fishery male "   << k <<" "<<i<<" "<< sel_fsh_m(k,i) << endl;
    report<<endl;
  }
  report << "Partial_F  Female  0      " <<partial_F_f  << endl;
  report << "Partial_F  Male    0      " <<partial_F_m  << endl;

  for (k=1;k<=nsrv;k++)
  {
    report << "Survey female " << k <<"  " <<sel_srv_f(k) << endl;
    report << "Survey male   " << k <<"  " <<sel_srv_m(k) << endl;
  }

  report << endl<<"Female_spawning_biomass " << endl;
    for (i=styr;i<=endyr;i++)
      report <<i<<", "<<SSB(i)<<endl;
  
  report << endl<<"Total_biomass " << endl;
    for (i=styr;i<=endyr;i++)
      report <<i<<",  "<<TotBiom(i)<<endl;

  report <<endl<<endl;
  report <<"F40= " << F40 << endl;
  report <<"F35= " << F35 << endl;
  report <<"F30= " << F30 << endl;
  report <<"Unfished_spawning_biomass_per_recruit " << SB0 << endl;
  report << "Spawning_biomass_per_recruit_from_F40_harvest_rate " << SBF40<<endl;
  report << "Spawning_biomass_per_recruit_from_F35_harvest_rate " << SBF35<<endl;
  report << "Spawning_biomass_per_recruit_from_F30_harvest_rate " << SBF30<<endl;

  report << "Projected_spawning_biomass " << endl;
  report << "F40_spawning_biomass " << future_SSB(1) <<endl;
  report << "F35_spawning_biomass " << future_SSB(2) <<endl;
  report << "F30_spawning_biomass " << future_SSB(3) <<endl;
  report << "F=0_spawning_biomass " << future_SSB(4) <<endl;

  report << "Projected_total_biomass " << endl;
  report << "F40_total_biomass " << future_TotBiom(1) <<endl;
  report << "F35_total_biomass " << future_TotBiom(2) <<endl;
  report << "F30_total_biomass " << future_TotBiom(3) <<endl;
  report << "F=0_total_biomass " << future_TotBiom(4) <<endl;

  report << "Projected_catch " << endl;
  report << "F40_harvest " << catch_future(1) <<endl;
  report << "F35_harvest " << catch_future(2) <<endl;
  report << "F30_harvest " << catch_future(3) <<endl;
  report << "F=0_harvest " << " 0 0 0 0 0 0 0 0 0 0 0 " <<endl;
  

  report << endl << "Likelihood_Components " << endl;
  report << "survey_likelihood " << srv_like << endl;
  report << "catch_likelihood " <<  catch_like << endl;
  report << "age_likelihood_for_fishery " << age_like_fsh << endl;
  report << "age_likeihood_for_survey " << age_like_srv << endl;
  report << "recruitment_likelilhood " << rec_like << endl;
  report << "selectivity_likelihood " << sel_like << endl;
  report << "q_Prior " <<q_like <<endl;
  report << "m_Prior " <<m_like <<endl;
  report << "F_penalty " << fpen << endl;
  if (phase_bottom_temps>0)
  {
    report << "alpha= " << alpha << endl;
    report << "beta= " << beta << endl;

    report << endl<<"Temperature_Effect_(q) " << endl;
    for (i=1;i<=nyrs_srv(1);i++)
      report <<yrs_srv(1,i)<<", "<<bottom_temps(1,i)<<", "<<mfexp(-alpha+beta*bottom_temps(1,i))<<endl;
  }
  report << " survey_q= " << q_srv<<endl;
  report << "M= " << natmort_f<<" "<<natmort_m << endl;
  report << endl << "Ricker_spawner-recruit_estimates" << endl;
  report << "stock_assessment_model_recruitment_estimates" << endl;
  report << SAM_recruits << endl;
  report << "Ricker_model_predicted_recruitment"<<endl;
  report << SRC_recruits << endl;
  report << "Ricker_alpha= " << endl;
  report << R_alpha << endl;
  report << "Ricker_beta = " << endl;
  report << R_beta << endl;

  report << "Estimated_catch_at_age " << endl;
  report <<"Fishery Year " << age_vector <<endl;
  for (k=1;k<=nfsh;k++)
    for (i=styr;i<=endyr;i++)
      report <<k <<"  " << i << " "<< catage_f(1,i) <<", "<<catage_m(1,i)<< endl;
  report << "Estimated_stock_recruitment_curve" << endl;
  report << "Stock recruitment " << endl;
  dvariable stmp;
  dvariable maxspawn=max(SSB)*1.2;
   for (i=0;i<=30;i++)
   {
     stmp = double(i)*maxspawn/30; 
     report <<  stmp<<" "<< R_alpha*stmp*(mfexp(-R_beta*stmp))<<endl;  // Ricker model
   }
  report << "Estimated_sex_ratio Year Total Mature Age_7+"<< endl;
   for (i=styr;i<=endyr;i++)
     report << " "<<  i << ", "<< 
     sum(natage_f(i))/sum(natage_f(i)+natage_m(i))<<" "<<
     sum(elem_prod(maturity(i),natage_f(i)))/sum(elem_prod(maturity(i),natage_f(i)+natage_m(i)))<<" "<<
     sum(natage_f(i)(7,nages))/sum(natage_f(i)(7,nages)+natage_m(i)(7,nages))<<" "<<
     endl;
  report << "Fishry_wts_age"<<endl;
  report << wt_fsh_f <<endl<<endl;
  report << wt_fsh_m <<endl<<endl;
  report << "Survey_wts_age"<<endl;
  report << wt_srv_f <<endl<<endl;
  report << wt_srv_m <<endl;
  report << "Increment_deviations"<<endl;
  report << incr_dev <<endl<<endl;
  report << "predicted_Observerd_wts"<<endl;
  report << wt_pred_f       <<endl<<endl;
  report << wt_obs_f       <<endl<<endl;
  report << "Wt_Like"<<endl;
  report << wt_like <<endl<<endl;
  report << "Age_Incr_component"<<endl;
  report << age_incr <<endl<<endl;
  report << "Future_Mean_wt"<<endl;
  report << "Female_wt_future: " <<wt_pop_fut_f <<endl<<endl;
  report << "Male_wt_future: "   <<wt_pop_fut_m <<endl<<endl;
  report << "Female_wt_base: " <<wt_vbg_f <<endl<<endl;
  report << "Male_wt_base: "   <<wt_vbg_m <<endl<<endl;
  // exit(1) ;
  
  report << "SARA file for Angie Greig" << endl;

  report << "Yellowfin sole       # stock  " << endl;
  report << "BSAI       # region     (AI AK BOG BSAI EBS GOA SEO WCWYK)" << endl;
  report << "2013       # ASSESS_YEAR - year assessment is presented to the SSC" << endl;
  report << "1a         # TIER  (1a 1b 2a 2b 3a 3b 4 5 6) " << endl;
  report << "none       # TIER2  if mixed (none 1a 1b 2a 2b 3a 3b 4 5 6)" << endl;
  report << "full       # UPDATE (new benchmark full partial)" << endl;
  report << "2          # LIFE_HIST - SAIP ratings (0 1 2 3 4 5)" << endl;
  report << "3          # ASSES_FREQ - SAIP ratings (0 1 2 3 4 5) " << endl;
  report << "5          # ASSES_LEV - SAIP ratings (0 1 2 3 4 5)" << endl;
  report << "5          # CATCH_DAT - SAIP ratings (0 1 2 3 4 5) " << endl;
  report << "3          # ABUND_DAT - SAIP ratings (0 1 2 3 4 5)" << endl;
  report << "654300     # Minimum B  Lower 95% confidence interval for spawning biomass in assessment year" << endl;
  report << "499000     # Maximum B  Upper 95% confidence interval for spawning biomass in assessment year" << endl;
  report << "366000     # BMSY  is equilibrium spawning biomass at MSY (Tiers 1-2) or 7/8 x B40% (Tier 3)" << endl;
  report << "ADMB       # MODEL - Required only if NMFS toolbox software used; optional otherwise " << endl;
  report << "NA         # VERSION - Required only if NMFS toolbox software used; optional otherwise" << endl;
  report << "2          # number of sexes  if 1 sex=ALL elseif 2 sex=(FEMALE, MALE) " << endl;
  report << "1          # number of fisheries" << endl;
  report << "1000000    # multiplier for recruitment, N at age, and survey number (1,1000,1000000)" << endl;
  report << "1          # recruitment age used by model or size" << endl;
  report << "1          # age+ or mmCW+ used for biomass estimate" << endl;
  report << "\"Single age\"        # Fishing mortality type such as \"Single age\" or \"exploitation rate\"" << endl;
  report << "\"Age model\"         # Fishing mortality source such as \"Model\" or \"(total catch (t))/(survey biomass (t))\"" << endl;
  report << "\"Age of maximum F\"  # Fishing mortality range such as \"Age of maximum F\"" << endl; 
  report << "#FISHERYDESC -list of fisheries (ALL TWL LGL POT FIX FOR DOM TWLJAN LGLMAY POTAUG ...)" << endl; 
  report << "ALL" << endl; 
  
  report <<"#FISHERYYEAR - list years used in the model " << endl;
   for (i=styr;  i<=endyr; i++)
      report << i << "	";
      report<<endl;  

  report<<"#AGE - list of ages used in the model"<<endl;
   for (i=1; i<=20;i++)
      report << i << "	";
      report<<endl;    

  report <<"#RECRUITMENT - Number of recruits by year " << endl;
   for (i=styr;  i<=endyr;  i++)
	   report  << natage_f(i,1)+natage_m(i,1) << "	";
	   report<<endl;     
	
  report <<"#SPAWNBIOMASS - Spawning biomass by year in metric tons " << endl;
   for (i=styr;  i<=endyr;  i++)
      report  << SSB(i) << "	";
      report<<endl;  

  report <<"#TOTALBIOMASS - Total biomass by year in metric tons " << endl;
   for (i=styr;  i<=endyr;  i++)
      report  << TotBiom << "	";
      report<<endl;

 // cout <<F_f<<endl;
 	
   report <<"#TOTFSHRYMORT - Fishing mortality rate by year " << endl;
  	for (i=styr;  i<=endyr;  i++)
  	   report  << (F_f(1,i,20)+ F_m(1,i,20))/2<< "	";
  	   report<<endl;
	  
  report <<"#TOTALCATCH - Total catch by year in metric tons " << endl;
   for (i=styr;  i<=endyr;  i++)
      report  << catch_bio(1,i) << "	";
      report<<endl;

  report <<"#MATURITY - Maturity ratio by age (females only)" << endl;  
      report  << maturity(endyr) << endl; 

  report <<"#SPAWNWT - Average spawning weight (in kg) for ages 8-20"<< endl; 
      report <<"0.165 0.212 0.259 0.292 327.8 0.361 0.39 0.412 0.429 0.46 0.47 0.49 0.56"<<endl;                              
      report<<endl;
                    
  report <<"#NATMORT - Natural mortality rate for females then males"<< endl; 
  for (i=1;  i<=20;  i++) 
  report  << 0.12 <<"	";
  report<< endl;   
  for (i=1;  i<=20;  i++) 
  report  << 0.12 <<"	";
  report<< endl;
  
  report << "#N_AT_AGE - Estimated numbers of female (first) then male (second) fish at age " << endl;
  for (i=styr; i<=endyr;i++)
    report <<natage_f(i)<< "	";
    report<<endl;

  for (i=styr; i<=endyr;i++)
    report <<natage_m (i)<< "	";
    report<<endl;
  
  report <<"#FSHRY_WT_KG - Fishery weight at age (in kg) females (first) males (second), only one fishery"<< endl;   
   report <<wt_fsh_f_in(1,endyr)/1000  << "	";
   report<<endl;         
 
   report <<wt_fsh_m_in(1,endyr)/1000  << "	";
   report<<endl;

  report << "#SELECTIVITY - Estimated fishery selectivity for females (first) males (second) at age " << endl;
   report <<" " <<sel_fsh_f(1,endyr);
   report<<endl;

   report <<" "  <<sel_fsh_m(1,endyr);
   report<<endl;
  report << "#SURVEYDESC"<<endl;
  report<<"EBS_trawl_survey BS_slope_trawl_survey AI_trawl_survey"<<endl;

  report<<"SURVEYMULT"<<endl;
  report<<"1 1 1"<<endl;
 
  report << "#EBS_trawl_survey - Bering Sea shelf survey biomass (Year, Obs_biomass, Pred_biomass) " << endl;
   for (i=1; i<=nsrv;i++)
     report << yrs_srv(i) << "	";
     report<<endl;
   for (i=1; i<=nsrv;i++) 
     report<< obs_srv(i)<< "	";
     report<< endl;

  report<<"#STOCKNOTES"<<endl;
  report<<"\"SAFE report indicates that this stock was not subjected to overfishing in 2012 and is neither overfished nor approaching a condition of being overfished in 2013.\""<<endl;

  cout <<"End of report file for phase "<<current_phase()<<endl;
 
BETWEEN_PHASES_SECTION

TOP_OF_MAIN_SECTION
  gradient_structure::set_MAX_NVAR_OFFSET(1600);
  gradient_structure::set_GRADSTACK_BUFFER_SIZE(200000);
  gradient_structure::set_NUM_DEPENDENT_VARIABLES(800); 
  gradient_structure::set_CMPDIF_BUFFER_SIZE(2000000);
  arrmblsize=10000000;

GLOBALS_SECTION
 # include "admodel.h"                      // Include AD class definitions
 // logs input for checking
	/**
	\def log_input(object)
	Prints name and value of \a object on ADMB report %ofstream file.
	*/
	#undef log_input
	#define log_input(object) writeinput << "# " #object "\n" << object << endl;
 // Shorter way to write to R_report
	/**
	\def REPORT(object)
	Prints name and value of \a object on ADMB report %ofstream file.
	*/
	#undef REPORT
	#define REPORT(object) R_report << "# " #object "\n" << object << endl;
  ofstream writeinput("writeinput.log");
  ofstream R_report("fm_R.rep");
  adstring model_name;
  adstring datafile;
  adstring projfile_name;
  adstring cntrlfile_name;
  adstring tmpstring;
  adstring repstring;
  ofstream evalout("evalout.rep");
  ofstream srecpar("srecpar.rep"); // To write srec-parameters for projection model
      
RUNTIME_SECTION
   maximum_function_evaluations 1000,500,1000,6000
   convergence_criteria .0000001,.001,.00001,1e-7

FUNCTION double mn_age(const dvar_vector& pobs)
  int lb1 = pobs.indexmin();
  int ub1 = pobs.indexmax();
  dvector av = age_vector(lb1,ub1)  ;
  double mobs = value(pobs*av);
  return mobs;

FUNCTION double Sd_age(const dvar_vector& pobs, const dvar_vector& phat)
  int lb1 = pobs.indexmin();
  int ub1 = pobs.indexmax();
  dvector av = age_vector(lb1,ub1)  ;
  double mobs = value(pobs*av);
  double stmp = value(sqrt(elem_prod(av,av)*pobs - mobs*mobs));
  return stmp;

FUNCTION double Eff_N_adj(const double, const dvar_vector& pobs, const dvar_vector& phat)
  int lb1 = pobs.indexmin();
  int ub1 = pobs.indexmax();
  dvector av = age_vector(lb1,ub1)  ;
  double mobs = value(pobs*av);
  double mhat = value(phat*av );
  double rtmp = mobs-mhat;
  double stmp = value(sqrt(elem_prod(av,av)*pobs - mobs*mobs));
  return square(stmp)/square(rtmp);

FUNCTION double Eff_N2(const dvar_vector& pobs, const dvar_vector& phat)
  int lb1 = pobs.indexmin();
  int ub1 = pobs.indexmax();
  dvector av = age_vector(lb1,ub1)  ;
  double mobs = value(pobs*av);
  double mhat = value(phat*av );
  double rtmp = mobs-mhat;
  double stmp = value(sqrt(elem_prod(av,av)*pobs - mobs*mobs));
  return square(stmp)/square(rtmp);

FUNCTION double Eff_N(const dvar_vector& pobs, const dvar_vector& phat)
  dvar_vector rtmp = elem_div((pobs-phat),sqrt(elem_prod(phat,(1-phat))));
  double vtmp;
  vtmp = value(norm2(rtmp)/size_count(rtmp));
  return 1/vtmp;


FUNCTION dvariable SolveF2(const dvar_vector& N_tmp_f, dvar_vector& N_tmp_m, double  TACin)
  dvariable dd = 10.;
  dvariable cc = TACin;
  dvar_vector wttmp_f   = wt_pop_fut_f;
  dvar_vector wttmp_m   = wt_pop_fut_m;
  dvariable btmp =  N_tmp_f * wttmp_f ;
  dvariable ftmp;
  ftmp = TACin/btmp;

  dvar_vector Fatmp_f = ftmp * partial_F_f;
  dvar_vector Fatmp_m = ftmp * partial_F_m;
  dvar_vector Z_tmp_f   = Fatmp_f + natmort_f;
  dvar_vector Z_tmp_m   = Fatmp_m + natmort_m;
  dvar_vector S_tmp_f   = exp(-Z_tmp_f);
  dvar_vector S_tmp_m   = exp(-Z_tmp_m);

  int icount;
  icount=0;
  while (dd > 1e-5)
  {
    icount++;
    ftmp   += (TACin-cc) / btmp;
    Fatmp_f = ftmp * partial_F_f;
    Z_tmp_f   = Fatmp_f + natmort_f;
    S_tmp_f   = mfexp( -Z_tmp_f );
    Fatmp_m = ftmp * partial_F_m;
    Z_tmp_m   = Fatmp_m + natmort_m;
    S_tmp_m   = mfexp( -Z_tmp_m );

    cc  = (wttmp_f * elem_prod(elem_div(Fatmp_f,  Z_tmp_f),elem_prod(1.-S_tmp_f,N_tmp_f))); // Catch equation (vectors)
    cc += (wttmp_m * elem_prod(elem_div(Fatmp_m,  Z_tmp_m),elem_prod(1.-S_tmp_m,N_tmp_m))); // Catch equation (vectors)
    dd = cc / TACin - 1.;
    //cout << ispp<<" "<< ftmp << " "<< cc << " "<<TACin<<endl; //cout << sel_F << endl << sel_M << endl << endl; //cout << Fratsel_F << endl << Fratsel_M << endl; //exit(1);
    if (dd<0.) dd *= -1.;
    // cout<<"Ftmp "<<cc<<" "<< ftmp<<endl;
    if (icount>100) dd=1e-6;
  }
  return(ftmp);


  /* FUNCTION dvariable SolveF2(const dvar_vector& N_tmp, double  TACin)
  dvariable dd = 10.;
  dvariable cc = TACin;
  dvar_vector wttmp   = wt_pop_fut_f;
  dvariable btmp =  N_tmp * wttmp ;
  dvariable ftmp;
  ftmp = TACin/btmp;

  dvar_vector Fatmp   = ftmp * partial_F;
  dvar_vector Z_tmp    = Fatmp+ natmort;
  dvar_vector S_tmp = exp(-Z_tmp);

  int icount;
  icount=0;
  while (dd > 1e-5)
  {
    icount++;
    ftmp += (TACin-cc) / btmp;
    Fatmp = ftmp * partial_F;
    Z_tmp = Fatmp + natmort;
    S_tmp = mfexp( -Z_tmp );
    cc = (wttmp * elem_prod(elem_div(Fatmp,  Z_tmp),elem_prod(1.-S_tmp,N_tmp))); // Catch equation (vectors)
    dd = cc / TACin - 1.;
    //cout << ispp<<" "<< ftmp << " "<< cc << " "<<TACin<<endl; //cout << sel_F << endl << sel_M << endl << endl; //cout << Fratsel_F << endl << Fratsel_M << endl; //exit(1);
    if (dd<0.) dd *= -1.;
    // cout<<"Ftmp "<<cc<<" "<< ftmp<<endl;
    if (icount>100) dd=1e-6;
  }
  return(ftmp);
  */

  
FUNCTION Write_sd
 ofstream sdreport("extra_sd.rep");
 // sdreport << "HM_Fmsyr AM_Fmsyr Avg_3yr_Catch GM_Biom GM_Biom2 ABC_HM OFL_AM ABC_HM2 OFL_AM2 Adjust_1 Adjust_2 "<<endl;
 sdreport << "Quantity "<<endyr+1<<" "<<endyr+2<<endl;
 dvariable cv_b = ABC_biom1.sd / ABC_biom1; 
 dvariable cv_b2= ABC_biom2.sd / ABC_biom2; 


 dvariable gm_b = exp(log(ABC_biom1)-(cv_b*cv_b)/2.);
 dvariable gm_b2= exp(log(ABC_biom2)-(cv_b2*cv_b2)/2.);

 dvariable hm_f = exp(logFmsyr - logFmsyr.sd*logFmsyr.sd /2.);
 dvariable am_f = exp(logFmsyr + logFmsyr.sd*logFmsyr.sd /2.);

 if(SSB_1 < value(Bmsy))
   adj_1 = value((SSB_1/Bmsy - 0.05)/(1.-0.05));
 if(SSB_2 < value(Bmsy))
   adj_2 = value((SSB_2/Bmsy - 0.05)/(1.-0.05));

   sdreport <<"HM_F "
            << hm_f << " "<<hm_f<<endl 
            <<"AM_F "
            << am_f << " "<<am_f<<endl 
            <<"GM_6+Biom "
            << gm_b << " "<<gm_b2<<endl 
            <<"Catch_assumed " 
            // << catch_bio(1,endyr)<<" "<<mean(catch_bio(1)(endyr-3,endyr)) <<endl
            << yr1_futcat <<" "<< yr2_futcat <<endl
            <<"ABC " 
            << gm_b  * hm_f * adj_1         << " " 
            << gm_b2 * hm_f * adj_1         << endl
            <<"OFL " 
            << gm_b  * am_f * adj_1         << " " 
            << gm_b2 * am_f * adj_2         << endl
            <<"SSB " 
            << SSB_1                        << " " 
            << SSB_2                        << endl
            <<"Applied_Adjustments " 
            <<                adj_1         << " " 
            <<                adj_2         << " " <<endl
            <<"Maturity_used "              <<endl
            << maturity(endyr)              <<" "<<endl
            << "wt "<<wt_pop_fut_f          <<endl ;

  sdreport.close();

  // From AMAK 
 /* FUNCTION Oper_Model
 // Initialize things used here only
  // Calc_Dependent_Vars();
  int nsims;
  ifstream sim_in("nsims.dat");
  sim_in >> nsims; sim_in.close();

  dvector ran_srv_vect(1,nsrv);
  ofstream SaveOM("Om_Out.dat",ios::app);
  double C_tmp;
  dvariable Fnow;
  dvariable meanrec;
  meanrec=mean(recruits);
  dvector new_srv(1,nsrv);
  new_srv.initialize();
  dvariable mean5plus;
  mean5plus.initialize();
  for (i=1975;i<=1999;i++)
    mean5plus += natage(i)(5,nages)*wt_fsh(1,i)(5,nages);
  mean5plus /= 25.;
  system("cls"); cout<<"Number of replicates: "<<endl;
  // Initialize recruitment in first year
  nage_future(styr_fut,1) = meanrec;
  nage_future(styr_fut)(2,nages)              = ++elem_prod(natage(endyr)(1,nages-1),S(endyr)(1,nages-1));
  nage_future(styr_fut,nages)                += natage(endyr,nages)*S(endyr,nages);
  for (int isim=1;isim<=nsims;isim++)
  {
    cout<<isim<<" ";
    // Copy file to get mean for Mgt Strategies
    system("init_stuff.bat");
    for (i=styr_fut;i<=endyr_fut;i++)
    {
      // Some unit normals...
      ran_srv_vect.fill_randn(rng);
      // Create new survey observations
      for (k = 1 ; k<= nsrv ; k++)
        new_srv(k) = mfexp(ran_srv_vect(k)*.2)*value(nage_future(i)*q_srv(k)*sel_srv(k,endyr)); // use value function since converts to a double
      // Append new survey observation to datafile
      ofstream srv_out("NewSrv.dat",ios::app);
      srv_out <<i<<" "<< new_srv<<endl; srv_out.close();
      system("ComputeTAC.bat"); // commandline function to get TAC (catchnext.dat)

      // Now read in TAC (actual catch)
      ifstream CatchNext("CatchNext.dat");
      CatchNext >> C_tmp; CatchNext.close();
      Fnow = SolveF2(endyr,nage_future(i), C_tmp);

      F_future(1,i) = sel_fsh(1,endyr) * Fnow;
      Z_future(i)   = F_future(1,i) + natmort;
      S_future(i)   = mfexp(-Z_future(i));
      // nage_future(i,1)  = SRecruit( Sp_Biom_future(i-rec_age) ) * mfexp(rec_dev_future(i)) ;     

      nage_future(i,1)  = meanrec;
      Sp_Biom_future(i) = wt_mature * elem_prod(nage_future(i),pow(S_future(i),spmo_frac)) ;
      // Now graduate for the next year....
      if (i<endyr_fut)
      {
        nage_future(i+1)(2,nages) = ++elem_prod(nage_future(i)(1,nages-1),S_future(i)(1,nages-1));
        nage_future(i+1,nages)   += nage_future(i,nages)*S_future(i,nages);
      }
      catage_future(i) = 0.; 
      for (k = 1 ; k<= nfsh ; k++)
        catage_future(i) += elem_prod(nage_future(i) , elem_prod(F_future(k,i) , elem_div( ( 1.- S_future(i) ) , Z_future(i))));
  
      SaveOM << model_name       <<
        " "  << isim             <<
        " "  << i                <<
        " "  << Fnow             <<
        " "  << nage_future(i)(5,nages)*wt_fsh(1,endyr)(5,nages)/mean5plus <<
        " "  << nage_future(i)(5,nages)*wt_fsh(1,endyr)(5,nages)<<
        " "  << Sp_Biom_future(i)<<
        " "  << catage_future(i)*wt_fsh(1,endyr)<<
        " "  << nage_future(i)<<
        " "  << F_future(1,i)<<
      endl;
    }
  }
  SaveOM.close();
  if (!mceval_phase())
    exit(1);
    */ 
FUNCTION Get_wt_age
  incr_dev.initialize();
  if (active(yr_incr)||active(age_incr)||active(sst_alpha))
  {
    switch (Growth_Option)
    {
      case 0 : // Use constant time-invariant growth (from base growth parameters)
        // No need to do anything since they are initialized at these values...
      break;
      case 1 : // Use empirical (values in data file) mean wts at age
        // No need to do anything since they are initialized at these values...
      break;
      case 2 : // Use base growth values (not estimated) and deviations decomposed by year and age
      {
        // Do it once for all years then apply to each fishery and survey...
        // initialize survey "1" and first year of growth to mean
        Initial_wt();
        for (i=styr+1;i<=endyr;i++)
        {
          // Age component estimated from 5-15
          incr_dev(i)(5,nages-5)     = yr_incr(i) + age_incr;
          incr_dev(i)(nages-4,nages) = incr_dev(i,nages-5) ;
          incr_dev(i)(2,4) = incr_dev(i,5) ;
          wt_srv_f(1,i,1) = wt_srv_f(1,i-1,1);
          wt_srv_m(1,i,1) = wt_srv_m(1,i-1,1);
          wt_srv_f(1,i)(2,nages) = ++wt_srv_f(1,i-1)(1,nages-1) + elem_prod(base_incr_f,mfexp(incr_dev(i))) ;
          wt_srv_m(1,i)(2,nages) = ++wt_srv_m(1,i-1)(1,nages-1) + elem_prod(base_incr_m,mfexp(incr_dev(i))) ;
          wt_pop_f(i) = wt_srv_f(1,i);
          wt_pop_m(i) = wt_srv_m(1,i);
        }
        Get_Pred_wt();
      }
      break;
      case 3 : // Use base growth values (not estimated) and deviations as fn of temperature and // decomposed by year and age
      {
        Initial_wt();
        for (i=styr+1;i<=endyr;i++)
        {
          // Age component estimated from 5-15
          if (i >=1982)
            incr_dev(i)(5,nages-5)     = sst_alpha * sst_anom(i) ; // + age_incr; vestigial
          else 
            incr_dev(i)(5,nages-5)     = 0.;

          incr_dev(i)(nages-4,nages) = incr_dev(i,nages-5) ;
          incr_dev(i)(2,4) = incr_dev(i,5) ;
          wt_srv_f(1,i,1) = wt_srv_f(1,i-1,1);
          wt_srv_m(1,i,1) = wt_srv_m(1,i-1,1);
          wt_srv_f(1,i)(2,nages) = ++wt_srv_f(1,i-1)(1,nages-1) + elem_prod(base_incr_f,mfexp(incr_dev(i))) ;
          wt_srv_m(1,i)(2,nages) = ++wt_srv_m(1,i-1)(1,nages-1) + elem_prod(base_incr_m,mfexp(incr_dev(i))) ;
          wt_pop_f(i) = wt_srv_f(1,i);
          wt_pop_m(i) = wt_srv_m(1,i);
        }
        Get_Pred_wt();
      }
      break;
    }
  }
  
FUNCTION Initial_wt
  wt_srv_f(1,styr) = wt_vbg_f;
  wt_srv_m(1,styr) = wt_vbg_m;
  wt_pop_f(styr)   = wt_vbg_f;
  wt_pop_m(styr)   = wt_vbg_m;

FUNCTION Get_Pred_wt
  // Get predicted weights
  for (i=styr_wt;i<=endyr_wt;i++)
  {
    wt_pred_f(i) = wt_srv_f(1,i);   
    wt_pred_m(i) = wt_srv_m(1,i); 
  }
  // for other surveys set equal to survey 1...
  for (k=2;k<=nsrv;k++)
    for (i=styr;i<=endyr;i++)
    {
      wt_srv_f(k,i) = wt_srv_f(1,i) ;
      wt_srv_m(k,i) = wt_srv_m(1,i) ;
    }
  // set fisheries equal to surveys 
  for (k=1;k<=nfsh;k++)
    for (i=styr;i<=endyr;i++)
    {
      wt_fsh_f(k,i) = wt_srv_f(1,i) ;
      wt_fsh_m(k,i) = wt_srv_m(1,i) ;
    }

FUNCTION Write_R_wts
  R_report<<"$Yr"<<endl; for (i=styr;i<=endyr;i++) R_report<<i<<" "; R_report<<endl;
  R_report<<"$Yr_wt"<<endl; for (i=styr_wt;i<=endyr_wt;i++) R_report<<i<<" "; R_report<<endl;

  R_report<<"$wt_obs_f" <<endl;
  R_report<<  wt_obs_f  <<endl;
  R_report<<"$wt_pred_f"<<endl;
  R_report<<  wt_pred_f <<endl;

  R_report<<"$wt_obs_m" <<endl;
  R_report<<  wt_obs_m  <<endl;
  R_report<<"$wt_pred_m"<<endl;
  R_report<< wt_pred_m  <<endl;

  R_report<<"$wt_srv_f" <<endl;
  R_report<<  wt_srv_f  <<endl;

  R_report<<"$wt_srv_m" <<endl;
  R_report<<  wt_srv_m  <<endl;

  R_report<<"$wt_like"  <<endl;
  R_report <<  wt_like(1)<<endl;

FUNCTION Write_R
  Write_R_wts();
  // R_report<<"$TotF"<<endl << Ftot<<endl;
  /* R_report<<"$TotBiom_NoFish"<<endl; for (i=styr;i<=endyr;i++) 
  {
    double lb=value(TotBiom_NoFish(i)/exp(2.*sqrt(log(1+square(TotBiom_NoFish.sd(i))/square(TotBiom_NoFish(i))))));
    double ub=value(TotBiom_NoFish(i)*exp(2.*sqrt(log(1+square(TotBiom_NoFish.sd(i))/square(TotBiom_NoFish(i))))));
    R_report<<i<<" "<<TotBiom_NoFish(i)<<" "<<TotBiom_NoFish.sd(i)<<" "<<lb<<" "<<ub<<endl;
  }
  R_report<<"$SSB_NoFishR"<<endl; for (i=styr;i<=endyr;i++) 
  {
    double lb=value(Sp_Biom_NoFishR(i)/exp(2.*sqrt(log(1+square(Sp_Biom_NoFishR.sd(i))/square(Sp_Biom_NoFishR(i))))));
    double ub=value(Sp_Biom_NoFishR(i)*exp(2.*sqrt(log(1+square(Sp_Biom_NoFishR.sd(i))/square(Sp_Biom_NoFishR(i))))));
    R_report<<i<<" "<<Sp_Biom_NoFishR(i)<<" "<<Sp_Biom_NoFishR.sd(i)<<" "<<lb<<" "<<ub<<endl;
  }
  */ 


  R_report<<"$SSB"<<endl; for (i=styr;i<=endyr;i++) 
  {
    double lb=value(SSB(i)/exp(2.*sqrt(log(1+square(SSB.sd(i))/square(SSB(i))))));
    double ub=value(SSB(i)*exp(2.*sqrt(log(1+square(SSB.sd(i))/square(SSB(i))))));
    R_report<<i<<" "<<SSB(i)<<" "<<SSB.sd(i)<<" "<<lb<<" "<<ub<<endl;
  }
  R_report<<"$TotBiom"<<endl; 
  for (i=styr;i<=endyr;i++) 
  {
    double lb=value(TotBiom(i)/exp(2.*sqrt(log(1+square(TotBiom.sd(i))/square(TotBiom(i))))));
    double ub=value(TotBiom(i)*exp(2.*sqrt(log(1+square(TotBiom.sd(i))/square(TotBiom(i))))));
    R_report<<i<<" "<<TotBiom(i)<<" "<<TotBiom.sd(i)<<" "<<lb<<" "<<ub<<endl;
  }

  /* for (int k=1;k<=5;k++){
    R_report<<"$SSB_fut_"<<k<<endl; 
    for (i=styr_fut;i<=endyr_fut;i++) 
    {
      double lb=value(future_biomass(k,i)/exp(2.*sqrt(log(1+square(future_biomass.sd(k,i))/square(future_biomass(k,i))))));
      double ub=value(future_biomass(k,i)*exp(2.*sqrt(log(1+square(future_biomass.sd(k,i))/square(future_biomass(k,i))))));
      R_report<<i<<" "<<future_biomass(k,i)<<" "<<future_biomass.sd(k,i)<<" "<<lb<<" "<<ub<<endl;
    }
  }
  double ctmp;
  for (int k=1;k<=5;k++){
    R_report<<"$Catch_fut_"<<k<<endl; 
    for (i=styr_fut;i<=endyr_fut;i++) 
    {
      if (k==5) ctmp=0.;else ctmp=value(catch_future(k,i));
      R_report<<i<<" "<<ctmp<<endl;
    }
  }
  R_report<<"$R"<<endl; for (i=styr;i<=endyr;i++) 
  {
    double lb=value(recruits(i)/exp(2.*sqrt(log(1+square(recruits.sd(i))/square(recruits(i))))));
    double ub=value(recruits(i)*exp(2.*sqrt(log(1+square(recruits.sd(i))/square(recruits(i))))));
    R_report<<i<<" "<<recruits(i)<<" "<<recruits.sd(i)<<" "<<lb<<" "<<ub<<endl;
  }
    R_report << "$N"<<endl;
    for (i=styr;i<=endyr;i++) 
      R_report <<   i << " "<< natage(i) << endl;
      R_report   << endl;

    for (int k=1;k<=nfsh;k++)
    {
      R_report << "$F_age_"<< (k) <<""<< endl ;
      for (i=styr;i<=endyr;i++) 
        R_report <<i<<" "<<F(k,i)<<" "<< endl;
        R_report   << endl;
    }

    R_report <<endl<< "$Fshry_names"<< endl;
    for (int k=1;k<=nfsh;k++)
      R_report << fshname(k) << endl ;

    R_report <<endl<< "$Index_names"<< endl;
    for (int k=1;k<=nsrv;k++)
      R_report << srvname(k) << endl ;

    for (int k=1;k<=nsrv;k++)
    {
      int ii=1;
      R_report <<endl<< "$Obs_Survey_"<< k <<""<< endl ;
      for (i=styr;i<=endyr;i++)
      {
        if (ii<=yrs_srv(k).indexmax())
        {
          if (yrs_srv(k,ii)==i)
          {
            R_report << i<< " "<< obs_srv(k,ii) << " "<< pred_srv(k,i) <<" "<< obs_se_srv(k,ii) <<endl; //values of survey index value (annual)
            ii++;
          }
          else
            R_report << i<< " -1 "<< " "<< pred_srv(k,i)<<" -1 "<<endl;
        }
        else
          R_report << i<< " -1 "<< " "<< pred_srv(k,i)<<" -1 "<<endl;
      }
      R_report   << endl;
    }

    R_report << endl<< "$Survey_Q"<<endl;
    R_report<< q_srv << endl;
    R_report   << endl;
    for (int k=1;k<=nfsh;k++)
    {
      if (nyrs_fsh_age(k)>0) 
      { 
        R_report << "$pobs_fsh_"<< (k) <<""<< endl;
        for (i=1;i<=nyrs_fsh_age(k);i++) 
          R_report << yrs_fsh_age(k,i)<< " "<< oac_fsh(k,i) << endl;
        R_report   << endl;
      }
    }
    for (int k=1;k<=nfsh;k++)
    {
      if (nyrs_fsh_age(k)>0) 
      { 
        R_report << "$phat_fsh_"<< (k) <<""<< endl;
        for (i=1;i<=nyrs_fsh_age(k);i++) 
          R_report << yrs_fsh_age(k,i)<< " "<< eac_fsh(k,i) << endl;
          R_report   << endl;
      }
    }
    for (int k=1;k<=nsrv;k++)
    {
      if (nyrs_srv_age(k)>0) 
      { 
        R_report << "$pobs_srv_"<<(k)<<""<<  endl;
        for (i=1;i<=nyrs_srv_age(k);i++) 
          R_report << yrs_srv_age(k,i)<< " "<< oac_srv(k,i) << endl;
          R_report   << endl;
      }
    }
    for (int k=1;k<=nsrv;k++)
    {
      if (nyrs_srv_age(k)>0) 
      { 
        R_report << "$phat_srv_"<<(k)<<""<<  endl;
        for (i=1;i<=nyrs_srv_age(k);i++) 
          R_report << yrs_srv_age(k,i)<< " "<< eac_srv(k,i) << endl;
          R_report   << endl;
      }
    }
    for (int k=1;k<=nfsh;k++)
    {
      R_report << endl<< "$Obs_catch_"<<(k) << endl;
      R_report << catch_bio(k) << endl;
      R_report   << endl;
      R_report << "$Pred_catch_" <<(k) << endl;
      R_report << pred_catch(k) << endl;
      R_report   << endl;
    }

    for (int k=1;k<=nfsh;k++)
    {
      R_report << "$F_fsh_"<<(k)<<" "<<endl;
      for (i=styr;i<=endyr;i++)
      {
        R_report<< i<< " ";
        R_report<< mean(F(k,i)) <<" "<< mean(F(k,i))*max(sel_fsh(k,i)) << " ";
        R_report<< endl;
      }
    }

    for (int k=1;k<=nfsh;k++)
    {
      R_report << endl<< "$sel_fsh_"<<(k)<<"" << endl;
      for (i=styr;i<=endyr;i++)
        R_report << k <<"  "<< i<<" "<<sel_fsh(k,i) << endl; 
      R_report   << endl;
    }

    for (int k=1;k<=nsrv;k++)
    {
      R_report << endl<< "$sel_srv_"<<(k)<<"" << endl;
      for (i=styr;i<=endyr;i++)
        R_report << k <<"  "<< i<<" "<<sel_srv(k,i) << endl;
        R_report << endl;

    }
    R_report << endl<< "$Stock_Rec"<< endl;
    for (i=styr_rec;i<=endyr;i++)
      if (active(log_Rzero))
        R_report << i<< " "<<Sp_Biom(i-rec_age)<< " "<< SRecruit(Sp_Biom(i-rec_age))<< " "<< mod_rec(i)<<endl;
      else 
        R_report << i<< " "<<Sp_Biom(i-rec_age)<< " "<< " 999" << " "<< mod_rec(i)<<endl;
        
        R_report   << endl;

    R_report <<"$stock_Rec_Curve"<<endl;
    R_report <<"0 0"<<endl;
    dvariable stock;
    for (i=1;i<=30;i++)
    {
      stock = double (i) * Bzero /25.;
      if (active(log_Rzero))
        R_report << stock <<" "<< SRecruit(stock)<<endl;
      else
        R_report << stock <<" 99 "<<endl;
    }
    R_report   << endl;

    R_report   << endl<<"$Like_Comp" <<endl;
    obj_comps(11)= obj_fun - sum(obj_comps) ; // Residual
    obj_comps(12)= obj_fun ;

    R_report   <<obj_comps<<endl;
    R_report   << endl;
    R_report   << endl<<"$Like_Comp_names" <<endl;
    R_report   <<"catch_like     "<<endl
             <<"age_like_fsh     "<<endl
             <<"sel_like_fsh     "<<endl
             <<"surv_like        "<<endl
             <<"age_like_srv     "<<endl
             <<"sel_like_srv     "<<endl
             <<"rec_like         "<<endl
             <<"fpen             "<<endl
             <<"post_priors_srvq "<<endl
             <<"post_priors      "<<endl
             <<"residual         "<<endl
             <<"total            "<<endl;
    for (int k=1;k<=nfsh;k++)
    {
      R_report << "$Sel_Fshry_"<< (k) <<""<<endl;
      R_report << sel_like_fsh(k) << endl;
    }
    R_report   << endl;
  
    for (int k=1;k<=nsrv;k++)
    {
      R_report << "$Survey_Index_"<< (k) <<"" <<endl;
      R_report<< surv_like(k)<<endl;
    }
    R_report   << endl;

    R_report << setw(10)<< setfixed() << setprecision(5) <<endl;
    for (int k=1;k<=nsrv;k++)
    {
      R_report << "$Age_Survey_"<< (k) <<"" <<endl;
      R_report << age_like_srv(k)<<endl;
    }
    R_report   << endl;

    for (int k=1;k<=nsrv;k++)
    {
      R_report << "$Sel_Survey_"<< (k) <<""<<endl;
      R_report<< sel_like_srv(k,1) <<" "<<sel_like_srv(k,2)<<" "<<sel_like_srv(k,3)<< endl;
    }
    R_report   << endl;

    R_report << setw(10)<< setfixed() << setprecision(5) <<endl;
    R_report   << "$Rec_Pen" <<endl<<sigmar<<"  "<<rec_like<<endl;
    R_report   << endl;

    R_report   << "$F_Pen" <<endl;
    R_report<<fpen(1)<<"  "<<fpen(2)<<endl;
    R_report   << endl;
    for (int k=1;k<=nsrv;k++)
    {
      R_report << "$Q_Survey_"<< (k) <<""<<endl
             << " "<<post_priors_srvq(k)
             << " "<< q_srv(k)
             << " "<< qprior(k)
             << " "<< cvqprior(k)<<endl;
      R_report << "$Q_power_Survey_"<< (k) <<""<<endl
             << " "<<post_priors_srvq(k)
             << " "<< q_power_srv(k)
             << " "<< q_power_prior(k)
             << " "<< cvq_power_prior(k)<<endl;
    }
             R_report   << endl;
    R_report << "$M"<<endl;
    R_report << " "<< post_priors(1)
             << " "<< M
             << " "<< natmortprior
             << " "<< cvnatmortprior <<endl;
    R_report   << endl;
    R_report << "$Steep"<<endl;
    R_report << " "<< post_priors(2)
             << " "<< steepness
             << " "<< steepnessprior
             << " "<< cvsteepnessprior <<endl;
    R_report   << endl;
    R_report << "$Sigmar"<<endl;
    R_report << " "<< post_priors(3)
             << " "<< sigmar
             << " "<< sigmarprior
             << " "<< cvsigmarprior <<endl;
    R_report   << endl;
    R_report<<"$Num_parameters_Est"<<endl;
    R_report<<initial_params::nvarcalc()<<endl;
    R_report   << endl;
    
  R_report<<"$Steep_Prior" <<endl;
  R_report<<steepnessprior<<" "<<
    cvsteepnessprior<<" "<<
    phase_srec<<" "<< endl;
    R_report   << endl;

  R_report<<"$sigmarPrior " <<endl;
  R_report<<sigmarprior<<" "<<  cvsigmarprior <<" "<<phase_sigmar<<endl;
  R_report   << endl;

  R_report<<"$Rec_estimated_in_styr_endyr " <<endl;
  R_report<<styr_rec    <<" "<<endyr        <<" "<<endl;
  R_report   << endl;
  R_report<<"$SR_Curve_fit__in_styr_endyr " <<endl;
  R_report<<styr_rec_est<<" "<<endyr_rec_est<<" "<<endl;
  R_report   << endl;
  R_report<<"$Model_styr_endyr" <<endl;
  R_report<<styr        <<" "<<endyr        <<" "<<endl;
  R_report   << endl;

  R_report<<"$M_prior "<<endl;
  R_report<< natmortprior<< " "<< cvnatmortprior<<" "<<phase_M<<endl;
  R_report   << endl;
  R_report<<"$qprior " <<endl;
  R_report<< qprior<<" "<<cvqprior<<" "<< phase_q<<endl;
  R_report<<"$q_power_prior " <<endl;
  R_report<< q_power_prior<<" "<<cvq_power_prior<<" "<< phase_q_power<<endl;
  R_report   << endl;

  R_report<<"$cv_catchbiomass " <<endl;
  R_report<<cv_catchbiomass<<" "<<endl;
  R_report   << endl;
  R_report<<"$Projection_years"<<endl;
  R_report<< nproj_yrs<<endl;
  R_report   << endl;
  
  R_report << "$Fsh_sel_opt_fish "<<endl;
  for (int k=1;k<=nfsh;k++)
    R_report<<k<<" "<<fsh_sel_opt(k)<<" "<<sel_change_in_fsh(k)<<endl;
    R_report   << endl;
   R_report<<"$Survey_Sel_Opt_Survey " <<endl;
  for (int k=1;k<=nsrv;k++)
  R_report<<k<<" "<<(srv_sel_opt(k))<<endl;
  R_report   << endl;
    
  R_report <<"$Phase_survey_Sel_Coffs "<<endl;
  R_report <<phase_selcoff_srv<<endl;
  R_report   << endl;
  R_report <<"$Fshry_Selages " << endl;
  R_report << nselages_in_fsh  <<endl;
  R_report   << endl;
  R_report <<"$Survy_Selages " <<endl;
  R_report <<nselages_in_srv <<endl;
  R_report   << endl;

  R_report << "$Phase_for_age_spec_fishery"<<endl;
  R_report <<phase_selcoff_fsh<<endl;
  R_report   << endl;
  R_report << "$Phase_for_logistic_fishery"<<endl;
  R_report <<phase_logist_fsh<<endl;
  R_report   << endl;
  R_report << "$Phase_for_dble_logistic_fishery "<<endl;
  R_report <<phase_dlogist_fsh<<endl;
  R_report   << endl;

  R_report << "$Phase_for_age_spec_survey  "<<endl;
  R_report <<phase_selcoff_srv<<endl;
  R_report   << endl;
  R_report << "$Phase_for_logistic_survey  "<<endl;
  R_report <<phase_logist_srv<<endl;
  R_report   << endl;
  R_report << "$Phase_for_dble_logistic_srvy "<<endl;
  R_report <<phase_dlogist_srv<<endl;
  R_report   << endl;
  
  for (int k=1;k<=nfsh;k++)
  {
    if (nyrs_fsh_age(k)>0)
    {
      R_report <<"$EffN_Fsh_"<<(k)<<""<<endl;
      for (i=1;i<=nyrs_fsh_age(k);i++)
      {
        double sda_tmp = Sd_age(oac_fsh(k,i));
        R_report << yrs_fsh_age(k,i);
        R_report<< " "<<Eff_N(oac_fsh(k,i),eac_fsh(k,i)) ;
        R_report << " "<<Eff_N2(oac_fsh(k,i),eac_fsh(k,i));
        R_report << " "<<mn_age(oac_fsh(k,i));
        R_report << " "<<mn_age(eac_fsh(k,i));
        R_report << " "<<sda_tmp;
        R_report << " "<<mn_age(oac_fsh(k,i)) - sda_tmp *2. / sqrt(n_sample_fsh_age(k,i));
        R_report << " "<<mn_age(oac_fsh(k,i)) + sda_tmp *2. / sqrt(n_sample_fsh_age(k,i));
        R_report <<endl;
      }
    }
  }

  for (int k=1;k<=nfsh;k++)
  {
    R_report <<"$C_fsh_" <<(k)<<"" << endl; 
    for (i=styr;i<=endyr;i++)
      R_report <<i<<" "<<catage(k,i)<< endl;
  }

  R_report <<"$wt_a_pop" << endl<< wt_pop  <<endl;
  R_report <<"$mature_a" << endl<< maturity<<endl;
  for (int k=1;k<=nfsh;k++)
  {
    R_report <<"$wt_fsh_"<<(k)<<""<<endl;
    for (i=styr;i<=endyr;i++)
      R_report <<i<<" "<<wt_fsh(k,i)<< endl;
  }
  
  for (int k=1;k<=nsrv;k++)
  {
    R_report <<"$wt_srv_"<<(k)<<""<<endl;
    for (i=styr;i<=endyr;i++)
      R_report <<i<<" "<<wt_srv(k,i)<< endl;
  }
  for (int k=1;k<=nsrv;k++)
  {
    if (nyrs_srv_age(k)>0)
    {
      R_report <<"$EffN_Survey_"<<(k)<<""<<endl;
      for (i=1;i<=nyrs_srv_age(k);i++)
      {
        double sda_tmp = Sd_age(oac_srv(k,i));
        R_report << yrs_srv_age(k,i)
                << " "<<Eff_N(oac_srv(k,i),eac_srv(k,i)) 
                 << " "<<Eff_N2(oac_srv(k,i),eac_srv(k,i))
                 << " "<<mn_age(oac_srv(k,i))
                 << " "<<mn_age(eac_srv(k,i))
                 << " "<<sda_tmp
                 << " "<<mn_age(oac_srv(k,i)) - sda_tmp *2. / sqrt(n_sample_srv_age(k,i))
                 << " "<<mn_age(oac_srv(k,i)) + sda_tmp *2. / sqrt(n_sample_srv_age(k,i))
                 <<endl;
      }
    }
  }
  */ 

  R_report.close();

FINAL_SECTION
  if (!do_wt_only)
  {
    Write_sd();
    Write_R();
  }
  else
    Write_R_wts();
  /*
		*/

