module globals
    use parameters
    implicit none
    save

    !prices
    real(dbl) r !interest rate
    real(dbl):: w !wage per efficiency unit of labor    
        
    !government spending  
    real(dbl) GovSpending
    real(dbl):: PropTax
    real(dbl) EarnsTax, EarnsTaxNew
    real(dbl):: GovSpendingBaseline= 0.05639641d0 !baseline: 0.10635448d0 !No HealthExp 0.10704468d0 !No HealthExp 1/2cfloors: 0.11099796d0 !No HealthNoMedicaidBaseline: 0.11146004d0 
        
    !asset grid
    real(dbl) avect(na), lnavect(na-1)
    real(dbl) amin, amax, lnamin, lnamax
    
    !earnings grid
    real(dbl) efmat(nef,nem,nset,nw),  emmat(nef,nem,nset,nw)
    !Puefmat is setup as follows:
    ! Prob(u'=u_1|u=u_1) Prob(u'=u_1|u=u_2) Prob(u'=u_1|u=u_3)
    ! Prob(u'=u_2|u=u_1) Prob(u'=u_2|u=u_2) Prob(u'=u_2|u=u_3)
    ! Prob(u'=u_3|u=u_1) Prob(u'=u_3|u=u_2) Prob(u'=u_3|u=u_3)
    !Hence Puefmat(i,j) = Prob(u'=u_i|u=u_j)
    real(dbl):: A = 1.20881959d0 !A is set such that w (wage per efficiency unit) is normalized to 1 !baseline  1.18257703d0 !No Medicaid/Health Exp
                                 !No HealthExp 1.1942298d0
    !real(dbl):: ufinit(nef) = (/0.05516671d0,	0.527020673d0,	0.257812617d0,	0.13d0, 0.03d0/) 
    !real(dbl):: uminit(nem) = (/0.05516671d0,	0.527020673d0,	0.257812617d0,	0.13d0, 0.03d0/) 
    real(dbl) uefgrid(nef), uinit(nef,nem)
    real(dbl) uemgrid(nem), corrInitial, sigmaInitial(2) 
    real(dbl) Puefmat(nef,nef), Puemmat(nem,nem)  
    real(dbl) Cmat(2,2)  !linear relationship between shocks and earnings      

    !average earnings grid
    real(dbl) aveEarnFvect(naef), logaveEarnFvect(naef-4)
    real(dbl) aveEarnMvect(naem), logaveEarnMvect(naem-4)
    real(dbl) aveEarnMin, aveEarnMax, socseckinks(3,2), AveMaleEarnings, fracWwork, AveEarningsAll, AveHHIncome, AveEarningsAllNew
    real(dbl) AveEarnsM65dist(naem)
    
    !medical expenses grid
    real(dbl) betam(20)   
    real(dbl) mmat(nm,nhht,4,3,nr), lnmmat(nm,nhht,4, 3,nr)
    !PTM's for each state: 1-HH, 2-FF, 3-MM, 4-HF, 5-HM
    real(dbl) Pummat(npm,npm,nr-1),  minit(npm,nhet), Ptmvect(ntm)
    real(dbl) MedExpDistMarried(nm,nhht,nr), MedExpDistWidow(nm,nsht,nr), MedExpDistWidower(nm,nsht,nr), autocorrMedExp
    real(dbl) umgrid(npm,3), utmgrid(ntm)
                 
    !health transition matrices
    real(dbl) betahF(7), betahM(7)  !constant, age, age*age, married, married*age, lagged health, lagged health*age    
    real(dbl):: PhmatF(nsht, nsht, 3, nr), PhmatM(nsht, nsht, 3, nr), hinitH(nhht,nhet), hinitF(nsht,nset), hinitM(nsht,nset)
        
    !survival probabilities                                         
    real(dbl) :: survivalprobVectF(nsht,3,nr-1), betasF(7)
    real(dbl) :: survivalprobVectM(nsht,3,nr-1), betasM(7)    
    
    !education distribution  !Not using this: !HHs of all ages from 20 - 59 in 1970. Table 4.b, page 25, in Marriage and Assortative Mating: How Have the Patterns Changed?,
    ! Elaina Rose, 2001
    !Using HRS for households 65-66 years of age.
    !Marital distribution and initial health distributions are also for HRS 65-66 year-olds. See InitialDists.do
    !real(dbl):: phi(nhet)  = (/2.5d0, 2.5d0, 2.5d0, 2.5d0/) !(/0.65d0, 0.65d0, 0.65d0, 0.65d0/) !(/1.3d0, 1.3d0, 1.3d0, 1.3d0/) !weight on disutility from working
    real(dbl):: phi1(nhet)  = (/0.29d0, 0.16d0, 0.11d0, 0.10d0/)
    real(dbl):: psi(nhet) = (/3.2d0, 1.6d0, 2.4d0, 1.7d0/) 
    !real(dbl):: phi1(nhet)  = (/0.0d0, 0.0d0, 0.0d0, 0.0d0/) !0.24d0
    !real(dbl):: psi(nhet) = (/0.0d0, 0.0d0, 0.0d0, 0.0d0/)   !2.73d0
    !real(dbl):: phi1(nhet)  = (/0.24d0, 0.24d0, 0.24d0, 0.24d0/) !0.24d0
    !real(dbl):: psi(nhet) = (/2.73d0, 2.73d0, 2.73d0, 2.73d0/)   !2.73d0         
    real(dbl):: psiR = 2.73d0 !2.73d0
    real(dbl):: educDist(nhet) = (/0.67d0, 0.14d0, 0.05d0, 0.14d0/)     !HH, HC, CH, CC                                              
    real(dbl):: sinitH(3,naem) ! = (/0.56d0, 0.29d0, 0.15d0/)
    real(dbl):: sinitHuncond(3) = (/0.57d0, 0.30d0, 0.13d0/)
    !real(dbl) btran(4)
    
    !optimal value functions and policies
    real(dbl) VnewWCube(na,nef,nem,naef,naem,nhet,nw), aPolicyWCube(na,nef,nem,naef,naem,nhet,nw)
    real(dbl) consumptionWCube(na,nef,nem,naef,naem,nhet,nw)
    real(dbl) labWCube(na,nef,nem,naef,naem,nhet,nw)
    real(dbl) VnewRCubeMarried(na,nm,naef,naem,nhht,4,nr), aPolicyRCubeMarried(na,nm,naef,naem,nhht,4,nr) 
    real(dbl) consumptionRCubeMarried(na,nm,naef,naem,nhht,4,nr)  
    real(dbl) VnewRCubeWidow(na,nm,naef,naem,nsht,2,nr), aPolicyRCubeWidow(na,nm,naef,naem,nsht,2,nr)
    real(dbl) consumptionRCubeWidow(na,nm,naef,naem,nsht,2,nr)
    real(dbl) VnewRCubeWidower(na,nm,naef,naem,nsht,2,nr), aPolicyRCubeWidower(na,nm,naef,naem,nsht,2,nr)
    real(dbl) consumptionRCubeWidower(na,nm,naef,naem,nsht,2,nr)
    real(dbl) UtilConsWCube(na,nef,nem,naef,naem,nhet,nw), UtilConsRCubeMarried(na,nm,naef,naem,nhht,4,nr)
    real(dbl) UtilConsRCubeWidow(na,nm,naef,naem,nsht,2,nr), UtilConsRCubeWidower(na,nm,naef,naem,nsht,2,nr)
    
    !invariant distributions
    real(dbl) psiW(na,nef,nem,naef,naem,nhet,nw) !fraction of working indiviuals in steady state
    real(dbl) psiRMarried(na,nm,naef,naem,nhht,4,nr), psiRWidow(na,nm,naef,naem,nsht,2,nr), psiRWidower(na,nm,naef,naem,nsht,2,nr) !fraction of alive retired individuals in steady state
    real(dbl) ProdDist(nef,nem,nw), etaMcube(nef,nem,naem,nset,nw+1), dist(nhht,3,nr), ProdDistW(nef,nem,naem,nhet,nw+1)
    integer EarnLocsMcube(nef,nem,naem,nset,nw+1)
    
    !results
    
    !Computed Ex Ante
    real(dbl) MaritalDist(3,nr), Frac65plus 
    real(dbl) AveFemaleHours, AveFemaleHoursByAge(nw), ConsByAgeID(T), AveFemaleHoursCondWork, AveFemaleHoursCondWorkByAge(nw)
    real(dbl) ExpAveLifetimeProdf(2),ExpAveLifetimeProdm(2)
    real(dbl) distNHentryage(3), aveyearsNH(3)
    real(dbl) Frac65plusbs, Frac6574bs, Frac7584bs, Frac85plusbs
    real(dbl) MedDistSmall(nr,npm), fracNH
    
    !Aggregates
    real(dbl) AggLaborSupply, AggLaborDemand, AggMedExp, AggOOPExp, AveEarningsInit, TotalTaxableIncome
    real(dbl) AveFrischElasticityM, AveFrischElasticityF, AveFrischElasticityFW, AveFemaleEarnings, AveHHEarnings
    real(dbl) AggCons, AggOutput, AggPretaxInc, TotalPayments, NumOldPeople   
    real(dbl) MeanOOPExp, MeanOOPExpCond, Frac65plusMedicaid, Frac6574Medicaid, Frac7584Medicaid, Frac85plusMedicaid, CapitalTaxes     
    real(dbl) FracAggMedExpOOP, OOPOverOutput, MedicaidPaymentsOverOutput, AggMedExpOverOutput, MeanOOPExpMedicaid
    real(dbl) TaxRateAveInc, TaxRateAveWrkInc, FracWomenLaborForce, AggWorkersAssets, AggRetireesAssets
    real(dbl) GovTransfersTo65Plus, MedicaidTransfers, FracIndRetsGovtTrans, FracIndWorksGovtTrans
    real(dbl) OOPExpOverOutputByAge(3), MedicaidExpOverOutputByAge(3)
    real(dbl) corrWageMHoursF, corrWageFM, corrWageMHoursFx, corrWageFMx 
    real(dbl) Frac65plusHHTopMedShock, Frac6574HHTopMedShock, Frac7584HHTopMedShock, Frac85plusHHTopMedShock
    real(dbl) AveYearsAliveF, AveYearsAliveM, AveYearsAliveF2, AveYearsAliveM2, MedicareRevenue, AddGovConsumption
    real(dbl) AggConsAtDeath, AggConsExcludeDeath, AggNHExp, AggConsNHResidents

    real(dbl) FracIndMedicaid, Frac6574IndMedicaid, Frac7584IndMedicaid
    real(dbl) Frac85plusIndMedicaid, FracIndByMaritalStatusAge(3,nr)
    real(dbl) FracWidowsMedicaid, Frac6566WidowsMedicaid, Frac6574WidowsMedicaid
    real(dbl) Frac7584WidowsMedicaid, Frac85plusWidowsMedicaid
    real(dbl) FracWidowersMedicaid, Frac6566WidowersMedicaid
    real(dbl) Frac6574WidowersMedicaid, Frac7584WidowersMedicaid, Frac85plusWidowersMedicaid
    real(dbl) FracMarriedMedicaid, Frac6566MarriedMedicaid, Frac6574MarriedMedicaid
    real(dbl) Frac7584MarriedMedicaid, Frac85plusMarriedMedicaid    
    
    !Distributions
    real(dbl) OOPExpGini, OOPmedShareUpper(3), OOPmedShare(5)    
    real(dbl) OOPExpGiniYoung, OOPmedShareUpperYoung(3), OOPmedShareYoung(5) 
    real(dbl) OOPExpGini6574, OOPmedShareUpper6574(3), OOPmedShare6574(5)   
    real(dbl) OOPExpGini7584, OOPmedShareUpper7584(3), OOPmedShare7584(5)   
    real(dbl) OOPExpGini85plus, OOPmedShareUpper85plus(3), OOPmedShare85plus(5)  
    real(dbl) WealthGini, WealthSharesUpper(3), WealthShares(5), XwealthVect(na)
    real(dbl) WealthGiniWorkingHHs, WealthGini6566
    real(dbl) EarnsGini,EarnsShares(5), EarnsSharesUpper(3)
    real(dbl) LifetimeEarnsGini6566,LifetimeEarnsShares(5), LifetimeEarnsSharesUpper(3)
    real(dbl) SocSecIncGini6566,SocSecIncShares6566(5), SocSecIncSharesUpper6566(3)    
    real(dbl) SocSecIncGini,SocSecIncShares(5), SocSecIncSharesUpper(3)    
    real(dbl) Frac65plusShares(5), Frac65plusVect(na)     
    real(dbl) MedMobMat(5,5), OOPMobMat(5,5), OOPMobMat85plus(5,5), MedMobMat85plus(5,5)
    real(dbl) WealthMobMatHH2yr(5,5), WealthMobMatHH2yr2164(5,5), WealthMobMatHH2yr65plus(5,5)
    real(dbl) WealthMobMatHH2yr6574(5,5), WealthMobMatHH2yr7584(5,5), WealthMobMatHH2yr85plus(5,5)
    !real(dbl) WealthMobMatHH4yr65plus(5,5), WealthMobMatHH4yr6574(5,5), WealthMobMatHH4yr7584(5,5), WealthMobMatHH4yr85plus(5,5)
    !real(dbl) WealthMobMatHH6yr65plus(5,5), WealthMobMatHH6yr6574(5,5), WealthMobMatHH6yr7584(5,5), WealthMobMatHH6yr85plus(5,5)
    !real(dbl) WealthMobMatHH8yr65plus(5,5), WealthMobMatHH8yr6574(5,5), WealthMobMatHH8yr7584(5,5), WealthMobMatHH8yr85plus(5,5)
    real(dbl) WealthMobMatHH2yr8594(5,5)
    real(dbl) WealthMobMatCondNH6574(10,10), WealthMobMatCondNH7584(10,10), WealthMobMatCondNH85plus(10,10)
    real(dbl) WealthMobMatCondHosp6574(10,10), WealthMobMatCondHosp7584(10,10), WealthMobMatCondHosp85plus(10,10)
    real(dbl) WealthMobMatCondBadHeal6574(10,10), WealthMobMatCondBadHeal7584(10,10), WealthMobMatCondBadHeal85plus(10,10)   
    real(dbl) WealthMobMatCondWidow6574(10,10), WealthMobMatCondWidow7584(10,10), WealthMobMatCondWidow85plus(10,10) 
    real(dbl) WealthMobMatCondWidower6574(10,10), WealthMobMatCondWidower7584(10,10), WealthMobMatCondWidower85plus(10,10) 
    real(dbl) WealthMobMatCondSpousalDeath6574(10,10), WealthMobMatCondSpousalDeath7584(10,10)
    real(dbl) WealthMobMatCondSpousalDeath85plus(10,10) 
    real(dbl) WealthMobMatInd2yr6574(5,5), WealthMobMatInd2yr7584(5,5), WealthMobMatInd2yr85plus(5,5)
    real(dbl) WealthMobMatHH2yr65plusCondPEQ(5,5,5)
    
    
    !statistics by age
    real(dbl) WealthByAge(T), ConsByAge(T), SocSecByAge(nr), EarnsFbyAge(nw)
    real(dbl) EarnsMbyAge(nw), EarnsFbyAgeID(nw), EarnsMbyAgeID(nw)
    real(dbl) WealthByAgeID2(T), AveUtilByAge(T), AveUtilConsByAge(T), AveUtilByWidAge(nr), FracWomenLabForByAge(nw)
    real(dbl) FracWidowsMedicaidByAge(nr), FracWidowersMedicaidByAge(nr), FracMarriedMedicaidByAge(nr)
    real(dbl) MeanOOPExpsMedicaidByAge(nr)
    real(dbl) GovTransByAge(T), GovTransByAgeCond(T), SocSecByAgeID(nr), AveEarnsFbyAge(nw+1)
    real(dbl) AveEarnsMbyAge(nw+1), AveEarnsFbyAgeID(nw+1), AveEarnsMbyAgeID(nw+1)
    real(dbl) AveUtilByAgeID(T), AveUtilConsByAgeID(T), AveUtilByEducID(nhet)
    real(dbl) AveUtilConsByEducID(nhet), FracIndMedicaidByAge(nr), NumOldPeopleByAge(nr)              
    real(dbl) AveUtilFWidbyAgeWid(nr), AveUtilMWidbyAgeWid(nr), FracIndGovtTransByAgeID(T)
    real(dbl) OOPExpGiniByAge(nr), FracHHTopMedShockByAge(nr)
    real(dbl) FracHHEnterMedicaid(3), FracIndEnterMedicaid(3), FracMarriedEnterMedicaid(3)
    real(dbl) FracWidowsEnterMedicaid(3), FracWidowersEnterMedicaid(3)
    real(dbl) FracNHMedicaid(nr), MedExpsNHMedicaid(nr), ConsNHMedicaid(nr) 
    real(dbl) FracNHPrivate(nr), MedExpsNHPrivate(nr), ConsNHPrivate(nr) 
       
    !Medical expenditures by age
    real(dbl) TotMedByAge(nr), TotMedByAgeID(nr), FracMedicaidByAge(nr)
    real(dbl) FracAtMaxAvectByAge(T), FracAtSecMaxAvectByAge(T)         
    !OOP expenditures by age
    real(dbl) OOPMedByAge(nr), MeanOOPExpByAge(nr), MeanOOPExpByAgeCond(nr), FracPosOOPByAge(nr)
    real(dbl) AveNHExpensesWidows(nr), AveNonNHExpensesWidows(nr), AveExpensesWidows(nr) !just of widows
    real(dbl) AveNHExpensesWidowers(nr), AveNonNHExpensesWidowers(nr), AveExpensesWidowers(nr)
    real(dbl) AveNHExpensesMarried(nr), AveNonNHExpensesMarried(nr), AveExpensesMarried(nr) 
    
    !Age and Male PE Quintiles
    real(dbl) WealthByAgePEM(T,5), ConsByAgePEM(T,5), UtilByAgePEM(T,5), UtilConsByAgePEM(T,5)
    real(dbl) EarnsMByAgePEM(nw,5), EarnsFByAgePEM(nw,5)
    real(dbl) LabFByAgePEM(nw,5), SocSecByAgePEM(nr,5), TotMedByAgePEM(nr,5), OOPExpByAgePEM(nr,5)
    real(dbl) GovTransferByAgePEM(T,5)
    real(dbl) MaritalDistAge65PEM(5,3), MaritalDistAge65Data(5,3) 
    real(dbl) MaritalDistAge65SSInc(5,3), MaritalDistAge65SSIncData(5,3), AggregateOOPSSQ(5,3)                
    real(dbl) FracRecGovTransByAgePEM(T,5), MedicaidExpByAgePEM(nr,5), AveEarnsQuintileCutoffs(5)
    real(dbl) AggregateWealthPEQ(5), AggregateConsPEQ(5), AggregateRetConsPEQ(5), MeanMedicaidPEQ(5)
    real(dbl) AggregateOOPPEQ(5), SocSecRepRateAge65(5,3)
    real(dbl) AggWorkersAssetsPEQ(5), AggRetireesAssetsPEQ(5), FracWorkersRecGovTrans(5), FracRetireesRecGovTrans(5)
    real(dbl) Frac6574RecGovTrans(5), Frac7584RecGovTrans(5), Frac85plusRecGovTrans(5)
    
    real(dbl) WealthByAgeAveMedExp(T,5), ConsByAgeAveMedExp(T,5), UtilByAgeAveMedExp(T,5), TotMedByAgeAveMedExp(nr,5)
    real(dbl) OOPExpByAgeAveMedExp(nr,5), MedicaidExpByAgeAveMedExp(nr,5), GovTransferByAgeAveMedExp(T,5)    
    
    !statistics by education
    real(dbl) FracWomenLabForByEduc(nhet), AveFemaleHoursByEduc(nhet), AveFemaleHoursCondWorkByEduc(nhet)
    
    !statistics by age and education    
    real(dbl) WealthByAgeEduc(T,nhet), ConsByAgeEduc(T,nhet), AveUtilByAgeEduc(T,nhet)
    real(dbl) AveUtilConsByAgeEduc(T,nhet), FracWomenLabForByAgeEduc(nw,nhet)
    real(dbl) AveFemaleHoursByAgeEduc(nw,nhet), FracRecGovTransByAgeEduc(T,nhet)
    real(dbl) AveMaleWagesByAgeEduc(nw,nset), AveFemaleWagesByAgeEduc(nw,nset)
        
    !statistics by HH health type, Educ, Marital Status
    real(dbl) AveUtilByHeal(nhht)
    real(dbl) AveUtilByEducHeal(nhet,nhht) , AveUtilByAgeHealMart(nr,nsht,3)
    real(dbl) AveTotExpsByMart(3), AveOOPExpsByMart(3), AveTotExpsByHealMart(nhht,3), AveOOPExpsByHealMart(nhht,3)
    
    !Useful Arrays
    real(dbl) SocSecArray(naef,naem,3)
    real(dbl) OOPExpArrayMarried(na,nm,naef,naem,nhht,4,nr)
    real(dbl) OOPExpArrayWidow(na,nm,naef,naem,nsht,2,nr)
    real(dbl) OOPExpArrayWidower(na,nm,naef,naem,nsht,2,nr)
          
    !Other
    real(dbl) FracRetsAtAveEarnsF(naef), FracRetsAtAveEarnsM(naem)  
    
    !Calculated in GE loop
    real(dbl) AggregateProdVect(nw), AggregateCapitalVect(T), CohortWeights(T)
    real(dbl) IncTaxByAge(T), GovTransfersVect(T), SSTaxesVect(nw), EarnsTaxesVect(nw)
    real(dbl) SocSecTaxesVect(nw), SocSecBenefitsVect(nr)
    real(dbl) AggregateProd, AggCapitalDemand, AggCapitalSupply, CapitalDiff, IncomeTaxes
    real(dbl) GovTransfers, SSTaxes, EarnsTaxes, AggBudgetConst 
    real(dbl) TotalPop, GovBudgetConstraint, RescourceConst   
    real(dbl) SumSquareMoments2
   
    real(dbl) FracProdLocM(nem,nw), FracProdLocF(nef,nw), FracEducLocH(nhet,nw)
    real(dbl) FracProdLocMID(nem,nw), FracProdLocFID(nef,nw), FracEducLocHID(nhet,nw)
   
    real(dbl) DataVect(51), LaborMarketClearing, BeqClearing
    integer EarnsLocs(na,nef,nem,naef,naem,nw)
    real(dbl) earnsFMat(na,nef,nem,naef,naem,nhet,nw), etaMat(na,nef,nem,naef,naem,nhet,nw)

   
    integer numprocs, ind(10), ier, my_rank, curstat
    integer getype, gbtype
    
    common/gevars/efmat, emmat, w, r, PropTax, aveEarnFvect, aveEarnMvect, AveEarningsAll, AveHHIncome, sinitH
    common/gbvars/mmat, avect, survivalprobVectF, survivalprobVectM, minit, Puefmat
    common/gbvars/Puemmat, Pummat, hinitH, PhmatM, PhmatF, Ptmvect
    real(dbl) part_Vcube(na,count_wef,count_wem,count_waef,count_waem,count_wet)
    real(dbl) part_CUcube(na,count_wef,count_wem,count_waef,count_waem,count_wet)
    real(dbl) part_labCube(na,count_wef,count_wem,count_waef,count_waem,count_wet)
    real(dbl) part_consCube(na,count_wef,count_wem,count_waef,count_waem,count_wet)
    real(dbl) part_aPolicyCube(na,count_wef,count_wem,count_waef,count_waem,count_wet)
    real(dbl) part_consRCube(na,count_rm,count_raef,count_raem,count_rh,count_rs)
    real(dbl) part_aPolicyRCube(na,count_rm,count_raef,count_raem,count_rh,count_rs)
    real(dbl) part_VRcube(na,count_rm,count_raef,count_raem,count_rh,count_rs)
    real(dbl) part_CURcube(na,count_rm,count_raef,count_raem,count_rh,count_rs)   
    real(dbl) final
end module globals
