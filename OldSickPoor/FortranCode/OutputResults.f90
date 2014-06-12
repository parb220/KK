module OutputResults
    use globals
    use output
    
    implicit none    
    contains    
          
    subroutine SaveResults()
        character(8) date
        character(10) time
        character(26) fileName  
      
               
        call date_and_time(date,time)
        fileName = 'SS_'//date(3:)//'_'//time(:6)//'.xls'    
        print *, "Saving results in file ", fileName   
        open(1,file = fileName)                
        
        call Out(1,"SumSquareMoments2",SumSquareMoments2,tab)
        write(1,'(a)',advance='no'), ret
        call Out(1,"NormalizeWage",NormalizeWage,tab)
        call Out(1,"PartialEqm",PartialEqm,tab)
        call Out(1,"ProportionalSocSec",ProportionalSocSec,tab)
        write(1,'(a)',advance='no') ret  
        call Out(1,"RunOnce",RunOnce,tab)      
        call Out(1,"OpenEconomy",OpenEconomy,tab) 
        call Out(1,"ExpensesZero",ExpensesZero,tab)               
        call Out(1,"ResultsOnly",ResultsOnly,tab)        
        call Out(1,"SaveSolution",SaveSolution,tab)
        call Out(1,"GEexperiment",GEexperiment,tab)        
        call Out(1,"UseEarnsTax",UseEarnsTax,tab)        
        call Out(1,"ScaleDownSocSecBenefit",ScaleDownSocSecBenefit,tab)      
        call Out(1,"ScaleDownMedicaidBenefit",ScaleDownMedicaidBenefit,tab)
        call Out(1,"MoveSocSecToMedicaid",MoveSocSecToMedicaid,tab)                  
        write(1,'(a)',advance='no') ret            
        write(1,'(a)') "grids"
        call Out(1,"nef",nef,tab)
        call Out(1,"nem",nem,tab)   
        call Out(1,"naef",naef,tab)             
        call Out(1,"naem",naem,tab)         
        call Out(1,"nm", nm, tab)
        call Out(1,"na",na,tab)
        write(1,'(a)',advance='no') ret   
        call Out(1,"nset",nset,tab)
        call Out(1,"nsht",nsht,tab)
        call Out(1,"nhet",nhet,tab)
        call Out(1,"nhht",nhht,tab)   
        call Out(1,"nl",nl,tab)        
        write(1,'(a)',advance='no') ret   
        call Out(1,"aveEarnMin", aveEarnMin, tab) 
        call Out(1,"aveEarnMax", aveEarnMax, tab)
        write(1,'(a)',advance='no') ret   
        call Out(1, "aveEarnFvect", aveEarnFvect, ret)          
        call Out(1, "aveEarnMvect", aveEarnMvect, ret)           
        write(1,'(a)',advance='no') ret 
        call Out(1,"socseckinks", socseckinks)         
        write(1,'(a)',advance='no') ret   
        call Out(1,"lnamin", lnamin, tab)
        call Out(1,"lnamax", lnamax, tab)
        write(1,'(a)',advance='no') ret
        call Out(1,"avect(1)", avect(1), tab)
        call Out(1,"avect(na-1)", avect(na-1), tab)
        call Out(1,"avect(na)", avect(na), tab)
        
        !write(1,'(a)',advance='no') ret  
        !write(1,'(a)',advance='no') ret  
        !do i=1,nef
        !do j=1,nem
        !    call Out(1,"efmat", transpose(efmat(i,j,:,:))) 
        !    write(1,'(a)',advance='no') ret 
        !end do
        !end do
        !write(1,'(a)',advance='no') ret  
        !write(1,'(a)',advance='no') ret 
        !do i=1,nef
        !do j=1,nem
        !    call Out(1,"emmat", transpose(emmat(i,j,:,:))) 
        !    write(1,'(a)',advance='no') ret            
        !end do
        !end do                
        !write(1,'(a)',advance='no'), ret
        !write(1,'(a)',advance='no'), ret
        !do i=1,nm
        !do j=1,nhht
        !    call Out(1,"mmat", transpose(mmat(i,j,:,:))) 
        !    write(1,'(a)',advance='no') ret 
        !end do
        !end do
        !write(1,'(a)',advance='no') ret  
        !write(1,'(a)',advance='no') ret         
        
        write(1,'(a)') "age structure"
        call Out(1,"T",T,tab)
        call Out(1,"nw",nw,tab)
        call Out(1,"nr",nr,tab)
        write(1,'(a)',advance='no') ret
        write(1,'(a)',advance='no'), ret

        write(1,'(a)') "preferences" 
        call Out(1,"beta",beta,tab)
        call Out(1,"gamma",gamma,tab)
        call Out(1,"sigma",sigma,tab)
        call Out(1,"psi",psi,tab)   
        write(1,'(a)',advance='no') ret        
        call Out(1,"psiR",psiR,tab)   
        write(1,'(a)',advance='no') ret           
        call Out(1,"phi1",phi1,tab)
        write(1,'(a)',advance='no') ret  
        call Out(1,"lambda",lambda,tab)
        call Out(1,"hbar",hbar,tab)               
        write(1,'(a)',advance='no') ret
        write(1,'(a)',advance='no'), ret
         
        write(1,'(a)') "technology" 
        call Out(1,"alpha", alpha, tab)
        call Out(1,"A", A, tab)
        call Out(1,"delta", delta, tab)
        write(1,'(a)',advance='no') ret
        write(1,'(a)',advance='no') ret
        
        write(1,'(a)') "Earnings process" 
        call Out(1,"betae0",betae0,tab)    
        call Out(1,"betae1",betae1,tab)
        call Out(1,"betae2",betae2,tab)
        call Out(1,"betae3",betae3,tab)
        call Out(1,"betae4",betae4,tab)
        write(1,'(a)',advance='no') ret        
        call Out(1,"rhoe (annual)",rhoe,tab)    
        call Out(1,"sigma_epse (annual)",sigma_epse,tab)
        write(1,'(a)',advance='no') ret
        call Out(1,"corr_epse",corr_epse,tab)
        call Out(1,"corr_epse0",corr_epse0,tab)
        call Out(1,"sigma_0 (annual)",sigma_0,tab)          
        write(1,'(a)',advance='no'), ret
        call Out(1,"uinit", uinit)
        write(1,'(a)',advance='no') ret
        call Out(1, "Puefmat", Puefmat)
        write(1,'(a)',advance='no') ret
        call Out(1, "Puemmat", Puemmat)
        write(1,'(a)',advance='no') ret
        call Out(1, "Cmat", Cmat)
        write(1,'(a)',advance='no') ret                    
        call Out(1, "uefgrid", uefgrid, tab)        
        write(1,'(a)',advance='no') ret  
        call Out(1, "uemgrid", uemgrid, tab)        
        write(1,'(a)',advance='no') ret          
        call Out(1, "educDist", educDist, tab)
        write(1,'(a)',advance='no') ret          
        call Out(1, "AveEarnsM65dist", AveEarnsM65dist, tab)     
        write(1,'(a)',advance='no') ret
        call Out(1, "ProdDist", ProdDist)     
        write(1,'(a)',advance='no') ret                   
        write(1,'(a)',advance='no') ret        
        write(1,'(a)',advance='no') ret
        
        write(1,'(a)') "medical expense process"
        call Out(1,"medscale", medscale,tab) 
        write(1,'(a)',advance='no') ret
        call Out(1,"NHshock", NHshock,tab) 
        write(1,'(a)',advance='no') ret
        call Out(1,"betams",betam(1:10),tab)
        write(1,'(a)',advance='no') ret
        call Out(1,"betams",betam(11:20),tab)
        write(1,'(a)',advance='no') ret        
        call Out(1,"rhom (annual)",rhom,tab)
        call Out(1,"sigma_epsm (annual)",sigma_epsm,tab)  
        write(1,'(a)',advance='no') ret             
        call Out(1,"minit", minit)
        write(1,'(a)',advance='no') ret        
        call Out(1, "umgrid", umgrid)
        write(1,'(a)',advance='no') ret                   
        call Out(1, "Pummat", Pummat(:,:,1))
        write(1,'(a)',advance='no') ret  
        call Out(1, "utmgrid", utmgrid,tab)
        write(1,'(a)',advance='no') ret                   
        call Out(1, "Ptmvect", Ptmvect,tab)
        write(1,'(a)',advance='no') ret                                  
        !write(1,'(a)',advance='no') ret
        !call Out(1, "MedExpDist", MedExpDist)     
        !write(1,'(a)',advance='no') ret 
        write(1,'(a)',advance='no') ret      
        
        write(1,'(a)') "health transition matrices"
        call Out(1,"betahF",betahF,tab)
        write(1,'(a)',advance='no') ret
        call Out(1,"betahM",betahM,tab)
        write(1,'(a)',advance='no') ret        
        call Out(1,"hinitH", hinitH)
        write(1,'(a)',advance='no') ret  
        call Out(1,"hinitF", hinitF)
        write(1,'(a)',advance='no') ret 
        call Out(1,"hinitM", hinitM)
        write(1,'(a)',advance='no') ret                                  
        
        write(1,'(a)') "demographics"
        write(1,'(a)',advance='no') ret   
        call Out(1,"population growth rate", ng,tab) 
        write(1,'(a)',advance='no') ret
        write(1,'(a)',advance='no'), ret
        call Out(1,"probHusband0", probHusband0,tab) 
        write(1,'(a)',advance='no') ret                 
        call Out(1,"sinitHa", sinitHa, tab)
        call Out(1,"sinitHb", sinitHb, tab)
        call Out(1,"sinitHaf", sinitHaf, tab)
        call Out(1,"sinitHbf", sinitHbf, tab)
        write(1,'(a)',advance='no') ret
        call Out(1,"sinitHuncond", sinitHuncond, tab)
        write(1,'(a)',advance='no') ret
        call Out(1,"sinitH", sinitH)
        write(1,'(a)',advance='no') ret
        write(1,'(a)',advance='no'), ret 
                        
        write(1,'(a)') "survival probabilities"
        write(1,'(a)',advance='no') ret   
        call Out(1,"betasF",betasF,tab) 
        write(1,'(a)',advance='no') ret 
        call Out(1,"betasM",betasM,tab) 
        write(1,'(a)',advance='no') ret 
        call Out(1,"survivalprobVectF", survivalprobVectF)
        write(1,'(a)',advance='no') ret 
        call Out(1,"survivalprobVectM", survivalprobVectM)
        write(1,'(a)',advance='no') ret         
        write(1,'(a)',advance='no'), ret 
        
        write(1,'(a)') "government"
        call Out(1,"clowerbar for working",clowerbarW,tab)
        call Out(1,"clowerbar for married retired",clowerbarMarried ,tab)
        call Out(1,"clowerbar in retired widows", clowerbarWidow ,tab)
        call Out(1,"clowerbar in retired widowers", clowerbarWidower ,tab)
        call Out(1,"ceiling", ceiling ,tab)
        write(1,'(a)',advance='no') ret   
        call Out(1,"GovSpending",GovSpending,tab) 
        write(1,'(a)',advance='no') ret
        !call Out(1,"GovSpendingBaseline",GovSpendingBaseline,tab) 
        !write(1,'(a)',advance='no') ret        
        call Out(1,"s1",s1,tab)
        call Out(1,"s2",s2,tab)
        call Out(1,"s3",s3,tab)
        call Out(1,"tau1",tau1,tab)
        call Out(1,"tau2",tau2,tab)
        call Out(1,"tau3",tau3,tab) 
        write(1,'(a)',advance='no') ret       
        call Out(1,"SocSecReduction",SocSecReduction,tab)                
        write(1,'(a)',advance='no') ret   
        call Out(1,"MedicaidReduction",MedicaidReduction,tab)                
        write(1,'(a)',advance='no') ret                   
        call Out(1,"emax",emax,tab) 
        call Out(1,"medded ",medded ,tab)
        write(1,'(a)',advance='no') ret       
        call Out(1,"a_mw ",a_mw,tab)
        call Out(1,"b_mw ",b_mw,tab)
        call Out(1,"a_mr ",a_mr,tab)    
        call Out(1,"b_mr ",b_mr,tab) 
        call Out(1,"a_sr ",a_sr,tab)    
        call Out(1,"b_sr ",b_sr,tab)   
        write(1,'(a)',advance='no') ret     
        call Out(1,"capTax ",capTax,tab)                   
        write(1,'(a)',advance='no') ret 
        write(1,'(a)',advance='no'), ret  
         
        write(1,'(a)') "equilibrium prices and taxes"
        call Out(1,"r",r,tab)
        call Out(1,"w",w,tab)
        !call Out(1,"bequest transfer",btran,tab)
        !call Out(1,"lstax",lstax,tab)
        !call Out(1,"earnstax",earnstax,tab)
        call Out(1,"GovSpending",GovSpending,tab)
        call Out(1,"AddGovConsumption",AddGovConsumption,tab)        
        call Out(1,"PropTax",PropTax,tab)    
        call Out(1,"EarnsTax",EarnsTax,tab)            
        call Out(1,"tauhatS",tauhatS,tab)                                             
        write(1,'(a)',advance='no'), ret   
        write(1,'(a)',advance='no'), ret         
         
        write(1,'(a)') "Tolerances and Errors"
        call Out(1,"eps ", eps ,tab)
        call Out(1,"fracUpdatedCapInit ", fracUpdatedCapInit ,tab)
        call Out(1,"fracUpdatedCap ", fracUpdatedCap ,tab)
        call Out(1,"tolCapital ", tolCapital ,tab)   
        call Out(1,"tolGovBudget ", tolGovBudget ,tab)   
        write(1,'(a)',advance='no'), ret          
        call Out(1,"Capital:Supply-Demand", CapitalDiff,tab)
        call Out(1,"GovBudgetConstraint:Revenue-Expend", GovBudgetConstraint,tab)
        call Out(1,"LaborMarketClearing:Guess Demand - Supply", LaborMarketClearing,tab)                        
        call Out(1,"RescourceConst",RescourceConst,tab)   
        call Out(1,"AggBudgetConst",AggBudgetConst,tab)        
        write(1,'(a)',advance='no'), ret   
        write(1,'(a)',advance='no'), ret   
                
        write(1,'(a)') "Aggregates"
        call Out(1,"Y",AggOutput,tab)    
        write(1,'(a)',advance='no') ret  
        call Out(1,"C", AggCons,tab)
        write(1,'(a)',advance='no') ret  
        call Out(1,"AggConsAtDeath", AggConsAtDeath,tab)
        write(1,'(a)',advance='no') ret  
        call Out(1,"AggConsExcludeDeath", AggConsExcludeDeath,tab)         
        write(1,'(a)',advance='no') ret              
        call Out(1,"K supply", AggCapitalSupply, tab)
        write(1,'(a)',advance='no') ret   
        call Out(1,"K demand", AggCapitalDemand, tab)
        write(1,'(a)',advance='no') ret                     
        call Out(1,"GovTransfers",GovTransfers,tab)        
        write(1,'(a)',advance='no') ret  
        call Out(1,"AggLaborSupply", AggLaborSupply,tab)
        write(1,'(a)',advance='no') ret 
        call Out(1,"AveMaleEarnings ", AveMaleEarnings, tab)
        write(1,'(a)',advance='no') ret    
        call Out(1,"AveFemaleEarnings", AveFemaleEarnings, tab) 
        write(1,'(a)',advance='no') ret 
        call Out(1,"AveHHEarnings", AveHHEarnings, tab)                    
        write(1,'(a)',advance='no') ret      
        call Out(1,"AveEarningsAll", AveEarningsAll, tab)    
        write(1,'(a)',advance='no') ret                 
        call Out(1,"IncomeTaxes", IncomeTaxes,tab)
        write(1,'(a)',advance='no') ret 
        call Out(1,"CapitalTaxes", CapitalTaxes,tab)
        write(1,'(a)',advance='no') ret                                  
        call Out(1,"AggMedExp",AggMedExp,tab)
        write(1,'(a)',advance='no') ret                
        call Out(1, "Aggregate OOP", AggOOPExp, tab)                
        write(1,'(a)',advance='no') ret  
        call Out(1,"MeanOOPExp",MeanOOPExp,tab)
        write(1,'(a)',advance='no') ret  
        call Out(1,"MeanOOPExp of Medicaid recipients",MeanOOPExpMedicaid,tab)        
        write(1,'(a)',advance='no') ret                
        call Out(1, "MeanOOPExpCond", MeanOOPExpCond, tab)                
        write(1,'(a)',advance='no') ret                                                                                                              
        call Out(1,"SSTaxes", SSTaxes, tab)
        write(1,'(a)',advance='no') ret   
        call Out(1,"EarnsTaxes", EarnsTaxes, tab)
        write(1,'(a)',advance='no') ret           
        call Out(1,"MedicareRevenue", MedicareRevenue, tab)
        write(1,'(a)',advance='no') ret                                                                          
        call Out(1,"TotalPayments", TotalPayments, tab)           
        write(1,'(a)',advance='no') ret    
        call Out(1,"AveHHIncome", AveHHIncome, tab) 
        write(1,'(a)',advance='no') ret    
        call Out(1,"AggNHExp", AggNHExp, tab)   
        write(1,'(a)',advance='no') ret    
        call Out(1,"AggConsNHResidents", AggConsNHResidents, tab)                                                  
        write(1,'(a)',advance='no') ret
        write(1,'(a)',advance='no'), ret                      
               
               
        call Out(1,"MaritalDist", MaritalDist(:,1:9))
        write(1,'(a)',advance='no'), ret   
        call Out(1,"MaritalDist", MaritalDist(:,10:nr))
        write(1,'(a)',advance='no'), ret                  
        write(1,'(a)',advance='no'), ret   
        call Out(1,"FracIndByMaritalStatusAge", FracIndByMaritalStatusAge(:,1:9))
        write(1,'(a)',advance='no'), ret   
        call Out(1,"FracIndByMaritalStatusAge", FracIndByMaritalStatusAge(:,10:nr))
        write(1,'(a)',advance='no'), ret                  
        write(1,'(a)',advance='no'), ret               
         
             
        call Out(1,"Target", "Data (%)", "Model (%)", tab)
        write(1,'(a)',advance='no') ret      
        call Out(1,"AveYearsAliveF", 0.0d0, AveYearsAliveF,tab)
        call Out(1,"AveYearsAliveF2 (cond alive age 65)", 19.2d0, AveYearsAliveF2,tab)
        call Out(1,"AveYearsAliveM", 0.0d0, AveYearsAliveM,tab)
        call Out(1,"AveYearsAliveM2 (cond alive age 65)", 16.3d0, AveYearsAliveM2,tab)        
        call Out(1,"SSTaxes/GDP", 5.0d0, SSTaxes/AggOutput*100, tab)
        call Out(1,"IncomeTaxes/GDP", 8.0d0, IncomeTaxes/AggOutput*100, tab)
        call Out(1,"CapitalTaxes/GDP", 2.8d0, CapitalTaxes/AggOutput*100, tab)                    
        call Out(1,"Frac65plus", 0.18d0, Frac65plus,tab)  
        call Out(1,"MeanOOPExpMedicaid/MeanOOPExp", 0.46d0, MeanOOPExpMedicaid/MeanOOPExp,tab)  
        
        !data numbers are from Age_Medicaid_Profiles_Pooled_02222013.xlsx       
        call Out(1,"FracIndMedicaid", 0.13d0, FracIndMedicaid,tab)         
        call Out(1,"Frac6574IndMedicaid", 0.11d0, Frac6574IndMedicaid,tab)  
        call Out(1,"Frac7584IndMedicaid", 0.12d0, Frac7584IndMedicaid,tab)  
        call Out(1,"Frac85plusIndMedicaid", 0.19d0, Frac85plusIndMedicaid,tab)  
        
        call Out(1,"FracWidowsMedicaid", 0.22d0, FracWidowsMedicaid,tab)   
        call Out(1,"Frac6566WidowsMedicaid", 0.26d0, Frac6566WidowsMedicaid,tab)    
        call Out(1,"Frac6574WidowsMedicaid", 0.22d0, Frac6574WidowsMedicaid,tab)
        call Out(1,"Frac7584WidowsMedicaid", 0.19d0, Frac7584WidowsMedicaid,tab)    
        call Out(1,"Frac85plusWidowsMedicaid", 0.24d0, Frac85plusWidowsMedicaid,tab)                    
        
        call Out(1,"FracWidowersMedicaid", 0.17d0, FracWidowersMedicaid,tab)   
        call Out(1,"Frac6566WidowersMedicaid", 0.18d0, Frac6566WidowersMedicaid,tab)    
        call Out(1,"Frac6574WidowersMedicaid", 0.19d0, Frac6574WidowersMedicaid,tab)    
        call Out(1,"Frac7584WidowersMedicaid", 0.15d0, Frac7584WidowersMedicaid,tab)    
        call Out(1,"Frac85plusWidowersMedicaid", 0.19d0, Frac85plusWidowersMedicaid,tab)    
        
        call Out(1,"FracMarriedMedicaid", 0.07d0, FracMarriedMedicaid,tab)
        call Out(1,"Frac6566MarriedMedicaid", 0.07d0, Frac6566MarriedMedicaid,tab)    
        call Out(1,"Frac6574MarriedMedicaid", 0.07d0, Frac6574MarriedMedicaid,tab)    
        call Out(1,"Frac7584MarriedMedicaid", 0.07d0, Frac7584MarriedMedicaid,tab)    
        call Out(1,"Frac85plusMarriedMedicaid", 0.11d0, Frac85plusMarriedMedicaid,tab)      
                 
        !data numbers are from Lifetime_Earnings_Stats_BothRetired_02222013.xlsx                 
        call Out(1,"Frac65plusHHMedicaid", 0.21d0, Frac65plusMedicaid,tab)
        call Out(1,"Frac6574HHMedicaid", 0.21d0, Frac6574Medicaid,tab)
        call Out(1,"Frac7584HHMedicaid", 0.19d0, Frac7584Medicaid,tab)
        call Out(1,"Frac85plusHHMedicaid", 0.27d0, Frac85plusMedicaid,tab)                 
                 
        call Out(1,"Percent 65+ HHs on Medicaid by age 65 Male LE Quintiles")
        call Out(1,"First", 69.0d0, FracRetireesRecGovTrans(1)*100, tab)
        call Out(1,"Second", 33.0d0, FracRetireesRecGovTrans(2)*100, tab)
        call Out(1,"Third", 14.0d0, FracRetireesRecGovTrans(3)*100, tab)
        call Out(1,"Fourth", 7.0d0, FracRetireesRecGovTrans(4)*100, tab)    
        call Out(1,"Fifth", 4.5d0, FracRetireesRecGovTrans(5)*100, tab)      
        call Out(1,"Percent 65-74 HHs on Medicaid by age 65 Male LE Quintiles")
        call Out(1,"First", 65.0d0, Frac6574RecGovTrans(1)*100, tab)
        call Out(1,"Second", 30.0d0, Frac6574RecGovTrans(2)*100, tab)
        call Out(1,"Third", 11.0d0, Frac6574RecGovTrans(3)*100, tab)
        call Out(1,"Fourth", 4.8d0, Frac6574RecGovTrans(4)*100, tab)    
        call Out(1,"Fifth", 3.7d0, Frac6574RecGovTrans(5)*100, tab)
        call Out(1,"Percent 75-84 HHs on Medicaid by age 65 Male LE Quintiles")
        call Out(1,"First", 73.0d0, Frac7584RecGovTrans(1)*100, tab)
        call Out(1,"Second", 32.0d0, Frac7584RecGovTrans(2)*100, tab)
        call Out(1,"Third", 13d0, Frac7584RecGovTrans(3)*100, tab)
        call Out(1,"Fourth", 6.6d0, Frac7584RecGovTrans(4)*100, tab)    
        call Out(1,"Fifth", 4.4d0, Frac7584RecGovTrans(5)*100, tab)    
        call Out(1,"Percent 85+ HHs on Medicaid by age 65 Male LE Quintiles")
        call Out(1,"First", 72.0d0, Frac85plusRecGovTrans(1)*100, tab)
        call Out(1,"Second", 38.0d0, Frac85plusRecGovTrans(2)*100, tab)
        call Out(1,"Third", 18.0d0, Frac85plusRecGovTrans(3)*100, tab)
        call Out(1,"Fourth", 11.0d0, Frac85plusRecGovTrans(4)*100, tab)    
        call Out(1,"Fifth", 6.7d0, Frac85plusRecGovTrans(5)*100, tab)                   
        
        !data numbers are from Fractions_New_Medicaid.xlsx !use Data_MedicaidwithExitandCarry        
        call Out(1,"FracHHEnterMedicaid 65-74", 0.0d0, FracHHEnterMedicaid(1),tab)
        call Out(1,"FracHHEnterMedicaid 75-84", 0.0d0, FracHHEnterMedicaid(2),tab)
        call Out(1,"FracHHEnterMedicaid 85+", 0.0d0, FracHHEnterMedicaid(3),tab) 

        call Out(1,"FracIndEnterMedicaid 65-74", 0.041d0, FracIndEnterMedicaid(1),tab)
        call Out(1,"FracIndEnterMedicaid 75-84", 0.041d0, FracIndEnterMedicaid(2),tab)
        call Out(1,"FracIndEnterMedicaid 85+", 0.074d0, FracIndEnterMedicaid(3),tab) 

        call Out(1,"FracMarriedEnterMedicaid 65-74", 0.028d0, FracMarriedEnterMedicaid(1),tab)
        call Out(1,"FracMarriedEnterMedicaid 75-84", 0.029d0, FracMarriedEnterMedicaid(2),tab)
        call Out(1,"FracMarriedEnterMedicaid 85+", 0.043d0, FracMarriedEnterMedicaid(3),tab) 
        
        call Out(1,"FracWidowsEnterMedicaid 65-74", 0.065d0, FracWidowsEnterMedicaid(1),tab)
        call Out(1,"FracWidowsEnterMedicaid 75-84", 0.055d0, FracWidowsEnterMedicaid(2),tab)
        call Out(1,"FracWidowsEnterMedicaid 85+", 0.088d0, FracWidowsEnterMedicaid(3),tab) 
        
        call Out(1,"FracWidowersEnterMedicaid 65-74", 0.077d0, FracWidowersEnterMedicaid(1),tab)
        call Out(1,"FracWidowersEnterMedicaid 75-84", 0.066d0, FracWidowersEnterMedicaid(2),tab)
        call Out(1,"FracWidowersEnterMedicaid 85+", 0.090d0, FracWidowersEnterMedicaid(3),tab)                                                
           
        !These data numbers came from Lifetime_Earnings_Stats_v2.xlsx for Households from HRS data
        call Out(1,"Frac65plusHHTopMedShock", 0.086d0, Frac65plusHHTopMedShock,tab)
        call Out(1,"Frac6574HHTopMedShock", 0.040d0, Frac6574HHTopMedShock,tab)
        call Out(1,"Frac7584HHTopMedShock", 0.080d0, Frac7584HHTopMedShock,tab)
        call Out(1,"Frac85plusHHTopMedShock", 0.21d0, Frac85plusHHTopMedShock,tab) 
        !These numbers come from Liu, McBrid, and Coughlin (1994) and Murtaugh, Kemper, Spillman, and Lepidus (1997)
        call Out(1,"frac 65 year-olds NH", 0.295d0, fracNH,tab) 
        call Out(1,"Fraction age 1st NH entry 65-74", 0.21d0, distNHentryage(1),tab)
        call Out(1,"Fraction age 1st NH entry 75-84", 0.46d0, distNHentryage(2),tab)
        call Out(1,"Fraction age 1st NH entry 85+", 0.33d0, distNHentryage(3),tab)               
        call Out(1,"aveyearsNH 65-74", 3.9d0, aveyearsNH(1),tab)
        call Out(1,"aveyearsNH 75-84", 3.2d0, aveyearsNH(2),tab)
        call Out(1,"aveyearsNH 85+", 2.9d0, aveyearsNH(3),tab)    
                                                                                                               
        call Out(1,"Pre-tax rate of return on capital (beta)", 11.5d0, r*100.0d0, tab)                
        call Out(1,"AveFemaleHoursCondWork", 0.367d0, AveFemaleHoursCondWork, tab)  
        call Out(1,"AveFemaleHoursCondWork (HH)", 0.333d0, AveFemaleHoursCondWorkByEduc(1), tab)  
        call Out(1,"AveFemaleHoursCondWork (HC)", 0.382d0, AveFemaleHoursCondWorkByEduc(2), tab)  
        call Out(1,"AveFemaleHoursCondWork (CH)", 0.351d0, AveFemaleHoursCondWorkByEduc(3), tab)  
        call Out(1,"AveFemaleHoursCondWork (CC)", 0.363d0, AveFemaleHoursCondWorkByEduc(4), tab)  
        call Out(1,"AveFemaleHours/AveMaleHours", 0.40d0, AveFemaleHours/hbar, tab)  
        call Out(1,"AveFemaleHours", 0.18d0, AveFemaleHours, tab)  
        call Out(1,"AveFemaleHours (HH)", 0.16d0, AveFemaleHoursByEduc(1), tab)  
        call Out(1,"AveFemaleHours (HC)", 0.17d0, AveFemaleHoursByEduc(2), tab)  
        call Out(1,"AveFemaleHours (CH)", 0.24d0, AveFemaleHoursByEduc(3), tab)  
        call Out(1,"AveFemaleHours (CC)", 0.21d0, AveFemaleHoursByEduc(4), tab)  
        call Out(1,"FracWomenLaborForce", 0.490d0, FracWomenLaborForce, tab) 
        call Out(1,"FracWomenLaborForce (HH)", 0.481d0, FracWomenLabForByEduc(1), tab)    
        call Out(1,"FracWomenLaborForce (HC)", 0.445d0, FracWomenLabForByEduc(2), tab) 
        call Out(1,"FracWomenLaborForce (CH)", 0.683d0, FracWomenLabForByEduc(3), tab)    
        call Out(1,"FracWomenLaborForce (CC)", 0.579d0, FracWomenLabForByEduc(4), tab)  
        call Out(1,"AveFrischElasticityF", 1.77d0, AveFrischElasticityF, tab)  
        call Out(1,"AveFrischElasticityFW", 1.77d0, AveFrischElasticityFW, tab)  
        call Out(1,"AveFrischElasticityM", 0.5d0, AveFrischElasticityM, tab)  
        call Out(1,"corrWageMHoursF", -0.11d0, corrWageMHoursF, tab) 
        call Out(1,"corrWageFM", 0.15d0, corrWageFM, tab) 
        call Out(1,"corrWageMHoursFx", -0.11d0, corrWageMHoursFx, tab) 
        call Out(1,"corrWageFMx", 0.15d0, corrWageFMx, tab)          
        call Out(1,"TotalMedExps/Output", 2.1d0, AggMedExp/AggOutput*100.0d0, tab)  
        call Out(1,"OOPExps/Output", 1.5d0, AggOOPExp/AggOutput*100.0d0, tab)   
        call Out(1,"MedicaidExps/Output", 0.6d0, (AggMedExp-AggOOPExp)/AggOutput*100.0d0, tab) 
        call Out(1,"OOP Health Expenditures: Fraction of GDP by Age")        
        call Out(1,"65-74", 0.61d0, OOPExpOverOutputByAge(1),tab)
        call Out(1,"75-84", 0.55d0, OOPExpOverOutputByAge(2),tab)
        call Out(1,"85+", 0.34d0, OOPExpOverOutputByAge(3),tab)        
        call Out(1,"Medicaid Expenditures: Fraction of GDP by Age")
        call Out(1,"65-74", 0.17d0, MedicaidExpOverOutputByAge(1),tab)
        call Out(1,"75-84", 0.23d0, MedicaidExpOverOutputByAge(2),tab)
        call Out(1,"85+", 0.23d0, MedicaidExpOverOutputByAge(3),tab)      
        write(1,'(a)',advance='no'), ret
        call Out(1,"Initial Conditions")  
        call Out(1,"OOP Health Expense Gini for 65-68 Year-Olds", 0.618d0, OOPExpGiniYoung, tab)
        call Out(1,"OOP Health Expense Quintiles for 65-68 Year-Olds")
        call Out(1,"First", 1.51d0, OOPmedShareYoung(1)*100, tab)
        call Out(1,"Second", 3.71d0, OOPmedShareYoung(2)*100, tab)
        call Out(1,"Third", 10.17d0, OOPmedShareYoung(3)*100, tab)
        call Out(1,"Fourth", 21.01d0, OOPmedShareYoung(4)*100, tab)    
        call Out(1,"Fifth", 63.60d0, OOPmedShareYoung(5)*100, tab)  
        call Out(1,"Share Paid by")        
        call Out(1,"Top 10", 45.41d0, OOPmedShareUpperYoung(1)*100, tab)      
        call Out(1,"Top 5", 31.73d0, OOPmedShareUpperYoung(2)*100, tab)      
        call Out(1,"Top 1", 14.16d0, OOPmedShareUpperYoung(3)*100, tab)    
        write(1,'(a)',advance='no'), ret  
        call Out(1,"Life-Cycle Dynamics")                
        call Out(1,"OOP Health Expense Gini", 0.675d0, OOPExpGini, tab)
        call Out(1,"OOP Health Expense Quintiles")
        call Out(1,"First", 1.14d0, OOPmedShare(1)*100, tab)
        call Out(1,"Second", 3.63d0, OOPmedShare(2)*100, tab)
        call Out(1,"Third", 8.67d0, OOPmedShare(3)*100, tab)
        call Out(1,"Fourth", 16.78d0, OOPmedShare(4)*100, tab)    
        call Out(1,"Fifth", 69.78d0, OOPmedShare(5)*100, tab)  
        call Out(1,"Share Paid by")        
        call Out(1,"Top 10", 54.90d0, OOPmedShareUpper(1)*100, tab)      
        call Out(1,"Top 5", 52.94d0, OOPmedShareUpper(2)*100, tab)      
        call Out(1,"Top 1", 21.63d0, OOPmedShareUpper(3)*100, tab)  
        write(1,'(a)',advance='no'), ret    
        call Out(1,"OOP Health Expense Gini for 65-74 Year-Olds", 0.600d0, OOPExpGini6574, tab)
        call Out(1,"OOP Health Expense Quintiles for 65-74 Year-Olds")
        call Out(1,"First", 1.47d0, OOPmedShare6574(1)*100, tab)
        call Out(1,"Second", 4.46d0, OOPmedShare6574(2)*100, tab)
        call Out(1,"Third", 10.91d0, OOPmedShare6574(3)*100, tab)
        call Out(1,"Fourth", 21.31d0, OOPmedShare6574(4)*100, tab)    
        call Out(1,"Fifth", 61.85d0, OOPmedShare6574(5)*100, tab)  
        call Out(1,"Share Paid by")        
        call Out(1,"Top 10", 43.56d0, OOPmedShareUpper6574(1)*100, tab)      
        call Out(1,"Top 5", 30.32d0, OOPmedShareUpper6574(2)*100, tab)      
        call Out(1,"Top 1", 13.60d0, OOPmedShareUpper6574(3)*100, tab)            
        write(1,'(a)',advance='no'), ret        
        call Out(1,"OOP Health Expense Gini for 75-84 Year-Olds", 0.621d0, OOPExpGini7584, tab)
        call Out(1,"OOP Health Expense Quintiles for 75-84 Year-Olds")
        call Out(1,"First", 1.41d0, OOPmedShare7584(1)*100, tab)
        call Out(1,"Second", 4.62d0, OOPmedShare7584(2)*100, tab)
        call Out(1,"Third", 10.24d0, OOPmedShare7584(3)*100, tab)
        call Out(1,"Fourth", 19.31d0, OOPmedShare7584(4)*100, tab)    
        call Out(1,"Fifth", 64.41d0, OOPmedShare7584(5)*100, tab)  
        call Out(1,"Share Paid by")        
        call Out(1,"Top 10", 47.79d0, OOPmedShareUpper7584(1)*100, tab)      
        call Out(1,"Top 5", 34.91d0, OOPmedShareUpper7584(2)*100, tab)      
        call Out(1,"Top 1", 16.36d0, OOPmedShareUpper7584(3)*100, tab)            
        write(1,'(a)',advance='no'), ret
        call Out(1,"OOP Health Expense Gini for 85+ Year-Olds", 0.755d0, OOPExpGini85plus, tab)
        call Out(1,"OOP Health Expense Quintiles for 85+ Year-Olds")
        call Out(1,"First", 0.72d0, OOPmedShare85plus(1)*100, tab)
        call Out(1,"Second", 2.25d0, OOPmedShare85plus(2)*100, tab)
        call Out(1,"Third", 5.84d0, OOPmedShare85plus(3)*100, tab)
        call Out(1,"Fourth", 11.99d0, OOPmedShare85plus(4)*100, tab)    
        call Out(1,"Fifth", 79.20d0, OOPmedShare85plus(5)*100, tab)  
        call Out(1,"Share Paid by")        
        call Out(1,"Top 10", 66.26d0, OOPmedShareUpper85plus(1)*100, tab)      
        call Out(1,"Top 5", 51.53d0, OOPmedShareUpper85plus(2)*100, tab)      
        call Out(1,"Top 1", 22.34d0, OOPmedShareUpper85plus(3)*100, tab)            
        write(1,'(a)',advance='no'), ret             
        call Out(1,"Wealth Gini", 0.80d0, WealthGini, tab)
        call Out(1,"Wealth Quintiles - Entire Population")
        call Out(1,"First", -0.3d0, WealthShares(1)*100, tab)
        call Out(1,"Second", 1.3d0, WealthShares(2)*100, tab)
        call Out(1,"Third", 5.0d0, WealthShares(3)*100, tab)
        call Out(1,"Fourth", 12.2d0, WealthShares(4)*100, tab)    
        call Out(1,"Fifth", 81.7d0, WealthShares(5)*100, tab)  
        call Out(1,"Share Paid by")        
        call Out(1,"Top 10", 69.1d0, WealthSharesUpper(1)*100, tab)      
        call Out(1,"Top 5", 57.8d0, WealthSharesUpper(2)*100, tab)      
        call Out(1,"Top 1", 34.7d0, WealthSharesUpper(3)*100, tab)        
        call Out(1,"Fraction 65+ in Each Wealth Quintile")
        call Out(1,"First", 0.0d0, Frac65plusShares(1)*100, tab)
        call Out(1,"Second", 0.0d0, Frac65plusShares(2)*100, tab)
        call Out(1,"Third", 0.0d0, Frac65plusShares(3)*100, tab)
        call Out(1,"Fourth", 0.0d0, Frac65plusShares(4)*100, tab)    
        call Out(1,"Fifth", 0.0d0, Frac65plusShares(5)*100, tab) 
        call Out(1,"Wealth Gini Working-Age Households", 0.0d0, WealthGiniWorkingHHs, tab)       
        call Out(1,"Wealth Gini 65-66", 0.0d0, WealthGini6566, tab)                           
        write(1,'(a)',advance='no'), ret                     
        call Out(1,"Earnings Gini", 0.61d0, EarnsGini, tab)
        call Out(1,"Earnings Quintiles - Entire Population")
        call Out(1,"First", -0.20d0, EarnsShares(1)*100, tab)
        call Out(1,"Second", 4.0d0, EarnsShares(2)*100, tab)
        call Out(1,"Third", 13.0d0, EarnsShares(3)*100, tab)
        call Out(1,"Fourth", 22.9d0, EarnsShares(4)*100, tab)    
        call Out(1,"Fifth", 60.2d0, EarnsShares(5)*100, tab)  
        call Out(1,"Share Paid by")        
        call Out(1,"Top 10", 69.1d0, EarnsSharesUpper(1)*100, tab)      
        call Out(1,"Top 5", 57.8d0, EarnsSharesUpper(2)*100, tab)      
        call Out(1,"Top 1", 34.7d0, EarnsSharesUpper(3)*100, tab)   
        write(1,'(a)',advance='no'), ret                   
        !call Out(1,"Lifetime Earnings Gini", 0.42d0, LifetimeEarnsGini6566, tab)
        !call Out(1,"Lifetime Earnings Quintiles - 65 Year-olds")
        !call Out(1,"First", 4.4d0, LifetimeEarnsShares(1)*100, tab)
        !call Out(1,"Second", 9.8d0, LifetimeEarnsShares(2)*100, tab)
        !call Out(1,"Third", 15.5d0, LifetimeEarnsShares(3)*100, tab)
        !call Out(1,"Fourth", 23.5d0, LifetimeEarnsShares(4)*100, tab)    
        !call Out(1,"Fifth", 46.9d0, LifetimeEarnsShares(5)*100, tab)  
        !call Out(1,"Share Paid by")        
        !call Out(1,"Top 10", 30.2d0, LifetimeEarnsSharesUpper(1)*100, tab)      
        !call Out(1,"Top 5", 19.5d0, LifetimeEarnsSharesUpper(2)*100, tab)      
        !call Out(1,"Top 1", 7.5d0, LifetimeEarnsSharesUpper(3)*100, tab)                                                          
        write(1,'(a)',advance='no'), ret            
        call Out(1,"SocSecIncGini6566", 0.34d0, SocSecIncGini6566, tab)
        call Out(1,"Soc Sec Income Quintiles - 65-66 Year-olds")
        call Out(1,"First", 5.3d0, SocSecIncShares6566(1)*100, tab)
        call Out(1,"Second", 12.0d0, SocSecIncShares6566(2)*100, tab)
        call Out(1,"Third", 18.4d0, SocSecIncShares6566(3)*100, tab)
        call Out(1,"Fourth", 25.4d0, SocSecIncShares6566(4)*100, tab)    
        call Out(1,"Fifth", 39.0d0, SocSecIncShares6566(5)*100, tab)  
        call Out(1,"Share Paid by")        
        call Out(1,"Top 10", 22.2d0, SocSecIncSharesUpper6566(1)*100, tab)      
        call Out(1,"Top 5", 12.2d0, SocSecIncSharesUpper6566(2)*100, tab)      
        call Out(1,"Top 1", 3.1d0, SocSecIncSharesUpper6566(3)*100, tab)                                                          
        write(1,'(a)',advance='no'), ret                                                             
        write(1,'(a)',advance='no'), ret
        call Out(1,"Soc Sec Income Gini", 0.27d0, SocSecIncGini, tab)
        call Out(1,"Soc Sec Income Quintiles - All retirees")
        call Out(1,"First", 8.2d0, SocSecIncShares(1)*100, tab)
        call Out(1,"Second", 14.2d0, SocSecIncShares(2)*100, tab)
        call Out(1,"Third", 18.6d0, SocSecIncShares(3)*100, tab)
        call Out(1,"Fourth", 24.5d0, SocSecIncShares(4)*100, tab)    
        call Out(1,"Fifth", 34.5d0, SocSecIncShares(5)*100, tab)  
        call Out(1,"Share Paid by")        
        call Out(1,"Top 10", 19.3d0, SocSecIncSharesUpper(1)*100, tab)      
        call Out(1,"Top 5", 10.6d0, SocSecIncSharesUpper(2)*100, tab)      
        call Out(1,"Top 1", 2.5d0, SocSecIncSharesUpper(3)*100, tab)                                                          
        write(1,'(a)',advance='no'), ret                                                             
        write(1,'(a)',advance='no'), ret                       

        write(1,'(a)'), "Average OOP Expenses/Overall Average HH Expenses by Soc Sec Inc Quintile: Married"        
        call Out(1, "Q1",  1.04d0, AggregateOOPSSQ(1,1),tab)           
        call Out(1, "Q2", 1.19d0, AggregateOOPSSQ(2,1),tab)           
        call Out(1, "Q3", 1.28d0, AggregateOOPSSQ(3,1),tab)           
        call Out(1, "Q4", 1.35d0, AggregateOOPSSQ(4,1),tab)                                    
        call Out(1, "Q5", 1.53d0, AggregateOOPSSQ(5,1),tab)                
        write(1,'(a)'), "Average OOP Expenses by Soc Sec Inc Quintile: Widow"        
        call Out(1, "Q1", 0.54d0, AggregateOOPSSQ(1,2),tab)     
        call Out(1, "Q2", 0.79d0, AggregateOOPSSQ(2,2),tab)    
        call Out(1, "Q3", 0.85d0, AggregateOOPSSQ(3,2),tab)    
        call Out(1, "Q4", 1.06d0, AggregateOOPSSQ(4,2),tab)                             
        call Out(1, "Q5", 1.11d0, AggregateOOPSSQ(5,2),tab)         
        write(1,'(a)'), "Average OOP Expenses by Soc Sec Inc Quintile: Widower"        
        call Out(1, "Q1", 0.47d0, AggregateOOPSSQ(1,3),tab)    
        call Out(1, "Q2", 0.59d0, AggregateOOPSSQ(2,3),tab)    
        call Out(1, "Q3", 0.72d0, AggregateOOPSSQ(3,3),tab)    
        call Out(1, "Q4", 0.95d0, AggregateOOPSSQ(4,3),tab)                         
        call Out(1, "Q5", 0.98d0, AggregateOOPSSQ(5,3),tab)         
        write(1,'(a)',advance='no') ret 
                       
        call Out(1,"AveTotExps: Widows/Married", 0.69d0, AveTotExpsByMart(2)/AveTotExpsByMart(1),tab)
        call Out(1,"AveTotExps: Widowers/Married", 0.66d0, AveTotExpsByMart(3)/AveTotExpsByMart(1),tab)
        call Out(1,"AveOOPExps: Widows/Married", 0.63d0, AveOOPExpsByMart(2)/AveOOPExpsByMart(1),tab)     
        call Out(1,"AveOOPExps: Widowers/Married", 0.55d0, AveOOPExpsByMart(3)/AveOOPExpsByMart(1),tab)
        call Out(1,"AveTotExps: BB Married/GG Married", 1.95d0, AveTotExpsByHealMart(1,1)/AveTotExpsByHealMart(4,1),tab)
        call Out(1,"AveTotExps: BG Married/GG Married", 1.50d0, AveTotExpsByHealMart(2,1)/AveTotExpsByHealMart(4,1),tab)
        call Out(1,"AveTotExps: GB Married/GG Married", 1.34d0, AveTotExpsByHealMart(3,1)/AveTotExpsByHealMart(4,1),tab)
        call Out(1,"AveTotExps: Bad Health Widows/Good Health Widows", 2.84d0, AveTotExpsByHealMart(1,2)/AveTotExpsByHealMart(2,2),tab)
        call Out(1,"AveTotExps: Bad Health Widowers/Good Health Widowers", 3.58d0, AveTotExpsByHealMart(1,3)/AveTotExpsByHealMart(2,3),tab)
        call Out(1,"AveOOPExps: BB Married/GG Married", 1.52d0, AveOOPExpsByHealMart(1,1)/AveOOPExpsByHealMart(4,1),tab)
        call Out(1,"AveOOPExps: BG Married/GG Married", 1.21d0, AveOOPExpsByHealMart(2,1)/AveOOPExpsByHealMart(4,1),tab)
        call Out(1,"AveOOPExps: GB Married/GG Married", 1.25d0, AveOOPExpsByHealMart(3,1)/AveOOPExpsByHealMart(4,1),tab)
        call Out(1,"AveOOPExps: Bad Health Widows/Good Health Widows", 2.18d0, AveOOPExpsByHealMart(1,2)/AveOOPExpsByHealMart(2,2),tab)
        call Out(1,"AveOOPExps: Bad Health Widowers/Good Health Widowers", 2.45d0, AveOOPExpsByHealMart(1,3)/AveOOPExpsByHealMart(2,3),tab)
        
        write(1,'(a)',advance='no') ret 
        write(1,'(a)',advance='no') ret     
        call Out(1,"MaritalDist Age65 PEM", MaritalDistAge65PEM,"MaritalDist Age65 Data", MaritalDistAge65Data)  
        call Out(1,"MaritalDist Age65 SSincome", MaritalDistAge65SSInc,"MaritalDist Age65 Data", MaritalDistAge65SSIncData)          
        call Out(1,"AveEarnsQuintileCutoffs",AveEarnsQuintileCutoffs,tab)            
        call Out(1,"MedMobMat",MedMobMat,"MedMobMat85plus",MedMobMat85plus)
        call Out(1,"OOPMobMat",OOPMobMat,"OOPMobMat85plus",OOPMobMat85plus)        
        call Out(1,"WealthMobMatHH2yr65plus",WealthMobMatHH2yr65plus)
        call Out(1,"WealthMobMatHH2yr65plusCondPEQ1",WealthMobMatHH2yr65plusCondPEQ(:,:,1))  
        call Out(1,"WealthMobMatHH2yr65plusCondPEQ2",WealthMobMatHH2yr65plusCondPEQ(:,:,2))  
        call Out(1,"WealthMobMatHH2yr65plusCondPEQ3",WealthMobMatHH2yr65plusCondPEQ(:,:,3))  
        call Out(1,"WealthMobMatHH2yr65plusCondPEQ4",WealthMobMatHH2yr65plusCondPEQ(:,:,4))  
        call Out(1,"WealthMobMatHH2yr65plusCondPEQ5",WealthMobMatHH2yr65plusCondPEQ(:,:,5))        
        call Out(1,"WealthMobMatHH2yr6574",WealthMobMatHH2yr6574,"WealthMobMatHH2yr7584",WealthMobMatHH2yr7584,"WealthMobMatHH2yr85plus",WealthMobMatHH2yr85plus)        
        call Out(1,"WealthMobMatHH2yr8594",WealthMobMatHH2yr8594)  
        !call Out(1,"WealthMobMatHH4yr65plus",WealthMobMatHH4yr65plus)
        !call Out(1,"WealthMobMatHH4yr6574",WealthMobMatHH4yr6574,"WealthMobMatHH4yr7584",WealthMobMatHH4yr7584,"WealthMobMatHH4yr85plus",WealthMobMatHH4yr85plus)        
        !call Out(1,"WealthMobMatHH6yr65plus",WealthMobMatHH6yr65plus)
        !call Out(1,"WealthMobMatHH6yr6574",WealthMobMatHH6yr6574,"WealthMobMatHH6yr7584",WealthMobMatHH6yr7584,"WealthMobMatHH6yr85plus",WealthMobMatHH6yr85plus)        
        !call Out(1,"WealthMobMatHH8yr65plus",WealthMobMatHH8yr65plus)
        !call Out(1,"WealthMobMatHH8yr6574",WealthMobMatHH8yr6574,"WealthMobMatHH8yr7584",WealthMobMatHH8yr7584,"WealthMobMatHH8yr85plus",WealthMobMatHH8yr85plus)        
        call Out(1,"WealthMobMatHH2yr",WealthMobMatHH2yr)
        call Out(1,"WealthMobMatInd2yr6574",WealthMobMatInd2yr6574,"WealthMobMatInd2yr7584",WealthMobMatInd2yr7584,"WealthMobMatInd2yr85plus",WealthMobMatInd2yr85plus)         
        !call Out(1,"WealthMobMatHH4yr",WealthMobMatHH4yr) 
        !call Out(1,"WealthMobMatHH6yr",WealthMobMatHH6yr) 
        !call Out(1,"WealthMobMatHH8yr",WealthMobMatHH8yr) 
        call Out(1,"WealthMobMatHH2yr2164",WealthMobMatHH2yr2164) 
        !call Out(1,"WealthMobMatHH4yr2164",WealthMobMatHH4yr2164) 
        !call Out(1,"WealthMobMatHH6yr2164",WealthMobMatHH6yr2164) 
        !call Out(1,"WealthMobMatHH8yr2164",WealthMobMatHH8yr2164) 
        call Out(1,"WealthMobMatCondNH6574",WealthMobMatCondNH6574) 
        call Out(1,"WealthMobMatCondNH7584",WealthMobMatCondNH7584) 
        call Out(1,"WealthMobMatCondNH85plus",WealthMobMatCondNH85plus)                        
        call Out(1,"WealthMobMatCondHosp6574",WealthMobMatCondHosp6574) 
        call Out(1,"WealthMobMatCondHosp7584",WealthMobMatCondHosp7584) 
        call Out(1,"WealthMobMatCondHosp85plus",WealthMobMatCondHosp85plus)  
        call Out(1,"WealthMobMatCondBadHeal6574",WealthMobMatCondBadHeal6574) 
        call Out(1,"WealthMobMatCondBadHeal7584",WealthMobMatCondBadHeal7584) 
        call Out(1,"WealthMobMatCondBadHeal85plus",WealthMobMatCondBadHeal85plus) 
        call Out(1,"WealthMobMatCondWidow6574",WealthMobMatCondWidow6574) 
        call Out(1,"WealthMobMatCondWidow7584",WealthMobMatCondWidow7584) 
        call Out(1,"WealthMobMatCondWidow85plus",WealthMobMatCondWidow85plus)
        call Out(1,"WealthMobMatCondWidower6574",WealthMobMatCondWidower6574) 
        call Out(1,"WealthMobMatCondWidower7584",WealthMobMatCondWidower7584) 
        call Out(1,"WealthMobMatCondWidower85plus",WealthMobMatCondWidower85plus)        
        call Out(1,"WealthMobMatCondSpousalDeath6574",WealthMobMatCondSpousalDeath6574) 
        call Out(1,"WealthMobMatCondSpousalDeath7584",WealthMobMatCondSpousalDeath7584) 
        call Out(1,"WealthMobMatCondSpousalDeath85plus",WealthMobMatCondSpousalDeath85plus)                                      
   
        write(1,'(a)',advance='no') ret 
        write(1,'(a)',advance='no') ret  
        write(1,'(a)'), "Statistics by Age"      
        call Out(1, "CohortWeights ID", CohortWeights,ret)                          
        call Out(1, "FracWomenLabForByAge ID", FracWomenLabForByAge,ret) 
        call Out(1, "AveFemaleHoursByAge ID", AveFemaleHoursByAge,ret) 
        call Out(1, "AveFemaleHoursCondWorkByAge ID", AveFemaleHoursCondWorkByAge,ret) 
        call Out(1, "AveEarnsFbyAge Sim", AveEarnsFbyAge, ret)
        call Out(1, "AveEarnsMbyAge Sim", AveEarnsMbyAge, ret)
        call Out(1, "AveEarnsFbyAge ID", AveEarnsFbyAgeID, ret)
        call Out(1, "AveEarnsMbyAge ID", AveEarnsMbyAgeID, ret)        
        call Out(1, "EarnsFbyAge Sim", EarnsFbyAge, ret)
        call Out(1, "EarnsMbyAge Sim", EarnsMbyAge, ret)
        call Out(1, "EarnsFbyAge ID", EarnsFbyAgeID, ret)
        call Out(1, "EarnsMbyAge ID", EarnsMbyAgeID, ret)
        call Out(1, "WealthByAge Sim", WealthByAge,ret)        
        call Out(1, "WealthByAge ID2", WealthByAgeID2,ret)        
        call Out(1, "ConsByAge Sim", ConsByAge,ret) 
        call Out(1, "ConsByAge ID", ConsByAgeID,ret) 
        call Out(1, "SocSecByAge Sim", SocSecByAge,ret)
        call Out(1, "SocSecByAge ID", SocSecByAgeID,ret)      
        call Out(1, "TotMedByAge Sim", TotMedByAge,ret)  
        call Out(1, "TotMedByAge ID", TotMedByAgeID,ret) 

        call Out(1, "AveNHExpensesWidows", AveNHExpensesWidows,ret) 
        call Out(1, "AveNonNHExpensesWidows", AveNonNHExpensesWidows,ret) 
        call Out(1, "AveExpensesWidows", AveExpensesWidows,ret)                 
        call Out(1, "AveNHExpensesWidowers", AveNHExpensesWidowers,ret) 
        call Out(1, "AveNonNHExpensesWidowers", AveNonNHExpensesWidowers,ret) 
        call Out(1, "AveExpensesWidowers", AveExpensesWidowers,ret) 
        call Out(1, "AveNHExpensesMarried", AveNHExpensesMarried,ret) 
        call Out(1, "AveNonNHExpensesMarried", AveNonNHExpensesMarried,ret) 
        call Out(1, "AveExpensesMarried", AveExpensesMarried,ret)    
        call Out(1, "FracNHMedicaid", FracNHMedicaid,ret)    
        call Out(1, "MedExpsNHMedicaid", MedExpsNHMedicaid,ret)    
        call Out(1, "ConsNHMedicaid", ConsNHMedicaid,ret) 
        call Out(1, "FracNHPrivate", FracNHPrivate,ret)    
        call Out(1, "MedExpsNHPrivate", MedExpsNHPrivate,ret)    
        call Out(1, "ConsNHPrivate", ConsNHPrivate,ret)                                         
        call Out(1, "FracPosOOPByAge ID", FracPosOOPByAge,ret)  
        call Out(1, "FracHHMedicaidByAge ID", FracMedicaidByAge,ret)
        call Out(1, "FracIndMedicaidByAge ID", FracIndMedicaidByAge,ret)
        call Out(1, "FracWidowsMedicaidByAge ID", FracWidowsMedicaidByAge,ret)
        call Out(1, "FracWidowersMedicaidByAge ID", FracWidowersMedicaidByAge,ret)
        call Out(1, "FracMarriedMedicaidByAge ID", FracMarriedMedicaidByAge,ret)
        call Out(1, "FracIndGovtTransByAge ID", FracIndGovtTransByAgeID,ret)           
        call Out(1, "MeanOOPExpByAge ID", MeanOOPExpByAge,ret)
        call Out(1, "OOPMedByAge Sim", OOPMedByAge,ret)        
        call Out(1, "MeanOOPExpByAgeCond ID", MeanOOPExpByAgeCond,ret)
        call Out(1, "FracHHTopMedShockByAge ID", FracHHTopMedShockByAge,ret)        
        call Out(1, "AveUtilByAge ID", AveUtilByAgeID,ret)
        call Out(1, "AveUtilByEduc ID ", AveUtilByEducID,ret)  
        call Out(1, "AveUtilByAge Sim", AveUtilByAge,ret)
        call Out(1, "AveUtilByAgeEduc Sim", AveUtilByAgeEduc) 
        call Out(1, "AveUtilConsByAge ID", AveUtilConsByAgeID,ret)
        call Out(1, "AveUtilConsByEduc ID ", AveUtilConsByEducID,ret)  
        call Out(1, "AveUtilConsByAge Sim", AveUtilConsByAge,ret)
        call Out(1, "AveUtilConsByAgeEduc Sim", AveUtilConsByAgeEduc)
        call Out(1, "AveMaleWagesByAgeEduc ID", AveMaleWagesByAgeEduc)
        call Out(1, "AveFemaleWagesByAgeEduc ID", AveFemaleWagesByAgeEduc)         
        call Out(1, "AveFemaleHoursByAgeEduc ID", AveFemaleHoursByAgeEduc)  
        call Out(1, "FracWomenLabForByAgeEduc ID", FracWomenLabForByAgeEduc)                    
        call Out(1, "WealthByAgeEduc Sim", WealthByAgeEduc)    
        call Out(1, "ConsByAgeEduc Sim", ConsByAgeEduc)    
        call Out(1, "FracRecGovTransByAgeEduc ID", FracRecGovTransByAgeEduc)     
        call Out(1, "Fraction at Highest Asset Point by Age", FracAtMaxAvectByAge,ret)
        call Out(1, "Fraction at Second Highest Asset Point by Age", FracAtSecMaxAvectByAge,ret)
        call Out(1, "Fraction Rets at Ave Earnings F", FracRetsAtAveEarnsF,ret)
        call Out(1, "Fraction Rets at Ave Earnings M", FracRetsAtAveEarnsM,ret)  
        call Out(1, "WealthByAgePEM Sim", WealthByAgePEM)    
        call Out(1, "ConsByAgePEM Sim", ConsByAgePEM)    
        call Out(1, "UtilByAgePEM Sim", UtilByAgePEM)    
        call Out(1, "EarnsMByAgePEM Sim", EarnsMByAgePEM)
        call Out(1, "EarnsFByAgePEM Sim", EarnsFByAgePEM) 
        call Out(1, "LabFByAgePEM Sim", LabFByAgePEM)
        call Out(1, "SocSecByAgePEM Sim", SocSecByAgePEM)     
        call Out(1, "TotMedByAgePEM Sim", TotMedByAgePEM)    
        call Out(1, "OOPExpByAgePEM Sim", OOPExpByAgePEM)     
        call Out(1, "MedicaidExpByAgePEM Sim", MedicaidExpByAgePEM)             
        call Out(1, "GovTransferByAgePEM Sim", GovTransferByAgePEM) 
        call Out(1, "FracRecGovTransByAgePEM Sim", FracRecGovTransByAgePEM) 
        call Out(1, "OOPExpGiniByAge ID", OOPExpGiniByAge, ret)            
        call Out(1, "AveUtilByAgeHealMart(:,:,1)", AveUtilByAgeHealMart(:,:,1))  
        call Out(1, "AveUtilByAgeHealMart(:,:,2)", AveUtilByAgeHealMart(:,:,2))  
        call Out(1, "AveUtilByAgeHealMart(:,:,3)", AveUtilByAgeHealMart(:,:,3))  
        call Out(1, "AveUtilFWidbyAgeWid Sim", AveUtilFWidbyAgeWid,ret) 
        call Out(1, "AveUtilMWidbyAgeWid Sim", AveUtilMWidbyAgeWid,ret)         
        !call Out(1, "FracProdLocM Sim", FracProdLocM)     
        !call Out(1, "FracProdLocF Sim", FracProdLocF) 
        !call Out(1, "FracEducLocH Sim", FracEducLocH) 
        !call Out(1, "FracProdLocM ID", FracProdLocMID)     
        !call Out(1, "FracProdLocF ID", FracProdLocFID) 
        !call Out(1, "FracEducLocH ID", FracEducLocHID)                                                                      
        !call Out(1, "WealthByAgeAveMedExp Sim", WealthByAgeAveMedExp)    
        !call Out(1, "ConsByAgeAveMedExp Sim", ConsByAgeAveMedExp)    
        !call Out(1, "UtilByAgeAveMedExp Sim", UtilByAgeAveMedExp)    
        !call Out(1, "TotMedByAgeAveMedExp Sim", TotMedByAgeAveMedExp)
        !call Out(1, "OOPExpByAgeAveMedExp Sim", OOPExpByAgeAveMedExp) 
        !call Out(1, "MedicaidExpByAgeAveMedExp Sim", MedicaidExpByAgeAveMedExp)
        !call Out(1, "GovTransferByAgeAveMedExp Sim", GovTransferByAgeAveMedExp)      
        
        GovSpending = (IncomeTaxes + SSTaxes  +CapitalTaxes + EarnsTaxes - GovTransfers - TotalPayments)/AggOutput
        write(1,'(a)',advance='no'), ret   
        write(1,'(a)',advance='no'), ret   
        write(1,'(a)') "Experiments Data"        
        call Out(1,"Y",AggOutput,tab)     
        write(1,'(a)',advance='no'), ret   
        call Out(1,"C", AggCons,tab) 
        write(1,'(a)',advance='no'), ret               
        call Out(1,"K supply", AggCapitalSupply, tab)       
        write(1,'(a)',advance='no'), ret  
        call Out(1,"K demand", AggCapitalDemand, tab)       
        write(1,'(a)',advance='no'), ret          
        call Out(1,"2K/Y (annual K/Y)", AggCapitalSupply/AggOutput*2.0d0, tab)       
        write(1,'(a)',advance='no'), ret
        call Out(1,"AggLaborSupply", AggLaborSupply, tab)       
        write(1,'(a)',advance='no'), ret 
        call Out(1,"AveFemaleEarnings", AveFemaleEarnings, tab)       
        write(1,'(a)',advance='no'), ret   
        call Out(1,"AveHHEarnings", AveHHEarnings, tab)       
        write(1,'(a)',advance='no'), ret  
        call Out(1,"AveEarningsAll", AveEarningsAll, tab)       
        write(1,'(a)',advance='no'), ret                                
        call Out(1,"G",GovSpending*AggOutput,tab)
        write(1,'(a)',advance='no'), ret             
        call Out(1,"G/Y  (%)",GovSpending*100.0d0,tab)
        write(1,'(a)',advance='no'), ret      
        call Out(1,"AddGovConsumption",AddGovConsumption,tab)     
        write(1,'(a)',advance='no'), ret                
        call Out(1,"GovTransfers",GovTransfers,tab)        
        write(1,'(a)',advance='no'), ret   
        call Out(1,"GovTransfers/Y (%)",GovTransfers/AggOutput*100.0d0,tab)        
        write(1,'(a)',advance='no'), ret   
        call Out(1,"SS Payments",TotalPayments,tab)        
        write(1,'(a)',advance='no'), ret   
        call Out(1,"SS Payments/Y (%)",TotalPayments/AggOutput*100.0d0,tab)        
        write(1,'(a)',advance='no'), ret  
        call Out(1,"GovTransfers + SS Payments",GovTransfers + TotalPayments,tab)        
        write(1,'(a)',advance='no'), ret   
        call Out(1,"(GovTransfers+SS Payments)/Y (%)",(GovTransfers + TotalPayments)/AggOutput*100.0d0,tab)        
        write(1,'(a)',advance='no'), ret 
        call Out(1,"TotalSSEarnsTaxRevenue",SSTaxes,tab)        
        write(1,'(a)',advance='no'), ret      
        call Out(1,"AdditionalLaborEarnsTaxRevenue",EarnsTaxes,tab)        
        write(1,'(a)',advance='no'), ret            
        call Out(1,"TotalSSEarnsTaxRevenue/Y (%)",(SSTaxes)/AggOutput*100.0d0,tab)        
        write(1,'(a)',advance='no'), ret  
        call Out(1,"TotalIncomeTaxRevenue",IncomeTaxes,tab)        
        write(1,'(a)',advance='no'), ret        
        call Out(1,"TotalIncomeTaxRevenue/Y (%)",IncomeTaxes/AggOutput*100.0d0,tab)        
        write(1,'(a)',advance='no'), ret                                 
        call Out(1,"GovTransfersToWorkers",GovTransfers-GovTransfersTo65Plus,tab)        
        write(1,'(a)',advance='no'), ret 
        call Out(1,"GovTransfersToWorkers/Y (%)",(GovTransfers-GovTransfersTo65Plus)/AggOutput*100.0d0,tab)        
        write(1,'(a)',advance='no'), ret                          
        call Out(1,"GovTransfersTo65Plus",GovTransfersTo65Plus,tab)        
        write(1,'(a)',advance='no'), ret   
        call Out(1,"GovTransfersTo65Plus/Y (%)",GovTransfersTo65Plus/AggOutput*100.0d0,tab)        
        write(1,'(a)',advance='no'), ret  
        call Out(1,"MedicaidTransfers",MedicaidTransfers,tab)        
        write(1,'(a)',advance='no'), ret   
        call Out(1,"MedicaidTransfers/Y (%)",MedicaidTransfers/AggOutput*100.0d0,tab)        
        write(1,'(a)',advance='no'), ret         
        call Out(1,"% of Workers Receiving Govt Trans", FracIndWorksGovtTrans*100, tab)
        write(1,'(a)',advance='no'), ret 
        call Out(1,"% of 65+ Receiving Govt Trans", FracIndRetsGovtTrans*100, tab)
        write(1,'(a)',advance='no'), ret                         
        call Out(1,"% of 65+ on Medicaid", FracIndMedicaid*100, tab)
        write(1,'(a)',advance='no'), ret                                   
        call Out(1, "Aggregate OOP", AggOOPExp, tab) 
        write(1,'(a)',advance='no'), ret           
        call Out(1,"OOP/Y (%)", AggOOPExp/AggOutput*100.0d0, tab)                         
        write(1,'(a)',advance='no'), ret           
        call Out(1,"OOP/Total (%)", AggOOPExp/AggMedExp*100.0d0, tab)                         
        write(1,'(a)',advance='no'), ret 
        call Out(1,"Total medical exps./Y", AggMedExp/AggOutput*100, tab)
        write(1,'(a)',advance='no'), ret               
        call Out(1,"SS tax rate",tauhatS,tab)         
        write(1,'(a)',advance='no'), ret     
        call Out(1,"PropTax",PropTax,tab)
        write(1,'(a)',advance='no'), ret     
        call Out(1,"EarnsTax",EarnsTax,tab)                 
        write(1,'(a)',advance='no'), ret                  
        write(1,'(a)',advance='no') ret    
        call Out(1,"r", r, tab)
        write(1,'(a)',advance='no') ret    
        call Out(1,"w", w, tab)
        write(1,'(a)',advance='no') ret    
        call Out(1, "Total Assets Workers", AggWorkersAssets,tab)      
        write(1,'(a)',advance='no'), ret      
        call Out(1, "Total Assets Retirees", AggRetireesAssets,tab)      
        write(1,'(a)',advance='no'), ret 
        call Out(1, "Total Assets 65 Year-olds", WealthByAge(nw+1)*CohortWeights(nw+1),tab)      
        write(1,'(a)',advance='no'), ret         
        call Out(1,"Agg Assets PE Q1",AggregateWealthPEQ(1),tab)
        write(1,'(a)',advance='no'), ret     
        call Out(1,"Agg Assets PE Q2",AggregateWealthPEQ(2),tab)
        write(1,'(a)',advance='no'), ret 
        call Out(1,"Agg Assets PE Q3",AggregateWealthPEQ(3),tab)
        write(1,'(a)',advance='no'), ret 
        call Out(1,"Agg Assets PE Q4",AggregateWealthPEQ(4),tab)
        write(1,'(a)',advance='no'), ret 
        call Out(1,"Agg Assets PE Q5",AggregateWealthPEQ(5),tab)
        write(1,'(a)',advance='no'), ret      
        call Out(1,"Agg Workers' Assets PE Q1",AggWorkersAssetsPEQ(1),tab)
        write(1,'(a)',advance='no'), ret     
        call Out(1,"Agg Workers' Assets PE Q2",AggWorkersAssetsPEQ(2),tab)
        write(1,'(a)',advance='no'), ret 
        call Out(1,"Agg Workers' Assets PE Q3",AggWorkersAssetsPEQ(3),tab)
        write(1,'(a)',advance='no'), ret 
        call Out(1,"Agg Workers' Assets PE Q4",AggWorkersAssetsPEQ(4),tab)
        write(1,'(a)',advance='no'), ret 
        call Out(1,"Agg Workers' Assets PE Q5",AggWorkersAssetsPEQ(5),tab)
        write(1,'(a)',advance='no'), ret 
        call Out(1,"Agg Retirees' Assets PE Q1",AggRetireesAssetsPEQ(1),tab)
        write(1,'(a)',advance='no'), ret     
        call Out(1,"Agg Retirees' Assets PE Q2",AggRetireesAssetsPEQ(2),tab)
        write(1,'(a)',advance='no'), ret 
        call Out(1,"Agg Retirees' Assets PE Q3",AggRetireesAssetsPEQ(3),tab)
        write(1,'(a)',advance='no'), ret 
        call Out(1,"Agg Retirees' Assets PE Q4",AggRetireesAssetsPEQ(4),tab)
        write(1,'(a)',advance='no'), ret 
        call Out(1,"Agg Retirees' Assets PE Q5",AggRetireesAssetsPEQ(5),tab)
        write(1,'(a)',advance='no'), ret                        
        call Out(1,"Agg Consumption PE Q1",AggregateConsPEQ(1),tab)
        write(1,'(a)',advance='no'), ret      
        call Out(1,"Agg Consumption PE Q2",AggregateConsPEQ(2),tab)
        write(1,'(a)',advance='no'), ret         
        call Out(1,"Agg Consumption PE Q3",AggregateConsPEQ(3),tab)
        write(1,'(a)',advance='no'), ret      
        call Out(1,"Agg Consumption PE Q4",AggregateConsPEQ(4),tab)
        write(1,'(a)',advance='no'), ret      
        call Out(1,"Agg Consumption PE Q5",AggregateConsPEQ(5),tab)
        write(1,'(a)',advance='no'), ret 
        call Out(1,"Aggregate Retirees Consumption PE Q1",AggregateRetConsPEQ(1),tab)
        write(1,'(a)',advance='no'), ret      
        call Out(1,"Aggregate Retirees Consumption PE Q2",AggregateRetConsPEQ(2),tab)
        write(1,'(a)',advance='no'), ret         
        call Out(1,"Aggregate Retirees Consumption PE Q3",AggregateRetConsPEQ(3),tab)
        write(1,'(a)',advance='no'), ret      
        call Out(1,"Aggregate Retirees Consumption PE Q4",AggregateRetConsPEQ(4),tab)
        write(1,'(a)',advance='no'), ret      
        call Out(1,"Aggregate Retirees Consumption PE Q5",AggregateRetConsPEQ(5),tab)        
        write(1,'(a)',advance='no'), ret          
        call Out(1,"Average Medicaid Transfers PE Q1",MeanMedicaidPEQ(1),tab)
        write(1,'(a)',advance='no'), ret     
        call Out(1,"Average Medicaid Transfers PE Q2",MeanMedicaidPEQ(2),tab)
        write(1,'(a)',advance='no'), ret 
        call Out(1,"Average Medicaid Transfers PE Q3",MeanMedicaidPEQ(3),tab)
        write(1,'(a)',advance='no'), ret 
        call Out(1,"Average Medicaid Transfers PE Q4",MeanMedicaidPEQ(4),tab)
        write(1,'(a)',advance='no'), ret 
        call Out(1,"Average Medicaid Transfers PE Q5",MeanMedicaidPEQ(5),tab)
        write(1,'(a)',advance='no'), ret                           
        call Out(1,"OOP PE Q1",AggregateOOPPEQ(1),tab)
        write(1,'(a)',advance='no'), ret     
        call Out(1,"OOP PE Q2",AggregateOOPPEQ(2),tab)
        write(1,'(a)',advance='no'), ret 
        call Out(1,"OOP PE Q3",AggregateOOPPEQ(3),tab)
        write(1,'(a)',advance='no'), ret 
        call Out(1,"OOP PE Q4",AggregateOOPPEQ(4),tab)
        write(1,'(a)',advance='no'), ret 
        call Out(1,"OOP PE Q5",AggregateOOPPEQ(5),tab)
        write(1,'(a)',advance='no'), ret  
        write(1,'(a)'), "SS replacement rate at Age 65 for Married"                                                            
        call Out(1,"PE Q1",SocSecRepRateAge65(1,1),tab)
        write(1,'(a)',advance='no'), ret     
        call Out(1,"PE Q2",SocSecRepRateAge65(2,1),tab)
        write(1,'(a)',advance='no'), ret 
        call Out(1,"PE Q3",SocSecRepRateAge65(3,1),tab)
        write(1,'(a)',advance='no'), ret 
        call Out(1,"PE Q4",SocSecRepRateAge65(4,1),tab)
        write(1,'(a)',advance='no'), ret 
        call Out(1,"PE Q5",SocSecRepRateAge65(5,1),tab)
        write(1,'(a)',advance='no'), ret    
        write(1,'(a)'), "SS replacement rate at Age 65 for Widows"                                                            
        call Out(1,"PE Q1",SocSecRepRateAge65(1,2),tab)
        write(1,'(a)',advance='no'), ret     
        call Out(1,"PE Q2",SocSecRepRateAge65(2,2),tab)
        write(1,'(a)',advance='no'), ret 
        call Out(1,"PE Q3",SocSecRepRateAge65(3,2),tab)
        write(1,'(a)',advance='no'), ret 
        call Out(1,"PE Q4",SocSecRepRateAge65(4,2),tab)
        write(1,'(a)',advance='no'), ret 
        call Out(1,"PE Q5",SocSecRepRateAge65(5,2),tab)
        write(1,'(a)',advance='no'), ret
        write(1,'(a)'), "SS replacement rate at Age 65 for Widowers"                                                            
        call Out(1,"PE Q1",SocSecRepRateAge65(1,3),tab)
        write(1,'(a)',advance='no'), ret     
        call Out(1,"PE Q2",SocSecRepRateAge65(2,3),tab)
        write(1,'(a)',advance='no'), ret 
        call Out(1,"PE Q3",SocSecRepRateAge65(3,3),tab)
        write(1,'(a)',advance='no'), ret 
        call Out(1,"PE Q4",SocSecRepRateAge65(4,3),tab)
        write(1,'(a)',advance='no'), ret 
        call Out(1,"PE Q5",SocSecRepRateAge65(5,3),tab)
        write(1,'(a)',advance='no'), ret                                                             
        write(1,'(a)'), "Fraction of Workers Rec Govt Transfers by Quintile"        
        call Out(1, "Q1", FracWorkersRecGovTrans(1),tab)    
        write(1,'(a)',advance='no'), ret 
        call Out(1, "Q2", FracWorkersRecGovTrans(2),tab)    
        write(1,'(a)',advance='no'), ret 
        call Out(1, "Q3", FracWorkersRecGovTrans(3),tab)    
        write(1,'(a)',advance='no'), ret 
        call Out(1, "Q4", FracWorkersRecGovTrans(4),tab)    
        write(1,'(a)',advance='no'), ret                         
        call Out(1, "Q5", FracWorkersRecGovTrans(5),tab)   
        write(1,'(a)',advance='no') ret  
        write(1,'(a)'), "Fraction of Retirees Rec Govt Transfers by Quintile"        
        call Out(1, "Q1", FracRetireesRecGovTrans(1),tab)    
        write(1,'(a)',advance='no'), ret 
        call Out(1, "Q2", FracRetireesRecGovTrans(2),tab)    
        write(1,'(a)',advance='no'), ret 
        call Out(1, "Q3", FracRetireesRecGovTrans(3),tab)    
        write(1,'(a)',advance='no'), ret 
        call Out(1, "Q4", FracRetireesRecGovTrans(4),tab)    
        write(1,'(a)',advance='no'), ret                         
        call Out(1, "Q5", FracRetireesRecGovTrans(5),tab)   
        write(1,'(a)',advance='no') ret     
        write(1,'(a)'), "Aggregate OOP Expenses by Quintile"        
        call Out(1, "Q1", AggregateOOPPEQ(1),tab)    
        write(1,'(a)',advance='no'), ret 
        call Out(1, "Q2", AggregateOOPPEQ(2),tab)    
        write(1,'(a)',advance='no'), ret 
        call Out(1, "Q3", AggregateOOPPEQ(3),tab)    
        write(1,'(a)',advance='no'), ret 
        call Out(1, "Q4", AggregateOOPPEQ(4),tab)    
        write(1,'(a)',advance='no'), ret                         
        call Out(1, "Q5", AggregateOOPPEQ(5),tab)   
        write(1,'(a)',advance='no'), ret                                 
        call Out(1,"Average Utility of Newborn",  AveUtilByAge(1), tab)              
        write(1,'(a)',advance='no'), ret 
        call Out(1,"Average Consumption oUtility of Newborn",  AveUtilConsByAge(1), tab)              
        write(1,'(a)',advance='no'), ret            
        write(1,'(a)'), "Average Utility by PE Quintiles"        
        call Out(1, "Q1", UtilByAgePEM(1,1),tab)    
        write(1,'(a)',advance='no'), ret 
        call Out(1, "Q2", UtilByAgePEM(1,2),tab)    
        write(1,'(a)',advance='no'), ret 
        call Out(1, "Q3", UtilByAgePEM(1,3),tab)    
        write(1,'(a)',advance='no'), ret 
        call Out(1, "Q4", UtilByAgePEM(1,4),tab)    
        write(1,'(a)',advance='no'), ret                         
        call Out(1, "Q5", UtilByAgePEM(1,5),tab)   
        write(1,'(a)',advance='no'), ret     
        write(1,'(a)'), "Average Consumption Utility by PE Quintiles"        
        call Out(1, "Q1", UtilConsByAgePEM(1,1),tab)    
        write(1,'(a)',advance='no'), ret 
        call Out(1, "Q2", UtilConsByAgePEM(1,2),tab)    
        write(1,'(a)',advance='no'), ret 
        call Out(1, "Q3", UtilConsByAgePEM(1,3),tab)    
        write(1,'(a)',advance='no'), ret 
        call Out(1, "Q4", UtilConsByAgePEM(1,4),tab)    
        write(1,'(a)',advance='no'), ret                         
        call Out(1, "Q5", UtilConsByAgePEM(1,5),tab)            
        write(1,'(a)',advance='no'), ret 
        write(1,'(a)'), "Average Utility by HH Educ Type"        
        call Out(1, "Female H Male H", AveUtilByAgeEduc(1,1),tab)    
        write(1,'(a)',advance='no'), ret 
        call Out(1, "Female H Male C", AveUtilByAgeEduc(1,2),tab)    
        write(1,'(a)',advance='no'), ret   
        call Out(1, "Female C Male H", AveUtilByAgeEduc(1,3),tab)    
        write(1,'(a)',advance='no'), ret 
        call Out(1, "Female C Male C", AveUtilByAgeEduc(1,4),tab)    
        write(1,'(a)',advance='no'), ret 
        write(1,'(a)'), "Average Consumption Utility by HH Educ Type"        
        call Out(1, "Female H Male H", AveUtilConsByAgeEduc(1,1),tab)    
        write(1,'(a)',advance='no'), ret 
        call Out(1, "Female H Male C", AveUtilConsByAgeEduc(1,2),tab)    
        write(1,'(a)',advance='no'), ret   
        call Out(1, "Female C Male H", AveUtilConsByAgeEduc(1,3),tab)    
        write(1,'(a)',advance='no'), ret 
        call Out(1, "Female C Male C", AveUtilConsByAgeEduc(1,4),tab)    
        write(1,'(a)',advance='no'), ret         
        call Out(1, "TotalTaxableIncome", TotalTaxableIncome,tab)    
        write(1,'(a)',advance='no'), ret   
        call Out(1,"AveFemaleHoursCondWork", AveFemaleHoursCondWork, tab)  
        write(1,'(a)',advance='no'), ret   
        call Out(1,"AveFemaleHoursCondWork (HH)", AveFemaleHoursCondWorkByEduc(1), tab)  
        write(1,'(a)',advance='no'), ret   
        call Out(1,"AveFemaleHoursCondWork (HC)",AveFemaleHoursCondWorkByEduc(2), tab)  
        write(1,'(a)',advance='no'), ret   
        call Out(1,"AveFemaleHoursCondWork (CH)", AveFemaleHoursCondWorkByEduc(3), tab) 
        write(1,'(a)',advance='no'), ret    
        call Out(1,"AveFemaleHoursCondWork (CC)", AveFemaleHoursCondWorkByEduc(4), tab)  
        write(1,'(a)',advance='no'), ret   
        call Out(1,"AveFemaleHours/AveMaleHours", AveFemaleHours/hbar, tab)  
        write(1,'(a)',advance='no'), ret   
        call Out(1,"AveFemaleHours", AveFemaleHours, tab)  
        write(1,'(a)',advance='no'), ret   
        call Out(1,"AveFemaleHours (HH)", AveFemaleHoursByEduc(1), tab)  
        write(1,'(a)',advance='no'), ret   
        call Out(1,"AveFemaleHours (HC)", AveFemaleHoursByEduc(2), tab)  
        write(1,'(a)',advance='no'), ret   
        call Out(1,"AveFemaleHours (CH)", AveFemaleHoursByEduc(3), tab)
        write(1,'(a)',advance='no'), ret     
        call Out(1,"AveFemaleHours (CC)", AveFemaleHoursByEduc(4), tab)  
        write(1,'(a)',advance='no'), ret   
        call Out(1,"FracWomenLaborForce", FracWomenLaborForce, tab) 
        write(1,'(a)',advance='no'), ret   
        call Out(1,"FracWomenLaborForce (HH)", FracWomenLabForByEduc(1), tab)   
        write(1,'(a)',advance='no'), ret    
        call Out(1,"FracWomenLaborForce (HC)", FracWomenLabForByEduc(2), tab) 
        write(1,'(a)',advance='no'), ret   
        call Out(1,"FracWomenLaborForce (CH)", FracWomenLabForByEduc(3), tab)    
        write(1,'(a)',advance='no'), ret   
        call Out(1,"FracWomenLaborForce (CC)", FracWomenLabForByEduc(4), tab)  
        write(1,'(a)',advance='no'), ret  
        call Out(1,"AggConsAtDeath", AggConsAtDeath,tab)
        write(1,'(a)',advance='no'), ret  
        call Out(1,"AggConsExcludeDeath", AggConsExcludeDeath,tab)                                              
        close(1)  

    end subroutine SaveResults 
end module OutputResults