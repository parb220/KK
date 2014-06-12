module parameters
    use constants
    implicit none
    save
    
    !How to remove SS in Open Economy Version 
    !1.Change flags
    !2.GovSpendingBaseline in globals should be set to baseline government spending as share of GDP
    !3.Set guesses for PropTax, AveEarningsAll, AveHHIncome
    !4.Set social security to 0 and reduce tax so that removal is revenue neutral.
    
    !How to get baseline with no Health expenses in Open Economy Version
    !1. Change flags
    !2. Recalibrate income tax shifter to get income taxes/GDP as in original baseline
    !3. Set guess on AveEarningsAll, AveHHIncome, make sure PropTax = 0.0d0   
   
    !Flags: Baseline Open Economy Version      
    !integer, parameter:: PartialEqm = 0         
    !integer, parameter:: GEexperiment = 0    
    !integer, parameter:: OpenEconomy = 1
    !integer, parameter:: RunOnce = 1
    !integer, parameter:: ExpensesZero = 0
    !integer, parameter:: UseEarnsTax = 0
    
    !Flags:GE experiments OpenEconomyVersion    
    integer, parameter:: PartialEqm = 0      
    integer, parameter:: GEexperiment = 1
    integer, parameter:: UseEarnsTax = 0
    integer, parameter:: OpenEconomy = 1   
    integer, parameter:: RunOnce = 1
    integer, parameter:: ExpensesZero = 0
    integer, parameter:: NoEarningsRisk = 1
    
    !Flags:No health expense baseline OpenEconomyVersion   
    !integer, parameter:: PartialEqm = 0      
    !integer, parameter:: GEexperiment = 0
    !integer, parameter:: OpenEconomy = 1   
    !integer, parameter:: RunOnce = 0    
    !integer, parameter:: ExpensesZero = 1
    
    !Flags:No health expense No Social Security Open Economy Version
    !integer, parameter:: PartialEqm = 0     
    !integer, parameter:: GEexperiment = 1
    !integer, parameter:: OpenEconomy = 1   
    !integer, parameter:: RunOnce = 1    
    !integer, parameter:: ExpensesZero = 1    
    
    !Flags: Baseline General Equilibrium version  
    integer, parameter:: NormalizeWage = 1 !When OpenEconomy=1 this flag doesn't matter
    
    integer, parameter:: ScaleDownSocSecBenefit  = 0
    integer, parameter:: ScaleDownMedicaidBenefit  = 0
    integer, parameter:: MoveSocSecToMedicaid = 0
    !additonal flags
    integer, parameter:: NonSeparableUtility = 0
    integer, parameter:: ProportionalSocSec = 0  
    integer, parameter:: ResultsOnly = 0
    integer, parameter:: SaveSolution = 0    
    integer, parameter:: SortMedExp = 0
    
    !number of asset grid points sent to a processor per iteration
    !workers
    integer, parameter:: count_wef = 5
    integer, parameter:: count_wem = 5    
    integer, parameter:: count_waef = 1
    integer, parameter:: count_waem = 1  
    integer, parameter:: count_wet = 1 
    !retirees    
    integer, parameter:: count_rm = 5    
    integer, parameter:: count_raef = 1
    integer, parameter:: count_raem = 1 
    integer, parameter:: count_rh = 1 
    integer, parameter:: count_rs = 1    
    !grid sizes
    integer, parameter:: nef = 5 !number of points in earnings grid
    integer, parameter:: nem = 5 !number of points in earnings grid  
    integer, parameter:: naef = 10 !10 !number of points in female average earnings grid 
    integer, parameter:: naem = 10 !10 !number of points in male average earnings grid      
    integer, parameter:: npm = 5 !5 !number of persistent shocks
    integer, parameter:: ntm = 2 !2 !number of transitory shocks    
    integer, parameter:: nm = npm*ntm !10 !total number of shocks        
    integer, parameter:: na = 50 !50 !number of points in asset grid      
    integer, parameter:: nset = 2 !number of individual education types
    integer, parameter:: nsht = 2 !number of individual health types
    integer, parameter:: nhet = 4 !number of household (FM) education types !HH, HC, CH, CC
    integer, parameter:: nhht = 4 !number of household health types !For married (FM) households: BB, BG, GB, GG !For single households: B, G
    
    integer, parameter:: nl = 10 !20 !grid size for labor supply

    !tolerances
    real(dbl), parameter:: eps = 1.d-10 !tolerance on golden section search 
    real(dbl), parameter:: fracUpdatedCapInit = 0.3d0 !fraction of updated capital determined by new capital
    real(dbl):: fracUpdatedCap = 0.3d0  !fraction of updated capital determined by new capital
    real(dbl):: tolCapital = 1.d-4 !tolerance on capital iteration
    real(dbl), parameter:: tolCapital1 = 1.d-4 !tolerance on capital iteration
    real(dbl), parameter:: tolCapital2 = 1.d-6 !tolerance on capital iteration
    real(dbl), parameter:: tolGovBudget = 1.d-8 !tolerance on government budget constraint
    real(dbl):: tolOuterLoop  = 1.d-5 !tolerance on government budget constraint
    real(dbl), parameter:: tolOuterLoop1  = 1.d-5 !tolerance on government budget constraint
    real(dbl), parameter:: tolOuterLoop2  = 1.d-5 !tolerance on government budget constraint
        
    !age structure    
    integer, parameter:: T = 40 !40 !40 !number of periods of adult life (period = 2 years so 100 - 21 + 1 = 80 years) 
    integer, parameter:: nw = 22 !22 !number of perods working (44 years)
    !integer, parameter:: T = 31 !40 !number of periods of adult life (period = 2 years so 100 - 21 + 1 = 80 years) 
    !integer, parameter:: nw = 22 !22 !number of perods working (44 years)
    integer, parameter:: nr = T - nw !18 !number of periods retired (36 years) 65+    

    !preferences !used in estimation
    real(dbl), parameter:: beta = 0.96d0 !0.85d0 !discount factor targert: interest rate
    real(dbl):: gamma = 2.0d0 !1.5d0 !frisch targe: empirical estimates of frisch for females
    real(dbl):: sigma = 2.0d0 !coefficient of risk aversion    
    real(dbl), parameter:: lambda = 0.67d0 !scaling of household consumption
    real(dbl), parameter:: hbar = 0.45d0 !male share of time spent working
    !real(dbl), parameter:: upsilon = 2.0d0 !male share of time spent working
    
    
    !technology
    real(dbl), parameter:: alpha = 0.33d0 !capital's income share    
    real(dbl), parameter:: delta = 0.145d0 !delta is set such that (I/Y)/(K/Y) = 0.21/1.5
    real(dbl), parameter:: gendergap = 0.62d0 !0.62d0  
    !earnings process    
    real(dbl), parameter:: betae0 = 2.014123d0 !-2.942d0 !2.43d0
    real(dbl), parameter:: betae1 = 0.0480392d0 !0.109d0
    real(dbl), parameter:: betae2 = -0.0008062d0 !-0.001d0
    real(dbl), parameter:: betae3 = -6.46d-07
    real(dbl), parameter:: betae4 = 0.4962578d0  !college dummy
    real(dbl), parameter:: rhoe = 0.973d0 !persistence
    real(dbl), parameter:: sigma_epse = 0.1225d0 !0.95d0 !0.85d0 !0.4d0 !error term standard deviation    
    real(dbl):: corr_epse = 0.05d0 !0.134d0 !cross-correlation of male and female earnings
    real(dbl):: corr_epse0 = 0.517d0
    real(dbl), parameter:: sigma_0 = 0.352d0
        
    !medical expenses process    
    real(dbl), parameter:: medscale = 2.36d0
    real(dbl), parameter:: rhom = 0.933d0 !0.909d0 !0.922d0 !annual persistence !2-year is rhom^2 = 0.850
    real(dbl), parameter:: sigma_epsm = 0.2120d0 !0.2157d0 !0.2243d0 !persistent component annual std. dev. !2-year is (rhom^2+(1+rhom^2)+1)^(1/2)*sigma_epsm = 0.5281
    real(dbl), parameter:: sigma_tranm = 0.8155d0 !transitory component annual std. dev. !2-year is 1.1533    
    real(dbl), parameter:: NHshock = 1.6d0 !Value of the largest shock
                                
    !demographics
    real(dbl), parameter:: ng = 0.019d0 !0.08d0 !population growth rate 
    
    real(dbl) sinitHa, sinitHaf    
    real(dbl):: sinitHb = 0.0d0 !0.1d0 !determines how fraction married changes with male lifetime earnings  
    real(dbl):: sinitHbf = 0.0d0 
    
    !government
    real(dbl), parameter:: FracMedicaidCovers = 0.80d0 
    real(dbl), parameter:: clowerbarW = 0.15d0 !consumption floor for workers     
    real(dbl):: clowerbarMarried = 0.31d0 !0.31d0   !0.001d0
    real(dbl):: clowerbarWidow = 0.33d0 !0.33d0    !0.001d0
    real(dbl):: clowerbarWidower = 0.32d0 !0.32d0 !0.001d0
    real(dbl), parameter:: ceiling = 0.0d0 
    !set to average total payroll tax (SS + Medicare) 12.4% SS + 2.9% Medicare 
    real(dbl), parameter::  tauhatS = 0.0105d0 !0.153d0 !0.0105d0  !0.029d0  !proportional labor income tax rate (to fund social security)  
    !social security payment function parameters
    !marginal payments
    real(dbl), parameter:: s1 = 0.0d0 !0.90d0
    real(dbl), parameter:: s2 = 0.0d0 !0.32d0
    real(dbl), parameter:: s3 = 0.0d0 !0.15d0
    !threshold levels as a fraction of average lifetime earnings
    real(dbl), parameter:: tau1 = 0.20d0
    real(dbl), parameter:: tau2 = 1.25d0
    real(dbl), parameter:: tau3 = 2.46d0         
    real(dbl), parameter:: shat = 0.44d0    
    real(dbl), parameter:: medded = 0.075d0 !medical expenses beyond this fraction of income reduce taxable income
    real(dbl), parameter:: emax = 2.4
    real(dbl), parameter:: SocSecReduction = 0.95d0 !0.94145d0 !0.8829d0   !0.941d0 
    real(dbl), parameter:: MedicaidReduction = 1.225d0 !0.775d0 !0.53d0 
        
    !income tax functions
    !set the tax function for retirees to estimates from "Taxation and the Household Labor Supply" by Guner et al.(2011), Table 1
    !this is the tax function as of 2000 
    !set the tax function for workers to estimates from Kaygusuz (2010) in 1980, Table 7
    !this is the tax function pre 1980's reforms 
    real(dbl), parameter:: a_mw = 0.1345d0
    real(dbl), parameter:: b_mw = 0.0971d0
    real(dbl), parameter:: a_mr = 0.113d0
    real(dbl), parameter:: b_mr = 0.073d0
    real(dbl), parameter:: a_sr = 0.153d0
    real(dbl), parameter:: b_sr = 0.057d0
    real(dbl), parameter:: incmin1s = 0.09d0 
    real(dbl), parameter:: incmin1m = 0.13d0
    real(dbl), parameter:: incbase1s = 0.53d0
    real(dbl), parameter:: incbase1m = 0.67d0
    real(dbl), parameter:: incbase2s = 0.72d0
    real(dbl), parameter:: incbase2m = 0.93d0
    real(dbl), parameter:: incshifter = -0.08d0
    
    !capital income taxes
    real(dbl), parameter:: capTax = 0.15d0
    
    !survial adjustment factors
    real(dbl), parameter:: survAdjF = 1.0142;
    real(dbl), parameter:: survAdjM = 1.0158;
    
    real(dbl), parameter:: probHusband0 = 0.0d0
    
    !simulations
    integer, parameter:: Ns = 350000   !85000 !150000  
    !distributions
    integer, parameter:: GSm = 1000 !2000 !200 !1000  
    integer, parameter:: GSe = 200 !200 !1000  
end module parameters
