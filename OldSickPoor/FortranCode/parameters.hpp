const int nef = 5; 	// number of points in earnings grid
const int nem = 5; 	// number of points in earnings grid
const int naef = 10; 	// number of points in female average earnings grid
const int naem = 10; 	// number of points in male average earnings grid
const int npm = 5; 	// number of persistent shocks
const int ntm = 2; 	// number of transitory shocks    
const int nm = npm*ntm;	// total number of shocks        
const int na = 50; 	// number of points in asset grid      
const int nset = 2; 	// number of individual education types
const int nsht = 2; 	// number of individual health types
const int nhet = 4; 	// number of household (FM) education types !HH, HC, CH, CC
const int nhht = 4; 	// number of household health types !For married (FM) households: BB, BG, GB, GG !For single households: B, G

const int T = 40;	// number of periods of adult life (period = 2 years so 100 - 21 + 1 = 80 years) 
const int nw = 22;	// number of perods working (44 years)
const int nr = T - nw; 	// number of periods retired (36 years) 65+    

