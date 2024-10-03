$ontext
DICE2023-beta-4-3-10 as of October 16, 2023
$offtext

$title October 12, 2023 (DICE2023-b-4-3-10.gms)

set        t  Time periods (5 years per period)                   /1*17/
PARAMETERS
** If optimal control
        ifopt    Indicator where optimized is 1 and base is 0     /1/
** Population and technology
        gama     Capital elasticity in production function        /.300    /
        pop1     Initial world population 2020 (millions)         /7752.9  /
        popadj   Growth rate to calibrate to 2050 pop projection  /0.145   /
        popasym  Asymptotic population (millions)                 /10825.  /
        dk       Depreciation rate on capital (per year)          /0.100    /
        q1       Initial world output 2020 (trill 2019 USD)       /135.7   /
        AL1       Initial level of total factor productivity       /5.84 /
        gA1      Initial growth rate for TFP per 5 years          /0.066   /
        delA     Decline rate of TFP per 5 years                  /0.0015  /        
** Emissions parameters and Non-CO2 GHG with sigma = emissions/output
        gsigma1   Initial growth of sigma (per year)                   / -0.015 /
        delgsig   Decline rate of gsigma per period                    /.96/
        asymgsig   Asympototic gsigma                                  /-.005/
        e1        Industrial emissions 2020 (GtCO2 per year)           / 12.5  / 
        miu1      Emissions control rate historical 2020               / .05    /
        fosslim   Maximum cumulative extraction fossil fuels (GtC)     / 6000   /
        CumEmiss0 Cumulative emissions 2020 (GtC)                      / 633.5/
* Climate damage parameters
        a1        Damage intercept                                    /0      /
        a2base    Damage quadratic term rev 01-13-23                  /0.003467/
        a3        Damage exponent                                     /2.00   /
** Abatement cost
        expcost2  Exponent of control cost function                   / 2.6  /
        pback2050 Cost of backstop 2019$ per tCO2 2050                / 515.  /
        gback     Initial cost decline backstop cost per year         / -.012 /
        cprice1   Carbon price 2020 2019$ per tCO2                    / 6    /
        gcprice   Growth rate of base carbon price per year           /.025   /
** Limits on emissions controls
        limmiu2070    Emission control limit from 2070          /1.0/
        limmiu2120    Emission control limit from 2120          /1.1/
        limmiu2200    Emission control limit from 2220          /1.05/
        limmiu2300    Emission control limit from 2300          /1.0/      
        delmiumax     Emission control delta limit per period   / 0.12/
** Preferences, growth uncertainty, and timing
        betaclim  Climate beta                                      / 0.5  /
        elasmu    Elasticity of marginal utility of consumption     / 0.95 /
        prstp     Pure rate of social time preference               /.001/
        pi        Capital risk premium                              / .05  /
        rartp       Risk-adjusted rate of time preference
        k0        Initial capital stock calibrated (1012 2019 USD)  / 295  /
        siggc1    Annual standard deviation of consumption growth   / .01 /
        sig1      Carbon intensity 2020 kgCO2-output 2020
** Scaling so that MU(C(1)) = 1 and objective function = PV consumption
        tstep       Years per Period                               / 5  /
        SRF         Scaling factor discounting                     /1000000/
        scale1      Multiplicative scaling coefficient             / 0.00891061 /
        scale2      Additive scaling coefficient                   /-6275.91/ ;
** Program control variables
sets     tfirst(t), tlast(t), tearly(t), tlate(t);
PARAMETERS
        L(t)           Level of population and labor
        aL(t)          Level of total factor productivity
        sigma(t)       CO2-emissions output ratio
        sigmatot(t)    GHG-output ratio
        gA(t)          Growth rate of productivity from
        gL(t)          Growth rate of labor and population
        gcost1         Growth of cost factor
        gsig(t)        Change in sigma (rate of decarbonization)
        eland(t)       Emissions from deforestation (GtCO2 per year)
        cost1tot(T)    Abatement cost adjusted for backstop and sigma
        pbacktime(t)   Backstop price 2019$ per ton CO2
        optlrsav       Optimal long-run savings rate used for transversality
        scc(t)         Social cost of carbon
        cpricebase(t)  Carbon price in base case
        ppm(t)         Atmospheric concentrations parts per million
        atfrac2020(t)  Atmospheric share since 2020
        atfrac1765(t)  Atmospheric fraction of emissions since 1765
        abaterat(t)    Abatement cost per net output
        miuup(t)       Upper bound on miu
        gbacktime(t)   Decline rate of backstop price
** Precautionary dynamic parameters
        varpcc(t)         Variance of per capita consumption 
        rprecaut(t)       Precautionary rate of return
        RR(t)             STP with precautionary factor
        RR1(t)            STP factor without precautionary factor;
** Time preference for climate investments and precautionary effect
        rartp           = exp( prstp + betaclim*pi)-1;
        varpcc(t)       =  min(Siggc1**2*5*(t.val-1),Siggc1**2*5*47);       
        rprecaut(t)     = -0.5*varpcc(t)*elasmu**2;
        RR1(t)          = 1/((1+rartp)**(tstep*(t.val-1)));
        RR(t)           = RR1(t)*(1+rprecaut(t))**(-tstep*(t.val-1));
        L("1") = pop1; loop(t, L(t+1)=L(t);); loop(t, L(t+1)  = L(t)*(popasym/L(t))**popadj ;);
        gA(t)           = gA1*exp(-delA*5*((t.val-1))); aL("1") = AL1; loop(t, aL(t+1)=aL(t)/((1-gA(t))););
        optlrsav        =(dk + .004)/(dk + .004*elasmu + rartp)*gama;
        cpricebase(t)   = cprice1*(1+gcprice)**(5*(t.val-1));
        pbacktime(t)    = pback2050*exp(-5*(.01)*(t.val-7));
        pbacktime(t)$(t.val > 7) = pback2050*exp(-5*(.001)*(t.val-7));
        sig1            = e1/(q1*(1-miu1)); sigma("1") = sig1;
        gsig(t)         = min(gsigma1*delgsig **((t.val-1)),asymgsig);
        loop(t, sigma(t+1)  = sigma(t)*exp(5*gsig(t)););
** Emissions limits
        miuup('1')= .05; miuup('2')= .10; miuup(t)$(t.val > 2)  = (delmiumax*(t.val-1));
        miuup(t)$(t.val > 8)  = 0.85+.05*(t.val-8); miuup(t)$(t.val > 11) = limmiu2070;
        miuup(t)$(t.val > 20) = limmiu2120; miuup(t)$(t.val > 37) = limmiu2200; miuup(t)$(t.val > 57) = limmiu2300;       
** Include file for non-CO2 GHGs
$include Include\Nonco2-b-4-3-1.gms
* Program control definitions
        tfirst(t) = yes$(t.val eq 1);
        tlast(t)  = yes$(t.val eq card(t));
VARIABLES
        MIU(t)          Emission control rate GHGs
        C(t)            Consumption (trillions 2019 US dollars per year)
        K(t)            Capital stock (trillions 2019 US dollars)
        CPC(t)          Per capita consumption (thousands 2019 USD per year)
        I(t)            Investment (trillions 2019 USD per year)
        S(t)            Gross savings rate as fraction of gross world product
        Y(t)            Gross world product net of abatement and damages (trillions 2019 USD per year)
        YGROSS(t)       Gross world product GROSS of abatement and damages (trillions 2019 USD per year)
        YNET(t)         Output net of damages equation (trillions 2019 USD per year)
        DAMAGES(t)      Damages (trillions 2019 USD per year)
        DAMFRAC(t)      Damages as fraction of gross output
        ABATECOST(t)    Cost of emissions reductions  (trillions 2019 USD per year)
        MCABATE(t)      Marginal cost of abatement (2019$ per ton CO2)
        CCATOT(t)       Total carbon emissions (GtC)
        PERIODU(t)      One period utility function
        CPRICE(t)       Carbon price (2019$ per ton of CO2)
        TOTPERIODU(t)   Period utility
        UTILITY         Welfare function
        RFACTLONG(t)
        RSHORT(t)       Real interest rate with precautionary(per annum year on year)
        RLONG(t)        Real interest rate from year 0 to T
;
NONNEGATIVE VARIABLES  MIU, TATM, MAT, MU, ML, Y, YNET, YGROSS, C, K, I, RFACTLONG;
EQUATIONS
*Emissions and Damages
        CCATOTEQ(t)      Cumulative total carbon emissions
        DAMFRACEQ(t)     Equation for damage fraction
        DAMEQ(t)         Damage equation
        ABATEEQ(t)       Cost of emissions reductions equation
        MCABATEEQ(t)     Equation for MC abatement
        CARBPRICEEQ(t)   Carbon price equation from abatement
*Economic variables
        YGROSSEQ(t)      Output gross equation
        YNETEQ(t)        Output net of damages equation
        YY(t)            Output net equation
        CC(t)            Consumption equation
        CPCE(t)          Per capita consumption definition
        SEQ(t)           Savings rate equation
        KK(t)            Capital balance equation
        RSHORTEQ(t)      Short-run interest rate equation
        RLONGeq(t)       Long-run interest rate equation
        RFACTLONGeq(t)   Long interest factor
* Utility
        TOTPERIODUEQ(t)  Period utility
        PERIODUEQ(t)     Instantaneous utility function equation
        UTILEQ           Objective function      ;

** Include file for DFAIR model and climate equations
$include Include\FAIR-beta-4-3-1.gms

**** Equations of the model
**Emissions and Damages
 eco2eq(t)..          ECO2(t)        =E= (sigma(t)*YGROSS(t) + eland(t))*(1-(MIU(t)));
 eindeq(t)..          EIND(t)        =E= (sigma(t)*YGROSS(t))*(1-(MIU(t)));
 eco2Eeq(t)..         ECO2E(t)       =E= (sigma(t)*YGROSS(t) + eland(t) + CO2E_GHGabateB(t))*(1-(MIU(t)));
 F_GHGabateEQ(t+1)..  F_GHGabate(t+1) =E= Fcoef2*F_GHGabate(t)+ Fcoef1*CO2E_GHGabateB(t)*(1-(MIU(t)));
 ccatoteq(t+1)..      CCATOT(t+1)    =E= CCATOT(t) +  ECO2(T)*(5/3.666) ;
 damfraceq(t) ..      DAMFRAC(t)     =E= (a1*(TATM(t)))+(a2base*(TATM(t))**a3) ;
 dameq(t)..           DAMAGES(t)     =E= YGROSS(t) * DAMFRAC(t);
 abateeq(T)..         ABATECOST(T)   =E= YGROSS(T) * COST1TOT(T) * (MIU(T)**EXPCOST2);
 mcabateeq(t)..       MCABATE(t)     =E= pbacktime(t) * MIU(t)**(expcost2-1);
 carbpriceeq(t)..     CPRICE(t)      =E= pbacktime(t) * (MIU(t))**(expcost2-1);
**Economic variables
 ygrosseq(t)..        YGROSS(t)      =E= (AL(t)*(L(t)/1000)**(1-gama))*(K(t)**gama);
 yneteq(t)..          YNET(t)        =E= YGROSS(t)*(1-damfrac(t));
 yy(t)..              Y(t)           =E= YNET(t) - ABATECOST(t);
 cc(t)..              C(t)           =E= Y(t) - I(t);
 cpce(t)..            CPC(t)         =E= 1000 * C(t) / L(t);
 seq(t)..             I(t)           =E= S(t) * Y(t);
 kk(t+1)..            K(t+1)         =L= (1-dk)**tstep * K(t) + tstep * I(t);
 RFACTLONGeq(t+1)..   RFACTLONG(t+1) =E= SRF*(cpc(t+1)/cpc('1'))**(-elasmu)*rr(t+1);
 RLONGeq(t+1)..       RLONG(t+1)     =E= -log(RFACTLONG(t+1)/SRF)/(5*t.val);
 RSHORTeq(t+1)..      RSHORT(t+1)    =E= -log(RFACTLONG(t+1)/Rfactlong(t))/5;
** Welfare functions 
 periodueq(t)..       PERIODU(t)     =E= ((C(T)*1000/L(T))**(1-elasmu)-1)/(1-elasmu)-1;
 totperiodueq(t)..    TOTPERIODU(t)  =E= PERIODU(t) * L(t) * RR(t);
 utileq..             UTILITY        =E= tstep * scale1 * sum(t,  TOTPERIODU(t)) + scale2;

* Various control rate limits, initial and terminal conditions
miu.up(t)       = miuup(t);
K.LO(t)         = 1;
C.LO(t)         = 2;
CPC.LO(t)       = .01;
RFACTLONG.lo(t) =.0001;
*set lag10(t) ;
*lag10(t)                =  yes$(t.val gt card(t)-10);
*S.FX(lag10(t))          = optlrsav;
s.fx(t)$(t.val > 37)    =.28;
ccatot.fx(tfirst)       = CumEmiss0;
K.FX(tfirst)            = k0;
F_GHGabate.fx(tfirst)   = F_GHGabate2020;
RFACTLONG.fx(tfirst)    = 1000000; 

** Solution options
option iterlim = 99900; option reslim = 99999; option solprint = on; option limrow = 0; option limcol = 0;

** Initial solution
model  CO2 /all/;
ifopt=1;
solve CO2 maximizing UTILITY using nlp ;
 
**** STATEMENTS FOR DEFINITIONS AND PUT STATEMENTS FOR MAJOR SCENARIOS
$include Include\Putlong-4-3-10.gms

*DISPLAY FOR MAJOR VARIABLES
option decimals = 6;
display gpccons, varpcc,RLONG.l, rprecaut,RSHORT.l,cpc.l,rr,miu.l,scc, mat.l,tatm.l,eco2eq.m,miu.l;
display ifopt, elasmu, rartp, pi, k0, betaclim, prstp,k0,siggc1,delA,a2base, utility.l ;
