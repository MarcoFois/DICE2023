* Discount program

ifopt=1;
elasmu   = .001;
prstp    = .05;
   RR1(t) = 1/((1+prstp)**(tstep*(t.val-1)));
        RR1(t) = 1/((1+prstp)**(tstep*(t.val-1)));
        RR1(t)$(t.val > 51) = 1/((1+prstp)**(5*51));
        RR(t)= RR1(t);

optlrsav = (dk + .004)/(dk + .004*elasmu + prstp)*gama;
        miuup('1')= .05;
        miuup('2')= .10;
        miuup(t)$(t.val > 2) = ( delmiumax*(t.val-1));
        miuup(t)$(t.val > 8) = 0.85+.05*(t.val-8);
        miuup(t)$(t.val > 11) = limmiu2070;
        miuup(t)$(t.val > 20) = limmiu2120;
        miuup(t)$(t.val > 37) = 1.05;
        miuup(t)$(t.val > 57) = 1.0;
miu.up(t) = miuup(t);
k0 = 290;
k.FX(tfirst)      = k0;
solve CO2 maximizing UTILITY using nlp ;
solve CO2 maximizing UTILITY using nlp ;
solve CO2 maximizing UTILITY using nlp ;

*Post-Solution Parameter-Assignment

scc(t) = -1000*eco2eq.m(t)/(.00001+cc.m(t));
ppm(t)    = mat.l(t)/2.13;
abaterat(t) = abatecost.l(t)/y.l(t);
atfrac2020(t) = ((mat.l(t)-mat0)/(ccatot.l(t)+.00001-CumEmiss0  ));
atfrac1765(t) = ((mat.l(t)-mateq)/(.00001+ccatot.l(t)  ));
FORC_CO2(t) = fco22x*((log((MAT.l(t)/mateq))/log(2)));
 
 
