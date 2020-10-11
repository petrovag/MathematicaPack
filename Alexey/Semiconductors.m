(* ::Package:: *)

(* ::Input::Initialization:: *)
BeginPackage["Alexey`Semiconductors`",{"Alexey`Units`"}]
Penetration::usage="Penetration[\"GaAs\",0|\[Infinity]] \[LongDash] \:0434\:0438\:044d\:043b\:0435\:043a\:0442\:0440\:0438\:0447\:0435\:0441\:043a\:0430\:044f \:043f\:0440\:043e\:043d\:0438\:0446\:0430\:0435\:043c\:043e\:0441\:0442\:044c \:043c\:0430\:0442\:0435\:0440\:0438\:0430\:043b\:0430";
EffectiveBohrRadius::usage="Effective Bohr Radius of matherial";
FrolichCoefficient::usage="Effective Frolich Coefficient";
EnergyGap::usage="EnergyGap[\"GaAs\"[,\:0442\:0435\:043c\:043f\:0435\:0440\:0430\:0442\:0443\:0440\:0430]]\:0417\:0430\:043f\:0440\:0435\:0449\:0435\:043d\:043d\:0430\:044f \:0437\:043e\:043d\:0430 \:0438\:043b\:0438 EnergyGap[\"\!\(\*SubscriptBox[\(Al\), \(x\)]\)\!\(\*SubscriptBox[\(Ga\), \(1 - x\)]\)As\",x,Temperature]";
SpinOrbitGap::usage="SpinOrbitGap[\"GaAs\"[,\:0442\:0435\:043c\:043f\:0435\:0440\:0430\:0442\:0443\:0440\:0430]] Spin Orbital Gap \:0438\:043b\:0438 SpinOrbitGap[\"\!\(\*SubscriptBox[\(Al\), \(x\)]\)\!\(\*SubscriptBox[\(Ga\), \(1 - x\)]\)As\",x,Temperature]";
InderectEnergyGapL6\[CapitalGamma]7::usage="InderectEnergyGapX8\[CapitalGamma]7[\"AlGaAs\",x_] Inderect Energy Gap";
InderectEnergyGapX8\[CapitalGamma]7::usage="InderectEnergyGapX8\[CapitalGamma]7[\"AlGaAs\",x_] Inderect Energy Gap";
LatticeConstant::usage="Lattice constant";
EffectiveElectronMass::usage="EffectiveElectronMass[\"GaAs\"[,Temperature]]";
EffectiveElectronMassFromConcentrationAndPressure::usage="Effective electron mass \!\(\*FormBox[SubsuperscriptBox[\(\[CapitalGamma]\), \(6\), \(c\)],
TraditionalForm]\) depends electron concentration and pressure";
EffectiveElectronMassFromEnergy::usage="Effective electron mass \!\(\*FormBox[SubsuperscriptBox[\(\[CapitalGamma]\), \(6\), \(c\)],
TraditionalForm]\) from Energy";
HoleMass::usage="\:041c\:0430\:0441\:0441\:0430 \:0434\:044b\:0440\:043a\:0438";
InderecSpinOrbitGapX8\[CapitalGamma]7::usage="InderectSpinOrbitGapX8\[CapitalGamma]7[\"AlGaAs\",x_] Inderect Spin Orbital Gap";


Begin["`Private`"]
tri[a_,b_,c_,x_]:=a+b x+c x^2;
(* \:0414\:0438\:044d\:043b\:0435\:043a\:0442\:0440\:0438\:0447\:0435\:0441\:043a\:0430\:044f \:043f\:0440\:043e\:043d\:0438\:0446\:0430\:0435\:043c\:043e\:0441\:0442\:044c *)
Penetration["GaAs",0]=13.18;Penetration["GaAs",Infinity]=10.89;
Penetration["InSb",0]=17.88;Penetration["InSb",Infinity]=15.68;
Penetration["InAs",0]=14.55;Penetration["GaAs",Infinity]=11.80;
Penetration["GaSb",0]=15.69;Penetration["GaSb",Infinity]=14.44;
Penetration["Ge",0]=16.;Penetration["Si",0]=11.7;

EffectiveBohrRadius[a_String]:=BohrRadius*Penetration[a,0]*ElectronMass/EffectiveElectronMass[a];


FrolichCoefficient[a_String]:=ElectronCharge^2/PlanckConstantReduced Sqrt[2 EffectiveElectronMass[a]/LOPhotonEnergy[a]](1/Penetration[a,Infinity]-1/Penetration[a,0])/2;
(* \:041f\:043e\:0441\:0442\:043e\:044f\:043d\:043d\:044b\:0435 \:0440\:0435\:0448\:0435\:0442\:043a\:0438 *)
LatticeConstant["InP"]=5.8687 Angstrom;
LatticeConstant["InAs"]=6.0583 Angstrom;
LatticeConstant["InSb"]=6.47937 Angstrom;
LatticeConstant["GaP"]=5.45117 Angstrom;
LatticeConstant["GaAs"]=5.65325 Angstrom;
LatticeConstant["AlAs"]=5.660 Angstrom;
LatticeConstant["AlSb"]=6.1355 Angstrom;

(* \:0417\:0430\:043f\:0440\:0435\:0449\:0435\:043d\:043d\:0430\:044f \:0437\:043e\:043d\:0430 *)
EnergyGap["GaAs",t_:300 Kelvin]:=(1519.2(300 Kelvin-t)+1420. t)meV/(300 Kelvin);
EnergyGap["InAs",t_:300 Kelvin]:=(418.(300 Kelvin-t)+354. t)meV/(300 Kelvin);
EnergyGap["InP",t_:300 Kelvin]:=(1423.6(300 Kelvin-t)+1340. t)meV/(300 Kelvin);
EnergyGap["InSb",t_:300 Kelvin]:=(235.2(300 Kelvin-t)+180. t)meV/(300 Kelvin);
EnergyGap["GaP",t_:300 Kelvin]:=(2869.(300 Kelvin-t)+2780. t)meV/(300 Kelvin);
EnergyGap["GaSb",t_:300 Kelvin]:=(811.3(300 Kelvin-t)+700. t)meV/(300 Kelvin);
EnergyGap["AlAs",t_:300 Kelvin]:=(3130(300 Kelvin-t)+2950. t)meV/(300 Kelvin);
EnergyGap["AlSb",t_:300 Kelvin]:=(2320(300 Kelvin-t)+2219. t)meV/(300 Kelvin);
EnergyGap[a_String,b_String,x_,t_:300 Kelvin]:=(1-x)*EnergyGap[a,t]+x*EnergyGap[b,t];
EnergyGap["AlGaAs",x_,T_:300 Kelvin]:=600(1.155x+0.37x^2-1.15x(T-300 Kelvin)10^-4)meV;
EnergyGap["AltGaAs",x_,T_]:=(1424` +1266` x+0266` x^2) meV;
EnergyGap["AlTGaAs",x_]:=1425 meV+1155 meV x+370 x^2 meV;


(* Spin-Orbital splitting*)

SpinOrbitGap["GaAs",t_:300 Kelvin]:=(341.(300 Kelvin-t)+340. t)meV/(300 Kelvin);
SpinOrbitGap["InAs",t_:300 Kelvin]:=(380.(300 Kelvin-t)+371. t)meV/(300 Kelvin);
SpinOrbitGap["InP",t_:300 Kelvin]:=(108.(300 Kelvin-t)+110. t)meV/(300 Kelvin);
SpinOrbitGap["InSb",t_:300 Kelvin]:=(235.2(300 Kelvin-t)+180. t)meV/(300 Kelvin);
SpinOrbitGap["GaP",t_:300 Kelvin]:=(820.(300 Kelvin-t)+800. t)meV/(300 Kelvin);
SpinOrbitGap["GaSb",t_:300 Kelvin]:=(811.3(300 Kelvin-t)+700. t)meV/(300 Kelvin);
SpinOrbitGap["AlAs",t_:300 Kelvin]:=(275.(300 Kelvin-t)+275. t)meV/(300 Kelvin);
SpinOrbitGap["AlSb",t_:300 Kelvin]:=(656.(300 Kelvin-t)+750. t)meV/(300 Kelvin);
SpinOrbitGap[a_String,b_String,x_,t_:300 Kelvin]:=(1-x)*SpinOrbitGap[a,t]+x*SpinOrbitGap[b,t];
SpinOrbitGap["AlGaAs",x_]:=tri[0.340,-0.131,0.071]ElectronVolt;
SpinOrbGap["AltGaAs",x_]:=340 meV (1-x)+275 x meV;

(* \:041d\:0435\:043f\:0440\:044f\:043c\:0430\:044f \:0449\:0435\:043b\:044c (\!\(
\*SubsuperscriptBox[\(X\), \(8\), \(c\)] - 
\*SubsuperscriptBox[\(\[CapitalGamma]\), \(7\), \(v\)]\)) *)

InderectEnergyGapX8\[CapitalGamma]7["AlGaAs",x_]:=tri[1.900,1.25,0.143]ElectronVolt;
InderecSpinOrbitGapX8\[CapitalGamma]7["AlGaAs",x_]=tri[0.240,-0.012,-0.028]ElectronVolt;

(* \:041d\:0435\:043f\:0440\:044f\:043c\:0430\:044f \:0449\:0435\:043b\:044c (\!\(
\*SubsuperscriptBox[\(L\), \(6\), \(c\)] - 
\*SubsuperscriptBox[\(\[CapitalGamma]\), \(7\), \(v\)]\)) *)

InderectEnergyGapL6\[CapitalGamma]7["AlGaAs",x_]:=tri[1.708,1.642,0.]ElectronVolt;

(* Effective Electron Masses*)

EffectiveElectronMass["GaAs",t_:300 Kelvin]:=(0.0665(300 Kelvin-t)+0.0679 t)ElectronMass/(300 Kelvin);
EffectiveElectronMass["InAs",t_:300 Kelvin]:=(0.023(300 Kelvin-t)+0.027 t)ElectronMass/(300 Kelvin);
EffectiveElectronMass["InP",t_:300 Kelvin]:=(0.079(300 Kelvin-t)+0.077 t)ElectronMass/(300 Kelvin);
EffectiveElectronMass["InSb",t_:300 Kelvin]:=(0.0139(300 Kelvin-t)+0.013 t)ElectronMass/(300 Kelvin);
EffectiveElectronMass["GaSb"]:=0.042 ElectronMass;
EffectiveElectronMass["AlAs"]:=0.124 ElectronMass;
EffectiveElectronMass[a_String,b_String,x_,t_:0]:=(1-x)*EffectiveElectronMass[a,t]+x*EffectiveElectronMass[b,t];

EffectiveElectronMassFromConcentrationAndPressure["AlGaAs",x_,n_,P_]:=EffectiveElectronMass["AlGaAs",x] ( 1+7.4 (10^-6) P )/(1-3.9 10^-15 n^(2/3))
EffectiveElectronMassFromEnergy["AlGaAs",x_,e_]:=EffectiveElectronMass["AlGaAs",x]+(0.0436e+0.236^2 e^2-0.147e^3);

(* Effective Electron Masses*)

HoleMass["InP"]={0.12 ElectronMass,0.6 ElectronMass,0};
HoleMass["GaAs"]={0.082 ElectronMass,{0.45 ElectronMass,0.57 ElectronMass},0};
HoleMass["GaSb"]={{0.047 ElectronMass,0.052ElectronMass},{0.32 ElectronMass,0.40 ElectronMass},0};
HoleMass["InAs"]={0.026 ElectronMass,0.41 ElectronMass,0};
HoleMass["InSb"]={0.016 ElectronMass,{0.40 ElectronMass,0.45 ElectronMass},0};
HoleMass["AlAs"]={0.066 ElectronMass,0.5 ElectronMass,0};
HoleMass["AlSb"]={0.11 ElectronMass,{0.4 ElectronMass,0.5 ElectronMass},0};
HoleMass["GaP"]={0.17 ElectronMass,0.67 ElectronMass,0};
HoleMass[a_String,b_String,x_]:=(1-x)*HoleMass[a]+x*HoleMass[b];
End[];
EndPackage[]
