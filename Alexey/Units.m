(* ::Package:: *)

(* :Title: *)
(* :Author:  *)
(* :Summary: *)
(* :Package Version: 0.7 *)
(* :Context: Alexey`Units` *)
(* :Keywords: *)
(* :Limitation: None. *)
(* :Discussion: *)

BeginPackage["Alexey`Units`"]

Convert::usage = "Convert[OldUnit,NewUnit] converts old unit to new, 
	Convert[number ArbitrarySystemOfUnit,NewUnit] converts arb. unit to new
	Convert[OldUnit,term*SystemOfUnit] converts unit to new system unit push term ."


Convert::incompatible="Incompatible units in `1` and `2`.";
Unit::trivial="Trivial basis of units: `1`";
ArbUnit::longort="Arb. system of unit `1` must have 3 orts";
Convert::dre="Attempt conversion of number `1` to unit `2` without 
	determination anything system of the unit: SU->....";
Convert::d= "Attempt conversion of number `1` to unit `2` in system
	non-arbitrary unit";

Unit::usage = "Unit[a,b,c] introduce system of unit with that basis.";
AddUnit::usage = "AddUnit[unit->old] introduce new unit use old unit,
option output OutputFormatUnit and TeXFormatUnit may be used";


ArbUnit::usage = "ArbUnit[a,b,c] introduce arbitrary system of unit 
with a,b,c=1, ArbUnit[a->Gram,b,c] make system with variable parameter
a (dimension length)"

QED::usage = "\:041a\:0432\:0430\:043d\:0442\:043e\:0432\:0430\:044f \:0441\:0438\:0441\:0442\:0435\:043c\:0430 \:0435\:0434\:0438\:043d\:0438\:0446"
Atomic::usage = "\:0410\:0442\:043e\:043c\:043d\:0430\:044f \:0441\:0438\:0441\:0442\:0435\:043c\:0430 \:0435\:0434\:0438\:043d\:0438\:0446"
CGS::usage = "\:0421\:0413\:0421"
QW::usage = "\:0421\:0438\:0441\:0442\:0435\:043c\:0430 \:0435\:0436\:0438\:043d\:0438\:0446 \:0441 \:0437\:0430\:0434\:0430\:043d\:043d\:043e\:0439 \:0434\:043b\:0438\:043d\:043e\:0439, \:043c\:0430\:0441\:0441\:043e\:0439 \:0438 \:043f\:043e\:0441\:0442\:043e\:044f\:043d\:043d\:043e\:0439 \:043f\:043b\:0430\:043d\:043a\:0430"
ToGaussBase::usage = "\:041d\:0430\:0431\:043e\:0440 \:043f\:0440\:0430\:0432\:0438\:043b \:0434\:043b\:044f \:0434\:043b\:044f \:043a\:043e\:043d\:0432\:0435\:0440\:0441\:0438\:0438 \:0432 \:0433\:0430\:0443\:0441\:0441\:043e\:0432\:0443 \:0441\:0438\:0441\:0442\:0435\:043c\:0443 \:0435\:0434\:0438\:043d\:0438\:0446"
BohrRadius::usage = "Bohr Radius";
ElectronMass::usage = "Mass of Electron";
PlanckConstantReduced::usage = "Planck Constant Reduced";
ToGaussBase::usage = "CGS";



(** fundamental SI units usage **)
ToGaussBase ={
Percent -> 1/100,
Degree -> Pi/180,
Meter -> 100 Centimeter,
Angstrom -> 10^-8 Centimeter,
Micron -> 10^-4 Centimeter,
Kilogram -> 1000 Gram,
Dyne -> Gram Centimeter/Second,
Newton -> 10^5 Gram Centimeter/Second,
Pascal ->10 Dyne /Centimeter^2,	
Joule -> 10000000 Gram (Centimeter/Second)^2,
ElectronVolt -> 0.160217733*10^-11 Gram (Centimeter/Second)^2,
meV -> 0.160217733*10^-14 Gram (Centimeter/Second)^2,
Erg -> Gram (Centimeter/Second)^2,
Rydberg -> 2.1799*10^-11 Gram (Centimeter/Second)^2,
Coulomb ->3 10^9 Centimeter^(3/2)Gram^(1/2)/Second,
Volt->3.34 10^-3 Centimeter^(1/2)Gram^(1/2)/Second,
Farad -> 8.99 10^11 Centimeter,
Amper->3 10^9 Centimeter^(3/2)Gram^(1/2)/Second^2,
Ohm->1.11 10^-12 Second/Centimeter,
Tesla ->10^4 Centimeter^(-1/2)Gram^(1/2)/Second,
Gauss->(*3.34 10^-11*) Centimeter^(-1/2)Gram^(1/2)/Second, (* ????? *)
Weber->3.34 10^-13 Centimeter^(3/2)Gram^(1/2)/Second,
Oersted->3 10^10 Centimeter^(3/2)Gram^(1/2)/Second,
Watt -> 10000000 Gram (Centimeter/Second)^2/Second,
Hertz -> 1/Second,
Kelvin -> 1.38 10^-16 Centimeter^2 Gram/Second^2
	};
ToGauss=Dispatch[ToGaussBase];

If[Not[ListQ[$Assumptions]], $Assumptions = {}]
AppendTo[$Assumptions,Centimeter>0];
AppendTo[$Assumptions,Second>0];
AppendTo[$Assumptions,Gram>0];
AppendTo[$Assumptions,Kelvin>0];
AppendTo[$Assumptions,Erg>0];
AppendTo[$Assumptions,Gauss];
AppendTo[$Assumptions,meV>0];

N[SpeedOfLight] = 2.99792458 10^10 Centimeter/Second;AppendTo[$Assumptions,SpeedOfLight>0];
N[PlanckConstant] = 6.6260755 10.^-27 Gram (Centimeter/Second)^2 Second;AppendTo[$Assumptions,PlanckConstant>0];
N[PlanckConstantReduced] = 6.6260755 10.^-27 Gram (Centimeter/Second)^2 Second/ (2 Pi);AppendTo[$Assumptions,PlanckConstantReduced>0];
N[\[HBar]] = N[PlanckConstantReduced];AppendTo[$Assumptions,\[HBar]>0];
N[ElectronCharge] =4.8032 10^-10 Centimeter^(3/2)Gram^(1/2)/Second;AppendTo[$Assumptions,ElectronCharge>0];
N[ElectronMass] = 9.1093897 10^-28 Gram;AppendTo[$Assumptions,ElectronMass>0];
N[ProtonMass] = 1.6726231 10^-24 Gram;AppendTo[$Assumptions,ProtonMass>0];
N[FineStructureConstant] = 1/137.0359895;AppendTo[$Assumptions,FineStructureConstant>0];
N[PlanckMass] = 2.17671 10^-5 Gram;AppendTo[$Assumptions,PlanckMass>0];
N[BohrRadius] = 0.529177249 10^-8 Centimeter;AppendTo[$Assumptions,BohrRadius>0];
N[RydbergConstant] = 109737.318  Centimeter^-1;AppendTo[$Assumptions,RydbergConstant>0];
N[ElectronComptonWavelength] = 2.426309 10^-10 Centimeter;AppendTo[$Assumptions,ElectronComptonWavelength>0];
N[ClassicalElectronRadius] = 2.817938 10^-13 Centimeter;AppendTo[$Assumptions,ClassicalElectronRadius>0];
N[ElectronMagneticMoment] = 9.284832 10^-21 Gram (Centimeter/Second)^2/Gauss;AppendTo[$Assumptions,ElectronMagneticMoment>0];
N[ElectronGFactor] = 1.0011596567;AppendTo[$Assumptions,ElectronGFactor>0];
N[BorhMagneton] = 9.27408 10^-21 Gram (Centimeter/Second)^2/Gauss;AppendTo[$Assumptions,BorhMagneton>0];
N[MagneticFluxQuantum] = 2.0678506 10^-15*3.34 10^-13 Centimeter^(3/2)Gram^(1/2)/Second;AppendTo[$Assumptions,MagneticFluxQuantum>0];
N[BoltzmannConstant] = 1.380658 10^-16 Erg/Kelvin;AppendTo[$Assumptions,BoltzmannConstant>0];

ct={Centimeter,Gram,Second};
Begin["`Private`"]

Convert[old_,new_]:=Module[{t,term},
If[Head[new]===ToUnits,(* Print["units to system ..."];*)
	t=Cancel[(N[old]/.ToGauss)/.new[[1]]];Return[t],	
	If[MemberQ[new,_ToUnits],(*Print["units to system with prefix term="];*)
		term=new/.{_ToUnits->1};(* Print["=prefix term=    ",term];*)
		t=(new/term)[[1]];(*Print["conversion rule=    ",t];*)
		t=Cancel@PowerExpand@Simplify[term ((N[old]/.ToGauss)/.t)/((term/.ToGauss))/.t,{Centimeter>0,Gram>0,Second>0}];(*Print[t];*)
		Return[t],


		If[MemberQ[old,_ToUnits],  (*Print["arb value to unit"];*)
			term=old/.{_ToUnits->1};(*Print["term=  ",term];*)
			t=Cases[old,_ToUnits][[1]][[1]];(*Print["conversion rule=    ",t];*)
			If[!NumberQ[term],Message[Convert::d,term,old];Return[]];
			(* t=new*Cancel[((new/.ToGauss)/.t) ];Return[term*t],*)
			t=Cancel@PowerExpand@Simplify[new/((new/.ToGauss)/.t),{Centimeter>0,Gram>0,Second>0}];Return[term*t],
			(*Message[Convert::arbsystosys,old,new];Return[old],*)
			(*Print["units to units ..."];*)
			t=Cancel[(N[old]/N[new])/.ToGauss];
			If[!NumberQ[t],Message[Convert::incompatible,old,new];Return[old],
			Return[t*new]]
		]
	]
]];
SetAttributes[Convert,Listable];

QED=ArbUnit[ElectronMass,SpeedOfLight,PlanckConstantReduced];
Atomic=ArbUnit[ElectronMass,BohrRadius,PlanckConstantReduced];
CGS=ToUnits[{}];
QW=ArbUnit[PlanckConstantReduced,WellWidth->Angstrom,Mass->ElectronMass];


Unit[basis___]:=Module[{x}, Off[Solve::svars] ; 
    If[Length[{basis}]==3,
	   x=Solve[({basis}/.ToGauss)=={basis},ct];
	   If[Length[x]==0,Message[Unit::trivial ,{basis}];Return[]];
	   On[Solve::svars] ; Return[ToUnits[Last[x]]]]]

ArbUnit[basis___]:=Module[{x,bus,subst},
	If[Length[{basis}]!=3,Message[ArbUnit::longort ,basis];Return[]];
	subst=Cases[N@{basis},a_Rule]/.ToGauss;	
	bas=(N@{basis}/.Ort[a___]->List[a])/.(Rule[a_,b_]->a);
	If[Length[subst]>0,
		x=(N[{bas}/.subst]/.{_Real->1})[[1]];(*Print["x=",x];*)
	(* x=Table[If[NumberQ[#[[1]]],Rest[#],#]& @{N[bas/.subst][[i]]} ,{i,3}];*)
		x=(ct/.Solve[x==bas,ct])[[1]];
		x=Table[(z:>Cancel[z/(N[y]/.ToGauss)])/.{y->x[[i]],z->ct[[i]]},{i,1,3}],
		x=Solve[N[bas/.ToGauss]=={1.,1.,1.}&&ct[[1]]>0&&ct[[2]]>0&&ct[[3]]>0,ct]];
		x=First@x;
Return[ToUnits[x]]]	

AddUnit[unit_->across_,opts___]:=Module[{ofo,tft},
	ToGaussBase=Append[ToGaussBase,unit->(across/.ToGauss)];
	ToGauss=Dispatch[ToGaussBase];
	ofo=OutputFormatUnit/.{opts};tft=TeXFormatUnit/.{opts};
	If[ofo!=0utputFormatUnit,Format[unit]:=ofo];
	If[tft!=TeXFormatUnit,Format[unit,TeXFormat]:=tft];
	]
SetAttributes[AddUnit,Listable]
End[]
EndPackage[]




