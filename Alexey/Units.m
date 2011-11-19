(* ::Package:: *)

(* :Title: *)
(* :Author:  *)
(* :Summary: *)
(* :Package Version: 0.5 *)
(* :Context: Alexey`Units` *)
(* :Keywords: *)
(* :Limitation: None. *)
(* :Discussion: *)

BeginPackage["Alexey`Units`"]



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

N[SpeedOfLight] = 2.99792458 10^10 Centimeter/Second;
N[PlanckConstant] = 6.6260755 10.^-27 Gram (Centimeter/Second)^2 Second;
N[PlanckConstantReduced] = 6.6260755 10.^-27 Gram (Centimeter/Second)^2 Second/ (2 Pi);
N[ElectronCharge] =4.8032 10^-10 Centimeter^(3/2)Gram^(1/2)/Second;
N[ElectronMass] = 9.1093897 10^-28 Gram;
N[ProtonMass] = 1.6726231 10^-24 Gram;
N[FineStructureConstant] = 1/137.0359895;
N[PlanckMass] = 2.17671 10^-5 Gram;
N[BohrRadius] = 0.529177249 10^-8 Centimeter;
N[RydbergConstant] = 109737.318  Centimeter^-1;
N[ElectronComptonWavelength] = 2.426309 10^-10 Centimeter;
N[ClassicalElectronRadius] = 2.817938 10^-13 Centimeter;
N[ElectronMagneticMoment] = 9.284832 10^-21 Gram (Centimeter/Second)^2/Gauss;
N[ElectronGFactor] = 1.0011596567;
N[BorhMagneton] = 9.27408 10^-21 Gram (Centimeter/Second)^2/Gauss;
N[MagneticFluxQuantum] = 2.0678506 10^-15*3.34 10^-13 Centimeter^(3/2)Gram^(1/2)/Second;
N[BoltzmannConstant] = 1.380658 10^-16 Erg/Kelvin;

ct={Centimeter,Gram,Second};

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

Convert[old_,new_]:=Module[{t,term},
If[Head[new]===ToUnits,(* Print["units to system ..."];*)
	t=Cancel[(old/.ToGauss)/.new[[1]]];Return[t],	
	If[MemberQ[new,_ToUnits],(*Print["units to system with prefix term="];*)
		term=new/.{_ToUnits->1};(* Print["=prefix term=    ",term];*)
		t=(new/term)[[1]];(*Print["conversion rule=    ",t];*)
		t=Cancel[term ((old/.ToGauss)/.t)/((term/.ToGauss))/.t];(*Print[t];*)
		Return[t],


		If[MemberQ[old,_ToUnits],  (*Print["arb value to unit"];*)
			term=old/.{_ToUnits->1};(*Print["term=  ",term];*)
			t=Cases[old,_ToUnits][[1]][[1]];(*Print["conversion rule=    ",t];*)
			If[!NumberQ[term],Message[Convert::d,term,old];Return[]];
			(* t=new*Cancel[((new/.ToGauss)/.t) ];Return[term*t],*)
			t=Cancel[new/((new/.ToGauss)/.t) ];Return[term*t],
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

Unit::usage = "Unit[a,b,c] introduce system of unit with that basis.";

Unit[basis___]:=Module[{x}, Off[Solve::svars] ; 
    If[Length[{basis}]==3,
	   x=Solve[({basis}/.ToGauss)=={basis},ct];
	   If[Length[x]==0,Message[Unit::trivial ,{basis}];Return[]];
	   On[Solve::svars] ; Return[ToUnits[Last[x]]]]]

ArbUnit::usage = "ArbUnit[a,b,c] introduce arbitrary system of unit 
with a,b,c=1, ArbUnit[a->Gram,b,c] make system with variable parameter
a (dimension length)"

ArbUnit[basis___]:=Module[{x,bus,subst},
 	If[Length[{basis}]!=3,Message[ArbUnit::longort ,basis];Return[]];
	subst=Cases[{basis},a_Rule]/.ToGauss;	(*Print[subst]; *)
	bas=({basis}/.Ort[a___]->List[a])/.(Rule[a_,b_]->a);(*Print["bas=",bas];*)
	If[Length[subst]>0,
		x=(N[{bas}/.subst]/.{_Real->1})[[1]];(*Print["x=",x];*)
	(* x=Table[If[NumberQ[#[[1]]],Rest[#],#]& @{N[bas/.subst][[i]]} ,{i,3}];Print[x];Print[bas]; *)
		x=(ct/.Solve[x==bas,ct])[[1]];(*Print[x];*)
		x=Table[(z:>Cancel[z/(N[y]/.ToGauss)])/.{y->x[[i]],z->ct[[i]]},{i,1,3}],
		x=Solve[N[bas/.ToGauss]=={1,1,1},ct][[1]]];	
Return[ToUnits[x]]]	

AddUnit::usage = "AddUnit[unit->old] introduce new unit use old unit,
option output OutputFormatUnit and TeXFormatUnit may be used";

AddUnit[unit_->across_,opts___]:=Module[{ofo,tft},
	ToGaussBase=Append[ToGaussBase,unit->(across/.ToGauss)];
	ToGauss=Dispatch[ToGaussBase];
	ofo=OutputFormatUnit/.{opts};tft=TeXFormatUnit/.{opts};
	If[ofo!=0utputFormatUnit,Format[unit]:=ofo];
	If[tft!=TeXFormatUnit,Format[unit,TeXFormat]:=tft];
	]
SetAttributes[AddUnit,Listable]
EndPackage[]




