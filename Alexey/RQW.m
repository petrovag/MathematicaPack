(* ::Package:: *)

(* :Title: *)
(* :Author:  *)
(* :Summary: *)
(* :Package Version: 0.1 *)
(* :Context: Alexey`RQW` *)
(* :Keywords: *)
(* :Limitation: None. *)
(* :Discussion: *)

BeginPackage["Alexey`RQW`",{"Alexey`Units`"}]

(* ::Input::Initialization:: *)
QWstatesK::usage="QWstatesK[width,depth,effm] --- \:0423\:0440\:043e\:0432\:043d\:0438 \:044d\:043d\:0435\:0440\:0433\:0438\:0438 \:0432 \:043a\:0432\:0430\:043d\:0442\:043e\:0432\:043e\:0439 \:044f\:043c\:0435, 1/Angstrom";
QWstatesE::usage="QWstatesE[width,depth,effm] --- \:0423\:0440\:043e\:0432\:043d\:0438 \:044d\:043d\:0435\:0440\:0433\:0438\:0438 \:0432 \:043a\:0432\:0430\:043d\:0442\:043e\:0432\:043e\:0439 \:044f\:043c\:0435 meV";

Begin["`Private`"]
QWstatesK[width_,depth_:300meV,eff_:0.067ElectronMass]:=Module[{u1system,a,eqEvenU1,nEvenU1,stEvenU1,nOddU1,eqOddU1,stOddU1},
u1system::usage="\:041e\:0442\:043d\:043e\:0441\:0438\:0442\:0435\:043b\:044c\:043d\:0430\:044f \:0441\:0438\:0441\:0442\:0435\:043c\:0430 \:0435\:0434\:0438\:043d\:0438\:0446 \:0432 \:043a\:043e\:0442\:043e\:0440\:043e\:0439 \:0433\:043b\:0443\:0431\:0438\:043d\:0430 \:044f\:043c\:044b \:0440\:0430\:0432\:043d\:0430 1";u1system=ArbUnit[PlanckConstantReduced,eff,depth];a=Convert[width,u1system];eqEvenU1::usage="eqEvenU1[u] \:041a\:0440\:0438\:0442\:0435\:0440\:0438\:0439 \:0447\:0435\:0442\:043d\:043e\:0433\:043e \:0441\:043e\:0441\:0442\:043e\:044f\:043d\:0438\:044f";eqEvenU1[a_]=Sqrt[2-k^2]-k Tan[(a k)/2];nEvenU1::usage="nEvenU1[a] \:041a\:043e\:043b\:0438\:0447\:0435\:0441\:0442\:0432\:043e \:0447\:0435\:0442\:043d\:044b\:0445 \:0441\:043e\:0441\:0442\:043e\:044f\:043d\:0438\:0439 \:0432 \:044f\:043c\:0435";
nEvenU1[a_]=1+Floor[(Sqrt[2]a)/(2\[Pi])];
stEvenU1::usage="stEvenU1[a] -- Wave number of the Even states in rectangular quantum well";stEvenU1[a_]:=Table[k/.FindRoot[eqEvenU1[a]==0,{k,\[Pi] (2j-1.8)/a,2\[Pi] (j-1)/a,\[Pi] (2j-1)/a}],{j,1,nEvenU1[a]}];
eqOddU1::usage="eqOddU1[u] \:041a\:0440\:0438\:0442\:0435\:0440\:0438\:0439 \:043d\:0435\:0447\:0435\:0442\:043d\:043e\:0433\:043e \:0441\:043e\:0441\:0442\:043e\:044f\:043d\:0438\:044f";eqOddU1[a_]=Sqrt[2-k^2]+k Cot[(a k)/2];nOddU1::usage="nOddU1[a] \:041a\:043e\:043b\:0438\:0447\:0435\:0441\:0442\:0432\:043e \:0447\:0435\:0442\:043d\:044b\:0445 \:0441\:043e\:0441\:0442\:043e\:044f\:043d\:0438\:0439 \:0432 \:044f\:043c\:0435";
nOddU1[a_]=Floor[1/2+(a Sqrt[1/2])/\[Pi]];stOddU1::usage="stOddU1[a] -- Wave number of the Odd states in rectangular quantum well";stOddU1[a_]:=Table[k/.FindRoot[eqOddU1[a]==0,{k,2\[Pi] (j-1/2)/a+0.0001,2\[Pi] (j-1)/a,2\[Pi] j/a}],{j,1,nOddU1[a]}];
Return[Convert[Sort[Join[stOddU1[a],stEvenU1[a]]]u1system,Angstrom^-1]]
];
QWstatesE[width_,depth_:300meV,eff_:0.067ElectronMass]:=Convert[(PlanckConstantReduced^2 QWstatesK[width,depth,eff]^2)/(2eff),meV];
End[]
EndPackage[]

