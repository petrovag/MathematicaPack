(* ::Package:: *)

(* ::Input::Initialization:: *)
TriangleQ[{j1_,j2_,j3_}]:=If[IntegerQ[j1+j2+j3]\[And]Abs[j1-j2]<=j3<=j1+j2,True,False];
\[CapitalPi][lst_]:=Sqrt[Product[(2lst[[i]]+1),{i,1,Length[lst]}]]


(* ::Input::Initialization:: *)
Off[ClebschGordan::tri];Off[ClebschGordan::phy];Off[SixJSymbol::tri];


(* ::Input::Initialization:: *)
TriangularQ::usage="\:0421\:043e\:043e\:0442\:0432\:0435\:0442\:0441\:0442\:0432\:0438\:0435 \:0441\:0443\:043c\:043c\:044b \:043c\:043e\:043c\:0435\:043d\:0442\:043e\:0432 \:043f\:0440\:0430\:0432\:0438\:043b\:0443 \:0442\:0440\:0435\:0443\:0433\:043e\:043b\:044c\:043d\:0438\:043a\:0430";TriangularQ[j1_,j2_,j3_]=FullSimplify[2j1\[Element]NonNegativeIntegers\[And]2j2\[Element]NonNegativeIntegers\[And]2j3\[Element]NonNegativeIntegers\[And]j1+j2+j3\[Element]NonNegativeIntegers\[And]Abs[j1-j2]<=j3<=j1+j2]


(* ::Input::Initialization:: *)
Tri::usage="\:0421\:043e\:043e\:0442\:0432\:0435\:0442\:0441\:0442\:0432\:0438\:0435 \:0441\:0443\:043c\:043c\:044b \:043c\:043e\:043c\:0435\:043d\:0442\:043e\:0432 \:043f\:0440\:0430\:0432\:0438\:043b\:0443 \:0442\:0440\:0435\:0443\:0433\:043e\:043b\:044c\:043d\:0438\:043a\:0430 1|0";Tri[j1_,j2_,j3_]=Boole@FullSimplify[2j1\[Element]NonNegativeIntegers\[And]2j2\[Element]NonNegativeIntegers\[And]2j3\[Element]NonNegativeIntegers\[And]j1+j2+j3\[Element]NonNegativeIntegers\[And]Abs[j1-j2]<=j3<=j1+j2]


(* ::Input::Initialization:: *)
PhysicalQ::usage="\:0424\:0438\:0437\:0438\:0447\:0435\:0441\:043a\:0438\:0439 \:0441\:043c\:044b\:0441\:043b \:0441\:0443\:043c\:043c\:044b \:0434\:0432\:0443\:0445 \:043c\:043e\:043c\:0435\:043d\:0442\:043e\:0432";PhysicalQ[{j1_,m1_},{j2_,m2_},{j3_,m3_}]:=FullSimplify[TriangleQ[{j1,j2,j3}]\[And]j1-m1\[Element]Integers\[And]j2-m2\[Element]Integers\[And]j3-m3\[Element]Integers\[And]-j1<=m1<=j1\[And]-j2<=m2<=j2\[And]-j3<=m3<=j3]


(* ::Input::Initialization:: *)
PhysicalQ::usage="\:0424\:0438\:0437\:0438\:0447\:0435\:0441\:043a\:0438\:0439 \:0441\:043c\:044b\:0441\:043b \:0441\:0443\:043c\:043c\:044b \:0434\:0432\:0443\:0445 \:043c\:043e\:043c\:0435\:043d\:0442\:043e\:0432";PhysicalQ[j1_,j2_,j3_]:=FullSimplify[TriangleQ[{j1,j2,j3}]]


(* ::Input::Initialization:: *)
\[CapitalDelta]3[a_,b_,c_]:=Sqrt[((a+b-c)!(a-b+c)!(-a+b+c)!)/(a+b+c+1)!]


(* ::Input::Initialization:: *)
PhysicalQ::usage="\:041a\:0440\:0438\:0442\:0435\:0440\:0438\:0439 \:0444\:0438\:0437\:0438\:0447\:043d\:043e\:0441\:0442\:0438 \:0441\:0443\:043c\:043c\:044b \:043c\:043e\:043c\:0435\:043d\:0442\:043e\:0432";PhysicalQ[{j1_,j2_,j12_},{j3_,j_,j23_}]:=FullSimplify[TriangularQ[j1,j2,j12]\[And]TriangularQ[j12,j3,j]\[And]TriangularQ[j2,j3,j23]\[And]TriangularQ[j23,j1,j]]


(* ::Input::Initialization:: *)
PhysicalQ::usage="\:041a\:0440\:0438\:0442\:0435\:0440\:0438\:0439 \:0444\:0438\:0437\:0438\:0447\:043d\:043e\:0441\:0442\:0438 \:0441\:0443\:043c\:043c\:044b \:043c\:043e\:043c\:0435\:043d\:0442\:043e\:0432";PhysicalQ[{j1_,j2_,j12_},{j3_,j_,j23_}]:=FullSimplify[TriangleQ[{j1,j2,j12}]\[And]TriangleQ[{j12,j3,j}]\[And]TriangleQ[{j2,j3,j23}]\[And]TriangleQ[{j23,j1,j}]]


(* ::Input::Initialization:: *)
sixJtri::usage="\:041f\:043e\:043a\:0430\:0437\:044b\:0432\:0430\:0435\:0442 \:0441\:043e\:043e\:0442\:0432\:0435\:0441\:0442\:0432\:0438\:0435 \:0430\:0440\:0433\:0443\:043c\:0435\:043d\:0442\:043e\:0432 6J \:0441\:0438\:043c\:0432\:043e\:043b\:0430 \:043f\:0440\:0430\:0432\:0438\:043b\:0443 \:0442\:0440\:0435\:0443\:0433\:043e\:043b\:044c\:043d\:0438\:043a\:0430";sixJtri[{j1_,j2_,j12_},{j3_,j_,j23_}]:=Hold[Tri[j1,j2,j12]Tri[j2,j3,j23]Tri[j12,j3,j]Tri[j23,j1,j]]


(* ::Input::Initialization:: *)
PhysicalQ::usage="\:041a\:0440\:0438\:0442\:0435\:0440\:0438\:0439 \:0444\:0438\:0437\:0438\:0447\:043d\:043e\:0441\:0442\:0438 \:0441\:0443\:043c\:043c\:044b \:043c\:043e\:043c\:0435\:043d\:0442\:043e\:0432";PhysicalQ[{a_,b_,c_},{d_,e_,f_},{g_,h_,j_}]:=FullSimplify[TriangularQ[a,b,c]\[And]TriangularQ[d,e,f]\[And]TriangularQ[g,h,j]\[And]TriangularQ[a,d,g]\[And]TriangularQ[b,e,h]\[And]TriangularQ[c,f,j]]


(* ::Input::Initialization:: *)
nineJtest::usage="\:041f\:043e\:043a\:0430\:0437\:044b\:0432\:0430\:0435\:0442 \:0441\:043e\:043e\:0442\:0432\:0435\:0441\:0442\:0432\:0438\:0435 \:0430\:0440\:0433\:0443\:043c\:0435\:043d\:0442\:043e\:0432 9j \:0441\:0438\:043c\:0432\:043e\:043b\:0430 \:043f\:0440\:0430\:0432\:0438\:043b\:0443 \:0442\:0440\:0435\:0443\:0433\:043e\:043b\:044c\:043d\:0438\:043a\:0430";nineJtest[{a_,b_,c_},{d_,e_,f_},{g_,h_,j_}]:=TableForm[{{TriangleQ[{a,b,c}],TriangleQ[{d,e,f}],TriangleQ[{g,h,j}],TriangleQ[{a,d,g}],TriangleQ[{b,e,h}],TriangleQ[{c,f,j}]}}\[Transpose],TableHeadings->{{"abc","def","ghj","adg","beh","cfj"}, {"{\[NoBreak]\*GridBox[{
{a, b, c},
{d, e, f},
{g, h, j}
}]\[NoBreak])"}}]


(* ::Input::Initialization:: *)
PhysicalQ[{a_,b_,c_},{d_,e_,f_},{g_,h_,j_}]:=FullSimplify[Triangle[{a,b,c}]\[And]TriangleQ[{d,e,f}]\[And]TriangleQ[{g,h,j}]\[And]TriangleQ[{a,d,g}]\[And]TriangleQ[{b,e,h}]\[And]TriangleQ[{c,f,j}]]


(* ::Input::Initialization:: *)
Clear[NineJSymbol]
	NineJSymbol[{j1_,j2_,j3_},{j4_,j5_,j6_},{j7_,j8_,j9_}] := Module[{kmin,kmax},
		kmin = Max[{Abs[j1-j9],Abs[j4-j8],Abs[j2-j6]}];kmax = Min[{Abs[j1+j9],Abs[j4+j8],Abs[j2+j6]}];
		Sum[(-1)^(2k) (2 k+1) SixJSymbol[{j1,j4,j7},{j8,j9,k}]  SixJSymbol[{j2,j5,j8},{j4,k,j6}] SixJSymbol[{j3,j6,j9},{k,j1,j2}],{k,kmin,kmax}]]



