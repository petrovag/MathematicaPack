(* ::Package:: *)

BeginPackage["Alexey`GramSchmidt`"]


ProjectionO::usage="ProjectionO[v1,v2] - it is projection vector v1 on v2";
GramSchmidt::usage="Procedure of the orthogonalisation for matrix of eighenvalues";



ProjectionO[v1_,v2_]:=(Conjugate[v1].v2*v2)/Conjugate[v2].v2;
MultipleProjection[v1_,vecs_]:=Plus@@(ProjectionO[v1,#1]&)/@vecs;
GramSchmidt[mat_]:=Fold[Join[#1,{#2-MultipleProjection[#2,#1]}]&,{},mat];


EndPackage[]
