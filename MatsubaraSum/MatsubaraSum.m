(* ::Package:: *)

(* Mathematica Package *)
(* Created by the Code Collector at Mon 19 Feb 2018 19:04:52 *)
(* ===== Begin ===== *)
BeginPackage["MatsubaraSum`"];
(*Exported symbols added here with SymbolName::usage*)
MatsubaraSum::usage="MatsubaraSum[\*StyleBox[\"expr\",\"TI\"],\*StyleBox[\"\!\(z\_1\)\", \"TI\"],\*StyleBox[\"\!\(z\_2\)\", \"TI\"],...] evaluates the Matsubara summation of \*StyleBox[\"expr\", \"TI\"] over the imaginary frequency \*StyleBox[\"\!\(z\_1\)\", \"TI\"],\*StyleBox[\"\!\(z\_2\)\", \"TI\"],....\nMatsubaraSum[\*StyleBox[\"expr\", \"TI\"],{\*StyleBox[\"\!\(z\_1\)\", \"TI\"],\*StyleBox[\"\!\(stat\_1\)\", \"TI\"]},{\*StyleBox[\"\!\(z\_2\)\", \"TI\"],\*StyleBox[\"\!\(stat\_2\)\", \"TI\"]},...] specifies the statistic type \*StyleBox[\"\!\(stat\_i\)\", \"TI\"] (which can be either \*StyleBox[\"Bosonic\", \"Program\"] or \*StyleBox[\"Fermionic\", \"Program\"]) of the imaginary frequency \*StyleBox[\"\!\(z\_i\)\", \"TI\"].";
Fermionic::usage="Fermionic represents the set of fermionic imaginary frequencies.";
Bosonic::usage="Bosonic represents the set of bosonic imaginary frequencies. ";
ControlledPlane::usage="ControlledPlane is an option for MatsubaraSum that specifies which half plane is to be controlled for convergence. The following settings can be used: Left, Right, or All. ";
StatisticalSign::usage="StatisticalSign[z] represents the statistical sign of the imaginary frequency z. ";
DistributionFunction::usage="DistributionFunction[\[Eta],x] specifies the statistical sign \[Eta].";
ZeroTemperatureLimit::usage="ZeroTemperatureLimit[expr] takes the zero temperature limit of the expression.";
Begin["`Private`"];

(* ===== Summation Arithmetic ===== *)

(* ----- Res ----- *)
(* Calculate the Residual of expr at z\[Rule]z0. *)
Options[Res]:={AccuracyGoal->10};
Res[expr_,{z_,z0_},OptionsPattern[Res]]:=Module[{acc,ZeroQ,\[Delta],R1,R},acc=OptionValue[AccuracyGoal];
ZeroQ=PossibleZeroQ[#1]||NumericQ[#1]&&Chop[#1,10^-(#2 acc)]==0&;
R1=Chop[Together[\[Delta] zReplace[expr,z->z0+\[Delta]]],10^-acc];
R[Rp_,m_]:=Module[{Rm},Rm=Chop[Together[((m-1) Rp+\[Delta] D[Rp,\[Delta]])/m],10^-acc];
If[ZeroQ[Denominator[Rm]/.\[Delta]->0,m],R[Rm,m+1],Rm/.\[Delta]->0]];
If[ZeroQ[Denominator[R1]/.\[Delta]->0,1],R[R1,2],R1/.\[Delta]->0]];
(* Replace z by z0 in expr,but skipping the StatisticalSign. *)
zReplace[expr_,z_->z0_]:=If[AtomQ[expr],Replace[expr,z->z0],If[Head[expr]===StatisticalSign,expr,zReplace[#,z->z0]&/@expr]];

(* ----- MatsubaraSum ----- *)
Options[MatsubaraSum]:={ControlledPlane->Right,Assumptions:>$Assumptions};
SyntaxInformation[MatsubaraSum]={"ArgumentsPattern"->{_,__,OptionsPattern[]},"LocalVariables"->{"Integrate",{2,Infinity}}};
SetAttributes[MatsubaraSum,HoldFirst];
(* Single frequency summation *)
MatsubaraSum[expr_,z_\[Element]stat_,OptionsPattern[MatsubaraSum]]:=Assuming[z\[Element]stat&&OptionValue[Assumptions],Module[{frac,den,sign,weight,raw},frac=Together[expr];
den=DeleteCases[Denominator[frac],_?(!PolynomialQ[#,z]&)];
sign=StatisticalSign[z];
weight=Mean[sign # Inactive[DistributionFunction][sign,# z]&/@Switch[OptionValue[ControlledPlane],Left,{-1},Right,{+1},All,{-1,1}]];
Activate@Normal@RootSum[Evaluate[den/.z->#]&,-Res[frac weight,{z,#}]&]]];
MatsubaraSum[expr_,z_,op:OptionsPattern[MatsubaraSum]]:=MatsubaraSum[expr,z\[Element]Indeterminate,op];
(* Multi frequency summation *)
MatsubaraSum[expr_,z1_,zs__,op:OptionsPattern[MatsubaraSum]]:=MatsubaraSum[MatsubaraSum[expr,z1,op],zs,op];

(* ===== Simplifications ===== *)

(* ----- StatisticalSign Properties ----- *)
StatisticalSign/:StatisticalSign[z_]^a_/;EvenQ[a]=1;
StatisticalSign/:StatisticalSign[z_]^a_/;OddQ[a]:=StatisticalSign[z];
StatisticalSign[z_]/;Assumptions`AAssumedQ[z\[Element]Fermionic]=-1;
StatisticalSign[z_]/;Assumptions`AAssumedQ[z\[Element]Bosonic]=+1;

(* ----- DistributionFunction Properties ----- *)
DistributionFunction[\[Eta]_,(a_: 1) (z_?FrequencyQ)+(x_: 0)]:=# DistributionFunction[\[Eta] #,x]&[StatisticalSign[z]^a];
DistributionFunction[-1,0]=1/2;
FrequencyQ[z_]:=Assumptions`AAssumedQ[z\[Element]Bosonic]||Assumptions`AAssumedQ[z\[Element]Fermionic]||Assumptions`AAssumedQ[z\[Element]Indeterminate];

(* ----- DistributionFunction Properties ----- *)
nTransform[expr_]:=Simplify[expr/.{DistributionFunction[\[Eta]_,Times[-1,x_]]:>-\[Eta]-DistributionFunction[\[Eta],x],Derivative[0,k_][DistributionFunction][\[Eta]_,Times[-1,x_]]:>-(-1)^k Derivative[0,k][DistributionFunction][\[Eta],x],DistributionFunction[-1,x_] DistributionFunction[1,x_]:>(DistributionFunction[1,x]-DistributionFunction[-1,x])/2}];
SetOptions[FullSimplify,TransformationFunctions->{Automatic,nTransform}];

(* ----- Zero Temperature Limit ----- *)
ZeroTemperatureLimit[expr_]:=expr/.{DistributionFunction[\[Eta]_,x_]:>\[Eta] (HeavisideTheta[x]-1)}

(* ===== End ===== *)
End[];
SetAttributes[{MatsubaraSum,StatisticalSign,DistributionFunction},{ReadProtected}];
EndPackage[];

(* ===== Notation ===== *)
Notation`AutoLoadNotationPalette = False;
Get["Notation`"];
Notation[\!\(\*
TagBox[
SubscriptBox["\[Eta]", "z_"],
"NotationTemplateTag"]\) \[DoubleLongLeftRightArrow] \!\(\*
TagBox[
RowBox[{"StatisticalSign", "[", "z_", "]"}],
"NotationTemplateTag"]\)];
Notation[\!\(\*
TagBox[
RowBox[{
SubscriptBox["n", "B"], "[", "x_", "]"}],
"NotationTemplateTag"]\) \[DoubleLongLeftRightArrow] \!\(\*
TagBox[
RowBox[{"DistributionFunction", "[", 
RowBox[{"1", ",", "x_"}], "]"}],
"NotationTemplateTag"]\)];
Notation[\!\(\*
TagBox[
RowBox[{
SubscriptBox["n", "F"], "[", "x_", "]"}],
"NotationTemplateTag"]\) \[DoubleLongLeftRightArrow] \!\(\*
TagBox[
RowBox[{"DistributionFunction", "[", 
RowBox[{
RowBox[{"-", "1"}], ",", "x_"}], "]"}],
"NotationTemplateTag"]\)];
Notation[\!\(\*
TagBox[
RowBox[{
SubscriptBox["n", "\[Eta]_"], "[", "x_", "]"}],
"NotationTemplateTag"]\) \[DoubleLongLeftRightArrow] \!\(\*
TagBox[
RowBox[{"DistributionFunction", "[", 
RowBox[{"\[Eta]_", ",", "x_"}], "]"}],
"NotationTemplateTag"]\)];
Notation[\!\(\*
TagBox[
RowBox[{
SubsuperscriptBox["n", "B", "\[Prime]"], "[", "x_", "]"}],
"NotationTemplateTag"]\) \[DoubleLongLeftRightArrow] \!\(\*
TagBox[
RowBox[{
RowBox[{
RowBox[{"Derivative", "[", 
RowBox[{"0", ",", "1"}], "]"}], "[", "DistributionFunction", "]"}], "[", 
RowBox[{"1", ",", "x_"}], "]"}],
"NotationTemplateTag"]\)];
Notation[\!\(\*
TagBox[
RowBox[{
SubsuperscriptBox["n", "F", "\[Prime]"], "[", "x_", "]"}],
"NotationTemplateTag"]\) \[DoubleLongLeftRightArrow] \!\(\*
TagBox[
RowBox[{
RowBox[{
RowBox[{"Derivative", "[", 
RowBox[{"0", ",", "1"}], "]"}], "[", "DistributionFunction", "]"}], "[", 
RowBox[{
RowBox[{"-", "1"}], ",", "x_"}], "]"}],
"NotationTemplateTag"]\)];
Notation[\!\(\*
TagBox[
RowBox[{
SubsuperscriptBox["n", "\[Eta]_", "\[Prime]"], "[", "x_", "]"}],
"NotationTemplateTag"]\) \[DoubleLongLeftRightArrow] \!\(\*
TagBox[
RowBox[{
RowBox[{
RowBox[{"Derivative", "[", 
RowBox[{"0", ",", "1"}], "]"}], "[", "DistributionFunction", "]"}], "[", 
RowBox[{"\[Eta]_", ",", "x_"}], "]"}],
"NotationTemplateTag"]\)];
Notation[\!\(\*
TagBox[
RowBox[{
SubsuperscriptBox["n", "B", "\[DoublePrime]"], "[", "x_", "]"}],
"NotationTemplateTag"]\) \[DoubleLongLeftRightArrow] \!\(\*
TagBox[
RowBox[{
RowBox[{
RowBox[{"Derivative", "[", 
RowBox[{"0", ",", "2"}], "]"}], "[", "DistributionFunction", "]"}], "[", 
RowBox[{"1", ",", "x_"}], "]"}],
"NotationTemplateTag"]\)];
Notation[\!\(\*
TagBox[
RowBox[{
SubsuperscriptBox["n", "F", "\[DoublePrime]"], "[", "x_", "]"}],
"NotationTemplateTag"]\) \[DoubleLongLeftRightArrow] \!\(\*
TagBox[
RowBox[{
RowBox[{
RowBox[{"Derivative", "[", 
RowBox[{"0", ",", "2"}], "]"}], "[", "DistributionFunction", "]"}], "[", 
RowBox[{
RowBox[{"-", "1"}], ",", "x_"}], "]"}],
"NotationTemplateTag"]\)];
Notation[\!\(\*
TagBox[
RowBox[{
SubsuperscriptBox["n", "\[Eta]_", "\[DoublePrime]"], "[", "x_", "]"}],
"NotationTemplateTag"]\) \[DoubleLongLeftRightArrow] \!\(\*
TagBox[
RowBox[{
RowBox[{
RowBox[{"Derivative", "[", 
RowBox[{"0", ",", "2"}], "]"}], "[", "DistributionFunction", "]"}], "[", 
RowBox[{"\[Eta]_", ",", "x_"}], "]"}],
"NotationTemplateTag"]\)];
Notation[\!\(\*
TagBox[
RowBox[{
SubsuperscriptBox["n", "B", 
TagBox[
RowBox[{"(", "k_", ")"}],
Derivative],
MultilineFunction->None], "[", "x_", "]"}],
"NotationTemplateTag"]\) \[DoubleLongLeftRightArrow] \!\(\*
TagBox[
RowBox[{
RowBox[{
RowBox[{"Derivative", "[", 
RowBox[{"0", ",", "k_"}], "]"}], "[", "DistributionFunction", "]"}], "[", 
RowBox[{"1", ",", "x_"}], "]"}],
"NotationTemplateTag"]\)];
Notation[\!\(\*
TagBox[
RowBox[{
SubsuperscriptBox["n", "F", 
TagBox[
RowBox[{"(", "k_", ")"}],
Derivative],
MultilineFunction->None], "[", "x_", "]"}],
"NotationTemplateTag"]\) \[DoubleLongLeftRightArrow] \!\(\*
TagBox[
RowBox[{
RowBox[{
RowBox[{"Derivative", "[", 
RowBox[{"0", ",", "k_"}], "]"}], "[", "DistributionFunction", "]"}], "[", 
RowBox[{
RowBox[{"-", "1"}], ",", "x_"}], "]"}],
"NotationTemplateTag"]\)];
Notation[\!\(\*
TagBox[
RowBox[{
SubsuperscriptBox["n", "\[Eta]_", 
TagBox[
RowBox[{"(", "k_", ")"}],
Derivative],
MultilineFunction->None], "[", "x_", "]"}],
"NotationTemplateTag"]\) \[DoubleLongLeftRightArrow] \!\(\*
TagBox[
RowBox[{
RowBox[{
RowBox[{"Derivative", "[", 
RowBox[{"0", ",", "k_"}], "]"}], "[", "DistributionFunction", "]"}], "[", 
RowBox[{"\[Eta]_", ",", "x_"}], "]"}],
"NotationTemplateTag"]\)];

