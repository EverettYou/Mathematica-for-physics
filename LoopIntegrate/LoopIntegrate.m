(* ::Package:: *)

(* Mathematica Package *)
(* Created by the Code Collector at Mon 26 Dec 2016 10:43:57 *)
(* ===== Begin ===== *)
BeginPackage["LoopIntegrate`"];
Loop::usage="Loop[\!\(TI\`expr\),{\!\(TI\`p\_1\),\!\(TI\`p\_2\),\[Ellipsis]},\!\(TI\`D\),\!\(TI\`x\),\!\(TI\`n\_x\)] denotes the loop integral of \!\(TI\`expr\) over momenta \!\(TI\`p\_1\), \!\(TI\`p\_2\), \[Ellipsis] in total spacetime dimension \!\(TI\`D\) with the symbol \!\(TI\`x\_i\) (\!\(TI\`i=1,\[Ellipsis],n\_x\)) used for Feynman parameters.";
Index::usage="Index[\!\(TI\`p\),\!\(TI\`\[Mu]\)] represents a momentum component \!\(TI\`p\_\[Mu]\).";
LeviCivitaEpsilon::usage="LeviCivitaEpsilon[\!\(TI\`i\_1\),\!\(TI\`i\_2\),\[Ellipsis]] denotes the Levi-Civita symbol.";
IntegrandInformation::usage="IntegrandInformation[\!\(TI\`loop\)] returns the analysis of the integrant of the loop integral \!\(TI\`loop\).";
FeynmanParameterize::usage="FeynmanParameterize[\!\(TI\`expr\)] applies the Feynman parameterization to \!\(TI\`expr\).";
MomentumShift::usage="MomentumShift[\!\(TI\`loop\),\!\(TI\`p\)] performs the momentum shift on \!\(TI\`p\) for the loop integral \!\(TI\`loop\). The loop integral must be Feynman parameterized.";
MomentumIntegrate::usage="MomentumIntegrate[\!\(TI\`loop\),\!\(TI\`p\)] integrate \!\(TI\`loop\) over the momentum \!\(TI\`p\) by applying the momentum integral formula. The loop integral must be Feynman parameterized.";
LoopReduce::usage="LoopReduce[\!\(TI\`expr\)] applies the momentum integral formula to reduce the loop integrals in \!\(TI\`expr\).";
DimensionRegularize::usage="DimensionRegularize[\!\(TI\`expr\),\!\(TI\`D\)\[Rule]\!\(TI\`D\_0\)] gives the dimension regularization of \!\(TI\`expr\) as \!\(TI\`D\)\[Rule]\!\(TI\`D\_0\).";
ParameterReduce::usage="ParameterReduce[\!\(TI\`expr\)] carries out the integral of \!\(TI\`expr\) over Feynman parameters.";
LoopIntegrate::usage="LoopIntegrate[\!\(TI\`expr\),{\!\(TI\`p\_1\),\!\(TI\`p\_2\),\[Ellipsis]},\!\(TI\`D\)] is a high level function that automatically evaluate the loop integral of \!\(TI\`expr\) over the momenta \!\(TI\`p\_1\),\!\(TI\`p\_2\),\[Ellipsis] in \!\(TI\`D\) dimensional spacetime.";
Begin["`Private`"];

(* ===== Symbolic System ===== *)

(* ----- Loop ----- *)
(* Basic properties and argument completion. *)
Loop[0,___]:=0;
Loop[expr_]:=Loop[expr,{},Global`\[GothicCapitalD],Global`x,0];
Loop[expr_,vars:{_Symbol...}]:=Loop[expr,vars,Global`\[GothicCapitalD],Global`x,0];
Loop[expr_,vars:{_Symbol...},D_]:=Loop[expr,vars,D,Global`x,0];
Loop[expr_,{},D_,x_,0]:=expr;
Loop[expr_,var_Symbol,rest___]:=Loop[expr,{var},rest];
SyntaxInformation[Loop]={"ArgumentPattern"->{_,_.,_.,_.},"LocalVariables"->{"Solve",{2}}};
(* Formatting. *)
Format[loop:Loop[expr_,vars:{_Symbol...},D_,x_,nx_]]:=Module[{strx,strnx,strD,dp,dx,form},{strx,strnx,strD}=ToString/@{x,nx,D};
dp[p_]:=RowBox[{"\[Integral]",FractionBox[RowBox[{SuperscriptBox["\[DifferentialD]",strD],ToString@p}],SuperscriptBox["(2\[Pi])",strD]]}];
dx=If[nx==0,Nothing,RowBox[{SubscriptBox["\[Integral]","\[EmptyUpTriangle]"],SuperscriptBox["\[DifferentialD]",strnx],strx}]];
form=RawBoxes@RowBox[Join[{dx},dp/@vars,{ToBoxes@expr}]];
Interpretation[HoldForm[Evaluate@form],loop]];

(* ----- Index ----- *)
(* Realization on arrays with specific indices *)
Index[a_?ArrayQ,i:_Integer..]:=Part[a,i];
(* Algebraic properties. *)
Index[1,__]:=1;
Index[a_?NumericQ,__]:=a;
Index[a:(_Indexed|_Power),___]:=a;
Index[a_+b_,i__]:=Index[a,i]+Index[b,i];
Index[a_ b_,i__]:=Index[a,i] Index[b,i];
(* Formatting. *)
Format[me:HoldPattern[Index[a_,i__]]]:=Interpretation[Tooltip[Subscript[a,i],Index],me];

(* ----- Symbol Format ----- *)
Unprotect[KroneckerDelta];
Format[me:KroneckerDelta[is___]]:=Interpretation[HoldForm[Subscript["\[Delta]",is]],me];
Format[me:LeviCivitaEpsilon[is___]]:=Interpretation[HoldForm[Subscript["\[Epsilon]",is]],me];
Protect[KroneckerDelta];
LeviCivitaEpsilon[is:_Integer..]:=Signature[{is}];

(* ===== Integration System ===== *)

(* ----- Integrand Analysis ----- *)
(* Analyze integrand structure.The integrand must be a single term.Sum of terms must be merged together or analyzed term by term. *)
IntegrandInformation[loop:Loop[expr:Except[_Plus],vars_,D_,x_,nx_]]:=Module[{result,denlst,n,As,as},result=facParser[expr,vars,x]/.wrapper->List;
denlst=Lookup[result,"den",{}];n=Length@denlst;
If[n==0,{As,as}={{},{}},{As,as}=Thread@denlst];
<|"cfac"->Times@@Lookup[result,"cfac",{}],"xfac"->Times@@Lookup[result,"xfac",{}],"num"->Times@@Lookup[result,"num",{}],"n"->n,"As"->As,"as"->as|>];
(* Factor parser split the factors to outer constants (cfac),inner constants (xfac),numerators (num) and denominators (den). *)
SetAttributes[wrapper,{Flat}];(*a flat function to hold the parsing result*)facParser[a_ b_,vars_,x_]:=Merge[{facParser[a,vars,x],facParser[b,vars,x]},wrapper@@#&];
facParser[a_^(n_: 1),vars_,x_]:=Module[{fac,exp},{fac,exp}=pExponent[a,vars];
If[exp==0,<|If[FreeQ[#,x],"cfac","xfac"]->wrapper[#]|>&[fac^n],Switch[If[NumericQ[n],If[n>0,"num","den"],"den"],"num",<|"num"->wrapper[fac^n]|>,"den",<|"den"->wrapper[{fac^(2/exp),-n exp/2}]|>]]];
pExponent[expr_,vars_]:=Module[{ind},{expr,Max[Exponent[expr/._Index:>ind,Append[vars,ind]]]}];
(* New loop integrals can be made based on the factor parsing result,such that the constants are factor out automatically. *)
mkLoop[0,___]:=0;
mkLoop[Indeterminate,___]:=Indeterminate;
mkLoop[ComplexInfinity,___]:=ComplexInfinity;
mkLoop[A_+B_,rest___]:=mkLoop[A,rest]+mkLoop[B,rest];
mkLoop[A:Except[_Plus],vars_: {},D_: Global`\[GothicCapitalD],x_: Global`x,nx_: 0,cfac0_: 1,xfac0_: 1]:=Module[{result,cfac1,xfac1},result=facParser[A,vars,x]/.wrapper->List;
cfac1=cfac0 Times@@Lookup[result,"cfac",{}];
xfac1=xfac0 Times@@Lookup[result,"xfac",{}];
cfac1 Loop[xfac1 Times@@Lookup[result,"num",{}]/Times@@(Power@@@Lookup[result,"den",{}]),vars,D,x,nx]];

(* ----- Feynman Parameterize ----- *)
(* Feynman parameterization. *)
FeynmanParameterize[loop_Loop]:=xFeynmanParameterize[loop,IntegrandInformation@loop];
FeynmanParameterize[expr_]:=expr/.loop_Loop:>FeynmanParameterize[loop];
xFeynmanParameterize[loop:Loop[expr_,vars_,D_,x_,nx_],info_]:=Module[{cfac,xfac,num,As,as,a,xs},If[info@"n"==0,loop,{cfac,xfac,num,As,as}=info/@{"cfac","xfac","num","As","as"};
If[info@"n"==1,cfac Loop[xfac num/Times@@(As^as),vars,D,x,nx],a=Total@as;
xs=Indexed[x,nx+#]&/@Range@Length@as;
cfac=cfac Gamma[a]/(Times@@Gamma[as]);
xfac=xfac Times@@MapThread[Power,{xs,as-1}];
cfac Loop[xfac num/(xs.As)^a,vars,D,x,nx+Length@xs]]]];

(* ----- Momentum Shift ----- *)
(* Perform momentum shift. *)
MomentumShift::mden="Multiple factors `1` of the denominator found in the integrant `2`. Use FeynmanParameterize to combine the denomenant first.";
MomentumShift[loop:Loop[expr_,{},D_,x_,nx_]]:=loop;
MomentumShift[loop:Loop[expr_,vars:{_Symbol..},D_,x_,nx_]]:=MomentumShift[loop,First@vars];
MomentumShift[loop:Loop[expr_,vars_,D_,x_,nx_],p_]:=Module[{info,cfac,xfac,num,r,a},info=IntegrandInformation@loop;
Switch[info@"n",0,loop,1,{cfac,xfac,num,r,a}=xMomentumShift[loop,p,info];
cfac Loop[xfac num/(p^2+r)^a,vars,D,x],_,Message[MomentumShift::mden,info@"As",expr];loop]];
MomentumShift[expr_]:=expr/.loop_Loop:>MomentumShift[loop];
xMomentumShift[loop:Loop[expr_,vars_,D_,x_,nx_],p_,info_]:=Module[{cfac,xfac,num,den,a,m2,q,cp,r},(*must ensure info@"n"\[Equal]1 when calling*){cfac,xfac,num}=info/@{"cfac","xfac","num"};
{den,a}=First/@info/@{"As","as"};
{m2,q,cp}=If[nx==0,#,Simplify[#,Total[Indexed[x,#]&/@Range@nx]==1]]&@CoefficientList[den,p];
{m2,q}={m2,-q/2}/cp;xfac=xfac/cp^a;
r=Simplify[m2-q^2];
num=num/.p->p+q;
{cfac,xfac,num,r,a}];

(* ----- Momentum Integration ----- *)
(* Apply momentum integral formula. *)
MomentumIntegrate::nden="The integrant `1` has no denominator that depends on the integral momentum `2`.";
MomentumIntegrate::mden="Multiple factors `1` of the denominator found in the integrant `2`. Use FeynmanParameterize to combine the denomenant first.";
MomentumIntegrate::oexp="The numerator of loop integrant `1` contains odd powers of integral momentum `2`.";
MomentumIntegrate[loop:Loop[expr_,{},D_,x_,nx_]]:=loop;
MomentumIntegrate[loop:Loop[expr_,vars:{_Symbol..},D_,x_,nx_]]:=MomentumIntegrate[loop,First@vars];
MomentumIntegrate[loop:Loop[expr_,vars_,D_,x_,nx_],p_]:=Module[{info,cfac,xfac,num,den,r,a},info=IntegrandInformation@loop;
Switch[info@"n",1,{cfac,xfac,num}=info/@{"cfac","xfac","num"};
{den,a}=First/@info/@{"As","as"};
r=Coefficient[den,p,0];
cfac=cfac/(4 \[Pi])^(D/2);
mkLoop[xMomentumIntegral[num,p,r,a,D],Complement[vars,{p}],D,x,nx,cfac,xfac],0,Message[MomentumIntegrate::nden,expr,p];loop,_,Message[MomentumIntegrate::mden,info@"As",expr];loop]];
MomentumIntegrate[expr_]:=expr/.loop_Loop:>MomentumIntegrate[loop];
xMomentumIntegrate[num_,p_,r_,a_,D_]:=Block[{contract,cwrapper,ptrans},contract[is:_Integer..]:=KroneckerDelta[is];
contract[i_,i_]:=D;
contract[i_,j_]:=KroneckerDelta[i,j];
ptrans[x_+y_]:=ptrans[x]+ptrans[y];
ptrans[cwrapper[c_]]:=c Gamma[a-D/2]/(Gamma[a] r^(a-D/2));
ptrans[cwrapper[c_] p1:(p|_Power|_Index)]:=c ptrans[Flatten[{p1}/.Power[p2_Index,n2_]:>ConstantArray[p2,n2]]];
ptrans[cwrapper[c_] pp_Times]:=c ptrans[Flatten[(List@@pp)/.Power[p1_Index,n1_]:>ConstantArray[p1,n1]]];
ptrans[plst:{pre___,p^(n_: 1),post___},aa_: a]:=Module[{restplst},restplst={pre,post};
If[EvenQ[n],Total[Binomial[n/2,#] (-r)^(n/2-#) ptrans[restplst,aa-#]&/@Range[0,n/2]],Message[MomentumIntegrate::oexp,num,p];0]];
ptrans[plst:{Index[p,_]...},aa_: a]:=Module[{inds,n},inds=Cases[plst,Index[p,i_]:>i];
n=Length@inds;
If[n==0,Gamma[aa-D/2]/(Gamma[aa] r^(aa-D/2)),If[OddQ[n],0,Gamma[aa-n/2-D/2]/(Gamma[aa] r^(aa-n/2-D/2) 2^(n/2))] Total[Times@@(contract@@@#)&/@pairing[inds]]]];
ptrans@Expand@Collect[num,p|Index[p,_],cwrapper]];
(* Find all pairings of a list.Called by xMomentumIntegrate for Wick contraction. *)
pairing[inds_]:=Join@@Table[{pair,##}&@@@pairing[Complement[inds,pair]],{pair,{First[inds],#}&/@Rest[inds]}]
pairing[{}]={{}};

(* ----- Loop Reduce ----- *)
(* Loop momentum reduction. *)
LoopReduce::nden="The integrant `1` has no denominator that depends on the integral momentum `2`.";
LoopReduce[loop:Loop[expr_,vars_List,D_,x_,nx_]]:=Fold[LoopReduce,loop,vars];
LoopReduce[loop:Loop[expr_,vars_,D_,x_,nx_],p_]:=Module[{info,cfac,xfac,num,r,a},info=IntegrandInformation@loop;
Switch[info@"n",1,{cfac,xfac,num,r,a}=xMomentumShift[loop,p,info];
cfac=cfac/(4 \[Pi])^(D/2);
mkLoop[xMomentumIntegrate[num,p,r,a,D],Complement[vars,{p}],D,x,nx,cfac,xfac],0,Message[LoopReduce::nden,expr,p];loop,_,LoopReduce[xFeynmanParameterize[loop,info],p]]];
LoopReduce[expr_,rest___]:=expr/.loop_Loop:>LoopReduce[loop,rest];

(* ----- Dimension Regularization ----- *)
(* Dimension regularization. *)
DimensionRegularize[0,_]:=0;
DimensionRegularize[Indeterminate,_]:=Indeterminate;
DimensionRegularize[a_+b_,Dlim_]:=DimensionRegularize[a,Dlim]+DimensionRegularize[b,Dlim];
DimensionRegularize[(loop:Loop[expr_,vars_,D_,x_,nx_]) (cfac_: 1),D_->D0_]:=mkLoop[xDimensionRegularize[cfac expr,D->D0],vars,D,x,nx];
DimensionRegularize[expr_?(FreeQ[Loop]),D_->D0_]:=xDimensionRegularize[expr,D->D0];
SyntaxInformation[DimensionRegularize]={"ArgumentsPattern"->{_,_},"LocalVariables"->{"Limit",{2}}};
xDimensionRegularize[expr_,D_->D0_]:=Module[{raw,res,reg},Block[{\[Epsilon]},raw=expr/.D->D0-\[Epsilon];
res=Residue[raw,{\[Epsilon],0}];
reg=SeriesCoefficient[raw,{\[Epsilon],0,0}];
If[res===0,reg,logreg[reg]]]];
logreg[a_?NumericQ+b_]:=logreg[b];
logreg[a_ b_]:=logreg[a] logreg[b];
logreg[a_?NumericQ]:=a;
logreg[a_?(FreeQ[Log])]:=a;
logreg[Log[a_]]:=Module[{vars,log},vars=DeleteCases[Variables[a],_Indexed];
Replace[Log[a/(a/.(#->Global`\[CapitalLambda]&/@vars))],Log[y1_^n1_ y2_^n2_]/;n1+n2==0:>n1 Log[y1/y2]]];

(* ----- Parameter Reduction ----- *)
ParameterReduce[loop:Loop[expr_,vars_,D_,x_,nx_]]:=Module[{xs,intlim},Switch[nx,0,loop,1,mkLoop[expr/.Indexed[x,1]->1,vars,D,x,0],_,xs=Indexed[x,#]&/@Range@nx;
mkLoop[Integrate[expr/.Last[xs]->1-Total@Most[xs],Sequence@@First@Last@Reap[Fold[(Sow[{#2,0,1-Total@#1}];
Append[#1,#2])&,{},Most[xs]]],GenerateConditions->False]],vars,D,x,0]];
ParameterReduce[expr_]:=expr/.loop_Loop:>ParameterReduce[loop];

(* ----- LoopIntegrate ----- *)
SyntaxInformation[LoopIntegrate]={"ArgumentsPattern"->{_,_.,_.},"LocalVariables"->{"Solve",{2}}};
LoopIntegrate[expr_,vars_: {},D0_: Global`\[GothicCapitalD]]:=If[IntegerQ[D0],ParameterReduce@DimensionRegularize[LoopReduce@mkLoop[Expand@expr,Flatten@{vars},D],D->D0],ParameterReduce@LoopReduce@mkLoop[Expand@expr,Flatten@{vars},D0]];

(* ===== End ===== *)
End[];
EndPackage[];

