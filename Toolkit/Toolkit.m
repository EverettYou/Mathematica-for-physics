(* ::Package:: *)

(* ::Code::Initialization:: *)
BeginPackage["Toolkit`"];
BZRules::usage="BZRules contains the rules for named momentum points.";
MomentumResolution::usage="MomentumResolution specifies the resolution of momentum in the BZPlot.";
BZPlot::usage="BZPlot makes Brillouin zone plot.";
ComplexMatrixPlot::usage="ComplexMatrixPlot makes matrix plot with complex colors.";
ComplexColor::usage="ComplexColor is a color function for complex numbers.";
ListDensityPlotR::usage="ListDensityPlotR makes ListDensityPlot using Raster.";
tTr::usage="tTr[{T1,T2,...},{{i1,j1},{i2,j2},...}] perform the tensor trace over a tensor network consist of tensors T1,T2,... by contracting pairs of legs specified by {i1,j2},{i2,j2}....\ntTr[Ts,ijs,{k1,k2,...}] after tensor trace, the dangling legs k1,k2,... should be arranged in the resulting tensor according to the order given in the list.\ntTr[Ts,ijs,{{k1,k2},{k3},{},...}] the dangling legs should be group together in the new tensor according to the pattern specified in the list (an empty leg can be represented by {}). ";
Eye::usage="Eye[d,n] returns the n-leg identity tensor with bond dimension d.";
Hermitianize::usage="Hermitianize[A] make matrix A Hermitian.";
Pf::usage="Pf[\!\(\*StyleBox[\"m\", \"TI\"]\)] gives the pfaffian of the screw symmetric matrix \!\(\*StyleBox[\"m\", \"TI\"]\). ";
Arrowhead::usage="Arrowhead[size, style] represent an arrowhead (as Graphics) of given absolute size. The style can be Filled, Unfilled, HalfFilled, HalfUnfilled.";
NumpyGet::usage="NumpyGet imports numpy array data.";
FortranGet::usage="FortranGet imports Fortran array data.";
FortranPut::usage="FortranPut exports Fortran array data.";
TensorLoad::usage="TensorLoad imports tensor data as SparseArray.";
TensorSave::usage="TensorSave exports SparseArray to tensor data.";
TensorCode::usage="TensorCode returns the code to compose the tensor.";
PostRules::usage="PostRules specifies the post rules for TensorCode.";
NextGraphics::"usage"="FollowingGraphics[n] returns the graphics in the next n cells.";
Begin["`Private`"];


(* ::Code::Initialization:: *)
Options[BZPlot]=
Join[{BZRules->Automatic,MomentumResolution->\[Pi]/32},Options[ListLinePlot]];
me:BZPlot[fun_,route_,opts:OptionsPattern[BZPlot]]:=
Module[{rules,div=OptionValue[MomentumResolution],
nroute,ks,ls,ticks,mticks},
rules=Switch[OptionValue[BZRules],
Automatic,
With[{dim=Max[Cases[DownValues[fun],HoldPattern[fun[l_]]:>Length[l],Infinity]]},
{"\[CapitalGamma]"->ConstantArray[0,dim],_->ConstantArray[\[Pi],dim]}],
_,
Flatten[SetBZRules[OptionValue[BZRules]]]];
nroute=N[route/.rules];
If[!MatchQ[nroute,{{_Real..}..}],Message[BZPlot::ursl];Return[HoldForm[me]]];
{ks,ls,ticks}=
Flatten[Reap[
Fold[
Function[{info,nextK},
Module[{klst,nowK,Ls, nowL,pl,inds},
{klst,Ls}=info;
nowK=Last[klst];
Sow[nowL=Last[Ls]];
pl=Norm[nextK-nowK];
inds=(Range[#]/#)&@Round[pl/div];
{Join[klst,(nowK+#(nextK-nowK))&/@inds],
Join[Ls,(nowL+# pl)&/@inds]}]],
{{First[#]},{0}},Rest[#]]],1]&@nroute;
ticks=Append[ticks,Last[ls]];
mticks=Cases[Thread[{ticks,route}],{_,_String}];
(* carry out the diagonalization and plot band structure *)
ListLinePlot[
Thread[{ls,#}]&/@Thread[fun/@ks],
FilterRules[{opts}/.{"MomentumTicks"->mticks},Options[ListLinePlot]],
AxesStyle->Directive[AbsoluteThickness[0.8],GrayLevel[0.7]],
Frame->True,
FrameTicks->{{Automatic,Automatic},{mticks,None}},
FrameStyle->AbsoluteThickness[0.8],
GridLinesStyle->Directive[AbsoluteThickness[0.8],GrayLevel[0.7]],
GridLines->{ticks,None}]
];
SetBZRules["Square",a_:1]:={"\[CapitalGamma]"->{0,0},"X"->{\[Pi],0}/a,"Y"->{0,\[Pi]}/a,"M"->{\[Pi],\[Pi]}/a,"A"->{\[Pi]/2,\[Pi]/2}/a,"B"->{-\[Pi]/2,\[Pi]/2}/a};
SetBZRules["Rectangular",{a_:1,b_:1}]:={"\[CapitalGamma]"->{0,0},"X"->{\[Pi]/a,0},"Y"->{0,\[Pi]/b},"M"->{\[Pi]/a,\[Pi]/b}};
SetBZRules["Hexagonal"|"Honeycomb",a_:1]:={"\[CapitalGamma]"->{0,0},"M"->\[Pi] {0,2/3}/a,"K"->\[Pi] {2/(3Sqrt[3]),2/3}/a,"A"->{2\[Pi]/(3Sqrt[3]),0}/a,"B"->{2\[Pi]/(3Sqrt[3]),2\[Pi]/9}/a,"K1"->\[Pi] {4/(3Sqrt[3]),0}/a,"K2"->\[Pi] {2/(3Sqrt[3]),-2/3}/a,"M1"->\[Pi] {1/Sqrt[3],1/3}/a,"M2"->\[Pi] {1/Sqrt[3],-1/3}/a,"M3"->\[Pi] {0,-2/3}/a};
SetBZRules["SimpleCubic"|"sc",a_:1]:={"\[CapitalGamma]"->{0,0,0},"X"->{\[Pi],0,0}/a,"Y"->{0,\[Pi],0}/a,"Z"->{0,0,\[Pi]}/a,"M"->{\[Pi],\[Pi],0}/a,"R"->{\[Pi],\[Pi],\[Pi]}/a};
SetBZRules[{s_String,p___}]:=SetBZRules[s,p];
SetBZRules[r_Rule]:=r;
SetBZRules[l_List]:=SetBZRules/@l;
SetBZRules[s_String,___]:=Block[{},Message[BZPlot::schx,s];{}]
BZPlot::schx="`1` is not a known lattice, like Square, Rectangular, Hexagonal(Honeycomb), SimpleCubic(sc). Consider adding it to the Toolkit package.";
BZPlot::ursl="Unable to resolve the momentum route.";


(* ::Code::Initialization:: *)
ComplexMatrixPlot[mat_,op:OptionsPattern[MatrixPlot]]:=
Module[{a,m},
a=Max@Abs@mat;
m=If[a>0.,mat/a,mat];
MatrixPlot[m,
Flatten[Join[{op},
{ColorRules->{z_:>ComplexColor[z]},
Frame->True,
FrameStyle->Opacity[0],FrameTicksStyle->Opacity[1]}]]]]


(* ::Code::Initialization:: *)
ComplexColor[0]=RGBColor[1,1,1];
ComplexColor[z_?NumericQ]:=Lighter[Blend[{RGBColor[0.91, 0.36, 0.46],RGBColor[1., 0.59, 0.4],RGBColor[0.97, 0.76, 0],RGBColor[0.72, 0.8, 0.46],RGBColor[0.25, 0.73, 0.3],RGBColor[0, 0.76, 0.93],RGBColor[0.43, 0.58, 0.99],RGBColor[0.72, 0.53, 0.995],RGBColor[0.91, 0.36, 0.46]},Mod[Arg[z]/(2\[Pi]),1]],1-Abs[z]];
ComplexColor[_]:=RGBColor[0,0,0];


(* ::Code::Initialization:: *)
Options[ListDensityPlotR]=Sort@Join[{DataRange->Automatic,PlotRange->Automatic,ColorFunction->Automatic},Options[Graphics]];
ListDensityPlotR[data_List,opts:OptionsPattern[]]:=
Module[{rng,drng,xmin,xmax,ymin,ymax,cfun},
Switch[OptionValue[PlotRange],
_?NumericQ,rng=Abs[OptionValue[PlotRange]],
All,rng=Max[Abs[Cases[Flatten[data],_?NumericQ]]],
_,rng=3StandardDeviation[Cases[Flatten[data],_?NumericQ]]];
rng=If[rng==0.,1,Ceiling[rng,10^Floor[RealExponent[rng]]]];
{{xmin,xmax},{ymin,ymax}}=
Switch[drng=OptionValue[DataRange],
Automatic,{1,#}&/@Reverse[Dimensions[data]],
_,drng];
cfun=Switch[OptionValue[ColorFunction],
Automatic,Blend[{RGBColor[0,0,2/3],RGBColor[1,1,1],RGBColor[2/3,0,0]},#]&,
_,OptionValue[ColorFunction]];
Graphics[
{Raster[Re[data],{{xmin,ymin},{xmax,ymax}},{-rng,rng},ColorFunction->cfun],
Text[Style["\[PlusMinus]"<>ToString[N[rng]],12,Opacity[0.5]],{xmax,ymax},{1,1}]},
FilterRules[{opts},DeleteCases[Options[Graphics],PlotRange->_]],AspectRatio->1,Frame->True]
];


(* ::Code::Initialization:: *)
tTr[Ts_,s_:{}]:=Activate@TensorContract[Inactive[TensorProduct]@@Ts,s];
tTr[Ts_,s_:{},{}]:=tTr[Ts,s];
tTr[Ts_,s_:{},ex:{_Integer..}]:=Activate@Transpose[TensorContract[Inactive[TensorProduct]@@Ts,s],Ordering@ex];
tTr[Ts_,s_:{},ex:{_List..}]:=Module[{ten,dim,shape},ten=tTr[Ts,s,Flatten@ex];dim=Dimensions[ten];shape=Times@@Take[dim,#]&/@MapThread[{#1+1,#2}&,Through@{Most,Rest}@Prepend[Accumulate[Length/@ex],0]];ArrayReshape[If[ArrayDepth[ten]==0,{ten},ten],shape]];
Eye[d_,n_:2]:=SparseArray[Band[ConstantArray[1,n]]->1,ConstantArray[d,n]];
Hermitianize[A_]:=(A+ConjugateTranspose[A])/2;


Pf[A_]:=
	Switch[Length[A],
		0,1,
		_?OddQ,0,
		_?EvenQ,
		If[MatchQ[A,{{_?NumericQ...}...}],
			nPf[A],
			xPf[A,1]
		]
	];
(* analytic *)
xPf[A_,p0_]:=
Module[{A0,n,pivot,sign=1,A1,p1},
	n=Length[A]/2;
	If[n==1,A[[1,2]],
		A0=A;
		pivot=First[Ordering[Abs[A0[[2 n-1,All]]],-1]];
		If[pivot!=2 n,
			A0[[{pivot,2 n},All]]=A0[[{2 n,pivot},All]];
			A0[[All,{pivot,2 n}]]=A0[[All,{2 n,pivot}]];
			sign=-1;
		];
		p1=A0[[2 n-1,2 n]];
		A1=p1 A0[[1;;2 n-2,1;;2 n-2]];
		A1+=(#-Transpose[#])&@
			Outer[Times,A0[[1;;2 n-2,2 n]],A0[[1;;2 n-2,2 n-1]]];
		A1/=p0;
		sign xPf[A1,p1]
	]
];
(* numerics *)
nPf[A_]:=Det[#1](Times@@Diagonal[#2,1][[1;;-1;;2]])&@@SchurDecomposition[A];


(* ::Code::Initialization:: *)
Options[Arrowhead]={
"Alignment"->1,
"AspectRatio"->5/16,
"Setback"->1/8,
"FrontArcControl"->{0,0},
"BackArcControl"->{1/4,1/6},
"Multiplicity"->1};
SetArcCtrlPt[p1_,p2_,v_]:=
(p1+p2)/2+v.{{#1,#2},{-#2,#1}}&@@(p2-p1);
Arrowhead[size_Integer:10,sty_String:"Filled",OptionsPattern[Arrowhead]]:=
Module[{p0,p1,p2,p3,p4,f,b,Rev,C,ah},
p0={0,0};
p1={-1,OptionValue["AspectRatio"]};
p2={-1+OptionValue["Setback"],0};
p3=SetArcCtrlPt[p1,p0,OptionValue["FrontArcControl"]];
p4=SetArcCtrlPt[p1,p2,OptionValue["BackArcControl"]];
f={p0,p3,p1};
b={p1,p4,p2};
Rev[ps_]:=Reverse[#{1,-1}&/@ps];
C=BSplineCurve[Offset/@(size #)]&;
ah=Switch[sty,
"Filled",FilledCurve[C/@{f,Join[b,Rev[b]],Rev[f]}],
"HalfFilled",FilledCurve[C/@{f,b}],
"Unfilled",C/@{f,Rev[f]},
"HalfUnfilled",C/@{f}];
With[{M=OptionValue["Multiplicity"]},
Graphics[
Table[ah/.Offset[{x_,y_}]:>
Offset[{x+size(M OptionValue["Alignment"]-(m-1)(1-OptionValue["Setback"])),y}],
{m,M}]]]
]


(* ::Code::Initialization:: *)
$BinaryTypes={"Byte","Character8","Character16","Complex64","Complex128","Complex256","Integer8","Integer16","Integer32","Integer64","Integer128","Real32","Real64","Real128","TerminatedString","UnsignedInteger8","UnsignedInteger16","UnsignedInteger32","UnsignedInteger64","UnsignedInteger128"};


(* ::Code::Initialization:: *)
CompleteExtension[filename_,extension_]:=
If[FileExtension[filename]=="",filename<>"."<>extension,filename];


(* ::Code::Initialization:: *)
NumpyGet[filename_String]:=Module[{str,magic,vera,verb,hlen,dict,parser,typemap,bitorder,type,shape,raw},
parser[{st_,mm_},input_]:=
Module[{state=st,memory=mm},
Switch[state,
0,(*out of dict*)
Switch[input,"{",state=1;Sow["{"]],
1,(*dict mode*)
Switch[input,
":",
Sow["->"],
",",
Sow[","],
" ",
Null,
"}",(*exit dict mode*)
state=0;Sow["}"],
"'",(*enter string mode*)
state=2;memory="",
"(",(*enter list mode*)
state=3;Sow["{"];memory="",
_,(*enter expression mode*)
state=4;memory=input],
2, (*string mode*)
Switch[input,
"'", (*exit string mode*)
state=1;Sow["\""<>memory<>"\""],
_, (*add input to memory*)
memory=memory<>input],
3, (*list mode*)
Switch[input,
")",(*exit list mode*)
state=1;Sow[memory<>"}"],
_,
memory=memory<>input],
4, (*expression mode*)
Switch[input,
","|" ",(*exit expression mode*)
state=1;Sow[memory];
parser[{state,memory},input](*parse again*),
_,(*add to memory*)
memory=memory<>input],
_,
Null];
{state,memory}];
typemap=<|"b"->"Byte","i"->"Integer","u"->"UnsignedInteger","f"->"Real","c"->"Complex","a"->"Character"|>;
str=OpenRead[CompleteExtension[filename,"npy"],BinaryFormat->True];magic=StringJoin@BinaryReadList[str,"Character8",6];
If[magic!="\.93NUMPY",Return[]];
{vera,verb}=BinaryReadList[str,"UnsignedInteger8",2];
hlen=Switch[vera,1,BinaryRead[str,"UnsignedInteger16"],2,BinaryRead[str,"UnsignedInteger32"]];
dict=BinaryReadList[str,"Character8",hlen];
Off[Syntax::com];
dict=Association@DeleteCases[ToExpression@StringJoin@First@Last@Reap@Fold[parser,{0,""},dict],Null];
On[Syntax::com];
bitorder=Switch[StringTake[dict["descr"],{1}],"<",-1,">",1];
type=typemap@StringTake[dict["descr"],{2}]<>ToString[8ToExpression@StringTake[dict["descr"],{3,-1}]];
shape=DeleteCases[dict["shape"],Null];
raw=BinaryReadList[str,type,Times@@shape];
Close[str];
If[dict["fortran_order"],
Transpose[First[Fold[Partition,raw,shape]],Reverse[Range[Length[shape]]]],
First[Fold[Partition,raw,Reverse@shape]]]];


(* ::Code::Initialization:: *)
FortranGet[filename_String]:=
Module[{str,BinaryType,ArrayDepth,ArrayDimensions,ArrayContents},
str=OpenRead[CompleteExtension[filename,"dat"],BinaryFormat->True];
BinaryType=$BinaryTypes[[BinaryRead[str,"Byte"]]];
ArrayDepth=BinaryRead[str,"Byte"];
ArrayDimensions=BinaryReadList[str,"Integer32",ArrayDepth];
ArrayContents=BinaryReadList[str,BinaryType,Times@@ArrayDimensions];
Close[str];
Transpose[First[Fold[Partition,ArrayContents,ArrayDimensions]],Reverse[Range[ArrayDepth]]]];


(* ::Code::Initialization:: *)
FortranPut[filename_,array_]:=
Module[{str,BinaryType,ArrayContents},
Close/@Streams[CompleteExtension[filename,"dat"]];
ArrayContents=Flatten[Transpose[array,Reverse[Range[Depth[array]-1]]]];
str=OpenWrite[CompleteExtension[filename,"dat"],BinaryFormat->True];
BinaryType=Which[
MemberQ[ArrayContents,_Complex],"Complex128",
MemberQ[ArrayContents,_Real],"Real64",
MemberQ[ArrayContents,_Integer],"Integer32",
True,"TerminatedString"
];
BinaryWrite[str,First[Flatten[Position[$BinaryTypes,BinaryType]]],"Byte"];
BinaryWrite[str,Depth[array]-1,"Byte"];
BinaryWrite[str,Dimensions[array],"Integer32"];
BinaryWrite[str,ArrayContents,BinaryType];
Close[str];
];


(* ::Code::Initialization:: *)
TensorLoad[filename_]:=
Module[{str,Nten,Ndim,Nrec,dims,inds,vals,tens},
str=OpenRead[CompleteExtension[filename,"ten"],BinaryFormat->True];
Nten=BinaryRead[str,"Integer32"];
tens=Table[
Ndim=BinaryRead[str,"Integer32"];
Nrec=BinaryRead[str,"Integer32"];
dims=BinaryReadList[str,"Integer32",Ndim];
inds=BinaryReadList[str,"Integer32",Nrec];
vals=BinaryReadList[str,"Complex128",Nrec];
SparseArray[
If[inds==={},
{},
MapThread[Rule,
{Thread@First@Last@Reap@Fold[
((Sow[Last@#+1];First@#)&@
Thread[QuotientRemainder[#1,#2]])&,
inds,dims],
vals}]],dims],{i,Nten}];
Close[str];
If[Nten==1,First[tens],tens]
];


(* ::Code::Initialization:: *)
TensorSave[filename_,ten_SparseArray]:=TensorSave[filename,{ten}];
TensorSave[filename_,array:{_SparseArray..}]:=
Module[{str,Nten,Ndim,Nrec,dims,inds,vals},
Close/@Streams[CompleteExtension[filename,"ten"]];
str=OpenWrite[CompleteExtension[filename,"ten"],BinaryFormat->True];
Nten=Length[array];
BinaryWrite[str,Nten,"Integer32"];
Function[{ten},
dims=Dimensions[ten];
Ndim=Length[dims];
With[{rules=Most@ArrayRules@ten,Ds=Most@FoldList[Times,1,dims]},
{inds,vals}=If[rules==={},
{{},{}},
Thread@SortBy[Function[{ind,val},{(ind-1).Ds,val}]@@@rules,First]]];
Nrec=Length[inds];
BinaryWrite[str,Ndim,"Integer32"];
BinaryWrite[str,Nrec,"Integer32"];
BinaryWrite[str,dims,"Integer32"];
BinaryWrite[str,inds,"Integer32"];
BinaryWrite[str,vals,"Complex128"];]/@array
Close[str];
]


(* ::Code::Initialization:: *)
Options[TensorCode]={PostRules->{}};
TensorCode[array_SparseArray,opts:OptionsPattern[]]:=
Module[{dims,inds,vals,ToVec},
dims=Dimensions[array];
With[{Ds=Most@FoldList[Times,1,dims]},
{inds,vals}=Thread@
SortBy[Function[{ind,val},{(ind-1).Ds,val}]@@@
Most@ArrayRules@array,
First]];
ToVec=StringJoin@Riffle[ToString/@FortranForm/@#,","]&;
"TENSOR(["<>ToVec[dims]<>"],["<>ToVec[inds]<>"],["<>StringReplace[ToVec[vals],OptionValue[PostRules]]<>"])"]


(* ::Code::Initialization:: *)
NextGraphics[n_Integer:1]:=NextGraphics[Range@n];
NextGraphics[ns_List]:=ToExpression/@Cases[NotebookRead[Nest[NextCell[#,CellStyle->"Output"]&,EvaluationCell[],#+1]]&/@ns,_GraphicsBox,3];


(* ::Code::Initialization:: *)
End[];
EndPackage[];
