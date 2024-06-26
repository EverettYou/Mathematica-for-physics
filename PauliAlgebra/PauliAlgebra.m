(* ::Package:: *)

(* Mathematica Package *)
(* Author: Everett You *)
(* Created by the Code Collector at Sat 6 Apr 2024 11:53:31 *)
(* from file: /Users/home/Library/CloudStorage/Dropbox/Mathematica/Projects/PauliAlgebra/developer.nb *)
(* ===== Begin ===== *)
BeginPackage["PauliAlgebra`"];
\[Sigma]::usage="\[Sigma][\!\(TI\`i\_1\),\!\(TI\`i\_2\),\[Ellipsis]] denotes \!\(TI\`\[Sigma]\^\(i\_1,i\_2,\[Ellipsis]\)\).";
\[Sigma]0::usage="\[Sigma]0[\!\(TI\`expr\)] gives the identity matrix of the same qubit dimension as \!\(TI\`expr\).";
Qubit::usage="Qubit[\!\(TI\`A\)] returns \!\(log\_2\) of the dimension of \[Sigma]-polynominal \!\(TI\`A\).";
\[Sigma]PolynomialQ::usage="\[Sigma]PolynomialQ[\!\(TI\`A\)] yield True if \!\(TI\`A\) is a \[Sigma]-polynomial, and yields Flase otherwise.";
C4::usage="C4[\!\(TI\`Q\)] returns C4 roation matrix generated by \!\(TI\`Q\) as \!\(exp((\[ImaginaryI] \[Pi]/4)\(TI\`Q\))\).";
Hadamard::usage="Hadamard[\[Sigma][0\[Ellipsis],_,0\[Ellipsis]]] returns the Hadamard gate on a marked qubit.";
Swap::usage="Swap[\[Sigma][0\[Ellipsis],_,0\[Ellipsis],_,0\[Ellipsis]]] returns the swap gate acting between two marked qubits.";
Controlled::usage="Controlled[\[Sigma][_,\[Ellipsis]]] returns the transform matrix acting on the lower subspace of a marked control qubit.\nControlled[A,B] returns the transformation of B controlled on A or vice versa";
Cl::usage="Cl[\!\(TI\`p\),\!\(TI\`q\)] gives the generators of the Clifford algebra \!\(TI\`Cl\_\(p,q\)\), consist of \!\(TI\`p\) symmetric matrices and \!\(TI\`q\) antisymmetric matrices.";
LieAlgebra::usage="LieAlgebra[{\!\(TI\`x\_1\),\!\(TI\`x\_2\),...}] closes the Lie algebra based on \!\(TI\`x\_1\),\!\(TI\`x\_2\),... under commutator. The result is a list of the algebra basis, such that \!\([\(TI\`x\_i\),\(TI\`x\_j\)]=\(TI\`f\_ijk x\_k\)\) is still an element in the algebra, i.e. a linear superposition of basis elements.";
Commutator::usage="Commutator[\!\(TI\`A\),\!\(TI\`B\)] gives the commutator \!\(TI\`[A,B]=A\[CenterDot]B-B\[CenterDot]A\).";
Anticommutator::usage="Anticommutator[\!\(TI\`A\),\!\(TI\`B\)] gives the anticommutator \!\(TI\`{A,B}=A\[CenterDot]B+B\[CenterDot]A\).";
CommuteQ::usage="CommuteQ[\!\(TI\`A\),\!\(TI\`B\)] yields True if \!\(TI\`A\[CenterDot]B=B\[CenterDot]A\), and yield False if not.\nCommuteQ[\!\(TI\`B\)] represents an operator that tests if an expression commute with \!\(TI\`B\).";
AnticommuteQ::usage="AnticommuteQ[\!\(TI\`A\),\!\(TI\`B\)] yields True if \!\(TI\`A\[CenterDot]B=-B\[CenterDot]A\), and yield False if not.\nAnticommuteQ[\!\(TI\`B\)] represents an operator that tests if an expression anticommute with \!\(TI\`B\).";
\[Sigma]Transpose::usage="\[Sigma]Transpose[\!\(TI\`A\)] transposes a \[Sigma]-polynomial \!\(TI\`A\[Rule]A\[Transpose]\).";
\[Sigma]Conjugate::usage="\[Sigma]Conjugate[\!\(TI\`A\)] complex conjugates a \[Sigma]-polynomial \!\(TI\`A\[Rule]A\^*\).";
\[Sigma]Hermitian::usage="\[Sigma]Hermitian[\!\(TI\`A\)] Hermitian conjugates a \[Sigma]-polynomial \!\(TI\`A\[Rule]A^\[Dagger]\).";
SymmetricQ::usage="SymmetricQ[\!\(TI\`A\)] yields True if \!\(TI\`A\[Transpose]=A\), and yield False if not.";
AntisymmetricQ::usage="AntisymmetricQ[\!\(TI\`A\)] yields True if \!\(TI\`A\[Transpose]=-A\), and yield False if not.";
\[Sigma]Select::usage="\[Sigma]Select[{\!\(TI\`crit\_i\),\[Ellipsis]},\!\(TI\`n\)] picks out all the \!\(TI\`n\)-qubit \[Sigma]-monomials \!\(TI\`\[Sigma]\_j\) for which all the criterions \!\(TI\`crit\_i\)[\!\(TI\`\[Sigma]\_j\)] yields True.\n\[Sigma]Select[\!\(TI\`crits\),\!\(TI\`list\)] picks out all the elements in \!\(TI\`list\) satisfying all the criterions \!\(TI\`crits\).";
OrthogonalTransform::usage="OrthogonalTransform[\!\(TI\`O\_1\),\!\(TI\`O\_2\),\[Ellipsis]] represents an operator that subsequently applies orthogonal transformations \!\(TI\`A\[Rule]O\_i\[Transpose]\[CenterDot]A\[CenterDot]O\_i\).";
UnitaryTransform::usage="UnitaryTransform[\!\(TI\`U\_1\),\!\(TI\`U\_2\),\[Ellipsis]] represents an operator that subsequently applies unitary transformations \!\(TI\`A\[Rule]U\_i\%\[Dagger]\[CenterDot]A\[CenterDot]U\_i\).";
ConjugateTransform::usage="ConjugateTransform[\!\(TI\`G\_1\),\!\(TI\`G\_2\),\[Ellipsis]] represents an operator that subsequently applies conjugate transformations \!\(TI\`A\[Rule]G\_i\%-1\[CenterDot]A\[CenterDot]G\_i\).";
Abstract::usage="Abstract[\!\(TI\`mat\)] gives the \[Sigma]-polynominal representation for the matrix \!\(TI\`mat\).";
Represent::usage="Represent[\!\(TI\`A\)] gives the matrix representation for the \[Sigma]-polynominal \!\(TI\`A\).";
ActionSpace::usage="ActionSpace[\!\(TI\`A\)] gives a list {\!\(TI\`D\_A\), \!\(TI\`basis\)}, with the matrix \!\(TI\`D\_A\) representing the action of \[Sigma]-polynominal \!\(TI\`A\) in the space spanned by \[Sigma]-monomial \!\(TI\`basis\).";
\[Sigma]Tr::usage="\[Sigma]Tr[\!\(TI\`A\)] gives the trace of the \[Sigma]-polynomial \!\(TI\`A\).";
nTr::usage="nTr[\!\(TI\`A\)] gives the normalized trace of the \[Sigma]-polynomial \!\(TI\`A\).";
\[Sigma]Det::usage="\[Sigma]Det[\!\(TI\`A\)] gives the determinant of the \[Sigma]-polynomial \!\(TI\`A\).";
\[Sigma]Inverse::usage="\[Sigma]Inverse[\!\(TI\`A\)] inverses the \[Sigma]-polynomial \!\(TI\`A\).";
\[Sigma]Power::usage="\[Sigma]Power[\!\(TI\`A\),\!\(TI\`x\)] gives the \[Sigma]-polynomial \!\(TI\`A\) to the power \!\(TI\`x\).";
\[Sigma]Sqrt::usage="\[Sigma]Sqrt[\!\(TI\`A\)] gives the square root of the \[Sigma]-polynomial \!\(TI\`A\)";
\[Sigma]Exp::usage="\[Sigma]Exp[\!\(TI\`A\)] gives the exponential of the \[Sigma]-polynomial \!\(TI\`A\)";
\[Sigma]Log::usage="\[Sigma]Log[\!\(TI\`A\)] gives the logarithm of the \[Sigma]-polynomial \!\(TI\`A\)";
Begin["`Private`"];

(* ===== Pauli Matrix ===== *)

(* ----- Legitimate Check ----- *)
(* [ Index Check (Abolished,2014-11-06) ] *)
(* Abolished:because checking indices are called too frequently,which affected the performace. *)

(* ----- Display ----- *)
Format[\[Sigma][a___],TraditionalForm]:=Superscript[\[Sigma],Row@{a}];

(* ----- Qubit ----- *)
(* Return log2 of matrix dimsion *)
Qubit[expr_]:=xQubit[expr];
(* HoldAll version for fast computation. *)
SetAttributes[xQubit,HoldAll];
xQubit[\[Sigma][a___]]:=Length[{a}];
xQubit[CircleTimes[x___]]:=Plus@@(xQubit/@Unevaluated[{x}]);
xQubit[Power[x_,_]]:=xQubit[x];
xQubit[Times[x__,y_]]:=xQubit[y];
xQubit[_[x_]]:=xQubit[x];
xQubit[_[x__]]:=Max[xQubit/@Unevaluated[{x}]];
xQubit[_]:=0;

(* ----- Tests ----- *)
(* [ No \[Sigma] Test ] *)
(* Return True if free from \[Sigma] *)
SetAttributes[\[Sigma]FreeQ,HoldAll];
\[Sigma]FreeQ[expr_]:=FreeQ[Unevaluated[expr],\[Sigma]];
(* [ \[Sigma]-Polynomial Test ] *)
(* Return True if input is a \[Sigma]-polynomial *)
SetAttributes[\[Sigma]PolynomialQ,HoldAll];
\[Sigma]PolynomialQ[_List|_SparseArray|_StructuredArray]:=False;
\[Sigma]PolynomialQ[expr_]:=Switch[xQubit[expr],0,!\[Sigma]FreeQ[expr],_Integer,True,_,False];

(* ----- Constructions ----- *)
(* [ Identity Matrix ] *)
(* Construct the idendity matrix of same dimension as input matrix *)
\[Sigma]0[n_Integer]:=\[Sigma]@@Array[0&,n];
\[Sigma]0[A_]:=\[Sigma]0[xQubit[A]];
(* [ Predefined Transformations ] *)
(* C4 rotation:exp(I \[Pi]/4 A) *)
C4[A_]:=(\[Sigma]0[A]+I A)/Sqrt[2];
(* [ Clifford Algebra ] *)
Cl[0,0]:={};
Cl[0,1]:={\[Sigma][2]};
Cl[0,2]:={\[Sigma][2,1],\[Sigma][2,3]};
Cl[0,3]:={\[Sigma][0,2,1],\[Sigma][0,2,3],\[Sigma][2,2,2]};
Cl[0,4]:={\[Sigma][0,2,1],\[Sigma][0,2,3],\[Sigma][2,1,0],\[Sigma][2,3,0]};
Cl[0,5]:={\[Sigma][0,2,1],\[Sigma][0,2,3],\[Sigma][2,1,0],\[Sigma][2,3,0],\[Sigma][2,2,2]};
Cl[0,6]:={\[Sigma][0,2,1],\[Sigma][0,2,3],\[Sigma][2,1,0],\[Sigma][2,3,0],\[Sigma][1,0,2],\[Sigma][3,0,2]};
Cl[0,7]:={\[Sigma][0,0,2,1],\[Sigma][0,0,2,3],\[Sigma][0,2,1,0],\[Sigma][0,2,3,0],\[Sigma][0,1,0,2],\[Sigma][0,3,0,2],\[Sigma][3,2,2,2]};
Cl[0,8]:={\[Sigma][0,0,2,1],\[Sigma][0,0,2,3],\[Sigma][0,2,1,0],\[Sigma][0,2,3,0],\[Sigma][2,1,0,0],\[Sigma][2,3,0,0],\[Sigma][1,0,0,2],\[Sigma][3,0,0,2]};
Cl[0,n_]/;n>8:=With[{\[Sigma]s=Cl[0,n-8]},Join[\[Sigma]0[\[Sigma]s]\[CircleTimes]#&/@Cl[0,8],#\[CircleTimes]\[Sigma][2,2,2,2]&/@\[Sigma]s]];
Cl[1,0]:={\[Sigma][3]};
Cl[n_,0]/;n>=2:=With[{\[Sigma]s=Cl[0,n-2]},Join[\[Sigma][2]\[CircleTimes]#&/@\[Sigma]s,{\[Sigma][1]\[CircleTimes]#,\[Sigma][3]\[CircleTimes]#}&@\[Sigma]0[\[Sigma]s]]];
Cl[p_,q_]/;p>0&&q>0:=With[{\[Sigma]s=Cl[p-1,q-1]},SortBy[Join[\[Sigma][3]\[CircleTimes]#&/@\[Sigma]s,{\[Sigma][1]\[CircleTimes]#,\[Sigma][2]\[CircleTimes]#}&@\[Sigma]0[\[Sigma]s]],OddQ@Count[#,2]&]];
Cl[n_]:=Block[{bit},bit=Ceiling[n/2];
Take[\[Sigma]@@PadRight[Append[ConstantArray[3,#1-1],#2],bit]&@@@Tuples[{Range[bit],{1,2}}],n]];
(* [ Lie Algebra ] *)
LieAlgebra[xs_]:=Block[{add},add[alg_,x_]/;MemberQ[alg,x]:=alg;
add[alg_,x_]:=Block[{res,new},res=Cases[Commutator[x,#]&/@alg,_\[Sigma],\[Infinity]];
new=Complement[res,alg];
Fold[add,Append[alg,x],new]];
Fold[add,{},xs]];

(* ----- Quantum Gates ----- *)
(* Hadamard gate *)
Hadamard[P_]:=Block[{n,ctr,A},n=Length[P];
ctr=Flatten@Position[P,_Blank];
CenterDot@@Table[Sum[\[Sigma]@@SparseArray[{i->a},n],{a,{1,3}}]/Sqrt[2],{i,ctr}]];
(* Swap gate *)
Swap[P_\[Sigma]]:=Block[{n,ctr,A},n=Length[P];
ctr=Flatten@Position[P,_Blank];
CenterDot@@Table[Sum[\[Sigma]@@SparseArray[#->a&/@ij,n],{a,0,3}]/2,{ij,Thread@Through[{Most,Rest}[ctr]]}]];
(* Controlled gate *)
Controlled[P_\[Sigma]]:=Block[{n,ctr,A},n=Length[P];
ctr=Flatten@Position[P,_Blank];
A=ReplacePart[P,#->0&/@ctr];
Factor[\[Sigma]0[n]+CenterDot@@Append[Table[(\[Sigma]0[n]-\[Sigma]@@SparseArray[{i->3},n])/2,{i,ctr}],A-\[Sigma]0[n]]]];

(* ===== Algebraic System ===== *)

(* ----- Tensor Product ----- *)
(* [ Definition ] *)
(* Tensor product Pauli matrices *)
CircleTimes[A_\[Sigma]]:=A;
CircleTimes[A___,\[Sigma][a___],\[Sigma][b___],B___]:=CircleTimes[A,\[Sigma][a,b],B];
(* \[Sigma]-free expressions are treated as number times \[Sigma][] *)
CircleTimes[A___,x_?\[Sigma]FreeQ,B___]:=x CircleTimes[A,B];
(* [ Algebraic Properties ] *)
(* Pull out factors *)
CircleTimes[A___,B_Times,C___]:=Times@@Lookup[#,False,{}]  CircleTimes[A,Times@@Lookup[#,True,{}],C]&@GroupBy[List@@B,\[Sigma]PolynomialQ];
CircleTimes[___,0,___]:=0;
(* Distribute over plus *)
me:CircleTimes[___,_Plus,___]:=Distribute[Unevaluated[me],Plus];

(* ----- Dot Product ----- *)
(* [ Definition ] *)
(* Definition of dot product. *)
CenterDot::xdim="The matrices `1` are incompatable in dimensions.";
CenterDot[A_\[Sigma]]:=A;
CenterDot[___,0,___]:=0;
me:CenterDot[As:_\[Sigma]..]:=Check[Times@@Flatten@Reap[IndexProduct@@@Thread[{As},\[Sigma]]],Message[CenterDot::xdim,{As}];HoldForm[me],Thread::tdlen];
(* Index product rule of Pauli algebra.(Do not assign the Flat attribute here,it will slow down the performance) *)
IndexProduct[0,i_]:=i;
IndexProduct[i_,0]:=i;
IndexProduct[i_,i_]:=0;
IndexProduct[1,2]:=(Sow[I];3);
IndexProduct[2,3]:=(Sow[I];1);
IndexProduct[3,1]:=(Sow[I];2);
IndexProduct[2,1]:=(Sow[-I];3);
IndexProduct[3,2]:=(Sow[-I];1);
IndexProduct[1,3]:=(Sow[-I];2);
IndexProduct[i_,j_,k__]:=IndexProduct[IndexProduct[i,j],k]
(* [ Algebraic Properties ] *)
(* Pull out factors *)
CenterDot[A___,B_Times,C___]:=Times@@Lookup[#,False,{}]  CenterDot[A,Times@@Lookup[#,True,{}],C]&@GroupBy[List@@B,\[Sigma]PolynomialQ];
(* Distribute over plus *)
me:CenterDot[___,_Plus,___]:=Distribute[Unevaluated[me],Plus];

(* ----- Commutation Relations ----- *)
(* [ Commutator and Anticommutator ] *)
Commutator[A_,B_]:=A\[CenterDot]B-B\[CenterDot]A;
Anticommutator[A_,B_]:=A\[CenterDot]B+B\[CenterDot]A;
(* [ CommuteQ and AnticommuteQ ] *)
CommuteQ[A_,B_]:=Commutator[A,B]===0;
AnticommuteQ[A_,B_]:=Anticommutator[A,B]===0;
CommuteQ[B_]:=CommuteQ[#,B]&;
AnticommuteQ[B_]:=AnticommuteQ[#,B]&;

(* ----- Symmetry Properties ----- *)
(* [ Conjugate and Transpose ] *)
(* Define \[Sigma] *)
\[Sigma]Transpose[A_]:=A/. \[Sigma][a___]:>(-1)^Count[{a},2] \[Sigma][a];
\[Sigma]Conjugate[A_+B_]:=\[Sigma]Conjugate[A]+\[Sigma]Conjugate[B];
\[Sigma]Hermitian[A_+B_]:=\[Sigma]Hermitian[A]+\[Sigma]Hermitian[B];
\[Sigma]Conjugate[A_ B_]:=\[Sigma]Conjugate[A] \[Sigma]Conjugate[B];
\[Sigma]Hermitian[A_ B_]:=\[Sigma]Hermitian[A] \[Sigma]Hermitian[B];
\[Sigma]Conjugate[A_]:=Conjugate[A]/. Conjugate[\[Sigma][a___]]:>(-1)^Count[{a},2] \[Sigma][a];
\[Sigma]Hermitian[A_]:=Conjugate[A]/. Conjugate[\[Sigma][a___]]:>\[Sigma][a];
(* [ SymmetricQ and AntisymmetricQ ] *)
(* fast algorithm for \[Sigma]-monomial *)
SymmetricQ[\[Sigma][a___]]:=EvenQ[Count[{a},2]];
AntisymmetricQ[\[Sigma][a___]]:=OddQ[Count[{a},2]];
(* general case *)
SymmetricQ[A_]:=Expand[A-\[Sigma]Transpose[A]]===0;
AntisymmetricQ[A_]:=Expand[A+\[Sigma]Transpose[A]]===0;

(* ----- \[Sigma]Select ----- *)
h:\[Sigma]Select[crits_List]:=With[{n=xQubit[crits]},If[n\[Element]Integers,\[Sigma]Select[Flatten@crits,n]]];
\[Sigma]Select[crits_List,n_Integer]:=\[Sigma]Select[Flatten@crits,\[Sigma]@@@Tuples[Range[0,3],n]];
\[Sigma]Select[crits_List,\[Sigma]s_List]:=Fold[Select[#1,#2]&,\[Sigma]s,Flatten@crits];
\[Sigma]Select[crit_,x___]:=\[Sigma]Select[{crit},x];

(* ----- Transformations ----- *)
(* {{Orthogonal:,A->O\[Transpose] A O},{Unitary:,A->SuperDagger[O] A O},{Conjugate:,A->O^-1 A O}} *)
OrthogonalTransform[Os___]:=Fold[Collect[\[Sigma]Transpose[#2]\[CenterDot]#1\[CenterDot]#2,_\[Sigma],Simplify]&,#,{Os}]&;
UnitaryTransform[Os___]:=Fold[Collect[\[Sigma]Hermitian[#2]\[CenterDot]#1\[CenterDot]#2,_\[Sigma],Simplify]&,#,{Os}]&;
ConjugateTransform[Os___]:=Fold[Collect[\[Sigma]Inverse[#2]\[CenterDot]#1\[CenterDot]#2,_\[Sigma],Simplify]&,#,{Os}]&;

(* ===== Representation System ===== *)

(* ----- Represent ----- *)
(* Give matrix represenation of \[Sigma]-polynomial *)
Represent[\[Sigma][]]=1;
Represent[\[Sigma][i_]]:=SparseArray[ArrayRules[PauliMatrix[i]]];
Represent[\[Sigma][a__]]:=KroneckerProduct@@(Represent[\[Sigma][#]]&/@{a});
Represent[expr_]:=expr/. s_\[Sigma]:>Represent[s];

(* ----- Abstract ----- *)
(* Give \[Sigma]-polynominal representation of matrix *)
Abstract[M_?MatrixQ]/;Function[#1==#2&&Log[2,#1]\[Element]Integers]@@Dimensions[M]:=xAbstract[M]
(* [ Kernel ] *)
xAbstract[M_]:=If[MatrixQ[M,PossibleZeroQ],0,Total[MapThread[CircleTimes[xAbstract[#1],#2]&,{Transpose[Map[bAbstract,Partition[M,{2,2}],{2}],{2,3,1}],(\[Sigma]/@Range[0,3])}]]];
xAbstract[{{x_}}]:=x \[Sigma][];
bAbstract[{{a_,b_},{c_,d_}}]:=Chop[{a+d,b+c,I (b-c),a-d}/2];

(* ===== Function System ===== *)

(* ----- ActionSpace ----- *)
(* Represent the action of Pauli polynominal in the Pauli matrix basis. *)
ActionSpace[A_?\[Sigma]PolynomialQ]:=Block[{Amat,\[Sigma]s,ord},{Amat,\[Sigma]s}=xActionSpace[A];
ord=Ordering[\[Sigma]s];
{Amat[[ord,ord]],\[Sigma]s[[ord]]}];
(* [ Kernel ] *)
xActionSpace[A_]:=Block[{\[Sigma]s,n,pos,Arule,i,Amat},\[Sigma]s=DeleteDuplicates@Cases[A,_\[Sigma],{0,Infinity}];
n=Length[\[Sigma]s];
pos[s_]:=If[Length[#]==0,AppendTo[\[Sigma]s,s];++n,First@First@#]&@Position[\[Sigma]s,s,{1},1];
Arule[p_]:=Block[{As},As=A\[CenterDot]\[Sigma]s[[p]];
Rule[{pos[#],p},Coefficient[As,#]]&/@Cases[As,_\[Sigma],{0,Infinity}]];
i=1;
Amat=SparseArray[Flatten[Last[Reap[While[i<=n&&i<=4^xQubit[A],Sow[Arule[i++]]]]]],{n,n}];
{Amat,\[Sigma]s}];

(* ----- \[Sigma]Tr ----- *)
\[Sigma]Tr[0]:=0;
\[Sigma]Tr[A:_?\[Sigma]PolynomialQ]:=2^xQubit[A] nTr[A];
nTr[A_]:=A/. {\[Sigma]0[A]->1,_\[Sigma]->0};

(* ----- \[Sigma]Det ----- *)
\[Sigma]Det[A:_?\[Sigma]PolynomialQ]:=xDet[A];
(* [ Kernel ] *)
xDet[A_]:=Block[{Amat,\[Sigma]s,n,M},{Amat,\[Sigma]s}=xActionSpace[A];
n=xQubit[A];
M=Length[\[Sigma]s];
Det[Amat]^(2^n/M)];

(* ----- \[Sigma]Inverse ----- *)
me:\[Sigma]Inverse[A:_?\[Sigma]PolynomialQ]:=Check[xInverse[A],Message[Inverse::sing,A];
HoldForm[me]];
(* [ Kernel ] *)
xInverse[A_]:=Block[{Amat,\[Sigma]s,sol,singular=False},{Amat,\[Sigma]s}=xActionSpace[A];
sol=Check[LinearSolve[Amat,nTr[\[Sigma]s]],singular=True];
If[!singular,invSimplify[sol . \[Sigma]s]]];
invSimplify[expr_]:=Collect[Numerator[#],_\[Sigma],Factor]/Simplify[Denominator[#]]&@aTogether[expr];
aTogether[expr_]:=expr//.{a_/d_+b_/d_:>(a+b)/d,a_/c_+b_/d_:>With[{lcm=PolynomialLCM[c,d]},(a Cancel[lcm/c]+b Cancel[lcm/d])/lcm]};

(* ----- \[Sigma]Power and \[Sigma]Sqrt ----- *)
me:\[Sigma]Power[A:_?\[Sigma]PolynomialQ,n_Integer]:=Check[Which[n==0,\[Sigma]0[A],n>0,nPower[A,n],n<0,nPower[\[Sigma]Inverse[A],Abs[n]]],HoldForm[me]];
me:\[Sigma]Power[A:_?\[Sigma]PolynomialQ,n_?NumericQ]:=Check[xPower[A,n],HoldForm[me]];
me:\[Sigma]Sqrt[A:Except[_List|_SparseArray|_StructuredArray]?\[Sigma]PolynomialQ]:=xPower[A,1/2];
(* [ Kernel (Integer Power) ] *)
(* Algorithm:divide and conquer *)
nPower[A_,n_]:=Block[{m,B},m=Floor[n/2];
B=nPower[A,m];
Collect[Switch[n-2 m,0,B\[CenterDot]B,1,B\[CenterDot]B\[CenterDot]A],_\[Sigma]]];
nPower[A_,0]:=\[Sigma]0[A];
nPower[A_,1]:=A;
nPower[A_,2]:=Collect[A\[CenterDot]A,_\[Sigma]];
(* [ Kernel (General Power) ] *)
xPower[A_,x_]:=Block[{Amat,\[Sigma]s},{Amat,\[Sigma]s}=xActionSpace[A];
MatrixPower[Amat,x,nTr[\[Sigma]s]] . \[Sigma]s]

(* ----- \[Sigma]Exp ----- *)
me:\[Sigma]Exp[A:_?\[Sigma]PolynomialQ]:=xExp[A];
(* [ Kernel ] *)
xExp[A_]:=Block[{Amat,\[Sigma]s},{Amat,\[Sigma]s}=xActionSpace[A];
MatrixExp[Amat,nTr[\[Sigma]s]] . \[Sigma]s]

(* ----- \[Sigma]Log ----- *)
me:\[Sigma]Log[A:_?\[Sigma]PolynomialQ]:=xLog[A];
(* [ Kernel ] *)
xLog[A_]:=Block[{Amat,\[Sigma]s},{Amat,\[Sigma]s}=xActionSpace[A];
\[Sigma]s . MatrixLog[Amat] . nTr[\[Sigma]s]]

(* ===== End ===== *)
End[];
EndPackage[];

