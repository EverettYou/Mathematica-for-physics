(* ::Package:: *)

(* Mathematica Package *)
(* Created by the Code Collector at Sun 11 Feb 2018 02:07:44 *)
(* ===== Fore Matter ===== *)

(* ----- Begin Package ----- *)
BeginPackage["DiagramEditor`"];
Diagram::usage="Diagram[___] represents a Feynman diagram.";
ToGraphics::usage="ToGraphics[_Diagram] converts a diagram to its graphics representation";
ImportDiagram::usage="ImportDiagram[_Diagram] imports the Feynman diagram to the diagram editor.";
ExportDiagram::usage="ExportDiagram[] exports the Feynman diagram from the diagram editor.";
DiagramEditor::usage="DiagramEditor[] launches the Feynman diagram editor.";
Begin["`Private`"];

(* ----- Global Parameters ----- *)
VertexRadius=0.05;
ArrowSize=0.05;
GridSpacing=0.05;
DefaultSelfLoopRadius=0.15;
FocusColor=ColorData["HTML"]["DarkGreen"];

(* ----- Global Variables ----- *)
(* Symbolic data of diagram. *)
LoopIDs={};(*list of loop indices*)LinkIDs={};(*list of link indices*)VertIDs={};(*list of vertex indices*)LastID=0;(*stack top index,which new index will follow*)PositionMap={};(*{id\[Rule]pos,...} graphics primitive position of object*)BoundaryMap={};(*{lid\[Rule]{vid,...},...} boundaries of line*)CoboundaryMap={};(*{vid\[Rule]{lid,...},...} coboundaries of vertex*)DirectionAngleMap={};(*{lid\[Rule]dag,...} direction angle of loop*)LoopRadiusMap={};(*{lid\[Rule]R,...} radius of loop*)TangentAngleMap={};(*{lid\[Rule]tan,...} tangent angle of link*)CoordinateMap={};(*{vid\[Rule]{x,y},...} coordinate of vertex*)StyleMap={};(*{id\[Rule]{sty,...},...} styles of object*)
(* Graphical data of diagram. *)
DynamicDiagram={};
DiagramPlotRange={{0,0},{0,0}};
(* System satus variables. *)
FocusedID=0;
MouseDownID=0;
MouseOverID=0;
MouseMovedID=0;
MouseUpID=0;
MouseDown=False;
MouseCoordinate={0,0};
MouseDownCoordinate={0,0};
MouseDownStatus="";
CanvasShift={0,0};

(* ===== Diagram ===== *)

(* ----- ImportDiagram ----- *)
(* Loading diagram data,initial processing and rendering. *)
ImportDiagram[Diagram[{$LoopIDs_List,$LinkIDs_List,$VertIDs_List},$BoundaryMap_List,$DirectionAngleMap_List,$LoopRadiusMap_List,$TangentAngleMap_List,$CoordinateMap_List,$StyleMap_List]]:=Module[{},(*data loading and Initialization*)LoopIDs=$LoopIDs;
LinkIDs=$LinkIDs;
VertIDs=$VertIDs;
LastID=Max[Join[LoopIDs,LinkIDs,VertIDs]];
PositionMap=GetPositionMap[];
BoundaryMap=$BoundaryMap;
CoboundaryMap=GetCoboundaryMap[];
DirectionAngleMap=$DirectionAngleMap;
LoopRadiusMap=$LoopRadiusMap;
TangentAngleMap=$TangentAngleMap;
CoordinateMap=$CoordinateMap;
StyleMap=$StyleMap;
(*dynamic diagram rendering*)FocusedID=0;
DynamicDiagram=MapThread[DynamicRender,{#,#/.StyleMap}]&@Join[LoopIDs,LinkIDs,VertIDs];
Canvas[DynamicDiagram];];

(* ----- Initialization Functions ----- *)
(* Obtain coboundary map from boundary map. *)
GetCoboundaryMap[]:=MapThread[Rule[#1,Flatten[#2]]&,{VertIDs,Last[Reap[Cases[BoundaryMap,HoldPattern[idloop_->idvert_]:>(Sow[idloop,#]&/@Flatten[{idvert}])],VertIDs]]}];
(* Update position map. *)
GetPositionMap[]:=MapIndexed[#1->First[#2]&,Join[LoopIDs,LinkIDs,VertIDs]];

(* ----- ExportDiagram ----- *)
(* Forming,optimizing and returning diagram object. *)
ExportDiagram[]:=Diagram[{LoopIDs,LinkIDs,VertIDs}/.PositionMap,BoundaryMap/.PositionMap,ReorderIndices[DirectionAngleMap],ReorderIndices[LoopRadiusMap],ReorderIndices[TangentAngleMap],ReorderIndices[CoordinateMap],ReorderIndices[StyleMap]];
SetAttributes[ReorderIndices,{HoldAll}];
ReorderIndices[map_]:=SortBy[MapThread[Rule,{map[[All,1]]/.PositionMap,map[[All,2]]}],First];

(* ===== Diagram Representation ===== *)

(* ----- Canned Rendering ----- *)
Format[diag_Diagram]:=Interpretation[Deploy[ToGraphics[diag]],diag];
ToGraphics[diag:Diagram[{$LoopIDs_List,$LinkIDs_List,$VertIDs_List},$BoundaryMap_List,$DirectionAngleMap_List,$LoopRadiusMap_List,$TangentAngleMap_List,$CoordinateMap_List,$StyleMap_List]]:=Module[{$CoboundaryMap,$ReferenceAngleMap,$StaticDiagram,$DiagramPlotRange},$CoboundaryMap=MapThread[Rule[#1,Flatten[#2]]&,{$VertIDs,Last[Reap[Cases[$BoundaryMap,HoldPattern[idloop_->idvert_]:>(Sow[idloop,#]&/@Flatten[{idvert}])],$VertIDs]]}];
$ReferenceAngleMap=Function[{lid},Module[{vid,aids,rvec},vid=lid/.$BoundaryMap;
aids=DeleteCases[Flatten[vid/.$CoboundaryMap/.$BoundaryMap],vid];
lid->If[aids=={},\[Pi]/2.,rvec=Total[Function[{p},((vid/.$CoordinateMap)-p)]/@(aids/.$CoordinateMap)];
If[Norm[rvec]==0.,\[Pi]/2.,ArcTan@@rvec]]]]/@$LoopIDs;
$StaticDiagram=Join[MapThread[lRender,{$LoopIDs/.$StyleMap,MapThread[xGetLoop,{$LoopIDs/.$BoundaryMap/.$CoordinateMap,($LoopIDs/.$DirectionAngleMap)+($LoopIDs/.$ReferenceAngleMap),$LoopIDs/.$LoopRadiusMap}]}],MapThread[lRender,{$LinkIDs/.$StyleMap,MapThread[xGetLink,Append[Thread[$LinkIDs/.$BoundaryMap/.$CoordinateMap],$LinkIDs/.$TangentAngleMap]]}],MapThread[xRender,{$VertIDs/.$StyleMap,List/@$VertIDs/.$CoordinateMap}]];
$DiagramPlotRange=If[Length@$CoordinateMap[[All,2]]==0,{{0,0},{0,0}},VertexRadius {{-1,1},{-1,1}}+Identity@@@IntervalUnion@@@Thread@Append[Cases[$StaticDiagram,x_?RegionQ:>Interval/@RegionBounds[x],\[Infinity]],Interval/@RegionBounds[Point[$CoordinateMap[[All,2]]]]]];
Graphics[$StaticDiagram,AxesOrigin->{0,0},BaselinePosition->(Axis->Center),PlotRange->$DiagramPlotRange,PlotRangePadding->None,ImagePadding->None,ImageSize->-60 Subtract@@@$DiagramPlotRange]];

(* ----- Get Angles ----- *)
(* Get reference angle of a vertex,given the vertex ID. *)
GetReferenceAngle[vid_Integer]:=Module[{aids,rvec},aids=DeleteCases[Flatten[vid/.CoboundaryMap/.BoundaryMap],vid];
If[aids=={},\[Pi]/2.,rvec=Total[Function[{p},((vid/.CoordinateMap)-p)]/@(aids/.CoordinateMap)];
If[Norm[rvec]==0.,\[Pi]/2.,ArcTan@@rvec]]];
GetReferenceAngle[vids:{_Integer..}]:=GetReferenceAngle/@vids;
(* Return the optimal tangent angle closest to \[Theta]0 from a vertex.Given the vertex ID. *)
OptimalTangentAngle[vid_Integer,p2:{_,_}]:=Module[{atan,p1,\[Theta]0,lid,loops,links,\[Theta]s1={},vps,\[Theta]s2={},\[Theta]s,\[Theta]r,ta0,f,val,min},atan[{}]=0;
atan[r:{_,_}]:=If[Norm[r]==0,0,ArcTan@@r];
p1=vid/.CoordinateMap;
\[Theta]0=atan[p2-p1];
lid=vid/.CoboundaryMap;
{loops,links}=Flatten/@Last[Reap[If[MemberQ[LoopIDs,#],Sow[#,"Loop"],Sow[#,"Link"]]&/@lid,{"Loop","Link"}]];
If[Length[loops]!=0,\[Theta]s1=Flatten[{#+\[Pi]/2,#-\[Pi]/2}&/@((loops/.DirectionAngleMap)+GetReferenceAngle[loops/.BoundaryMap])]];
If[Length[links]!=0,vps=links/.BoundaryMap;
\[Theta]s2=atan/@(-Subtract@@@(vps/.CoordinateMap))+(links/.TangentAngleMap)+Replace[vps,{{vid,_}->0,{_,vid}->\[Pi]},{1}]];
\[Theta]s=Mod[Join[\[Theta]s1,\[Theta]s2],2 \[Pi]];
\[Theta]r=atan[Mean[VertIDs/.CoordinateMap]-p1];
min=Length[\[Theta]s];
f=(2 Cos[#]^3+Cos[2 #])/3&;
Do[val=Sum[f[ta+\[Theta]0-\[Theta]],{\[Theta],\[Theta]s}]-0.02 Cos[ta]+0.001 Cos[ta+\[Theta]0-\[Theta]r];
If[val<min,min=val;ta0=ta;],{ta,-\[Pi],\[Pi],\[Pi]/12}];
ta0];
OptimalTangentAngle[fromID_Integer,toID_Integer]:=OptimalTangentAngle[fromID,toID/.CoordinateMap];

(* ----- Get Curve for Loops and Links ----- *)
GetLoop[id_Integer]:=Module[{p,\[Alpha],R},p=id/.BoundaryMap/.CoordinateMap;
\[Alpha]=(id/.DirectionAngleMap)+GetReferenceAngle[id/.BoundaryMap];
R=id/.LoopRadiusMap;
xGetLoop[p,\[Alpha],R]];
xGetLoop[p_,\[Alpha]_,R_]:={2 \[Pi] R,Evaluate[p+{R Cos[\[Alpha]]-(R+#2) Cos[\[Alpha]-#1/R],R Sin[\[Alpha]]-(R+#2) Sin[\[Alpha]-#1/R]}]&};
GetLink[id_Integer]:=Module[{p1,p2},{p1,p2}=id/.BoundaryMap/.CoordinateMap;
xGetLink[p1,p2,id/.TangentAngleMap]];
xGetLink[p1:{_,_},p2:{_,_},\[Alpha]_]:=Module[{l,t,n,o,r},l=Norm[p2-p1];
If[l==0.,{1,Evaluate[p1+0 #1+0 #2]&},t=Normalize[p2-p1];
n={{0,-1},{1,0}}.t;
If[Sin[\[Alpha]]==0.,(*straight line*){l,Evaluate[p1+t #1+n #2]&},(*chord*)o=(p1+p2)/2-n Norm[p2-p1] Cot[\[Alpha]]/2;
r=l/2/Sin[\[Alpha]];
{2 \[Alpha] r,Evaluate[o+(r+#2) (t Sin[#1/r-\[Alpha]]+n Cos[#1/r-\[Alpha]])]&}]]];

(* ----- Default Options ----- *)
$DefaultOptions={"None"->{},"Dot"->{"Size"->1},"Circle"->{"Size"->1},"Cross"->{"Size"->1},"Plus"->{"Size"->1},"Square"->{"Size"->1},"Diamond"->{"Size"->1},"CircleDot"->{"Size"->1},"CircleCross"->{"Size"->1},"CirclePlus"->{"Size"->1},"Line"->{"Style"->{}},"DoubleLine"->{"Spacing"->0.4,"Style"->{}},"Wave"->{"Amplitude"->0.5,"Style"->{}},"DoubleWave"->{"Amplitude"->0.5,"Spacing"->0.4,"Style"->{}},"Spiral"->{"Amplitude"->0.5,"CoilSize"->1,"Style"->{}},"DoubleSpiral"->{"Amplitude"->0.5,"CoilSize"->1,"Spacing"->0.4,"Style"->{}},"Zigzag"->{"Amplitude"->0.5,"Style"->{}},"DoubleZigzag"->{"Amplitude"->0.5,"Spacing"->0.4,"Style"->{}},"Arrow"->{"Position"->0.5,"Direction"->1},"DoubleArrow"->{"Position"->0.2,"Direction"->1},"InnerArrow"->{"Position"->0.2},"OuterArrow"->{"Position"->0.2}};
OptVal[funname_,ops_,optname_]:=OptionValue[Join[ops,funname/.$DefaultOptions],optname]

(* ----- Rendering Functions ----- *)
Render[id_,sty_]:=Which[MemberQ[VertIDs,id],vRender[id,sty],MemberQ[LoopIDs,id],oRender[id,sty],MemberQ[LinkIDs,id],cRender[id,sty],True,{}];
(* vRender-Vertex rendering funtion. *)
vRender[id_,sty_]:=xRender[sty,{id/.CoordinateMap}];
(* xRender-general point redering function *)
xRender[sty_List,info_]:=xRender[#,info]&/@sty;
xRender[sty_,info_]:=sty;
xRender[x_Function,info_]:=x@@info;
xRender[{"None",___},_]:={};
xRender[{"Dot",opt_: {}},{p_}]:={Disk[p,OptVal["Dot",opt,"Size"] VertexRadius]};
xRender[{"Circle",opt_: {}},{p_}]:={{White,Disk[##]},Circle[##]}&[p,OptVal["Circle",opt,"Size"] VertexRadius];
xRender[{"Cross",opt_: {}},{p_}]:={Line[{{#1-{#2,#2},#1+{#2,#2}},{#1+{-#2,#2},#1+{#2,-#2}}}]}&[p,OptVal["Cross",opt,"Size"] VertexRadius/Sqrt[2]];
xRender[{"Plus",opt_: {}},{p_}]:={Line[{{#1-{0,#2},#1+{0,#2}},{#1-{#2,0},#1+{#2,0}}}]}&[p,OptVal["Plus",opt,"Size"] VertexRadius];
xRender[{"Square",opt_: {}},{p_}]:={{White,Rectangle[#1-{#2,#2},#1+{#2,#2}]},Line[{#1+{#2,#2},#1+{-#2,#2},#1+{-#2,-#2},#1+{#2,-#2},#1+{#2,#2}}]}&[p,OptVal["Square",opt,"Size"] VertexRadius/Sqrt[2]];
xRender[{"Diamond",opt_: {}},{p_}]:={{White,Polygon[{#1+{#2,0},#1+{0,#2},#1+{-#2,0},#1+{0,-#2}}]},Line[{#1+{#2,0},#1+{0,#2},#1+{-#2,0},#1+{0,-#2},#1+{#2,0}}]}&[p,OptVal["Diamond",opt,"Size"] VertexRadius];
xRender[{"CircleDot",opt_: {}},{p_}]:={{White,Disk[##]},Circle[##],Disk[#1,#2/2]}&[p,OptVal["CircleDot",opt,"Size"] VertexRadius];
xRender[{"CircleCross",opt_: {}},{p_}]:={{White,Disk[##]},Circle[##],Line[{{#1-0.6 {#2,#2},#1+0.6 {#2,#2}},{#1+0.6 {-#2,#2},#1+0.6 {#2,-#2}}}]}&[p,OptVal["CircleCross",opt,"Size"] VertexRadius];
xRender[{"CirclePlus",opt_: {}},{p_}]:={{White,Disk[##]},Circle[##],Line[{{#1-0.9 {0,#2},#1+0.9 {0,#2}},{#1-0.9 {#2,0},#1+0.9 {#2,0}}}]}&[p,OptVal["CirclePlus",opt,"Size"] VertexRadius];
$VertexStyles={"None","Dot","Circle","Cross","Plus","Square","Diamond","CircleDot","CircleCross","CirclePlus"};
(* oRender-Loop rendering function.cRender-Link redering function. *)
oRender[id_,sty_]:=lRender[sty,GetLoop[id]];
cRender[id_,sty_]:=lRender[sty,GetLink[id]];
(* lRender-general curve redering function. *)
lRender[sty_List,info_]:=lRender[#,info]&/@sty;
lRender[sty_,info_]:=sty;
lRender[x_Function,info_]:=x@@info;
lRender[{"None",___},_]:={};
lRender[{"Line",opt_: {}},{L_,Curve_}]:=Join[OptVal["Line",opt,"Style"],{Line[Table[Curve[x,0],{x,0,L,L/32}]]}];
lRender[{"DoubleLine",opt_: {}},{L_,Curve_}]:=Join[OptVal["DoubleLine",opt,"Style"],{Line[Table[Curve[x,y],{y,OptVal["DoubleLine",opt,"Spacing"] {-ArrowSize,ArrowSize}},{x,0,L,L/32}]]}];
lRender[{"DashedLine",opt_: {}},info_]:=lRender[{"Line",Prepend[opt,"Style"->{AbsoluteDashing[{1,3,1,0}]}]},info];
lRender[{"DashedDoubleLine",opt_: {}},info_]:=lRender[{"DoubleLine",Prepend[opt,"Style"->{AbsoluteDashing[{1,3,1,0}]}]},info];
lRender[{"DottedLine",opt_: {}},info_]:=lRender[{"Line",Prepend[opt,"Style"->{AbsoluteDashing[{0.1,4,0.1,0}]}]},info];
lRender[{"DottedDoubleLine",opt_: {}},info_]:=lRender[{"DoubleLine",Prepend[opt,"Style"->{AbsoluteDashing[{0.1,4,0.1,0}]}]},info];
lRender[{"Wave",opt_: {}},{L_,Curve_}]:=With[{\[Lambda]=L/Round[L/(2 ArrowSize)]},Join[OptVal["Wave",opt,"Style"],{Line[Table[Curve[x,OptVal["Wave",opt,"Amplitude"] ArrowSize Sin[(2 \[Pi]) x/\[Lambda]]],{x,0,L,\[Lambda]/16}]]}]];
lRender[{"DoubleWave",opt_: {}},{L_,Curve_}]:=With[{\[Lambda]=L/Round[L/(2 ArrowSize)]},Join[OptVal["DoubleWave",opt,"Style"],{Line[Table[Curve[x,OptVal["DoubleWave",opt,"Amplitude"] ArrowSize Sin[(2 \[Pi]) x/\[Lambda]]+y],{y,OptVal["DoubleWave",opt,"Spacing"] {-ArrowSize,ArrowSize}},{x,0,L,\[Lambda]/16}]]}]];
lRender[{"Spiral",opt_: {}},{L_,Curve_}]:=With[{\[Lambda]=L/Round[L/(2 ArrowSize)]},Join[OptVal["Spiral",opt,"Style"],{Line[Table[Curve[x+OptVal["Spiral",opt,"CoilSize"] ArrowSize Sin[(2 \[Pi]) x/\[Lambda]],OptVal["Spiral",opt,"Amplitude"] ArrowSize (1-Cos[(2 \[Pi]) x/\[Lambda]])],{x,0,L,\[Lambda]/16}]]}]];
lRender[{"DoubleSpiral",opt_: {}},{L_,Curve_}]:=With[{\[Lambda]=L/Round[L/(2 ArrowSize)]},Join[OptVal["DoubleSpiral",opt,"Style"],{Line[Table[Curve[x+OptVal["DoubleSpiral",opt,"CoilSize"] ArrowSize Sin[(2 \[Pi]) x/\[Lambda]+\[Pi] Sign[y]/2],OptVal["DoubleSpiral",opt,"Amplitude"] ArrowSize Sign[y] (1-Cos[(2 \[Pi]) x/\[Lambda]+\[Pi] Sign[y]/2])+0.5 y],{y,OptVal["DoubleSpiral",opt,"Spacing"] {-ArrowSize,ArrowSize}},{x,0,L,\[Lambda]/16}]]}]];
lRender[{"Zigzag",opt_: {}},{L_,Curve_}]:=With[{\[Lambda]=L/Round[L/(2 ArrowSize)]},Join[OptVal["Zigzag",opt,"Style"],{Line[Table[Curve[x,OptVal["Zigzag",opt,"Amplitude"] ArrowSize TriangleWave[x/\[Lambda]+1/4]],{x,0,L,\[Lambda]/2}]]}]];
lRender[{"DoubleZigzag",opt_: {}},{L_,Curve_}]:=With[{\[Lambda]=L/Round[L/(2 ArrowSize)]},Join[OptVal["DoubleZigzag",opt,"Style"],{Line[Table[Curve[x,OptVal["DoubleZigzag",opt,"Amplitude"] ArrowSize TriangleWave[x/\[Lambda]]+y],{y,OptVal["DoubleZigzag",opt,"Spacing"] {-ArrowSize,ArrowSize}},{x,0,L,\[Lambda]/16}]]}]];
lRender[{"Arrow",opt_: {}},{L_,Curve_}]:=With[{TransFun=Composition[TranslationTransform[Curve[OptVal["Arrow",opt,"Position"] L,0]],RotationTransform[{{OptVal["Arrow",opt,"Direction"],0},If[Norm[#]==0.,{1,0},Normalize[#]]&@Derivative[1,0][Curve][OptVal["Arrow",opt,"Position"] L,0]}],ScalingTransform[{ArrowSize,ArrowSize}]]},{Line[TransFun/@{{-1,1},{1,0},{-1,-1}}]}];
lRender[{"DoubleArrow",opt_: {}},info_]:={lRender[{"Arrow",{"Position"->OptVal["DoubleArrow",opt,"Position"],"Direction"->OptVal["DoubleArrow",opt,"Direction"]}},info],lRender[{"Arrow",{"Position"->1-OptVal["DoubleArrow",opt,"Position"],"Direction"->OptVal["DoubleArrow",opt,"Direction"]}},info]};
lRender[{"InnerArrow",opt_: {}},info_]:={lRender[{"Arrow",{"Position"->OptVal["InnerArrow",opt,"Position"],"Direction"->1}},info],lRender[{"Arrow",{"Position"->1-OptVal["InnerArrow",opt,"Position"],"Direction"->-1}},info]};
lRender[{"OuterArrow",opt_: {}},info_]:={lRender[{"Arrow",{"Position"->OptVal["OuterArrow",opt,"Position"],"Direction"->-1}},info],lRender[{"Arrow",{"Position"->1-OptVal["OuterArrow",opt,"Position"],"Direction"->1}},info]}
$LineStyles={"Line","DoubleLine","DashedLine","DashedDoubleLine","DottedLine","DottedDoubleLine","Wave","DoubleWave","Spiral","DoubleSpiral","Zigzag","DoubleZigzag"};
$ArrowStyles={"None","Arrow","DoubleArrow","InnerArrow","OuterArrow"};

(* ===== Diagram Editor ===== *)

(* ----- Board ----- *)
Board={{AbsoluteThickness[1],{LightGray,Line[Table[{{l,-1},{l,1}},{l,-1,1,4 GridSpacing}]],Line[Table[{{-1,l},{1,l}},{l,-1,1,4 GridSpacing}]]},{Gray,Line[GridSpacing {{{-1,0},{1,0}},{{0,-1},{0,1}}}]}},{Opacity[0],Rectangle[{-1,-1},{1,1}]}};

(* ----- Canvas ----- *)
Canvas[diag_]:=Module[{pts},pts=Join[Flatten[Cases[diag,Line[pts_]:>Cases[pts,{_?NumericQ,_?NumericQ},Infinity],Infinity],1],CoordinateMap[[All,2]]];
If[Length[pts]==0,DiagramPlotRange={{0,0},{0,0}};
{diag},DiagramPlotRange=VertexRadius {{-1.5,1.5},{-1.5,1.5}}+(Through[{Min,Max}[#]]&/@Thread[pts]);
{{Opacity[0.1],Rectangle@@Transpose[DiagramPlotRange]},diag}]];
InCanvas[p_]:=Module[{x,y,xmin,ymin,xmax,ymax},{x,y}=p;
{{xmin,xmax},{ymin,ymax}}=DiagramPlotRange;
(xmin<x<xmax&&ymin<y<ymax)];

(* ----- Property Panel ----- *)
PropertyPanel[id_Integer]:=Style[Column[Which[id==0,(*canvas*){Row[{NumberForm[Length[VertIDs],2,NumberPadding->{" ","0"}]," vertices, ",NumberForm[Length[Join[LoopIDs,LinkIDs]],2,NumberPadding->{" ","0"}]," lines.",Spacer[40],Button[Style["clear",Underlined,FontFamily->"Helvetica"],ClearDiagram[],Appearance->None]},ImageSize->{180,15}]},MemberQ[VertIDs,id],(*vertex*){DynamicModule[{sty},sty=id/.StyleMap;
Row[{StyleMenu[Graphics[xRender[{#},{{0,0}}],PlotRange->1.4 VertexRadius,ImageSize->10]&,"vertex",$VertexStyles,id,sty,1],Spacer[101],ButtonRemove[id]},ImageSize->{180,15}]]},MemberQ[Join[LoopIDs,LinkIDs],id],(*loop*){DynamicModule[{sty},sty=id/.StyleMap;
Row[{StyleMenu[Graphics[lRender[{#},{0.3,{#1,#2}&}],PlotRange->{{0,0.3},1.4 {-ArrowSize,ArrowSize}},ImageSize->24]&,"line",$LineStyles,id,sty,1]," + ",StyleMenu[Graphics[lRender[{{GrayLevel[0.7],{"Line"}},{#}},{0.5,{#1,#2}&}],PlotRange->{{0,0.5},1.5 {-ArrowSize,ArrowSize}},ImageSize->24]&,"arrow",$ArrowStyles,id,sty,2],Spacer[69],ButtonRemove[id]},ImageSize->{180,15}]]},True,{}]],FontFamily->"Helvetica"];
SetAttributes[StyleMenu,HoldAll];
StyleMenu[RenderFunction_,name_,stys_,id_,sty_,loc_]:=ActionMenu[Style[name,Underlined,FontFamily->"Helvetica"],(RenderFunction[#]:>(sty[[loc]]={#};
UpdateTo[StyleMap,id->sty];UpdateDynamicDiagram[id];))&/@stys,ContentPadding->False,FrameMargins->0,Appearance->None];
ButtonRemove[id_]:=Button[Style["remove",Underlined,FontFamily->"Helvetica"],RemoveObject[id],Appearance->None];

(* ----- Interface ----- *)
DiagramEditor[]:=CreateWindow[PaletteNotebook[Deploy[Framed[Column[{Row[{Button[Style["import",Underlined,FontFamily->"Helvetica"],If[Length[#]!=0,ImportDiagram[First[#]]]&@Cases[NotebookRead[InputNotebook[]],_Diagram,Infinity],Appearance->None],Spacer[40],Button[Style["export",Underlined,FontFamily->"Helvetica"],NotebookWrite[InputNotebook[],ToBoxes[ExportDiagram[]]],Appearance->None]}],EventHandler[(*diagram dynamics*)Dynamic[Graphics[{Board,Translate[Canvas[DynamicDiagram],CanvasShift]},PlotRange->1.05,ImageSize->{189,189}],TrackedSymbols:>{CanvasShift,DynamicDiagram}],{"MouseDown":>(MouseDown=True;
(*transfer focus*)If[FocusedID!=MouseDownID,TransferFocus[FocusedID,MouseDownID]];
(*keep mouse down coordinate*)MouseDownCoordinate=MouseCoordinate;
(*record mouse down status*)If[CurrentValue["ShiftKey"],If[MouseDownID==0,MouseDownStatus="AddVertex"];
If[MemberQ[VertIDs,MouseDownID],BeginTrialLink[MouseDownID];
MouseDownStatus="Linking"],If[MouseDownID!=0,MouseDownStatus="ObjectDragging",If[InCanvas[MouseDownCoordinate],MouseDownStatus="CanvasDragging"]]];
(*clear mouse up ID*)MouseUpID=0;),"MouseMoved":>((*determine mouse over ID*)MouseOverID=If[MouseMovedID!=0,MouseMovedID,0];
MouseMovedID=0;
(*actions*)If[MouseDown,Switch[MouseDownStatus,"CanvasDragging",CanvasShift=MouseCoordinate-MouseDownCoordinate,"ObjectDragging",DragObject[FocusedID,MouseCoordinate],"Linking",UpdateTrialLink[MouseDownID,MouseOverID],_,Null]];),"MouseUp":>(MouseDown=False;
(*if canvas moved,put it down*)Switch[MouseDownStatus,"CanvasDragging",PutCanvas[CanvasShift];
CanvasShift={0,0},"AddVertex",AddVertex[MouseCoordinate];,"Linking",EndTrialLink[MouseDownID,MouseOverID],_,Null];
MouseDownStatus="";
(*clear mouse down ID*)MouseDownID=0;)}],(*system dynamics*)Dynamic[(*track mouse coordinate*)MouseCoordinate=MousePosition[{"Graphics",Graphics},MouseCoordinate];
PropertyPanel[FocusedID]]},Center],RoundingRadius->8]],Saveable->False,Editable->False,WindowMargins->With[{ws=OptionValue[AbsoluteOptions[InputNotebook[]],WindowSize],wm=OptionValue[AbsoluteOptions[InputNotebook[]],WindowMargins]},{{wm[[1,1]]+ws[[1]]+5,Automatic},{Automatic,wm[[2,2]]+5}}]],Background->White,WindowTitle->"Diagram Editor"];

(* ----- Map Operations ----- *)
SetAttributes[UpdateTo,HoldFirst];
UpdateTo[map_,rules_]:=(map=Replace[map,rules/.{Rule[a_,b_]:>RuleDelayed[a->_,a->b]},{1}]);
SetAttributes[RemoveFrom,HoldFirst];
RemoveFrom[map_,id_Integer]:=(map=DeleteCases[map,Rule[id,_]]);
RemoveFrom[map_,ids:{_Integer..}]:=(map=DeleteCases[map,Alternatives@@(Rule[#,_]&/@ids)]);
RemoveFrom[map_,{}]:=Null;

(* ----- DynamicRender ----- *)
DynamicRender[id_,{{"None"}}]:=EventHandler[If[id==FocusedID,{Opacity[0.5],FocusColor,Disk[id/.CoordinateMap,VertexRadius]},Mouseover[{Opacity[0],Disk[id/.CoordinateMap,2 VertexRadius]},{Opacity[0.3],Disk[id/.CoordinateMap,2 VertexRadius]}]],{"MouseDown":>(MouseDownID=id;),"MouseMoved":>(MouseMovedID=id;),"MouseUp":>(MouseUpID=id;)}];
DynamicRender[id_,sty_]:=With[{g=Render[id,sty]},EventHandler[If[id==FocusedID,{FocusColor,g},g],{"MouseDown":>(MouseDownID=id;),"MouseMoved":>(MouseMovedID=id;),"MouseUp":>(MouseUpID=id;)}]];

(* ----- Update Dynamic Diagram ----- *)
UpdateDynamicDiagram[id_Integer]:=(DynamicDiagram[[id/.PositionMap]]=DynamicRender[id,id/.StyleMap]);
UpdateDynamicDiagram[ids:{_Integer..}]:=(DynamicDiagram[[ids/.PositionMap]]=MapThread[DynamicRender,{ids,ids/.StyleMap}]);

(* ----- Transfer Focus ----- *)
TransferFocus[0,newID_]:=(FocusedID=newID;
UpdateDynamicDiagram[newID]);
TransferFocus[oldID_,0]:=(FocusedID=0;
UpdateDynamicDiagram[oldID]);
TransferFocus[oldID_,newID_]:=(FocusedID=newID;
UpdateDynamicDiagram[{oldID,newID}]);

(* ----- Drag Object ----- *)
DragObject[id_,p0_]:=Which[MemberQ[VertIDs,id],(*vertex dragging*)Module[{bIDs,lIDs},UpdateTo[CoordinateMap,{id->Round[p0,GridSpacing]}];
bIDs=id/.CoboundaryMap;
lIDs=Intersection[Flatten[bIDs/.BoundaryMap/.CoboundaryMap],LoopIDs];
UpdateDynamicDiagram[Join[{id},bIDs,lIDs]];];,MemberQ[LoopIDs,id],(*loop dragging*)Module[{vid,pvec,ar},vid=id/.BoundaryMap;
pvec=p0-(vid/.CoordinateMap);
ar=GetReferenceAngle[vid/.BoundaryMap];
UpdateTo[DirectionAngleMap,{id->Round[Mod[(ArcTan@@pvec)-ar,2 \[Pi],-\[Pi]]/\[Pi],1/12] \[Pi]}];
UpdateTo[LoopRadiusMap,{id->Max[Round[Norm[pvec],GridSpacing]/2,GridSpacing]}];
UpdateDynamicDiagram[id];];,MemberQ[LinkIDs,id],(*loop dragging*)Module[{p1,p2,ta},{p1,p2}=id/.BoundaryMap/.CoordinateMap;
ta=(Round[#/\[Pi],1/12] \[Pi])&@If[Norm[p2-p0]^2+Norm[p0-p1]^2<=Norm[p1-p2]^2,ArcSin[Det[{p2-p0,p0-p1}]/Norm[p2-p0]/Norm[p0-p1]],(-#+Sign[#] \[Pi])&@ArcSin[Det[{p2-p0,p0-p1}]/Norm[p2-p0]/Norm[p0-p1]]];
UpdateTo[TangentAngleMap,{id->ta}];
UpdateDynamicDiagram[id];];,True,Null;];

(* ----- Put Canvas ----- *)
PutCanvas[shift_]:=(CoordinateMap[[All,2]]=(#+Round[shift,GridSpacing])&/@CoordinateMap[[All,2]];
DynamicDiagram=MapThread[DynamicRender,{#,#/.StyleMap}]&@Join[LoopIDs,LinkIDs,VertIDs]);

(* ----- Add Vertex ----- *)
AddVertex[p_]:=Module[{oldID},LastID++;
AppendTo[VertIDs,LastID];
AppendTo[CoordinateMap,LastID->Round[p,GridSpacing]];
AppendTo[CoboundaryMap,LastID->{}];
AppendTo[StyleMap,LastID->{{"None"}}];
AppendTo[PositionMap,LastID->Length[PositionMap]+1];
oldID=FocusedID;FocusedID=LastID;
If[oldID!=0,UpdateDynamicDiagram[oldID]];
AppendTo[DynamicDiagram,DynamicRender[LastID,{{"None"}}]];
PositionMap=GetPositionMap[];];

(* ----- Trial Link ----- *)
BeginTrialLink[fromID_]:=Module[{},LastID++;
AppendTo[BoundaryMap,LastID->fromID];
AppendTo[DirectionAngleMap,LastID->0];
AppendTo[LoopRadiusMap,LastID->DefaultSelfLoopRadius];
AppendTo[TangentAngleMap,LastID->0];
AppendTo[StyleMap,LastID->{{"Line"},{"None"}}];
PrependTo[DynamicDiagram,{}]];
UpdateTrialLink[fromID_,toID_]:=If[MemberQ[VertIDs,toID],If[fromID==toID,UpdateTo[BoundaryMap,LastID->fromID];
DynamicDiagram[[1]]=oRender[LastID,{{"Line"},{"None"}}],UpdateTo[BoundaryMap,LastID->{fromID,toID}];
UpdateTo[TangentAngleMap,LastID->OptimalTangentAngle[fromID,toID]];
DynamicDiagram[[1]]=cRender[LastID,{{"Line"},{"None"}}]],UpdateTo[BoundaryMap,LastID->{fromID,toID}];
UpdateTo[TangentAngleMap,LastID->OptimalTangentAngle[fromID,MouseCoordinate]];
DynamicDiagram[[1]]={Opacity[0.3],lRender[{{"Line"},{"None"}},xGetLink[fromID/.CoordinateMap,MouseCoordinate,LastID/.TangentAngleMap]]}]
EndTrialLink[fromID_,toID_]:=Module[{pos},DynamicDiagram=Rest[DynamicDiagram];
If[MemberQ[VertIDs,toID],If[fromID==toID,AppendTo[LoopIDs,LastID];
UpdateTo[CoboundaryMap,fromID->Append[fromID/.CoboundaryMap,LastID]];
RemoveFrom[TangentAngleMap,LastID];
pos=Length[LoopIDs];
DynamicDiagram=Insert[DynamicDiagram,DynamicRender[LastID,LastID/.StyleMap],pos];
PositionMap=GetPositionMap[];,AppendTo[LinkIDs,LastID];
UpdateTo[CoboundaryMap,{fromID->Append[fromID/.CoboundaryMap,LastID],toID->Append[toID/.CoboundaryMap,LastID]}];
RemoveFrom[DirectionAngleMap,LastID];
RemoveFrom[LoopRadiusMap,LastID];
pos=Length[Join[LoopIDs,LinkIDs]];
DynamicDiagram=Insert[DynamicDiagram,DynamicRender[LastID,LastID/.StyleMap],pos];
PositionMap=GetPositionMap[];],RemoveFrom[BoundaryMap,LastID];
RemoveFrom[DirectionAngleMap,LastID];
RemoveFrom[LoopRadiusMap,LastID];
RemoveFrom[TangentAngleMap,LastID];
RemoveFrom[StyleMap,LastID];]];

(* ----- Remove Object ----- *)
RemoveObject[id_]:=(xRemoveObject[id];FocusedID=0;);
xRemoveObject[id_]:=(Which[MemberQ[VertIDs,id],xRemoveObject/@(id/.CoboundaryMap);
RemoveFrom[CoboundaryMap,id];
RemoveFrom[CoordinateMap,id];
RemoveFrom[StyleMap,id];
DynamicDiagram=Delete[DynamicDiagram,id/.PositionMap];
VertIDs=DeleteCases[VertIDs,id];,MemberQ[LoopIDs,id],RemoveFrom[BoundaryMap,id];
CoboundaryMap[[All,2]]=DeleteCases[CoboundaryMap[[All,2]],id,{2}];
RemoveFrom[DirectionAngleMap,id];
RemoveFrom[LoopRadiusMap,id];
RemoveFrom[StyleMap,id];
DynamicDiagram=Delete[DynamicDiagram,id/.PositionMap];
LoopIDs=DeleteCases[LoopIDs,id];,MemberQ[LinkIDs,id],RemoveFrom[BoundaryMap,id];
CoboundaryMap[[All,2]]=DeleteCases[CoboundaryMap[[All,2]],id,{2}];
RemoveFrom[TangentAngleMap,id];
RemoveFrom[StyleMap,id];
DynamicDiagram=Delete[DynamicDiagram,id/.PositionMap];
LinkIDs=DeleteCases[LinkIDs,id];,True,Null];
PositionMap=GetPositionMap[];);

(* ----- Clear Diagram ----- *)
ClearDiagram[]:=(LastID=0;
VertIDs={};
LoopIDs={};
LinkIDs={};
BoundaryMap={};
CoordinateMap={};
DirectionAngleMap={};
LoopRadiusMap={};
TangentAngleMap={};
StyleMap={};
CoboundaryMap={};
PositionMap={};
DynamicDiagram={};
DiagramPlotRange={{0,0},{0,0}};);

(* ===== Endmatter ===== *)
End[];
EndPackage[];

