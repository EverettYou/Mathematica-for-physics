(* ::Package:: *)

(* Mathematica Package *)
(* Author: Everett You *)
(* Created by the Code Collector at Sat 20 Apr 2019 16:21:01 *)
(* from file: /Users/everettyou/Dropbox/Mathematica/Projects/Theme/Academic.nb *)
(* ===== Academic Color Scheme ===== *)

(* ----- Named Colors ----- *)
Begin["System`"];
Unprotect[DarkRed,DarkGreen,DarkBlue,DarkCyan,DarkMagenta,DarkYellow,DarkBrown,DarkOrange,DarkPink,DarkPurple,Red,Green,Blue,Cyan,Magenta,Yellow,Brown,Orange,Pink,Purple,LightRed,LightGreen,LightBlue,LightCyan,LightMagenta,LightYellow,LightBrown,LightOrange,LightPink,LightPurple];
{{DarkRed,DarkGreen,DarkBlue,DarkCyan,DarkMagenta,DarkYellow,DarkBrown,DarkOrange,DarkPink,DarkPurple},{Red,Green,Blue,Cyan,Magenta,Yellow,Brown,Orange,Pink,Purple},{LightRed,LightGreen,LightBlue,LightCyan,LightMagenta,LightYellow,LightBrown,LightOrange,LightPink,LightPurple}}={{RGBColor[0.534081,0.0853132,0.16669],RGBColor[0.09,0.38,0.12],RGBColor[0.163302,0.119982,0.79353],RGBColor[0.13,0.58,0.89],RGBColor[0.5450980392156862,0,0.5450980392156862],RGBColor[0.68,0.54,0.19],RGBColor[0.44,0.22,0.08],RGBColor[0.8235294117647058,0.4117647058823529,0.11764705882352941`],RGBColor[0.66,0.08235294117647059,0.48],RGBColor[0.29411764705882354`,0,0.5098039215686274]},{RGBColor[0.790588,0.201176,0.],RGBColor[0.,0.596078,0.109804],RGBColor[0.192157,0.388235,0.807843],RGBColor[0.14,0.7490196078431373,0.9500000000000001],RGBColor[0.8549019607843137,0.4392156862745098,0.8392156862745098],RGBColor[1.,0.8431372549019608,0.23],RGBColor[{Rational[5,8],Rational[41,128],Rational[45,256]}],RGBColor[1.,0.607843,0.],RGBColor[0.8588235294117647,0.4392156862745098,0.5764705882352941],RGBColor[0.567426,0.32317,0.729831]},{RGBColor[0.9411764705882353,0.5019607843137255,0.5019607843137255],RGBColor[0.5607843137254902,0.7372549019607844,0.5607843137254902],RGBColor[0.39215686274509803`,0.5843137254901961,0.9294117647058824],RGBColor[0.5294117647058824,0.807843137254902,0.9803921568627451],RGBColor[0.8666666666666667,0.6274509803921569,0.8666666666666667],RGBColor[0.9411764705882353,0.9019607843137255,0.5490196078431373],RGBColor[{Rational[111,128],Rational[23,32],Rational[135,256]}],RGBColor[{Rational[255,256],Rational[51,64],Rational[153,256]}],RGBColor[1.,0.7529411764705882,0.796078431372549],RGBColor[0.6823529411764706,0.5794117647058823,0.8941176470588235]}};
Protect[DarkRed,DarkGreen,DarkBlue,DarkCyan,DarkMagenta,DarkYellow,DarkBrown,DarkOrange,DarkPink,DarkPurple,Red,Green,Blue,Cyan,Magenta,Yellow,Brown,Orange,Pink,Purple,LightRed,LightGreen,LightBlue,LightCyan,LightMagenta,LightYellow,LightBrown,LightOrange,LightPink,LightPurple];
End[];

(* ----- Vivid Color Scheme ----- *)
Begin["DataPaclets`ColorDataDump`"];
Function[{scheme},AppendTo[colorSchemeNames,scheme[[1,1]]];
AppendTo[colorSchemes,scheme];]/@{{{0,0,{"vivid color","Vivid Color"},"VividColor"},{"Indexed","Charting"},1,{1,\[Infinity],1},ToColor[If[1<=#1<=5,{RGBColor[0.192157,0.388235,0.807843],RGBColor[0.790588,0.201176,0.],RGBColor[0.,0.596078,0.109804],RGBColor[1.,0.607843,0.],RGBColor[0.567426,0.32317,0.729831]}[[#1]],Blend[{RGBColor[0.81,0.225,0.225],RGBColor[0.855,0.45,0.1215],RGBColor[0.9,0.675,0.],RGBColor[0.495,0.63,0.],RGBColor[0.,0.63,0.63],RGBColor[0.225,0.45,0.9],RGBColor[0.45,0.315,0.9],RGBColor[0.558,0.252,0.63],RGBColor[0.675,0.252,0.45],RGBColor[0.81,0.225,0.225]},FractionalPart[(4+#1-1)/GoldenRatio]]],RGBColor]&,"{\"Indexed\", \"VividColor\"}"},{{"BlueRedSplit","blue-red split",{}},{"Gradients"},1,{0,1},{RGBColor[0.163302,0.119982,0.79353],RGBColor[0.254221,0.313173,0.892833],RGBColor[0.407119,0.543513,0.938275],RGBColor[0.572715,0.73338,0.95065],RGBColor[0.720374,0.855234,0.928635],RGBColor[0.831017,0.903518,0.868326],GrayLevel[1],RGBColor[0.894452,0.880139,0.77279],RGBColor[0.907999,0.789417,0.652903],RGBColor[0.874505,0.639254,0.522424],RGBColor[0.79915,0.446142,0.391971],RGBColor[0.685695,0.242449,0.268261],RGBColor[0.534081,0.0853132,0.16669]},""}};
End[];

(* ===== Academic Plot Theme ===== *)

(* ----- Begin ----- *)
Begin["System`PlotThemeDump`"];
Themes`ThemeRules;(*preload Theme system*)
(* Define the base theme. *)
resolvePlotTheme["Academic",def:_String]:=Themes`SetWeight[Join[resolvePlotTheme["AcademicFrame",def],resolvePlotTheme["Figure",def],resolvePlotTheme["AcademicStyle",def],resolvePlotTheme["VividColor",def]],Themes`$DesignWeight];

(* ----- Axes Features ----- *)
(* [ Specific Features for 2D Plots ] *)
(* Academic figures are framed by default. *)
resolvePlotTheme["AcademicFrame",def:_String]:=Themes`SetWeight[Join[{Axes->False,Frame->True},resolvePlotTheme["AcademicFrame2D",def]],$ComponentWeight];
(* Cases with axes also. *)
resolvePlotTheme["AcademicFrame",def:"Plot"|"ListPlot"|"ListLinePlot"|"ListStepPlot"|"PolarPlot"|"ListPolarPlot"|"PairedBarChart"|"PairedHistogram"]:=Themes`SetWeight[Join[{Axes->True,Frame->True},resolvePlotTheme["AcademicFrame2D",def]],$ComponentWeight];
(* Frame not specified,but MeshStyle specified to be thin and light. *)
resolvePlotTheme["AcademicFrame",def:"ArrayPlot"|"MatrixPlot"]:=Themes`SetWeight[Join[{MeshStyle->Directive[AbsoluteThickness[0.5],Opacity[0.25]]},resolvePlotTheme["AcademicFrame2D",def]],$ComponentWeight];
(* Charts are not framed. *)
resolvePlotTheme["AcademicFrame",def:"BarChart"|"PieChart"|"RectangleChart"|"SectorChart"|"CandlestickChart"|"KagiChart"|"LineBreakChart"|"PointFigureChart"|"RenkoChart"|"InteractiveTradingChart"|"TradingChart"|"NumberLinePlot"|"TimelinePlot"|"WordCloud"]:=resolvePlotTheme["AcademicFrame2D",def];
(* [ Specific Features for 3D Plots ] *)
(* Front axes back box. *)
resolvePlotTheme["AcademicFrame",def:_String/;StringMatchQ[def,___~~"3D"]]:=Themes`SetWeight[Join[{Axes->True,AxesEdge->{{-1,-1},{1,-1},{-1,-1}},Boxed->{Left,Bottom,Back}},resolvePlotTheme["AcademicFrame3D",def]],$ComponentWeight];
(* Front box back axes. *)
resolvePlotTheme["AcademicFrame","ChromaticityPlot3D"]:=Themes`SetWeight[Join[{Axes->True,AxesEdge->{{-1,-1},{-1,-1},{-1,1}},Boxed->{Left,Top,Front}},resolvePlotTheme["AcademicFrame3D","ChromaticityPlot3D"]],$ComponentWeight];
(* Chart3Ds are not boxed. *)
resolvePlotTheme["AcademicFrame",def:"BarChart3D"|"PieChart3D"|"RectangleChart3D"|"SectorChart3D"]:=resolvePlotTheme["AcademicFrame3D",def];
(* [ Common Axes Features ] *)
(* Mathematica's working theme:axes and frames too thin,terribly grayish,and tick/label font too small,but grids too thick.All these are fixed in the new theme. *)
resolvePlotTheme["AcademicFrame2D",_]:=Themes`SetWeight[{AxesStyle->Directive[AbsoluteThickness[1],monoColor,FontSize->14],FrameStyle->Directive[AbsoluteThickness[1],monoColor,FontSize->14],TicksStyle->Directive[monoColor,FontSize->12],FrameTicksStyle->Directive[monoColor,FontSize->12],GridLinesStyle->Directive[AbsoluteThickness[0.5],Opacity[0.5]]},$ComponentWeight];
resolvePlotTheme["AcademicFrame3D",_]:=Themes`SetWeight[{AxesStyle->Directive[AbsoluteThickness[1],monoColor,FontSize->14],TicksStyle->Directive[monoColor,FontSize->12],BoxStyle->monoColor},$ComponentWeight];

(* ----- Size Features ----- *)
(* 2D plots 180 pts. *)
resolvePlotTheme["Figure",def:_String]:=Themes`SetWeight[{ImageSizeRaw->{{180},{180}},LabelStyle->Directive[monoColor,FontSize->12]},Themes`$SizeWeight];
(* ArrayPlot,MatrixPlot smaller,140 pts. *)
resolvePlotTheme["Figure","ArrayPlot"|"MatrixPlot"]:=Themes`SetWeight[{ImageSizeRaw->{{140},{140}},LabelStyle->Directive[monoColor,FontSize->12]},Themes`$SizeWeight];
(* 3D plots 200 pts. *)
resolvePlotTheme["Figure",def:_String/;StringMatchQ[def,___~~"3D"]]:=Themes`SetWeight[{LabelStyle->Directive[monoColor,FontSize->12]},Themes`$SizeWeight];
(* Size not specified for LinePlots (because they are wide). *)
resolvePlotTheme["Figure","NumberLinePlot"|"TimelinePlot"]:={};

(* ----- Style Features ----- *)
(* Use absolute size. *)
resolvePlotTheme["AcademicStyle",def:_String]:=Themes`SetWeight[{"DefaultPlotStyle"->Directive[AbsolutePointSize[4],AbsoluteThickness[1.5]],MeshStyle->Directive[monoColor,AbsoluteThickness[1]],"DefaultBoundaryStyle"->Directive[monoColor]},$LineThicknessWeight];
(* Special cases *)
resolvePlotTheme["AcademicStyle","ContourPlot"]:=Themes`SetWeight[{ContourStyle->Directive[Opacity[1],AbsoluteThickness[0.8]],ExclusionsStyle->{None,Directive[Black,AbsoluteThickness[1]]},"DefaultPlotStyle"->Directive[AbsolutePointSize[4],AbsoluteThickness[1]],MeshStyle->Directive[monoColor,AbsoluteThickness[1]],"DefaultBoundaryStyle"->Directive[monoColor]},$LineThicknessWeight];
resolvePlotTheme["AcademicStyle","ComplexPlot"]:=Themes`SetWeight[{BoundaryStyle->AbsoluteThickness[1],"DefaultPlotStyle"->Directive[AbsolutePointSize[4],AbsoluteThickness[1]],MeshStyle->Directive[monoColor,AbsoluteThickness[1]],"DefaultBoundaryStyle"->Directive[monoColor]},$LineThicknessWeight];

(* ----- Color Features ----- *)
(* Color scheme based on VibrantColor,which is bright and vivid. *)
resolvePlotTheme["VividColor",def:_String]:=Module[{},$ThemeColorIndexed=0;
$ThemeColorDensity="BlueRedSplit";(*Thermometer for density*)$ThemeColorArrayPlot={Black,White};(*Grayscale for Array*)$ThemeColorDensity3D="BlueRedSplit";(*Thermometer for density*)$ThemeColorVectorDensity="VibrantColorVectorDensityGradient";
$ThemeColorFinancial={Green,Red};
$ThemeColorGradient={Red,Orange,Green,Blue,Purple};
$ThemeColorMatrix={{0,Darker[Blue]},{0.1,Blue},{0.499999,Lighter[Blue,0.8]},{0.5,ToColor[White,RGBColor]},{0.500001,Lighter[Red,0.8]},{0.9,Red},{1,Darker[Red]}};
$ThemeColorFractal="VibrantColorFractalGradient";
$ThemeColorWavelet={Blue,Red,Orange,White};
resolvePlotTheme["ColorStyle",def]];

(* ----- Point Marker Features ----- *)
(* Define markers. *)
System`SmallOpenMarkers := {Graphics[{{GrayLevel[1], Disk[{0, 0}, Offset[{2., 2.}, {0., 0.}]]}, {AbsoluteThickness[1.5], Dashing[{}], Circle[{0, 0}, Offset[{2., 2.}, {0., 0.}]]}}, ImageSize -> 10], Graphics[{{GrayLevel[1], Polygon[{Offset[{0, 2.6}], Offset[{-2.25166604983954, -1.3}], Offset[{2.25166604983954, -1.3}]}]}, {AbsoluteThickness[1.5], Dashing[{}], JoinedCurve[Line[{Offset[{0, 2.6}], Offset[{-2.25166604983954, -1.3}], Offset[{2.25166604983954, -1.3}], Offset[{0, 2.6}]}], CurveClosed -> True]}}, ImageSize -> 10], Graphics[{{GrayLevel[1], Polygon[{Offset[{0, 2.5}], Offset[{2.5, 0}], Offset[{0, -2.5}], Offset[{-2.5, 0}]}]}, {AbsoluteThickness[1.5], Dashing[{}], Line[{Offset[{0, 2.5}], Offset[{2.5, 0}], Offset[{0, -2.5}], Offset[{-2.5, 0}], Offset[{0, 2.5}]}]}}, ImageSize -> 10], Graphics[{{GrayLevel[1], Polygon[{Offset[{-1.8, -1.8}], Offset[{1.8, -1.8}], Offset[{1.8, 1.8}], Offset[{-1.8, 1.8}], Offset[{-1.8, -1.8}]}]}, {AbsoluteThickness[1.5], Dashing[{}], Line[{Offset[{-1.8, -1.8}], Offset[{1.8, -1.8}], Offset[{1.8, 1.8}], Offset[{-1.8, 1.8}], Offset[{-1.8, -1.8}]}]}}, ImageSize -> 10], Graphics[{{GrayLevel[1], Polygon[{Offset[{0, -2.6}], Offset[{-2.25166604983954, 1.3}], Offset[{2.25166604983954, 1.3}]}]}, {AbsoluteThickness[1.5], Dashing[{}], JoinedCurve[Line[{Offset[{0, -2.6}], Offset[{-2.25166604983954, 1.3}], Offset[{2.25166604983954, 1.3}], Offset[{0, -2.6}]}], CurveClosed -> True]}}, ImageSize -> 10]};
resolvePlotTheme["SmallOpenMarkers",def:_String]:=Themes`SetWeight[{PlotMarkers->System`SmallOpenMarkers},$ComponentWeight];

(* ----- Deploy Plot Theme ----- *)
End[];
$PlotTheme="Academic";(*Set to default plot theme*)

