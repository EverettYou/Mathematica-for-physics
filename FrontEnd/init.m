(* ::Package:: *)

SetOptions[$FrontEnd, 
"DisplayImagePixels"->"Automatic",
Default2DTool->"Select",
Default3DTool->"RotateView",
VersionsLaunched->{"12.3.1"},
FontSubstitutions->{
 "CMU" -> "CMU Serif", "LaTeX" -> "CMU Serif", "Serif" -> "CMU Serif", "Sans" -> 
  "CMU Sans Serif", "Sans Serif" -> "CMU Sans Serif", "Extra" -> 
  "CMU Serif Extra", "Serif Extra" -> "CMU Serif Extra", "Condensed" -> 
  "CMU Sans Serif Demi Condensed", "Letter" -> "CMU Concrete", "Concrete" -> 
  "CMU Concrete", "Bright" -> "CMU Bright", "Classical" -> 
  "CMU Classical Serif", "Code" -> "CMU Typewriter Text", "Typewriter" -> 
  "CMU Typewriter Text", "Helv" -> "Helvetica", "Arial" -> "Helvetica", 
  "Times New Roman" -> "Times", "Courier New" -> "Courier", "Lucidabright" -> 
  "Times", "Charter" -> "Helvetica", "Lucidatypewriter" -> "Courier", "Fixed" -> 
  "Monaco", "AGaramond" -> "Times", "Avant Garde" -> "Helvetica", "Bodoni" -> 
  "Times", "Bookman" -> "Times", "Caslon 3 Roman" -> "Times", 
  "Caslon 540 Roman" -> "Times", "Century Old Style" -> "Times", "Chicago" -> 
  "Helvetica", "Dom Casual" -> "Helvetica", "Futura" -> "Helvetica", 
  "Futura Book" -> "Helvetica", "Futura Condensed" -> "Helvetica", 
  "C Futura Condensed" -> "Helvetica", "Futura Light" -> "Helvetica", 
  "L Futura Light" -> "Helvetica", "Galliard" -> "Times", "Garamond" -> 
  "Times", "Geneva" -> "Helvetica", "GillSans" -> "Gill Sans", "Gill Sans" -> 
  "Helvetica", "Goudy" -> "Times", "Helvetica Condensed" -> "Helvetica", 
  "C Helvetica Condensed" -> "Helvetica", "Helvetica Light" -> "Helvetica", 
  "L Helvetica Light" -> "Helvetica", "Helvetica Narrow" -> "Helvetica", 
  "N Helvetica Narrow" -> "Helvetica", "Melior" -> "Times", "Meridien" -> 
  "Times", "Mishawaka" -> "Helvetica", "Monaco" -> "Helvetica", "Myriad Pro" -> 
  "Lucida Grande", "New Baskerville" -> "Times", "New Century Schlbk" -> 
  "Times", "New York" -> "Times", "Optima" -> "Helvetica", "Palatino" -> 
  "Times", "Palatino Linotype" -> "Palatino", "Prestige Elite" -> "Times", 
  "Sabon" -> "Times", "Tekton" -> "Helvetica", "Trump Mediaeval" -> "Times", 
  "Univers 45 Light" -> "Helvetica", "L Univers 45 Light" -> "Helvetica", 
  "Univers 47 CondensedLight" -> "Helvetica", "CL Univers 47 CondensedLight" -> 
  "Helvetica", "Univers 55" -> "Helvetica", "Univers 57 Condensed" -> 
  "Helvetica", "C Univers 57 Condensed" -> "Helvetica", "Zapf Chancery" -> 
  "Times", "Abadi MT Condensed Light" -> "Helvetica", "Algerian" -> "Times", 
  "Arial Black" -> "Helvetica", "Arial Narrow" -> "Helvetica", 
  "Arial Rounded MT Bold" -> "Helvetica", "Billboard" -> "Helvetica", 
  "Book Antiqua" -> "Times", "Bookman Old Style" -> "Times", "Braggadocio" -> 
  "Helvetica", "Britannic Bold" -> "Helvetica", "Brush Script MT" -> 
  "Helvetica", "Calisto MT" -> "Helvetica", "Gill" -> "Trebuchet MS", 
  "Trebuchet MS" -> "Helvetica", "Helvetica Neue" -> "Helvetica", 
  "Baskerville" -> "Georgia", "Cochin" -> "Constantia", "Calibri" -> "Arial", 
  "Cambria" -> "Times New Roman", "Candara" -> "Trebuchet MS", "Consolas" -> 
  "Menlo", "Constantia" -> "Palatino", "Corbel" -> "Lucida Grande", 
  "Century Gothic" -> "Helvetica", "Century Schoolbook" -> "Times", 
  "Colonna MT" -> "Times", "Comic Sans MS" -> "Helvetica", "Desdemona" -> 
  "Helvetica", "Excalibur Logotype" -> "Helvetica", "Excalibur Monospace" -> 
  "Courier", "Excalibur Script" -> "Times", "Fixedsys" -> "Helvetica", 
  "Footlight MT Light" -> "Times", "Georgia" -> "Times", "Haettenschweiler" -> 
  "Helvetica", "Impact" -> "Helvetica", "Kino MT" -> "Helvetica", "Kool" -> 
  "Helvetica", "Lucida Console" -> "Monaco", "Lucida Grande" -> "Tahoma", 
  "Lucida Sans" -> "Helvetica", "Matura MT Script Capitals" -> "Times", 
  "Modern" -> "Helvetica", "MS LineDraw" -> "Courier", "MS Sans Serif" -> 
  "Helvetica", "MS Serif" -> "Times", "News Gothic MT" -> "Helvetica", 
  "Playbill" -> "Helvetica", "Small Fonts" -> "Helvetica", "System" -> 
  "Helvetica", "Tahoma" -> "Helvetica", "Verdana" -> "Helvetica", 
  "Wide Latin" -> "Times", "Wingdings" -> "Times", "Gothic" -> "Helvetica", 
  "New century schoolbook" -> "Times", "Swiss721bt" -> "Helvetica", 
  "Terminal" -> "Courier", "Utopia" -> "Times", "Frutiger LT Std 55 Roman" -> 
  "Helvetica", "FrutigerLTStd-Light" -> "Helvetica", "Janson Text LT Std" -> 
  "Times", "WriCMTT-Base" -> "Courier", "Bitstream Vera Sans" -> "Helvetica", 
  "Bitstream Vera Sans Mono" -> "Menlo", "DejaVu Sans Mono" -> "Menlo", 
  "Lato" -> "Gill Sans"},
MessageOptions->{"ObsoleteCDFFormatWarning"->True},
NotebookSecurityOptions->{"TrustedPath"->{
  FrontEnd`FileName[{FrontEnd`$ApplicationDocumentsDirectory}], 
  FrontEnd`FileName[{$InstallationDirectory}], 
  FrontEnd`FileName[{$BaseDirectory}], 
  FrontEnd`FileName[{$UserBaseDirectory}], "~/"}},
PrivateFrontEndOptions->{"DialogSettings"->{
 "Login" -> {"RememberMe" -> True}, 
  "Preferences" -> {
   "SubTabs" -> {
     "Appearance" -> "Graphics", "SyntaxColoring" -> "Other", "Numbers" -> 
      "Multiplication", "Security" -> "NotebookSecurity", "Internet" -> 
      "AutoUpdate", "Mail" -> "IncomingMail"}}},
"ExperimentalSettings"->{"CodeAssist" -> {"ToolbarFormatButton" -> True}},
"InterfaceSettings"->{
 "CodeFormatter" -> {"ShowDropDown" -> True}, 
  "PredictiveInterface" -> {"ShowMinimized" -> False, "FirstUse" -> False}},
"ShowAtStartup"->"NewDocument",
"WolframAlphaSettings"->{
 "BaseURL" -> "Automatic", "Autoload" -> True, "SendMathematicaSessionInfo" -> 
  True, "AppID" -> Automatic, "Asynchronous" -> True, "Interactive" -> True, 
  "Reinterpret" -> True, "Recalculate" -> True, "MathematicaFormsFormatType" -> 
  "MostlyInputForm", "ExtrusionClickClose" -> False, "ExtrusionClickEvaluate" -> 
  False}},
InputAliases->{
 "up" -> "\[UpArrow]", "dn" -> "\[DownArrow]", "->" -> "\[RightArrow]", "<-" -> 
  "\[LeftArrow]", "<<" -> "\[LessLess]", ">>" -> "\[GreaterGreater]", "/" -> 
  "\[Checkmark]", 
  "kd" -> TemplateBox[{"\[Placeholder]"}, "KroneckerDeltaSeq"],
  "ket" -> 
  TemplateBox[{"\[Placeholder]"}, "Ket", DisplayFunction -> (StyleBox[
      RowBox[{"\[LeftBracketingBar]", #, "\[RightAngleBracket]"}], 
      AutoSpacing -> False]& )], 
  "bra" -> 
  TemplateBox[{"\[Placeholder]"}, "Bra", DisplayFunction -> (StyleBox[
      RowBox[{"\[LeftAngleBracket]", #, "\[RightBracketingBar]"}], 
      AutoSpacing -> False]& )], 
  "braket" -> 
  TemplateBox[{"\[Placeholder]", "\[Placeholder]"}, "BraKet", 
    DisplayFunction -> (StyleBox[
      RowBox[{
       "\[LeftAngleBracket]", #, "\[VerticalSeparator]", #2, 
        "\[RightAngleBracket]"}], AutoSpacing -> False]& )]},
TrackCellChangeTimes->False,
ShowAutoSpellCheck->True,
ShowPredictiveInterface->False,
AutoStyleOptions->{"CommentStyle"->{
 FontColor -> 
  RGBColor[0.29620813305867094`, 0.6892652780956741, 0.9107347219043259], 
  FontWeight -> Plain, ShowAutoStyles -> False, ShowSyntaxStyles -> False, 
  AutoNumberFormatting -> False, TranslationOptions -> {"Enabled" -> False}},
"EmphasizedSyntaxErrorStyle"->{
 FontColor -> 
  RGBColor[0.6651712825207904, 0.17590600442511636`, 0.09016556038757916], 
  Background -> RGBColor[1, 0.86, 0.86]},
"ExcessArgumentStyle"->{
 FontColor -> 
  RGBColor[0.6651712825207904, 0.17590600442511636`, 0.09016556038757916]},
"FunctionLocalVariableStyle"->{
 FontColor -> 
  RGBColor[0.21475547417410543`, 0.5216449225604639, 0.15168993667505912`]},
"LocalScopeConflictStyle"->{
 FontColor -> 
  RGBColor[0.6651712825207904, 0.17590600442511636`, 0.09016556038757916]},
"LocalVariableStyle"->{
 FontColor -> 
  RGBColor[0.5120164797436484, 0.25990691996643017`, 0.15808346684977492`]},
"MissingArgumentStyle"->{
 FontColor -> 
  RGBColor[0.6651712825207904, 0.17590600442511636`, 0.09016556038757916]},
"OrderOfEvaluationConflictStyle"->{
 FontColor -> 
  RGBColor[0.6651712825207904, 0.17590600442511636`, 0.09016556038757916]},
"PatternVariableStyle"->{
 FontColor -> 
  RGBColor[0.45439841306172274`, 0.25384908827344166`, 0.6460517280842298], 
  FontSlant -> "Italic"},
"SymbolShadowingStyle"->{
 FontColor -> 
  RGBColor[0.6651712825207904, 0.17590600442511636`, 0.09016556038757916]},
"SyntaxErrorStyle"->{
 FontColor -> RGBColor[0.7600061036087586, 0.3300068665598535, 0.8]},
"UndefinedSymbolStyle"->{
 FontColor -> 
  RGBColor[0.18173495078965438`, 0.30737773708705274`, 0.7314412146181429]},
"UnknownOptionStyle"->{
 FontColor -> 
  RGBColor[0.6651712825207904, 0.17590600442511636`, 0.09016556038757916]},
"UnwantedAssignmentStyle"->{
 FontColor -> 
  RGBColor[0.6651712825207904, 0.17590600442511636`, 0.09016556038757916]}},
CodeAssistOptions->{"FloatingElementEnable"->False, 
 "CodeToolsOptions"->Association[
 "CodeFormatterInteractiveReparse" -> True],
"HeadHighlightStyle"->{
 Background -> 
  RGBColor[0.9672388799877928, 0.8162203402761883, 0.28880750743877315`]},
"MatchHighlightStyle"->{
 Background -> 
  RGBColor[0.9672388799877928, 0.8162203402761883, 0.28880750743877315`]}},
SpellingOptions->{"AutoSpellCheckPopupDelay"->10000},
SpellingDictionaries->{"CorrectWords"->{
 "Abrikosov", "anticommute", "Anticommute", "anticommuting", "Anticommuting", 
  "Antiferromagnet", "antisymmetrize", "Antisymmetrize", "anyons", "Anyons", 
  "bilayer", "Bilayer", "bilinearity", "Bilinearity", "bipartition", 
  "Bipartition", "Bogoliubov", "bosonic", "Bosonic", "Bosonization", "brane", 
  "Brane", "branes", "Branes", "cocycles", "Cocycles", "codifferential", 
  "Codifferential", "decohere", "Decohere", "Decoherent", "deconfine", 
  "Deconfine", "deconfined", "Deconfined", "deconfinement", "Deconfinement", 
  "dyon", "Dyon", "eigen", "Eigen", "eigenbasis", "Eigenbasis", 
  "eigenenergies", "Eigenenergies", "eigenenergy", "Eigenenergy", 
  "eigenspaces", "Eigenspaces", "Eq", "exponentiate", "Exponentiate", 
  "exponentiated", "Exponentiated", "extremization", "Extremization", 
  "fermionic", "Fermionic", "Fierz", "fractionalization", "Fractionalization",
   "fractionalize", "Fractionalize", "fractionalized", "Fractionalized", 
  "gapless", "Gapless", "gaplessness", "Gaplessness", "gaugable", "Gaugable", 
  "gauge", "Gauge", "Hellmann", "Higgsed", "Higgsing", "irrep", "Irrep", 
  "Kagome", "ket", "Ket", "kets", "Kets", "Luttinger", "magnon", "Magnon", 
  "Majorana", "Neel", "orthogonalization", "Orthogonalization", "paramagnet", 
  "Paramagnet", "partons", "Partons", "plaquette", "Plaquette", "plaquettes", 
  "Plaquettes", "prefactors", "Prefactors", "premodular", "Premodular", 
  "quantumly", "Quantumly", "quasiparticles", "Quasiparticles", "qubit", 
  "Qubit", "qudit", "Qudit", "qudits", "Qudits", "qutrit", "Qutrit", "semion",
   "Semion", "Shastry", "Simons", "spinless", "Spinless", "spinon", "Spinon", 
  "spinons", "Spinons", "spinor", "Spinor", "Stratonovich", "Subfigures", 
  "sublattice", "Sublattice", "sublattices", "Sublattices", "superfluid", 
  "Superfluid", "th", "Th", "thermalization", "Thermalization", "thermalized",
   "Thermalized", "tricritical", "Tricritical", "unkinked", "Unkinked", 
  "vierbein", "Vierbein", "vison", "Vison", "visons", "Visons", "Wannier", 
  "Wess", "Wyle", "Yizhuang", "Yukawa", "Zumino", "Emergentism", "symmorphic",
   "Symmorphic", "Nonsymmorphic", "Seitz"},
"IncorrectWords"->{},
"Suggestions"->{}},
AutoMultiplicationSymbol->{"Numbers", "LineBreaks"},
NumberMultiplier->"\[Times]",
RenderingOptions->{"HardwareAntialiasingQuality"->1.}]
