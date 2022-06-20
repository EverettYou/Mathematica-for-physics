(* ::Package:: *)

(* ::Text:: *)
(*Palettes directory:*)
(*FileNameJoin[{$BaseDirectory, "SystemFiles", "FrontEnd", "Palettes"}] *)
(*FileNameJoin[{$UserBaseDirectory, "SystemFiles", "FrontEnd", "Palettes"}] *)


(* ::Code::Initialization:: *)
FileOverwrite[file1_,file2_]:=
	(If[FileType[file2]==File,DeleteFile[file2]];
	CopyFile[file1,file2];)


(* ::Code::Initialization:: *)
DirectoryOverwrite[dir1_,dir2_]:=
	(If[FileType[dir2]==Directory,DeleteDirectory[dir2,DeleteContents->True]];
	CopyDirectory[dir1,dir2];)


(* ::Text:: *)
(*Configure global preference. *)


(* ::Code::Initialization:: *)
Module[{sd},
sd=#->(#1->(Union[#,Capitalize/@#]&@Sort[#2])&@@@OptionValue[Options[$FrontEnd,#],#]&@#)&@SpellingDictionaries;
FileOverwrite[FileNameJoin[{NotebookDirectory[],"FrontEnd","init.m"}],
FileNameJoin[{$UserBaseDirectory, "FrontEnd", "init.m"}]];
Get[FileNameJoin[{$UserBaseDirectory, "FrontEnd", "init.m"}]];
SetOptions[$FrontEnd,sd];];


(* ::Code::Initialization:: *)
FileOverwrite[FileNameJoin[{NotebookDirectory[],"Kernel","init.m"}],
	FileNameJoin[{$UserBaseDirectory, "Kernel", "init.m"}]];


(* ::Text:: *)
(*Install style sheets*)


(* ::Code::Initialization:: *)
FileOverwrite[FileNameJoin[{NotebookDirectory[],"FrontEnd","CMU Article.nb"}],
	FileNameJoin[{$UserBaseDirectory, 
		"SystemFiles", "FrontEnd", "StyleSheets","CMU Article.nb"}]];


(* ::Text:: *)
(*Install Packages*)


(* ::Code::Initialization:: *)
DirectoryOverwrite[FileNameJoin[{NotebookDirectory[],"MatsubaraSum"}],
	FileNameJoin[{$InstallationDirectory,"AddOns","ExtraPackages","MatsubaraSum"}]]


(* ::Code::Initialization:: *)
DirectoryOverwrite[FileNameJoin[{NotebookDirectory[],"DiagramEditor"}],
	FileNameJoin[{$InstallationDirectory,"AddOns","ExtraPackages","DiagramEditor"}]]


(* ::Code::Initialization:: *)
DirectoryOverwrite[FileNameJoin[{NotebookDirectory[],"PauliAlgebra"}],
	FileNameJoin[{$InstallationDirectory,"AddOns","ExtraPackages","PauliAlgebra"}]]


(* ::Code::Initialization:: *)
DirectoryOverwrite[FileNameJoin[{NotebookDirectory[],"LoopIntegrate"}],
	FileNameJoin[{$InstallationDirectory,"AddOns","ExtraPackages","LoopIntegrate"}]]


(* ::Code::Initialization:: *)
DirectoryOverwrite[FileNameJoin[{NotebookDirectory[],"Themes"}],
	FileNameJoin[{$InstallationDirectory,"AddOns","ExtraPackages","Themes"}]]


(* ::Code::Initialization:: *)
DirectoryOverwrite[FileNameJoin[{NotebookDirectory[],"Toolkit"}],
	FileNameJoin[{$InstallationDirectory,"AddOns","ExtraPackages","Toolkit"}]]
