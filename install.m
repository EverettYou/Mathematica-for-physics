(* ::Package:: *)

(* ::Text:: *)
(*Palettes directory:*)
(*FileNameJoin[{$BaseDirectory, "SystemFiles", "FrontEnd", "Palettes"}] *)
(*FileNameJoin[{$UserBaseDirectory, "SystemFiles", "FrontEnd", "Palettes"}] *)


FileOverwrite[file1_,file2_]:=
	(If[FileType[file2]==File,DeleteFile[file2]];
	CopyFile[file1,file2];)


DirectoryOverwrite[dir1_,dir2_]:=
	(If[FileType[dir2]==Directory,DeleteDirectory[dir2,DeleteContents->True]];
	CopyDirectory[dir1,dir2];)


(* ::Text:: *)
(*Configure global preference. *)


Get[FileNameJoin[{NotebookDirectory[],"FrontEnd","init.m"}]];


FileOverwrite[FileNameJoin[{NotebookDirectory[],"Kernel","init.m"}],
	FileNameJoin[{$UserBaseDirectory, "Kernel", "init.m"}]];


(* ::Text:: *)
(*Update spelling dictionaries*)


With[{file=FileNameJoin[{NotebookDirectory[],"SystemFiles", "SpellingDictionaries","UserCorrectWords.m"}]},
	Put[CurrentValue[$FrontEnd,{SpellingDictionaries,"CorrectWords"}]=#,file]&@
	 Union[#,Capitalize/@#]&@Sort@
	  Join[CurrentValue[$FrontEnd,{SpellingDictionaries,"CorrectWords"}], Get[file]]]


(* ::Text:: *)
(*Install style sheets*)


FileOverwrite[FileNameJoin[{NotebookDirectory[],"FrontEnd","CMU Article.nb"}],
	FileNameJoin[{$UserBaseDirectory, 
		"SystemFiles", "FrontEnd", "StyleSheets","CMU Article.nb"}]];


(* ::Text:: *)
(*Install Packages*)


DirectoryOverwrite[FileNameJoin[{NotebookDirectory[],"MatsubaraSum"}],
	FileNameJoin[{$InstallationDirectory,"AddOns","ExtraPackages","MatsubaraSum"}]]


DirectoryOverwrite[FileNameJoin[{NotebookDirectory[],"PauliAlgebra"}],
	FileNameJoin[{$InstallationDirectory,"AddOns","ExtraPackages","PauliAlgebra"}]]


DirectoryOverwrite[FileNameJoin[{NotebookDirectory[],"LoopIntegrate"}],
	FileNameJoin[{$InstallationDirectory,"AddOns","ExtraPackages","LoopIntegrate"}]]


DirectoryOverwrite[FileNameJoin[{NotebookDirectory[],"Themes"}],
	FileNameJoin[{$InstallationDirectory,"AddOns","ExtraPackages","Themes"}]]


DirectoryOverwrite[FileNameJoin[{NotebookDirectory[],"Toolkit"}],
	FileNameJoin[{$InstallationDirectory,"AddOns","ExtraPackages","Toolkit"}]]
