(* ::Package:: *)
(*Author: Dominik Lindorfer*)
(*email: dlindo@posteo.at*)

BeginPackage["ModList`"]


ScaleY::usage = 
	"ScaleY[list_, parameter_] Scales every Element y of list {x,y} by 1/parameter."

ShiftX::usage = 
	"ShiftX[list_, parameter_] Set every Element x of list {x,y} to x + parameter."

ShiftY::usage = 
	"ShiftY[list_, parameter_] Set every Element y of list {x,y} to y + parameter."

Take2::usage =
   "Take2[list_, a_, b_] Takes the Elements a & b of a sublist in list. Example: Take2[{{a,b,c,d,},{a,b,c,d}}, 1, 2] = {{a,b},{a,b}}."

Begin[ "Private`"]


ScaleY[list_,para_]:=Module[{i,list1},list1=list;
For[i=1,i<=Length[list],i++,list1[[i]]={list[[i,1]],list[[i,2]]/para}];
Return[list1]]
ShiftX[list_,para_]:=Module[{i,list1},list1=list;
For[i=1,i<=Length[list],i++,list1[[i]]={list[[i,1]]+para,list[[i,2]]}];
Return[list1]]
ShiftY[list_,para_]:=Module[{i,list1},list1=list;
For[i=1,i<=Length[list],i++,list1[[i]]={list[[i,1]],list[[i,2]]+para}];
Return[list1]]
AddLists[list_,list1_]:=Module[{i,list2},list2=list;
For[i=1,i<=Length[list],i++,list2[[i]]={list[[i,1]],list[[i,2]]+list1[[i,2]]}];
Return[list2]]

Take2[list_, a_, b_] := Module[{},

  Map[#[[{a, b}]] &, list]
]


End[]


EndPackage[]
