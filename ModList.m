(* ::Package:: *)

BeginPackage["ModList`"]


ScaleY::usage = 
	"ScaleY[list_, parameter_] Scales every Element y of list {x,y} by 1/parameter."

ShiftX::usage = 
	"ShiftX[list_, parameter_] Set every Element x of list {x,y} to x + parameter."

ShiftY::usage = 
	"ShiftY[list_, parameter_] Set every Element y of list {x,y} to y + parameter."

Take2::usage =
   "Take2[list_, a_, b_] Takes the Elements a & b of a sublist in list. Example: Take2[{{a,b,c,d,},{a,b,c,d}}, 1, 2] = {{a,b},{a,b}}."

TakeSubList::usage = "TakeSubList[list_, a_, b_] Takes the Elements a to b of a sublist in list. Example: TakeSubList[{{a,b,c,d,},{a,b,c,d}}, 1, 3] = {{a,b,c},{a,b,c}}."

TakeSubListExcept::usage = "TakeSubListExcept[list_, a_, b_] Takes all Elements without the sublist from a to b in a list. Example: TakeSubList[{{a,b,c,d,},{a,b,c,d}}, 2, 3] = {{a, d},{a, d}}."

InsertSubList::usage = "InsertSubList[list_, sublist_, position_, NestedLists_: False] Inserts the sublist into a list at the given position. If NestedLists is set to True, list and sublist are expected to be NestedLists. \n \n Example 1: InsertSubList[{a, b, c, d, e}, {z, x, y}, 3] = {a, b, z, x, y, c, d, e}. \n Example 2: a1 = {{100, 1000, 2000}, {100, 1000, 2000}}; a2 = {{1, 2, 3, 4}, {5, 6, 7, 8}}; InsertSubList[a2, a1, 2, True] = {{1, 100, 1000, 2000, 2, 3, 4}, {5, 100, 1000, 2000, 6, 7, 8}}"


Begin[ "Private`"]


Take2[list_, a_, b_] := Module[{},

  Map[#[[{a, b}]] &, list]
]

TakeSubList[list_, X__] := Map[Take[#, {X}]&, list]

TakeSubListExcept[list_, X__] := Map[Drop[#, {X}] &, list]

InsertSubList[list_, sublist_, position_, NestedLists_: False] := Module[{},
  
  If[	NestedLists,
   
   		MapThread[Flatten[Insert[#1, #2, position]] &, {list, sublist}],
   
   		Flatten[Insert[list, sublist, position]],
   
   		Print["Specify if a Nested-List is given to InsertSubList[]!"]
   	]
  
]


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


End[]


EndPackage[]
