(* ::Package:: *)

(*Author: Dominik Lindorfer*)
(*email: dlindo@posteo.at*)


BeginPackage["StylePlot`"]


StyleListPlot::usage = 
	"ListStylePlot[list_, Options_] Plots lists with style! Use Options[] to see the Default Options."

Begin[ "Private`"]

<< LevelScheme`CustomTicks`
SetOptions[LinTicks, MajorTickLength -> {0.025, 0}, MinorTickLength -> {0.013, 0}];

Options[StyleListPlot] = {
   
   PlotRange -> {{640, 710}, All},
   PlotStyle -> {{Black, Thickness[0.008]}, {Red, 
      Thickness[0.008]}, {Blue, Thickness[0.008]}, {Purple, 
      Thickness[0.008]}},
   Joined -> True,
   LabelStyle -> 30,
   Frame -> True,
   FrameStyle -> Thickness[0.0035],
   FrameTicks -> {{LinTicks, StripTickLabels[LinTicks]}, {LinTicks, 
      StripTickLabels[LinTicks]}},

   GridLines -> {Automatic, Automatic},
   GridLinesStyle -> Directive[GrayLevel[0.4, 0.5], AbsoluteThickness[1], AbsoluteDashing[{1, 2}]],
   PlotTheme -> {"Detailed", "Classic"},
   
	ImageSize -> 800,
   
   LegendText -> {"Expressions"}, 
   LegendPlacement -> Right, 
   LegendSpacing -> {1.7, -1.2}, 
   FrameLabels -> {{"", ""}, {"", ""}},
   PlotMarkers->None,
   LegendMargins -> {{0.5, 0.5}, {-15, -15}},
   LegendFunction -> (Framed[#, BoxFrame -> {{2, 4}, {4, 4}}, Background->White] &),

   PlotLegendsLineColor-> {Directive[Black, AbsoluteThickness[5]],
      Directive[Red, AbsoluteThickness[5]],
      Directive[Blue, AbsoluteThickness[5]],
      Directive[Purple, AbsoluteThickness[5]]}

};

StyleListPlot[list_, opts : OptionsPattern[]] := ListPlot[list,
  PlotRange -> OptionValue[PlotRange],
  PlotStyle -> OptionValue[PlotStyle],
  Joined -> OptionValue[Joined],
  LabelStyle -> OptionValue[LabelStyle],
  Frame -> OptionValue[Frame],
  FrameStyle -> OptionValue[FrameStyle],
  FrameLabel -> 
   Map[Style[#, Black] &, OptionValue[FrameLabels], {2}],
  FrameTicks -> OptionValue[FrameTicks],
  
  GridLines -> OptionValue[GridLines],
  GridLinesStyle -> OptionValue[GridLinesStyle],
  PlotTheme -> OptionValue[PlotTheme],
  
  ImageSize -> OptionValue[ImageSize],
  PlotMarkers -> OptionValue[PlotMarkers],

  PlotLegends -> Placed[LineLegend[
     OptionValue[PlotLegendsLineColor],
     OptionValue[LegendText],
     Spacings -> OptionValue[LegendSpacing],
     LegendMargins -> OptionValue[LegendMargins],
     LabelStyle -> Directive[15],
     LegendMarkerSize -> 50,
     LegendFunction -> OptionValue[LegendFunction], 
     Background -> White], 
    ToExpression[OptionValue[LegendPlacement]]]
]


End[]


EndPackage[]
