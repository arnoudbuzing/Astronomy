(* ::Package:: *)

(* ::Section:: *)
(*Package Header*)


BeginPackage["WolframExternalFunctions`Astronomy`"];


(* ::Text:: *)
(*Declare your public symbols here:*)


SkyView;
SkyViewSurveys;


Begin["`Private`"];

PythonSessionGet[id_String, deps_List, prolog_String : ""] := SelectFirst[
  ExternalSessions[],
  #["ID"] === id &,
  StartExternalSession[{
    {"Python","StandardErrorFunction"->Null},
    "ID" -> id,
    "Evaluator" -> <|
      "Dependencies" -> Complement[deps,{"math","unicodedata","textwrap"}] /. { "sklearn" -> "scikit-learn" },
      "EnvironmentName" -> id,
      "PythonRuntime" -> "3.11"
      |>,
    "SessionProlog" -> prolog
    }]
  ]

LoadExternalFunction["Python", fun_String, extra_String : ""] := Module[{components, pkg, ctx, ef, wl},
  components = StringSplit[fun, "."];
  pkg = First[components];
  ctx = StringRiffle[Most[components], "."];
  ExternalEvaluate[PythonSessionGet[pkg, {pkg}],"import " <> ctx];
  ExternalEvaluate[PythonSessionGet[pkg, {pkg}], extra];
  ef = ExternalFunction[PythonSessionGet[pkg, {pkg}], fun];
  wl = StringReplace["ArnoudBuzing`ExternalFunctions`" <> fun, {"." -> "˘", "_" -> "˘"}];
  With[{s = wl}, Quiet[Remove[s]]];
  With[{s = Symbol[wl], rhs = ef}, OwnValues[s] = {HoldPattern[s] :> rhs}];
  Symbol[wl]
  ]

$AstroPackages = {"astropy", "agnpy", "APLpy", "asdf-astropy", "astroalign", "astroML", 
"astroplan", "astropy", "astropy-healpix", "astroquery", 
"astroscrappy", "baseband", "BayesicFitting", "ccdproc", 
"cluster-lensing", "corral-pipeline", "dust_extinction", 
"einsteinpy", "feets", "gala", "galpy", "gammapy", "ginga", 
"glueviz", "gwcs", "halotools", "hendrics", "hips", "imexam", 
"kanon", "lenstronomy", "ligo.skymap", "linetools", "mocpy", "naima", 
"omnifit", "photutils", "poliastro", "pycbc", "pydl", "pyregion", 
"pyspeckit", "pyvo", "regions", "regularizepsf", "reproject", 
"sncosmo", "specreduce", "spectral-cube", "specutils", 
"spherical-geometry", "statmorph", "stingray", "synphot"};

SkyViewSurveys[] := Module[{session},
    session = PythonSessionGet["astropy", $AstroPackages, "import astropy"];
    ExternalEvaluate[session,"from astroquery.skyview import SkyView"];
    ExternalEvaluate[session,"import json"];
    Dataset @ ExternalEvaluate[session,"SkyView.survey_dict"]
]


SkyView[position_String, survey_String] := Module[{session,result},
    session = PythonSessionGet["astropy", $AstroPackages, "import astropy"];
    ExternalEvaluate[session,"from astroquery.skyview import SkyView"];
    result =ExternalFunction[session, "
def xxx(position,survey):
    result = SkyView.get_images(position,survey)
    return result[0][0].data
"][position,survey];
    If[Head[result]===NumericArray,Image[Rescale[Normal[result]]],result]
]


End[];
EndPackage[];