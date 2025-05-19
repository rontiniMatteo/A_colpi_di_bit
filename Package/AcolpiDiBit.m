### Codice pulito
(* ::Package:: *)

(* :Title: Main *)
(* :Context: Main` *)
(* :Author: Daniele Russo, Nicola Modugno *)
(* :Version: 3.4 *)
(* :Date: 2025‑05‑06 *)

(* :Summary:
   Pacchetto principale che coordina le fasi del gioco, dall'inizializzazione
   alla battaglia, e gestisce l’interfaccia utente.
*)

(* :Keywords: battaglia navale, UI, Dynamic refactoring *)
(* :Requirements: Mathematica 12.0+, Util`, Battle`, Interaction` *)

BeginPackage["AcolpiDiBit`", {"Util`", "Battle`", "Interaction`"}];

placementUI::usage =
  "placementUI[] visualizza l’interfaccia di posizionamento navi.";

Begin["`Private`"];

(* ───────────────────────────────────────────────────────────── *)
(*  FUNZIONE PUBBLICA                                           *)
(* ───────────────────────────────────────────────────────────── *)

placementUI[] :=
 DynamicModule[
  {
   seedValue        = ToString@RandomInteger[1024],
   baseValue        = 2,
   difficultyLevel  = 3,
   phase            = 1,
   initDone         = False,
   message          = "",
   cpuShips, battleStarted = False,
   userShips, userGrid, cpuGrid,
   difficultyLevels
  },

  difficultyLevels = getDifficultyLevels[];
  setSeed[ToExpression@seedValue];

  Style[
   Column[{
     Dynamic[renderPhase[]]      (* Dynamic “corto”: chiama una sola funzione *)
   }],
   FontFamily -> "Arial"
  ]
];

(* ───────────────────────────────────────────────────────────── *)
(*  DISPATCHER DINAMICO (chiamato dall’unico Dynamic “corto”)   *)
(* ───────────────────────────────────────────────────────────── *)

renderPhase[] :=
 Switch[phase,
  1, phaseOneUI[],
  2, phaseTwoUI[],
  3, phaseThreeUI[],
  _, Style["Fase sconosciuta", Red]
 ];

(* ───────────────────────────────────────────────────────────── *)
(*  FASI DELL’INTERFACCIA                                      *)
(* ───────────────────────────────────────────────────────────── *)

phaseOneUI[] :=
 Column[{
   askSeedInput[Function[input, seedValue = input]],
   Spacer[10],
   askBaseChoice[Function[input, baseValue = input]],
   Spacer[10],
   Row[{
     "Livello di difficoltà: ",
     PopupMenu[
      Dynamic[difficultyLevel],
      Table[i -> difficultyLevels[[i, 1]], {i, Length@difficultyLevels}]
     ]
   }],
   Spacer[10],
   Button["Conferma Impostazioni", confirmInitProcess[]],
   Spacer[10],
   messageDisplayDyn[]
 }];

phaseTwoUI[] :=
 Column[{
   placementDM[],
   Button["Reset Game", phase = 1;]
 }];

phaseThreeUI[] :=
 Column[{
   Style["Battaglia Navale in Base " <> ToString@baseValue, Bold, 20, Red],
   Spacer[10],
   battleStartDyn[],
   Spacer[10],
   Button["Reset Game", resetGame[]; phase = 1;]
 }];

(* ───────────────────────────────────────────────────────────── *)
(*  FASE 1 – logica                                             *)
(* ───────────────────────────────────────────────────────────── *)

confirmInitProcess[] :=
 If[
  isSeed@seedValue && isBase@baseValue,
  message = "";
  If[initPhase[ToExpression@seedValue, baseValue, difficultyLevel],
   phase = 2; initDone = True,
   message = "Errore durante l’inizializzazione. Riprova."
  ],
  message = "Seed non valido!\nInserisci un numero intero decimale."
 ];

(* ───────────────────────────────────────────────────────────── *)
(*  FASE 2 – DynamicModule compatto                             *)
(* ───────────────────────────────────────────────────────────── *)

placementDM[] :=
 DynamicModule[
  {
   currentShip      = 1, start = "", end = "",
   shipPlacementMsg = "", placementDone = False,
   gridSize         = getGridSize[]
  },

  Column[{
    Style["Fase di Posizionamento Navi", Bold, 16],
    Style[
     "Livello: " <> difficultyLevels[[difficultyLevel, 1]] <>
      "  •  Griglia " <> ToString@gridSize <> " × " <> ToString@gridSize,
     Italic
    ],

    Row[{
      (* ── Colonna sinistra ── *)
      Column[{
        currentShipRowDyn[],
        Grid[{
          {"Inizio:", InputField[Dynamic[start], String,
             Enabled -> Dynamic[! placementDone]]},
          {"Fine:",   InputField[Dynamic[end],   String,
             Enabled -> Dynamic[! placementDone]]},

          {"",
           Button["Conferma", confirmPlacementProcess[],
            Enabled -> Dynamic[! placementDone]]
          },

          {"",
           Button["Avvia Battaglia",
            (
             userShips = getUserShips[];
             userGrid  = getUserGrid[];
             phase     = 3;
            ),
            Enabled -> Dynamic[placementDone]]
          },

          {"",
           Row[{helpUser@baseValue, Spacer[30], helpUserPersonalized@baseValue}]
          }
        }],
        placementMsgDyn[]
      }],

      Spacer[30],

      (* ── Colonna destra ── *)
      Column[{
        Style["La tua flotta", Bold, 14],
        gridPreviewDyn[],
        Style["Navi da posizionare:", Bold, 12],
        remainingShipsDyn[]
      }]
    }]
  }]
 ];

confirmPlacementProcess[] :=
 Module[{result = placeUserShip[start, end]},
  If[result[[1]],
   start = ""; end = "";
   If[Length@getRemainingShipLengths[] == 0,
    placementDone     = True;
    shipPlacementMsg  = "Tutte le navi sono state posizionate!",
    currentShip++; shipPlacementMsg = result[[2]]
   ],
   shipPlacementMsg   = result[[2]]
  ]
 ];

(* ───────────────────────────────────────────────────────────── *)
(*  FASE 3 – Dynamic compatto                                   *)
(* ───────────────────────────────────────────────────────────── *)

startBattleRender[] :=
 startGame[
  getUserShips[],
  getCpuShip[],
  getUserGrid[],
  getCpuGrid[],
  baseValue,
  getGridSize[]
 ];

battleStartDyn[] := Dynamic[startBattleRender[]];

(* ───────────────────────────────────────────────────────────── *)
(*  DYNAMIC “CORTI”                                             *)
(* ───────────────────────────────────────────────────────────── *)

displayMessage[msg_] := Style[msg, Red];
messageDisplayDyn[]  := Dynamic[displayMessage@message];

currentShipLabel[i_] := Row[{"Nave ", i, ":"}];
currentShipRowDyn[]  := Dynamic[currentShipLabel@currentShip];

gridPreview[] :=
 Util`showGrid[getUserGrid[], True];
gridPreviewDyn[] := Dynamic[gridPreview[]];

remainingShipsCalc[] :=
 Module[{r = getRemainingShipLengths[]},
  If[Length@r > 0,
   "Lunghezze rimanenti: " <> ToString@r,
   "Tutte le navi sono state posizionate!"
  ]
 ];
remainingShipsDyn[] := Dynamic[remainingShipsCalc[]];

(* ───────────────────────────────────────────────────────────── *)
End[];
EndPackage[];
