module Web exposing (..)

import Events as Ev
import ProcessStory as Ps

import Browser
import Browser.Events
import Json.Decode as Decode
import Html exposing (Html)
import Html.Attributes as Attr 
import Html.Events as Events
import File exposing (File)
import File.Select as Select
import Task

-- MAIN
main : Program Flags Model Msg
main = 
  Browser.element
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions 
    }

-- MODEL
type alias Model = 
  { story : Ev.Story
  , chapterCount : Int
  , newChapter : Bool
  , events : List (Ev.Event) 
  , dialogue : List (Ev.Dialogue)
  , dec1 : Ev.Decision
  , dec2 : Ev.Decision
  , points : Ev.Points 
  , prevPoints : Ev.Points -- needed for undo
  }

type Msg 
  = Noop 
  | Inc 
  | Dec1 
  | Dec2 
  | Redo 
  | FileRequested
  | FileSelected File
  | FileLoaded String

type alias Flags = ()

-- initialization
init : Flags -> (Model, Cmd Msg)
init() =
  (initModel, Cmd.none)

initModel : Model
initModel = -- nonsense to be fixed when the file is loaded
  { story = Ev.endStory
  , chapterCount = 1
  , newChapter = False
  , events = []
  , dialogue = []
  , dec1 = Ev.empty
  , dec2 = Ev.empty
  , points = []
  , prevPoints = [] -- needed for undo button
  }

-- fill model after file is chosen (second init sorta)
fillModel : Ev.Story -> Model
fillModel st =
  { story = st
  , chapterCount = 1
  , newChapter = True
  , events = Ev.getEventList (Ev.getChapter st 1)
  , dialogue = Ev.getDialogueList (Ev.getEvent (Ev.getChapter st 1))
  , dec1 = Ev.empty
  , dec2 = Ev.empty
  , points = Ev.getStoryPoints st
  , prevPoints = Ev.getStoryPoints st -- needed for undo button
  }

-- MODEL HELPER FUNCTIONS
setDec : Int -> Ev.Dialogue -> Ev.Decision
setDec num dia =
  let (dec1, dec2) = Ev.getDecisions dia in
    if num == 1 then dec1
    else             dec2

removeDialogue : List(Ev.Dialogue) -> List(Ev.Dialogue)
removeDialogue dias =
  case dias of
    _::rest -> rest 
    [] -> Debug.todo "there should not be an empty dialogue list"

getFirstDialogue : List(Ev.Dialogue) -> Ev.Dialogue
getFirstDialogue dias = 
  case dias of 
    dia::_ -> dia
    [] -> Debug.todo "cannot find first dialogue of an empty list"

-- dialogue list update for when a new event occurs (special case)
newDialogueList : List(Ev.Event) -> List(Ev.Dialogue)
newDialogueList evs =
  case evs of 
    e::rest -> Ev.getDialogueList e 
    [] -> Debug.todo "new event lists should not be new"


removeEvents : List(Ev.Event) -> Ev.Points -> List(Ev.Event)
removeEvents evs pt =
  case evs of 
    _::ev::rest ->
      let 
        (c, threshold) = Ev.getInfo ev
        t = Ev.getCurrentPoints pt c
      in
        if t >= threshold then ev::rest 
        else removeEvents (ev::rest) pt
    _ -> []

-- remove events for when a new chapter happens (special case)
newEventList : List(Ev.Event) -> Ev.Points -> List(Ev.Event)
newEventList evs pt =
  case evs of
    ev::rest ->
      let 
        (c, threshold) = Ev.getInfo ev
        t = Ev.getCurrentPoints pt c
      in
        if t >= threshold then ev::rest
        else removeEvents (ev::rest) pt
    _ -> Debug.todo "event list should not be empty at the begining of the chap"

getFirstEvent : List(Ev.Event) -> Ev.Event
getFirstEvent evs =
  case evs of
    e::_ -> e
    [] -> Debug.todo "events should not be empty"

updateChapter : Model -> Int -> Model 
updateChapter m n =
  let 
    newChap = Ev.getChapter m.story (n)
    newChapNum = Ev.getChapterNumber newChap
  in 
    if newChapNum == 0 then initModel -- check if this works or breaks everything
    else
      let
        newEvList = newEventList (Ev.getEventList newChap) m.points
        newDiaList = newDialogueList newEvList
      in
        if n > m.chapterCount then 
          { story = m.story
          , chapterCount = newChapNum
          , newChapter = True
          , events = newEvList
          , dialogue = newDiaList
          , dec1 = setDec 1 (getFirstDialogue newDiaList)
          , dec2 = setDec 2 (getFirstDialogue newDiaList)
          , points = m.points
          -- update to the points for the chapter just finished
          , prevPoints = m.points 
          } 
        else -- redo
          { story = m.story
          , chapterCount = newChapNum
          , newChapter = True
          , events = newEvList
          , dialogue = newDiaList
          , dec1 = setDec 1 (getFirstDialogue newDiaList)
          , dec2 = setDec 2 (getFirstDialogue newDiaList)
          -- restore points from begining of chapter
          , points = m.prevPoints
          , prevPoints = m.prevPoints 
          } 

updateEvent : Model -> Model
updateEvent m =
  case m.events of 
    _::_::_ -> 
      let
          newEvList = removeEvents m.events m.points
          newDiaList = newDialogueList newEvList
      in
        if newEvList == [] then updateChapter m (m.chapterCount + 1)
        else
          { story = m.story
          , chapterCount = m.chapterCount
          , newChapter = False
          , events = newEvList
          , dialogue = newDiaList
          , dec1 = setDec 1 (getFirstDialogue newDiaList)
          , dec2 = setDec 2 (getFirstDialogue newDiaList)
          , points = m.points
          , prevPoints = m.prevPoints
          } 
    _::[] -> updateChapter m (m.chapterCount + 1)
    [] -> Debug.todo "issue because events should not be empty"

updateDialogue: Model -> Model
updateDialogue m = 
  case m.dialogue of
    _::_::_ ->
      let newDiaList = removeDialogue m.dialogue in
        { story = m.story
        , chapterCount = m.chapterCount
        , newChapter = False
        , events = m.events
        , dialogue = newDiaList
        , dec1 = setDec 1 (getFirstDialogue newDiaList)
        , dec2 = setDec 2 (getFirstDialogue newDiaList)
        , points = m.points
        , prevPoints = m.prevPoints
        } 
    _::[] -> updateEvent m
    [] -> Debug.todo "issue because dialogue should not be empty"  

updateDecision1: Model -> Model
updateDecision1 m =
  let
    oldCharacter = Tuple.first (Ev.getInfo (getFirstEvent m.events))
    updatePoints = Ev.incrementPoints (Ev.getPoints m.dec1) m.points oldCharacter
  in
    case m.dialogue of
    _::_::_ ->
      let newDiaList = removeDialogue m.dialogue in
        { story = m.story
        , chapterCount = m.chapterCount
        , newChapter = False
        , events = m.events
        , dialogue = newDiaList
        , dec1 = setDec 1 (getFirstDialogue newDiaList)
        , dec2 = setDec 2 (getFirstDialogue newDiaList)
        , points = updatePoints
        , prevPoints = m.prevPoints
        } 
    _ -> Debug.todo "issue because choosing a decision should not be the last piece of dialogue."  
  
updateDecision2: Model -> Model
updateDecision2 m =
  let
    oldCharacter = Tuple.first (Ev.getInfo (getFirstEvent m.events))
    updatePoints = Ev.incrementPoints (Ev.getPoints m.dec2) m.points oldCharacter
  in
    case m.dialogue of
    _::_::_ ->
      let newDiaList = removeDialogue m.dialogue in
        { story = m.story
        , chapterCount = m.chapterCount
        , newChapter = False
        , events = m.events
        , dialogue = newDiaList
        , dec1 = setDec 1 (getFirstDialogue newDiaList)
        , dec2 = setDec 2 (getFirstDialogue newDiaList)
        , points = updatePoints
        , prevPoints = m.prevPoints
        } 
    _ -> Debug.todo "issue because choosing a decision should not be the last piece of dialogue."  

-- UPDATE
update: Msg -> Model -> (Model, Cmd Msg)
update msg m = 
  case msg of 
    Noop -> (m, Cmd.none)
    Inc -> (updateDialogue m, Cmd.none)
    Dec1 -> (updateDecision1 m, Cmd.none)
    Dec2 -> (updateDecision2 m, Cmd.none)
    Redo -> 
      if m.chapterCount == 1 then 
        (updateChapter m 1, Cmd.none)
      else
        (updateChapter m m.chapterCount, Cmd.none)
    FileRequested -> (m, Select.file ["text/text"] FileSelected)
    FileSelected file -> 
      (m
      , Task.perform FileLoaded (File.toString file))
    FileLoaded content -> (fillModel (Ps.getStory content), Cmd.none)

-- SUBSCRIPTIONS
keyDecoder : Decode.Decoder String 
keyDecoder =
  Decode.field "key" Decode.string 

subscriptions : Model -> Sub Msg
subscriptions m =
  Browser.Events.onKeyDown (Decode.map (\key -> if key == "ArrowRight" && m.chapterCount /= 0 && Ev.isEmpty m.dec1 then Inc else Noop) keyDecoder)

-- VIEW HELPER FUNCTIONS

styles0 =
  [ ("position", "fixed")
  , ("top", "40%")
  , ("left", "50%")
  , ("transform", "translate(-50%, -50%)")
  , ("text-align", "center")
  ]
styles1 =
  [ ("position", "fixed")
  , ("top", "48%")
  , ("left", "50%")
  , ("transform", "translate(-50%, -50%)")
  , ("text-align", "center")
  ]
styles2 =
  [ ("position", "fixed")
  , ("top", "52%")
  , ("left", "50%")
  , ("transform", "translate(-50%, -50%)")
  , ("text-align", "center")
  ]
styles3 =
  [ ("position", "fixed")
  , ("top", "56%")
  , ("left", "50%")
  , ("transform", "translate(-50%, -50%)")
  , ("text-align", "center")
  ]
styles4 =
  [ ("position", "fixed")
  , ("top", "5%")
  , ("left", "90%")
  , ("transform", "translate(-50%, -50%)")
  , ("text-align", "center")
  ]

endView : Html Msg
endView = 
  let b = Html.button [ Events.onClick FileRequested ] [ Html.text "Load Story" ]
  in  Html.div [] [ Html.div (List.map (\(k, v) -> Attr.style k v) styles1) [b] ]

choiceView : Model -> Html Msg
choiceView m = 
  let
    dialogue = getFirstDialogue m.dialogue
    text = Html.text(Ev.getDialogueText dialogue)
    character = Html.text(Ev.getCharacter dialogue  ++ ": ")
    decision1 = Html.button [ Events.onClick Dec1 ] [ Html.text (Ev.getDecisionText m.dec1)]
    decision2 = Html.button [ Events.onClick Dec2 ] [ Html.text (Ev.getDecisionText m.dec2)]
    goback = Html.button [ Events.onClick Redo ] [Html.text "Redo chapter!"]
  in
    Html.div []
      [ Html.div (List.map (\(k, v) -> Attr.style k v) styles1) [character]
      , Html.div (List.map (\(k, v) -> Attr.style k v) styles2) [text]
      , Html.div (List.map (\(k, v) -> Attr.style k v) styles3) [decision1, decision2]
      , Html.div (List.map (\(k, v) -> Attr.style k v) styles4) [goback]
      ]

defaultView : Model -> Html Msg 
defaultView m =
  let
    dialogue = getFirstDialogue m.dialogue
    text = Html.text(Ev.getDialogueText dialogue ++ " > ")
    character = Html.text(Ev.getCharacter dialogue  ++ ": ")
    goback = Html.button [ Events.onClick Redo ] [Html.text "Redo chapter!"]
    title = Html.text("Chapter " ++ String.fromInt m.chapterCount)
  in
    if m.newChapter then
      Html.div []
        [ Html.div (List.map (\(k, v) -> Attr.style k v) styles1) [character]
        , Html.div (List.map (\(k, v) -> Attr.style k v) styles2) [text]
        , Html.div (List.map (\(k, v) -> Attr.style k v) styles4) [goback]
        , Html.div (List.map (\(k, v) -> Attr.style k v) styles0) [title]
        ]
    else 
      Html.div []
          [ Html.div (List.map (\(k, v) -> Attr.style k v) styles1) [character]
          , Html.div (List.map (\(k, v) -> Attr.style k v) styles2) [text]
          , Html.div (List.map (\(k, v) -> Attr.style k v) styles4) [goback]
          ]


-- VIEW
view : Model -> Html Msg
view m =
  if m.story == Ev.endStory then endView
  else
    if not (Ev.isEmpty m.dec1) then choiceView m
    else defaultView m