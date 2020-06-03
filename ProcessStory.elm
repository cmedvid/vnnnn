module ProcessStory exposing (..)
import Events as Ev

-- these function names could have definitely been more consistent, but it 
  -- seemed too late once I had finished.

-- create the story from file contents
getStory : String -> Ev.Story
getStory file = 
  let split = String.split "\n---\n" file in
    processStory split

-- getStory helper
processStory : List (String) -> Ev.Story
processStory s =
  case s of 
    x::y::[] -> 
      let points = splitPoints x
          chaps = splitChapterSections y 
      in
        Ev.makeStory points chaps
    _ -> Debug.todo "issue with formatting for story"


-- POINT PROCESSING

-- get points from file information
splitPoints : String -> Ev.Points
splitPoints s =
  let names = String.split "|" s in
    processPoints names

-- splitpoints helper
processPoints : List (String) -> Ev.Points
processPoints s = 
  case s of 
    [] -> []
    x::rest -> (x, 0)::(processPoints rest)

-- CHAPTER LIST PROCESSING

-- get the chapter from file information
splitChapterSections : String -> List (Ev.Chapter) -- length of this return is number of chapters
splitChapterSections s =
  let chaps = String.split "\n\n\n\n" s in
    processChapterSections chaps 1

-- splitChapterSections helper
processChapterSections : List (String) -> Int -> List (Ev.Chapter)
processChapterSections s num =
-- this num is a counter for the chapters. its clunky but it works for now
  case s of 
    [] -> []
    chap::rest -> (Ev.makeChapter (splitEventSections chap) num)::(processChapterSections rest (num+1))

-- EVENT LIST PROCESSING

-- get the events from the file information
splitEventSections : String -> List (Ev.Event)
splitEventSections s =
  let events = String.split "\n\n\n" s in
    processAllEventsForChapter events

-- splitEventSections helper
processAllEventsForChapter : List (String) -> List (Ev.Event)
processAllEventsForChapter ss =
  case ss of 
    [] -> []
    s::rest -> (splitEventContents s)::(processAllEventsForChapter rest)

-- create the event
splitEventContents : String -> Ev.Event
splitEventContents s =
  let strings = String.split "\n" s in
    case strings of 
      x::rest -> 
        Ev.createEvent (processEvent x) (processDialogueInEvent rest)
        -- first one is event information (first and second are name and pt)
        -- rest are dialogue decision combos
            -- in dialogue, first and second are name and text
                            -- third, fourth and fifth, sixth are decisions
        -- stuff is separated by |
      _ -> Debug.todo "this is a problem in splitEventContents"


-- EVENT INFO PROCESSING

-- form the event information
processEvent : String -> (String, Int)
processEvent s = 
  let info = String.split "|" s in
    case info of
      x::y::[] ->
        let threshold = Maybe.withDefault -100 (String.toInt y) in
          if threshold == -100 then Debug.todo "point in event line wasn't an int"
          else (x, threshold)
      _ -> Debug.todo "event strings are empty"


-- DIALOGUE PROCESSING

-- get the dialogue in the event from the file
processDialogueInEvent : List (String) -> List (Ev.Dialogue)
processDialogueInEvent ss =
  case ss of 
    [] -> []
    s::rest -> (splitDialogue s)::(processDialogueInEvent rest)

-- processDialogueInEvent helper
splitDialogue : String -> Ev.Dialogue
splitDialogue s =
  -- length is 4 or 6 (6 if decisions are involved)
  let dia = String.split "|" s in
    case dia of
      name::text::_::_::[] -> Ev.makeDialogueNoDecision name text
      name::text::dia1::pt1::dia2::pt2::[] -> 
        let point1 = Maybe.withDefault -100 (String.toInt pt1)
            point2 = Maybe.withDefault -100 (String.toInt pt2) in
          if point1 == -100 || point2 == -100 then 
            Debug.todo "point in dialogue string is not int"
          else Ev.makeDialogueWDecision name text dia1 point1 dia2 point2
      _ -> Debug.todo "dialogue sting not in correct format"
