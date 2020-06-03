module Events exposing (..)

-- character name
type alias Character = String
-- amount of points needed to enter an event
type alias Threshold = Int
-- text for a whole slew of things
type alias Text = String
-- current points accumulated for characters
type alias Points =  (List ((Character, Int)))

-- response text and points associated with it
type Decision = E | Dec (Text, Int)

-- text and interactive response in gameplay
type Dialogue = Dia (Character, Text, (Decision, Decision))

-- group of interactions with 1 character
type Event = Info (Character, Threshold) (List (Dialogue))

-- all the events for a set time period (a round I guess? or a chapter?)
-- count is the chapter number i guess?
type Chapter = Chapter Int (List (Event))

-- all the events in the game
type Story = End | Story Points Int (List (Chapter))



------ TESTING THE DATA STRUCTURES ------
-- TEST STORY STRUCTURE 2

pt = [("Character1", 0), ("Character2", 0), ("Character3", 0)]

-- decisions:
d1 = Dec ("worth 5 points", 5)
d2 = Dec ("worth 0 points", 0)
-- chapter 1
  -- event 1 
c1e1d1 = Dia ("Character1", "This is the first dialogue in event 1 in chapter 1.", (E, E))
c1e1d2 = Dia ("Character1", "Make a decision. Choosing the 5 point one will let you enter c2e1.", (d1, d2))
c1e1d3 = Dia ("Character1", "I see you made a choice. Go to event c1e2.", (E, E))

c1e1 = Info ("Character1", 0) [c1e1d1, c1e1d2, c1e1d3]
  -- event 2
c1e2d1 = Dia ("Character2", "You made it to the second event in chapter 1. Make another choice.", (d1, d2))
c1e2d2 = Dia ("Character2", "Nice. If you chose the 5 point option, you should be able to enter c2e2.", (E, E))

c1e2 = Info ("Character2", 0) [c1e2d1, c1e2d2]

c1 = Chapter 1 [c1e1, c1e2]

-- chapter 2
  -- event 1 -> need to pass chap1 event 1
c2e1d1 = Dia ("Character1", "Oh cool you made it. Welcome to Chapter 2, event 1.", (E, E))
c2e1 = Info ("Character1", 5) [c2e1d1]
  -- event 2 -> need to pass chap1 event 2
c2e2d1 = Dia ("Character2", "Hey hey. Chapter 2 event 2.", (E, E))
c2e2 = Info ("Character2", 5) [c2e2d1]
  -- event 3 -> 0 point threshold
c2e3d1 = Dia ("Character3", "Wow even if you struck out with the other two you should still be here.", (E, E))
c2e3d2 = Dia ("Character3", "Oh don't give me that look. At least you're here now.", (E, E))
c2e3d3 = Dia ("Character3", "Welcome to chapter 2, event 3. And the end of the test.", (E, E))
c2e3 = Info ("Character3", 0) [c2e3d1, c2e3d2, c2e3d3]
c2 = Chapter 2 [c2e1, c2e2, c2e3]
st = Story pt 2 [c1, c2]

------ SOME BASIC HELPERS ------
-- STORY

-- given points and a list of chapters make a story
makeStory : Points -> List(Chapter) -> Story
makeStory p chap =
  Story p (List.length chap) chap

-- make a story end
endStory : Story
endStory = End

--given a story, return the numerically specified chapter
getChapter : Story -> Int -> Chapter
getChapter s x =
  case s of 
    End -> Chapter 0 []
    Story p c chap ->
      if x > c then Chapter 0 []
      else
        case chap of 
          [] -> Debug.todo "no chapters in story"
          (Chapter num events)::rest -> if num == x then (Chapter num events)
                                        else getChapter (Story p c rest) x

-- return the list of points in the story
getStoryPoints :  Story -> Points
getStoryPoints s =
  case s of 
    End -> []
    Story p _ _ -> p

-- CHAPTER

-- given a list of events and a chapter count, return a chapter
makeChapter : List (Event) -> Int -> Chapter
makeChapter ev num =
  Chapter num ev

-- given a chapter, return the number
getChapterNumber : Chapter -> Int
getChapterNumber c =
  case c of 
    Chapter num _ -> num

-- given a chapter, return the first event 
getEvent : Chapter -> Event
getEvent c =
  case c of 
    Chapter _ [] -> Info ("", 0) []
    Chapter _ (x::_) -> x

-- given a chapter, return a list of the events 
getEventList : Chapter -> List (Event)
getEventList c =
  case c of 
    Chapter _ [] -> []
    Chapter _ e -> e


-- EVENT

-- given the character and threshold info as well as a dialogue, make an event
createEvent : (String, Int) -> List (Dialogue) -> Event
createEvent i d =
  Info i d

--given Event, return identifying information 
getInfo : Event -> (Character, Threshold)
getInfo e =
  case e of
    Info info _ -> info

--given Event, return list of dialogue 
getDialogueList : Event -> List (Dialogue)
getDialogueList e =
  case e of 
    Info _ [] -> Debug.todo "no following dialogue in event"
    Info _ d -> d

-- given Event, return the first dialogue in the event
getDialogueInEvent : Event -> Dialogue
getDialogueInEvent e =
  case e of 
    Info _ [] -> Debug.todo "no dialogue in event"
    Info _ (x::_) -> x


-- DIALOGUE

-- create dialogue with character name and text
makeDialogueNoDecision : String -> String -> Dialogue
makeDialogueNoDecision char txt =
  Dia (char, txt, (E, E))

-- create dialogue as above and also create decisions with text and values
makeDialogueWDecision : String -> String -> String -> Int -> String -> Int -> Dialogue
makeDialogueWDecision n t dc1 p1 dc2 p2 =
  let dia1 = makeDecision dc1 p1
      dia2 = makeDecision dc2 p2 in
    Dia (n, t, (dia1, dia2))

--given dialogue, return the associated character
getCharacter : Dialogue -> Character
getCharacter d =
  case d of 
    Dia (char, _, _) -> char

--given dialogue, return the associated text
getDialogueText : Dialogue -> Text
getDialogueText d = 
  case d of 
    Dia (_, text, _) -> text

--given dialogue, return the associated decisions
getDecisions : Dialogue -> (Decision, Decision)
getDecisions d = 
  case d of 
    Dia (_, _, decisions) -> decisions


-- DECISION
makeDecision : String -> Int -> Decision
makeDecision s i =
  Dec (s, i)

--given a decision, return F if empty
empty : Decision
empty = E

-- check if a decision is empty
isEmpty : Decision -> Bool
isEmpty d = 
  d == E

--given a decision, return the associated points
getPoints : Decision -> Int
getPoints d =
  case d of 
    E -> Debug.todo "decision is empty and has no points"
    Dec (_, pts) -> pts

--given a decision, return the associated text
getDecisionText : Decision -> Text
getDecisionText d =
  case d of 
    E -> Debug.todo "decision is empty and has no text"
    Dec (text, _) -> text

-- POINTS

-- retrieve the points accumulated for a specific character 
getCurrentPoints : Points -> Character -> Int
getCurrentPoints p n =
  case p of
    (name, val)::rest ->
      if n == name then val
      else getCurrentPoints rest n
    _ -> Debug.todo "cannot find points for name not in list"

-- increase the points accumulated for a specific character
incrementPoints : Int -> Points -> Character -> Points
incrementPoints v p n =
  case p of
    (name, val)::rest ->
      if n == name then 
        (name, val+v)::rest
      else (name, val)::(incrementPoints v rest n)
    _ -> Debug.todo "name is not in the list for points"

-- helper function for testing whether points are updating correctly
pointToString : Points -> List (String)
pointToString p =
  case p of 
    [] -> []
    (name, val)::rest -> name::(String.fromInt val)::(pointToString rest)