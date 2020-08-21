module PredefinedFunctions (
    delay,
    br,
    convert,
    generate,
    fgenerate,
    welcome,
    introduction,
    choose_name,
    invalid_move,
    move_explanation,
    theEnd,
    d,
    lvl1,
    lvl2,
    lvl3,
    lvl4,
    dashes,
    when_ready,
    location_intro,
    intro1,
    intro2,
    intro3,
    intro4,
    beginner_inventory,
    room1,
    room2,
    room3,
    room4,
    l1r1intro,
    l2r1intro,
    l2r2intro,
    l3r1intro,
    l3r2intro,
    l4r1intro,
    l4r2intro,
    l4r3intro,
    guide1,
    guide2,
    gl,
    input
    ) where

import Control.Concurrent -- importing for 'threadDelay' function purpose
import Control.Monad -- importing for 'forM' function purpose


-- time delay for output; by t*10000 i made function more realisticly presenting practical portion of 1 second, making it into 100 parts. 100 would mean 1s, 10 - 0.1s, 1 - 0.01s 
delay :: Int -> IO ()
delay t = threadDelay (t*10000)

-- when new line needed, i generate it with 'putStrLn ""'. to get multiple lines at one simple call - br x (of course it gets splitted into multiple inside), x is given as an argument and being used for recalling recursively same function. At the same time, x stands for how many required. 'br' name I made in relation to other language - HTML, standing for tag '<br>' break line.
br :: Int -> IO ()
br 1 = putStrLn ""
br x = do
    putStrLn ""
    br (x-1)

-- made it for user input so it prints 'input: ' to indicate that next step requires input from a user - more user friendly feedback of actions.
input :: IO (String)
input = do
    putStr "input: " -- textual indication of input
    userInput <- getLine -- taking input from a user, then assigning to userInput
    return (userInput) --returning user input for function's result

-- converting String into list of Strings (each letter becomes individual String); I tried with [Char] concept but failed to make it work, and did not experiment further because String concept served just fine, even its not correct way.
convert :: String -> [String]
convert text = map return text

-- here is the function where running text animation is generated, so and I called it 'generate'. function 'forM' takes
-- a list of Strings and passes each element from a list to second argument where using lambda have created function to
-- pass that individual String (letter) to 'do' block and execute the printing of single letter, then delaying 0.04s 
-- till next element from the list to be executed. This way it looks like a running text on the screen, with such small time gaps. 
generate x = forM x (\text -> do  
    putStr text
    delay 4 -- = 0.04s
    )

-- used for textual output in fightStage function exclusively (to make entire fight action faster, since there is a lot of repeatible content)
fgenerate x = forM x (\text -> do  
    putStr text
    delay 2 -- = 0.02s
    )

-- used only for room & level indication (with dash symbols) with faster tick rate
d x = forM x (\dash -> do  
    putStr dash
    delay 1 -- = 0.01s
    )

dashes  = convert "- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -\n\n"
lvl1    = convert " - - - - - - - - - - - - Vault of War (Level 1) - - - - - - - - - - - -\n\n"
lvl2    = convert " - - - - - - - - - - Catacomb of Famine (Level 2) - - - - - - - - - - -\n\n"
lvl3    = convert "- - - - - - - - - - - Pit of Pestilence (Level 3) - - - - - - - - - - -\n\n"
lvl4    = convert " - - - - - - - - - - Sepulchre of Death (Level 4) - - - - - - - - - - -\n\n"
room1   = convert "- - - - - - - - - - - - - - - - Room 1 - - - - - - - - - - - - - - - - -\n\n"
room2   = convert "- - - - - - - - - - - - - - - - Room 2 - - - - - - - - - - - - - - - - -\n\n"
room3   = convert "- - - - - - - - - - - - - - - - Room 3 - - - - - - - - - - - - - - - - -\n\n"
room4   = convert "- - - - - - - - - - - - - - - - Room 4 - - - - - - - - - - - - - - - - -\n\n"


-- below are prepared [String] type text lines, which will be generated (make running text) on demand by calling (example) 'generate welcome'

welcome = convert "Welcome to Adventure Game! (prototype1)\n\n"  

introduction = convert "[RANDOM TEXT] Aenean vel leo eget lorem vulputate condimentum.\nVivamus sit amet turpis dictum, lacinia leo non, luctus urna.\nNullam faucibus magna ut laoreet lobortis.\nNam in massa in lorem accumsan auctor ornare vel dui.\nCurabitur porttitor velit a metus fringilla interdum.\nPhasellus ut arcu et velit aliquam tincidunt.\n\n"

introduction1 = convert "abc\n\n"

choose_name = convert "Please choose your character name:\n\n"

when_ready = convert "Comeback when you are ready and type 'yes' to begin.\n\n"

location_intro = convert "You squeeze through the hole and find a ladder a few feet down leading\ninto the Stronghold of Security Dangeon...\n\n"

guide1 = convert "You are starting the game with such data:\n\nhealth points: 10\nhealing points: 2\nattack damage points: 2\n\n"

guide2 = convert "When you are in a fighting stage, you will be prompted to input\nsomething. There are 2 available commands:\n\n'atk' - to attack\n'hp' - to heal yourself\n\n"

gl = convert "Good luck!\n\n"

intro1 = convert "Level 1 consists of 1 room.\n\n"
intro2 = convert "Level 2 consists of 2 rooms.\n\n"
intro3 = convert "Level 3 consists of 2 rooms.\n\n"
intro4 = convert "Level 4 consists of 3 rooms.\n\n"

l1r1intro = convert "Suddenly you are attacked by a Small Rat (LVL 1)...\n\n"
l2r1intro = convert "Suddenly you are attacked by a Goblin (LVL 5)...\n\n"
l2r2intro = convert "Suddenly you are attacked by a Wolf (LVL 11)...\n\n"
l3r1intro = convert "Suddenly you are attacked by a Minotaur (LVL 19)...\n\n"
l3r2intro = convert "Suddenly you are attacked by a Giant Spider (LVL 26)...\n\n"
l4r1intro = convert "Suddenly you are attacked by a Scorpion (LVL 35)...\n\n"
l4r2intro = convert "Suddenly you are attacked by a Skeleton (LVL 41)...\n\n"
l4r3intro = convert "Suddenly you are attacked by a Shade (LVL 57)...\n\n"

theEnd = convert "- - - - - - - - - - - - THE END - - - - - - - - - - - -\n"



-- below are not used in current game version

beginner_inventory = convert "Your starting inventory has few items to begin your journey with:\n\n"

invalid_move = convert "!!! Invalid direction given.\n"

move_explanation = convert "Please type a keyword for the direction followed by a number of steps you wish to move.\n\nAvailable direction keywords:\n\n'north x' - move to NORTH x steps (max 3)\n\n'east x' - move to EAST x steps (max 3)\n\n'south x' - move to SOUTH x steps (max 3)\n\n'west x' - move to WEST x steps (max 3)\n"

