import PredefinedFunctions -- importing other functions I have written in other file - Predefined Functions.hs

import Data.Char -- importing functions collection which includes 'toUpper' function used in this project

-- IF YOU WISH SO, IGNORE COMMENTED CODE LINES (left them for this game future development)

-- about commented code block below: not used; this code was created for different version of game,
-- main idea of the game was changed; kept for reference purposes of possible future ideas

-- direction "north 1" = do                
--     putStrLn "You moved North 1 step"
-- direction "north 2" = do
--     putStrLn "You moved North 2 steps"
-- direction "north 3" = do
--     putStrLn "You moved North 3 steps"
-- direction "east 1" = do
--     putStrLn "You moved East 1 step"
-- direction "east 2" = do
--     putStrLn "You moved East 2 steps"
-- direction "east 3" = do
--     putStrLn "You moved East 3 steps"
-- direction "south 1" = do
--     putStrLn "You moved South 1 step"
-- direction "south 2" = do
--     putStrLn "You moved South 2 steps"
-- direction "south 3" = do
--     putStrLn "You moved South 3 steps"
-- direction "west 1" = do
--     putStrLn "You moved West 1 step"
-- direction "west 2" = do
--     putStrLn "You moved West 2 steps"
-- direction "west 3" = do
--     putStrLn "You moved West 3 steps"
-- direction _ = do
--     generate invalid_move
--     br 1
--     generate move_explanation
--     actionX <- getLine
--     direction actionX


-- default_inventory :: [(String, String, Int)] -- type, name, value
-- default_inventory = [("gold", "coins", 250), ("health", "small healing potion", 2), ("weapon", "bronze sword", 2), ("key", "chest key", 0)]

-- goldDATA :: [(String, String, Int)] -> (String, String, Int) -> (String, String, Int)
-- goldDATA [] y = ("default", "default", 0)
-- goldDATA ((a,b,c):z) (d,e,f)    | a == d    = (a,b,c)
--                                 | otherwise = goldDATA z (d,e,f)

-- gold :: [(String, String, Int)] -> (String, String, Int)
-- gold x = goldDATA x ("gold", "default", 0)

-- first :: (a, b, c) -> a  
-- first (x, _, _) = x  
  
-- second :: (a, b, c) -> b  
-- second (_, y, _) = y  
  
-- third :: (a, b, c) -> c  
-- third (_, _, z) = z 

-- updated gold = gold+15


-- function left to exist for testing purpose
-- test1 :: IO (Int) 
-- test1 = do
--     br 1
--     let gold_x = (updated 21)
--     return (gold_x)

-- function left to exist for testing purpose
-- test2 :: Int -> IO (Int) 
-- test2 gold = do
--     br 1
--     let g = (updated gold)
--     return (g)


-- STARTS FROM HERE --

-- 'check' function is used in 'intro' function to check if user has inputed 'yes' in order to proceed, otherwise executes 'do' block 
-- where asks user to input answer again, then function calls itself (using recursion) to check answer again
check "yes" = putStr ""
check _ = do
    generate when_ready -- prints text (please check PredifinedFunctions.hs file)
    x <- input -- getting input from user and asigning to x (please check PredifinedFunctions.hs file)
    br 1 -- break line command (please check PredifinedFunctions.hs file)
    check x -- calling itself (recursion)

-- intro covers the introdution of the game
intro = do
    br 1
    generate welcome 
    delay 100 -- = 1s (please check PredifinedFunctions.hs file)
    generate introduction
    delay 100
    generate choose_name
    name <- input
    br 1
    --let aNAME = map toUpper name -- left in case of reusing character name in future functions
    generate (convert ("Hi " ++ (map toUpper name) ++  ", are you ready for the challenge to begin?\n\nType 'yes' if you are.\n\n")) -- in 'generate' function (running text animation) 'map' with 'toUpper' functions been used to make character name in capital letters (please check PredifinedFunctions.hs file)
    x <- input
    br 1
    check x 
    --gold1 <- test1 --this commented block left for testing purposes
    --gold2 <- test2 gold1
    --print gold2
    br 1

-- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -  
-- fighting scenario function
-- taking 8 arguments and returning Numeric value (of type (custom name) _0or1)
-- arguments: x = player health points, y = enemy health points, npc = enemy name, atkP = attack damage points of player, atkNPC = attack damage points of enemy, hp = healing points, r = message after victory, l = player maximum health points
fightStage :: Num _0or1 => Int -> Int -> String -> Int -> Int -> Int -> String -> Int -> IO _0or1 
fightStage x y npc atkP atkNPC hp r l = do
    if x > 0 && y > 0 then do -- checks if player and enemy are alive
        fgenerate (convert ("You (HP:" ++ (show x :: String) ++ ") VS " ++ npc ++ " (HP:" ++ (show y :: String) ++ ")")) -- 'fgenerate' same as 'generate' but with faster animation (please check PredifinedFunctions.hs file); (show y :: String) is used to convert Int type value to String, because fgenerate requires String type
        br 2
        let m = x-atkNPC -- this action is the attack from enemy; x (player health points) - atkNPC (attack damage points of enemy)
        delay 60 -- = 0.6s
        fgenerate (convert (npc ++ " dealt " ++ (show atkNPC :: String) ++ " damage points to you\n")) -- message (output) to user about damage dealt by enemy
        if m > 0 && y > 0 then do -- checks again if player and enemy are alive (more important is player, since he received damage) after player health points have been modified after received attack
            fgenerate (convert ("You (HP:" ++ (show m :: String) ++ ") VS " ++ npc ++ " (HP:" ++ (show y :: String) ++ ")"))
            br 2
            your_action <- input -- options: 1) "atk" - attack 2) "hp" - restore health points; user's turn to attack or heal himself in one tick action
            if your_action == "atk" then do -- checks if attack was chosen
                if (y-atkP) > 0 then do -- checks if after damage dealt to enemy, the health points of enemy are still above 0 (alive)
                    fgenerate (convert ("You dealt " ++ (show atkP :: String) ++ " damage points to " ++ npc ++ "\n"))
                    fightStage m (y-atkP) npc atkP atkNPC hp r l -- calling itself (recursion) with enemy's health lowered (y-atkP)
                else do -- all other cases (keeping in mind that enemy health is 0 or less after received damage); the actions are same as first 'if' but with added text output saying that enemy was killed
                    fgenerate (convert ("You dealt " ++ (show atkP :: String) ++ " damage points to " ++ npc ++ " and killed\n"))
                    fightStage m (y-atkP) npc atkP atkNPC hp r l
            else if your_action == "hp" then do -- checks if healing was chosen
                if (m+hp) <= l then do -- checks if healing had been used, would it exceed or not the limit (player maximum health points (l)), this case - if not
                    fgenerate (convert "You consumed healing potion and increased your health points\n")
                    fightStage (m+hp) y npc atkP atkNPC hp r l -- increases player's health points (m+hp)
                -- else if (m+1) <= 10 then do
                --     generate (convert "You consumed healing potion and increased your health points\n")
                --     fightStage (m+hp) y npc atkP atkNPC hp r l
                else do -- if exceeds limit, no argument modification and message will be prompted
                    fgenerate (convert "The healing potion dose is too big to consume right now\n")
                    fightStage m y npc atkP atkNPC hp r l
            else do fgenerate (convert "Wrong input\n") -- if player typed something what is not matching predifined commands, message of wrong input will be prompted and user will loose a tick, recalling fightStage with same argument values
                    fightStage m y npc atkP atkNPC hp r l
        else do -- if player after received damage is dead (health points equal ir less than 0)
            br 1
            generate (convert "You have been killed...\n")
            return (0) -- message for outter function (lXroomX) of fight outcome, in this case - defeat
    else do -- if player was last to blast hit to enemy and enemy's health was <=0, then player is determined as winner
        br 1
        generate (convert ("Congratulations! " ++ r ++ "\n\n")) -- special message (stats improvements)
        return (1) -- message for outter function (lXroomX) of fight outcome, in this case - victory

-- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -   
-- LEVEL 1, ROOM 1
l1room1 x = do -- x argument is an inventory [(String, String, Int)]; its not being used in current game version
    delay 60 -- = 0.6s
    generate l1r1intro
    result <- fightStage 10 3 "a Small Rat" 2 1 2 "Healing has been upgraded to 3 health points per dose" 10 -- assigning returned value (0 or 1) from return statement in fightStage function to a result; passing 8 arguments (Int & String types) for fightStage function
    if result == 0 then l1room1 x -- start over fightStage; x does not do anything for current game version, just recalling function l1room1, like recursion, but with 'if' statement
    else return ("passed") -- return value is not used anywhere currently, its for testing and future plans; if-else allows to continue to next room/level

-- LEVEL 1
level1 x = do
    --let amount = third (gold x) -- please ignore
    --print amount -- please ignore
    generate intro1
    d room1 
    l1room1 x
    --print (gold x) -- please ignore
    br 2
    let updatedInventory = x -- not used practically yet, for future plans
    return (updatedInventory)

-- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
-- LEVEL 2, ROOM 1
l2room1 x = do
    delay 60
    generate l2r1intro
    result <- fightStage 10 5 "a Goblin" 2 2 3 "Now your attacks can deal 3 damage points" 10
    if result == 0 then l2room1 x
    else return ("passed")

-- LEVEL 2, ROOM 2
l2room2 x = do
    delay 60
    generate l2r2intro
    result <- fightStage 10 9 "a Wolf" 3 2 3 "Healing has been upgraded to 4 health points per dose" 10
    if result == 0 then l2room2 x
    else return ("passed")

-- LEVEL 2
level2 x = do
    generate intro2
    d room1 
    l2room1 x
    d room2
    l2room2 x
    br 2
    let updatedInventory = x
    return (updatedInventory)

-- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
-- LEVEL 3, ROOM 1
l3room1 x = do
    delay 60
    generate l3r1intro
    result <- fightStage 10 10 "a Minotaur" 3 3 4 "Healing has been upgraded to 5 health points per dose\n& Now your attacks can deal 4 damage points" 10
    if result == 0 then l3room1 x
    else return ("passed")

-- LEVEL 3, ROOM 2
l3room2 x = do
    delay 60
    generate l3r2intro
    result <- fightStage 10 14 "a Giant Spider" 4 3 5 "Your health has been increased to 12 points\n& Healing has been upgraded to 6 health points per dose" 10
    if result == 0 then l3room2 x
    else return ("passed")

-- LEVEL 3
level3 x = do
    generate intro3
    d room1 
    l3room1 x
    d room2
    l3room2 x
    br 2
    let updatedInventory = x
    return (updatedInventory)

-- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
-- LEVEL 4, ROOM 1
l4room1 x = do
    delay 60
    generate l4r1intro
    result <- fightStage 12 20 "a Scorpion" 4 4 6 "Your health has been increased to 16 points\n& Healing has been upgraded to 7 health points per dose\n& Now your attacks can deal 5 damage points" 12
    if result == 0 then l4room1 x
    else return ("passed")

-- LEVEL 4, ROOM 2
l4room2 x = do
    delay 60
    generate l4r2intro
    result <- fightStage 16 22 "a Skeleton" 5 6 7 "Now your attacks can deal 6 damage points" 16
    if result == 0 then l4room2 x
    else return ("passed")

-- LEVEL 4, ROOM 3
l4room3 x = do
    delay 60
    generate l4r3intro
    result <- fightStage 16 32 "a Shade" 6 5 7 "You have finished the Adventure Game (prototype1)" 16
    if result == 0 then l4room3 x
    else return ("passed")

-- LEVEL 4
level4 x = do
    generate intro4
    d room1 
    l4room1 x
    d room2
    l4room2 x
    d room3
    l4room3 x
    br 2

-- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

-- MAIN 'DO' BLOCK - calls functions written above and written in PredifinedFunctions.hs file
main = do  
    br 3
    d dashes
    intro
    d dashes
    br 1
    generate location_intro
    delay 60
    generate guide1 -- stating player's basic statistics
    delay 60
    generate guide2 -- explains commands
    delay 40
    generate gl 
    delay 150
    br 1
    let part1 = 0 -- partX would have carried steps value across all levels. I had a quick last minute idea, for current game version, of counting how many steps player did. Goal would hav been to make fewest steps possible by finishing game. But, failed to make it work.
    d lvl1
    part2 <- level1 part1
    d lvl2
    part3 <- level2 part2
    d lvl3
    part4 <- level3 part3
    d lvl4
    level4 part3
    generate theEnd
    

-- saved for future plans
    -- let inventory0 = default_inventory
    -- d lvl1
    -- inventory1 <- level1 inventory0
    -- d lvl2
    -- inventory2 <- level2 inventory1
    -- d lvl3
    -- inventory3 <- level3 inventory2
    -- d lvl4
    -- level4 inventory3