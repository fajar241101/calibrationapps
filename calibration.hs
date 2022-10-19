
-- Import Dependency Library
import Data.List
import Data.Time
import Data.Time.Format
import qualified Data.Char as Foo
import Data.Time.Clock
import Data.Time.Calendar
import System.Console.ANSI

-- Data Type Setup Configuration
type InstrumentDatabase = (EquipmentName, EquType,EquChar,CalibType)
type ObsLog = (EquipmentName,EquChar,SystemDate)
type ObservationRegistered = (EquipmentName,SystemDate,DateReg)
type ObservationRegisteredByName = (EquipmentName,EquChar,CalProb,CalibType,EquType)
type EquRecommender = (EquipmentName,EquType,CalProb)
type EquipmentName = String
type EquType = String
type EquChar = [String]
type EquChar2 = String
type CalibType = String
type SystemDate = String
type DateReg = String
type CalProb = Integer

-- Parse Date Data
current_date :: Day
current_date = parseDay "2022-10-12"

-- List Function for Instrument/ Equipment Database Registration
list_instrumentdb :: [InstrumentDatabase]
list_instrumentdb = []
list_obslog :: [ObsLog]
list_obslog = []
list_instrumentdb_registered :: [ObservationRegistered]
list_instrumentdb_registered = []


-- Check and Count how many values in two lists are equal
checkIfContains :: [String] -> String -> Integer
checkIfContains x y = case elemIndex y x of
                    Nothing -> 0
                    Just n  -> 1

checkIfContainsList :: [String] -> [String] -> Integer
checkIfContainsList _ [] = 0
checkIfContainsList [] _ = 0
checkIfContainsList x (y:ys) = if (checkIfContains x y >= 1) then 1 + checkIfContainsList x ys else checkIfContainsList x ys


-- Makes comparisons between the Calibration Observation Log Reports and Instrument/ Equipment Database to discover "Recalibration" as part of recommender 
-- (in the search by name -> Equipment Category | returns whether the Equipment/ Instrument required Recalibration or considered "Pass" or "Defective" )

compareEquCharAll :: ObsLog -> [InstrumentDatabase] -> [EquRecommender]
compareEquCharAll _ [] = []
compareEquCharAll (xn,xd,xp) ((yn,yc,yd,yp):ys) = ((xn::EquipmentName,yc::EquType,checkIfContainsList xd yd::CalProb)::EquRecommender):compareEquCharAll (xn,xd,xp) ys

compareEquCharAllByName :: String -> [ObsLog] -> [InstrumentDatabase] -> [EquRecommender]
compareEquCharAllByName name [] [] = []
compareEquCharAllByName name [] _ = []
compareEquCharAllByName name ((xn,xd,xp):xs) y = if (xn == name) then (compareEquCharAll (xn,xd,xp) y) ++ (compareEquCharAllByName name xs y) else (compareEquCharAllByName name xs y) 

max_list :: [EquRecommender] -> String
max_list [(n,v,c)] = "Instrument/Equipment: " ++ n ++ " -> Asset Category: " ++ v 
max_list ((x1,v1,c1):(x2,v2,c2):xs) = if (c1 > c2) then max_list ((x1,v1,c1):xs) else max_list((x2,v2,c2):xs)


-- Makes comparisons between the Calibration Observation Log Reports and Instrument/ Equipment Database to discover "Recalibration" as part of recommender  
-- of Instrument/ Equipment  (returns a list of Instrument/ Equipment  and Instrument/ Equipment Observation Characteristic )

calibTypeCompareEquCharAll :: ObsLog -> [InstrumentDatabase] -> [ObservationRegisteredByName]
calibTypeCompareEquCharAll _ [] = []
calibTypeCompareEquCharAll (xn,xd,xp) ((yn,yc,yd,yp):ys) = ((xn::EquipmentName,xd::EquChar,checkIfContainsList xd yd::CalProb, yp::CalibType, yc::EquType)::ObservationRegisteredByName):calibTypeCompareEquCharAll (xn,xd,xp) ys

calibTypecompareEquCharAllByName :: String -> [ObsLog] -> [InstrumentDatabase] -> [ObservationRegisteredByName]
calibTypecompareEquCharAllByName name [] [] = []
calibTypecompareEquCharAllByName name [] _ = []
calibTypecompareEquCharAllByName name _ [] = []
calibTypecompareEquCharAllByName name ((xn,xd,xp):xs) y = if (xn == name) then (calibTypeCompareEquCharAll (xn,xd,xp) y) ++ (calibTypecompareEquCharAllByName name xs y) else (calibTypecompareEquCharAllByName name xs y) 

calibTypeMax_list :: [ObservationRegisteredByName] -> String
calibTypeMax_list [(n,s,c,q,v)] = q
calibTypeMax_list ((n1,s1,c1,q1,v1):(n2,s2,c2,q2,v2):xs) = if (c1 > c2) then calibTypeMax_list ((n1,s1,c1,q1,v1):xs) else calibTypeMax_list((n2,s2,c2,q2,v2):xs)

circularproductMax_listGraph :: [ObservationRegisteredByName] -> String
circularproductMax_listGraph [(n,s,c,q,v)] = "\n Instrument/Equipment:  " ++ n  ++ "\n -- Asset Type: " ++ v ++ "\n -- Calibration Recommendation: " ++ q ++ ";" ++ "\n "
circularproductMax_listGraph ((n1,s1,c1,q1,v1):(n2,s2,c2,q2,v2):xs) = if (c1 > c2) then circularproductMax_listGraph ((n1,s1,c1,q1,v1):xs) else circularproductMax_listGraph((n2,s2,c2,q2,v2):xs)


-- Function That converts DATA-STRING to DATA-DAY
parseDay :: String -> Day
parseDay s = parseTimeOrError True defaultTimeLocale "%Y-%m-%d" s


-- Functions to Registered Instrument/Equipment in Database
insert_instrumentdb :: IO()
insert_instrumentdb = do clearScreen
                         now <- getCurrentTime
                         let (year, month, day) = toGregorian $ utctDay now
                         putStr "************************************************ \n" 
                         putStr "** Instrument/Equipment Database Registration ** \n"
                         putStr ("**      Current System Date: " ++ show day ++ "-" ++ show month ++ "-" ++ show year ++ "       **\n")
                         putStr "************************************************ \n" 
                         putStr "Instrument/ Equipment Name: (xxxx-xxxx-xxxx) \n"
                         n <- getLine
                         putStr "Asset Category: (1-Instrument 2-Equipment) \n"
                         c <- getLine
                         putStr "Calibration Observation Characteristic: (xxxxx,xxxxx,....,xxxx)  \n"
                         d <- getLine
                         putStr "Calibration Recommendation: (1-Pass-Calibration, 2-Recalibration , 3-Defective) \n"
                         p <- getLine
                         appendFile "instrumentdb.txt" (n ++ "\t" ++ c ++ "\t" ++ d ++ "\t" ++ p ++ "\n")
                         putStr "Add another Instrument/ Equipment? (y/n): \n"
                         resp <- getLine
                         if (resp=="y" || resp=="Y") then insert_instrumentdb else return()


-- Functions to Registered Instrument/Equipment Calibration Observation Log-Report
insert_obslog :: IO()
insert_obslog = do clearScreen
                   now <- getCurrentTime
                   let (year, month, day) = toGregorian $ utctDay now 
                   putStr "************************************************************** \n" 
                   putStr "**  Instrument/Equipment Calibration Observation Log-Report ** \n"
                   putStr ("**             Current System Date: " ++ show day ++ "-" ++ show month ++ "-" ++ show year ++ "              **\n")
                   putStr "************************************************************** \n" 
                   putStr "Instrument/ Equipment Name: (xxxx-xxxx-xxxx) \n"
                   n <- getLine
                   putStr "Calibration Observation Characteristic: (xxxxx,xxxxx,....,xxxx) \n"
                   d <- getLine
                   putStr "Calibration Date (yyyy-mm-dd): \n"
                   p <- getLine
                   print("Calibration Observation Log-Report Processed... \n")
                   appendFile "calibrationobservationlog.txt" (n ++ "\t" ++ d ++ "\t" ++ p ++ "\n")
                   print("Calibration Observation Log-Report Saved Success! \n")
                   putStr "Add Other Calibration Observation Log-Report ? : \n"
                   resp <- getLine
                   if (resp=="y" || resp=="Y") then insert_obslog else return()


-- Functions that load the table of Instrument/Equipment Calibration Observation Log-Report  (from TXT files )
loadTab_obslog = do s <-readFile "calibrationobservationlog.txt"
                    return (gerlist_obslog (map words (lines s)))

gerlist_obslog [] = []
gerlist_obslog ([n,d,p]:xs) = ((n::EquipmentName,split d::EquChar,p::SystemDate)::ObsLog):(gerlist_obslog xs)

print_lst_obslog [] =""
print_lst_obslog ((n,d,p):xs) = "Instrument/Equipment: " ++ n ++ "-> Observation Logs = " ++ print_observation_each d ++ "-> Calibration Date: " ++ p ++ "\n"  ++ (print_lst_obslog xs) 

gerlist_calibtype [] = []
gerlist_calibtype ([n,d,p]:xs) = ((n::EquipmentName,split d::EquChar,p::SystemDate)::ObsLog):(gerlist_obslog xs)

loadTab_instrumentdb = do s <-readFile "instrumentdb.txt"
                          return (gerlist_instrumentdatabase (map words (lines s)))

gerlist_instrumentdatabase [] = []
gerlist_instrumentdatabase ([n,c,d,p]:xs) = ((n::EquipmentName,c::EquType,split d::EquChar,p::CalibType)::InstrumentDatabase):(gerlist_instrumentdatabase xs)


-- Function that loads the table of Instrument/Equipment Calibration Recommendation -> Based on the table of Instrument/ Equipment Main Database.
loadTab_caltype p d = do return (isCalibType p d)

isCalibType :: [ObsLog] -> [InstrumentDatabase] -> [ObservationRegistered]
isCalibType [] [] = []
isCalibType _ [] = []
isCalibType [] _ = []
isCalibType ((xn,xd,xp):xs) y = if (calibTypeMax_list (calibTypecompareEquCharAllByName xn ((xn,xd,xp):xs) y) == "2-Recalibration") then return ((xn::EquipmentName,xp::SystemDate,showGregorian (addDays 40 (parseDay xp))::DateReg)::ObservationRegistered) ++ isCalibType xs y else isCalibType xs y


-- Functions to Print or Display Instrument/ Equipment Database
print_lst_instdb [] =""
print_lst_instdb ((n,c,d,p):xs) = "Instrument/Equipment: " ++ n ++ "-> Type = " ++ c ++ "-> Observation Characteristic: " ++ print_observation_each d ++ "-> Recommendation: " ++ p ++ "\n" ++ (print_lst_instdb xs)

print_lst_calibtype date [] =""
print_lst_calibtype date ((n,c,e):xs) = if ( (diffDays date (parseDay e)) < 0) then "Instrument/Equipment = " ++ n ++ ", Date Registered = " ++ c ++ "\n" ++ (print_lst_calibtype date xs) else (print_lst_calibtype date xs)

print_lst_calibtype_newDate_still date [] =""
print_lst_calibtype_newDate_still date ((n,c,e):xs) = if ( (diffDays date (parseDay e)) <= 0) then "Instrument/Equipment = " ++ n ++ ", Date Registered = " ++ c ++ "\n" ++ (print_lst_calibtype_newDate_still date xs) else (print_lst_calibtype_newDate_still date xs)

print_lst_calibtype_newDate_out date [] =""
print_lst_calibtype_newDate_out date ((n,c,e):xs) = if ( (diffDays date (parseDay e)) >= 0) then "Instrument/Equipment = " ++ n ++ ", Date Registered = " ++ c ++ "\n" ++ (print_lst_calibtype_newDate_out date xs) else (print_lst_calibtype_newDate_out date xs)


-- Function that counts the number of Instrument/ Equipment which required "Recalibration"
count_calibType [] = 0
count_calibType (x:xs) = 1 + count_calibType xs


-- Function that converts a STRING to [STRING] (entering Observation Characteristic separated by commas in a LIST)
split str = case break (==',') str of
                (a, ',':b) -> a : split b
                (a, "")    -> [a]

print_observation_each [] = ""
print_observation_each (x:xs) = x ++ ", " ++ print_observation_each xs


-- Function that loads the TXT with the current system date
load_date = do s <-readFile "datesystem.txt"
               return (parseDay s)


-- Function that generates a graph of ( Instrument/Equipment Name -> Asset Category -> Calibration Recommendation )
graph :: [ObsLog] -> [InstrumentDatabase] -> String
graph [] [] = ""
graph _ [] = ""
graph [] _ = ""
graph ((xn,xd,xp):xs) y = circularproductMax_listGraph (calibTypecompareEquCharAllByName xn ((xn,xd,xp):xs) y) ++ graph xs y


-- Main function that contains all the actions of the program
main :: IO()
main = do list_obslog <- loadTab_obslog
          list_instrumentdb <- loadTab_instrumentdb
          current_date <- load_date
          now <- getCurrentTime
          let (year, month, day) = toGregorian $ utctDay now
          list_instrumentdb_registered <- loadTab_caltype list_obslog list_instrumentdb         
          putStrLn(" ")
          putStrLn "*****************************************************************"     
          putStrLn "**    INSTRUMENT CALIBRATION RECOMMENDER NOTIFICATION SYSTEM   **"
          putStrLn "**          AI Data Analytics - Blockchain Technology          **"
          putStrLn "**             Developed By: MF Fajar Ramadhan SH              **"
          putStrLn "**         Electronic & Instrumentation - MIPA UGM 2020        **"
          putStr ("**               Current System Date: " ++ show day ++ "-" ++ show month ++ "-" ++ show year ++ "               **\n")
          putStrLn "*****************************************************************"
          putStr "[1] Add Instrument/ Equipment Database \n"
          putStr "[2] Add Instrument/Equipment Calibration Observation Log-Report \n"
          putStr "[3] List Instrument/ Equipment Database \n"
          putStr "[4] List Calibration Observation Log-Report \n"
          putStr "[5] List of Top 3 (Three) Instrument/ Equipment Registered \n"
          putStr "[6] Counting Total Instrument/ Equipment Required Recalibration \n"
          putStr "[7] Search for Instrument/ Equipments  \n"
          putStr "[8] Updated Date System \n"
          putStr "[9] Infographic Instrument/ Equipments Calibration Recommendation Notification \n"
          putStr "Option Selection: \n"
          resp <- getLine
          if (resp=="1") then do insert_instrumentdb
                              
          else if (resp=="2") then do insert_obslog

          else if (resp=="3") then do putStr(print_lst_instdb list_instrumentdb)

          else if (resp=="4") then do putStr(print_lst_obslog list_obslog)
          
          else if (resp=="5") then do let result = print_lst_calibtype current_date list_instrumentdb_registered
                                      if (result == "") then putStr("\n No Instrument/Equipment has been registered!\n") else putStr(result)

          else if (resp=="6") then do putStr("Total Instrument/ Equipment Required Recalibration: ")
                                      print(count_calibType list_instrumentdb_registered)

          else if (resp=="7") then do putStr "Search Instrument/ Equipment: \n"
                                      name <- getLine
                                      let results = compareEquCharAllByName name list_obslog list_instrumentdb
                                      if (results == []) then print("Instrument/ Equipment Not Found") else print(max_list results)

          else if (resp=="8") then do putStr "Set Date (yyyy-mm-dd): \n"
                                      newdata <- getLine
                                      let newdate = parseDay newdata
                                      putStr "\nUpdated System DateTime: \n"
                                      writeFile "datesystem.txt" (showGregorian newdate)
                                      let newlist = print_lst_calibtype_newDate_still newdate list_instrumentdb_registered
                                      if (newlist == "") then putStr(" - No Instrument/ Equipment Recorded.\n") else putStr(newlist)
                                      
                                      putStr "\nUpdated Database with Current New Date]:\n"
                                      let newlist = print_lst_calibtype_newDate_out newdate list_instrumentdb_registered
                                      if (newlist == "") then putStr(" - No Instrument/ Equipment Recorded.\n") else putStr(newlist)

          else if (resp=="9") then do putStrLn("\n ** Calibration Recommendation Notification Infographic ** \n" ++ graph list_obslog list_instrumentdb ++"\n ************** Recommender-System **************\n")                                 

          else error "Selected Option Not Found \n"

          putStr "\nDo you wish to continue (y/n)? : \n"
          resp <- getLine
          if (resp=="y" || resp=="Y") then main else return()