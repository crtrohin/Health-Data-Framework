
-- =============== DO NOT MODIFY ===================

{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}

-- ==================================================

module Tasks where

import Dataset
import Data.List
import Text.Printf
import Data.Array

import Common

import Data.Maybe (fromMaybe)

type CSV = String
type Value = String
type Row = [Value]
type Table = [Row]
type ColumnName = String

-- Prerequisities
split_by :: Char -> String -> [String]
split_by x = foldr op [""]
  where op char acc
            | char == x = "":acc
            | otherwise = (char:head(acc)):tail(acc)

read_csv :: CSV -> Table
read_csv = (map (split_by ',')) . (split_by '\n')

write_csv :: Table -> CSV
write_csv = (foldr (++) []).
            (intersperse "\n").
            (map (foldr (++) [])).
            (map (intersperse ","))


{-
    TASK SET 1
-}


toStr x = printf ("%.2f") x
division x y = (fromIntegral x / fromIntegral y) :: Float

{-
  This function (mySum) does the following:
    i) removes the header of the table -> ["Name","10","11","12","13","14","15","16","17"]
    ii) removes the name of the participant from each line
    iii) converts string values to integer : ["10"] -> 10
    iv) performs sum operation on each line, summing the number of steps for each hour, for every participant
-}

mySum :: Table -> [Integer]
mySum = map (sum.
            (map read).
            tail).
        tail

{-
  This function (get_list_avg_steps) does the following:
    i) finds the average number of steps for each participant
    ii) converts the numbers which represent the average for every participant back into strings
-}

get_list_avg_steps :: Table -> Row
get_list_avg_steps m = map (toStr . (\x -> division x 8)) (mySum m)

{-
  This function (compute_average_steps) does the following:
    i) introduces the name of each participant back by creating lists with names and values representing averages
    ii) introduces a new header for the table that is returned :  ["Name","Average Number of Steps"]
-}

header1 = ["Name","Average Number of Steps"]

compute_average_steps :: Table -> Table
compute_average_steps m = header1 : zipWith (\x y -> [x,y]) ((map head (tail m))) (get_list_avg_steps m)

-- Task 2

-- Number of people who have achieved their goal:
get_passed_people_num :: Table -> Int
get_passed_people_num m = length (filter (>= 1000) (mySum m))

-- Percentage of people who have achieved their:
get_passed_people_percentage :: Table -> Float
get_passed_people_percentage m = division (get_passed_people_num m) (length m - 1)


-- Average number of daily steps
get_steps_avg :: Table -> Float
get_steps_avg m = division (sum (mySum m)) (length m - 1)

-- Task 3

get_avg_steps_per_h m = map (\x -> 'H':x) (tail (head m)):
                          (map (toStr . (\x -> division x (length m - 1)) . sum)
                            (transpose (map ((map read) . tail) (tail m)))):[]


-- Task 4

header4_1 = ["column","range1","range2","range3"]
header4_2 = [["VeryActiveMinutes"],["FairlyActiveMinutes"],["LightlyActiveMinutes"]]

{-
  The local function (distribute) does the following:
    i) takes the list [0,0,0] as an accumulator
    ii) the first element from the lists stores the the number of people 
        who have spent a number of minutes included in the range [50,100) for each intensity etc.
-}

get_activ_summary :: Table -> Table
get_activ_summary m = header4_1 : zipWith (++) header4_2
                        (map ((map show) . (foldr distribute [0,0,0]))
                            (transpose (map ((map read) . drop 3) (tail m))))
                    where
                          distribute el [range1,range2,range3]
                            | (0 <= el) && (el < 50) = [(range1 + 1), range2, range3]
                            | (50 <= el) && (el < 100) = [range1, (range2 + 1), range3]
                            | otherwise = [range1, range2, (range3 + 1)]

-- Task 5
header5 = ["Name", "Total Steps"]


get_ranking :: Table -> Table
get_ranking m = header5 : (sortBy comp (map (take 2) (tail m)))
    where
        comp (name1:steps1:_) (name2:steps2:_)
            | (read steps1::Integer) > (read steps2::Integer) = GT
            | (read steps1::Integer) == (read steps2::Integer) = compare name1 name2
            | otherwise = LT


-- Task 6

header6 = ["Name", "Average first 4h", "Average last 4h", "Difference"]

{-
    This function (extractFour) does the following:
        i) it takes a function as a parameter which can either be drop or take, 
           and extracts first or last four hours for every person
-}

extractFour :: (Int -> Row -> Row) -> Table -> Table
extractFour f m = map (f 4) (map tail (tail m))

{-
    This function (sumSteps) does the following:
        i) Converts string values to int values
        ii) Performs the sum operation on the first (last) four hours, depending on 
           the function that is given as a parameter (take, drop)
-}

sumSteps :: (Int -> Row -> Row) -> Table -> [Integer]
sumSteps f m = map (sum . (map read)) (extractFour f m)

{-
    This function (avgFour) does the following:
        i) Finds the average number of steps for each person in those first (last) 4 hours
-}

avgFour :: (Int -> Row -> Row) -> Table -> [Float]
avgFour f m = map (\x -> division x 4) (sumSteps f m)

{-
    This function (combineAvgFour) does the following:
        i) Combines the average of first four hours and last four hours
-}

combineAvgFour :: Table -> [[Float]]
combineAvgFour m = zipWith (\x y -> [x,y]) (avgFour take m) (avgFour drop m)

{-
    This function (findDiff) does the following:
        i) Calculate the difference between the averages
-}

findDiff :: Table -> [[Float]]
findDiff m = zipWith (\x y -> [abs (x - y)]) (avgFour take m) (avgFour drop m)

{-
    This function (combineAll) does the following:
        i) Combines the first name, the two averages and the difference into a list
-}

combineAll :: Table -> Table
combineAll m = zipWith (++)
                    (map ((\x -> [x]) . head) (tail m))
                    (map (map toStr) (zipWith (++) (combineAvgFour m) (findDiff m)))

{-
    This function (get_steps_diff_table) does the following:
        i) Adds the given header and then sorts the matrix that results from applying 
           function combineAll after difference, and if equal, in alphabetical order
           after name
-}

get_steps_diff_table :: Table -> Table
get_steps_diff_table m = header6 : (sortBy comp (combineAll m))
    where
        comp (name1:_:_:diff1:_) (name2:_:_:diff2:_)
            | (read diff1::Float) > (read diff2::Float) = GT
            | (read diff1::Float) == (read diff2::Float) = compare name1 name2
            | otherwise = LT


-- Task 7

-- Applies the given function to all the values
vmap :: (Value -> Value) -> Table -> Table
vmap f m = map (map f) m

-- Task 8

-- Applies the given function to all the entries
rmap :: (Row -> Row) -> [String] -> Table -> Table
rmap f s m = s : (map f (tail m))


get_sleep_total :: Row -> Row
get_sleep_total r = (head r) : [toStr (sum (map (\x -> read x::Float) (tail r)))]


{-
    TASK SET 2
-}

-- Task 1

-- Write a function which takes a column name and a Table and returns the Table sorted by that column 
-- (if multiple entries have the same values, then it is sorted by the first column). 
-- Sorting is ascending for columns with numeric values and lexicographical for strings.
-- If the value is missing (“”), then it is considered before all values (e.g. “” < “0”).

-- Extracts the column names of a table
columnNames :: Table -> Row
columnNames table = head table

-- Converts a Maybe Int value to a Int value
convToInt :: Maybe Int -> Int
convToInt (Just x) = x
convToInt _ = -1

-- Finds the index of the column which is equal to the name column given
columnIndex :: ColumnName -> Table -> Int
columnIndex column table = convToInt (elemIndex column (columnNames table))

-- Determines whether a string can be converted into a number
isNumber :: String -> Bool
isNumber str =
    case (reads str) :: [(Double, String)] of
      [(_, "")] -> True
      _         -> False

-- Finds an element on a given position
extractElem :: Int -> Row -> String
extractElem index row = (head . drop index) row

tsort :: ColumnName -> Table -> Table
tsort column table = (head table):(sortBy op (tail table))
    where
        k = columnIndex column table
        op r1 r2 = convertAndCompare (extractElem k r1) (extractElem k r2)
            where
                convertAndCompare el1 el2
                    | isNumber el1 == True = comp (read el1 :: Double) (read el2 :: Double)
                    | otherwise = comp el1 el2
                comp a b
                    | a > b = GT
                    | a == b = compare (head r1) (head r2)
                    | otherwise = LT
-- Task 2

-- Implement a function which takes Tables t1 and t2 and adds all rows from t2 at the end of t1, if column names coincide.
-- If columns names are not the same, t1 remains unchanged.

vunion :: Table -> Table -> Table
vunion t1 t2
    | head t1 == head t2 = transpose (zipWith (++) (transpose t1) (transpose (tail t2)))
    | otherwise = t1

-- Task 3

-- Implement a function which takes Tables t1 and t2 and extends each row of t1 with a row of t2 (simple horizontal union of 2 tables).
-- If one of the tables has more lines than the other, the shorter table is padded with rows containing only empty strings.

-- Adds an element at the end of a list.
append :: a -> [a] -> [a]
append x xs = xs ++ [x]

-- Creates a stream of empty strings.
createEmptyStrings :: Int -> Row
createEmptyStrings n = "":(createEmptyStrings (n - 1))

-- Adds the empty string sequence to the table that is the shortest.
addEmptyString :: Table -> Table -> Table
addEmptyString t1 t2
    | length t1 > length t2 = addEmptyString t1 (append (take (length (head t2)) (createEmptyStrings (length (head t2)))) t2)
    | otherwise = t2

hunion :: Table -> Table -> Table
hunion t1 t2
    | length t1 > length t2 = hunion t1 (addEmptyString t1 t2)
    | length t1 < length t2 = hunion (addEmptyString t2 t1) t2
    | otherwise = zipWith (++) t1 t2

-- Task 4

-- Implement table join with respect to a key (a column name). 
-- If an entry from table 1 has the same value for the key as an entry from table 2, their contents must be merged. 
-- If there are other columns with the same name in both tables, then the value from t2 overrides the value from t1, unless it is null (“”).

-- Appends two lists and removes duplicates using function nub.
combine:: [String] -> [String] -> [String]
combine x y = nub (x ++ y)

mergeNameColumns :: Table -> Table -> Row
mergeNameColumns t1 t2 = head (zipWith combine t1 t2)

-- Extracts the entries of a given column name.
extractEntr :: ColumnName -> Table -> Row
extractEntr column t = map (\[x] -> x) (projection [column] t)

-- Gets the index of a given element in a column.
getIndx :: ColumnName -> Value -> Table -> Maybe Int
getIndx column el t = elemIndex el (extractEntr column t)

-- Iterates through the entries of the first column that coincides with 
-- the column which has as name the given name and searches each entry
-- in the column of the second table with the same propriety.
-- Afterwards, it saves the indexes of the same entry from each table using pairs.
markEntries :: ColumnName -> Table -> Table -> [(Maybe Int, Maybe Int)]
markEntries column t1 t2 = map (\el -> ((getIndx column el t1),(getIndx column el t2)))
                                            (extractEntr column t1)

-- Deletes the entries that were not found in the second table.
deleteNotFound :: ColumnName -> Table -> Table -> [(Maybe Int, Maybe Int)]
deleteNotFound column t1 t2 = filter (\(a,b) -> not (b == Nothing)) (markEntries column t1 t2)

-- Converts Maybe Int values to Int values
convertDataType :: ColumnName -> Table -> Table -> [(Int, Int)]
convertDataType column t1 t2 = map (\(Just x, Just y) -> (x, y)) (deleteNotFound column t1 t2)

-- Searches a given column two given rows and selects one of the two values.
-- If the given column doesn't exist in one of the rows, it is chosen the value from the other one.
-- If the given column exists in the same row, it is chosen the value from the second row, if not null.
-- Otherwise, it is chosen the  value from the first row.
selectValue :: Value -> Row -> Row -> Table -> Table -> Value
selectValue column row1 row2 t1 t2
    | columnIndex column t1 == -1 = row2 !! (columnIndex column t2)
    | columnIndex column t2 == -1 = row1 !! (columnIndex column t1)
    | not (null (row2 !! (columnIndex column t2))) = row2 !! (columnIndex column t2)
    | otherwise = row1 !! (columnIndex column t1)

-- Iterates through the final name columns and generates a merged row.
mergeRows :: Int -> Int -> Table -> Table -> Row
mergeRows indx1 indx2 t1 t2 = map (\column -> selectValue column (t1 !! indx1) (t2 !! indx2) t1 t2) (mergeNameColumns t1 t2)

tjoin :: ColumnName -> Table -> Table -> Table
tjoin key_column t1 t2 = [ mergeRows indx1 indx2 t1 t2 | (indx1, indx2) <- (convertDataType key_column t1 t2)]

-- Task 5

-- Write the implementation for the cartesian product of 2 tables. The new column names are given. 
-- To generate the entries in the resulting table you have to apply the given operation on each entry in t1 with each entry in t2.

cartesian :: (Row -> Row -> Row) -> [ColumnName] -> Table -> Table -> Table
cartesian new_row_function new_column_names t1 t2 = new_column_names : concat (map (\lx -> map (\ly -> (new_row_function lx ly)) (tail t2)) (tail t1))

-- Task 6

-- Extract from a table those columns specified by name in the first argument.

projection :: [ColumnName] -> Table -> Table
projection columns_to_extract t = transpose (map (\columnName -> map (\row -> extractElem (columnIndex columnName t) row) t) columns_to_extract)

-- Task 7

-- Filter rows based on a given condition applied to a specified column.

filterTable :: (Value -> Bool) -> ColumnName -> Table -> Table
filterTable condition key_column t = (head t) : filter (\row -> condition (extractElem (columnIndex key_column t) row)) (tail t)

-- Task 8 TO_DO


{-
    TASK SET 3
-}


-- 3.1

data Query =
    FromTable Table
    | AsList String Query
    | Sort String Query
    | ValueMap (Value -> Value) Query
    | RowMap (Row -> Row) [String] Query
    | VUnion Query Query
    | HUnion Query Query
    | TableJoin String Query Query
    | Cartesian (Row -> Row -> Row) [String] Query Query
    | Projection [String] Query
    | forall a. FEval a => Filter (FilterCondition a) Query -- 3.4
    | Graph EdgeOp Query -- 3.5
    
instance Show QResult where
    show (List l) = show l
    show (Table t) = show t

class Eval a where
    eval :: a -> QResult

instance Eval Query where
    eval (FromTable table) = Table table
    eval (AsList colname query) = case (eval query) of
                                (Table table) -> List (tail (extractEntr colname table))
    eval (Sort colname query) = case (eval query) of
                                (Table table) -> Table (tsort colname table)
    eval (ValueMap op query) = case (eval query) of
                                (Table table) -> Table (vmap op table)
    eval (RowMap op colnames query) = case (eval query) of
                                (Table table) -> Table (rmap op colnames table)
    eval (VUnion query1 query2) = case (eval query1, eval query2) of
                                (Table table1, Table table2) -> Table (vunion table1 table2)
    eval (HUnion query1 query2) = case (eval query1, eval query2) of
                                (Table table1, Table table2) -> Table (hunion table1 table2)
    eval (TableJoin colname query1 query2) = case (eval query1, eval query2) of
                                (Table table1, Table table2) -> Table (tjoin colname table1 table2)
    eval (Cartesian op colnames query1 query2) = case (eval query1, eval query2) of
                                (Table table1, Table table2) -> Table (cartesian op colnames table1 table2)
    eval (Projection colnames query) = case (eval query) of
                                (Table table) -> Table (projection colnames table)
    eval (Filter filterCondition query) = case (eval query) of
                                (Table table) -> Table ((head table) : (filter (feval (head table) filterCondition) (tail table)))
    eval (Graph edgeOp query) = case (eval query) of
                                (Table table) -> Table (fromTableToGraph edgeOp table)
-- 3.2 & 3.3

type FilterOp = Row -> Bool

data FilterCondition a =
    Eq String a |
    Lt String a |
    Gt String a |
    In String [a] |
    FNot (FilterCondition a) |
    FieldEq String String

class FEval a where
    feval :: [String] -> (FilterCondition a) -> FilterOp

instance FEval Float where
    -- checks if value from column colname is equal to ref.
    feval colNames (Eq colname ref) = (\row -> ref == (read (extractElem (convToInt (elemIndex colname colNames)) row) :: Float))
    -- checks if value from column colname is less than ref.
    feval colNames (Lt colname ref) = (\row -> ref > (read (extractElem (convToInt (elemIndex colname colNames)) row) :: Float))
    -- checks if value from column colname is greater than ref.
    feval colNames (Gt colname ref) = (\row -> ref < (read (extractElem (convToInt (elemIndex colname colNames)) row) :: Float))
    -- checks if value from column colname is in list.
    feval colNames (In colname list) = (\row -> elem (read (extractElem (convToInt (elemIndex colname colNames)) row) :: Float) list)
    -- negates condition.
    feval colNames (FNot cond) = not . (feval colNames cond)
    -- checks if values from columns colname1 and colname2 are equal.
    feval colNames (FieldEq colname1 colname2) = (\row -> (read (extractElem (convToInt (elemIndex colname1 colNames)) row) :: Float) == (read (extractElem (convToInt (elemIndex colname2 colNames)) row) :: Float))

instance FEval String where
    feval colNames (Eq colname ref) = (\row -> ref == (extractElem (convToInt (elemIndex colname colNames)) row))
    feval colNames (Lt colname ref) = (\row -> ref > (extractElem (convToInt (elemIndex colname colNames)) row))
    feval colNames (Gt colname ref) = (\row -> ref < (extractElem (convToInt (elemIndex colname colNames)) row))
    feval colNames (In colname list) = (\row -> elem (extractElem (convToInt (elemIndex colname colNames)) row) list)
    feval colNames (FNot cond) = not . (feval colNames cond)
    feval colNames (FieldEq colname1 colname2) = (\row -> (extractElem (convToInt (elemIndex colname1 colNames)) row) == (extractElem (convToInt (elemIndex colname2 colNames)) row))

-- 3.4

-- where EdgeOp is defined:
type EdgeOp = Row -> Row -> Maybe Value

fromTableToGraph :: EdgeOp -> Table -> Table
fromTableToGraph edgeOp table = ["From","To","Value"] : concat (transformToGraph edgeOp table)

transformToGraph :: EdgeOp -> Table -> [Table]
transformToGraph edgeOp [x] = []
transformToGraph edgeOp table = (filter (/= []) ((checkOpOnEntries edgeOp (head table) (tail table)):(transformToGraph edgeOp (tail table))))

checkOpOnEntries :: Foldable t => (Row -> Row -> Maybe Value)-> Row -> t Row -> Table
checkOpOnEntries edgeOp row = (foldr op [])
    where
        op [] acc = acc
        op another_row acc
            | (edgeOp row another_row /= Nothing) && ((head row) <= (head another_row)) = ((head row):(head another_row):(fromMaybe "" (edgeOp row another_row):[])):acc
            | (edgeOp row another_row /= Nothing) && ((head row) > (head another_row)) = ((head another_row):(head row):(fromMaybe "" (edgeOp row another_row):[])):acc
            | otherwise = acc

-- 3.5
edge_op3 :: EdgeOp
edge_op3 (n1:l1) (n2:l2) = aux l1 l2 0
    where
        aux [] [] 0 = Nothing
        aux [] [] acc = Just (show acc)
        aux l1 l2 acc
            | (read (head l1) :: Float) == (read (head l2) :: Float) = aux (tail l1) (tail l2) (acc + 1)
            | otherwise = aux (tail l1) (tail l2) acc

similarities_query :: Query
similarities_query = Sort "Value" (Filter filter1 (Graph edge_op3 (FromTable Dataset.eight_hours)))
                            where filter1 = (Gt "Value" 4.0 ) :: FilterCondition Float

-- 3.6 (Typos)
-- The Levenshtein distance between two strings a,b

lev :: String -> String -> Int
lev xs ys = arr ! (m,n)
    where
        (m,n) = (length xs, length ys)
        x = array (1,m) (zip [1..] xs)
        y = array (1,n) (zip [1..] ys)
        arr = array ((0,0), (m,n)) [(ij, dist ij) | ij <- range ((0,0), (m,n))]
        dist (0,j) = j
        dist (i,0) = i
        dist (i,j) = minimum [arr ! (i-1,j) + 1, arr ! (i,j-1) + 1,
            if x ! i == y ! j then arr ! (i-1,j-1) else 1 + arr ! (i-1,j-1)]

minim :: [(String, String, Int)] -> (String, String, Int)
minim []       = ("","",0)
minim [x]      = x
minim (x:xs)   = min1 x (minim xs)

min1 :: (String, String, Int) -> (String, String, Int) -> (String, String, Int)
min1 (corr1, typos1, a) (corr2, typos2, b)
    | a > b  = (corr2, typos2, b)
    | a < b  = (corr1, typos1, a)
    | a == b = (corr1, typos1, a)

correct_table :: ColumnName -> Table -> Table -> Table
correct_table col csv1 csv2 = aux
    where
        row1 = extractEntr col csv1
        row2 = extractEntr col csv2
        indx = fromMaybe (-1) (elemIndex col (head csv1))
        ref = (filter (/= "") [(if (length (filter (== ref) row1) == 0) then ref else "") | ref <- row2])
        t = (filter (/= "") [(if (length (filter (== ref) row2) == 0) then ref else "") | ref <- row1])
        corrTyposPair = map (\(corr, typos, dist) -> (typos, corr)) (map (\a -> minim ((map (\b -> (a, b, (lev a b))) t))) ref)
        aux = map op csv1
        op row = if (maybePair row == Nothing) then row 
                else (map (\x -> if (x == extractTypo (maybePair row)) then extractCorr (maybePair row) else x) row)
        maybePair row = find (\(typos, corr) -> typos == row !! indx) corrTyposPair
        extractCorr (Just (_,corr)) = corr
        extractTypo (Just (typo, _)) = typo