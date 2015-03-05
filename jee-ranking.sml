(********************************************************************************************************)
(****************--------------------------------RANKING SYSTEM-----------------------------*************)
(****************-----------------------------Author: Harinder Pal -------------------------*************)
(********************************************************************************************************)

(*********************************************************************************************************
Description of the revisions:
	There has been multiple revisions of the code, some were intuitive and are not shown here.
	Among the others:
	1. In the begining all the functions were written without scopes (without using "local")
	2. Separate mergeSort functions were written, which is later modified to a single merge sort passing separate R functions
	3. Exception is used to handle invalid inputs
**********************************************************************************************************)

(*********************************************************************************************************
-----------------------------------Merge functions earlier used as per revision 2-----------------------
fun mergeSortTuple2Temp R [] = []
	| mergeSortTuple2Temp R [h] = [h]
	| mergeSortTuple2Temp R L = 
		let fun split [] = ([],[])
			| split [h] = ([h],[])
			| split (h1::h2::t) =
				let val (left,right) = split t;
					in (h1::left,h2::right)
				end;
			val (left,right) = split L;
			fun merge (R,[],[]) = []
			| merge (R,[],(L2 as h2::t2)) = L2
			| merge (R ,(L1 as h1::t1),[]) = L1
			| merge (R ,(L1 as (h1_num,h1_val)::t1),(L2 as (h2_num,h2_val)::t2)) =
				if R(h1_val,h2_val) then (h1_num,h1_val) :: (merge(R,t1,L2 ))
				else (h2_num,h2_val)::(merge(R,L1,t2));
			val sortedLeft = mergeSortTuple2Temp R left;
			val sortedRight = mergeSortTuple2Temp R right;
		in merge (R,sortedLeft,sortedRight)
		end;

fun mergeSortTuple2Dec (L:(int*real) list) = mergeSortTuple2Temp (op >) L;


fun mergeSortTuple3Temp R [] = []
	| mergeSortTuple3Temp R [h] = [h]
	| mergeSortTuple3Temp R L = 
		let fun split [] = ([],[])
			| split [h] = ([h],[])
			| split (h1::h2::t) =
				let val (left,right) = split t;
					in (h1::left,h2::right)
				end;
			val (left,right) = split L;
			fun merge (R,[],[]) = []
			| merge (R,[],(L2 as h2::t2)) = L2
			| merge (R ,(L1 as h1::t1),[]) = L1
			| merge (R ,(L1 as (h1_num,h1_val,h1_list)::t1),(L2 as (h2_num,h2_val,h2_list)::t2)) =
				if R(h1_val,h2_val) then (h1_num,h1_val,h1_list) :: (merge(R,t1,L2 ))
				else (h2_num,h2_val,h2_list)::(merge(R,L1,t2));
			val sortedLeft = mergeSortTuple3Temp R left;
			val sortedRight = mergeSortTuple3Temp R right;
		in merge (R,sortedLeft,sortedRight)
		end;

fun mergeSortTuple3Dec (L:(int * real* real list) list) = mergeSortTuple3Temp (op >) L;
**********************************************************************************************************)



(***************************************************************************************************************
val jee_ranking = fn : real list list * int * real -> real list list
----------------------------------------------

Functionality : calculates the jee ranking of candidates & displays in required format

Time Complexity : O(q*c*log(c))
Space Complexity : O(q*c)
****************************************************************************************************************)
fun jee_ranking (inList,q,w) = 
	let 

		exception Invalid_Input

(***************************************************************************************************************
val validateQ = fn : 'a list * int -> bool
----------------------------------------------

Functionality : validates q (number of question papers)

Time Complexity : O(q) - traversing the list to calculate the length
Space Complexity : O(q*c) - no extra space is used

****************************************************************************************************************)
fun validateQ (L,q) = 
	if length (L) = q then true else false;



(***************************************************************************************************************
val validateW = fn : real list list * real -> bool
----------------------------------------------

Functionality : validates w (maximum marks in a question paper)

Time Complexity : O(m*n) where m is the length of outer list & n is the length of inner list
Space Complexity : O(m*n)

****************************************************************************************************************)
local
fun validateWTemp ([],w:real) = true
	| validateWTemp ((h:real)::t,w) = 
	if h>w then false 
	else validateWTemp(t,w)
in
fun validateW ([],w:real) = true
	| validateW ((h:real list)::t,w) = 
	if not(validateWTemp(h,w)) then false
	else validateW(t,w)
end;



(***************************************************************************************************************
val getC = fn : 'a list list -> int
----------------------------------------------

Functionality : validates the length of all the lists, returns 0 if not, else return the length i.e. c

Time Complexity : O(m*n)
Space Complexity : O(m*n)

****************************************************************************************************************)
local
fun validateCTemp([],c) = true
	| validateCTemp(h::t,c) =
	if not(length (h) = c) then false
	else validateCTemp(t,c)
in
fun getC ([]) = 0
	| getC (h::t) = 
	let val c = length(h)
	in 
		if validateCTemp (t,c) then c
	else 0
	end;
end;


(***************************************************************************************************************
val addNumTo1Tuple = fn : int * 'a list -> (int * 'a) list
----------------------------------------------

Functionality : prefix all the elements with increasing order of numbers

Time Complexity : O(n) where n is length of the list
Space Complexity : O(n)

****************************************************************************************************************)
fun addNumTo1Tuple (n,[]) = []
	| addNumTo1Tuple (n,h::[]) = [(n,h)]
	| addNumTo1Tuple (n,h::t) = 
		(n,h)::addNumTo1Tuple(n+1,t);


(***************************************************************************************************************
val addNumTo2Tuple = fn : int * ('a * 'b) list -> (int * 'a * 'b) list
----------------------------------------------

Functionality : prefix all the elements with increasing order of numbers

Time Complexity : O(n) where n is length of the list
Space Complexity : O(n)

****************************************************************************************************************)
fun addNumTo2Tuple (n,[]) = []
	| addNumTo2Tuple (n,(h1,h2)::[]) = [(n,h1,h2)]
	| addNumTo2Tuple (n,(h1,h2)::t) = 
		(n,h1,h2)::addNumTo2Tuple(n+1,t);



(***************************************************************************************************************
val addNumTo2TupleSameNumToEqualValues = fn : int * ('a * real * 'b) list -> (int * 'a * real * 'b) list
----------------------------------------------

Functionality : prefix all the elements with increasing order of numbers giving same numbers to the equal elements
			  : input lists shall be sorted

Time Complexity : O(n) where n is length of the list
Space Complexity : O(n)

****************************************************************************************************************)
fun addNumTo2TupleSameNumToEqualValues (n,[]) = []
	| addNumTo2TupleSameNumToEqualValues (n,(a1,a2:real,a3)::[]) = [(n,a1,a2,a3)]
	| addNumTo2TupleSameNumToEqualValues (n,(a1,a2:real,a3)::(b1,b2,b3)::t) = 
		if (a2 > b2 orelse b2 > a2) then (n,a1,a2,a3)::addNumTo2TupleSameNumToEqualValues(n+1,(b1,b2,b3)::t)
		else (n,a1,a2,a3)::addNumTo2TupleSameNumToEqualValues(n,(b1,b2,b3)::t);



(***************************************************************************************************************
val mergeSort = fn : ('a * 'a -> bool) -> 'a list -> 'a list
----------------------------------------------

Functionality : sorts the list according to input R

Time Complexity : O(m*n*logn) where n is length of the list & O(m) is the time taken for "R"
Space Complexity : O(m*n)

****************************************************************************************************************)
fun mergeSort R [] = []
	| mergeSort R [h] = [h]
	| mergeSort R L = 
		let fun split [] = ([],[])
			| split [h] = ([h],[])
			| split (h1::h2::t) =
				let val (left,right) = split t;
					in (h1::left,h2::right)
				end;
			val (left,right) = split L;
			fun merge (R,[],[]) = []
			| merge (R,[],(L2 as h2::t2)) = L2
			| merge (R ,(L1 as h1::t1),[]) = L1
			| merge (R ,(L1 as h1::t1),(L2 as h2::t2)) =
				if R(h1,h2) then h1 :: (merge(R,t1,L2 ))
				else (h2)::(merge(R,L1,t2));
			val sortedLeft = mergeSort R left;
			val sortedRight = mergeSort R right;
		in merge (R,sortedLeft,sortedRight)
		end;


(***************************************************************************************************************
val R_AscTuple3 = fn : ('a * real * 'b) * ('c * real * 'd) -> bool
----------------------------------------------

Functionality : boolean relation comparing the #2 of 3 tuple

Time Complexity : O(1)
Space Complexity : O(n)

****************************************************************************************************************)
fun R_AscTuple3 (a as (a_num,a_val:real,a_list),b as (b_num,b_val:real,b_list)) = 
	if(a_val<b_val) then true
	else false;



(***************************************************************************************************************
val mergeSortTuple3Asc = fn : (int * real * real list) list -> (int * real * real list) list
----------------------------------------------

Functionality : merge sorts according to "R_AscTuple3"

Time Complexity : O(n logn)
Space Complexity : O(m*n)

****************************************************************************************************************)
fun mergeSortTuple3Asc (L:(int * real* real list) list) = mergeSort (R_AscTuple3) L;



(***************************************************************************************************************
val R_AscTuple4 = fn : ('a * 'b * real * 'c) * ('d * 'e * real * 'f) -> bool
----------------------------------------------

Functionality : boolean relation comparing the #3 of 4 tuple

Time Complexity : O(1)
Space Complexity : O(n)

****************************************************************************************************************)
fun R_AscTuple4 (a as (a_class,a_num,a_val:real,a_list),b as (b_class,b_num,b_val:real,b_list)) = 
	if(a_val<b_val) then true
	else false;


(***************************************************************************************************************
val mergeSortTuple4Asc = fn : (int * int * real * real list) list -> (int * int * real * real list) list
----------------------------------------------

Functionality : merge sorts according to "R_AscTuple4"

Time Complexity : O(n*logn)
Space Complexity : O(m*n)

****************************************************************************************************************)
fun mergeSortTuple4Asc (L:(int * int * real* real list) list) = mergeSort (R_AscTuple4) L;




(***************************************************************************************************************
val sumLists = fn : real list * real list -> real list
----------------------------------------------

Functionality : sum the individual elements of two lists
			  : Length of lists shall be same

Time Complexity : O(n) where n is the length of the lists
Space Complexity : O(n)

****************************************************************************************************************)
fun sumLists ([],[]) = []
	| sumLists ((h:real)::t,[]) = h::t
	| sumLists ([],(h:real)::t) = h::t
	| sumLists ((h:real)::t,(x:real)::y) =
		(h+x)::sumLists(t,y);


(***************************************************************************************************************
val sumNLists = fn : real list list -> real list
----------------------------------------------

Functionality : sum the elements of the lists
			  : Length of lists shall be same

Time Complexity : O(m*n)
Space Complexity : O(m*n)

****************************************************************************************************************)
fun sumNLists ([]) = []
	| sumNLists ((h:real list)::[]) = 
		sumLists(h,[])
	| sumNLists ((h:real list)::f::[]) = 
		sumLists(h,f)
	| sumNLists ((h:real list)::f::t) = 
		sumNLists (sumLists(h,f)::t);

(***************************************************************************************************************
val totalEachCandi = fn : real list list -> real list
----------------------------------------------

Functionality : Uses "sumNLists" to find the total of each candidate

Time Complexity : O(m*n)
Space Complexity : O(m*n)

****************************************************************************************************************)
fun totalEachCandi([]) = []
	| totalEachCandi(L) = 
		sumNLists(L);


(***************************************************************************************************************
val totalEachCandiWithNum = fn : 'a list -> (int * 'a) list
----------------------------------------------

Functionality : Uses "addNumTo1Tuple" to attach the candidate number to total marks

Time Complexity : O(n)
Space Complexity : O(n)

****************************************************************************************************************)
fun totalEachCandiWithNum (L) = addNumTo1Tuple (1,L);




(***************************************************************************************************************
val matrix_transpose = fn : 'a list list -> 'a list list
----------------------------------------------

Functionality : transpose the matrix

Time Complexity : O(m*n)
Space Complexity : O(m*n)

****************************************************************************************************************)
fun matrix_transpose ([]::_) = []
  | matrix_transpose rows = (map hd rows) :: matrix_transpose (map tl rows);




(***************************************************************************************************************
val calcMarksOfCandidatesForEachClassTemp = fn : (int * 'b * 'c * real list) list -> real list list
----------------------------------------------

Functionality : helper function used to calculate the marks of candidates for each class

Time Complexity : O(n)
Space Complexity : O(m*n)

****************************************************************************************************************)
fun calcMarksOfCandidatesForEachClassTemp([]) = []
	| calcMarksOfCandidatesForEachClassTemp(L as ((h_k:int),h_i,h_avg,h_l)::[]) = [h_l]
	| calcMarksOfCandidatesForEachClassTemp(L as (h1 as h1_k,h1_i,h1_avg,h1_l)::(h2 as h2_k,h2_i,h2_avg,h2_l)::t) = 
		let val sum_l = sumLists (h1_l, h2_l)
		in 
			if (h1_k = h2_k) then 			
					calcMarksOfCandidatesForEachClassTemp ((h2_k,h2_i,h2_avg,sum_l)::t)
		
			else 
					h1_l::calcMarksOfCandidatesForEachClassTemp((h2_k,h2_i,h2_avg,h2_l)::t)
		end;


(***************************************************************************************************************
val calcMarksOfCandidatesForEachClass = fn : (''a * 'b * 'c * real list) list -> real list list
----------------------------------------------

Functionality : function used to calculate the marks of candidates for each class

Time Complexity : O(m*n)
Space Complexity : O(m*n)

****************************************************************************************************************)
fun calcMarksOfCandidatesForEachClass(L) = 
	matrix_transpose (calcMarksOfCandidatesForEachClassTemp(L));





(***************************************************************************************************************
val sumElements = fn : real list -> real
----------------------------------------------

Functionality : sums up the elements of the list

Time Complexity : O(n)
Space Complexity : O(n)

****************************************************************************************************************)
fun sumElements ([]) = 0.0
	| sumElements ((h:real)::[]) = h
	| sumElements ((h:real)::t) = 
		h+sumElements(t);


(***************************************************************************************************************
val divideBy = fn : real -> real -> real
----------------------------------------------

Functionality : divides a number by other

Time Complexity : O(1)
Space Complexity : O(1)

****************************************************************************************************************)
fun divideBy b = fn a => a/b;


(***************************************************************************************************************
val avgMarksQues = fn : real list list * int -> real list
----------------------------------------------

Functionality : Calculates the average of marks obtained by candidates in all papers

Time Complexity : O(m*n)
Space Complexity : O(m*n)

****************************************************************************************************************)
fun avgMarksQues([],c) = []
	| avgMarksQues(L,c) =		
			map (divideBy (real(c))) (map sumElements L);
		

(***************************************************************************************************************
val avgMarksQuesWithNum = fn : 'a list -> (int * 'a) list
----------------------------------------------

Functionality : attaches quesNum to the avgMarks

Time Complexity : O(n)
Space Complexity : O(n)

****************************************************************************************************************)
fun avgMarksQuesWithNum (L) = addNumTo1Tuple (1,L);


(***************************************************************************************************************
val joinInputWithAvgMarksQuesWithNum = fn : ('a * 'b) list * 'c list -> ('a * 'b * 'c) list
----------------------------------------------

Functionality : input is joined over here since, else it would take lot time to read from the input while calculating total marks for each student in each class
			  : length shall be same for both lists

Time Complexity : O(n)
Space Complexity : O(n)

****************************************************************************************************************)
fun joinInputWithAvgMarksQuesWithNum ([], []) = []
	| joinInputWithAvgMarksQuesWithNum ([],inputList) = raise Invalid_Input
	| joinInputWithAvgMarksQuesWithNum (L,[]) = raise Invalid_Input
	| joinInputWithAvgMarksQuesWithNum (L as (h1Index,h1Avg)::[],inputList as h2::[]) = 
		[(h1Index,h1Avg,h2)]
	| joinInputWithAvgMarksQuesWithNum (L as (h1Index,h1Avg)::t1,inputList as h2::t2) = 
		(h1Index,h1Avg,h2)::joinInputWithAvgMarksQuesWithNum(t1,t2);


(***************************************************************************************************************
val avgMarksQuesWithNumWithClass = fn : ('a * real * 'b) list -> (int * 'a * real * 'b) list
----------------------------------------------

Functionality : gives the class number (input list shall be sorted)

Time Complexity : O(n)
Space Complexity : O(n)

****************************************************************************************************************)
fun avgMarksQuesWithNumWithClass (L) = addNumTo2TupleSameNumToEqualValues (1,L);



(***************************************************************************************************************
val getFirstList = fn : real * int * ('a * 'b * real * 'c) list -> real list
----------------------------------------------

Functionality : Getting the output | generates the first list as required
			  : input - aj_withNum_joinedWithInput_sortedOnVal_withClass

Time Complexity : O(n)
Space Complexity : O(n)

****************************************************************************************************************)
fun getFirstList (w,q,[]) = raise Invalid_Input
	| getFirstList (w,q,L as (h_k,h_i,h_avg,h_l)::[]) = 
	0.0::0.0::(w*real(q))::[h_avg]
	| getFirstList (w,q,L as (h_k,h_i,h_avg,h_l)::t) = 
	0.0::0.0::(w*real(q))::(map #3 L);



(***************************************************************************************************************
val getCandiMarks_accToQuesAvg = fn : ('a * 'b * 'c * 'd list) list -> 'd list list
----------------------------------------------

Functionality : gets the candidate marks according to the question average
			  : input - aj_withNum_joinedWithInput_sortedOnVal_withClass

Time Complexity : O(m*n)
Space Complexity : O(m*n)

****************************************************************************************************************)
fun getCandiMarks_accToQuesAvg ([]) = raise Invalid_Input
	| getCandiMarks_accToQuesAvg (L as (h_k,h_i,h_avg,h_l)::[]) = 
	let val mTemp1 = map #4 L
	in 
		matrix_transpose (mTemp1)
	end
	| getCandiMarks_accToQuesAvg (L as (h_k,h_i,h_avg,h_l)::t) = 
	let val mTemp2 = map #4 L
	in 
		matrix_transpose (mTemp2)
	end;


open List;


(***************************************************************************************************************
val getCandiOutput = fn : (int * int * real) list * real list list -> real list list
----------------------------------------------

Functionality : Getting the output | generates the candidate lists as required
			  : input - ti_withNum_sorted_withRanks, candiMarks_accToQuesAvg
			  : Lists shall be of equal length

Time Complexity : O(m*n)
Space Complexity : O(m*n)

****************************************************************************************************************)
fun getCandiOutput ([], L2) = [] 
	
	| getCandiOutput (L1 as (h1_rank:int,h1_num:int,h1_total:real)::[], L2) = 
	(real(h1_rank)::real(h1_num)::h1_total::nth(L2,h1_num-1))::[]
	
	| getCandiOutput (L1 as (h1_rank:int,h1_num:int,h1_total:real)::t1, L2) = 
	(real(h1_rank)::real(h1_num)::h1_total::nth(L2,h1_num-1))::getCandiOutput (t1,L2);



(***************************************************************************************************************
val getFinalOutput = fn : 'a list * 'a list list -> 'a list list
----------------------------------------------

Functionality : generates the final output

Time Complexity : O(1)
Space Complexity : O(m*n)

****************************************************************************************************************)
fun getFinalOutput ([],[]) = []
	| getFinalOutput (L1,L2) = 
		L1::L2;

val isValid_q = validateQ (inList,q);
val isValid_w = validateW (inList, w);
val c = getC(inList);

val aj = avgMarksQues (inList,c);
val aj_withNum = avgMarksQuesWithNum(aj);
val aj_withNum_joinedWithInput = joinInputWithAvgMarksQuesWithNum(aj_withNum, inList);
val aj_withNum_joinedWithInput_sortedOnValAsc = mergeSortTuple3Asc(aj_withNum_joinedWithInput);
val aj_withNum_joinedWithInput_sortedOnValAsc_withClass = avgMarksQuesWithNumWithClass(aj_withNum_joinedWithInput_sortedOnValAsc);

val candiMarks_perClass = calcMarksOfCandidatesForEachClass(aj_withNum_joinedWithInput_sortedOnValAsc_withClass);



(***************************************************************************************************************
val relateOnClasses = fn : real list * real list -> int
----------------------------------------------

Functionality : compare the candidate marks according to the classes
			  : returns 1 if candidate1 has more marks, 2 if candidate2 has more marks, 3 if they have same marks in all classes

Time Complexity : O(n)
Space Complexity : O(n)

****************************************************************************************************************)
fun relateOnClasses ([], []) = 0
	| relateOnClasses ([],c2_classMarks) = raise Invalid_Input
	| relateOnClasses (c1_classMarks,[]) = raise Invalid_Input
	| relateOnClasses(c1_classMarks as (h1:real)::t1, c2_classMarks as h2::t2) = 
	if(h1>h2) then 1
	else if (h1<h2) then 2
	else relateOnClasses(t1,t2);



(***************************************************************************************************************
val R_sortCandis = fn : (int * real) * (int * real) -> bool
----------------------------------------------

Functionality : Relation function to be used in merge sort, to sort the candidates according to ranks

Time Complexity : O(n)
Space Complexity : O(n)

****************************************************************************************************************)
fun R_sortCandis (c1 as (c1_n,c1_t:real),c2 as (c2_n,c2_t:real)) = 
	if(c1_t > c2_t) then true
	else if(c1_t < c2_t) then false
	else 
		let val rel = relateOnClasses (nth(candiMarks_perClass,c1_n-1),nth(candiMarks_perClass,c2_n-1))
			in if (rel = 1) then true
				else if (rel = 2) then false
				else 
					if (c1_n > c2_n) then false
					else true
 		end;


(***************************************************************************************************************
val mergeSort_candis = fn : (int * real) list -> (int * real) list
----------------------------------------------

Functionality : merge sorts using "R_sortCandis"

Time Complexity : O(m*n*logn)
Space Complexity : O(m*n)

****************************************************************************************************************)
fun mergeSort_candis (L:(int*real) list) = mergeSort (R_sortCandis) L;

		

(***************************************************************************************************************
val assignRanksTemp = fn : int * (int * real) list -> (int * int * real) list
----------------------------------------------

Functionality : temp function used to finally assign ranks to the candidates
			  : h1_total can't be less than h2_total

Time Complexity : O(n)
Space Complexity : O(n)

****************************************************************************************************************)
(* rel can't be 2*) (*h1_total can't be less than h2_total*)
fun assignRanksTemp (n, []) = []
	| assignRanksTemp (n, L as (h1 as (h1_num,h1_total:real))::[]) =
		[(n,h1_num,h1_total)]

	| assignRanksTemp (n, L as (h1 as (h1_num,h1_total:real))::(h2 as (h2_num,h2_total))::t) =
		if (h1_total > h2_total) then (n,h1_num,h1_total)::assignRanksTemp(n+1,h2::t)
		else 
			let val rel = relateOnClasses (nth(candiMarks_perClass,h1_num-1),nth(candiMarks_perClass,h2_num-1))
				in if (rel = 1) then (n,h1_num,h1_total)::assignRanksTemp(n+1,h2::t)
					else (n,h1_num,h1_total)::assignRanksTemp(n,h2::t)
			end;

(***************************************************************************************************************
val assignRanks = fn : (int * real) list -> (int * int * real) list
----------------------------------------------

Functionality : assign ranks to the candidates

Time Complexity : O(n)
Space Complexity : O(n)

****************************************************************************************************************)
fun assignRanks (L) = assignRanksTemp (1,L);

val ti = totalEachCandi (inList);
val ti_withNum = totalEachCandiWithNum (ti);
val ti_withNum_sorted = mergeSort_candis (ti_withNum);
val ti_withNum_sorted_withRanks = assignRanks (ti_withNum_sorted);
val candiMarks_accToQuesAvg = getCandiMarks_accToQuesAvg(aj_withNum_joinedWithInput_sortedOnValAsc_withClass);
val firstOutputList = getFirstList (w,q,aj_withNum_joinedWithInput_sortedOnValAsc_withClass);
val candiOutputList = getCandiOutput (ti_withNum_sorted_withRanks, candiMarks_accToQuesAvg);
val finalOutput = getFinalOutput (firstOutputList, candiOutputList);

	in 
		if (isValid_q = false) then raise Invalid_Input 
		else if (isValid_w = false) then raise Invalid_Input
		else if (c = 0) then raise Invalid_Input
		else 
			finalOutput
	end;
