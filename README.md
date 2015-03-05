# JEE_Ranking_System

JEE_Ranking_System is a program written in sml(standard ML) which calculates the rank of the candidates in the JEE like exams. The below mentioned is the complete details of the implementation.

RUNNING THE RANKING SYSTEM
---

1. you need to have 'sml' installed. 
2. go to 'sml' shell & type: use "jee-ranking.sml";
3. You shall see the function signature: "val jee_ranking = fn : real list list * int * real -> real list list"
i.e. jee_ranking takes 3 inputs:
a) list of list of reals : list of q lists, each of length c, containing numbers in the range 0..w, representing
the marks obtained by the candidates in the increasing order of their roll numbers.
b) int : q
c) real : w

4. Here c is the number of candidates, w is the weightage of each question paper (all question papers have equal weightage), q is the number if question papers

5. Run the program as jee_ranking (l,q,w) "eg: jee_ranking ([[2.0,3.0],[1.0,3.0]],2,5.0);" & see the results.

6. The output is a single list of length c + 1, such that each element of the list is a list of length (q + 3).
Here 1st list is the head & rest c lists gives the details of each candidates.
The c lists are sorted according to rank & roll no. of the candidates
The elements inside a list includes rank, roll no., total marks & marks in q question papers.


Please read the Ranking_Details to know the exact implementation.
