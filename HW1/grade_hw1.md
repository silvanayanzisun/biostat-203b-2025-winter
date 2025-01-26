*Yanzi Sun*

### Overall Grade: 123/130

### Quality of report: 10/10

-   Is the homework submitted (git tag time) before deadline? Take 10 pts off per day for late submission.  

-   Is the final report in a human readable format html and pdf? 

-   Is the report prepared as a dynamic document (Quarto) for better reproducibility?

-   Is the report clear (whole sentences, typos, grammar)? Do readers have a clear idea what's going on and how results are produced by just reading the report? Take points off if the solutions are too succinct to grasp, or there are too many typos/grammar. 

### Completeness, correctness and efficiency of solution: 79/80

- Q1 (10/10)

	Is the GitHub.com repository name set up correctly? If using name other than `biostat-203b-2025-winter`, take 5 points off.

- Q2 (20/20)

	If CITI training is not completed successfully, take 15 points off. 
	
	If PhysioNet crecential is not complete, take 5 pts off.

- Q3 (20/20)

	Q3.1, if the gz files are ever decompressed or copied in the solutions, take 5 points off.
	
	For Q3.5-7, should skip the header when finding the unique values of each variable. Take 5 points of if not done so.

- Q4 (10/10)

	It's fine to just count the lines containing each name. If a student figures out a way to count the words (one line may contain the same name multiple times), give bonus points.
	
	**Q4.3: Line 16-20, not 15-20.** (-0 pt)

- Q5 (9/10)

    **Please report the unusual thing in Sep 1752.** (-1 pt)

- Q6 (10/10)
	    
### Usage of Git: 10/10

-   Are branches (`main` and `develop`) correctly set up? Is the hw submission put into the `main` branch?

-   Are there enough commits (>=5) in develop branch? Are commit messages clear? The commits should span out, not clustered the day before deadline. 
          
-   Is the hw1 submission tagged? 

-   Are the folders (`hw1`, `hw2`, ...) created correctly? 
  
-   Do not put auxiliary files into version control. If files such as `.Rhistory`, `.RData`, `.Rproj.user`, `.DS_Store`, etc., are in Git, take 5 points off.

-   If those gz data files or `pg42671` are in Git, take 5 points off.

### Reproducibility: 10/10

-   Are the materials (files and instructions) submitted to the `main` branch sufficient for reproducing all the results? Just click the `Render` button will produce the final `html`? 

-   If necessary, are there clear instructions, either in report or in a separate file, how to reproduce the results?

### R code style: 14/20

For bash commands, only enforce the 80-character rule. Take 2 pts off for each violation. 

-   [Rule 2.6](https://style.tidyverse.org/syntax.html#long-function-calls) The maximum line length is 80 characters. Long URLs and strings are exceptions.  

    **Line 106, 107, 120.**

-   [Rule 2.5.1](https://style.tidyverse.org/syntax.html#indenting) When indenting your code, use two spaces.  

-   [Rule 2.2.4](https://style.tidyverse.org/syntax.html#infix-operators) Place spaces around all infix operators (=, +, -, &lt;-, etc.).  

-   [Rule 2.2.1.](https://style.tidyverse.org/syntax.html#commas) Do not place a space before a comma, but always place one after a comma.  

-   [Rule 2.2.2](https://style.tidyverse.org/syntax.html#parentheses) Do not place spaces around code in parentheses or square brackets. Place a space before left parenthesis, except in a function call.
