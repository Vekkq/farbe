
# deletion usage?


## lowlevel deletion

* no safeguards
* effort



## gc deletion

* delayed
* somewhat uncertain



## scoped deletion
call deletes all in it once the function returns

* safe
* unflexible, wouldnt work alone for "open world" rendering



## transition deletion
retains all objects that are called early after transition

* uncertain
* complicated



# plan

do gc deletions.

deletions get priority on the gl thread. thread calls gc once after every frame.




