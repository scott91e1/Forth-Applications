(*
 * LANGUAGE    : ANS Forth 
 * PROJECT     : Forth Environments
 * DESCRIPTION : Eliza is a psychiatrist of the Carl Roger school.
 * CATEGORY    : AI Game, text based, by Weizenbaum.
 * AUTHOR      : Marcel Hendrix, November 11, 1986
 * LAST CHANGE : July 24, 1993, Marcel Hendrix, case problem my$ My$
 * LAST CHANGE : March 20, 1992, Marcel Hendrix, new TO strings
 * LAST CHANGE : March 15, 1992, Marcel Hendrix 
 *)


	NEEDS -miscutil
	NEEDS -terminal
	NEEDS -strings

	REVISION -eliza "ÄÄÄ The Psychiater      Version 1.21 ÄÄÄ"

	PRIVATES

	
        3 =: #Resp		PRIVATE
      #17 =: #Conjupairs	PRIVATE
	0 VALUE last-c		PRIVATE
	0 VALUE char#		PRIVATE
	0 VALUE phrase_voc	PRIVATE

	DEFER ECHO		PRIVATE


: Rmargin	C/L #10 - ;	PRIVATE


WARNING @  WARNING OFF

: CR		CR  CLEAR char# ; PRIVATE	

: SPACE		char# IF  SPACE  1 +TO char#
		   ENDIF ; PRIVATE

WARNING !

: EMIT'		char# 1+ Rmargin > OVER BL = AND	\ <char> --- <>
		    IF	CR DROP  
		  ELSE  DUP TO last-c  EMIT  1 +TO char# 
		 ENDIF ; PRIVATE

: PRINT-?	last-c '?' <>  last-c '!' <>  AND	\ <> --- <>
		IF '.' EMIT' ENDIF ; PRIVATE

: `TYPE'	ABS #255 MIN				\ <addr> <u> --- <>
		0 ?DO
			C@+
		        DUP '*' <> IF EMIT'		\ This is all..
			         ELSE DROP -1 +TO char#
				      ECHO		\ More (forward)
			        ENDIF
		 LOOP DROP ; PRIVATE


-- print a CR or BL, then the string

: .STRING 	DUP char# + Rmargin >			\ <addr> <cnt> --- <>
		   IF  CR
		 ELSE  SPACE
		ENDIF `TYPE' ; PRIVATE


 S" Please do not repeat yourself."	$CONSTANT Notrepeat$	PRIVATE
 S" Goodbye"				$CONSTANT Goodbye$	PRIVATE
 S" Ok, hope to see you again."	 	$CONSTANT Farewell$	PRIVATE
 S" Hello..."				$CONSTANT Hello$	PRIVATE
 S" The doctor is in..please stand by."	$CONSTANT Doctorin$	PRIVATE
 S" Welcome in my shrinker's office."   $CONSTANT Session$	PRIVATE

 S" are you"				$CONSTANT Areyou$	PRIVATE
 S" are_you"				$CONSTANT Are_you$	PRIVATE
 S" you are"				$CONSTANT Youare$  	PRIVATE
 S" you_are"				$CONSTANT You_are$	PRIVATE
 S" am I"				$CONSTANT AmI$		PRIVATE
 S" am_I"				$CONSTANT Am_I$		PRIVATE
 S" I am"				$CONSTANT Iam$  	PRIVATE
 S" I_am"				$CONSTANT I_am$		PRIVATE
 S" YOU"				$CONSTANT YOU$		PRIVATE
 S" my"					$CONSTANT myl$		PRIVATE
 S" My"					$CONSTANT Myu$		PRIVATE


-- Read ahead in text file. This doesn't work with a terminal.
-- A nice feature: the read text is interpreted, so { 1 2 + } works!

: READ-INFILE	REFILL 0= ABORT" REFILL: Sorry"
		TIB #TIB @ EVALUATE ; PRIVATE


-- Now read n strings ( 1 per line) from THIS file into a string array.

: READ-$ARRAY	LOCAL arr			\ <n> <$mid> --- <>
		0 ?DO		
		      READ-INFILE  TO I (( arr )) DO$ARRAY
		 LOOP 
		REFILL 0= ABORT" REFILL: Sorry" ; PRIVATE


-- Read n strings ( 2 per line) from THIS file into a string array.

: 2READ-$ARRAY	LOCAL arr			\ <n> <$mid> --- <>
		  0 ?DO		
		        READ-INFILE 		\ <> --- <a1> <u1> <a2> <u2>
		        TO I 1+ (( arr )) DO$ARRAY
		        TO I    (( arr )) DO$ARRAY
		2 +LOOP 
		REFILL 0= ABORT" REFILL: Sorry" ; PRIVATE


	8 $ARRAY random_replies PRIVATE   #40 NEW$ARRAY random_replies

	8  $MID random_replies  READ-$ARRAY
		S" What does that suggest to you?"
		S" Please elaborate on that"
		S" I'm not sure I understand that fully"
		S" Why?"
		S" That's very interesting"
		S" Well....please continue....."
		S" And then?"
		S" I see..Please tell me more about that"


	STRING temp    PRIVATE		#80 NEW temp  
	STRING temp2   PRIVATE		#80 NEW temp2
	STRING temp3   PRIVATE		#80 NEW temp3
	STRING old     PRIVATE		#80 NEW old
	STRING keep    PRIVATE		#80 NEW keep
	STRING work    PRIVATE		#80 NEW work


	#99 =: PUSH!	PRIVATE
	#66 =: PICK!	PRIVATE
	#33 =: EMPTY?	PRIVATE
	#24 =: /lines	PRIVATE
	#80 =: /chars	PRIVATE


: STACK	CREATE	HERE >S  0 , ( addr)  0 , ( sp)		\ <lines> <size> --- <>
		* DUP ALLOCATE ?ALLOCATE
		DUP S> !
		SWAP ERASE
	FORGET>	@ FREE ?ALLOCATE
	DOES>	DUP @ LOCAL $stack
		CELL+ LOCAL $sp
		CASE 
		  PUSH! OF  $stack  $sp @ /chars *  +	\ <c-addr> <u> --- <>
			    PACK DROP  
			    $sp @ 1+  /lines MOD  $sp !
		     ENDOF
		  PICK! OF  $stack  $sp @ CHOOSE	\ <> --- <c-addr> <u>
			    /chars * +  COUNT
		     ENDOF 
		 EMPTY? OF  $sp @ 1 U<			\ <> --- <f>
		     ENDOF
		ENDCASE ; PRIVATE

		/lines /chars STACK CMDS  PRIVATE

: OPENING-MESSAGE	 
		CLS 
		#20 #10 AT-XY Doctorin$	.STRING
		#1000 MS CLS 
		#20 #10 AT-XY Session$	.STRING
		#00 #13 AT-XY Hello$	.STRING ; PRIVATE


: INPUT		BEGIN   
		   CR C/L 2- 0 DO   'Ä' EMIT LOOP
		   CR ." $ "  $ID temp #80 $INPUT  
		   SIZEOF temp 0= IF QUIT ENDIF		\ Empty string
		   temp keep $=	 			\ the same as before!
		WHILE	
		   CR Notrepeat$ .STRING
		REPEAT	
		temp TO keep
		'.' RTRIM temp  '?' RTRIM temp  temp TO old
		Goodbye$ INDEX temp  -1 <> IF CR Farewell$ .STRING
					      CR QUIT
			                ENDIF ; PRIVATE


	#Conjupairs 2* $ARRAY conjugations PRIVATE   8 NEW$ARRAY conjugations

	#Conjupairs 2*  $MID conjugations  2READ-$ARRAY 
		S" are"		S" am"
 		S" am"		S" are"	
		S" you"    	S" me"
		S" my"    	S" your"
		S" your"  	S" my"
		S" was"    	S" were"
		S" mine"	S" yours"
		S" you"    	S" I"
		S" I"	  	S" you"
		S" I've"	S" you've"
		S" you've"	S" I've"
		S" you are"	S" I_am"	
		S" are you"	S" am_I"	
		S" I am"	S" you_are"	
		S" am I"	S" are_you"	
		S" myself" 	S" yourself"
		S" yourself" 	S" myself"


	7 $ARRAY  earlier_remarks  PRIVATE	#60 NEW$ARRAY earlier_remarks

	7  $MID earlier_remarks  READ-$ARRAY  
		S" Please tell me more about your*"
		S" Is there a link here with your*?"
		S" Does that have anything to do with your*?"
		S" Why don't we go back and discuss your* a little more?"
		S" Does any connection between that and your* suggest itself?"
		S" Would you prefer to talk about your*"
		S" I think perhaps worries about your* are bothering you"


: USE-EARLY-REMARKS
		CR EMPTY? CMDS IF 8 CHOOSE random_replies .STRING 
				  EXIT 
			    ENDIF
		7 CHOOSE earlier_remarks 
		0 ?DO	C@+ DUP '*' 
			<> IF EMIT'
			 ELSE DROP PICK! CMDS .STRING
			ENDIF
		 LOOP DROP ; PRIVATE


-- Take first blank-delimited word of userinput, the rest if no delimiter 
-- found.

: NEXT-WORD	BL SPLIT old  				\ <> --- <c-addr> <u>
		   IF DROP 2SWAP TO temp  TO old
		      temp
		 ELSE old  0 0 TO old 
		ENDIF ; PRIVATE

: CONJUGATED	#Conjupairs 2*				\ <addr><u> -- <adr><u>
		  0 ?DO	
			2DUP I conjugations  COMPARE
			0= IF   2DROP
				I 1+ conjugations
				LEAVE
			ENDIF
		2 +LOOP ; PRIVATE

: .CONJUGATED	CONJUGATED .STRING ; PRIVATE		\ <c-addr> <u> --- <>


-- alternative trigger: ``my'' or ``My''

: "MY"-INPUT?	myl$ INDEX old 
		DUP -1 <> IF  3 + #255  MID old  PUSH! CMDS EXIT ENDIF  DROP
		Myu$ INDEX old 
		DUP -1 <> IF  3 + #255  MID old  PUSH! CMDS EXIT ENDIF  DROP ;
		PRIVATE

: echo.it	Areyou$ Are_you$ REPLACE old		\ <> --- <>
		Youare$ You_are$ REPLACE old
		AmI$    Am_I$    REPLACE old
		Iam$    I_am$    REPLACE old
		BEGIN
		   NEXT-WORD DUP
		WHILE  
		   .CONJUGATED
		REPEAT 	2DROP ; PRIVATE

		' echo.it IS ECHO


-- LOOKUP searches in PHRASE only.

: LOOKUP 	\ <c-addr> <u> --- <token> <true> | <false>
		phrase_voc SEARCH-WORDLIST ; PRIVATE


: get$		>S  		 			\ <n> --- <c-addr> <u>
		NEXT-WORD TO temp2			\ could be 0 string
		S> 0  ?DO
			  S" _" +TO temp2
			  NEXT-WORD +TO temp2
		     LOOP 
		temp2 ; PRIVATE


: ?PHRASE	FALSE  					\ <> --- <bool>
		   1 3 DO 
			  old TO work   
			  I get$
			  LOOKUP IF  EXECUTE 0= LEAVE
			       ELSE  work TO old
			      ENDIF
		 -1 +LOOP ; PRIVATE


: ?WORD		FALSE >S  old TO work			\ <> --- <bool>
		BEGIN   
		   NEXT-WORD 
		   DUP  S 0=  AND
		WHILE   
		   LOOKUP IF  EXECUTE S> INVERT >S
		       ENDIF
		REPEAT 2DROP 
		S> DUP FALSE = IF  work TO old  
			    ENDIF ; PRIVATE


	4 $ARRAY w's  PRIVATE	5 NEW$ARRAY w's

	S" Why"	   TO 0 w's
	S" When"   TO 1 w's
	S" Where"  TO 2 w's
	S" Who"	   TO 3 w's


--  Why do I stink ... ==> (why don't YOU tell me)  why you do stink.

: "W"-INPUT?	4 0 DO   
			I w's INDEX old
			0= IF	
				NEXT-WORD   TO temp2  1 BL RPASTE temp2
				S"  " TO temp3
				NEXT-WORD  +TO temp3  1 BL RPASTE temp3
			 	NEXT-WORD  TO+ temp3
				temp3 +TO temp2
				temp2 TO+ old  
				LEAVE
			ENDIF
		  LOOP ; PRIVATE


-- The main word.

: DOCTOR	OPENING-MESSAGE
		BEGIN	
		  INPUT   
		  "MY"-INPUT?
		  ?PHRASE  0= IF "W"-INPUT? 
				 ?WORD 0= IF USE-EARLY-REMARKS ENDIF
			   ENDIF
		  PRINT-?
		AGAIN ;


:ABOUT	CR ." ÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ"
	CR ." Start with:  DOCTOR   <cr> "
	CR ." Stop  with:  Goodbye. <cr> (Case-sensitive, notice the '.')" 
	CR ." ÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ" ;


-- Compare oldinput$ against the trigger phrase vocabulary

#300 =: capacity

	capacity $ARRAY phrases PRIVATE	

	0 VALUE #phrase	        PRIVATE


: TPHRASE	CREATE	#phrase ,	\ <c-addr> <u> ... <c-addr> <u> --- <>
			#Resp 0 DO 
				   DUP NEW (( #phrase )) phrases  
				        TO (( #phrase )) phrases
				   1 +TO #phrase
			      LOOP
		DOES>	@  #Resp CHOOSE +  phrases CR .STRING ; PRIVATE


		.HELP CR



-- Type randomly one of three possible response strings.
-- Add your trigger phrases (about 200 free yet) and amaze your friends...


VOCABULARY PHRASE	ALSO PHRASE DEFINITIONS	  CURRENT @ TO phrase_voc

S" Why do you need*" 
S" Would it really be helpful if you got*"
S" Are you sure you need*"			TPHRASE  I_need

S" Do you really think I don't*" 
S" Perhaps I eventually will*"
S" Do you really want me to*"			TPHRASE Why_don't_you

S" Do you think you should be able to*"
S" Why can't you*"
S" Perhaps you didn't try"			TPHRASE Why_can't_I

S" Why are you interested whether I am or not*"
S" Would you prefer it if I were not*"
S" Perhaps you sometimes dream I am*"		TPHRASE Are_you

S" How do you know you can't*"
S" Have you tried?"
S" Perhaps, now, you can*"			TPHRASE I_can't

S" Did you come to me because you are*"
S" Do you think it is absolutely normal to be*"
S" How long have you been*"			TPHRASE I_am

S" Do you enjoy being*"
S" Why tell me you're*"
S" Why are you*"				TPHRASE I'm

S" What would it mean to you if you got*" 
S" Why do you want*"
S" What would it add to your life if you got*" 
						TPHRASE I_want

S" Why do you ask?"  
S" How would an answer to that help you?"
S" What do you think?"				TPHRASE What

S" How would you solve that?"
S" It would be best to answer that for yourself"
S" What is it you're really asking?"		TPHRASE How

S" Do you often think about such questions?"
S" What answer would put your mind at rest?"
S" Who do you think*"				TPHRASE Who

S" That's a pretty silly question"
S" Do you really need to know where*"
S" What would it mean to you if I told you where*"
						TPHRASE Where

S" Things have a habit of happening more or less at the right time"
S" The time should not be discussed here"
S" How should I know when*"			TPHRASE When

S" Please repeat the information needed to tell you why*"
S" Why don't y o u tell me the reason why*"
S" Do you really need to know why*"     	TPHRASE Why

S" Is that the real reason?" 
S" What else does that explain?"
S" What other reasons come to mind?"    	TPHRASE Because

S" In what other circumstances do you apologize?"
S" There are many times when no apology is needed"
S" What feelings do you have when you apologize?" 
						TPHRASE Sorry

S" How are you.. I'm looking forward to another chat with you"
S" Hello to you.. I'm glad you could drop by today"
S" Hello.. it's good to see you"		TPHRASE Hello

S" Hi there.. I'm glad to see you here today"
S" Hi. I'm glad you've dropped by......we've got lots of time to chat"
S" Hi to you..relax now, and let's talk about your situation"
						TPHRASE Hi

S" You seem a little hesitant" 
S" That's pretty indecisive"
S" In what other situations do you show such a tentative approach?"
						TPHRASE Maybe

S" That's pretty forceful. What does it suggest to you?"
S" Are you saying that just to be negative"
S" Why are you being so negative about it?" 	TPHRASE No

S" Please give me a specific example"  
S" When?"
S" Isn't `ALWAYS' a little strong?"		TPHRASE Always

S" Do you doubt*"  
S" Do you really think so?"
S" But you are not sure*"			TPHRASE I_think

S" Why do you bring up the subject of friends?"
S" Please tell me more about your friendship.."
S" What is your best memory of a friend?"	TPHRASE friend

S" In what way do your friends' reactions bother you?"
S" What made you start to talk about friends just now?"
S" In what way do your friends impose on you?"	TPHRASE friends

S" What feelings do you get, sitting there talking to me like this?"
S" Are you thinking about me in particular"
S" What aspect of computers interests you the most?"
						TPHRASE computer

S" How do you dare bring up such obscene subject matter!"
S" Oh no, we are NOT going to describe our sex life are we!"
S" Why not discuss something more down to earth, like your stamp collection?"
						TPHRASE tForth

S" Work... I can look at it for ages"
S" I know what it is when your boss hates you"
S" It is a universal problem, but that's no solace"
						TPHRASE work

S" How sick can you get."
S" Read about that thing in Reader's Digest. You mean FORTRAN eeh?"
S" Does your wife know that you still have the habit?"
						TPHRASE Forth

S" That's my man! I like seeing too, especially pretty women"
S" Read about that thing in Fortune Magazine. Are you a millionaire yet?"
S" Any other perversities? You still beat your wife and kids?"
						TPHRASE C

S" Do you think it is*"	 
S" In what circumstances would it*"
S" It could well be that*"			TPHRASE Is_it

S" What degree of certainty would you place on it being*"
S" Are you certain that it's*"
S" What emotions would you feel if I told you that it probably isn't*"
						TPHRASE It_is

S" What makes you think I can't*"  
S" Don't you think that I can*"
S" Perhaps you would like to be able to*"	TPHRASE Can_you

S" Perhaps you don't want to*"
S" Do you want to be able to*"
S" I doubt it"					TPHRASE Can_I

S" Why do you think I am*"
S" Perhaps you would like to be*"
S" Does it please you to believe I am*"		TPHRASE You_are

S" Why do you think I am*"
S" Why do you say I'm*"
S" Does it please you to believe I am*"		TPHRASE You're

S" Don't you really*"  
S" Why don't you*"
S" Do you want to be able to*"			TPHRASE I_don't

S" Tell me more about such feelings" 
S" Do you often feel*"
S" Do you enjoy feeling*"			TPHRASE I_feel

S" Let's explore that statement a bit"
S" What emotions do such feelings stir up in you?"
S" Do you often feel like that?"		TPHRASE feel

S" Why tell me that you've*"
S" How can I help you with*"
S" It's obvious to me that you have*"		TPHRASE I_have

S" Could you explain why you would*"
S" How sure are you that you would*"
S" Who else have you told you would*"		TPHRASE I_would

S" Of course there is*" 
S" It's likely that there is*"
S" Would you like there to be*"			TPHRASE Is_there

S" What does it mean to you, that your*"
S" That's interesting! You really said your*, didn't you?"
S" I see, your*"				TPHRASE My

S" This session is to help you...not to discuss me"
S" What prompted you to say that about me?"
S" Remember, I'm taking notes on all this to solve your situation"
						TPHRASE You


			 ONLY FORTH DEFINITIONS

	CR #phrase DEC. .( strings used, out of ) capacity DEC. CR

	DEPRIVE

			      (* End of Source *) 
