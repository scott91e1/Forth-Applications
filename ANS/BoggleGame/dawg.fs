\ DAWG.F - Directed Acyclic Word Graph
\
\ By Ian Osgood  iano@quirkster.com

\
\ TRIE/DAWG node structure definition
\
1 30 LSHIFT    CONSTANT EOB_MASK
1 29 LSHIFT    CONSTANT EOW_MASK
1 24 LSHIFT 1- CONSTANT INDEX_MASK

: Let ( node -- 1-26) 24 RSHIFT 31 AND ;
: EOW ( node -- nz )  EOW_MASK AND ;
: EOB ( node -- nz )  EOB_MASK AND ;
: Ind ( node -- index ) INDEX_MASK AND ;

: InitLet ( 1-26 -- node ) 24 LSHIFT ;

: let>c ( 1-26 -- a-z ) [CHAR] a + 1- ;
: c>let ( a-z -- 1-26 ) [CHAR] a - 1+ ;
: ?c>let ( c -- 0-26 )
  DUP [CHAR] a [CHAR] z 1+ WITHIN if c>let ELSE DROP 0 THEN ;

\
\ DAWG usage utilities  (start session with "load-dawg dawg.out")
\

VARIABLE dawg

: read-trie ( fname count -- trie^ code )
  R/O OPEN-FILE            ?DUP IF EXIT THEN ( file )
  DUP FILE-SIZE            ?DUP IF EXIT THEN ( file udsize)
  D>S DUP ALLOCATE         ?DUP IF EXIT THEN ( file size mem^ )
  DUP 2OVER SWAP READ-FILE ?DUP IF EXIT THEN ( file size mem read )
  ROT <>                   ?DUP IF EXIT THEN ( file mem )
  SWAP CLOSE-FILE ;

: load-dawg  BL PARSE read-trie ABORT" Can't load dawg!" dawg ! ;
: unload-dawg  dawg @ FREE DROP ;

: dawg-root ( -- root-block ) dawg @ DUP @ CELLS + ;
: dawg@i ( index -- block ) CELLS dawg @ + ;

: CELL-   1 CELLS - ;
: letter-in-block ( letter block-addr -- node-addr | 0 )
  CELL-
  BEGIN CELL+ 2DUP @ Let - ?DUP 0= IF NIP EXIT THEN
        0< OVER @ EOB OR
  UNTIL
  2DROP 0 ;

