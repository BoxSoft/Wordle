                              PROGRAM

  INCLUDE('Errors.clw'),ONCE
  !INCLUDE('AnyScreen.inc'),ONCE

WORD_LENGTH                   EQUATE(5)

                              MAP
LoadWords                       PROCEDURE
CountDoubles                    PROCEDURE
ScoreWords                      PROCEDURE
UI                              PROCEDURE
                                INCLUDE('STDebug.inc')
                              END

WordQ                         QUEUE                         !Dictionary of 5-letter words
Word                            STRING(WORD_LENGTH)
Score                           LONG
                              END

                              ITEMIZE(0)
LetterState:Unknown             EQUATE
LetterState:Nowhere             EQUATE
LetterState:Missed              EQUATE
LetterState:Correct             EQUATE
                              END

LetterKeyClass                CLASS,TYPE                    !Class to manage clickable letter keys
State                           BYTE                        !From LetterState:*
StringFEQ                       SIGNED                      !Letter key STRING
RegionFEQ                       SIGNED                      !Created REGION overlapping STRING
Construct                       PROCEDURE
InitControl                     PROCEDURE(SIGNED StringFEQ) !Initialize screen for letter key STRING
Display                         PROCEDURE                   !Adjust display for the current State
SetState                        PROCEDURE(BYTE State)       !Set a particular LetterState:*
TakeEvent                       PROCEDURE                   !Watch for user clicking on the REGION
                              END

LetterQ                       QUEUE                         !Queue of all 26 letters
Letter                          STRING(1)                   !Letter
Score                           LONG                        !Total score of letter
ScorePos                        LONG,DIM(5)                 !Score of letter in each word character position (1-5)
Handler                         &LetterKeyClass                !Instantiated object
                              END

Letters                       CLASS                                      !Class to manage all letters as a collection
Construct                       PROCEDURE
Destruct                        PROCEDURE
FetchLetter                     PROCEDURE(STRING Letter)                 !Utility to fetch LetterQ record
AccumScore                      PROCEDURE(STRING Letter,BYTE Pos)        !Add up Letter score during dictionary analysis
GetScore                        PROCEDURE(STRING Letter,BYTE Pos),LONG   !Get the score for a letter in specified position
InitControl                     PROCEDURE(SIGNED StringFEQ)              !Setup the control for a particular letter key
SetState                        PROCEDURE(STRING Letter,BYTE State)      !Set the state for a particular letter
Display                         PROCEDURE(STRING Correct,STRING Missed)  !Update the display for all letter keys
TakeEvent                       PROCEDURE                                !Handle letter key events (passing down as necessary)
FailsNowhere                    PROCEDURE(STRING Word),BOOL              !Does word contain any letters with LetterState:Nowhere?
                              END

  CODE
  LoadWords()
  !CountDoubles()
  ScoreWords()
  UI()
  
!==============================================================================
LoadWords                     PROCEDURE
WordsFile                       FILE,DRIVER('ASCII'),NAME('.\english-words\words_alpha.txt'),PRE(WF)
                                  RECORD
Word                                STRING(30)
                                  END
                                END
  CODE
  OPEN(WordsFile, 22h)
  SET(WordsFile)
  LOOP
    NEXT(WordsFile)
    IF ERRORCODE() <> NoError THEN BREAK.
    IF LEN(CLIP(WF:Word)) <> WORD_LENGTH THEN CYCLE.
        
    CLEAR(WordQ)
    WordQ.Word = UPPER(WF:Word)
    ADD(WordQ)
  END

!==============================================================================
CountDoubles                  PROCEDURE
W                               LONG,AUTO
C                               BYTE,AUTO
D                               BYTE,AUTO
DoubleQ                         WordQ
  CODE
W LOOP W = 1 TO RECORDS(WordQ)
    GET(WordQ, W)
    LOOP C = 1 TO WORD_LENGTH-1
      LOOP D = C+1 TO WORD_LENGTH
        IF WordQ.Word[D] = WordQ.Word[C]
          DoubleQ = WordQ
          ADD(DoubleQ)
          CYCLE W
        END
      END
    END
  END
  ST::DebugQueue(DoubleQ)

!==============================================================================
ScoreWords                    PROCEDURE
                              MAP
ScoreWord                       PROCEDURE(STRING Word),LONG
                              END
W                               LONG,AUTO
C                               BYTE,AUTO
  CODE
  LOOP W = 1 TO RECORDS(WordQ)
    GET(WordQ, W)
    LOOP C = 1 TO WORD_LENGTH
      Letters.AccumScore(WordQ.Word[C], C)
    END
  END
  SORT(LetterQ, -LetterQ.Score)
  !ST::DebugQueue(LetterQ, 'Letter Scores')

  LOOP W = 1 TO RECORDS(WordQ)
    GET(WordQ, W)
    WordQ.Score = ScoreWord(WordQ.Word)
    PUT(WordQ)
  END
  SORT(WordQ, -WordQ.Score)
  !ST::DebugQueue(WordQ, 'Word Scores')

!--------------------------------------
ScoreWord                     PROCEDURE(STRING Word)!,LONG
N                               BYTE
Score                           LONG(0)
  CODE
  LOOP N = 1 TO WORD_LENGTH
    !IF N > 1 AND INSTRING(Word[N], Word[1:N-1], 1, 1) !Already scored this letter in this word?
    !  CYCLE
    !END
    Score += Letters.GetScore(Word[N], N)
  END
  RETURN Score
    
!==============================================================================
UI                            PROCEDURE
                              MAP
InitWindow                      PROCEDURE
Compute                         PROCEDURE
                              END

CorrectGroup                    GROUP,PRE
Correct1                          STRING(1)
Correct2                          STRING(1)
Correct3                          STRING(1)
Correct4                          STRING(1)
Correct5                          STRING(1)
                                END
Correct                         STRING(WORD_LENGTH),OVER(CorrectGroup)

MissedGroup                     GROUP,TYPE
Letter                            STRING(1)
Pos1                              BYTE
Pos2                              BYTE
Pos3                              BYTE
Pos4                              BYTE
Pos5                              BYTE
                                END
Missed1                         LIKE(MissedGroup),PRE(Missed1)
Missed2                         LIKE(MissedGroup),PRE(Missed2)
Missed3                         LIKE(MissedGroup),PRE(Missed3)
Missed4                         LIKE(MissedGroup),PRE(Missed4)
Missed5                         LIKE(MissedGroup),PRE(Missed5)

PossibleQ                       WordQ

Window                          WINDOW('Wordle Tool'),AT(,,172,142),CENTER,GRAY,SYSTEM,ICON('Wordle.ico'),FONT('Segoe UI',14,COLOR:White), |
                                    COLOR(COLOR:Black)
                                  PROMPT('&Correct:'),AT(4,4),USE(?Correct:Prompt),TRN
                                  ENTRY(@s1),AT(50,4,10),USE(Correct1),FLAT,CENTER,FONT(,,,FONT:bold),COLOR(04E8D52H),UPR
                                  ENTRY(@s1),AT(63,4,10),USE(Correct2),FLAT,CENTER,FONT(,,,FONT:bold),COLOR(04E8D52H),UPR
                                  ENTRY(@s1),AT(76,4,10),USE(Correct3),FLAT,CENTER,FONT(,,,FONT:bold),COLOR(04E8D52H),UPR
                                  ENTRY(@s1),AT(89,4,10),USE(Correct4),FLAT,CENTER,FONT(,,,FONT:bold),COLOR(04E8D52H),UPR
                                  ENTRY(@s1),AT(102,4,10),USE(Correct5),FLAT,CENTER,FONT(,,,FONT:bold),COLOR(04E8D52H),UPR
                                  PROMPT('&Missed:'),AT(4,24),USE(?Missed1:Prompt),TRN
                                  ENTRY(@s1),AT(34,24,10),USE(Missed1:Letter),FLAT,CENTER,FONT(,,,FONT:bold),COLOR(03B9FB4H),UPR
                                  CHECK,AT(51,24,8),USE(Missed1:Pos1)
                                  CHECK,AT(64,24,8),USE(Missed1:Pos2)
                                  CHECK,AT(77,24,8),USE(Missed1:Pos3)
                                  CHECK,AT(90,24,8),USE(Missed1:Pos4)
                                  CHECK,AT(103,24,8),USE(Missed1:Pos5)
                                  ENTRY(@s1),AT(34,39,10),USE(Missed2:Letter),FLAT,CENTER,FONT(,,,FONT:bold),COLOR(03B9FB4H),UPR
                                  CHECK,AT(51,39,8),USE(Missed2:Pos1)
                                  CHECK,AT(64,39,8),USE(Missed2:Pos2)
                                  CHECK,AT(77,39,8),USE(Missed2:Pos3)
                                  CHECK,AT(90,39,8),USE(Missed2:Pos4)
                                  CHECK,AT(103,39,8),USE(Missed2:Pos5)
                                  ENTRY(@s1),AT(34,54,10),USE(Missed3:Letter),FLAT,CENTER,FONT(,,,FONT:bold),COLOR(03B9FB4H),UPR
                                  CHECK,AT(51,54,8),USE(Missed3:Pos1)
                                  CHECK,AT(64,54,8),USE(Missed3:Pos2)
                                  CHECK,AT(77,54,8),USE(Missed3:Pos3)
                                  CHECK,AT(90,54,8),USE(Missed3:Pos4)
                                  CHECK,AT(103,54,8),USE(Missed3:Pos5)
                                  ENTRY(@s1),AT(34,69,10),USE(Missed4:Letter),FLAT,CENTER,FONT(,,,FONT:bold),COLOR(03B9FB4H),UPR
                                  CHECK,AT(51,69,8),USE(Missed4:Pos1)
                                  CHECK,AT(64,69,8),USE(Missed4:Pos2)
                                  CHECK,AT(77,69,8),USE(Missed4:Pos3)
                                  CHECK,AT(90,69,8),USE(Missed4:Pos4)
                                  CHECK,AT(103,69,8),USE(Missed4:Pos5)
                                  ENTRY(@s1),AT(34,84,10),USE(Missed5:Letter),FLAT,CENTER,FONT(,,,FONT:bold),COLOR(03B9FB4H),UPR
                                  CHECK,AT(51,84,8),USE(Missed5:Pos1)
                                  CHECK,AT(64,84,8),USE(Missed5:Pos2)
                                  CHECK,AT(77,84,8),USE(Missed5:Pos3)
                                  CHECK,AT(90,84,8),USE(Missed5:Pos4)
                                  CHECK,AT(103,84,8),USE(Missed5:Pos5)
                                  STRING('Q'),AT(4,104,10,10),USE(?Letter:Q),CENTER,COLOR(COLOR:Gray)
                                  STRING('W'),AT(16,104,10,10),USE(?Letter:W),CENTER,COLOR(COLOR:Gray)
                                  STRING('E'),AT(28,104,10,10),USE(?Letter:E),CENTER,COLOR(COLOR:Gray)
                                  STRING('R'),AT(40,104,10,10),USE(?Letter:R),CENTER,COLOR(COLOR:Gray)
                                  STRING('T'),AT(52,104,10,10),USE(?Letter:T),CENTER,COLOR(COLOR:Gray)
                                  STRING('Y'),AT(64,104,10,10),USE(?Letter:Y),CENTER,COLOR(COLOR:Gray)
                                  STRING('U'),AT(76,104,10,10),USE(?Letter:U),CENTER,COLOR(COLOR:Gray)
                                  STRING('I'),AT(88,104,10,10),USE(?Letter:I),CENTER,COLOR(COLOR:Gray)
                                  STRING('O'),AT(100,104,10,10),USE(?Letter:O),CENTER,COLOR(COLOR:Gray)
                                  STRING('P'),AT(112,104,10,10),USE(?Letter:P),CENTER,COLOR(COLOR:Gray)
                                  STRING('A'),AT(10,116,10,10),USE(?Letter:A),CENTER,COLOR(COLOR:Gray)
                                  STRING('S'),AT(22,116,10,10),USE(?Letter:S),CENTER,COLOR(COLOR:Gray)
                                  STRING('D'),AT(34,116,10,10),USE(?Letter:D),CENTER,COLOR(COLOR:Gray)
                                  STRING('F'),AT(46,116,10,10),USE(?Letter:F),CENTER,COLOR(COLOR:Gray)
                                  STRING('G'),AT(58,116,10,10),USE(?Letter:G),CENTER,COLOR(COLOR:Gray)
                                  STRING('H'),AT(70,116,10,10),USE(?Letter:H),CENTER,COLOR(COLOR:Gray)
                                  STRING('J'),AT(82,116,10,10),USE(?Letter:J),CENTER,COLOR(COLOR:Gray)
                                  STRING('K'),AT(94,116,10,10),USE(?Letter:K),CENTER,COLOR(COLOR:Gray)
                                  STRING('L'),AT(106,116,10,10),USE(?Letter:L),CENTER,COLOR(COLOR:Gray)
                                  STRING('Z'),AT(22,128,10,10),USE(?Letter:Z),CENTER,COLOR(COLOR:Gray)
                                  STRING('X'),AT(34,128,10,10),USE(?Letter:X),CENTER,COLOR(COLOR:Gray)
                                  STRING('C'),AT(46,128,10,10),USE(?Letter:C),CENTER,COLOR(COLOR:Gray)
                                  STRING('V'),AT(58,128,10,10),USE(?Letter:V),CENTER,COLOR(COLOR:Gray)
                                  STRING('B'),AT(70,128,10,10),USE(?Letter:B),CENTER,COLOR(COLOR:Gray)
                                  STRING('N'),AT(82,128,10,10),USE(?Letter:N),CENTER,COLOR(COLOR:Gray)
                                  STRING('M'),AT(94,128,10,10),USE(?Letter:M),CENTER,COLOR(COLOR:Gray)
                                  LIST,AT(127,4,42,134),USE(?PossibleList),FLAT,NOBAR,VSCROLL,FROM(PossibleQ), |
                                      FORMAT('30L(2)@S5@')
                                END

  CODE
  OPEN(Window)
  InitWindow()
  ACCEPT
    Letters.TakeEvent()
    CASE EVENT()
    OF EVENT:OpenWindow
      Compute()
    OF EVENT:Accepted
      Compute()
      Letters.Display(Correct, | 
          Missed1.Letter &     |
          Missed2.Letter &     |
          Missed3.Letter &     |
          Missed4.Letter &     |
          Missed5.Letter )
    END
  END

!--------------------------------------
InitWindow                    PROCEDURE
StringFEQ                       SIGNED,AUTO
  CODE
  LOOP StringFEQ = ?Letter:Q TO ?Letter:M
    Letters.InitControl(StringFEQ)
  END
  
!--------------------------------------
Compute                       PROCEDURE
                              MAP
FailsCorrect                    PROCEDURE,BOOL
FailsMissed                     PROCEDURE(*MissedGroup Missed),BOOL
                              END
W                               LONG
  CODE
  FREE(PossibleQ)
  LOOP W = 1 TO RECORDS(WordQ)
    GET(WordQ, W)

    IF FailsCorrect() THEN CYCLE.
    
    IF FailsMissed(Missed1) THEN CYCLE.
    IF FailsMissed(Missed2) THEN CYCLE.
    IF FailsMissed(Missed3) THEN CYCLE.
    IF FailsMissed(Missed4) THEN CYCLE.
    IF FailsMissed(Missed5) THEN CYCLE.
   
    IF Letters.FailsNowhere(WordQ.Word) THEN CYCLE.

    PossibleQ.Word = LOWER(WordQ.Word)
    ADD(PossibleQ)
  END
  !ST::DebugQueue(PossibleQ)
  DISPLAY(?PossibleList)

FailsCorrect                  PROCEDURE!,BOOL
X                               LONG
  CODE
  IF Correct <> ''
    LOOP X = 1 TO WORD_LENGTH
      CASE Correct[X]
      OF '' OROF WordQ.Word[X]
        !No-op
      ELSE
        RETURN TRUE
      END
    END
  END
  RETURN FALSE

FailsMissed                   PROCEDURE(*MissedGroup Missed)!,BOOL
!MissedGroup                     GROUP,TYPE
!Letter                            STRING(1)
!Pos1                              BYTE
!Pos2                              BYTE
!Pos3                              BYTE
!Pos4                              BYTE
!Pos5                              BYTE
!                                END

M                               GROUP,AUTO
Letter                            STRING(1)
Pos                               BYTE,DIM(WORD_LENGTH)
                                END
C                               BYTE,AUTO
  CODE
  ASSERT(SIZE(MissedGroup)=SIZE(M), 'FailsMissed M group is different size than MissedGroup')

  IF Missed.Letter <> ''
    !The letter needs to be somewhere in the word
    IF NOT INSTRING(Missed.Letter, WordQ.Word)
      RETURN TRUE
    END
    
    !But not in a position were it's specified not to be
    M = Missed
    LOOP C = 1 TO WORD_LENGTH
      IF M.Pos[C] AND WordQ.Word[C] = M.Letter THEN RETURN TRUE.
    END
  END
  RETURN FALSE

!==============================================================================
!==============================================================================
LetterKeyClass.Construct      PROCEDURE
  CODE
  SELF.State = LetterState:Unknown

!==============================================================================
LetterKeyClass.InitControl    PROCEDURE(SIGNED StringFEQ)
X                               LONG,AUTO
Y                               LONG,AUTO
W                               LONG,AUTO
H                               LONG,AUTO
  CODE
  SELF.StringFEQ = StringFEQ
  SELF.RegionFEQ = CREATE(0, CREATE:region)
  GETPOSITION(SELF.StringFEQ, X, Y, W, H)
  SETPOSITION(SELF.RegionFEQ, X, Y, W, H)
  UNHIDE(SELF.RegionFEQ)
  SELF.Display()

!==============================================================================
LetterKeyClass.Display        PROCEDURE
                              MAP
SetColor                        PROCEDURE(LONG Background,LONG Foreground=COLOR:White)
                              END
  CODE
  ASSERT(SELF.StringFEQ <> 0, 'LetterKeyClass.Display StringFEQ=0')
  CASE SELF.State
    ;OF LetterState:Unknown;  SetColor(8684417                 )  !Gray
    ;OF LetterState:Nowhere;  SetColor(3947066, COLOR:LightGray)  !Dark Gray
    ;OF LetterState:Missed ;  SetColor(3907508                 )  !Yellow
    ;OF LetterState:Correct;  SetColor(5147986                 )  !Green
  END
  
  SELF.RegionFEQ{PROP:Cursor} = CHOOSE(~INLIST(SELF.State, LetterState:Missed, LetterState:Correct), | 
      CURSOR:Hand, CURSOR:None)

!--------------------------------------
SetColor                      PROCEDURE(LONG Background,LONG Foreground)
  CODE
  SELF.StringFEQ{PROP:Background} = Background
  SELF.StringFEQ{PROP:FontColor } = Foreground

!==============================================================================
LetterKeyClass.SetState       PROCEDURE(BYTE State)
  CODE
  SELF.State = State
  SELF.Display()
  
!==============================================================================
!Allow toggling the letter key between Nowhere and Unknown.
!If it's Missing or Correct, then beep and leave as is.
LetterKeyClass.TakeEvent      PROCEDURE
  CODE
  ASSERT(SELF.RegionFEQ <> 0, 'LetterKeyClass.TakeEvent RegionFEQ=0')

  IF ACCEPTED() = SELF.RegionFEQ
    CASE SELF.State
      ;OF LetterState:Unknown;  SELF.SetState(LetterState:Nowhere)
      ;OF LetterState:Nowhere;  SELF.SetState(LetterState:Unknown)
      ;ELSE                  ;  BEEP
    END
  END

!==============================================================================
!==============================================================================
Letters.Construct             PROCEDURE
                              MAP
AddLetter                       PROCEDURE(STRING Letter)
                              END
C                               BYTE,AUTO
  CODE
  LOOP C = VAL('A') TO VAL('Z')
    AddLetter(CHR(C))
  END

!--------------------------------------
AddLetter                     PROCEDURE(STRING Letter)
  CODE
  CLEAR(LetterQ)
  LetterQ.Letter   = Letter
  LetterQ.Handler &= NEW LetterKeyClass
  ADD(LetterQ, LetterQ.Letter)

!==============================================================================
Letters.Destruct              PROCEDURE
  CODE
  LOOP WHILE RECORDS(LetterQ)
    GET(LetterQ, 1)
    DISPOSE(LetterQ.Handler)
    DELETE(LetterQ)
  END
  
!==============================================================================
Letters.FetchLetter           PROCEDURE(STRING Letter)
  CODE
  LetterQ.Letter = Letter
  GET(LetterQ, LetterQ.Letter)
  ASSERT(ERRORCODE()=NoError, 'Missing Letter "'& Letter &'" in call to Letters.FetchLetter')

!==============================================================================
Letters.AccumScore            PROCEDURE(STRING Letter,BYTE Pos)
  CODE
  SELF.FetchLetter(Letter)
  LetterQ.Score         += 1
  LetterQ.ScorePos[Pos] += 1
  PUT(LetterQ)

!==============================================================================
Letters.GetScore              PROCEDURE(STRING Letter,BYTE Pos)!,LONG
  CODE
  SELF.FetchLetter(Letter)
  RETURN LetterQ.ScorePos[Pos]

!==============================================================================
Letters.InitControl           PROCEDURE(SIGNED StringFEQ)
  CODE
  SELF.FetchLetter(StringFEQ{PROP:Text})
  LetterQ.Handler.InitControl(StringFEQ)

!==============================================================================
Letters.SetState              PROCEDURE(STRING Letter,BYTE State)
  CODE
  SELF.FetchLetter(Letter)
  LetterQ.Handler.SetState(State)

!==============================================================================
Letters.Display               PROCEDURE(STRING Correct,STRING Missed)
L                               BYTE,AUTO
  CODE
  LOOP L = 1 TO RECORDS(LetterQ)
    GET(LetterQ, L)
    IF INSTRING(LetterQ.Letter, Correct)
      LetterQ.Handler.SetState(LetterState:Correct)
    ELSIF INSTRING(LetterQ.Letter, Missed)
      LetterQ.Handler.SetState(LetterState:Missed)
    ELSIF LetterQ.Handler.State <> LetterState:Nowhere
      LetterQ.Handler.SetState(LetterState:Unknown)
    END
  END

!==============================================================================
Letters.TakeEvent             PROCEDURE
L                               BYTE,AUTO
  CODE
  LOOP L = 1 TO RECORDS(LetterQ)
    GET(LetterQ, L)
    LetterQ.Handler.TakeEvent()
  END

!==============================================================================
Letters.FailsNowhere          PROCEDURE(STRING Word)!,BOOL
L                               BYTE,AUTO
  CODE
  LOOP L = 1 TO RECORDS(LetterQ)
    GET(LetterQ, L)
    IF LetterQ.Handler.State = LetterState:Nowhere
      IF INSTRING(LetterQ.Letter, Word)
        RETURN TRUE
      END
    END
  END
  RETURN FALSE
  