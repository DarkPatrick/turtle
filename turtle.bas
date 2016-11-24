#include "fbgfx.bi"
#If __FB_LANG__ = "fb"
#include "file.bi"
#include "consts.bas"
#include "rndlist.bas"
Using FB
#EndIf

Type TVar
  Sign As String * 5
  Value As LongInt
End Type

Type TImage
  W As Integer
  H As Integer
  Im As Any Ptr
End Type

Type TAction
  action( 1 To MAX_ACTIONS ) As UByte
  actions As Integer = 0
End Type

Type TPoint
  x As Double
  y As Double
End Type

Type TTurtle
  phi As Double
  vx As Double
  vy As Double
  crd As TPoint
  lostTurtleCdr( 1 To STACK_DEPTH ) As TPoint
  lostTurtlePhi( 1 To STACK_DEPTH ) As Double
  numOfPaths As ULongInt
  rule( 1 To MAX_RULES ) As TAction
  ruleSign( 1 To MAX_RULES ) As String = { "=" }
  rules As Short = 0
  randNumbers( 1 To MAX_RULES ) As Integer
  randNumber( 1 To MAX_RULES, 1 To MAX_ACTIONS ) As Short
  goodRnd( 0 To STACK_DEPTH ) As Byte = { FALSE }
  brackets As ULongInt
  addBrackets As ULongInt
  openBrackets( 0 To STACK_DEPTH ) As ULongInt
  openAddBrackets( 0 To STACK_DEPTH ) As ULongInt
  randPoints As ULongInt
End Type

Type TSemiTurtle
  rule As TAction
  randNumbers As Integer
  randNumber( 1 To MAX_ACTIONS ) As Short
End Type

Dim Shared As String CDir
CDir = CurDir()
Dim Shared As LongInt XScreen, YScreen
Dim Shared As String word, rule1, rule2
Dim Shared As ULongInt level, points = 0
Dim Shared As Double phi, tempPhi
Dim Shared As TTurtle turtle
Dim Shared As TImage GlobalIm, ScreenIm
Dim Shared As Integer xMin, xMax, yMin, yMax
Dim Shared As UInteger fracColor = RGB( 0, 255, 0 )
Dim Shared As Integer scale = STANDART_SCALE
Dim Shared As Double revx, revy
Dim Shared As LongInt startPointer, Vars
startPointer = Rnd() * LIST_SIZE
Dim Shared As Double scaleK = 1.1
Dim Shared simpleVar( 1 To 100 ) As TVar

Declare Sub default()
Declare Sub realizeGrammar( word As String, rule As UByte )
Declare Sub readRule( ruleN As Integer, fileName As String )
Declare Sub readGrammarFromFile()
Declare Sub enterData()
Declare Sub runLittleTurtle( recLevel As ULongInt, ruleN As UByte, needsDrawing As Byte )
Declare Sub doIt()
Declare Sub drawMap()
Declare Function movement() As Byte
Declare Function scaleMap() As Byte
Declare sub save()
Declare Sub imageActions()
Declare Sub mainMenu()
Declare Sub newGame()

Screen 20, 32,,GFX_WINDOWED

Cls

Sub default()
  xMin = MAX_LEN
  yMin = MAX_LEN
  xMax = -MAX_LEN
  yMax = -MAX_LEN
  XScreen = SCREEN_WIDTH / 2
  YScreen = SCREEN_HEIGHT / 2
  ListPointer = startPointer
End Sub

Sub realizeGrammar( word As String, rule As UByte )
Dim As Integer i, j, news = 0
Dim As String st1, w
Dim As Integer figNum = 0
Dim As Integer randNum
  If ( rule > turtle.rules ) Then turtle.rules = rule
  turtle.rule( rule ).actions = Len( word )
  turtle.randNumbers( rule ) = 0
  For i = 0 To Len( word ) - 1 - figNum
    If ( i > Len( word ) - 1 - figNum ) Then Exit For
    If ( Chr( word[ i + figNum ] ) = "+" ) Then
      turtle.rule( rule ).action( i + 1 ) = PLUS
    ElseIf ( Chr( word[ i + figNum ] ) = "-" ) Then
      turtle.rule( rule ).action( i + 1 ) = MINUS
    ElseIf ( Chr( word[ i + figNum ] ) = "[" ) Then
      turtle.rule( rule ).action( i + 1 ) = NEW_BRANCH
    ElseIf ( Chr( word[ i + figNum ] ) = "]" ) Then
      turtle.rule( rule ).action( i + 1 ) = END_BRANCH
    ElseIf ( Chr( word[ i + figNum ] ) = "{" ) Then
      turtle.rule( rule ).action( i + 1 ) = IF_BEGIN
    ElseIf ( Chr( word[ i + figNum ] ) = "}" ) Then
      turtle.rule( rule ).action( i + 1 ) = IF_END
    ElseIf ( Chr( word[ i + figNum ] ) = "(" ) Then
      turtle.rule( rule ).action( i + 1 ) = ELSE_BEGIN
    ElseIf ( Chr( word[ i + figNum ] ) = ")" ) Then
      turtle.rule( rule ).action( i + 1 ) = ELSE_END
    ElseIf ( Chr( word[ i + figNum ] ) = "!" ) Then
      turtle.rule( rule ).action( i + 1 ) = BREAK
    ElseIf ( Chr( word[ i + figNum ] ) = "#" ) Then
      turtle.rule( rule ).action( i + 1 ) = FORGOT_BRANCH
    ElseIf ( ( word[ i + figNum ] > 47 ) And ( word[ i + figNum ] < 58 ) ) Then
      turtle.rule( rule ).action( i + 1 ) = RAND
      randNum = word[ i + figNum ] - 48
      While ( ( word[ i + figNum + 1 ] > 47 ) And ( word[ i + figNum + 1 ] < 58 ) )
        figNum += 1
        randNum = randNum * 10 + ( word[ i + figNum ] - 48 )
      Wend
      turtle.randNumbers( rule ) += 1
      turtle.randNumber( rule, turtle.randNumbers( rule ) ) = randNum
    Else
      For j = 1 To turtle.rules
        If ( turtle.ruleSign( j ) = Chr( word[ i + figNum ] ) ) Then
          turtle.rule( rule ).action( i + 1 ) = j
          j = 0
          Exit For
        EndIf
      Next j
      If ( j > 0 ) Then
        news += 1
        turtle.ruleSign( turtle.rules + 1 ) = chr( word[ i + figNum ] )
        Print( Chr( word[ i + figNum ] ) + "=" );
        Input w
        turtle.rule( rule ).action( i + 1 ) = rule + news
        If ( Chr( word[ i + figNum ] ) = "F" ) Then GO_AND_DRAW = rule + news
        If ( Chr( word[ i + figNum ] ) = "b" ) Then GO = rule + news
        realizeGrammar( w, rule + news )
      EndIf
    EndIf
  Next i
  turtle.rule( rule ).actions -= figNum
End Sub

Sub readRule( ruleN As Integer, fileName As String )
Dim As String s
Dim As Integer figNum, randNum, start
Dim As Integer j, k
  Open fileName For Input As #3
  For j = 1 To ruleN
    Input #3, s
    If ( Chr( s[ 0 ] ) = "/" ) Then j -= 1
  Next j
  If ( Chr( s[ 0 ] ) = "?" ) Then
    start = -2
    Print( "current rule" )
    For j = 1 To Len( s ) - 1
      Print( Chr( s[ j ] ) );
    Next j
    Print( "" )
    Print( "set new rule" )
    Print( Chr( s[ 1 ] ) + "=" );
    s = ""
    Input "", s
  Else
    start = 0
  EndIf
  figNum = start
  turtle.randNumbers( ruleN ) = 0
  turtle.rule( ruleN ).actions = Len( s ) - 2
  For j = 1 To turtle.rule( ruleN ).actions - figNum
    If ( j > turtle.rule( ruleN ).actions - figNum ) Then Exit For
    Select Case Chr( s[ j + 1 + figNum ] )
      Case "-"
        turtle.rule( ruleN ).action( j ) = MINUS
      Case "+"
        turtle.rule( ruleN ).action( j ) = PLUS
      Case "["
        turtle.rule( ruleN ).action( j ) = NEW_BRANCH
      Case "]"
        turtle.rule( ruleN ).action( j ) = END_BRANCH
      Case "{"
        turtle.rule( ruleN ).action( j ) = IF_BEGIN
      Case "}"
        turtle.rule( ruleN ).action( j ) = IF_END
      Case "("
        turtle.rule( ruleN ).action( j ) = ELSE_BEGIN
      Case ")"
        turtle.rule( ruleN ).action( j ) = ELSE_END
      Case "!"
        turtle.rule( ruleN ).action( j ) = BREAK
      Case "#"
        turtle.rule( ruleN ).action( j ) = FORGOT_BRANCH
    	Case "<":
        turtle.rule( ruleN ).action( j ) = START_VAR
    	Case ">":
    	  turtle.rule( ruleN ).action( j ) = END_VAR
    	Case "0" To "9"
        turtle.rule( ruleN ).action( j ) = RAND
        randNum = s[ j + 1 + figNum ] - 48
        While ( ( s[ j + figNum + 2 ] > 47 ) And ( s[ j + figNum + 2 ] < 58 ) )
          figNum += 1
          randNum = randNum * 10 + ( s[ j + figNum + 1 ] - 48 )
        Wend
        turtle.randNumbers( ruleN ) += 1
        turtle.randNumber( ruleN, turtle.randNumbers( ruleN ) ) = randNum
    	Case Else
        For k = 1 To turtle.rules
          If ( turtle.ruleSign( k ) = Chr( s[ j + 1 + figNum ] ) ) Then
            turtle.rule( ruleN ).action( j ) = k
            If ( turtle.ruleSign( k ) = "F" ) Then GO_AND_DRAW = k
            If ( turtle.ruleSign( k ) = "b" ) Then GO = k
            Exit For
          Else
            turtle.rule( ruleN ).action( j ) = Chr( s[ j + 1 + figNum ] )
          EndIf
        Next k
    End Select
  Next j
  turtle.rule( ruleN ).actions -= figNum
  Close #3
End Sub

Sub readGrammarFromFile()
Dim As Integer i, j, k
Dim As String s, st
Dim As Integer randNum, figNum, start = 0
  s = ""
  For i = 1 To Len( word ) - 1
    s += Chr( word[ i ] )
  Next i
  ChDir( CDir + "\saves\" )
  Open s For Input As #2
    turtle.rules = 0
    While ( Not( Eof( 2 ) ) )
      Input #2, st
      If ( Chr( st[ 0 ] ) <> "/" ) Then
        turtle.rules += 1
        If ( Chr( st[ 0 ] ) <> "?" ) Then turtle.ruleSign( turtle.rules ) = Chr( st[ 0 ] ) Else turtle.ruleSign( turtle.rules ) = Chr( st[ 1 ] )
      Else
        For i = 1 To Len( st ) - 1
          Print( Chr( st[ i ] ) );
        Next i
        Print( "" )
      EndIf
    Wend
  Close #2
  For i = 1 To turtle.rules
    readRule( i, s )
  Next i
  ChDir( CDir )
End Sub

Sub enterData()
  turtle.ruleSign( 1 ) = "="
  Input "enter grammar: ", word
  If ( Chr( word[ 0 ] ) = "~" ) Then
    readGrammarFromFile()
  Else
    realizeGrammar( word, 1 )
  EndIf
  Input "enter number of movements: ", level
  Print( "" )
  Input "phi(grad): ", phi
  phi = phi * PI / 180
  Input "turtle phi(grad): ", turtle.phi
  turtle.phi = turtle.phi * PI / 180
  tempPhi = turtle.phi
  Input "vx: ", turtle.vx
  Input "vy: ", turtle.vy
  revx = turtle.vx
  revy = turtle.vy
End Sub

Sub runLittleTurtle( recLevel As ULongInt, ruleN As UByte, needsDrawing As Byte )
Dim As Integer i, j
Dim As Integer x, y, randsPassed = 0
  For i = 1 To turtle.rule( ruleN ).actions
    If ( ( turtle.goodRnd( turtle.randPoints ) <> TRUE ) And ( turtle.brackets > turtle.openBrackets( turtle.randPoints ) ) ) Then
      If ( turtle.rule( ruleN ).action( i ) = IF_BEGIN ) Then turtle.brackets += 1
      If ( turtle.rule( ruleN ).action( i ) = IF_END ) And ( turtle.brackets > 0 ) Then
        turtle.brackets -= 1
        If ( turtle.brackets = turtle.openBrackets( turtle.randPoints ) ) Then turtle.randPoints -= 1
      EndIf
    Else
      If ( ( turtle.goodRnd( turtle.randPoints ) <> FALSE ) And ( turtle.addBrackets > turtle.openAddBrackets( turtle.randPoints ) ) ) Then
        If ( turtle.rule( ruleN ).action( i ) = ELSE_BEGIN ) Then turtle.addBrackets += 1
        If ( turtle.rule( ruleN ).action( i ) = ELSE_END ) And ( turtle.addBrackets > 0 ) Then
          turtle.addBrackets -= 1
          If ( turtle.addBrackets = turtle.openAddBrackets( turtle.randPoints ) ) Then turtle.randPoints -= 1
        EndIf
      Else
        Select Case turtle.rule( ruleN ).action( i )
          Case GO_AND_DRAW
        	   If ( recLevel = 0 ) Then
        	     If ( NeedsDrawing = TRUE ) Then
        	       x = turtle.crd.x
        	       y = turtle.crd.y
        	     Else
        	       If ( turtle.crd.x < xMin ) Then xMin = turtle.crd.x
                If ( turtle.crd.y < yMin ) Then yMin = turtle.crd.y
                If ( turtle.crd.x > xMax ) Then xMax = turtle.crd.x
                If ( turtle.crd.y > yMax ) Then yMax = turtle.crd.y
        	     EndIf
               turtle.crd.x += turtle.vx * Cos( turtle.phi )
               turtle.crd.y += turtle.vy * Sin( turtle.phi )
               If ( NeedsDrawing = TRUE ) Then
                 If ( ( x >= xMin ) And ( y >= yMin ) And ( x <= xMax ) And ( y <= yMax ) And ( turtle.crd.x >= xMin ) And ( turtle.crd.y >= yMin ) And ( turtle.crd.x <= xMax ) And ( turtle.crd.y <= yMax ) ) Then Line GlobalIm.Im, ( x - xMin, y - yMin ) - ( turtle.crd.x - xMin, turtle.crd.y - yMin ), fracColor
               Else
                 If ( turtle.crd.x < xMin ) Then xMin = turtle.crd.x
                 If ( turtle.crd.y < yMin ) Then yMin = turtle.crd.y
                 If ( turtle.crd.x > xMax ) Then xMax = turtle.crd.x
                 If ( turtle.crd.y > yMax ) Then yMax = turtle.crd.y
               EndIf
        	   Else
        	     runLittleTurtle( recLevel - 1, GO_AND_DRAW, NeedsDrawing )
        	   EndIf
          Case GO
        	   If ( recLevel = 0 ) Then
              turtle.crd.x += turtle.vx * Cos( turtle.phi )
              turtle.crd.y += turtle.vy * Sin( turtle.phi )
        	   Else
        	     runLittleTurtle( recLevel - 1, GO, NeedsDrawing )
        	   EndIf
          Case PLUS
        	   turtle.phi += phi
        	   If ( turtle.phi > 2 * PI ) Then turtle.phi -= 2 * PI
          Case MINUS
            turtle.phi -= phi
            If ( turtle.phi < -2 * PI ) Then turtle.phi += 2 * PI
          Case NEW_BRANCH
            turtle.numOfPaths += 1
            turtle.lostTurtleCdr( turtle.numOfPaths ).x = turtle.crd.x
            turtle.lostTurtleCdr( turtle.numOfPaths ).y = turtle.crd.y
            turtle.lostTurtlePhi( turtle.numOfPaths ) = turtle.phi
          Case END_BRANCH
            If ( turtle.lostTurtlePhi( turtle.numOfPaths ) < 10000 ) Then
              turtle.crd.x = turtle.lostTurtleCdr( turtle.numOfPaths ).x
              turtle.crd.y = turtle.lostTurtleCdr( turtle.numOfPaths ).y
              turtle.phi = turtle.lostTurtlePhi( turtle.numOfPaths )
            EndIf
            turtle.numOfPaths -= 1
        	 Case IF_BEGIN
            turtle.randPoints += 1
            turtle.openBrackets( turtle.randPoints ) = turtle.brackets
            turtle.openAddBrackets( turtle.randPoints ) = MAX_ULONGINT
            turtle.brackets += 1
          Case IF_END
            If ( turtle.brackets > 0 ) Then turtle.brackets -= 1
            turtle.randPoints -= 1
          Case ELSE_BEGIN
            turtle.randPoints += 1
            turtle.openAddBrackets( turtle.randPoints ) = turtle.addBrackets
            turtle.openBrackets( turtle.randPoints ) = MAX_ULONGINT
            turtle.addBrackets += 1
          Case ELSE_END
            If ( turtle.addBrackets > 0 ) Then turtle.addBrackets -= 1
            turtle.randPoints -= 1
          Case BREAK
            For j = turtle.randPoints To 0 Step -1
              If ( j = 0 ) Then Exit Sub
              If ( turtle.goodRnd( j ) <> NOTHING ) Then
                turtle.goodRnd( j ) = NOTHING
                Exit For
              EndIf
            Next j
        	 Case FORGOT_BRANCH
            For j = turtle.numOfPaths To 1 Step -1
              If ( turtle.lostTurtlePhi( j ) < 10000 ) Then
                turtle.lostTurtlePhi( j ) = 15000
                Exit For
              EndIf
            Next j
        	 Case RAND
            randsPassed += 1
            If ( ( GetRndNum() Mod 1000 ) < turtle.randNumber( ruleN, randsPassed ) ) Then
              turtle.goodRnd( turtle.randPoints + 1 ) = TRUE
            Else
              turtle.goodRnd( turtle.randPoints + 1 ) = FALSE
            EndIf
          Case else:
            If ( recLevel > 0 ) Then runLittleTurtle( recLevel - 1, turtle.rule( ruleN ).action( i ), NeedsDrawing ) Else Exit Select
        End Select
      EndIf
    EndIf
  Next i
End Sub

Sub doIt()
  points = 0
  turtle.crd.x = 0
  turtle.crd.y = 0
  turtle.numOfPaths = 0
  turtle.brackets = 0
  turtle.addBrackets = 0
  turtle.phi = tempPhi
  turtle.randPoints = 0
  turtle.openBrackets( 0 ) = MAX_ULONGINT
  turtle.openAddBrackets( 0 ) = MAX_ULONGINT
  ListPointer = startPointer
  runLittleTurtle( level, 1, FALSE )
  If ( xMax = -MAX_LEN )  Then xMax = 0
  If ( yMax = -MAX_LEN )  Then yMax = 0
  If ( xMin = MAX_LEN )  Then xMin = 0
  If ( yMin = MAX_LEN )  Then yMin = 0
  xMax += 1
  yMax += 1
  xMin -= 1
  yMin -= 1
  If ( ( Abs( xMax - xMin ) > MAX_LEN ) Or ( Abs( xMax - yMin ) > MAX_LEN ) ) Then
    xMax = MAX_LEN + xMin
    yMax = MAX_LEN + yMin
  EndIf
  GlobalIm.W = Abs( xMax - xMin ) + 1
  GlobalIm.H = Abs( yMax - yMin ) + 1
  GlobalIm.Im = ImageCreate( GlobalIm.W, GlobalIm.H )
  Line GlobalIm.Im, ( 0, 0 ) - ( GlobalIm.W - 1, GlobalIm.H - 1 ), RGB( 0, 0, 0 ), BF
  turtle.crd.x = 0
  turtle.crd.y = 0
  turtle.numOfPaths = 0
  turtle.brackets = 0
  turtle.addBrackets = 0
  turtle.phi = tempPhi
  ListPointer = startPointer
  turtle.randPoints = 0
  turtle.openBrackets( 0 ) = MAX_ULONGINT
  turtle.openAddBrackets( 0 ) = MAX_ULONGINT
  runLittleTurtle( level, 1, TRUE )
End Sub

Sub drawMap()
  Cls
  Line ScreenIm.Im, ( 0, 0 ) - ( ScreenIm.W - 1, ScreenIm.H - 1 ), RGB( 255, 0, 255 ), BF
    Put ScreenIm.Im, ( xMin + XScreen, yMin + YScreen ), GlobalIm.Im, Trans
  Put ( 0, 0 ),  ScreenIm.Im, Trans
End Sub

Function movement() As Byte
Dim As Byte need = FALSE
  If ( MultiKey( SC_LEFT ) ) Then XScreen += 10: need = TRUE
  If ( MultiKey( SC_UP ) ) Then YScreen += 10: need = TRUE
  If ( MultiKey( SC_RIGHT ) ) Then XScreen -= 10: need = TRUE
  If ( MultiKey( SC_DOWN ) ) Then YScreen -= 10: need = TRUE
Return need
End Function

Function scaleMap() As Byte
Dim As Byte need = FALSE
Dim As Integer oldX, oldY
  If ( MultiKey( SC_MINUS ) ) Then
    turtle.vx /= scaleK
    turtle.vy /= scaleK
    oldX = ( XScreen - xMin ) / scaleK
    oldY = ( YScreen - yMin ) / scaleK
    ImageDestroy( GlobalIm.Im )
    default()
    doIt()
    'XScreen = oldX + xMin
    'YScreen = oldY + yMin
    need = TRUE
  EndIf
  If ( MultiKey( SC_EQUALS ) ) Then
    turtle.vx *= scaleK
    turtle.vy *= scaleK
    ImageDestroy( GlobalIm.Im )
    default()
    doIt()
    need = TRUE
  EndIf
  If ( MultiKey( SC_Z ) ) Then
    turtle.vx = revx
    turtle.vy = revy
    ImageDestroy( GlobalIm.Im )
    default()
    doIt()
    need = TRUE
  EndIf
Return need
End Function

sub save()
Dim As String file, s
Dim As Integer i, j
  ChDir( CDir + "\saves\" )
  Cls
  While ( InKey <> "" )
  Wend
  Input "Enter file name: ", file
  BSave file+ ".bmp", GlobalIm.Im
  Open file + ".txt" For Output As #1
  For i = 1 To turtle.rules
    turtle.randNumbers( i ) = 0
    s = turtle.ruleSign( i ) + "="
    For j = 1 To turtle.rule( i ).actions
      Select Case turtle.rule( i ).action( j )
        Case PLUS
          s += "+"
      	 Case MINUS
      	   s += "-"
      	 Case NEW_BRANCH
          s += "["
      	 Case END_BRANCH
          s += "]"
        Case IF_BEGIN
          s += "{"
        Case IF_END
          s += "}"
        Case ELSE_BEGIN
          s += "("
        Case ELSE_END
          s += ")"
        Case BREAK
          s += "!"
        Case FORGOT_BRANCH
          s += "#"
        Case RAND
          turtle.randNumbers( i ) += 1
          s += Str( turtle.randNumber( i, turtle.randNumbers( i ) ) )
      	 Case Else
          s += turtle.ruleSign( turtle.rule( i ).action( j ) )
      End Select
    Next j
    Print #1, s
  Next i
  Close #1
  ChDir( CDir )
End Sub

Sub imageActions()
Dim As Byte need
Dim As Byte always = TRUE
  While ( always = TRUE )
    need = FALSE
    need = movement()
    If ( need = FALSE ) Then need = scaleMap() Else scaleMap()
    If ( MultiKey( SC_ESCAPE ) ) Then
      mainMenu()
      need = TRUE
    EndIf
    If ( need = TRUE ) Then
      ScreenLock
      drawMap()
      ScreenUnLock
    EndIf
    While ( InKey <> "" )
    Wend
    ScreenSync
  Wend
End Sub

Sub mainMenu()
Dim As Byte always = TRUE
  While ( MultiKey( SC_ESCAPE ) )
  Wend
  While ( always = TRUE )
    Cls
    Print( "ESC -> exit program" )
    Print( "c -> continue" )
    Print( "s -> save current grammar" )
    Print( "n -> create new grammar" )
    ScreenSync
    If ( MultiKey( SC_ESCAPE ) ) Then Stop
    If ( MultiKey( SC_C ) ) Then Exit Sub
    If ( MultiKey( SC_S ) ) Then save()
    If ( MultiKey( SC_N ) ) Then
      ImageDestroy( GlobalIm.Im )
      Clear( turtle,, SizeOf( turtle ) )
      While ( InKey <> "" )
      Wend
      newGame()
      Exit Sub
    EndIf
  Wend
End Sub

Sub newGame()
  ScreenIm.Im = ImageCreate( ScreenIm.W, ScreenIm.H )
  default()
  enterData()
  doIt()
  drawMap()
  'ListPointer = Rnd() * LIST_SIZE
End Sub

'LoadListFromFile( "list#1.trt" )
CreateRandomList( LIST_SIZE )
ScreenIm.W = SCREEN_WIDTH
ScreenIm.H = SCREEN_HEIGHT
newGame()

imageActions()