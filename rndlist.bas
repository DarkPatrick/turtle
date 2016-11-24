#Include Once "consts.bas"

Randomize Timer

Const MAX_NUMBER = 100000

ReDim Preserve Shared RandomList( 1 To 0 ) As UInteger

Dim Shared As UInteger ListPointer = 0

Declare Function CreateRandomList( N As UInteger = 1000 ) As Integer
Declare Function GetRndNum() As UInteger
Declare Function SaveListToFile( FileName As String ) As Integer
Declare Function LoadListFromFile( FileName As String ) As Integer

Function CreateRandomList( N As UInteger = 1000 ) As Integer
Dim As Integer I = 0
  ReDim Preserve RandomList( 1 To N )
  For I = 1 To N
    RandomList( I ) = Int( Rnd() * MAX_NUMBER )
  Next I
  Return TRUE
End Function

Function GetRndNum() As UInteger
Dim As Integer UB = 0
  ListPointer += 1
  UB = UBound( RandomList )
  If ( UB = 0 ) Then
    Return 0
    Exit Function
  EndIf
  If ( ListPointer > UB ) Then ListPointer = 1
  Return RandomList( ListPointer )
End Function

Function SaveListToFile( FileName As String ) As Integer
Dim As Integer I, UB = 0
  UB = UBound( RandomList )
  Open FileName For Output As #1
  Close #1
  Open FileName For Append As #1
  Write #1, UB
  For I = 1 To UB
    Write #1, RandomList( I )
  Next I
  Close #1
  Return TRUE
End Function

Function LoadListFromFile( FileName As String ) As Integer
Dim As Integer I, UB = 0
  Open FileName For Input As #1
  Input #1, UB
  ReDim Preserve RandomList( 1 To UB )
  For I = 1 To UB
    Input #1, RandomList( I )
  Next I
  Close #1
  Return TRUE
End Function