' TIC TAC TOE - SIMULATION v2.1 - Epson RC+ 7.5.4 R1 / C4-A601S
' English-language edition; game logic and motion instructions are unchanged.
' Only for the existing Virtual 3 workcell; no physical vacuum control.
' Robot1; Block1..Block5; Token1..Token4 (case-sensitive names).
' EOAT: Hand mounted on Tool0, Position 0,0,0, Rotation 180,0,0.
' Existing nominal Tool1: 0.17611,-0.08078,95.17244,0,0,0.
'
' ENTRY FUNCTIONS (Run Window):
' TTT_SetupSim: define points, check targets, and save robot1.pts.
'               Does not execute motion or change the configured TCP.
' TTT_ReturnDemo: return Token1 from the center and Block1 from the corner.
'                 Also accepts one or both pieces already in their trays.
' TTT_SimGame: game with motion; requires all nine pieces in their trays.
' TTT_main: console logic test without motion.
'
' Start with no piece attached. First Go to P0 (tested approach posture)
' to establish a downward suction-cup orientation; then Move to P100.
' v2.1 corrects the linear startup that caused error 4250/J3 at P100.
' The scene and TCP remain in the existing virtual controller.
' This ZIP does not replace SimObjects.dat or include virtual configuration.
'
' POINTS: P0 is the validated orientation and posture reference.
' P100 standby. P101..109 block board contacts. P111..119 token contacts.
' P201..205 block source positions. P211..214 token source positions.
' P900..903 and P980..981 are temporary points reserved by these routines.
' Point definitions are rebuilt when a simulation entry function starts.
' TCP heights: source=10; block on board=13.5; token on board=12.
' Horizontal transfer at Z=110. New positions are nominal CAD values;
' TargetOK checks targets, not the complete path or collisions.
'
' Convention: cell=(row-1)*3+column. Columns increase robot X;
' rows increase robot Y. X centers=307,358,409; nominal spacing=51 mm.
' Small Y corrections match the centers in the supplied STEP file.
'
' LJM(...,2) adjusts J4/J6 turns without changing the reference Wrist flag.
' Each segment is normalized using its predecessor and current position.
' A fault preserves pieces and board state; no automatic retry occurs.
' After interruption, reconcile CAD positions and state before restarting.
'
' Prepared and reviewed outside RC+. Build and a full test in RC+ are
' still required. Two individual manual transfers were tested by the user.

Global Integer g_Board(9), g_Origin(9), g_Slot(9)
Global Integer g_Player, g_Result, g_MoveCount, g_UsedX, g_UsedO
Global Integer g_Request, g_Column, g_Row, g_LastError, g_ControllerError
Global Boolean g_Ready, g_Busy, g_Fault

Boolean m_ConsoleInput, g_MotionEnabled, g_UseCAD, g_UsePhysicalIO
Double m_TransferZ
String m_CADObject$, m_MotionStage$

Function TTT_main
    Call TTT_Configure
    Call TTT_RunSession
Fend

Function TTT_SetupSim
    OnErr GoTo setupFault
    Call TTT_Configure
    Call TTT_DefinePoints
    If Not TTT_CheckAllTargets() Then
        Print "Do not start motion. Check the indicated target."
        Exit Function
    EndIf
    SavePoints "robot1.pts"
    Print "Points defined and saved. No motion was executed."
    Print "Next: TTT_ReturnDemo; then TTT_SimGame."
    Exit Function
setupFault:
    g_ControllerError = Err
    g_Fault = True
    Print "SETUP ERROR: ", g_ControllerError
    Print "CAD object: ", m_CADObject$, " / Stage: ", m_MotionStage$
Fend

Function TTT_SimGame
    OnErr GoTo simFault
    Call TTT_Configure
    g_MotionEnabled = True
    Call TTT_DefinePoints
    If Not TTT_CheckAllTargets() Then
        Print "Do not start the game. Check the indicated target."
        Exit Function
    EndIf
    If Not TTT_AllHome() Then
        Print "Do not start the game: pieces are missing from their source positions."
        Print "Use TTT_ReturnDemo for the two pieces from the manual test."
        Exit Function
    EndIf
    Call TTT_RunSession
    Exit Function
simFault:
    g_ControllerError = Err
    g_Fault = True
    g_Ready = False
    g_Busy = True
    Print "SIMULATION STARTUP ERROR: ", g_ControllerError
    Print "CAD object: ", m_CADObject$, " / Stage: ", m_MotionStage$
Fend

Function TTT_ReturnDemo
    Integer slot
    Boolean blockHome, blockBoard, tokenHome, tokenBoard

    OnErr GoTo returnFault
    Call TTT_Configure
    g_MotionEnabled = True
    Call TTT_DefinePoints
    If Not TTT_CheckAllTargets() Then
        Exit Function
    EndIf

    blockHome = TTT_AtPoint(1, 1, 201)
    blockBoard = TTT_AtPoint(1, 1, 101)
    tokenHome = TTT_AtPoint(2, 1, 211)
    tokenBoard = TTT_AtPoint(2, 1, 115)
    If Not blockHome And Not blockBoard Then
        Print "Block1 is neither at its source nor in cell 1."
        Exit Function
    EndIf
    If Not tokenHome And Not tokenBoard Then
        Print "Token1 is neither at its source nor in the center cell."
        Exit Function
    EndIf
    For slot = 2 To 5
        If Not TTT_AtPoint(1, slot, 200 + slot) Then
            Print "Check Block", slot, ": not in its tray."
            Exit Function
        EndIf
    Next slot
    For slot = 2 To 4
        If Not TTT_AtPoint(2, slot, 210 + slot) Then
            Print "Check Token", slot, ": not in its tray."
            Exit Function
        EndIf
    Next slot
    If blockHome And tokenHome Then
        Print "Both pieces are already in their trays."
        Exit Function
    EndIf

    Call TTT_InitMotion
    If tokenBoard Then
        Call TTT_Transfer(115, 211, 2, 1)
    EndIf
    If blockBoard Then
        Call TTT_Transfer(101, 201, 1, 1)
    EndIf
    If TTT_AllHome() Then
        Call TTT_ResetGame
        Print "Return test completed. Board empty."
    Else
        Print "Check the final positions before starting the game."
    EndIf
    SimSet ResetCollision
    Motor Off
    Exit Function
returnFault:
    g_ControllerError = Err
    g_Fault = True
    g_Ready = False
    g_Busy = True
    Print "RETURN DEMO ERROR: ", g_ControllerError
    Print "CAD object: ", m_CADObject$, " / Stage: ", m_MotionStage$
    Print "Keep the scene unchanged and check the piece state."
Fend

Function TTT_Configure
    m_ConsoleInput = True
    g_MotionEnabled = False
    g_UseCAD = True
    g_UsePhysicalIO = False
    m_TransferZ = 110.0
    m_CADObject$ = ""
    m_MotionStage$ = ""
    g_Fault = False
    g_ControllerError = 0
Fend

Function TTT_DefinePoints
    Integer boardRow, boardColumn, cell
    Double x, blockY, tokenY

    Robot 1
    Tool 1
    AutoLJM Off
    m_MotionStage$ = "Define points using P0"
    P100 = P0 :X(358) :Y(0) :Z(110)
    For boardRow = 1 To 3
        For boardColumn = 1 To 3
            cell = (boardRow - 1) * 3 + boardColumn
            x = 307 + (boardColumn - 1) * 51
            blockY = (boardRow - 2) * 51
            tokenY = (boardRow - 2) * 51
            If boardColumn = 1 Then
                blockY = blockY - 0.00381
            EndIf
            If boardRow <> 2 Then
                tokenY = tokenY - 0.00381
            EndIf
            P(100 + cell) = P0 :X(x) :Y(blockY) :Z(13.5)
            P(110 + cell) = P0 :X(x) :Y(tokenY) :Z(12)
        Next boardColumn
    Next boardRow
    P201 = P0 :X(328) :Y(-185) :Z(10)
    P202 = P0 :X(358) :Y(-185) :Z(10)
    P203 = P0 :X(388) :Y(-185) :Z(10)
    P204 = P0 :X(328) :Y(155) :Z(10)
    P205 = P0 :X(358) :Y(155) :Z(10)
    P211 = P0 :X(328) :Y(-155) :Z(10)
    P212 = P0 :X(358) :Y(-155) :Z(10)
    P213 = P0 :X(328) :Y(185) :Z(10)
    P214 = P0 :X(358) :Y(185) :Z(10)
Fend

Function TTT_CheckPoint(pointNumber As Integer) As Boolean
    TTT_CheckPoint = False
    m_MotionStage$ = "Check targets with TargetOK"
    P980 = LJM(P(pointNumber) :Z(m_TransferZ), Here, 2)
    P981 = LJM(P(pointNumber), P980, 2)
    If Not TargetOK(P980) Then
        Print "Approach target out of range: P", pointNumber
        Exit Function
    EndIf
    If Not TargetOK(P981) Then
        Print "Contact target out of range: P", pointNumber
        Exit Function
    EndIf
    TTT_CheckPoint = True
Fend

Function TTT_CheckAllTargets As Boolean
    Integer i

    TTT_CheckAllTargets = False
    If Not TTT_CheckPoint(100) Then
        Exit Function
    EndIf
    For i = 101 To 109
        If Not TTT_CheckPoint(i) Then
            Exit Function
        EndIf
    Next i
    For i = 111 To 119
        If Not TTT_CheckPoint(i) Then
            Exit Function
        EndIf
    Next i
    For i = 201 To 205
        If Not TTT_CheckPoint(i) Then
            Exit Function
        EndIf
    Next i
    For i = 211 To 214
        If Not TTT_CheckPoint(i) Then
            Exit Function
        EndIf
    Next i
    TTT_CheckAllTargets = True
Fend

Function TTT_AtPoint(pieceType As Integer, pieceSlot As Integer, pointNumber As Integer) As Boolean
    String objectName$
    Double x, y, z, expectedX, expectedY, expectedZ

    expectedX = CX(P(pointNumber))
    expectedY = CY(P(pointNumber))
    expectedZ = CZ(P(pointNumber)) - 6
    If pieceType = 1 Then
        objectName$ = "Block" + LTrim$(Str$(pieceSlot))
        ' Block CAD origin differs from its centre; global Rz=90 deg.
        expectedX = expectedX + 10
        expectedY = expectedY - 12
    Else
        objectName$ = "Token" + LTrim$(Str$(pieceSlot))
    EndIf
    m_CADObject$ = objectName$
    m_MotionStage$ = "Read CAD position"
    SimGet objectName$.PositionX, x
    SimGet objectName$.PositionY, y
    SimGet objectName$.PositionZ, z
    TTT_AtPoint = False
    If Abs(x - expectedX) < 0.3 And Abs(y - expectedY) < 0.3 And Abs(z - expectedZ) < 0.3 Then
        TTT_AtPoint = True
    EndIf
Fend

Function TTT_AllHome As Boolean
    Integer slot

    TTT_AllHome = False
    For slot = 1 To 5
        If Not TTT_AtPoint(1, slot, 200 + slot) Then
            Print "Block", slot, " is not at its source position."
            Exit Function
        EndIf
    Next slot
    For slot = 1 To 4
        If Not TTT_AtPoint(2, slot, 210 + slot) Then
            Print "Token", slot, " is not at its source position."
            Exit Function
        EndIf
    Next slot
    TTT_AllHome = True
Fend

Function TTT_InitMotion
    Robot 1
    Tool 1
    AutoLJM Off
    CP Off
    Motor On
    Power Low
    Speed 20
    Accel 20, 20
    SpeedS 800
    AccelS 2500, 2500
    m_CADObject$ = ""
    ' Establish the tested downward pose with PTP; do not force a low-Z CP
    ' reorientation from an arbitrary initial posture.
    m_MotionStage$ = "Initial joint motion to P0"
    Print "MOTION: ", m_MotionStage$
    P900 = LJM(P0, Here, 2)
    Go P900
    WaitPos
    m_MotionStage$ = "Move to standby P100"
    Print "MOTION: ", m_MotionStage$
    P900 = LJM(P100, Here, 2)
    Move P900
    WaitPos
Fend

Function TTT_Transfer(sourcePoint As Integer, targetPoint As Integer, pieceType As Integer, pieceSlot As Integer)
    String pieceName$

    If Not g_MotionEnabled Then
        Print "LOGIC: P", sourcePoint, " -> P", targetPoint
        Exit Function
    EndIf
    If pieceType = 1 Then
        pieceName$ = "Block" + LTrim$(Str$(pieceSlot))
    Else
        pieceName$ = "Token" + LTrim$(Str$(pieceSlot))
    EndIf
    m_CADObject$ = pieceName$
    Robot 1
    Tool 1

    m_MotionStage$ = "Lift before transfer"
    Print "MOTION: ", pieceName$, " / ", m_MotionStage$
    P900 = LJM(Here @0 :Z(m_TransferZ), Here, 2)
    Move P900
    WaitPos

    m_MotionStage$ = "Approach source"
    Print "MOTION: ", pieceName$, " / ", m_MotionStage$
    P901 = LJM(P(sourcePoint) :Z(m_TransferZ), Here, 2)
    P902 = LJM(P(sourcePoint), P901, 2)
    Move P901
    WaitPos
    m_MotionStage$ = "Descend to source"
    Print "MOTION: ", pieceName$, " / ", m_MotionStage$
    Move P902
    WaitPos
    m_MotionStage$ = "Pick CAD object"
    Print "MOTION: ", pieceName$, " / ", m_MotionStage$
    SimSet Robot1.Pick, pieceName$, 1

    m_MotionStage$ = "Lift piece"
    Print "MOTION: ", pieceName$, " / ", m_MotionStage$
    P900 = LJM(Here @0 :Z(m_TransferZ), Here, 2)
    Move P900
    WaitPos

    m_MotionStage$ = "Approach target"
    Print "MOTION: ", pieceName$, " / ", m_MotionStage$
    P901 = LJM(P(targetPoint) :Z(m_TransferZ), Here, 2)
    P903 = LJM(P(targetPoint), P901, 2)
    Move P901
    WaitPos
    m_MotionStage$ = "Descend to target"
    Print "MOTION: ", pieceName$, " / ", m_MotionStage$
    Move P903
    WaitPos
    m_MotionStage$ = "Place CAD object"
    Print "MOTION: ", pieceName$, " / ", m_MotionStage$
    SimSet Robot1.Place, pieceName$

    m_MotionStage$ = "Retract suction cup"
    Print "MOTION: ", pieceName$, " / ", m_MotionStage$
    P900 = LJM(Here @0 :Z(m_TransferZ), Here, 2)
    Move P900
    WaitPos
    SimSet ResetCollision
    m_MotionStage$ = "Transfer completed"
Fend

Function TTT_RunSession
    Integer command, selectedColumn, selectedRow

    OnErr GoTo faultHandler
    Call TTT_ResetGame
    g_Fault = False
    g_ControllerError = 0
    g_Ready = False
    g_Busy = True

    If g_MotionEnabled Then
        Call TTT_InitMotion
    EndIf

    Do
        ' Discard requests received during the previous movement.
        g_Request = 0
        g_Busy = False
        g_Ready = True
        Call TTT_ReadRequest
        g_Ready = False
        g_Busy = True
        command = g_Request
        selectedColumn = g_Column
        selectedRow = g_Row
        g_Request = 0
        g_LastError = 0

        If command = 1 Then
            Call TTT_Play(selectedColumn, selectedRow)
        ElseIf command = 2 Then
            Call TTT_Collect
        ElseIf command = 9 Then
            If g_MoveCount = 0 Then
                Exit Do
            EndIf
            g_LastError = 6
        Else
            g_LastError = 5
        EndIf

        Call TTT_ShowState
    Loop

    g_Request = 0
    g_Ready = False
    g_Busy = False
    If g_MotionEnabled Then
        Motor Off
    EndIf
    Exit Function

faultHandler:
    g_ControllerError = Err
    g_Fault = True
    g_Ready = False
    g_Busy = True
    g_Request = 0
    Print "EPSON ERROR: ", g_ControllerError
    Print "CAD object: ", m_CADObject$, " / Stage: ", m_MotionStage$
    Print "Check the piece state before restarting."
    ' Do not open gripper, clear board or resume motion automatically.
Fend

Function TTT_ResetGame
    Integer cell

    For cell = 0 To 9
        g_Board(cell) = 0
        g_Origin(cell) = 0
        g_Slot(cell) = 0
    Next cell
    g_Player = 1
    g_Result = 0
    g_MoveCount = 0
    g_UsedX = 0
    g_UsedO = 0
    g_LastError = 0
Fend

Function TTT_ReadRequest
    If m_ConsoleInput Then
        Print "1=Confirm move, 2=Clear board, 9=Exit"
        Print "Turn: ", g_Player, " (1=X, 2=O)"
        Input g_Request
        If g_Request = 1 Then
            Print "Column X (1..3):"
            Input g_Column
            Print "Row Y (1..3):"
            Input g_Row
        EndIf
    Else
        ' Requires a real communication adapter to fill this mailbox.
        Wait g_Request <> 0
    EndIf
Fend

Function TTT_Play(selectedColumn As Integer, selectedRow As Integer)
    Integer cell, pieceSlot, sourcePoint, targetPoint

    If g_Result <> 0 Then
        g_LastError = 3
        Exit Function
    EndIf
    If selectedColumn < 1 Or selectedColumn > 3 Then
        g_LastError = 1
        Exit Function
    EndIf
    If selectedRow < 1 Or selectedRow > 3 Then
        g_LastError = 1
        Exit Function
    EndIf

    cell = (selectedRow - 1) * 3 + selectedColumn
    If g_Board(cell) <> 0 Then
        g_LastError = 2
        Exit Function
    EndIf

    If g_Player = 1 Then
        If g_UsedX >= 5 Then
            g_LastError = 4
            Exit Function
        EndIf
        pieceSlot = g_UsedX + 1
        sourcePoint = 200 + pieceSlot
        targetPoint = 100 + cell
    Else
        If g_UsedO >= 4 Then
            g_LastError = 4
            Exit Function
        EndIf
        pieceSlot = g_UsedO + 1
        sourcePoint = 210 + pieceSlot
        targetPoint = 110 + cell
    EndIf

    Call TTT_Transfer(sourcePoint, targetPoint, g_Player, pieceSlot)

    ' Commit the board only after transfer and retraction finish.
    g_Board(cell) = g_Player
    g_Origin(cell) = sourcePoint
    g_Slot(cell) = pieceSlot
    g_MoveCount = g_MoveCount + 1
    If g_Player = 1 Then
        g_UsedX = g_UsedX + 1
    Else
        g_UsedO = g_UsedO + 1
    EndIf

    g_Result = TTT_CheckResult()
    If g_Result = 0 Then
        g_Player = 3 - g_Player
    EndIf
Fend

Function TTT_Line(a As Integer, b As Integer, c As Integer) As Integer
    TTT_Line = 0
    If g_Board(a) <> 0 Then
        If g_Board(a) = g_Board(b) And g_Board(b) = g_Board(c) Then
            TTT_Line = g_Board(a)
        EndIf
    EndIf
Fend

Function TTT_CheckResult As Integer
    Integer lineIndex, result

    TTT_CheckResult = 0
    For lineIndex = 0 To 2
        result = TTT_Line(3 * lineIndex + 1, 3 * lineIndex + 2, 3 * lineIndex + 3)
        If result <> 0 Then
            TTT_CheckResult = result
            Exit Function
        EndIf
    Next lineIndex
    For lineIndex = 1 To 3
        result = TTT_Line(lineIndex, lineIndex + 3, lineIndex + 6)
        If result <> 0 Then
            TTT_CheckResult = result
            Exit Function
        EndIf
    Next lineIndex
    result = TTT_Line(1, 5, 9)
    If result <> 0 Then
        TTT_CheckResult = result
        Exit Function
    EndIf
    result = TTT_Line(3, 5, 7)
    If result <> 0 Then
        TTT_CheckResult = result
        Exit Function
    EndIf
    If g_MoveCount = 9 Then
        TTT_CheckResult = 3
    EndIf
Fend

Function TTT_Collect
    Integer cell, boardPoint, pieceType

    For cell = 9 To 1 Step -1
        pieceType = g_Board(cell)
        If pieceType <> 0 Then
            If pieceType = 1 Then
                boardPoint = 100 + cell
            Else
                boardPoint = 110 + cell
            EndIf
            Call TTT_Transfer(boardPoint, g_Origin(cell), pieceType, g_Slot(cell))
            g_Board(cell) = 0
            g_Origin(cell) = 0
            g_Slot(cell) = 0
        EndIf
    Next cell
    Call TTT_ResetGame
Fend

Function TTT_ShowState
    Print "Board: 0=empty, 1=X, 2=O"
    Print g_Board(1), " ", g_Board(2), " ", g_Board(3)
    Print g_Board(4), " ", g_Board(5), " ", g_Board(6)
    Print g_Board(7), " ", g_Board(8), " ", g_Board(9)
    Print "Error: ", g_LastError
    If g_Result = 1 Then
        Print "X wins. Clear the board for a new game."
    ElseIf g_Result = 2 Then
        Print "O wins. Clear the board for a new game."
    ElseIf g_Result = 3 Then
        Print "Draw. Clear the board for a new game."
    Else
        Print "Next turn: ", g_Player
    EndIf
Fend
' HMI GUI BUILDER v0.1 - APPEND TO Main_TicTacToe_v1.prg
' This function must remain in the SAME file as TTT_Configure:
' m_ConsoleInput and g_MotionEnabled have file scope in v2.1.
' Global HMI variables are declared in frmTTT.prg.
' mode=1: logic only. mode=2: existing Virtual 3 CAD simulation.
' Do not run mode 2 against a physical controller.

Function TTT_HMI_Run(mode As Integer)
    OnErr GoTo hmiFault
    Call TTT_Configure
    m_ConsoleInput = False
    g_HMIMode = mode
    g_HMIMessage$ = ""

    If mode = 2 Then
        g_MotionEnabled = True
        Call TTT_DefinePoints
        If Not TTT_CheckAllTargets() Then
            g_HMIMessage$ = "Check the points listed in the Run Window."
            g_Ready = False
            g_Busy = False
            g_HMIActive = False
            Exit Function
        EndIf
        If Not TTT_AllHome() Then
            g_HMIMessage$ = "Pieces missing from the trays. Check the scene."
            g_Ready = False
            g_Busy = False
            g_HMIActive = False
            Exit Function
        EndIf
    ElseIf mode <> 1 Then
        g_HMIMessage$ = "Invalid HMI mode."
        g_Ready = False
        g_Busy = False
        g_HMIActive = False
        Exit Function
    EndIf

    Call TTT_RunSession
    g_HMIActive = False
    If g_Fault Then
        g_HMIMessage$ = "Error: check the pieces and motion stage in the Run Window."
    Else
        g_HMIMessage$ = "Session ended. Board empty."
    EndIf
    Exit Function

hmiFault:
    g_ControllerError = Err
    g_Fault = True
    g_Ready = False
    g_Busy = True
    g_Request = 0
    g_HMIActive = False
    g_HMIMessage$ = "Startup error. Check the Run Window."
    Print "HMI STARTUP ERROR: ", g_ControllerError
    Print "CAD object: ", m_CADObject$, " / Stage: ", m_MotionStage$
Fend


