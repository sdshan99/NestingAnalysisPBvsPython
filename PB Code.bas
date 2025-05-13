'               Compiler Directives
#COMPILE EXE "G:\ESSDi CAD\Bin\ESSDi AutoMarker.Exe"
#DEBUG ERROR ON
#DIM NONE
#OPTIMIZE SPEED
#OPTION VERSION4

'               Resources
#RESOURCE "Auto Marker.Pbr"

'               Windows Libraries
#INCLUDE "G:\PBWin90\WinAPI\Win32api.Inc"
#INCLUDE "G:\PBWin90\WinAPI\ComDlg32.Inc"
#INCLUDE "G:\PBWin90\WinAPI\CommCtrl.Inc"

'               Protection Library Module
%WM_PROTECT = %WM_USER + 2012

'               Our Variable Types
DEFLNG L, H, C
DEFSTR T
DEFDBL F
DEFSNG S
DEFQUD Q
DEFDWD D
DEFEXT E
DEFINT I

'               Our Libraries
#INCLUDE "Auto Marker.Inc"

'               Our Function Prototypes
#INCLUDE "Auto Marker.bi"

'               Our Dll Declare Functions

DECLARE FUNCTION DISKCHECK LIB "Marker.dll" ALIAS "DiskCheck"() AS STRING
DECLARE FUNCTION SerialCheckDesktop LIB"Marker.dll" ALIAS "SERIALCHECKDesktop"(BYVAL modulename AS STRING) AS LONG
DECLARE FUNCTION ProfilePath LIB"Marker.dll" ALIAS "PROFILEPATH"() AS STRING
DECLARE FUNCTION Protection_Error_Message LIB"Marker.dll" ALIAS "PROTECTIONERRORMESSAGE"() AS LONG
DECLARE FUNCTION client_name LIB "module.dll" ALIAS "client_name"() AS STRING
DECLARE FUNCTION GetProfilePath() AS STRING
'               Main Procedure (Invoked by Windows)

TYPE PolyPoint
  x AS SINGLE
  y AS SINGLE
END TYPE

TYPE PolyArray
  ya AS LONG
  xy(1 TO 1024) AS PolyPoint
END TYPE

GLOBAL UIntersectPointsColumns() AS DataBaseDataEnvironmentType     '   today
GLOBAL UPolygons() AS DataBaseDataEnvironmentType                   '   today
GLOBAL UPolyCenter() AS DataBaseDataEnvironmentType                 '   today

FUNCTION PBMAIN() AS LONG
    '   For testing splash
    AdminUser = 1
    '   Check if any duplication of file(.Lay)
    LResult = EnumChildWindows(GetDesktopWindow, CODEPTR(ChildCallbackStart), 0&)       '   2021 GD
    '   If the file is already open then show msg
    IF AdminUser < 0 THEN EXIT FUNCTION                                                 '   2021 GD
    '   Generate random sequences
    RANDOMIZE TIMER
    '   A primary check...
    module$ = CHR$(65,117,116,111,77,97,114,107,101,114,46,101,120,101)
    l_Serial_License = 1' SerialCheckDesktop(module$)
    '   Initialise the Startup Dialog
    'ShowStartupPicture %True                                       '   2021 GD
    '   Wait till the protection thread is done...
    '  While g_l_thread_started = %True: Sleep (50): Wend
    IF VAL(GetProfilePath ()) THEN EXIT FUNCTION
    '   Initialise Variables
    IF l_Serial_License THEN InitialiseVariables
    '   If the Check up is valid then proceed
    IF l_Serial_License THEN InitialiseTheScreen
    '   Terminate Variables
    IF l_Serial_License THEN TerminateVariables
    '   If the license is invalid then say so...
    IF l_Serial_License = %False THEN Protection_Error_Message ()
END FUNCTION


SUB AutomaticPlacementForWidth (BYVAL SFabricWidth)              '2020
    '   Get the handle of the Dialog
    HDialog = LModule(%LONG_MODULE_MAIN_HANDLE)
    '   Get the Handle of the Wait Cursor
    HCursor = LModule(%LONG_MODULE_WAIT_CURSOR)
    '   Reset the cursor to Arrow
    HPrevious = SetClassLong (HDialog, %GCL_HCURSOR, HCursor)
    '   Get the Screen Aspect Ratio
    SScrnAspect  = SModule(%SINGLE_MODULE_SCREEN_ASPECT)
    '   Get the Zoom Factor
    SZoomFactor  = SModule(%SINGLE_MODULE_ZOOM_FACTOR)
    '   Get the Actual Aspect of the Drawing
    SAspect = SScrnAspect * SZoomFactor
    '   Set Thread variable
    ThreadVariable = 1
    '   Create the thrtead for interrupt
    THREAD CREATE MyThread(0) TO DThread
    '   Move the scroll bar to starting pos
    MoveTheScrollBarHorizontally HDialog,MAKLNG(%SB_PAGEUP,0), 0
    '*'*'*'*'*'*'*'*'*'*'*'*'*'*'*'*'*'*'*'*'*'*'*'*'*'*'*'*'*'*'*'*'*'*'*'*'*'*'*'*'*'*'*'*'*'*'*'*'*'*'*'*'*'*'*'*'*
    '   Parse thru all the Pieces
    FOR LMarkerIndex = %Vert_First TO UBOUND(UMarkerPlacement)
        '   Set the State of this Piece to be Not Placed
        UMarkerOthers(LMarkerIndex).SHorizontal = %MARKER_PIECE_NOTPLACED
        '   Set default flip to piece
        IF UMarkerStatus(LMarkerIndex).SHorizontal > %Vert_Zero THEN UMarkerStatus(LMarkerIndex).SHorizontal = %MARKER_PIECE_CANBEFLIPPED
        '   Set the Status of this Piece to canbefolded if fold enables
        IF UMarkerStatus(LMarkerIndex).LDataType THEN UMarkerStatus(LMarkerIndex).LDataType = %MARKER_PIECE_CANBEFOLDED
        '   Set the given angle                 '   2021 GD December
        UMarkerPlacement(LMarkerIndex).SExtraData = Rapper_GetFromSingleArray (TToolEnvironment(%CONTROL_SETUP_PIECE_BIASANGLE), UMarkerIndex(LMarkerIndex).SVertical)
        '   Redefine the Dimensions & Area of the Piece
        SaveTheDimensionOfThePiece LMarkerIndex
    NEXT
    '*'*'*'*'*'*'*'*'*'*'*'*'*'*'*'*'*'*'*'*'*'*'*'*'*'*'*'*'*'*'*'*'*'*'*'*'*'*'*'*'*'*'*'*'*'*'*'*'*'*'*'*'*'*'*'*'*
    '   Storage area for the tubular piece & mirror direction
    REDIM LUMarkerTubular(UBOUND(UMarkerPlacement)), LmirrorDirection(1 TO UBOUND(UMarkerPieces, 2))
    '   Set the open/tubular for all pieces
    ApplyOpenTubularForAllPieces LUMarkerTubular(), LmirrorDirection()
    '*'*'*'*'*'*'*'*'*'*'*'*'*'*'*'*'*'*'*'*'*'*'*'*'*'*'*'*'*'*'*'*'*'*'*'*'*'*'*'*'*'*'*'*'*'*'*'*'*'*'*'*'*'*'*'*'*
    '   Set the flip for all pieces
    ApplyFlipForAllPieces
    '*'*'*'*'*'*'*'*'*'*'*'*'*'*'*'*'*'*'*'*'*'*'*'*'*'*'*'*'*'*'*'*'*'*'*'*'*'*'*'*'*'*'*'*'*'*'*'*'*'*'*'*'*'*'*'*'*
    '   Get the Left Selv Edge Allowance
    SLeftAllowance = SMarkerEnviron(%SINGLE_MARKER_FABRICLEFTALLOWANCE)
    '   Get the Right Selv Edge Allowance
    SRightAllowance = SMarkerEnviron(%SINGLE_MARKER_FABRICRIGHTALLOWANCE)
    '   Get the Top Selv Edge Allowance
    STopAllowance = SMarkerEnviron(%SINGLE_MARKER_FABRICTOPALLOWANCE)
    '   Get the Down Selv Edge Allowance
    SDownAllowance = SMarkerEnviron(%SINGLE_MARKER_FABRICDOWNALLOWANCE)
    '   Default values for bestmarker
    TLogicTime = FORMAT$(TIMER,"#") : CombinationMarker = 0 : SBestCombination = 0
    '   Get the Number Of Pieces
    LNumberOfPieces = UBOUND(UMarkerPlacement)
    '   Define the Consumption of the Marker
    SConsumption = 1E10
    '   For bump purpose
    PlotLineRestore = 1 : TOpenPiece = ""
    '   Get the Size of the Marker Window
    LScreenLength = SModule(%SINGLE_MODULE_SCREEN_XTERM)
    '   Convert this Pixels to Units
    DIALOG PIXELS HDialog, LScreenLength, LYPos TO UNITS LScreenLength, LYPos
    '   Show the time for placement
    CONTROL ADD LABEL, HDialog, %CONTROL_ESTTIME, "Estimated Time :", LScreenLength - 470, 5, 55, 10
    CONTROL ADD LABEL, HDialog, %CONTROL_TIMECAL, "Calculating..", LScreenLength - 410, 5, 60, 10
    CONTROL SET COLOR HDialog, %CONTROL_TIMECAL, %RGB_RED, -1
    CONTROL REDRAW HDialog, %CONTROL_ESTTIME
    CONTROL REDRAW HDialog, %CONTROL_TIMECAL
    '   Starting Combination
    LStartCombi = 1
    '   Ending Combination based on user selection
    LEndCombi = IIF(LMarkerEnviron(%LONG_MARKER_MARKERCOMBINATION), 9, 43)
    LEndCombi = IIF(LMarkerEnviron(%LONG_MARKER_MARKERCOMBINATION), 1, 1)     '''''''''''''''''''''''''''
    '   Get comination start and end(only for developers, need to command this when bundled the module)
    'LStartCombi = VAL( INPUTBOX$("Enter starting of combination", "ESSDi AutoMarker", STR$(LStartCombi))  )
    'LEndCombi = VAL( INPUTBOX$("Enter ending of combination", "ESSDi AutoMarker", STR$(LEndCombi))  )
    '   Loop for combination
    FOR SCombiLoop = LStartCombi TO LEndCombi
'        LIterate = 0
'        select case SCombiLoop
'            case 6,8,10,12,14,16,18,20,27,29,31,33,35,37,39,41 : LIterate = 1
'        end select
'        IF LIterate THEN ITERATE
        '   Set the progress in the screen
        PROGRESSBAR STEP HDialog, %CONTROL_PROGRESSBAR
        '   Parse thru all the Pieces
        FOR LMarkerIndex = %Vert_First TO UBOUND(UMarkerPlacement)
            '   Check if piece is hidden or not
            IF UMarkerOthers(LMarkerIndex).SHorizontal = %MARKER_PIECE_HIDDEN THEN ITERATE
            '   Set the State of this Piece to be Not Placed
            UMarkerOthers(LMarkerIndex).SHorizontal = %MARKER_PIECE_NOTPLACED
        NEXT
        '   Stoarge to store the bump distance
        REDIM UMarkerTop(0) AS DataBaseDataEnvironmentType
        REDIM UMarkerTopIndex(UBOUND(UMarkerPlacement)) AS DataBaseDataEnvironmentType

        REDIM UMarkerBtm(0) AS DataBaseDataEnvironmentType
        REDIM UMarkerBtmIndex(UBOUND(UMarkerPlacement)) AS DataBaseDataEnvironmentType

        REDIM UMarkerLeft(0) AS DataBaseDataEnvironmentType
        REDIM UMarkerLeftIndex(UBOUND(UMarkerPlacement)) AS DataBaseDataEnvironmentType

        REDIM UMarkerRight(0) AS DataBaseDataEnvironmentType
        REDIM UMarkerRightIndex(UBOUND(UMarkerPlacement)) AS DataBaseDataEnvironmentType
        '   Parse thru each warps
        FOR LWarpIndex = 1 TO PARSECOUNT(TMarkerEnviron(%TEXT_MARKER_WARPWAY), $CRLF)
            '   Split the text for corresponding warp
            TText = PARSE$(TMarkerEnviron(%TEXT_MARKER_WARPWAY), $CRLF, LWarpIndex)
            '   Get the secondary warp width from storage
            SWarpWidth = SMarkerEnviron(%SINGLE_MARKER_SECONDARYWARP)
            '   Check if warp presents
            IF SWarpWidth = 0 OR TText = "" THEN SWarpWidth = SFabricWidth - SRightAllowance : TText = ""
            '   Storage allocation for pieces to place
            REDIM UPieceIndex(0) AS DataBaseDataEnvironmentType
            '   Sort pieces and return
            sortByArea(UPieceIndex(), SCombiLoop, LUMarkerTubular(), TText)
            '   Define
            SPatternsArea = 0 : SPlaceWidth = SFabricWidth - SRightAllowance - SLeftAllowance
            '   Parse thru all pieces
            FOR LAreaIndex = 1 TO UBOUND(UPieceIndex)
                '   Add all piece area
                SPatternsArea += UMarkerDimension(UPieceIndex(LAreaIndex).SHorizontal).SExtraData
            NEXT
            '   Calculate the length based on area of patterns and width of fabric
            SPlaceLength = SPatternsArea / SPlaceWidth
            'MSGBOX "Area - " + STR$(SPatternsArea)+"  Width - "+STR$(SPlaceWidth)+"  Length - "+STR$(SPlaceLength)
            '   Storage allocation for small piece to place
            REDIM UPieceSmallIndex(0) AS DataBaseDataEnvironmentType
            '   Sort pieces and return
            sortSmallestPieces(UPieceSmallIndex(), TText)
            '   Check to proceed further
            IF UBOUND(UPieceIndex) = 0 THEN ITERATE
            '*'*'*'*'*'*'*'*'*'*'*'*'*'*'*'*'*'*'*'*'*'*'*'*'*'*'*'*'*'*'*'*'*'*'*'*'*'*'*'*'*'*'*'*'*'*'*'*'*'*'*'*'*'*'*'*'*
            '   Get the frame length from storage
            SPltFrameLength = SMarkerEnviron(%SINGLE_MARKER_PLOTTERFRAME)
            '   Define the frame count & length
            SFrameCount = 0 : SFrameLengthCount = 0
            '   Parse thru the array
            FOR LMarkerIndex = UBOUND(UMarkerDivPltLine) TO 1 STEP -1
                '   Clear the plt Points Marked
                IF UMarkerDivPltLine(LMarkerIndex).LDataType = %DRAWING_MARKER_PLT_LINE THEN DeleteCurveData UMarkerDivPltLine(), LMarkerIndex, LMarkerIndex
                IF UMarkerDivPltLine(LMarkerIndex).LDataType = %DRAWING_MARKER_WRP_LINE THEN DeleteCurveData UMarkerDivPltLine(), LMarkerIndex, LMarkerIndex
            NEXT
            '   Loop if frames are
            WHILE %True
                '   Define The Ending Point Of Our Marker in X
                SxStart = FIX(SWarpWidth * LWarpIndex)
                '   Define The Starting Point Of Our Marker in X
                SxTerm = CEIL(SWarpWidth * (LWarpIndex-1) + IIF(LWarpIndex = 1, SLeftAllowance, 1))
                '   Skip if goes out of border
                IF SxTerm < (SFabricWidth - SRightAllowance) AND _
                    SxStart > SFabricWidth - SRightAllowance THEN

                    SxStart = SFabricWidth - SRightAllowance
                ELSEIF SxStart > SFabricWidth - SRightAllowance THEN
                    EXIT, EXIT
                END IF
                '   Add the warp width of the fabric
                AddCurveData UMarkerDivPltLine(), SxTerm, SxStart, %DRAWING_MARKER_WRP_LINE, 0
                '   Define The Starting Point Of Our Marker in Y
                SyStart = STopAllowance + SFrameLengthCount + IIF(SFrameCount, 1, 0)
                '   Add the frame length
                SFrameLengthCount += SPltFrameLength
                '   Define The Ending Point Of Our Marker in Y
                SyTerm = IIF(SPltFrameLength, SFrameLengthCount, -1)
                '   Place the patterns
                PlaceThePatterns(UPieceIndex(), SCombiLoop, LUMarkerTubular(), LmirrorDirection(), SFabricWidth, DThread, _
                    SxStart, SxTerm, SyStart, SyTerm, TText)
                '   Check if exit is initiated and exit if it is
                IF ThreadVariable = 0 THEN EXIT, EXIT, EXIT
                '   Check if any piece yet to place
                IF CheckIfAnyPieceToPlace(UPieceIndex(), SPltFrameLength, SxStart, SxTerm) = 0 THEN EXIT

                SPlaceLength = -1
                '   If frame length presents
                IF SPltFrameLength THEN
                    '   Add the frame length
                    AddCurveData UMarkerDivPltLine(), SFrameLengthCount, 0, %DRAWING_MARKER_PLT_LINE, 0
                    '   Increase the frame count
                    INCR SFrameCount
                ELSE
                    SFrameLengthCount = FIX(SMarkerEnviron(%SINGLE_MARKER_FABRICLENGTH)+1)
                END IF
            WEND
        NEXT
        '   Get the Length for this Placement
        ComputeMarkerInformation
        '   Get the consumption for this Length
        SFabricLength = SMarkerEnviron(%SINGLE_MARKER_FABRICLENGTH)
        '   Based on this width & Length, get the consumption
        SFabricArea = SFabricWidth * SFabricLength
        '   Get the placed pieces area
        SPlacedArea = SModule(%SINGLE_MODULE_PLACED_AREA)
        '   If the fabric area exists then accept it
        IF SFabricArea > %Vert_Zero AND SPlacedArea > %Vert_Zero THEN
            '   If this area is minimal then accept it
            IF SFabricArea < SConsumption THEN
                '   Set this as the new fabric area
                SConsumption = SFabricArea
                '   Set this as the new Fabric Width
                SNewFabricWidth = SCombiLoop
                '   Stoarge allocation for best combination
                REDIM UMarkerBestCombination(%Vert_Zero) AS DataBaseDataEnvironmentType
                '   Parse thru all the pieces
                FOR LMarkerIndex = %Vert_First TO UBOUND(UMarkerPlacement)
                    '   Get the placement of the piece
                    LoadCurveDataSingle UMarkerPlacement(), LMarkerIndex, SHorizontal, SVertical, 0, 0
                    '   Get the Piece Flags from storage area
                    LoadCurveDataSingle UMarkerStatus(), LMarkerIndex, SPieceFlipped, 0, 0, 0
                    '   Store each placement status
                    AddCurveData UMarkerBestCombination(), SHorizontal, SVertical, UMarkerOthers(LMarkerIndex).SHorizontal, SPieceFlipped
                NEXT
                '   Refresh the Screen
                UpdateTheModule %True
                '   Stoarge to store the bump distance
                REDIM UMarkerTopExt(0) AS DataBaseDataEnvironmentType
                REDIM UMarkerTopIndexExt(0) AS DataBaseDataEnvironmentType

                REDIM UMarkerBtmExt(0) AS DataBaseDataEnvironmentType
                REDIM UMarkerBtmIndexExt(0) AS DataBaseDataEnvironmentType

                REDIM UMarkerLeftExt(0) AS DataBaseDataEnvironmentType
                REDIM UMarkerLeftIndexExt(0) AS DataBaseDataEnvironmentType

                REDIM UMarkerRightExt(0) AS DataBaseDataEnvironmentType
                REDIM UMarkerRightIndexExt(0) AS DataBaseDataEnvironmentType
                '   Transfer the Cut Pattern to the Seam Pattern Storage Area
                AddSourceCurveToDestin UMarkerTop(), UMarkerTopExt()
                AddSourceCurveToDestin UMarkerTopIndex(), UMarkerTopIndexExt()

                AddSourceCurveToDestin UMarkerBtm(), UMarkerBtmExt()
                AddSourceCurveToDestin UMarkerBtmIndex(), UMarkerBtmIndexExt()

                AddSourceCurveToDestin UMarkerLeft(), UMarkerLeftExt()
                AddSourceCurveToDestin UMarkerLeftIndex(), UMarkerLeftIndexExt()

                AddSourceCurveToDestin UMarkerRight(), UMarkerRightExt()
                AddSourceCurveToDestin UMarkerRightIndex(), UMarkerRightIndexExt()
            END IF
        END IF

        IF SCombiLoop = 1 THEN
            '   Store the end time for process
            TCombiTime = FORMAT$(TIMER,"#")
            '   Calculate the time for process
            LTime = (VAL(TCombiTime)-VAL(TLogicTime))
        END IF
        '   Calculate the time
        LCombiTime = LTime * (LEndCombi-SCombiLoop) + IIF(UPieceSmallIndex(%Vert_First).SHorizontal <> 0 , 90, 0)
        '   Show the time
        CONTROL SET TEXT HDialog, %CONTROL_TIMECAL, STR$(FIX(LCombiTime/60))+ " Mins " + STR$(LCombiTime MOD 60) + " Secs"
        'UpdateTheModule %True : MSGBOX "Combi  - "+STR$(SCombiLoop)
    NEXT SCombiLoop
    '   Set best efficiency flags & position for all pieces
    IF UBOUND(UMarkerBestCombination) > 0 THEN
        '   Parse thru all the pieces
        FOR LMarkerIndex = %Vert_First TO UBOUND(UMarkerPlacement)
            '   Get the placement of the piece
            LoadCurveDataSingle UMarkerBestCombination(), LMarkerIndex, SHorizontal, SVertical, LPieceStatus, SPieceFlipped
            '   Get the placement of the piece
            LoadCurveDataSingle UMarkerPlacement(), LMarkerIndex, 0, 0, LPieceOrientation, SRotationAngle
            '   Set the placement of the piece
            SaveCurveDataSingle UMarkerPlacement(), LMarkerIndex, SHorizontal, SVertical, LPieceOrientation, SRotationAngle
            '   Get the Piece Flags from storage area
            LoadCurveDataSingle UMarkerStatus(), LMarkerIndex, 0, SPieceBlocked, LPieceFolded, SPiecePaired
            '   Set the Piece Flags from storage area
            SaveCurveDataSingle UMarkerStatus(), LMarkerIndex, SPieceFlipped, SPieceBlocked, LPieceFolded, SPiecePaired
            '   Get the Piece Index from Storage area
            LoadCurveDataSingle UMarkerIndex(), LMarkerIndex, SIndexOfSize, SIndexOfPiece, 0, SIndexOfGarment
            '   if small pattern then skip
            IF UMarkerSmallPattern(SIndexOfPiece).LDataType = 1 THEN ITERATE
            '   Set piece Status
            UMarkerOthers(LMarkerIndex).SHorizontal = LPieceStatus
            '   Check if piece placed and proceed
            IF UMarkerOthers(LMarkerIndex).SHorizontal = %MARKER_PIECE_PLACED OR UMarkerOthers(LMarkerIndex).SHorizontal = %MARKER_PIECE_HANGING THEN
                IF LMarkerEnviron(%LONG_MARKER_FABRICTYPE) = %FABRICTYPE_TUBULAR THEN
                    '   Updata piece placed count
                    UMarkerPieces(SIndexOfSize, SIndexOfPiece).SVertical += IIF(LPieceFolded = %MARKER_PIECE_FOLDED, 4, 2)
                ELSEIF LMarkerEnviron(%LONG_MARKER_FABRICTYPE) = %FABRICTYPE_SPLITOPEN THEN
                    '   Updata piece placed count
                    UMarkerPieces(SIndexOfSize, SIndexOfPiece).SVertical += IIF&(LPieceFolded = %MARKER_PIECE_FOLDED, 2, 1)
                END IF

            ELSEIF UMarkerOthers(LMarkerIndex).SHorizontal = %MARKER_PIECE_NOTPLACED THEN

                SELECT CASE LMarkerEnviron(%LONG_MARKER_FABRICTYPE)
                    CASE %FABRICTYPE_TUBULAR    : LNumberOfPieceToHide = IIF(LPieceFolded = %MARKER_PIECE_FOLDED, 3, 1)
                    CASE %FABRICTYPE_SPLITOPEN  : LNumberOfPieceToHide = IIF(LPieceFolded = %MARKER_PIECE_FOLDED, 1, 0)
                END SELECT

                UMarkerStatus(LMarkerIndex).LDataType = %MARKER_PIECE_CANBEFOLDED

                IF LNumberOfPieceToHide THEN TOpenPiece = "Yes"

                FOR LHideIndex = %Vert_First TO LNumberOfPieceToHide
                    UnPlaceHiddenPieceExt SIndexOfSize, SIndexOfPiece, UMarkerBestCombination()     '   2021 GD
                NEXT

                LMarkerIndex += LNumberOfPieceToHide
            END IF
        NEXT
        '   Stoarge to store the bump distance
        REDIM UMarkerTop(0) AS DataBaseDataEnvironmentType
        REDIM UMarkerTopIndex(0) AS DataBaseDataEnvironmentType

        REDIM UMarkerBtm(0) AS DataBaseDataEnvironmentType
        REDIM UMarkerBtmIndex(0) AS DataBaseDataEnvironmentType

        REDIM UMarkerLeft(0) AS DataBaseDataEnvironmentType
        REDIM UMarkerLeftIndex(0) AS DataBaseDataEnvironmentType

        REDIM UMarkerRight(0) AS DataBaseDataEnvironmentType
        REDIM UMarkerRightIndex(0) AS DataBaseDataEnvironmentType
        '   Transfer the Cut Pattern to the Seam Pattern Storage Area
        AddSourceCurveToDestin UMarkerTopExt(), UMarkerTop()
        AddSourceCurveToDestin UMarkerTopIndexExt(), UMarkerTopIndex()

        AddSourceCurveToDestin UMarkerBtmExt(), UMarkerBtm()
        AddSourceCurveToDestin UMarkerBtmIndexExt(), UMarkerBtmIndex()

        AddSourceCurveToDestin UMarkerLeftExt(), UMarkerLeft()
        AddSourceCurveToDestin UMarkerLeftIndexExt(), UMarkerLeftIndex()

        AddSourceCurveToDestin UMarkerRightExt(), UMarkerRight()
        AddSourceCurveToDestin UMarkerRightIndexExt(), UMarkerRightIndex()
        '   Refresh the Screen
        'UpdateTheModule %True
        '   Parse thru each warps
        FOR LWarpIndex = 1 TO PARSECOUNT(TMarkerEnviron(%TEXT_MARKER_WARPWAY), $CRLF)
            '   Split the text for corresponding warp
            TText = PARSE$(TMarkerEnviron(%TEXT_MARKER_WARPWAY), $CRLF, LWarpIndex)
            '   Get the secondary warp width from storage
            SWarpWidth = SMarkerEnviron(%SINGLE_MARKER_SECONDARYWARP)
            '   Check if warp presents
            IF SWarpWidth = 0 OR TText = "" THEN SWarpWidth = SFabricWidth - SRightAllowance : TText = ""
            '   Storage allocation for small piece to place
            REDIM UPieceSmallIndex(0) AS DataBaseDataEnvironmentType
            '   Sort pieces and return
            sortSmallestPieces(UPieceSmallIndex(), TText)
            '   Check if piece presents
            IF UPieceSmallIndex(%Vert_First).SHorizontal <> 0 THEN
                '   Define The Starting Point Of Our Marker in X
                SxStart = FIX(SWarpWidth * (LWarpIndex-1) + IIF(LWarpIndex = 1, SLeftAllowance, 1))
                '   Define The Ending Point Of Our Marker in X
                SxTerm = CEIL(SWarpWidth * LWarpIndex)
                '   Skip if goes out of border
                IF SxStart < (SFabricWidth - SRightAllowance) AND _
                    SxTerm > SFabricWidth - SRightAllowance THEN

                    SxTerm = SFabricWidth - SRightAllowance
                ELSEIF SxTerm > SFabricWidth - SRightAllowance THEN
                    EXIT
                END IF
                '   Define The Starting Point Of Our Marker in Y
                SyStart = FIX(STopAllowance)
                '   Define The Ending Point Of Our Marker in Y
                SyTerm = CEIL(SMarkerEnviron(%SINGLE_MARKER_FABRICLENGTH))
                '   Storage for previous polygons dimensions
                REDIM UPrePolyDimen(%Vert_Zero) AS DataBaseDataEnvironmentType
                '   Define the count values
                LSmallCount = 0
                '   Do until max pieces fills
                WHILE %True
                    '   Check if exit is initiated and exit if it is
                    IF ThreadVariable = 0 THEN EXIT LOOP
                    '   Place the smallest pieces in gaps
                    PlaceTheSmallestPattern SxStart, SxTerm, SyStart, SyTerm, UPieceSmallIndex(), UPrePolyDimen(), DThread
                    '   Define the count
                    LCount = 0
                    '   Parse thru all pieces
                    FOR LLineIndex = %Vert_First TO UBOUND(UPieceSmallIndex)
                        '   Get the piece index
                        LMarkerIndex = UPieceSmallIndex(LLineIndex).SHorizontal
                        '   Check the piece state
                        IF UMarkerOthers(LMarkerIndex).SHorizontal = %MARKER_PIECE_PLACED THEN INCR LCount
                    NEXT
                    IF LSmallCount = LCount THEN EXIT LOOP
                    LSmallCount = LCount
                WEND
                '   Get the Length for this Placement
                ComputeMarkerInformation
                '   Define The Ending Point Of Our Marker in X
                SxStart = FIX(SWarpWidth * LWarpIndex)
                '   Define The Starting Point Of Our Marker in X
                SxTerm = CEIL(SWarpWidth * (LWarpIndex-1) + IIF(LWarpIndex = 1, SLeftAllowance, 1))
                '   Skip if goes out of border
                IF SxTerm < (SFabricWidth - SRightAllowance) AND _
                    SxStart > SFabricWidth - SRightAllowance THEN

                    SxStart = SFabricWidth - SRightAllowance
                ELSEIF SxStart > SFabricWidth - SRightAllowance THEN
                    EXIT
                END IF
                '   Define The Starting Point Of Our Marker in Y
                SyStart = SMarkerEnviron(%SINGLE_MARKER_FABRICLENGTH) + 1
                '   Define The Ending Point Of Our Marker in Y
                SyTerm = IIF(SPltFrameLength, SFrameLengthCount, -1)
                '   Place the patterns
                WHILE %True
                    '   Check if exit is initiated and exit if it is
                    IF ThreadVariable = 0 THEN EXIT LOOP
                    '   Place the remaining patterns
                    PlaceThePatterns(UPieceSmallIndex(), SCombiLoop, LUMarkerTubular(), LmirrorDirection(), SFabricWidth, DThread, _
                        SxStart, SxTerm, SyStart, SyTerm, TText)
                    '   Check if exit is initiated and exit if it is
                    IF ThreadVariable = 0 THEN EXIT LOOP
                    '   Check if any piece yet to place
                    IF CheckIfAnyPieceToPlace(UPieceSmallIndex(), SPltFrameLength, SxStart, SxTerm) = 0 THEN EXIT LOOP
                    '   If frame length presents
                    IF SPltFrameLength THEN
                        '   Add the frame length
                        AddCurveData UMarkerDivPltLine(), SFrameLengthCount, 0, %DRAWING_MARKER_PLT_LINE, 0
                        '   Define The Starting Point Of Our Marker in Y
                        SyStart = STopAllowance + SFrameLengthCount + IIF(SFrameCount, 1, 0)
                        '   Add the frame length
                        SFrameLengthCount += SPltFrameLength
                        '   Define The Ending Point Of Our Marker in Y
                        SyTerm = IIF(SPltFrameLength, SFrameLengthCount, -1)
                        '   Increase the frame count
                        INCR SFrameCount
                    END IF
                WEND
                '   Parse thru all the pieces to count placed/unplaced
                FOR LIndex = %Vert_First TO UBOUND(UPieceSmallIndex)
                    '   Get the index from storage
                    LMarkerIndex = UPieceSmallIndex(LIndex).SHorizontal
                    '   Get the Piece Flags from storage area
                    LoadCurveDataSingle UMarkerStatus(), LMarkerIndex, SPieceFlipped, SPieceBlocked, LPieceFolded, SPiecePaired
                    '   Get the Piece Index from Storage area
                    LoadCurveDataSingle UMarkerIndex(), LMarkerIndex, SIndexOfSize, SIndexOfPiece, 0, SIndexOfGarment
                    '   Check if piece placed and proceed
                    IF UMarkerOthers(LMarkerIndex).SHorizontal = %MARKER_PIECE_PLACED OR UMarkerOthers(LMarkerIndex).SHorizontal = %MARKER_PIECE_HANGING THEN
                        IF LMarkerEnviron(%LONG_MARKER_FABRICTYPE) = %FABRICTYPE_TUBULAR THEN
                            '   Updata piece placed count
                            UMarkerPieces(SIndexOfSize, SIndexOfPiece).SVertical += IIF(LPieceFolded = %MARKER_PIECE_FOLDED, 4, 2)
                        ELSEIF LMarkerEnviron(%LONG_MARKER_FABRICTYPE) = %FABRICTYPE_SPLITOPEN THEN
                            '   Updata piece placed count
                            UMarkerPieces(SIndexOfSize, SIndexOfPiece).SVertical += IIF&(LPieceFolded = %MARKER_PIECE_FOLDED, 2, 1)
                        END IF
                    ELSEIF UMarkerOthers(LMarkerIndex).SHorizontal = %MARKER_PIECE_NOTPLACED THEN

                        SELECT CASE LMarkerEnviron(%LONG_MARKER_FABRICTYPE)
                            CASE %FABRICTYPE_TUBULAR    : LNumberOfPieceToHide = IIF(LPieceFolded = %MARKER_PIECE_FOLDED, 3, 1)
                            CASE %FABRICTYPE_SPLITOPEN  : LNumberOfPieceToHide = IIF(LPieceFolded = %MARKER_PIECE_FOLDED, 1, 0)
                        END SELECT

                        UMarkerStatus(LMarkerIndex).LDataType = %MARKER_PIECE_CANBEFOLDED

                        IF LNumberOfPieceToHide THEN TOpenPiece = "Yes"

                        FOR LHideIndex = %Vert_First TO LNumberOfPieceToHide
                            UnPlaceHiddenPieceExt1 SIndexOfSize, SIndexOfPiece
                        NEXT

                        LMarkerIndex += LNumberOfPieceToHide
                    END IF
                NEXT
            END IF
        NEXT
    END IF

    IF TOpenPiece = "Yes" THEN MSGBOX "Some Pattern are not placed because of Fabric Width.", %MB_TASKMODAL OR %MB_ICONWARNING, "Mirror Patterns"
    '   Reset the progress
    PROGRESSBAR SET POS HDialog, %CONTROL_PROGRESSBAR, 0
    '   Kill the time
    CONTROL KILL HDialog, %CONTROL_ESTTIME
    CONTROL KILL HDialog, %CONTROL_TIMECAL
    '   Delete storage from memory
    ERASE LUMarkerTubular(), LmirrorDirection(), pieceArea(), pieceIndex(), UMarkerBestCombination()
    '   Store the end time for process
    TEndTime = FORMAT$(TIMER,"#")
    '   Calculate the time for process
    LMarkerIndex = VAL(TEndTime)-VAL(TLogicTime)
    '   ReSet Thread variable
    ThreadVariable = 0
    '   Close the thread
    THREAD CLOSE DThread TO Lthread
    '*'*'*'*'*'*'*'*'*'*'*'*'*'*'*'*'*'*'*'*'*'*'*'*'*'*'*'*'*'*'*'*'*'*'*'*'*'*'*'*'*'*'*'*'*'*'*'*'*'*'*'*'*'*'*'*'*
    '   Get the Handle of the Tool Bar
    HToolBar = LModule(%LONG_MODULE_TOOL_HANDLE)
    '   Check if any tool active
    IF LModule(%LONG_MODULE_CHILD_ACTIVE) THEN
        '   Transfer the Tool Window Details to Environment
        TerminateControlsForChild HDialog
        '   Clear all the Controls
        KillControlsForDialog HDialog, UChild()
        '   Get the Previous Tool
        LPreviousTool = LModule(%LONG_MODULE_TOOL_ACTIVE)
        '   UnHighlight the Previously Selected Tool
        PushOutTheTool HToolBar, LPreviousTool
    END IF
    '   Refresh the Screen
    'UpdateTheModule %True
    '   Reset the cursor to Arrow
    SetClassLong HDialog, %GCL_HCURSOR, HPrevious
    '   Autosave the marker as given name
    IF SMarkerEnviron(%SINGLE_MARKER_AUTOSAVE)OR LToolEnvironment(%CONTROL_TOOL_OPTIMIZEFABRIC_AUTOSAVE) THEN
        '   Define the name & path of the marker to save
        TMarkerName = CURDIR$+"\"+TMarkerEnviron(%TEXT_MARKER_AUTOSAVE)+$FILE_MARKER_EXTENSION
        '   Clear the modified Flag
        LModule(%LONG_MODULE_MARKER_CHANGE) = %False
        '   Set the Name of the MArker
        TModule(%TEXT_MODULE_MARKERNAME) = TMarkerName
        '   Reset Autosave falg
        SMarkerEnviron(%SINGLE_MARKER_AUTOSAVE) = %False
        '   Reset default Autosave name
        TMarkerEnviron(%TEXT_MARKER_AUTOSAVE) = "ESSDi-Marker"
        '   Save the marker setup to this file
        SaveMarkerToFile TMarkerName
    END IF
    '   For bump purpose
    PlotLineRestore = 0
    '   Re-Select the Pieces
    ProcessForEdit_SelectAll
    '   Place or hang the Selected Pieces
    PlaceOrHangTheSelectedPiece
    '   Store the time of process
    processTime = MAKLNG(LMarkerIndex MOD 60,FIX(LMarkerIndex/60))
    '   Move the scroll bar to starting pos
    'MoveTheScrollBarHorizontally HDialog,MAKLNG(%SB_PAGEUP,0), 0
    '   Show status
    'MSGBOX "Combination no : " + STR$(SNewFabricWidth)
END SUB

SUB PlaceThePatterns (BYREF UPieceIndex() AS DataBaseDataEnvironmentType, BYVAL SCombiLoop, BYREF LUMarkerTubular(), BYREF LmirrorDirection(), BYVAL SFabricWidth, BYVAL DThread, _
    BYVAL SxFirst, BYVAL SxEnd, BYREF SyFirst, BYREF SyEnd, BYVAL TWarpText)

    '   Get the consumption for this Length
    SFabricLength = SMarkerEnviron(%SINGLE_MARKER_FABRICLENGTH)
    '   Storage for placed patterns & Layer
    REDIM UPrePlacedPatterns(UBOUND(UMarkerIndex)) AS DataBaseDataEnvironmentType
    REDIM UPlacedPatterns(UBOUND(UMarkerIndex)) AS DataBaseDataEnvironmentType
    '   Storage for flip of all pieces
    REDIM UFlip(UBOUND(UMarkerIndex)) AS DataBaseDataEnvironmentType
    '   Parse thru all the pieces
    FOR StempSort = %Vert_First TO UBOUND(UPieceIndex)
        '   Get the index from storage
        LIndex = UPieceIndex(StempSort).SHorizontal
        '   Get the Piece Flags from storage area
        LoadCurveDataSingle UMarkerStatus(), LIndex, SPieceFlipped, SPieceBlocked, LPieceFolded, SPiecePaired
        '   Get the placement of the piece
        LoadCurveDataSingle UMarkerPlacement(), LIndex, SHorizontal, SVertical, LPieceOrientation, SRotationAngle
        '   Set the flip based on pairing
        SFlip = IIF(SMarkerEnviron(%SINGLE_MARKER_ONEGARMENTONEWAY) = 0 AND SPiecePaired = 0 AND SPieceFlipped > 0, 4, 2)
        '   Add flip for the piece in storage
        SIndex = LIndex : LPieceFlipped = SPieceFlipped
        SaveCurveData UFlip(), LIndex, SIndex, SFlip, 1, LPieceFlipped
    NEXT
    '   Define the count value
    LFirst = 0 : LTemperature = 200
    '   Loop until best comes
    WHILE LTemperature <> 0

        INCR LFirst

        '   Stoarge to store the bump distance
        REDIM UMarkerTop(0) AS DataBaseDataEnvironmentType
        REDIM UMarkerTopIndex(UBOUND(UMarkerPlacement)) AS DataBaseDataEnvironmentType

        REDIM UMarkerBtm(0) AS DataBaseDataEnvironmentType
        REDIM UMarkerBtmIndex(UBOUND(UMarkerPlacement)) AS DataBaseDataEnvironmentType

        REDIM UMarkerLeft(0) AS DataBaseDataEnvironmentType
        REDIM UMarkerLeftIndex(UBOUND(UMarkerPlacement)) AS DataBaseDataEnvironmentType

        REDIM UMarkerRight(0) AS DataBaseDataEnvironmentType
        REDIM UMarkerRightIndex(UBOUND(UMarkerPlacement)) AS DataBaseDataEnvironmentType

        '   Define The Starting & Ending Point Of Our Marker in X
        SxStart = SxFirst : SxTerm = SxEnd
        '   Define The Starting & Ending Point Of Our Marker in Y
        SyStart = SyFirst : SyTerm = SyFirst
        '   Swap the ending and starting side
        SWAP SxStart, SxTerm
        '   Get the bump tolerance
        SBumpTolerance = SModule(%SINGLE_MODULE_BUMP_TOLERANCE)
        '   Reset flags to use below
        SlastIndexI = 0 : SlastIndexJ = 0 : Lflip = 0 : SLayer = 0 : SMaxLayerWdt = 0
        '   Check if not first time
        IF LFirst > 1 THEN
            '   Storage allocation for pieces to place
            REDIM UPieceIndex(0) AS DataBaseDataEnvironmentType
            '   Sort pieces and return
            sortByArea(UPieceIndex(), SCombiLoop, LUMarkerTubular(), TWarpText)
'            '   Get the randomly one piece for sort replacement
'            LRandom = RND(1,UBOUND(UPieceIndex))
'            LoadCurveDatasingle UPieceIndex(), LRandom, SHorizontal, SVertical, LPieceOrientation, SRotationAngle
'            DeleteCurveData UPieceIndex(), LRandom, LRandom
'            AddCurveData UPieceIndex(), SHorizontal, SVertical, LPieceOrientation, SRotationAngle
            '   Get the randomly one piece
            LRandom = RND(1,UBOUND(UPieceIndex))
            '   Get the index of random piece
            LIndex = UPieceIndex(LRandom).SHorizontal
            '   Get the flip type from storage
            LFlipType = UFlip(LIndex).SVertical

            'if LFlipType = UFlip(LIndex).LDataType then iterate loop
            '   Get the flip of the piece from storage
            SPieceFlipped = UMarkerStatus(LIndex).SHorizontal
            '   Condition for flip type
            IF LFlipType = 2 THEN
                '   Set the Flip Flags based on Lflip
                SELECT CASE SPieceFlipped
                    CASE %MARKER_PIECE_CANBEFLIPPED
                        UMarkerStatus(LIndex).SHorizontal = %MARKER_PIECE_FLIPFLAPPED  : UFlip(LIndex).SExtraData = %MARKER_PIECE_FLIPFLAPPED  : UFlip(LIndex).LDataType += 1
                    CASE %MARKER_PIECE_FLIPFLAPPED
                        UMarkerStatus(LIndex).SHorizontal = %MARKER_PIECE_CANBEFLIPPED : UFlip(LIndex).SExtraData = %MARKER_PIECE_CANBEFLIPPED : UFlip(LIndex).LDataType += 1
                    CASE %MARKER_PIECE_FLIPPED
                        UMarkerStatus(LIndex).SHorizontal = %MARKER_PIECE_FLAPPED      : UFlip(LIndex).SExtraData = %MARKER_PIECE_FLAPPED      : UFlip(LIndex).LDataType += 1
                    CASE %MARKER_PIECE_FLAPPED
                        UMarkerStatus(LIndex).SHorizontal = %MARKER_PIECE_FLIPPED      : UFlip(LIndex).SExtraData = %MARKER_PIECE_FLIPPED      : UFlip(LIndex).LDataType += 1
                END SELECT
            ELSE
                '   Set the Flip Flags based on Lflip
                SELECT CASE RND(0,3)
                    CASE 0
                        UMarkerStatus(LIndex).SHorizontal = %MARKER_PIECE_CANBEFLIPPED : UFlip(LIndex).SExtraData = %MARKER_PIECE_CANBEFLIPPED : UFlip(LIndex).LDataType += 1
                    CASE 1
                        UMarkerStatus(LIndex).SHorizontal = %MARKER_PIECE_FLIPFLAPPED  : UFlip(LIndex).SExtraData = %MARKER_PIECE_FLIPFLAPPED  : UFlip(LIndex).LDataType += 1
                    CASE 2
                        UMarkerStatus(LIndex).SHorizontal = %MARKER_PIECE_FLIPPED      : UFlip(LIndex).SExtraData = %MARKER_PIECE_FLIPPED      : UFlip(LIndex).LDataType += 1
                    CASE 3
                        UMarkerStatus(LIndex).SHorizontal = %MARKER_PIECE_FLAPPED      : UFlip(LIndex).SExtraData = %MARKER_PIECE_FLAPPED      : UFlip(LIndex).LDataType += 1
                END SELECT
            END IF


            'MSGBOX STR$(LIndex)+","+STR$(LFirst)+","+STR$(UMarkerStatus(LIndex).SHorizontal)

        END IF

        '   Parse thru all the pieces
        FOR StempSort = %Vert_First TO UBOUND(UPieceIndex)                   '   Logic begins
            '   Assign the pattern index based on sorting
            LMarkerIndex = UPieceIndex(StempSort).SHorizontal
            '   Check the State of this Piece
            IF UMarkerOthers(LMarkerIndex).SHorizontal = %MARKER_PIECE_PLACED OR UMarkerOthers(LMarkerIndex).SHorizontal = %MARKER_PIECE_HIDDEN THEN ITERATE FOR
            '   Redefine the Dimensions & Area of the Piece
            SaveTheDimensionOfThePiece LMarkerIndex
            '   Get the Width & Length of the Pattern
            SSeamXCen = UMarkerDimension(LMarkerIndex).SHorizontal : SSeamYCen = UMarkerDimension(LMarkerIndex).SVertical
            '   Check if piece width is bigger than fabric width
            IF SSeamXCen > ABS(SxEnd - SxFirst) THEN ITERATE

            IF SyTerm <> SyFirst THEN SyTerm = SMaxLayerWdt

            IF SyEnd <> -1 AND SyTerm + SSeamYCen > SyEnd THEN ITERATE

            SyStart = SyTerm : SyTerm += SSeamYCen

            SyTerm += IIF(SyStart = SyFirst, 0, SBumpTolerance)     '1
            SyStart += IIF(SyStart = SyFirst, 0, SBumpTolerance)    '1

            INCR SLayer

            ' Step variable for Looping
            LStepForLoop = IIF(SxStart < SxTerm, 1, -1)

            SlastIndexJ = IIF(LStepForLoop = 1, SxEnd, SxFirst)

            SLastflip = -1 : SlastIndexI = 0 : SPreviousIndexJ = SlastIndexJ

            FOR LSwapIndex = 1 TO UBOUND(UPieceIndex) : UPieceIndex(LSwapIndex).LDataType = %False : NEXT

            '   Parse thru all the pieces
            FOR StempSort1 = StempSort TO UBOUND(UPieceIndex)
                '   Get the index from storage
                LIndex = UPieceIndex(StempSort1).SHorizontal
                '   Check the State of this Piece
                IF UMarkerOthers(LIndex).SHorizontal = %MARKER_PIECE_PLACED OR UMarkerOthers(LIndex).SHorizontal = %MARKER_PIECE_HIDDEN THEN ITERATE FOR
                '   Get the Piece Index from Storage area
                LoadCurveDataSingle UMarkerIndex(), LIndex, SIndexOfSize, SIndexOfPiece, 0, SIndexOfGarment
                '   Get the Piece Flags from storage area
                LoadCurveDataSingle UMarkerStatus(), LIndex, SPieceFlipped, SPieceBlocked, LPieceFolded, SPiecePaired
                '   Get the placement of the piece
                LoadCurveDataSingle UMarkerPlacement(), LIndex, 0, 0, LPieceOrientation, SRotationAngle
                '   Get the Tubular Flags from Storage area
                LoadCurveDataSingle UMarkerExtras(), SIndexOfPiece, STubularTop, STubularBottom, LLayPercentage, SSymmetric
                '   Load this piece from the Data File
                LoadSizeFromStorageAreaWithoutEnviron SIndexOfPiece, SIndexOfSize
                '   Check the combination conditions
                'IF LMarkerEnviron(%LONG_MARKER_MARKERCOMBINATION) THEN
                    '   Set the Current Piece Flip Flags based on Lflip if condition satisfies
                    'IF SMarkerEnviron(%SINGLE_MARKER_ONEGARMENTONEWAY) = 0 AND SPiecePaired = 0 AND SPieceFlipped > 0 THEN SetFlipBasedOnFlag UMarkerStatus(LIndex).SHorizontal, Lflip
                'END IF
                '   Define the Bias of the piece
                SRotationAngle = ABS(SRotationAngle)
                '   Check with angle falg for(+ or -)
                SRotationAngle *= IIF(UMarkerOthers(LIndex).LDataType = 0, 1, UMarkerOthers(LIndex).LDataType)
                '   Check the flip and rotate
                IF SRotationAngle <> 0 AND (UMarkerStatus(LIndex).SHorizontal = %MARKER_PIECE_CANBEFLIPPED OR UMarkerStatus(LIndex).SHorizontal = (%MARKER_PIECE_FLIPPED XOR %MARKER_PIECE_FLAPPED)) THEN
                    UMarkerPlacement(LIndex).SExtraData = -SRotationAngle
                END IF
                '   If angle is negative then store negative flag else positive flag
                UMarkerOthers(LIndex).LDataType = IIF(SRotationAngle < 0, -1, 1)
                '   Redefine the Dimensions & Area of the Piece
                SaveTheDimensionOfThePiece LIndex
                '   Get the Dimension of the Seam Pattern
                GetPatternDimension USeamPattern(), SxMin, SyMin, SxMax, SyMax
                '   Get the Center of the Pattern
                SSeamXCen = (SxMax - SxMin) / 2 : SSeamYCen = (SyMax - SyMin) / 2

                LtubularTop = 0

                THREAD STATUS DThread TO Lthread
                IF Lthread = 0 THEN
                    IF (UBOUND(TMarker) = 0) AND (UBOUND(LMarker) = 0) AND (UBOUND(SMarker) = 0) THEN
                        Lthread = MSGBOX("YES to Stop AutoMarker..."+$CRLF+"NO to Close...", %MB_TASKMODAL OR %MB_YESNO OR %MB_ICONWARNING, "Warning")
                        IF Lthread = %IDYES THEN
                            ThreadVariable = 0
                            THREAD CLOSE DThread TO Lthread
                            EXIT, EXIT, EXIT
                        ELSEIF Lthread = %IDNO THEN
                            ThreadVariable = 0
                            THREAD CLOSE DThread TO Lthread
                            ThreadVariable = 1
                            THREAD CREATE MyThread(0) TO DThread
                        END IF
                    ELSE
                        Lthread = MSGBOX("YES to Stop Multi-AutoMarker..."+$CRLF+"NO to Stop current AutoMarker and continue with next..." _
                            +$CRLF+"CANCEL to close...", %MB_TASKMODAL OR %MB_YESNOCANCEL OR %MB_ICONWARNING, "Warning")
                        IF Lthread = %IDYES THEN
                            ThreadVariable = 0
                            THREAD CLOSE DThread TO Lthread
                            REDIM TMarker(0), LMarker(0), SMarker(0)
                            EXIT, EXIT, EXIT
                        ELSEIF Lthread = %IDNO THEN
                            ThreadVariable = 0
                            THREAD CLOSE DThread TO Lthread
                            EXIT, EXIT, EXIT
                        ELSEIF Lthread = %IDCANCEL THEN
                            ThreadVariable = 0
                            THREAD CLOSE DThread TO Lthread
                            ThreadVariable = 1
                            THREAD CREATE MyThread(0) TO DThread
                        END IF
                    END IF
                END IF

                FOR SILoop = SyStart TO SyTerm
                    StempI = SILoop

                    SILoop += SSeamYCen + SlastIndexI

                    IF SyEnd <> -1 AND (SILoop > SyEnd OR SILoop + SSeamYCen > SyEnd) THEN SlastIndexI = 0 : EXIT, ITERATE     '   layer if plotter frame 2021 GD August

                    FOR SJLoop = SxStart TO SxTerm STEP LStepForLoop

                        IF LStepForLoop = 1 THEN
                            IF SJLoop <= SlastIndexJ THEN SJLoop = SlastIndexJ + SSeamXCen   '  jump to x term - center point of the pattern
                            IF SJLoop > SxTerm THEN EXIT, EXIT, ITERATE
                            IF SJLoop + SSeamXCen > SxTerm THEN
                                SJLoop -= (SJLoop + SSeamXCen - SxTerm)
                                IF (SJLoop + SSeamXCen) - SxTerm => 0.01 THEN EXIT, EXIT, ITERATE
                            END IF
                        ELSE
                            IF SJLoop => SlastIndexJ THEN SJLoop = SlastIndexJ - SSeamXCen
                            IF SJLoop < SxTerm THEN EXIT, EXIT, ITERATE
                            IF SJLoop - SSeamXCen < SxTerm THEN
                                SJLoop = (SxTerm + SSeamXCen + 1)
                                IF SJLoop - SSeamXCen < SxTerm THEN EXIT, EXIT, ITERATE
                            END IF
                        END IF

                        StempJ = SJLoop
                        '-'-'-'-'-'-'-'-'-'-'-'-'-'-'-'-'-'-'-'-'-'-'-'-'-'-'-'-'-'-'-'-'-'-'-'-'-'-'-'-'-'-'-'-'-'-'-'-'-'-'-'-'-'-'-'-'-
                        IF LUMarkerTubular(LIndex) = 3 THEN
                            '   Set the tubular position for a piece
                            SetPositionForTubular SJLoop, SxStart, SxTerm, SSeamXCen, LtubularTop, LIndex, LmirrorDirection(SIndexOfPiece), LStepForLoop
                            '   If failed then skip this piece
                            IF LtubularTop = 0 THEN EXIT, EXIT, ITERATE
                        END IF
                        '-'-'-'-'-'-'-'-'-'-'-'-'-'-'-'-'-'-'-'-'-'-'-'-'-'-'-'-'-'-'-'-'-'-'-'-'-'-'-'-'-'-'-'-'-'-'-'-'-'-'-'-'-'-'-'-'-
                        '   Get the placement of the piece
                        LoadCurveDataSingle UMarkerPlacement(), LIndex, 0, 0, LPieceOrientation, SRotationAngle
                        '   Set the placement of the piece
                        SaveCurveDataSingle UMarkerPlacement(), LIndex, SJLoop, SILoop, LPieceOrientation, SRotationAngle
                        '   Show status in the status bar
                        Rapper_SetStatusBarText LModule(%LONG_MODULE_STATUS_HANDLE), %MODULE_STATUS_TEXTMESSAGE, "Temp = "+STR$(LTemperature)+" Loop = "+STR$(LFirst)
                        '   Select the current piece
                        UMarkerOthers(LIndex).SHorizontal = %MARKER_PIECE_SELECTED
                        '   Place the current selected piece
                        PlaceOrHangTheSelectedPiece1 LIndex

                        IF UMarkerOthers(LIndex).SHorizontal = %MARKER_PIECE_HANGING THEN
                            '   Set the placement of the piece
                            SaveCurveDataSingle UMarkerPlacement(), LIndex, SJLoop, SyTerm-SSeamYCen, LPieceOrientation, SRotationAngle
                            '   Select the current piece
                            UMarkerOthers(LIndex).SHorizontal = %MARKER_PIECE_SELECTED
                            '   Place the current selected piece
                            PlaceOrHangTheSelectedPiece1 LIndex
                        END IF

                        '   Check if piece placed and proceed
                        IF UMarkerOthers(LIndex).SHorizontal = %MARKER_PIECE_PLACED THEN
                            '-'-'-'-'-'-'-'-'-'-'-'-'-'-'-'-'-'-'-'-'-'-'-'-'-'-'-'-'-'-'-'-'-'-'-'-'-'-'-'-'-'-'-'-'-'-'-'-'-'-'-'-'-'-'-'-'-
                            '   Apply bump for the piece
                            ApplyBumpForThePiece LIndex, LStepForLoop, LUMarkerTubular(LIndex), SCombiLoop
                            '   Get the placement of the piece
                            LoadCurveDataSingle UMarkerPlacement(), LIndex, SJLoop, SILoop, LPieceOrientation, SRotationAngle
                            '   Check for first time
                            IF LFirst = 1 THEN
                                '   Set the placement of the piece in storage
                                SaveCurveDataSingle UPrePlacedPatterns(), LIndex, SJLoop, SILoop, LIndex, UMarkerStatus(LIndex).SHorizontal
                            ELSE
                                '   Set the placement of the piece in storage
                                SaveCurveDataSingle UPlacedPatterns(), LIndex, SJLoop, SILoop, LIndex, UMarkerStatus(LIndex).SHorizontal
                            END IF
                            'updatethemodule 1 : msgbox str$(LIndex)
                            '   Check max length in the layer
                            SMaxLayerWdt = IIF(SMaxLayerWdt < SILoop + SSeamYCen, SILoop + SSeamYCen, SMaxLayerWdt)
                            '-'-'-'-'-'-'-'-'-'-'-'-'-'-'-'-'-'-'-'-'-'-'-'-'-'-'-'-'-'-'-'-'-'-'-'-'-'-'-'-'-'-'-'-'-'-'-'-'-'-'-'-'-'-'-'-'-
                            IF LogicToPlaceRight(LIndex, SlastIndexI, StempSort1, SPreviousIndexJ, SyTerm, SyStart, SILoop, SJLoop, SSeamXCen, SSeamYCen, LStepForLoop, UPieceIndex()) THEN
                                SLastflip = Lflip
                            END  IF
                            '-'-'-'-'-'-'-'-'-'-'-'-'-'-'-'-'-'-'-'-'-'-'-'-'-'-'-'-'-'-'-'-'-'-'-'-'-'-'-'-'-'-'-'-'-'-'-'-'-'-'-'-'-'-'-'-'-
                            IF LtubularTop <> 1 AND SlastIndexI = 0 THEN
                                SlastIndexJ = IIF(LStepForLoop = 1, SJLoop + SSeamXCen + SBumpTolerance, SJLoop - SSeamXCen - SBumpTolerance)   '  1
                            END IF

                            IF LStepForLoop = 1 THEN
                                IF SlastIndexJ < SPreviousIndexJ AND SlastIndexI = 0 THEN
                                    SlastIndexJ = SPreviousIndexJ
                                    IF SLastflip <> -1 THEN Lflip = SLastflip : SLastflip = -1
                                END IF
                            ELSE
                                IF SlastIndexJ > SPreviousIndexJ AND SlastIndexI = 0 THEN
                                    SlastIndexJ = SPreviousIndexJ
                                    IF SLastflip <> -1 THEN Lflip = SLastflip : SLastflip = -1
                                END IF
                            END IF

                            '   Move to next pattern
                            EXIT, EXIT, ITERATE
                        ELSEIF UMarkerOthers(LIndex).SHorizontal = %MARKER_PIECE_HANGING THEN
                            '   Set the State of this Piece to be Not Placed
                            UMarkerOthers(LIndex).SHorizontal = %MARKER_PIECE_NOTPLACED

                            EXIT, EXIT, ITERATE
                        END IF

                    SJLoop = StempJ
                    NEXT SJLoop

                SILoop = StempI
                NEXT SILoop

            NEXT StempSort1
            DIALOG DOEVENTS
        NEXT StempSort
        '   Get the Placed Piece Information
        ComputeMarkerInformation

        IF SFabricLength = 0 THEN
            SFabricLength = SMarkerEnviron(%SINGLE_MARKER_FABRICLENGTH)
            UpdateTheModule %True
            '   Parse thru all the pieces
            FOR StempSort = %Vert_First TO UBOUND(UPieceIndex)
                '   Get the index from storage
                LIndex = UPieceIndex(StempSort).SHorizontal
                '   Set the piece status to not placed
                UMarkerOthers(LIndex).SHorizontal = %MARKER_PIECE_NOTPLACED
            NEXT

        ELSEIF SFabricLength < SMarkerEnviron(%SINGLE_MARKER_FABRICLENGTH) THEN
            SRandom = RND'(0,1)
            SDiffLength = -(SMarkerEnviron(%SINGLE_MARKER_FABRICLENGTH) - SFabricLength)

            IF SRandom < EXP(SDiffLength / (1 * LTemperature)) THEN

            ELSE
                UpdateTheModule %True
                '   Parse thru all the pieces
                FOR StempSort = %Vert_First TO UBOUND(UPieceIndex)
                    '   Get the index from storage
                    LIndex = UPieceIndex(StempSort).SHorizontal
                    '   Set the piece status to not placed
                    UMarkerOthers(LIndex).SHorizontal = %MARKER_PIECE_NOTPLACED
                NEXT
                '   Storage for placed patterns & Layer
                REDIM UPrePlacedPatterns(0) AS DataBaseDataEnvironmentType
                '   Update the fabric length
                SFabricLength = SMarkerEnviron(%SINGLE_MARKER_FABRICLENGTH)
                '   Transfer the actual grade values to our base pattern
                AddSourceCurveToDestin UPlacedPatterns(), UPrePlacedPatterns()

                LTemperature -= 10

            END IF
        ELSE
            UpdateTheModule %True
            '   Parse thru all the pieces
            FOR StempSort = %Vert_First TO UBOUND(UPieceIndex)
                '   Get the index from storage
                LIndex = UPieceIndex(StempSort).SHorizontal
                '   Set the piece status to not placed
                UMarkerOthers(LIndex).SHorizontal = %MARKER_PIECE_NOTPLACED
            NEXT
            '   Storage for placed patterns & Layer
            REDIM UPrePlacedPatterns(0) AS DataBaseDataEnvironmentType
            '   Update the fabric length
            SFabricLength = SMarkerEnviron(%SINGLE_MARKER_FABRICLENGTH)
            '   Transfer the actual grade values to our base pattern
            AddSourceCurveToDestin UPlacedPatterns(), UPrePlacedPatterns()
        END IF
    WEND
    '   Parse thru all the pieces
    FOR LtempSort = %Vert_First TO UBOUND(UPieceIndex)
        '   Get the index from storage
        LIndex = UPieceIndex(LtempSort).SHorizontal

        '   Set the placement of the piece in storage
        LoadCurveDataSingle UPrePlacedPatterns(), LtempSort, SJLoop, SILoop, LIndex, SFlip
        '   Get the placement of the piece
        LoadCurveDataSingle UMarkerPlacement(), LIndex, 0, 0, LPieceOrientation, SRotationAngle
        '   Set the placement of the piece
        SaveCurveDataSingle UMarkerPlacement(), LIndex, SJLoop, SILoop, LPieceOrientation, SRotationAngle

        UMarkerStatus(LIndex).SHorizontal = SFlip

        '   Set the piece status to not placed
        UMarkerOthers(LIndex).SHorizontal = %MARKER_PIECE_PLACED
    NEXT

    UpdateTheModule %True : MSGBOX STR$(LFirst) +"   --- placed"
END SUB

SUB PlaceTheSmallestPattern(BYVAL SxStart, BYVAL SxTerm, BYVAL SyStart, BYVAL SyTerm, BYREF UPieceSmallIndex() AS DataBaseDataEnvironmentType, BYREF UPrePolyDimen() AS DataBaseDataEnvironmentType, _
    BYVAL DThread)
    '   Storage for all polygons
    REDIM UPolygons(%Vert_Zero)
    '   Storage to store each polygon points index start to end in UPolygons() array
    REDIM UPolyDimen(%Vert_Zero) AS DataBaseDataEnvironmentType
    '   Storage for polygons with reduced points
    REDIM UReducePolygons(%Vert_Zero) AS DataBaseDataEnvironmentType
    '   Storage for polygons dimensions
    REDIM UPolyMinMax(%Vert_Zero) AS DataBaseDataEnvironmentType
    '   Storage for all column center points of all polygons
    REDIM UPolyCenter(%Vert_Zero)
    '   Storage for all column end to end distance of all polygons
    REDIM SPolyDist(%Vert_Zero) AS SINGLE
    'TStart = FORMAT$(TIMER,"#")
    '   Get the polygons
    GetThePolygons UPolygons(), UReducePolygons(), UPolyDimen(), UPolyCenter(), UPolyMinMax(), SPolyDist(), UPieceSmallIndex(), SxStart, SyStart, SxTerm, SyTerm, UPrePolyDimen()
    '   Define the defaults
    SameSize = 0 : SamePiece = 0 : LastPolygon = 0 : LastPiece = 0
    '   Parse thru all pieces
    FOR LLineIndex = %Vert_First TO UBOUND(UPieceSmallIndex)
        '   Get the piece index
        LMarkerIndex = UPieceSmallIndex(LLineIndex).SHorizontal
        '   Get the piece state
        SPieceState = UMarkerOthers(LMarkerIndex).SHorizontal
        '   Check the status of piece
        IF SPieceState = %MARKER_PIECE_PLACED OR SPieceState = %MARKER_PIECE_HIDDEN THEN ITERATE FOR
        '   Get the placement of the piece
        LoadCurveDataSingle UMarkerPlacement(), LMarkerIndex, 0, 0, LPieceOrientation, SRotationAngle
        '   Get the Piece Flags from storage area
        LoadCurveDataSingle UMarkerStatus(), LMarkerIndex, SPieceFlipped, SPieceBlocked, LPieceFolded, SPiecePaired
        '   Get the Piece Index from Storage area
        LoadCurveDataSingle UMarkerIndex(), LMarkerIndex, SIndexOfSize, SIndexOfPiece, 0, SIndexOfGarment
        '   Check and skip if same piece exists again
        IF SSimilarSize = SIndexOfSize AND SSimilarPiece = SIndexOfPiece AND LastPiece = %Vert_First THEN ITERATE
        '   Define the Bias of the piece
        SRotationAngle = ABS(UMarkerPlacement(LMarkerIndex).SExtraData)
        '   Check with angle flag for(+ or -)
        SRotationAngle *= IIF(UMarkerOthers(LMarkerIndex).LDataType = 0, 1, UMarkerOthers(LMarkerIndex).LDataType)
        '   Check the flip and rotate
        IF SRotationAngle <> 0 AND (SPieceFlipped = %MARKER_PIECE_CANBEFLIPPED OR SPieceFlipped = %MARKER_PIECE_FLIPFLAPPED) THEN
            UMarkerPlacement(LMarkerIndex).SExtraData = -SRotationAngle
        END IF
        '   If angle is negative then store negative flag else positive flag
        UMarkerOthers(LMarkerIndex).LDataType = IIF(SRotationAngle < 0, -1, 1)
        '   Get the dimension of the pattern
        SPieceWidth = UMarkerDimension(LMarkerIndex).SHorizontal : SPieceLength = UMarkerDimension(LMarkerIndex).SVertical
        '   Check if user wants to stop in between
        THREAD STATUS DThread TO Lthread
        IF Lthread = 0 THEN
            Lthread = MSGBOX("YES to Stop AutoMarker..."+$CRLF+"NO to Close...", %MB_TASKMODAL OR %MB_YESNO OR %MB_ICONWARNING, "Warning")
            IF Lthread = %IDYES THEN
                ThreadVariable = 0
                THREAD CLOSE DThread TO Lthread
                EXIT
            ELSEIF Lthread = %IDNO THEN
                ThreadVariable = 0
                THREAD CLOSE DThread TO Lthread
                ThreadVariable = 1
                THREAD CREATE MyThread(0) TO DThread
            END IF
        END IF
        '   Storage allocation for the flip curve
        REDIM UFlip(%Vert_Zero) AS DataBaseDataEnvironmentType
        '   Set the Current Piece Flip Flags based on Lflip if condition satisfies
        IF SMarkerEnviron(%SINGLE_MARKER_ONEGARMENTONEWAY) = 0 AND SPiecePaired = 0 AND SPieceFlipped > 0 THEN
            '   Storage to store intersection distance
            REDIM SyDist(%Vert_Zero)
            '   Set the flip for the piece
            UMarkerStatus(LMarkerIndex).SHorizontal = %MARKER_PIECE_CANBEFLIPPED
            '   Get the distance of the piece in column
            GetPieceColumnDistance LMarkerIndex, SyDist(), UFlip()
            '   Set the flip for the piece
            UMarkerStatus(LMarkerIndex).SHorizontal = %MARKER_PIECE_FLIPFLAPPED
            '   Get the distance of the piece in column
            GetPieceColumnDistance LMarkerIndex, SyDist(), UFlip()
        ELSE
            '   Storage to store intersection distance
            REDIM SyDist(%Vert_Zero)
            '   Get the distance of the piece in column
            GetPieceColumnDistance LMarkerIndex, SyDist(), UFlip()
        END IF
        '   Store the indexes for skip same
        LastPolygon = LastPolygon'IIF(SameSize = SIndexOfSize AND SamePiece = SIndexOfPiece, LastPolygon, %Vert_First)
        '   Parse thru all polygons
        FOR LPolyIndex = LastPolygon TO UBOUND(UPolyDimen)
            '   Get the dimension of the polygon
            SxMin = UPolyMinMax((LPolyIndex*2)-1).SHorizontal : SxMax = UPolyMinMax((LPolyIndex*2)-1).SVertical
            SyMin = UPolyMinMax(LPolyIndex * 2).SHorizontal : SyMax = UPolyMinMax(LPolyIndex*2).SVertical
            '   Check the dimensions of polygon & piece
            IF SxMax - SxMin < SPieceWidth AND SyMax - SyMin < SPieceLength THEN ITERATE
            '   Get the center points of a polygon
            LoadCurveData UPolyDimen(), LPolyIndex, Sx1, Sy1, LInd, LExt
            '   Storage allocation for our Polygon
            REDIM UMatchSeam(%Vert_Zero) AS DataBaseDataEnvironmentType
            '   Storage allocation for our Polygon
            REDIM UMatchReduceSeam(%Vert_Zero) AS DataBaseDataEnvironmentType
            '   Get the points from storage
            AddSourceCurveToDestinByRange UPolygons(), UMatchSeam(), LInd, LExt
            '   Get the points from storage
            AddSourceCurveToDestinByRange UReducePolygons(), UMatchReduceSeam(), Sx1, Sy1
            '   Get the index of start and end of distance
            LoadCurveData UPolygons(),LExt, 0, 0, LCenStart, LCenTerm
            '   Get the column count of the piece
            LDistCount = UFlip(%Vert_First).SVertical
            '   Parse thru all points in polygon
            FOR LDistIndex = LCenStart TO LCenTerm - LDistCount
                IF SPolyDist(LDistIndex) = 0 THEN LDistIndex = UPolyCenter(LDistIndex).SExtraData : ITERATE
                '   Parse thru all flips
                FOR LFlipIndex = %Vert_Zero TO %Vert_Third
                    IF SMarkerEnviron(%SINGLE_MARKER_ONEGARMENTONEWAY) = 0 AND SPiecePaired = 0 AND SPieceFlipped > 0 THEN
                        '   Set the flip for piece
                        SetFlipBasedOnFlag UMarkerStatus(LMarkerIndex).SHorizontal, LFlipIndex
                    ELSEIF LFlipIndex > %Vert_Zero THEN
                        EXIT FOR
                    END IF
                    '   Check the flip of piece and get the start index from storage
                    LStart = IIF(LFlipIndex MOD 2, UFlip(%Vert_Second).SHorizontal, UFlip(%Vert_First).SHorizontal)
                    '   Check the flip of piece and get the end index from storage
                    LEnd = IIF(LFlipIndex MOD 2, UFlip(%Vert_Second).SVertical, UFlip(%Vert_First).SVertical)
                    '   Define the default values
                    LCount = LStart-1 : LPlace = 1

                    '   Parse thru find if place is available
                    FOR LSearch = LDistIndex TO LDistIndex + LDistCount
                        INCR LCount
                        IF LSearch <> LDistIndex AND (UPolyCenter(LSearch).SVertical - 1 <> UPolyCenter(LSearch-1).SVertical) THEN LPlace = 0 : EXIT FOR
                        IF SPolyDist(LSearch) < SyDist(LCount) THEN LPlace = 0 : EXIT FOR
                    NEXT

                    IF LPlace THEN
                        '   Find the middle index of the balck area
                        LCenterIndex = LDistIndex + (LDistCount /2) - 1
                        '   Calculate the top x position of the piece
                        SxP = UPolyCenter(LCenterIndex).SHorizontal + SPolyDist(LCenterIndex) / 2
                        '   Set the center point for the piece
                        UMarkerPlacement(LMarkerIndex).SHorizontal = SxP - SPieceWidth / 2
                        UMarkerPlacement(LMarkerIndex).SVertical = UPolyCenter(LCenterIndex).SVertical
                        '   Get the Pattern Curve for the Pattern to Bump
                        GetPieceCurveForPiece LMarkerIndex
                        '   move downwards the piece
                        CheckTheCenterPoint UMatchSeam(), USeamPattern(), LMarkerIndex
                        '   Get the Pattern Curve for the Pattern to Bump
                        GetPieceCurveForPiece LMarkerIndex
                        '   Check the current piece is in the polygon
                        LOverLapStatus = CheckForPieceInPieceExt(UMatchReduceSeam(), USeamPattern())
                        '   Check the piece
                        IF LOverLapStatus = 1 THEN
                            '   Set the piece state as selected
                            UMarkerOthers(LMarkerIndex).SHorizontal = %MARKER_PIECE_SELECTED
                            '   Place the current selected piece
                            PlaceOrHangTheSelectedPiece1 LMarkerIndex
                            '   Check if piece state to placed or not
                            IF UMarkerOthers(LMarkerIndex).SHorizontal = %MARKER_PIECE_PLACED THEN
                                '   Add  the placed piece into the polygon
                                'ReduceThePolygon(UPolygons(), UMatchSeam(), UPolyCenter(), SPolyDist(), LMarkerIndex, LInd, LExt, UPolyDimen(), LPolyIndex, _
                                '    UReducePolygons(), UMatchReduceSeam())
                                '   Set flags to reduce unwanted search
                                SameSize = SIndexOfSize : SamePiece = SIndexOfPiece : LastPolygon = LPolyIndex
                                '   Update the polygon is valid
                                UPolyMinMax(LPolyIndex*2).SExtraData = 1
                                '   Set flags to reduce unwanted search
                                SSimilarSize = SIndexOfSize : SSimilarPiece = SIndexOfPiece : LastPiece = %Vert_Zero
                                '   Parse all the distance
                                FOR LSearch = LDistIndex TO LDistIndex + LDistCount
                                    SPolyDist(LSearch) = 0
                                    UPolyCenter(LSearch).SExtraData = LDistIndex + LDistCount
                                NEXT
                                '   Add the piece in bump storage
                                AddThePieceToBump LMarkerIndex
                                'updatethemodule 1 ': MSGBOX STR$(LOverLapStatus)
                                DIALOG DOEVENTS
                                '   Skip the below lines and move to next piece
                                EXIT, EXIT, EXIT, ITERATE
                            ELSE
                                '   Set the piece state as not placed
                                UMarkerOthers(LMarkerIndex).SHorizontal = %MARKER_PIECE_NOTPLACED
                                '   Set flags to reduce unwanted search
                                SSimilarSize = SIndexOfSize : SSimilarPiece = SIndexOfPiece : LastPiece = %Vert_First
                            END IF
                        ELSE
                            '   Set the piece state as not placed
                            UMarkerOthers(LMarkerIndex).SHorizontal = %MARKER_PIECE_NOTPLACED
                            '   Set flags to reduce unwanted search
                            SSimilarSize = SIndexOfSize : SSimilarPiece = SIndexOfPiece : LastPiece = %Vert_First
                        END IF
                    END IF
                NEXT
            NEXT
        NEXT
    NEXT
    '   Parse thru all polygons
    FOR LPolyIndex = LastPolygon TO UBOUND(UPolyDimen)
        '   Check no of piecea placed in each polygon
        IF UPolyMinMax(LPolyIndex*2).SExtraData = 0 THEN
            '   Get the dimension of the polygon
            SxMin = UPolyMinMax((LPolyIndex*2)-1).SHorizontal : SyMin = UPolyMinMax(LPolyIndex * 2).SHorizontal
            '   Add the polygon dimensions
            AddCurveData UPrePolyDimen(), SxMin, SyMin, 0, 0
        END IF
    NEXT
    '   Parse thru all pieces
    FOR LLineIndex = %Vert_First TO UBOUND(UPieceSmallIndex)
        '   Get the piece index
        LMarkerIndex = UPieceSmallIndex(LLineIndex).SHorizontal
        '   Check the piece state
        IF UMarkerOthers(LMarkerIndex).SHorizontal = %MARKER_PIECE_HANGING THEN
            '   Set the State of this Piece to be Not Placed
            UMarkerOthers(LMarkerIndex).SHorizontal = %MARKER_PIECE_NOTPLACED
        END IF
    NEXT
END SUB

SUB ReduceThePolygon(BYREF UPolygons() AS DataBaseDataEnvironmentType, BYREF UMatchPoly() AS DataBaseDataEnvironmentType, _
    BYREF UPolyCenter() AS DataBaseDataEnvironmentType, BYREF SPolyDist(), BYVAL LPieceIndex, BYVAL LStart, BYVAL LEnd, _
    BYREF UPolyDimen() AS DataBaseDataEnvironmentType, BYVAL LPolyIndex, UReducePolygons() AS DataBaseDataEnvironmentType, _
    UMatchReduceSeam() AS DataBaseDataEnvironmentType)

    '   Get the index of start and end of distance
    LoadCurveData UPolygons(),LEnd, 0, 0, LCenStart, LCenTerm
    '   Storage for getting top points of the piece
    REDIM USeamTop(%Vert_Zero) AS DataBaseDataEnvironmentType
    '   Storage for getting bottom points of the piece
    REDIM USeamBottom(%Vert_Zero) AS DataBaseDataEnvironmentType
    '   Get the Pattern Curve for the Pattern to Bump
    GetPieceCurveForPiece LPieceIndex
    '   Get the bottom & top points of the pattern
    FindIntersectionPointsOfPatternTB USeamPattern(), USeamTop(), USeamBottom()
    '   Storage for create new polygon
    REDIM UCurve(%Vert_Zero) AS DataBaseDataEnvironmentType
    '   Load the points from storage
    LoadCurveData USeamBottom(), %Vert_First, Sx3, Sy3, 0, 0
    '   Store the point in storage
    AddCurveData UCurve(), Sx3, Sy3, 7, 7
    '   Parse thru all points in bottom of pattern
    FOR LCurve = %Vert_Second TO UBOUND(USeamBottom)
        '   Load the points from storage
        LoadCurveData USeamBottom(), LCurve, SxP, SyP, 0, 0
        '   Store the point in storage
        AddCurveData UCurve(), SxP, SyP, 5, 5
        AddCurveData UCurve(), SxP, SyP, 7, 7
    NEXT
    '   Parse thru all points in top of pattern
    FOR LCurve = UBOUND(USeamTop) TO %Vert_First STEP -%Vert_First
        '   Load the points from storage
        LoadCurveData USeamTop(), LCurve, SxP, SyP, 0, 0
        '   Store the point in storage
        AddCurveData UCurve(), SxP, SyP, 6, 6
        AddCurveData UCurve(), SxP, SyP, 8, 8
    NEXT
    '   Store the point in storage
    AddCurveData UCurve(), Sx3, Sy3, 5, 5
    '   Storage for reduce form
    REDIM UReduceCurve(%Vert_Zero) AS DataBaseDataEnvironmentType
    AddCurveData UReduceCurve(), 0, 0, 0, 0
    AddCurveData UReduceCurve(), 0, 0, 0, 0
    '   Reduce the points if has same angle
    ReduceTheCurve UCurve(), UReduceCurve(), UBOUND(UReduceCurve)+%Vert_First
    '   Get the point from storage
    LoadCurveData UReduceCurve(), %Vert_Third, SxP, SyP, LTool, LData
    '   Add the point to last of stoarge
    AddCurveData UReduceCurve(), SxP, SyP, LTool, LData
    '   Delete the first point
    DeleteCurveData UReduceCurve(), %Vert_Third, %Vert_Third
    '   Get the point from storage
    LoadCurveData UReduceCurve(), %Vert_Third, SxP, SyP, LTool, LData
    AddCurveData UReduceCurve(), SxP, SyP-1, LTool, LData
    AddCurveData UReduceCurve(), SxP, SyP-1, LTool, LData
    '   Delete the first point
    DeleteCurveData UCurve(), %Vert_First, %Vert_First
    '   Store the point in storage
    AddCurveData UCurve(), Sx3, Sy3, 7, 7
    '   Storage for getting bottom points of the polygon
    REDIM UPolygonBottom(%Vert_Zero) AS DataBaseDataEnvironmentType
    '   Parse thru all points
    FOR LCurveIndex = %Vert_First TO UBOUND(USeamBottom)
        '   Get the point from storage
        LoadCurveData USeamBottom(), LCurveIndex, SxP, SyP, LTool, LData
        '   Assign default values
        SDistance = 1E10 : LIndex = 0
        '   Parse thru the polygon
        FOR LBase = UBOUND(UMatchPoly) - %Vert_First TO %Vert_Second STEP -%Vert_Second
            '   Get the point from storage
            LoadCurveData UMatchPoly(), LBase, Sx3, Sy3, LTool, LData
            '   Check to find nearest point
            IF LTool <> 2 AND LTool <> 4 AND Sy3 = SyP AND Sx3 <= SxP AND SDistance > Maths_LengthOfLine(SxP, SyP, Sx3, Sy3) THEN
                '   Store the index
                SDistance = Maths_LengthOfLine(SxP, SyP, Sx3, Sy3) : LIndex = LBase
            END IF
        NEXT
        '   Store the points in storage
        AddCurveData UPolygonBottom(), 0, 0, LIndex, LIndex-1
    NEXT

    '   Storage for getting bottom points of the polygon
    REDIM UPolygonCenter(%Vert_Zero) AS DataBaseDataEnvironmentType
    '   Parse thru all points
    FOR LCurveIndex = %Vert_First TO UBOUND(USeamBottom)
        '   Get the point from storage
        LoadCurveData USeamBottom(), LCurveIndex, SxP, SyP, LTool, LData
        '   Assign default values
        STopDist = 1E10 : SBtmDist = 1E10 : LTopIndex = 0 : LBtmIndex = 0
        '   Parse thru the polygon
        FOR LBase = LCenStart TO LCenTerm
            '   Get the point from storage
            LoadCurveData UPolyCenter(), LBase, Sx3, Sy3, LTool, LData
            '   Check to find nearest point
            IF Sy3 = SyP AND Sx3 > SxP AND STopDist > Maths_LengthOfLine(SxP, SyP, Sx3, Sy3) THEN
                '   Store the index
                STopDist = Maths_LengthOfLine(SxP, SyP, Sx3, Sy3) : LTopIndex = LBase
            END IF
            '   Check to find nearest point
            IF Sy3 = SyP AND Sx3 < SxP AND SBtmDist > Maths_LengthOfLine(SxP, SyP, Sx3, Sy3) THEN
                '   Store the index
                SBtmDist = Maths_LengthOfLine(SxP, SyP, Sx3, Sy3) : LBtmIndex = LBase
            END IF
        NEXT
        IF LTopIndex <> 0 THEN
            '   Store the points in storage
            AddCurveData UPolygonCenter(), 0, 0, LTopIndex, 2
        ELSE
            '   Store the points in storage
            AddCurveData UPolygonCenter(), 0, 0, LBtmIndex, 1
        END IF
    NEXT
    '   Define a Temporary Storage Area
    REDIM UTemp(%Vert_Zero) AS DataBaseDataEnvironmentType : REDIM STempPolyDist(%Vert_Zero)
    '   Parse thru all points in seam bottom
    FOR LCurve = %Vert_First TO UBOUND(USeamTop)
        '   Load the points
        LoadCurveData USeamTop(), LCurve, SxP, SyP, 0, 0
        '   Load the points
        LoadCurveData USeamBottom(), LCurve, SxPB, SyPB, 0, 0
        '   Get the indexes from storage
        LBase = UPolygonBottom(LCurve).LDataType : LCenCurve = UPolygonCenter(LCurve).LDataType
        '   Load the points from polygon
        LoadCurveData UMatchPoly(), LBase, Sx3, Sy3, LTool, LData
        '   Get the Width Of the Line
        SDistanceBtm = Maths_LengthOfLine(SxPB, SyPB, Sx3, Sy3)
        '   Store the distance
        AddCurvePoints STempPolyDist(), SDistanceBtm
        '   Get the Width Of the Line
        SDistance = Maths_LengthOfLine(SxP, SyP, Sx3, Sy3)
        '   restore the distance
        SPolyDist(LCenCurve) -= SDistance
        '   Update the center point
        UPolyCenter(LCenCurve).SHorizontal = SxP + (SPolyDist(LCenCurve) / 2)
        '   Add the center point in temp storage
        AddCurveData UTemp(), SDistanceBtm / 2 + Sx3 , SyP, 0, 0
    NEXT
    '   Load the points from storage
    LoadCurveData UMatchPoly(), UPolygonBottom(%Vert_Second).LDataType, SxP, SyP, 0, 0
    '   Get the point from storage
    SaveCurveData UReduceCurve(), %Vert_First, SxP, SyP, 0, 0
    SaveCurveData UReduceCurve(), %Vert_Second, SxP, SyP, 0, 0
    '   Get the nearest point index
    LMatchIndex = Maths_GetClosestPointIndex (UMatchReduceSeam(), SxP, SyP)
    '   Load the points from storage
    LoadCurveData UMatchPoly(), UPolygonBottom(%Vert_First).LDataType, SxP, SyP, 0, 0
    AddCurveData UReduceCurve(), SxP, SyP, 0, 0
    AddCurveData UReduceCurve(), SxP, SyP, 0, 0
    '   Get the center points of a polygon
    LoadCurveData UPolyDimen(), LPolyIndex, SxP, SyP, 0, 0

    '   Define a Temporary Storage Area
    REDIM UTemporary(%Vert_Zero) AS DataBaseDataEnvironmentType
    '   Transfer the Curve to this Temporary Storage Area
    AddSourceCurveToDestinByRange UMatchPoly(), UTemporary(), %Vert_First, UPolygonBottom(%Vert_Second).LDataType
    '   Append the curve back to our storage area
    AddSourceCurveToDestin UCurve(), UTemporary()
    '   Transfer the Curve to this Temporary Storage Area
    AddSourceCurveToDestinByRange UMatchPoly(), UTemporary(), UPolygonBottom(%Vert_Second).LDataType+1, UBOUND(UMatchPoly)

    '   Define a Temporary Storage Area
    REDIM UReducedTemp(%Vert_Zero) AS DataBaseDataEnvironmentType
    '   Transfer the Curve to thisz Temporary Storage Area
    AddSourceCurveToDestinByRange UReducePolygons(), UReducedTemp(), %Vert_First, LMatchIndex + Sxp - 1
    '   Append the curve back to our storage area
    AddSourceCurveToDestin UReduceCurve(), UReducedTemp()
    '   Transfer the Curve to thisz Temporary Storage Area
    AddSourceCurveToDestinByRange UReducePolygons(), UReducedTemp(), LMatchIndex + Sxp, UBOUND(UReducePolygons)

    '   Define a Temporary Storage Area
    REDIM UTempDist(%Vert_Zero) AS DataBaseDataEnvironmentType
    '   Transfer the Curve to thisz Temporary Storage Area
    AddSourceCurveToDestinByRange UPolyCenter(), UTempDist(), %Vert_First, LCenTerm
    '   Update the current polygon index
    UTemporary(UBOUND(UTemporary)).SExtraData += UBOUND(UTemp)
    '   Append the curve back to our storage area
    AddSourceCurveToDestin UTemp(), UTempDist()
    '   Transfer the Curve to thisz Temporary Storage Area
    AddSourceCurveToDestinByRange UPolyCenter(), UTempDist(), LCenTerm + %Vert_First, UBOUND(UPolyCenter)

    '   Parse thru all polygons center point
    FOR LIndex = UBOUND(STempPolyDist) TO %Vert_First STEP -%Vert_First
        '   Increase the storage
        REDIM PRESERVE SPolyDist(UBOUND(SPolyDist) + %Vert_First)
        '   Store the distance
        ARRAY INSERT SPolyDist(LCenTerm + %Vert_First), STempPolyDist(LIndex)
    NEXT

    '   Parse thru all polygons
    UPolyDimen(LPolyIndex).SVertical += UBOUND(UReduceCurve) : UPolyDimen(LPolyIndex).SExtraData += UBOUND(UCurve)
    '   Parse thru all polygons
    FOR LIndex = LPolyIndex + %Vert_First TO UBOUND(UPolyDimen)
        '   Get the center points of a polygon
        LoadCurveData UPolyDimen(), LIndex, Sx1, Sy1, LInd , LExt
        '   Update the center points of a polygon
        SaveCurveData UPolyDimen(), LIndex, Sx1 + UBOUND(UReduceCurve), Sy1 + UBOUND(UReduceCurve), LInd + UBOUND(UCurve), LExt + UBOUND(UCurve)
        '   Update the center point indexes
        UPolygons(LExt).LDataType += UBOUND(UTemp) : UPolygons(LExt).SExtraData += UBOUND(UTemp)
    NEXT

    '   Define a Temporary Storage Area
    REDIM UPolygonsBackup(%Vert_Zero) AS DataBaseDataEnvironmentType
    '   Transfer the Curve to thisz Temporary Storage Area
    AddSourceCurveToDestinByRange UPolygons(), UPolygonsBackup(), %Vert_First, LStart-1
    '   Append the curve back to our storage area
    AddSourceCurveToDestin UTemporary(), UPolygonsBackup()
    '   Transfer the Curve to this Temporary Storage Area
    AddSourceCurveToDestinByRange UPolygons(), UPolygonsBackup(), LEnd + 1, UBOUND(UPolygons)
    '   Restore the storage for polygons
    REDIM UPolygons(%Vert_Zero) AS DataBaseDataEnvironmentType
    '   Restore the storage for polygons center points
    REDIM UPolyCenter(%Vert_Zero) AS DataBaseDataEnvironmentType
    '   Restore the storage for polygons center points
    REDIM UReducePolygons(%Vert_Zero) AS DataBaseDataEnvironmentType
    '   Append the curve back to our storage area
    AddSourceCurveToDestin UPolygonsBackup(), UPolygons()
    '   Append the curve back to our storage area
    AddSourceCurveToDestin UTempDist(), UPolyCenter()
    '   Append the curve back to our storage area
    AddSourceCurveToDestin UReducedTemp(), UReducePolygons()
END SUB

SUB GetPieceColumnDistance(BYVAL LPieceIndex, BYREF SyDist(), BYREF UFlip()  AS DataBaseDataEnvironmentType)
    '   Store the starting index
    LStart = UBOUND(SyDist) + 1
    '   Get the Pattern Curve for the Pattern to Bump
    GetPieceCurveForPiece LPieceIndex
    '   Find the each column distance
    FindIntersectionDistOfPattern SyDist(), USeamPattern()
    '   Store the ending index
    LEnd = UBOUND(SyDist)
    '   Store the start to end index in storage
    AddCurveData UFlip(), LStart, LEnd, 0, 0
END SUB

SUB CheckTheCenterPoint(BYREF UMatchPoly() AS DataBaseDataEnvironmentType, BYREF USeamPattern() AS DataBaseDataEnvironmentType, BYVAL LPieceIndex)
    '   Storage for getting top points of the piece
    REDIM USeamTop(%Vert_Zero) AS DataBaseDataEnvironmentType
    '   Storage to get points
    REDIM USeamBottom(%Vert_Zero) AS DataBaseDataEnvironmentType
    '   Get the bottom & top points of the pattern
    FindIntersectionPointsOfPatternTB USeamPattern(), USeamTop(), USeamBottom()
    '   Assign default values
    SDistance = 1E10
    '   Parse thru all points
    FOR LCurveIndex = %Vert_First TO UBOUND(USeamBottom)
        '   Get the point from storage
        LoadCurveData USeamBottom(), LCurveIndex, SxP, SyP, LTool1, LData1
        '   Parse thru the polygon
        FOR LBase = UBOUND(UMatchPoly) - %Vert_First TO %Vert_Second STEP -%Vert_Second
            '   Get the point from storage
            LoadCurveData UMatchPoly(), LBase, Sx3, Sy3, LTool, LData
            '   Check to find nearest point
            IF LTool <> 2 AND LTool <> 4 AND Sy3 = SyP AND Sx3 <= SxP AND SDistance > Maths_LengthOfLine(SxP, SyP, Sx3, Sy3) THEN
                '   Store the index
                SDistance = Maths_LengthOfLine(SxP, SyP, Sx3, Sy3)
            END IF
        NEXT
    NEXT
    '   Get the bump tolerance
    SBumpTolerance = SModule(%SINGLE_MODULE_BUMP_TOLERANCE)

    UMarkerPlacement(LPieceIndex).SHorizontal = UMarkerPlacement(LPieceIndex).SHorizontal - SDistance + SBumpTolerance  '0.5
    UMarkerPlacement(LPieceIndex).SVertical += SBumpTolerance   '0.5

END SUB

SUB FindIntersectionPointsOfPatternTB(BYREF UCurve() AS DataBaseDataEnvironmentType, BYREF UTop() AS DataBaseDataEnvironmentType, BYREF UBtm() AS DataBaseDataEnvironmentType)
    '   Get the dimension of the piece
    GetPatternDimension UCurve(), SxMin, SyMin, SxMax, SyMax
    '   Get the min & max value
    SMin = FIX(SyMin) : SMax = CEIL(SyMax)
    '   Storage to get top & bottom points
    REDIM UTop(SMax-SMin) AS DataBaseDataEnvironmentType, UBtm(SMax-SMin) AS DataBaseDataEnvironmentType
    '   Parse thru all the lines of the Second Curve
    FOR LCurveIndex = %Vert_First TO UBOUND(UCurve) STEP %Vert_Second
        '   Get the coordinates of the curve
        LoadCurveData UCurve(), LCurveIndex, Sx1, Sy1, 0, 0
        LoadCurveData UCurve(), LCurveIndex + 1, Sx2, Sy2, 0, 0
        '   Get the all points in the line
        GetPointsOnCurveYExt2 Sx1, Sy1, Sx2, Sy2, SMin, UTop(), UBtm()
    NEXT
END SUB

SUB FindIntersectionPointsOfPatternLR(BYREF UCurve() AS DataBaseDataEnvironmentType, BYREF ULeft() AS DataBaseDataEnvironmentType, BYREF URight() AS DataBaseDataEnvironmentType)
    '   Get the dimension of the piece
    GetPatternDimension UCurve(), SxMin, SyMin, SxMax, SyMax
    '   Parse thru min to max
    FOR LLineIndex = SxMin TO SxMax
        '   Storage to get intersections
        REDIM STemp(%Vert_Zero)
        '   Parse thru all the lines of the Second Curve
        FOR LCurveIndex = %Vert_First TO UBOUND(UCurve) STEP %Vert_Second
            '   Get the coordinates of the curve
            LoadCurveData UCurve(), LCurveIndex, Sx3, Sy3, 0, 0
            LoadCurveData UCurve(), LCurveIndex + 1, Sx4, Sy4, 0, 0
            '   Check if this two windows overlap
            LPieceOverLap = Maths_CheckIfWindowOverlap(LLineIndex, SyMin, LLineIndex, SyMax, Sx3, Sy3, Sx4, Sy4)
            '   Check If the window really overlaps
            IF LPieceOverLap THEN
                '   Yep the two windows overlap.  Chek if the line overlap
                LIntersect = Maths_GetLineIntersect(LLineIndex, SyMin, LLineIndex, SyMax, Sx3, Sy3, Sx4, Sy4, SxP, SyP)
                '   If the Intersection is possible then check if it lies within the two lines
                IF LIntersect = %True THEN
                    LInLine1 = Maths_IsPointInLine (LLineIndex, SyMin, LLineIndex, SyMax, SxP, SyP)
                    LInLine2 = Maths_IsPointInLine (Sx3, Sy3, Sx4, Sy4, SxP, SyP)
                    '   Check the points
                    IF LInLine1 AND LInLine2 THEN
                        '   Store the intersection point
                        AddCurvePoints STemp(), SyP
                    END IF
                END IF
            END IF
        NEXT
        IF UBOUND(STemp) > 0 THEN
            '   Sort the points
            ARRAY SORT STemp(1)
            '   Add the Distance in storage
            AddCurveData ULeft(), LLineIndex, STemp(1), 0, 0
            '   Add the Distance in storage
            AddCurveData URight(), LLineIndex, STemp(UBOUND(STemp)), 0, 0
        END IF
    NEXT
END SUB

SUB FindIntersectionDistOfPattern(BYREF SyDist(), BYREF UCurve() AS DataBaseDataEnvironmentType)
    '   Get the dimension of the piece
    GetPatternDimension UCurve(), SxMin, SyMin, SxMax, SyMax
    '   Get the min & max value
    SMin = FIX(SyMin-SyMin) : SMax = CEIL(SyMax-SyMin)
    '   Storage to get top & bottom points
    REDIM UTop(SMax-SMin) AS DataBaseDataEnvironmentType, UBtm(SMax-SMin) AS DataBaseDataEnvironmentType
    LLast = UBOUND(SyDist)
    REDIM PRESERVE SyDist(LLast + (SMax-SMin))
    '   Parse thru all the lines of the Curve
    FOR LCurveIndex = %Vert_First TO UBOUND(UCurve) STEP %Vert_Second
        '   Get the coordinates of the curve
        LoadCurveData UCurve(), LCurveIndex, Sx1, Sy1, 0, 0
        LoadCurveData UCurve(), LCurveIndex + 1, Sx2, Sy2, 0, 0
        '   Find the points lies on the line in Y
        GetPointsOnCurveYExt Sx1-SxMin, Sy1-SyMin, Sx2-SxMin, Sy2-SyMin, SMin, SyDist(), UTop(), UBtm(), LLast
    NEXT
END SUB

SUB GetThePolygons(BYREF UPolygons() AS DataBaseDataEnvironmentType, BYREF UReducePolygons() AS DataBaseDataEnvironmentType, _
    BYREF UPolyDimen() AS DataBaseDataEnvironmentType, BYREF UPolyCenter() AS DataBaseDataEnvironmentType, _
    BYREF UPolyMinMax() AS DataBaseDataEnvironmentType, SPolyDist(), BYREF UPieceSmallIndex() AS DataBaseDataEnvironmentType, BYVAL SxStart, BYVAL SyStart, BYVAL SxTerm, BYVAL SyTerm, _
    BYREF UPrePolyDimen() AS DataBaseDataEnvironmentType)

    '   Stoarge to store the
    REDIM TColumn(SyTerm)
    '   Get the intersecting points of line
    GetAllIntersectionPointsOfColumns (TColumn(), SxStart, SyStart, SxTerm, SyTerm)
    '   Storage for getting intersections
    REDIM UIntersectPointsColumns (%Vert_Zero)
    '   Parse thru all columns
    FOR SIndex = 0 TO UBOUND(TColumn)
        '   Get the text from storage
        TText = TColumn(SIndex)
        '   Skip if no points
        IF TText = "" THEN ITERATE
        '   Storage for all points in the column
        REDIM SSingle(0)
        '   Parse thru all points in the column
        FOR LText = %Vert_First TO PARSECOUNT(TText)
            '   Store the distance
            AddCurvePoints SSingle(), VAL(PARSE$(TText, LText))
        NEXT
        '   Sort the points in asc order
        ARRAY SORT SSingle()
        '   Parse thru all points
        FOR LPointIndex = %Vert_First TO UBOUND(SSingle)

            IF LPointIndex = 1 AND Maths_LengthOfLine(SxStart, SIndex, SSingle(LPointIndex), SIndex) > 10 THEN
                '   Store the Points Of the Line
                AddCurveData UIntersectPointsColumns(), SxStart, SIndex, 0, 2
                AddCurveData UIntersectPointsColumns(), SSingle(LPointIndex), SIndex, 0, 1
            ELSEIF LPointIndex = UBOUND(SSingle) AND Maths_LengthOfLine(SSingle(LPointIndex), SIndex, SxTerm, SIndex) > 10 THEN
                '   Store the Points Of the Line
                AddCurveData UIntersectPointsColumns(), SSingle(LPointIndex), SIndex, 0, 2
                AddCurveData UIntersectPointsColumns(), SxTerm, SIndex, 0, 1
            ELSEIF LPointIndex MOD 2 = 0 AND LPointIndex+1 < UBOUND(SSingle) AND Maths_LengthOfLine(SSingle(LPointIndex), SIndex, SSingle(LPointIndex+1), SIndex) > 10 THEN
                '   Store the Points Of the Line
                AddCurveData UIntersectPointsColumns(), SSingle(LPointIndex), SIndex, 0, 2
                AddCurveData UIntersectPointsColumns(), SSingle(LPointIndex+1), SIndex, 0, 1
            END IF
        NEXT
    NEXT
    '   Flag for polygons
    LPolygonCount = 0
    '   Get the Smallest Dimension from the smallest Pieces
    GetSmallestLAW UPieceSmallIndex(), SPieceWidth, SPieceLength, SPieceArea
    '   Parse thru all points
    FOR LIndex = %Vert_First TO UBOUND(UIntersectPointsColumns) STEP 2
        '   Get the points from storage
        LoadCurveData UIntersectPointsColumns(), LIndex, Sx1, Sy1, LInd, LExt
        LoadCurveData UIntersectPointsColumns(), LIndex+1, Sx2, Sy2, LInd2, LExt2
        '   Check the points
        IF LInd <> 0 OR LInd2 <> 0 THEN ITERATE
        '   Storage for parallel lines
        REDIM UCurve(%Vert_Zero) AS DataBaseDataEnvironmentType
        '   Store the Points Of the Line
        AddCurveData UCurve(), Sx1, Sy1, LIndex, LExt
        AddCurveData UCurve(), Sx2, Sy2, LIndex+1, LExt2
        '   Parse thru all points
        FOR LLineIndex = LIndex + 2 TO UBOUND(UIntersectPointsColumns) STEP 2
            '   Get the points from storage
            LoadCurveData UIntersectPointsColumns(), LLineIndex, Sx3, Sy3, LInd3, LExt3
            LoadCurveData UIntersectPointsColumns(), LLineIndex+1, Sx4, Sy4, LInd4, LExt4
            '   Check the points
            IF LInd3 <> 0 OR LInd4 <> 0 THEN ITERATE
            '   Check if this two windows overlap
            LPieceOverLap = Maths_CheckIfWindowOverlap(Sx1, Sy1, Sx2, SyTerm, Sx3, Sy3, Sx4, Sy4)
            '   Check If the window really overlaps
            IF LPieceOverLap THEN
                '   Store the Points Of the Line
                AddCurveData UCurve(), Sx3, Sy3, LLineIndex, LExt3
                AddCurveData UCurve(), Sx4, Sy4, LLineIndex+1, LExt4
            END IF
        NEXT
        '   Flag for 1st line
        SFirst = UCurve(%Vert_First).SVertical - 1
        '   Storage for the nearest points
        REDIM UCorrectPoints(%Vert_Zero) AS DataBaseDataEnvironmentType
        '   Parse thru all parallel lines
        FOR LCurveIndex = %Vert_First TO UBOUND(UCurve) STEP 2
            LoadCurveData UCurve(), LCurveIndex, Sx3, Sy3, LTool3, LExt3
            LoadCurveData UCurve(), LCurveIndex+1, Sx4, Sy4, LTool4, LExt4
            '   Check if point is nearest or not
            IF SFirst + 1 = Sy3 THEN
                '   Check if this two windows overlap
                LPieceOverLap = Maths_CheckIfWindowOverlap(Sx1, Sy1, Sx2, Sy1+1.5, Sx3, Sy3, Sx4, Sy4)
                '   Check If the window really overlaps
                IF LPieceOverLap OR LCurveIndex = %Vert_First THEN
                    '   Set the flag
                    SFirst = Sy3
                    '   Store the Points Of the Line
                    AddCurveData UCorrectPoints(), Sx3, Sy3, LTool3, LExt3
                    AddCurveData UCorrectPoints(), Sx4, Sy4, LTool4, LExt4

                    Sx1 = Sx3 : Sy1 = Sy3 : Sx2 = Sx4 : Sy2 = Sy4
                END IF
            ELSE
                EXIT FOR
            END IF
        NEXT

        FOR LCurveIndex = %Vert_First TO UBOUND(UCorrectPoints) STEP 2
            UIntersectPointsColumns(UCorrectPoints(LCurveIndex).LDataType).LDataType = UCorrectPoints(UBOUND(UCorrectPoints)-1).LDataType
            UIntersectPointsColumns(UCorrectPoints(LCurveIndex+1).LDataType).LDataType = UCorrectPoints(UBOUND(UCorrectPoints)).LDataType
        NEXT

        IF UBOUND(UCorrectPoints) > 0 THEN
            '   Increase the polygon count
            REDIM UPoly(%Vert_Zero) AS DataBaseDataEnvironmentType
            '   Load the starting points
            LoadCurveData UCorrectPoints(), %Vert_First, Sx1, Sy1, LTool1, LExt1
            LoadCurveData UCorrectPoints(), %Vert_Second, Sx2, Sy2, LTool2, LExt2
            '   Store the Points Of the Line
            AddCurveData UPoly(), Sx1, Sy1, 1, 1
            AddCurveData UPoly(), Sx2, Sy2, 2, 2
            AddCurveData UPoly(), Sx2, Sy2, 4, 4
            '   Store top points
            FOR LCurveIndex = %Vert_Third TO UBOUND(UCorrectPoints)-2 STEP 2
                LoadCurveData UCorrectPoints(), LCurveIndex+1, Sx3, Sy3, LTool3, LExt3
                '   Store the Points Of the Line
                AddCurveData UPoly(), Sx3, Sy3, 2, 2
                AddCurveData UPoly(), Sx3, Sy3, 4, 4
            NEXT
            '   Load the ending points
            LoadCurveData UCorrectPoints(), UBOUND(UCorrectPoints)-1, Sx3, Sy3, LTool3, LExt3
            LoadCurveData UCorrectPoints(), UBOUND(UCorrectPoints), Sx4, Sy4, LTool4, LExt4
            '   Store the Points Of the Line
            AddCurveData UPoly(), Sx4, Sy4, 2, 2
            AddCurveData UPoly(), Sx4, Sy4, 4, 4
            AddCurveData UPoly(), Sx3, Sy3, 1, 1
            AddCurveData UPoly(), Sx3, Sy3, 3, 3
            '   Store bottom points
            FOR LCurveIndex = UBOUND(UCorrectPoints)-2 TO %Vert_Third STEP -2
                LoadCurveData UCorrectPoints(), LCurveIndex-1, Sx5, Sy5, 0, LExt5
                '   Store the Points Of the Line
                AddCurveData UPoly(), Sx5, Sy5, 1, 1
                AddCurveData UPoly(), Sx5, Sy5, 3, 3
            NEXT
            '   Store the Points Of the Line
            AddCurveData UPoly(), Sx1, Sy1, 3, 3
            '   Yep the two windows overlap.  Chek if the line overlap
            Maths_GetLineMidPoint(Sx1, Sy1, Sx4, Sy4, SxM1, SyM1)
            Maths_GetLineMidPoint(Sx2, Sy2, Sx3, Sy3, SxM2, SyM2)
            Maths_GetLineMidPoint(SxM1, SyM1, SxM2, SyM2, SxP, SyP)
            '   Get the Dimension of the Seam Pattern
            GetPatternDimension UPoly(), SxMin, SyMin, SxMax, SyMax
            '   Yep it does.  Now, Chek if such an dimensions exists already
            LPolyIndex = GetClosestCornerOnCurveByIndexExt(UPrePolyDimen(), SxMin, SyMin)
            '   Check for valid polygon
            IF ABS(GetPatternArea(UPoly())) > SPieceArea AND (SXMax - SxMin) > SPieceWidth AND (SYMax - SyMin) > SPieceLength AND LPolyIndex = 0 THEN
                '   Store the dimensions of the polygon
                AddCurveData UPolyMinMax(), SxMin, SxMax, LPolygonCount, 0
                AddCurveData UPolyMinMax(), SyMin, SyMax, LPolygonCount, 0
                '   Increase the polygon count
                INCR LPolygonCount
                '   Get the starting position of the polygon
                LStartPoly = UBOUND(UReducePolygons)+1
                '   Reduce the points if has same angle
                ReduceTheCurve UPoly(), UReducePolygons(), LStartPoly
                '   Get the ending position of the polygon
                LEndPoly = UBOUND(UReducePolygons)
                '   Get the starting position of the polygon
                LStartPoint = UBOUND(UPolygons)+1
                '  Transfer the data in UCurve to UCurveCancel
                AddSourceCurveToDestin UPoly(), UPolygons()
                '   Get the ending position of the polygon
                LEndPoint = UBOUND(UPolygons)
                '   Store the center point of the polygon
                AddCurveData UPolyDimen(), LStartPoly, LEndPoly, LStartPoint, LEndPoint
                LStartPoint = UBOUND(UPolyCenter)+1
                FOR LCurveIndex = %Vert_First TO UBOUND(UCorrectPoints) STEP 2
                    LoadCurveData UCorrectPoints(), LCurveIndex, Sx3, Sy3, 0, 0
                    LoadCurveData UCorrectPoints(), LCurveIndex+1, Sx4, Sy4, 0, 0
                    Maths_GetLineMidPoint(Sx3, Sy3, Sx4, Sy4, SxP, SyP)
                    AddCurveData UPolyCenter(), SxP, SyP, LPolygonCount, 0
                    AddCurvePoints SPolyDist(), Maths_LengthOfLine(Sx3, Sy3, Sx4, Sy4)
                NEXT
                LEndPoint = UBOUND(UPolyCenter)
                UPolygons(UBOUND(UPolygons)).LDataType = LStartPoint : UPolygons(UBOUND(UPolygons)).SExtraData = LEndPoint
            END IF
        END IF
    NEXT
END SUB

SUB ReduceTheCurve(BYREF USource() AS DataBaseDataEnvironmentType, BYREF UDestin() AS DataBaseDataEnvironmentType, BYVAL LStartPoly)
    '   Slope values
    SameSlope = 400
    '   Parse thru all points
    FOR LCurveIndex = %Vert_First TO UBOUND(USource) STEP 2
        '   Load the points
        LoadCurveData USource(), LCurveIndex, Sx1, Sy1, LTool1, LExt1
        LoadCurveData USource(), LCurveIndex+1, Sx2, Sy2, LTool2, LExt2
        '   Get the angle of the line
        Maths_GetAngleOfLine Sx1, Sy1, Sx2, Sy2, SAngle
        '   Check the angle
        IF SameSlope = 400 THEN
            '   Store if condition satisfies
            SameSlope = SAngle
            '   Store the point in stoarge
            AddCurveData UDestin(), Sx1, Sy1, LTool1, LExt1
        ELSEIF SAngle <> SameSlope AND ABS(SAngle - SameSlope) > 0.1 THEN
            '   Store the point in stoarge
            AddCurveData UDestin(), Sx1, Sy1, LTool1, LExt1
            '   Store the point in stoarge
            AddCurveData UDestin(), Sx1, Sy1, LTool1, LExt1
            '   Restore the angle
            SameSlope = SAngle
        END IF
    NEXT
    '   Load the first point
    LoadCurveData UDestin(), LStartPoly, Sx1, Sy1, LTool1, LExt1
    '   Store the point in storage
    AddCurveData UDestin(), Sx1, Sy1, LTool1, LExt1
END SUB

SUB GetAllIntersectionPointsOfColumns (BYREF TColumn(), BYVAL SxStart, BYVAL SyStart, BYVAL SxTerm, BYVAL SyTerm)
    '   Parse thru all pieces
    FOR LIndex = %Vert_First TO UBOUND(UMarkerPlacement)
        '   Get the status of piece
        SStatus = UMarkerOthers(LIndex).SHorizontal
        '   Get the placement of the piece
        LoadCurveDataSingle UMarkerPlacement(), LIndex, SHorizontal, SVertical, 0, 0
        '   Get the Dimension of the Pattern
        LoadCurveDataSingle UMarkerDimension(), LIndex, SHorizontalSize, SVerticalSize, 0, 0
        '   Define the Border of the placed Pattern
        SxMin = SHorizontal - SHorizontalSize / 2 : SxMax = SHorizontal + SHorizontalSize / 2
        SyMin = SVertical - SVerticalSize / 2 : SyMax = SVertical + SVerticalSize / 2
        '   Check the status of piece
        IF (SStatus = %MARKER_PIECE_PLACED OR SStatus = %MARKER_PIECE_HANGING) AND SxMin => SxStart AND SxMax <= SxTerm AND SyMin => SyStart AND SyMax <= SyTerm THEN
            '   Get the Pattern Curve for the Pattern to Bump
            GetPieceCurveForPiece LIndex
            '   Get the dimension of the piece
            GetPatternDimension USeamPattern(), SxMin, SyMin, SxMax, SyMax
            '   Parse thru all the lines of the Second Curve
            FOR LCurveIndex = %Vert_First TO UBOUND(USeamPattern) STEP %Vert_Second
                '   Get the coordinates of the curve
                LoadCurveData USeamPattern(), LCurveIndex, Sx1, Sy1, 0, 0
                LoadCurveData USeamPattern(), LCurveIndex + 1, Sx2, Sy2, 0, 0
                '   Find the points lies on the line in Y
                GetPointsOnCurveY TColumn(), Sx1, Sy1, Sx2, Sy2
            NEXT
        END IF
    NEXT
END SUB

SUB GetPointsOnCurveY (BYREF TColumn(), BYVAL Sx1, BYVAL Sy1, BYVAL Sx2, BYVAL Sy2)
    '   Calculate the number of points to find
    SCount = ABS(FIX(Sy1)- FIX(Sy2))
    '   Slope of the line
    Slope = (Sy2-Sy1) / (Sx2-Sx1)
    'IF SCount = 0 THEN SCount = 1 : Slope = 1
    '   Swap for minimum
    Sy3 = MIN(Sy1,Sy2) : Sx3 = IIF(Sy3 = Sy2, Sx2, Sx1)
    '   Parse thru number of points
    FOR LPointIndex = %Vert_First TO SCount
        '   Find the point on X
        SxP = (FIX(Sy3) + LPointIndex - Sy3) / Slope + Sx3
        '   Find the point on Y
        SyP = FIX(Sy3) + LPointIndex
        '   Store the point in storage
        TColumn(SyP) += IIF$(TColumn(SyP) = "", STR$(SxP), "," + STR$(SxP))
    NEXT
END SUB

SUB GetPointsOnCurveYExt (BYVAL Sx1, BYVAL Sy1, BYVAL Sx2, BYVAL Sy2, BYVAL SMin, BYREF SyDist(), BYREF UTop() AS DataBaseDataEnvironmentType, _
    BYREF UBtm() AS DataBaseDataEnvironmentType, BYVAL LLast)

    '   Calculate the number of points to find
    SCount = ABS(FIX(Sy1)- FIX(Sy2))
    '   Slope of the line
    Slope = (Sy2-Sy1) / (Sx2-Sx1)
    '   Swap for minimum
    Sy3 = MIN(Sy1,Sy2) : Sx3 = IIF(Sy3 = Sy2, Sx2, Sx1)
    '   Parse thru number of points
    FOR LPointIndex = %Vert_First TO SCount
        '   Find the point on X
        SxP = (FIX(Sy3) + LPointIndex - Sy3) / Slope + Sx3
        '   Find the point on Y
        SyP = FIX(Sy3) + LPointIndex
        '   Store the point in storage
        IF UTop(SyP-SMin).SVertical <> 0 AND UTop(SyP-SMin).SHorizontal < SxP THEN UTop(SyP-SMin).SHorizontal = SxP
        IF UBtm(SyP-SMin).SVertical <> 0 AND UBtm(SyP-SMin).SHorizontal > SxP THEN UBtm(SyP-SMin).SHorizontal = SxP

        IF UTop(SyP-SMin).SVertical = 0 THEN UTop(SyP-SMin).SHorizontal = SxP : UTop(SyP-SMin).SVertical = SyP
        IF UBtm(SyP-SMin).SVertical = 0 THEN UBtm(SyP-SMin).SHorizontal = SxP : UBtm(SyP-SMin).SVertical = SyP
        '   Get the Width Of the Line
        SDistance = Maths_LengthOfLine(UTop(SyP-SMin).SHorizontal, SyP, UBtm(SyP-SMin).SHorizontal, SyP)

        LSize = LLast + (SyP-SMin)

        SyDist(LSize) = IIF(SyDist(LSize) < SDistance, SDistance, SyDist(LSize))
    NEXT
END SUB

SUB GetPointsOnCurveYExt2 (BYVAL Sx1, BYVAL Sy1, BYVAL Sx2, BYVAL Sy2, BYVAL SMin, BYREF UTop() AS DataBaseDataEnvironmentType, BYREF UBtm() AS DataBaseDataEnvironmentType)
    '   Calculate the number of points to find
    SCount = ABS(FIX(Sy1)- FIX(Sy2))
    '   Slope of the line
    Slope = (Sy2-Sy1) / (Sx2-Sx1)
    '   Swap for minimum
    Sy3 = MIN(Sy1,Sy2) : Sx3 = IIF(Sy3 = Sy2, Sx2, Sx1)
    '   Parse thru number of points
    FOR LPointIndex = %Vert_First TO SCount
        '   Find the point on X
        SxP = (FIX(Sy3) + LPointIndex - Sy3) / Slope + Sx3
        '   Find the point on Y
        SyP = FIX(Sy3) + LPointIndex
        '   Store the point in storage
        IF UTop(SyP-SMin).SVertical <> 0 AND UTop(SyP-SMin).SHorizontal < SxP THEN UTop(SyP-SMin).SHorizontal = SxP
        IF UBtm(SyP-SMin).SVertical <> 0 AND UBtm(SyP-SMin).SHorizontal > SxP THEN UBtm(SyP-SMin).SHorizontal = SxP

        IF UTop(SyP-SMin).SVertical = 0 THEN UTop(SyP-SMin).SHorizontal = SxP : UTop(SyP-SMin).SVertical = SyP
        IF UBtm(SyP-SMin).SVertical = 0 THEN UBtm(SyP-SMin).SHorizontal = SxP : UBtm(SyP-SMin).SVertical = SyP
    NEXT
END SUB

SUB AddThePieceToBump (BYVAL LIndex)
    '   Get the Pattern Curve for the Pattern to Bump
    GetPieceCurveForPiece LIndex
    '   Get the dimension of the curve
    GetPatternDimension USeamPattern(), SxMin, SyMin, SxMax, SyMax
    '   Storage for max and min for the dimensions
    REDIM UDimensions(%Vert_Zero) AS DataBaseDataEnvironmentType
    '   Get the max and min for the dimensions
    GetPatternDimensionExt USeamPattern(), SxMin, SyMin, SxMax, SyMax, UDimensions()
    '   Storage for all side points
    REDIM UTop(%Vert_Zero) AS DataBaseDataEnvironmentType  : REDIM UBtm(%Vert_Zero) AS DataBaseDataEnvironmentType
    REDIM ULeft(%Vert_Zero) AS DataBaseDataEnvironmentType : REDIM URight(%Vert_Zero) AS DataBaseDataEnvironmentType
    '   Get the size of the curve
    LSize = UBOUND(USeamPattern)
    '   Get the index of all corners
    LThird = UDimensions(%Vert_Third).LDataType     : LEighth = UDimensions(%Vert_Eighth).LDataType     '    Down
    LFirst = UDimensions(%Vert_First).LDataType     : LSixth = UDimensions(%Vert_Sixth).LDataType       '    Left
    LFourth = UDimensions(%Vert_Fourth).LDataType   : LSeventh = UDimensions(%Vert_Seventh).LDataType   '    Top
    LSecond = UDimensions(%Vert_Second).LDataType   : LFifth = UDimensions(%Vert_Fifth).LDataType       '    Right
    '   Get the Down side points of the curve
    AddSourceCurveToDestinByRangeSide USeamPattern(), UBtm(), LEighth, LThird
    '   Get the Left side points of the curve
    AddSourceCurveToDestinByRangeSide USeamPattern(), ULeft(), LFirst, LSixth
    '   Get the Top side points of the curve
    AddSourceCurveToDestinByRangeSide USeamPattern(), UTop(), LFourth, LSeventh
    '   Get the Right side points of the curve
    AddSourceCurveToDestinByRangeSide USeamPattern(), URight(), LFifth, LSecond
    '   Store the piece left and right side for ease bump
    StoreThePointsBackToStorageX ULeft(), URight(), LIndex
    '   Store the piece top and bottom side for ease bump
    StoreThePointsBackToStorageY UTop(), UBtm(), LIndex
END SUB

SUB GetSmallestLAW(BYREF UPieceSmallIndex() AS DataBaseDataEnvironmentType, BYREF SPieceWidth, BYREF SPieceLength, BYREF SPieceArea)
    '   Store the max values
    SPieceWidth = 1E10 : SPieceLength = 1E10 : SPieceArea = 1E10
    '   Parse thru all the pieces
    FOR LLineIndex = %Vert_First TO UBOUND(UPieceSmallIndex)
        '   Get the piece Index from storage
        LPieceIndex = UPieceSmallIndex(LLineIndex).SHorizontal
        '   Check the State of this Piece
        IF UMarkerOthers(LPieceIndex).SHorizontal = %MARKER_PIECE_PLACED THEN ITERATE FOR
        '   Get the dimensions of piece
        LoadCurveDataSingle UMarkerDimension(), LPieceIndex, SWidth, SLength, 0, SArea
        '   Check for smallest width
        IF SWidth < SPieceWidth THEN SPieceWidth = SWidth
        '   Check for smallest length
        IF SLength < SPieceLength THEN SPieceLength = SLength
        '   Check for smallest area
        IF SArea < SPieceArea THEN SPieceArea = SArea
    NEXT
END SUB

SUB sortSmallestPieces(BYREF UPieceSmallIndex() AS DataBaseDataEnvironmentType, BYVAL TSizes)
    '   Storage allocation for pieceindex to sort
    REDIM pieceArea(1 TO 1) AS SINGLE, pieceSmallIndex(1 TO 1) AS SINGLE
    '   Get the lay mode from storage
    SLayMode = SMarkerEnviron(%SINGLE_MARKER_LAYMODE)
    '   Count variable
    LIndex = 1
    '   Check for warp way
    IF TSizes <> "" THEN
        '   Storage to store the sizes in warp ways
        REDIM TWarpSize(1 TO PARSECOUNT(TSizes))
        '   Store all sizes to storage
        PARSE TSizes, TWarpSize()
    END IF
    '   Parse thru all the pieces
    FOR LLineIndex = %Vert_First TO UBOUND(UMarkerPlacement)
        '   Check the State of this Piece
        IF UMarkerOthers(LLineIndex).SHorizontal = %MARKER_PIECE_HIDDEN OR UMarkerSmallPattern(UMarkerIndex(LLineIndex).SVertical).LDataType = 2 OR _
            UMarkerSmallPattern(UMarkerIndex(LLineIndex).SVertical).LDataType = 0 THEN ITERATE FOR
        '   Scan the size is present in the way of warp
        ARRAY SCAN TWarpSize(1), =TStyleSizeName(UMarkerIndex(LLineIndex).SHorizontal), TO LFound
        '   Check if piece selected for warp
        IF UBOUND(TWarpSize) > -1 AND LFound = 0 THEN ITERATE
        '   Redefine the Dimensions & Area of the Piece
        SaveTheDimensionOfThePiece LLineIndex
        '   Storage allocation for pieceindex to sort
        REDIM PRESERVE pieceArea(1 TO LIndex) AS SINGLE, pieceIndex(1 TO LIndex) AS SINGLE
        '   Store the piece length/area from storage
        pieceArea(LIndex) = IIF(SLayMode,UMarkerDimension(LLineIndex).SExtraData,UMarkerDimension(LLineIndex).SVertical)
        '   Store the piece index
        pieceIndex(LIndex) = LLineIndex
        '   Increase count
        INCR LIndex
        '   Get the Piece Index from Storage area
        LoadCurveDataSingle UMarkerIndex(), LLineIndex, SIndexOfSize, SIndexOfPiece, 0, SIndexOfGarment
        '   Reset the count of placed pieces
        UMarkerPieces(SIndexOfSize, SIndexOfPiece).SVertical = 0
        '   Set the State of this Piece to be Not Placed
        UMarkerOthers(LLineIndex).SHorizontal = %MARKER_PIECE_NOTPLACED
    NEXT LLineIndex

    ARRAY SORT pieceArea(1), TAGARRAY pieceIndex(), DESCEND

    REDIM SPieceArray(%Vert_Zero)

    FOR LPieceIndex = 1 TO UBOUND(pieceIndex)
        IF pieceIndex(LPieceIndex) <> 0 THEN
            '   Get the Piece Index from Storage area
            LoadCurveDataSingle UMarkerIndex(), pieceIndex(LPieceIndex), SIndexOfSize, SIndexOfPiece, 0, 0

            AddCurvePoints SPieceArray(), pieceIndex(LPieceIndex)

            FOR LCopyIndex = LPieceIndex+1 TO UBOUND(pieceIndex)

                IF pieceIndex(LCopyIndex) <> 0 THEN
                    '   Get the Piece Index from Storage area
                    LoadCurveDataSingle UMarkerIndex(), pieceIndex(LCopyIndex), SSizeIndex, SPieceIndex, 0, 0

                    IF SIndexOfSize = SSizeIndex AND SIndexOfPiece = SPieceIndex THEN
                        AddCurvePoints SPieceArray(), pieceIndex(LCopyIndex)
                        pieceIndex(LCopyIndex) = 0
                    END IF
                END IF
            NEXT
        END IF
    NEXT

    REDIM pieceIndex(%Vert_Zero)

    FOR LPieceIndex = 1 TO UBOUND(SPieceArray)
        AddCurvePoints pieceIndex(), SPieceArray(LPieceIndex)
        '   Add the piece index to storage
        AddCurveData UPieceSmallIndex(), SPieceArray(LPieceIndex), 0, 0, 0
    NEXT

END SUB

'   Sub to sort pattern based on length or area choosed by user
FUNCTION sortByArea(BYREF UPieceIndex() AS DataBaseDataEnvironmentType, BYVAL SCombiLoop AS SINGLE, BYREF LUMarkerTubular(), BYVAL TSizes) AS LONG
    '   Storage allocation for pieceindex to sort
    REDIM pieceArea(1 TO 1) AS SINGLE, pieceIndex(1 TO 1) AS SINGLE
    '   Set the flip for all pieces
    ApplyFlipForAllPieces           '   2022 GD February
    '   Get the lay mode from storage
    SLayMode = SMarkerEnviron(%SINGLE_MARKER_LAYMODE)
    '   Count variable
    LIndex = UBOUND(pieceIndex)
    '   Check for warp way
    IF TSizes <> "" THEN
        '   Storage to store the sizes in warp ways
        REDIM TWarpSize(1 TO PARSECOUNT(TSizes))
        '   Store all sizes to storage
        PARSE TSizes, TWarpSize()
    END IF
    '   Parse thru all the pieces
    FOR LLineIndex = %Vert_First TO UBOUND(UMarkerPlacement)
        '   Check the State of this Piece
        IF UMarkerOthers(LLineIndex).SHorizontal = %MARKER_PIECE_HIDDEN OR UMarkerSmallPattern(UMarkerIndex(LLineIndex).SVertical).LDataType = 1 THEN ITERATE FOR
        '   Scan the size is present in the way of warp
        ARRAY SCAN TWarpSize(1), =TStyleSizeName(UMarkerIndex(LLineIndex).SHorizontal), TO LFound
        '   Check if piece selected for warp
        IF UBOUND(TWarpSize) > -1 AND LFound = 0 THEN ITERATE
        '   Redefine the Dimensions & Area of the Piece
        SaveTheDimensionOfThePiece LLineIndex
        '   Storage allocation for pieceindex to sort
        REDIM PRESERVE pieceArea(1 TO LIndex) AS SINGLE, pieceIndex(1 TO LIndex) AS SINGLE
        '   Store the piece length/area/width from storage
        'pieceArea(LIndex) = IIF(SLayMode,UMarkerDimension(LLineIndex).SExtraData, UMarkerDimension(LLineIndex).SVertical)
        SELECT CASE SLayMode
            CASE 0 : pieceArea(LIndex) = UMarkerDimension(LLineIndex).SVertical
            CASE 1 : pieceArea(LIndex) = UMarkerDimension(LLineIndex).SExtraData
            CASE 2 : pieceArea(LIndex) = UMarkerDimension(LLineIndex).SHorizontal
        END SELECT
        '   Store the piece index
        pieceIndex(LIndex) = LLineIndex
        '   Increase count
        INCR LIndex
        '   Get the Piece Index from Storage area
        LoadCurveDataSingle UMarkerIndex(), LLineIndex, SIndexOfSize, SIndexOfPiece, 0, SIndexOfGarment
        '   Reset the count of placed pieces
        UMarkerPieces(SIndexOfSize, SIndexOfPiece).SVertical = 0
        '   Set the State of this Piece to be Not Placed
        UMarkerOthers(LLineIndex).SHorizontal = %MARKER_PIECE_NOTPLACED
    NEXT LLineIndex

    ARRAY SORT pieceArea(1), TAGARRAY pieceIndex(), DESCEND

    REDIM LCombiNation(0), LSizeCombi(0), LPieceArray(UBOUND(pieceIndex))

    FOR LMarkerIndex = %Vert_First TO UBOUND(pieceIndex)
        LPieceArray(LMarkerIndex) = pieceIndex(LMarkerIndex)
        '   Get the Piece Index from Storage area
        LoadCurveDataSingle UMarkerIndex(), pieceIndex(LMarkerIndex), SIndexOfSize, SIndexOfPiece, 0, SIndexOfGarment
        ARRAY SCAN LCombiNation(), =SIndexOfPiece, TO LLineIndex
        IF LLineIndex = 0 THEN
            REDIM PRESERVE LCombiNation(UBOUND(LCombiNation) + 1)
            LCombiNation(UBOUND(LCombiNation)) = SIndexOfPiece
        END IF

        ARRAY SCAN LSizeCombi(), =SIndexOfSize, TO LLineIndex
        IF LLineIndex = 0 THEN
            REDIM PRESERVE LSizeCombi(UBOUND(LSizeCombi) + 1)
            LSizeCombi(UBOUND(LSizeCombi)) = SIndexOfSize
        END IF
    NEXT LMarkerIndex

    LLineIndex = 0

    IF LMarkerEnviron(%LONG_MARKER_MARKERCOMBINATION) THEN
        SELECT CASE SCombiLoop
            CASE 2 : SCombiLoop = 5
            CASE 4 : SCombiLoop = 21
            CASE 6 : SCombiLoop = 26
            CASE 8 : SCombiLoop = 42
            CASE 9 : SCombiLoop = 43
        END SELECT
    END IF

    IF SCombiLoop > 4 AND SCombiLoop < 21 THEN   'Combination of a largest and smallest as a pair of the same pattern and goes on....

        FOR LIndex = %Vert_First TO UBOUND(LCombiNation)
            LPieceIndex = LCombiNation(LIndex)
            LPieceCount = 0
            FOR LMarkerIndex = %Vert_First TO UBOUND(LPieceArray)
                '   Get the Piece Index from Storage area
                LoadCurveDataSingle UMarkerIndex(), LPieceArray(LMarkerIndex), SIndexOfSize, SIndexOfPiece, 0, SIndexOfGarment
                IF SIndexOfPiece = LPieceIndex THEN
                    INCR LLineIndex : INCR LPieceCount
                    pieceIndex(LLineIndex) = LPieceArray(LMarkerIndex)
                END IF
            NEXT LMarkerIndex

            IF SCombiLoop >= 5 AND SCombiLoop <= 8 THEN LTempInd = 1 : LFlag = -1
            IF SCombiLoop >= 9 AND SCombiLoop <= 12 THEN LTempInd = 0 : LFlag = -1
            IF SCombiLoop >= 13 AND SCombiLoop <= 16 THEN LTempInd = 1 : LFlag = 1
            IF SCombiLoop >= 17 AND SCombiLoop <= 20 THEN LTempInd = 0 : LFlag = 0

            FOR LSortIndex = LLineIndex - LPieceCount + 1 TO LLineIndex - LPieceCount + (CEIL(LPieceCount/2)-1)
                LTempIndex = pieceIndex(LLineIndex)
                ARRAY DELETE pieceIndex(LLineIndex) FOR 1, 0
                ARRAY INSERT pieceIndex(LSortIndex + LTempInd), LTempIndex

                IF LFlag = 0 THEN
                    LTempInd += 2 : LFlag = 1
                ELSEIF LFlag = 1 THEN
                    LFlag = 0
                ELSE
                    LTempInd += 1
                END IF

            NEXT LSortIndex

        NEXT LIndex

    END IF

    LLineIndex = 0
    '   For tubular change logic in it
    '   it adds 1 open 1 tubular only but it will change too automatically identifies number of open and tubular are fit to a width and then add it
    '   also check tubular percentage for this lay D:\STYLES\yellow t shirt\Tubular.Lay
    IF SCombiLoop = 21 AND LMarkerEnviron(%LONG_MARKER_FABRICTYPE) = %FABRICTYPE_TUBULAR THEN
        FOR LIndex = %Vert_First TO UBOUND(LCombiNation)

            LPieceIndex = LCombiNation(LIndex)

            LPieceCount = 0 : REDIM STempArray(0), STempArray1(0)
            FOR LMarkerIndex = %Vert_First TO UBOUND(LPieceArray)
                '   Get the Piece Index from Storage area
                LoadCurveDataSingle UMarkerIndex(), LPieceArray(LMarkerIndex), SIndexOfSize, SIndexOfPiece, 0, SIndexOfGarment
                IF SIndexOfPiece = LPieceIndex THEN
                    INCR LLineIndex : INCR LPieceCount
                    pieceIndex(LLineIndex) = LPieceArray(LMarkerIndex)
                    '   Check for tubular or open pattern
                    IF LUMarkerTubular(pieceIndex(LLineIndex)) = 3 THEN
                        REDIM PRESERVE STempArray(UBOUND(STempArray)+1)
                        STempArray(UBOUND(STempArray)) = pieceIndex(LLineIndex)
                    ELSEIF LUMarkerTubular(pieceIndex(LLineIndex)) = 2 THEN
                        REDIM PRESERVE STempArray1(UBOUND(STempArray1)+1)
                        STempArray1(UBOUND(STempArray1)) = pieceIndex(LLineIndex)
                    END IF
                END IF
            NEXT LMarkerIndex

            IF UBOUND(STempArray) = 0 OR UBOUND(STempArray1) = 0 THEN
                ITERATE FOR
            ELSEIF UBOUND(STempArray) = UBOUND(STempArray1) * 2 THEN
                LTempInd = LLineIndex - LPieceCount + 1 : LDoubleIndex = 1
                FOR LMarkerIndex = 1 TO UBOUND(STempArray1)
                    pieceIndex(LTempInd) = STempArray(LDoubleIndex)
                    pieceIndex(LTempInd+1) = STempArray1(UBOUND(STempArray1) - (LMarkerIndex-1))
                    pieceIndex(LTempInd+2) = STempArray(LDoubleIndex + 1)
                    LTempInd += 3 : LDoubleIndex += 2
                NEXT
            ELSEIF UBOUND(STempArray) = UBOUND(STempArray1) THEN       '''''
                LTempInd = LLineIndex - LPieceCount + 1
                FOR LMarkerIndex = 1 TO UBOUND(STempArray)
                    pieceIndex(LTempInd) = STempArray1(LMarkerIndex)
                    pieceIndex(LTempInd+1) = STempArray(UBOUND(STempArray) - (LMarkerIndex-1))
                    LTempInd += 2
                NEXT
            END IF

            ERASE STempArray(), STempArray1()

        NEXT LIndex
    END IF

    LLineIndex = 0

    IF SCombiLoop > 25 AND SCombiLoop < 42 THEN   ' Combination of the largest and smallest from 2 bigger pattern sets and goes on...

        LNumberOfPieces = IIF(UBOUND(LCombiNation) MOD 2, UBOUND(LCombiNation) - 1, UBOUND(LCombiNation))
        LNumberOfPieces = UBOUND(LCombiNation)

        FOR LIndex = %Vert_First TO LNumberOfPieces STEP 2
            LPieceIndex = LCombiNation(LIndex)
            LPieceIndex1 = LCombiNation(LIndex + 1)
            LPieceCount = 0

            FOR LMarkerIndex = %Vert_First TO UBOUND(LPieceArray)
                '   Get the Piece Index from Storage area
                LoadCurveDataSingle UMarkerIndex(), LPieceArray(LMarkerIndex), SIndexOfSize, SIndexOfPiece, 0, SIndexOfGarment
                IF SIndexOfPiece = LPieceIndex THEN
                    INCR LLineIndex : INCR LPieceCount
                    pieceIndex(LLineIndex) = LPieceArray(LMarkerIndex)
                END IF
            NEXT LMarkerIndex

            FOR LMarkerIndex = %Vert_First TO UBOUND(LPieceArray)
                '   Get the Piece Index from Storage area
                LoadCurveDataSingle UMarkerIndex(), LPieceArray(LMarkerIndex), SIndexOfSize, SIndexOfPiece, 0, SIndexOfGarment
                'IF SIndexOfPiece = LPieceIndex1 AND LNumberOfPieces => LIndex + 1 THEN
                IF SIndexOfPiece = LPieceIndex1 THEN
                    INCR LLineIndex : INCR LPieceCount
                    pieceIndex(LLineIndex) = LPieceArray(LMarkerIndex)
                END IF
            NEXT LMarkerIndex

            IF SCombiLoop >= 26 AND SCombiLoop <= 29 THEN LTempInd = 1 : LFlag = -1
            IF SCombiLoop >= 30 AND SCombiLoop <= 33 THEN LTempInd = 0 : LFlag = -1
            IF SCombiLoop >= 34 AND SCombiLoop <= 37 THEN LTempInd = 1 : LFlag = 1
            IF SCombiLoop >= 38 AND SCombiLoop <= 41 THEN LTempInd = 0 : LFlag = 0

            FOR LSortIndex = LLineIndex - LPieceCount + 1 TO LLineIndex - LPieceCount + (CEIL(LPieceCount/2)-1)
                LTempIndex = pieceIndex(LLineIndex)
                ARRAY DELETE pieceIndex(LLineIndex) FOR 1, 0
                ARRAY INSERT pieceIndex(LSortIndex + LTempInd), LTempIndex

                IF LFlag = 0 THEN
                    LTempInd += 2 : LFlag = 1
                ELSEIF LFlag = 1 THEN
                    LFlag = 0
                ELSE
                    LTempInd += 1
                END IF

            NEXT LSortIndex

'            FOR LMarkerIndex = %Vert_First TO UBOUND(LPieceArray)
'                '   Get the Piece Index from Storage area
'                LoadCurveDataSingle UMarkerIndex(), LPieceArray(LMarkerIndex), SIndexOfSize, SIndexOfPiece, 0, SIndexOfGarment
'                IF SIndexOfPiece = LPieceIndex1 THEN
'                    INCR LLineIndex : INCR LPieceCount
'                    pieceIndex(LLineIndex) = LPieceArray(LMarkerIndex)
'                END IF
'            NEXT LMarkerIndex
'
'            LTempInd = 1
'            FOR LSortIndex = LLineIndex - LPieceCount + 1 TO LLineIndex - LPieceCount + (CEIL(LPieceCount/2))
'                IF LSortIndex + LTempInd > LLineIndex THEN ITERATE              '   2021 GD August to avoid index out of bound
'                LTempIndex = pieceIndex((CEIL(LPieceCount/2)) + LTempInd)
'                ARRAY DELETE pieceIndex((CEIL(LPieceCount/2)) + LTempInd)
'                ARRAY INSERT pieceIndex(LSortIndex + LTempInd), LTempIndex
'                LTempInd += 1
'            NEXT LSortIndex
'
'            LTempInd = 1
'            FOR LSortIndex = LLineIndex - LPieceCount + 1 TO LLineIndex - LPieceCount + (FIX(LPieceCount/2))      ' 2021 GD CEIL
'                IF LSortIndex + LTempInd > LLineIndex THEN ITERATE              '   2021 GD August to avoid index out of bound
'                LTempIndex = pieceIndex(LLineIndex)
'                ARRAY DELETE pieceIndex(LLineIndex)
'                ARRAY INSERT pieceIndex(LSortIndex + LTempInd), LTempIndex
'                LTempInd += 1
'            NEXT LSortIndex

        NEXT LIndex

        IF UBOUND(LCombiNation) MOD 2 THEN
            LPieceIndex = LCombiNation(UBOUND(LCombiNation))
            FOR LMarkerIndex = %Vert_First TO UBOUND(LPieceArray)
                LoadCurveDataSingle UMarkerIndex(), LPieceArray(LMarkerIndex), SIndexOfSize, SIndexOfPiece, 0, SIndexOfGarment
                IF SIndexOfPiece = LPieceIndex THEN
                    INCR LLineIndex
                    pieceIndex(LLineIndex) = LPieceArray(LMarkerIndex)
                END IF
            NEXT
        END IF

        ARRAY SCAN pieceIndex(), = 0, TO LPieceIndex
        IF LPieceIndex THEN REDIM pieceIndex(0)
    END IF

    IF (SCombiLoop = 42 OR SCombiLoop = 43) THEN                                      '   paired

        FOR LIndex = %Vert_First TO UBOUND(LCombiNation)
            '   Get the symmetric direction
            SSymmetric = UMarkerExtras(LCombiNation(LIndex)).SExtraData
            '   Parse thru all the pieces
            FOR LMarkerIndex = %Vert_First TO UBOUND(UMarkerPlacement)
                '   Get the Piece Index from Storage area
                LoadCurveDataSingle UMarkerIndex(), LMarkerIndex, SIndexOfSize, SIndexOfPiece, 0, SIndexOfGarment
                '   Check the piece index
                IF LCombiNation(LIndex) = SIndexOfPiece AND SSymmetric = 1 THEN
                    SELECT CASE UMarkerStatus(LMarkerIndex).SHorizontal
                        CASE %MARKER_PIECE_CANBEFLIPPED   : UMarkerStatus(LMarkerIndex).SHorizontal = %MARKER_PIECE_FLIPPED
                        CASE %MARKER_PIECE_FLIPPED        : UMarkerStatus(LMarkerIndex).SHorizontal = %MARKER_PIECE_CANBEFLIPPED
                        CASE %MARKER_PIECE_FLAPPED        : UMarkerStatus(LMarkerIndex).SHorizontal = %MARKER_PIECE_FLIPFLAPPED
                        CASE %MARKER_PIECE_FLIPFLAPPED    : UMarkerStatus(LMarkerIndex).SHorizontal = %MARKER_PIECE_FLAPPED
                    END SELECT
                ELSEIF LCombiNation(LIndex) = SIndexOfPiece AND SSymmetric = 3 THEN
                    SELECT CASE UMarkerStatus(LMarkerIndex).SHorizontal
                        CASE %MARKER_PIECE_CANBEFLIPPED   : UMarkerStatus(LMarkerIndex).SHorizontal = %MARKER_PIECE_FLIPFLAPPED
                        CASE %MARKER_PIECE_FLIPPED        : UMarkerStatus(LMarkerIndex).SHorizontal = %MARKER_PIECE_FLAPPED
                        CASE %MARKER_PIECE_FLAPPED        : UMarkerStatus(LMarkerIndex).SHorizontal = %MARKER_PIECE_FLIPPED
                        CASE %MARKER_PIECE_FLIPFLAPPED    : UMarkerStatus(LMarkerIndex).SHorizontal = %MARKER_PIECE_CANBEFLIPPED
                    END SELECT
                ELSEIF LCombiNation(LIndex) = SIndexOfPiece AND SSymmetric = 4 THEN
                    SELECT CASE UMarkerStatus(LMarkerIndex).SHorizontal
                        CASE %MARKER_PIECE_CANBEFLIPPED   : UMarkerStatus(LMarkerIndex).SHorizontal = %MARKER_PIECE_FLAPPED
                        CASE %MARKER_PIECE_FLIPPED        : UMarkerStatus(LMarkerIndex).SHorizontal = %MARKER_PIECE_FLIPFLAPPED
                        CASE %MARKER_PIECE_FLAPPED        : UMarkerStatus(LMarkerIndex).SHorizontal = %MARKER_PIECE_CANBEFLIPPED
                        CASE %MARKER_PIECE_FLIPFLAPPED    : UMarkerStatus(LMarkerIndex).SHorizontal = %MARKER_PIECE_FLIPPED
                    END SELECT
                END IF
            NEXT
        NEXT

        IF SCombiLoop = 42 THEN
            LLineIndex = 0

            FOR LIndex = %Vert_First TO UBOUND(LSizeCombi)
                LSizeIndex = LSizeCombi(LIndex)
                '   Get the Ratio for this Size
                LMarkerRatio = UMarkerRatio(LSizeIndex).SExtraData

                LNumberOfPieces = IIF(UBOUND(LCombiNation) MOD 2, UBOUND(LCombiNation) - 1, UBOUND(LCombiNation))

                FOR LPieceIndex = 1 TO UBOUND(LCombiNation)'LNumberOfPieces STEP 2

                    FOR LMarkerIndex = %Vert_First TO UBOUND(LPieceArray)
                        '   Get the Piece Index from Storage area
                        LoadCurveDataSingle UMarkerIndex(), LPieceArray(LMarkerIndex), SIndexOfSize, SIndexOfPiece, 0, SIndexOfGarment
                        IF SIndexOfSize = LSizeIndex AND LCombiNation(LPieceIndex) = SIndexOfPiece THEN
                            INCR LLineIndex
                            pieceIndex(LLineIndex) = LPieceArray(LMarkerIndex)
                        END IF
                    NEXT LMarkerIndex

                    'FOR LMarkerIndex = %Vert_First TO UBOUND(LPieceArray)
                        '   Get the Piece Index from Storage area
                        'LoadCurveDataSingle UMarkerIndex(), LPieceArray(LMarkerIndex), SIndexOfSize, SIndexOfPiece, 0, SIndexOfGarment
                        'IF SIndexOfSize = LSizeIndex AND LCombiNation(LPieceIndex+1) = SIndexOfPiece THEN
                            'INCR LLineIndex
                            'pieceIndex(LLineIndex) = LPieceArray(LMarkerIndex)
                        'END IF
                    'NEXT LMarkerIndex
                NEXT LPieceIndex
            NEXT LIndex

        ELSEIF SCombiLoop = 43 THEN

            LNumberOfPieces = IIF(UBOUND(LCombiNation) MOD 2, UBOUND(LCombiNation) - 1, UBOUND(LCombiNation))

            LLineIndex = 0

            NinenthCombi LPieceArray(), LSizeCombi(), LCombiNation(), 1

            FOR LIndex = %Vert_First TO LNumberOfPieces STEP 2
                LPieceIndex = LCombiNation(LIndex)
                LPieceIndex1 = LCombiNation(LIndex + 1)

                LPieceCount = 0
                REDIM STemp1(0), STemp2(0), STemp(0)

                FOR LMarkerIndex = %Vert_First TO UBOUND(LPieceArray)
                    '   Get the Piece Index from Storage area
                    LoadCurveDataSingle UMarkerIndex(), LPieceArray(LMarkerIndex), SIndexOfSize, SIndexOfPiece, 0, SIndexOfGarment
                    IF SIndexOfPiece = LPieceIndex THEN

                        REDIM PRESERVE STemp1(UBOUND(STemp1)+1)
                        STemp1(UBOUND(STemp1)) = LPieceArray(LMarkerIndex)

                        ARRAY SCAN pieceIndex(1), = LPieceArray(LMarkerIndex), TO LFound
                        ARRAY DELETE pieceIndex(LFound)
                    ELSEIF SIndexOfPiece = LPieceIndex1 THEN

                        REDIM PRESERVE STemp2(UBOUND(STemp2)+1)
                        STemp2(UBOUND(STemp2)) = LPieceArray(LMarkerIndex)

                        ARRAY SCAN pieceIndex(1), = LPieceArray(LMarkerIndex), TO LFound
                        ARRAY DELETE pieceIndex(LFound)
                    END IF
                NEXT LMarkerIndex

                FOR LMarkerIndex = %Vert_First TO UBOUND(STemp1)+UBOUND(STemp2) STEP 4

                    INCR LPieceCount
                    IF (LPieceCount MOD 4 = 1) OR (LPieceCount MOD 4 = 0) THEN
                        ''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''1
                        IF LPieceCount MOD 4 = 0 THEN CopySourceToDestin STemp2(), STemp() _
                            ELSE CopySourceToDestin STemp1(), STemp()
                        '********************************************************************************************************
                        FOR LSortIndex = %Vert_First TO UBOUND(STemp)
                            IF UMarkerStatus(STemp(LSortIndex)).SHorizontal = %MARKER_PIECE_FLIPPED THEN
                                INCR LLineIndex
                                ARRAY INSERT pieceIndex(LLineIndex), STemp(LSortIndex)
                                ARRAY DELETE STemp(LSortIndex)
                                REDIM PRESERVE STemp(UBOUND(STemp)-1)
                                EXIT FOR
                            END IF
                        NEXT
                        '********************************************************************************************************
                        IF LPieceCount MOD 4 = 0 THEN CopySourceToDestin STemp(), STemp2() _
                            ELSE CopySourceToDestin STemp(), STemp1()
                        ''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''2
                        IF LPieceCount MOD 4 = 1 THEN CopySourceToDestin STemp2(), STemp() _
                            ELSE CopySourceToDestin STemp1(), STemp()

                        LFlipped = IIF(UMarkerExtras(LPieceIndex).SExtraData <> UMarkerExtras(LPieceIndex1).SExtraData, %MARKER_PIECE_CANBEFLIPPED, %MARKER_PIECE_FLAPPED)
                        '********************************************************************************************************
                        FOR LSortIndex = UBOUND(STemp) TO 1 STEP -1
                            IF UMarkerStatus(STemp(LSortIndex)).SHorizontal = LFlipped THEN
                                INCR LLineIndex
                                ARRAY INSERT pieceIndex(LLineIndex), STemp(LSortIndex)
                                ARRAY DELETE STemp(LSortIndex)
                                REDIM PRESERVE STemp(UBOUND(STemp)-1)
                                EXIT FOR
                            END IF
                        NEXT
                        '********************************************************************************************************
                        IF LPieceCount MOD 4 = 1 THEN CopySourceToDestin STemp(), STemp2() _
                            ELSE CopySourceToDestin STemp(), STemp1()
                        ''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''3
                        IF LPieceCount MOD 4 = 1 THEN CopySourceToDestin STemp2(), STemp() _
                            ELSE CopySourceToDestin STemp1(), STemp()

                        LFlipped = IIF(UMarkerExtras(LPieceIndex).SExtraData <> UMarkerExtras(LPieceIndex1).SExtraData, %MARKER_PIECE_FLIPPED, %MARKER_PIECE_FLIPFLAPPED)
                        '********************************************************************************************************
                        FOR LSortIndex = UBOUND(STemp) TO 1 STEP -1
                            IF UMarkerStatus(STemp(LSortIndex)).SHorizontal = LFlipped THEN
                                INCR LLineIndex
                                ARRAY INSERT pieceIndex(LLineIndex), STemp(LSortIndex)
                                ARRAY DELETE STemp(LSortIndex)
                                REDIM PRESERVE STemp(UBOUND(STemp)-1)
                                EXIT FOR
                            END IF
                        NEXT
                        '********************************************************************************************************
                        IF LPieceCount MOD 4 = 1 THEN CopySourceToDestin STemp(), STemp2() _
                            ELSE CopySourceToDestin STemp(), STemp1()
                        ''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''4
                        IF LPieceCount MOD 4 = 0 THEN CopySourceToDestin STemp2(), STemp() _
                            ELSE CopySourceToDestin STemp1(), STemp()
                        '********************************************************************************************************
                        FOR LSortIndex = %Vert_First TO UBOUND(STemp)
                            IF UMarkerStatus(STemp(LSortIndex)).SHorizontal = %MARKER_PIECE_CANBEFLIPPED THEN
                                INCR LLineIndex
                                ARRAY INSERT pieceIndex(LLineIndex), STemp(LSortIndex)
                                ARRAY DELETE STemp(LSortIndex)
                                REDIM PRESERVE STemp(UBOUND(STemp)-1)
                                EXIT FOR
                            END IF
                        NEXT
                        '********************************************************************************************************
                        IF LPieceCount MOD 4 = 0 THEN CopySourceToDestin STemp(), STemp2() _
                            ELSE CopySourceToDestin STemp(), STemp1()
                        ''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
                    ELSE
                        ''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''1
                        IF LPieceCount MOD 4 = 2 THEN CopySourceToDestin STemp2(), STemp() _
                            ELSE CopySourceToDestin STemp1(), STemp()
                        '********************************************************************************************************
                        FOR LSortIndex = %Vert_First TO UBOUND(STemp)
                            IF UMarkerStatus(STemp(LSortIndex)).SHorizontal = %MARKER_PIECE_FLIPFLAPPED THEN
                                INCR LLineIndex
                                ARRAY INSERT pieceIndex(LLineIndex), STemp(LSortIndex)
                                ARRAY DELETE STemp(LSortIndex)
                                REDIM PRESERVE STemp(UBOUND(STemp)-1)
                                EXIT FOR
                            END IF
                        NEXT
                        '********************************************************************************************************
                        IF LPieceCount MOD 4 = 2 THEN CopySourceToDestin STemp(), STemp2() _
                            ELSE CopySourceToDestin STemp(), STemp1()
                        ''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''2
                        IF LPieceCount MOD 4 = 3 THEN CopySourceToDestin STemp2(), STemp() _
                            ELSE CopySourceToDestin STemp1(), STemp()

                        LFlipped = IIF(UMarkerExtras(LPieceIndex).SExtraData <> UMarkerExtras(LPieceIndex1).SExtraData, %MARKER_PIECE_FLAPPED, %MARKER_PIECE_CANBEFLIPPED)
                        '********************************************************************************************************
                        FOR LSortIndex = UBOUND(STemp) TO 1 STEP -1
                            IF UMarkerStatus(STemp(LSortIndex)).SHorizontal = LFlipped THEN
                                INCR LLineIndex
                                ARRAY INSERT pieceIndex(LLineIndex), STemp(LSortIndex)
                                ARRAY DELETE STemp(LSortIndex)
                                REDIM PRESERVE STemp(UBOUND(STemp)-1)
                                EXIT FOR
                            END IF
                        NEXT
                        '********************************************************************************************************
                        IF LPieceCount MOD 4 = 3 THEN CopySourceToDestin STemp(), STemp2() _
                            ELSE CopySourceToDestin STemp(), STemp1()
                        ''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''3
                        IF LPieceCount MOD 4 = 3 THEN CopySourceToDestin STemp2(), STemp() _
                            ELSE CopySourceToDestin STemp1(), STemp()

                        LFlipped = IIF(UMarkerExtras(LPieceIndex).SExtraData <> UMarkerExtras(LPieceIndex1).SExtraData, %MARKER_PIECE_FLIPFLAPPED, %MARKER_PIECE_FLIPPED)
                        '********************************************************************************************************
                        FOR LSortIndex = UBOUND(STemp) TO 1 STEP -1
                            IF UMarkerStatus(STemp(LSortIndex)).SHorizontal = LFlipped THEN
                                INCR LLineIndex
                                ARRAY INSERT pieceIndex(LLineIndex), STemp(LSortIndex)
                                ARRAY DELETE STemp(LSortIndex)
                                REDIM PRESERVE STemp(UBOUND(STemp)-1)
                                EXIT FOR
                            END IF
                        NEXT
                        '********************************************************************************************************
                        IF LPieceCount MOD 4 = 3 THEN CopySourceToDestin STemp(), STemp2() _
                            ELSE CopySourceToDestin STemp(), STemp1()
                        ''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''4
                        IF LPieceCount MOD 4 = 2 THEN CopySourceToDestin STemp2(), STemp() _
                            ELSE CopySourceToDestin STemp1(), STemp()
                        '********************************************************************************************************
                        FOR LSortIndex = %Vert_First TO UBOUND(STemp)
                            IF UMarkerStatus(STemp(LSortIndex)).SHorizontal = %MARKER_PIECE_FLAPPED THEN
                                INCR LLineIndex
                                ARRAY INSERT pieceIndex(LLineIndex), STemp(LSortIndex)
                                ARRAY DELETE STemp(LSortIndex)
                                REDIM PRESERVE STemp(UBOUND(STemp)-1)
                                EXIT FOR
                            END IF
                        NEXT
                        '********************************************************************************************************
                        IF LPieceCount MOD 4 = 2 THEN CopySourceToDestin STemp(), STemp2() _
                            ELSE CopySourceToDestin STemp(), STemp1()
                        ''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
                    END IF
                NEXT

                FOR LSortIndex = 1 TO UBOUND(STemp1)
                    INCR LLineIndex
                    ARRAY INSERT pieceIndex(LLineIndex), STemp1(LSortIndex)
                NEXT

                FOR LSortIndex = 1 TO UBOUND(STemp2)
                    INCR LLineIndex
                    ARRAY INSERT pieceIndex(LLineIndex), STemp2(LSortIndex)
                NEXT

            NEXT LIndex

        END IF

        FOR LIndex = %Vert_First TO UBOUND(LCombiNation)
            '   Get the symmetric direction
            SSymmetric = UMarkerExtras(LCombiNation(LIndex)).SExtraData
            '   Parse thru all the pieces
            FOR LMarkerIndex = %Vert_First TO UBOUND(UMarkerPlacement)
                '   Get the Piece Index from Storage area
                LoadCurveDataSingle UMarkerIndex(), LMarkerIndex, SIndexOfSize, SIndexOfPiece, 0, SIndexOfGarment
                '   Check the piece index
                IF LCombiNation(LIndex) = SIndexOfPiece AND SSymmetric = 1 THEN
                    SELECT CASE UMarkerStatus(LMarkerIndex).SHorizontal
                        CASE %MARKER_PIECE_CANBEFLIPPED   : UMarkerStatus(LMarkerIndex).SHorizontal = %MARKER_PIECE_FLIPPED
                        CASE %MARKER_PIECE_FLIPPED        : UMarkerStatus(LMarkerIndex).SHorizontal = %MARKER_PIECE_CANBEFLIPPED
                        CASE %MARKER_PIECE_FLAPPED        : UMarkerStatus(LMarkerIndex).SHorizontal = %MARKER_PIECE_FLIPFLAPPED
                        CASE %MARKER_PIECE_FLIPFLAPPED    : UMarkerStatus(LMarkerIndex).SHorizontal = %MARKER_PIECE_FLAPPED
                    END SELECT
                ELSEIF LCombiNation(LIndex) = SIndexOfPiece AND SSymmetric = 3 THEN
                    SELECT CASE UMarkerStatus(LMarkerIndex).SHorizontal
                        CASE %MARKER_PIECE_CANBEFLIPPED   : UMarkerStatus(LMarkerIndex).SHorizontal = %MARKER_PIECE_FLIPFLAPPED
                        CASE %MARKER_PIECE_FLIPPED        : UMarkerStatus(LMarkerIndex).SHorizontal = %MARKER_PIECE_FLAPPED
                        CASE %MARKER_PIECE_FLAPPED        : UMarkerStatus(LMarkerIndex).SHorizontal = %MARKER_PIECE_FLIPPED
                        CASE %MARKER_PIECE_FLIPFLAPPED    : UMarkerStatus(LMarkerIndex).SHorizontal = %MARKER_PIECE_CANBEFLIPPED
                    END SELECT
                ELSEIF LCombiNation(LIndex) = SIndexOfPiece AND SSymmetric = 4 THEN
                    SELECT CASE UMarkerStatus(LMarkerIndex).SHorizontal
                        CASE %MARKER_PIECE_CANBEFLIPPED   : UMarkerStatus(LMarkerIndex).SHorizontal = %MARKER_PIECE_FLAPPED
                        CASE %MARKER_PIECE_FLIPPED        : UMarkerStatus(LMarkerIndex).SHorizontal = %MARKER_PIECE_FLIPFLAPPED
                        CASE %MARKER_PIECE_FLAPPED        : UMarkerStatus(LMarkerIndex).SHorizontal = %MARKER_PIECE_CANBEFLIPPED
                        CASE %MARKER_PIECE_FLIPFLAPPED    : UMarkerStatus(LMarkerIndex).SHorizontal = %MARKER_PIECE_FLIPPED
                    END SELECT
                END IF
            NEXT
        NEXT

        ARRAY SCAN pieceIndex(), = 0, TO LPieceIndex
        IF LPieceIndex THEN REDIM pieceIndex(0)
    END IF
    '   Parse thru all pieces
    FOR LIndex = %Vert_First TO UBOUND(pieceIndex)
        '   Add the piece index to storage
        AddCurveData UPieceIndex(), pieceIndex(LIndex), 0, 0, 0
    NEXT

    ERASE LCombiNation(), LPieceArray(), LSizeCombi(), LFlipBased()

END FUNCTION

SUB GetSymmetricForPiece(BYREF UCurve() AS DataBaseDataEnvironmentType, BYREF SSymmetric)
    '   Get the Dimension of the Seam Pattern
    GetPatternDimension UCurve(), SxMin, SyMin, SxMax, SyMax

    'msgbox str$(SxMin)+","+str$(SyMin)+$crlf+STR$(SxMax)+","+STR$(SyMax)

    LoadCurveData UCurve(), %Vert_First, SxP, SyP, 0, 0

    '   Storage area to store the points from seam
    REDIM USeamTopLeft(0) AS DataBaseDataEnvironmentType, USeamTopRight(0) AS DataBaseDataEnvironmentType
    REDIM USeamBtmLeft(0) AS DataBaseDataEnvironmentType, USeamBtmRight(0) AS DataBaseDataEnvironmentType

    IF SxP < 0 AND SyP < 0 THEN
        LTopLeft = 0 : LBtmLeft = 5 : LTopRight = 5 : LBtmRight = 5
    ELSEIF SxP < 0 AND SyP > 0 THEN
        LTopLeft = 5 : LBtmLeft = 5 : LTopRight = 0 : LBtmRight = 5
    ELSEIF SxP > 0 AND SyP > 0 THEN
        LTopLeft = 5 : LBtmLeft = 5 : LTopRight = 5 : LBtmRight = 0
    ELSEIF SxP > 0 AND SyP < 0 THEN
        LTopLeft = 5 : LBtmLeft = 0 : LTopRight = 5 : LBtmRight = 5
    END IF

    '   Parse thru all points
    FOR LLineIndex = 1 TO UBOUND(UCurve)
        LoadCurveData UCurve(), LLineIndex, SxP, SyP, 0, 0

        IF SxP < 0 AND SyP < 0 THEN

            IF LTopLeft = -3 OR LBtmLeft = 0 OR LTopRight = -2 OR LBtmRight = -1 THEN
                Sx1 = USeamBtmLeft(UBOUND(USeamBtmLeft)).SHorizontal : Sy1 = USeamBtmLeft(UBOUND(USeamBtmLeft)).SVertical

                '   Check If the window really overlaps
                IF Maths_CheckIfWindowOverlap(Sx1, Sy1, SxP, SyP, 0, 0, 0, SyMin) THEN
                    '   If the Intersection is possible then check if it lies within the two lines
                    IF Maths_GetLineIntersect(Sx1, Sy1, SxP, SyP, 0, 0, 0, SyMin, Sx3, Sy3) = %True THEN
                        '   Check the points
                        IF Maths_IsPointInLine (Sx1, Sy1, SxP, SyP, Sx3, Sy3) AND Maths_IsPointInLine (0, 0, 0, SyMin, Sx3, Sy3) THEN Sx1 = Sx3 : Sy1 = Sy3
                    END IF
                END IF

                AddCurveData USeamBtmLeft(), 0, Sy1, LLineIndex, 0 : AddCurveData USeamBtmLeft(), 0, Sy1, LLineIndex, 0
                AddCurveData USeamBtmLeft(), 0, 0, LLineIndex, 0

                AddCurveData USeamTopLeft(), 0, 0, LLineIndex, 0
                AddCurveData USeamTopLeft(), 0, Sy1, LLineIndex, 0 : AddCurveData USeamTopLeft(), 0, Sy1, LLineIndex, 0

                IF LTopLeft = -3 THEN LTopLeft = -4
                IF LBtmLeft = 0 THEN LBtmLeft = -1
                IF LTopRight = -2 THEN LTopRight = -3
                IF LBtmRight = -1 THEN LBtmRight = -2
            ELSEIF LTopLeft = 3 OR LBtmLeft = 2 OR LTopRight = 0 OR LBtmRight = 1 THEN
                Sx1 = USeamTopRight(UBOUND(USeamTopRight)).SHorizontal : Sy1 = USeamTopRight(UBOUND(USeamTopRight)).SVertical

                '   Check If the window really overlaps
                IF Maths_CheckIfWindowOverlap(Sx1, Sy1, SxP, SyP, 0, 0, SxMin, 0) THEN
                    '   If the Intersection is possible then check if it lies within the two lines
                    IF Maths_GetLineIntersect(Sx1, Sy1, SxP, SyP, 0, 0, SxMin, 0, Sx3, Sy3) = %True THEN
                        '   Check the points
                        IF Maths_IsPointInLine (Sx1, Sy1, SxP, SyP, Sx3, Sy3) AND Maths_IsPointInLine (0, 0, SxMin, 0, Sx3, Sy3) THEN Sx1 = Sx3 : Sy1 = Sy3
                    END IF
                END IF

                AddCurveData USeamTopRight(), Sx1, 0, LLineIndex, 0 : AddCurveData USeamTopRight(), Sx1, 0, LLineIndex, 0
                AddCurveData USeamTopRight(), 0, 0, LLineIndex, 0

                AddCurveData USeamTopLeft(), 0, 0, LLineIndex, 0
                AddCurveData USeamTopLeft(), Sx1, 0, LLineIndex, 0 : AddCurveData USeamTopLeft(), Sx1, 0, LLineIndex, 0

                IF LTopLeft = 3 THEN LTopLeft = 4
                IF LBtmLeft = 2 THEN LBtmLeft = 3
                IF LTopRight = 0 THEN LTopRight = 1
                IF LBtmRight = 1 THEN LBtmRight = 2
            END IF

            AddCurveData USeamTopLeft(), SxP, SyP, LLineIndex, 0

        ELSEIF SxP < 0 AND SyP > 0 THEN

            IF LTopLeft = 0 OR LBtmLeft = -1 OR LTopRight = -3 OR LBtmRight = -2 THEN
                Sx1 = USeamTopLeft(UBOUND(USeamTopLeft)).SHorizontal : Sy1 = USeamTopLeft(UBOUND(USeamTopLeft)).SVertical

                '   Check If the window really overlaps
                IF Maths_CheckIfWindowOverlap(Sx1, Sy1, SxP, SyP, 0, 0, SxMin, 0) THEN
                    '   If the Intersection is possible then check if it lies within the two lines
                    IF Maths_GetLineIntersect(Sx1, Sy1, SxP, SyP, 0, 0, SxMin, 0, Sx3, Sy3) = %True THEN
                        '   Check the points
                        IF Maths_IsPointInLine (Sx1, Sy1, SxP, SyP, Sx3, Sy3) AND Maths_IsPointInLine (0, 0, SxMin, 0, Sx3, Sy3) THEN Sx1 = Sx3 : Sy1 = Sy3
                    END IF
                END IF

                AddCurveData USeamTopLeft(), Sx1, 0, LLineIndex, 0 : AddCurveData USeamTopLeft(), Sx1, 0, LLineIndex, 0
                AddCurveData USeamTopLeft(), 0, 0, LLineIndex, 0

                AddCurveData USeamTopRight(), 0, 0, LLineIndex, 0
                AddCurveData USeamTopRight(), Sx1, 0, LLineIndex, 0 : AddCurveData USeamTopRight(), Sx1, 0, LLineIndex, 0

                IF LTopLeft = 0 THEN LTopLeft = -1
                IF LBtmLeft = -1 THEN LBtmLeft = -2
                IF LTopRight = -3 THEN LTopRight = -4
                IF LBtmRight = -2 THEN LBtmRight = -3
            ELSEIF LTopLeft = 2 OR LBtmLeft = 1 OR LTopRight = 3 OR LBtmRight = 0 THEN
                Sx1 = USeamBtmRight(UBOUND(USeamBtmRight)).SHorizontal : Sy1 = USeamBtmRight(UBOUND(USeamBtmRight)).SVertical

                '   Check If the window really overlaps
                IF Maths_CheckIfWindowOverlap(Sx1, Sy1, SxP, SyP, 0, 0, 0, SyMax) THEN
                    '   If the Intersection is possible then check if it lies within the two lines
                    IF Maths_GetLineIntersect(Sx1, Sy1, SxP, SyP, 0, 0, 0, SyMax, Sx3, Sy3) = %True THEN
                        '   Check the points
                        IF Maths_IsPointInLine (Sx1, Sy1, SxP, SyP, Sx3, Sy3) AND Maths_IsPointInLine (0, 0, 0, SyMax, Sx3, Sy3) THEN Sx1 = Sx3 : Sy1 = Sy3
                    END IF
                END IF

                AddCurveData USeamBtmRight(), 0, Sy1, LLineIndex, 0 : AddCurveData USeamBtmRight(), 0, Sy1, LLineIndex, 0
                AddCurveData USeamBtmRight(), 0, 0, LLineIndex, 0

                AddCurveData USeamTopRight(), 0, 0, LLineIndex, 0
                AddCurveData USeamTopRight(), 0, Sy1, LLineIndex, 0 : AddCurveData USeamTopRight(), 0, Sy1, LLineIndex, 0

                IF LTopLeft = 2 THEN LTopLeft = 3
                IF LBtmLeft = 1 THEN LBtmLeft = 2
                IF LTopRight = 3 THEN LTopRight = 4
                IF LBtmRight = 0 THEN LBtmRight = 1
            END IF

            AddCurveData USeamTopRight(), SxP, SyP, LLineIndex, 0

        ELSEIF SxP > 0 AND SyP > 0 THEN

            IF LTopLeft = -1 OR LBtmLeft = -2 OR LTopRight = 0 OR LBtmRight = -3 THEN
                Sx1 = USeamTopRight(UBOUND(USeamTopRight)).SHorizontal : Sy1 = USeamTopRight(UBOUND(USeamTopRight)).SVertical

                '   Check If the window really overlaps
                IF Maths_CheckIfWindowOverlap(Sx1, Sy1, SxP, SyP, 0, 0, 0, SyMax) THEN
                    '   If the Intersection is possible then check if it lies within the two lines
                    IF Maths_GetLineIntersect(Sx1, Sy1, SxP, SyP, 0, 0, 0, SyMax, Sx3, Sy3) = %True THEN
                        '   Check the points
                        IF Maths_IsPointInLine (Sx1, Sy1, SxP, SyP, Sx3, Sy3) AND Maths_IsPointInLine (0, 0, 0, SyMax, Sx3, Sy3) THEN Sx1 = Sx3 : Sy1 = Sy3
                    END IF
                END IF

                AddCurveData USeamTopRight(), 0, Sy1, LLineIndex, 0 : AddCurveData USeamTopRight(), 0, Sy1, LLineIndex, 0
                AddCurveData USeamTopRight(), 0, 0, LLineIndex, 0

                AddCurveData USeamBtmRight(), 0, 0, LLineIndex, 0
                AddCurveData USeamBtmRight(), 0, Sy1, LLineIndex, 0 : AddCurveData USeamBtmRight(), 0, Sy1, LLineIndex, 0

                IF LTopLeft = -1 THEN LTopLeft = -2
                IF LBtmLeft = -2 THEN LBtmLeft = -3
                IF LTopRight = 0 THEN LTopRight = -1
                IF LBtmRight = -3 THEN LBtmRight = -4
            ELSEIF LTopLeft = 1 OR LBtmLeft = 0 OR LTopRight = 2 OR LBtmRight = 3 THEN
                Sx1 = USeamBtmLeft(UBOUND(USeamBtmLeft)).SHorizontal : Sy1 = USeamBtmLeft(UBOUND(USeamBtmLeft)).SVertical

                '   Check If the window really overlaps
                IF Maths_CheckIfWindowOverlap(Sx1, Sy1, SxP, SyP, 0, 0, SxMax, 0) THEN
                    '   If the Intersection is possible then check if it lies within the two lines
                    IF Maths_GetLineIntersect(Sx1, Sy1, SxP, SyP, 0, 0, SxMax, 0, Sx3, Sy3) = %True THEN
                        '   Check the points
                        IF Maths_IsPointInLine (Sx1, Sy1, SxP, SyP, Sx3, Sy3) AND Maths_IsPointInLine (0, 0, SxMax, 0, Sx3, Sy3) THEN Sx1 = Sx3 : Sy1 = Sy3
                    END IF
                END IF

                AddCurveData USeamBtmLeft(), Sx1, 0, LLineIndex, 0 : AddCurveData USeamBtmLeft(), Sx1, 0, LLineIndex, 0
                AddCurveData USeamBtmLeft(), 0, 0, LLineIndex, 0

                AddCurveData USeamBtmRight(), 0, 0, LLineIndex, 0
                AddCurveData USeamBtmRight(), Sx1, 0, LLineIndex, 0 : AddCurveData USeamBtmRight(), Sx1, 0, LLineIndex, 0

                IF LTopLeft = 1 THEN LTopLeft = 2
                IF LBtmLeft = 0 THEN LBtmLeft = 1
                IF LTopRight = 2 THEN LTopRight = 3
                IF LBtmRight = 3 THEN LBtmRight = 4
            END IF

            AddCurveData USeamBtmRight(), SxP, SyP, LLineIndex, 0

        ELSEIF SxP > 0 AND SyP < 0 THEN

            IF LTopLeft = -2 OR LBtmLeft = -3 OR LTopRight = -1 OR LBtmRight = 0 THEN
                Sx1 = USeamBtmRight(UBOUND(USeamBtmRight)).SHorizontal : Sy1 = USeamBtmRight(UBOUND(USeamBtmRight)).SVertical

                '   Check If the window really overlaps
                IF Maths_CheckIfWindowOverlap(Sx1, Sy1, SxP, SyP, 0, 0, SxMax, 0) THEN
                    '   If the Intersection is possible then check if it lies within the two lines
                    IF Maths_GetLineIntersect(Sx1, Sy1, SxP, SyP, 0, 0, SxMax, 0, Sx3, Sy3) = %True THEN
                        '   Check the points
                        IF Maths_IsPointInLine (Sx1, Sy1, SxP, SyP, Sx3, Sy3) AND Maths_IsPointInLine (0, 0, SxMax, 0, Sx3, Sy3) THEN Sx1 = Sx3 : Sy1 = Sy3
                    END IF
                END IF

                AddCurveData USeamBtmRight(), Sx1, 0, LLineIndex, 0 : AddCurveData USeamBtmRight(), Sx1, 0, LLineIndex, 0
                AddCurveData USeamBtmRight(), 0, 0, LLineIndex, 0

                AddCurveData USeamBtmLeft(), 0, 0, LLineIndex, 0
                AddCurveData USeamBtmLeft(), Sx1, 0, LLineIndex, 0 : AddCurveData USeamBtmLeft(), Sx1, 0, LLineIndex, 0

                IF LTopLeft = -2 THEN LTopLeft = -3
                IF LBtmLeft = -3 THEN LBtmLeft = -4
                IF LTopRight = -1 THEN LTopRight = -2
                IF LBtmRight = 0 THEN LBtmRight = -1
            ELSEIF LTopLeft = 0 OR LBtmLeft = 3 OR LTopRight = 1 OR LBtmRight = 2 THEN
                Sx1 = USeamTopLeft(UBOUND(USeamTopLeft)).SHorizontal : Sy1 = USeamTopLeft(UBOUND(USeamTopLeft)).SVertical

                '   Check If the window really overlaps
                IF Maths_CheckIfWindowOverlap(Sx1, Sy1, SxP, SyP, 0, 0, 0, SyMin) THEN
                    '   If the Intersection is possible then check if it lies within the two lines
                    IF Maths_GetLineIntersect(Sx1, Sy1, SxP, SyP, 0, 0, 0, SyMin, Sx3, Sy3) = %True THEN
                        '   Check the points
                        IF Maths_IsPointInLine (Sx1, Sy1, SxP, SyP, Sx3, Sy3) AND Maths_IsPointInLine (0, 0, 0, SyMin, Sx3, Sy3) THEN Sx1 = Sx3 : Sy1 = Sy3
                    END IF
                END IF

                AddCurveData USeamTopLeft(), 0, Sy1, LLineIndex, 0 : AddCurveData USeamTopLeft(), 0, Sy1, LLineIndex, 0
                AddCurveData USeamTopLeft(), 0, 0, LLineIndex, 0

                AddCurveData USeamBtmLeft(), 0, 0, LLineIndex, 0
                AddCurveData USeamBtmLeft(), 0, Sy1, LLineIndex, 0 : AddCurveData USeamBtmLeft(), 0, Sy1, LLineIndex, 0

                IF LTopLeft = 0 THEN LTopLeft = 1
                IF LBtmLeft = 3 THEN LBtmLeft = 4
                IF LTopRight = 1 THEN LTopRight = 2
                IF LBtmRight = 2 THEN LBtmRight = 3
            END IF

            AddCurveData USeamBtmLeft(), SxP, SyP, LLineIndex, 0

        END IF
    NEXT

    '   Get the Area of the left side Pattern
    STopLeft = GetPatternArea (USeamTopLeft())
    '   Get the Area of the right side Pattern
    STopRight = GetPatternArea (USeamTopRight())
    '   Get the Area of the left side Pattern
    SBtmRight = GetPatternArea (USeamBtmRight())
    '   Get the Area of the right side Pattern
    SBtmLeft = GetPatternArea (USeamBtmLeft())

'    PrintTheCurveToFile1 "F:\USeamTopLeft.txt", USeamTopLeft()
'    PrintTheCurveToFile1 "F:\USeamTopRight.txt", USeamTopRight()
'    PrintTheCurveToFile1 "F:\USeamBtmRight.txt", USeamBtmRight()
'    PrintTheCurveToFile1 "F:\USeamBtmLeft.txt", USeamBtmLeft()
'    PrintTheCurveToFile1 "F:\UCurve.txt", UCurve()

    STopSide = STopLeft + STopRight : SBtmSide = SBtmRight + SBtmLeft

    IF STopSide < SBtmSide THEN
        'TText = IIF$(STopLeft > STopRight, "Top Left", "Top Right")
        SSymmetric = IIF(STopLeft > STopRight, 2, 4)
    ELSE
        'TText = IIF$(SBtmLeft > SBtmRight, "Bottom Left", "Bottom Right")
        SSymmetric = IIF(SBtmRight > SBtmLeft, 1, 3)
    END IF

    'MSGBOX TText + $CRLF + STR$(SSymmetric)
END SUB

SUB NinenthCombi(BYREF LPieceArray(), BYREF LSizeCombi(), BYREF LCombiNation(), BYVAL LIndex)

    LPieceIndex = LCombiNation(LIndex)
    LPieceIndex1 = LCombiNation(LIndex+1)

    SMaxWidth = 0 : LSize1 = 0 : LSize2 = 0

    FOR LSizeLoop = 1 TO UBOUND(LSizeCombi)

        LSize = LSizeCombi(LSizeLoop)
        FOR LMarkerIndex = %Vert_First TO UBOUND(LPieceArray)
            '   Get the Piece Index from Storage area
            LoadCurveDataSingle UMarkerIndex(), LPieceArray(LMarkerIndex), SIndexOfSize, SIndexOfPiece, 0, SIndexOfGarment
            IF SIndexOfPiece = LPieceIndex THEN
                LFound = LPieceArray(LMarkerIndex)

                SFoundSize = UMarkerIndex(LFound).SHorizontal
                SFoundPiece = UMarkerIndex(LFound).SVertical
                EXIT FOR
            END IF
        NEXT LMarkerIndex

        IF LSize = SFoundSize THEN ITERATE

        LPieceCount = 0 : LLineIndex = 0

        REDIM STemp1(0), STemp2(0), STemp(0)

        FOR LMarkerIndex = %Vert_First TO UBOUND(LPieceArray)
            '   Get the Piece Index from Storage area
            LoadCurveDataSingle UMarkerIndex(), LPieceArray(LMarkerIndex), SIndexOfSize, SIndexOfPiece, 0, SIndexOfGarment
            IF SIndexOfPiece = LPieceIndex THEN
                REDIM PRESERVE STemp1(UBOUND(STemp1)+1)
                STemp1(UBOUND(STemp1)) = LPieceArray(LMarkerIndex)
            ELSEIF SIndexOfPiece = LPieceIndex1 THEN
                REDIM PRESERVE STemp2(UBOUND(STemp2)+1)
                STemp2(UBOUND(STemp2)) = LPieceArray(LMarkerIndex)
            END IF
        NEXT LMarkerIndex

        'LIndex for piece index
        FOR LMarkerIndex = %Vert_First TO UBOUND(STemp1)+UBOUND(STemp2) STEP 4

            INCR LPieceCount : REDIM LWidthArray(0) : LLineIndex = 0
            IF (LPieceCount MOD 4 = 1) OR (LPieceCount MOD 4 = 0) THEN

                ''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''1
                IF LPieceCount MOD 4 = 0 THEN CopySourceToDestin STemp2(), STemp() _
                    ELSE CopySourceToDestin STemp1(), STemp()
                '********************************************************************************************************
                FOR LSortIndex = %Vert_First TO UBOUND(STemp)
                    IF UMarkerStatus(STemp(LSortIndex)).SHorizontal = %MARKER_PIECE_FLIPPED THEN
                        INCR LLineIndex
                        REDIM PRESERVE LWidthArray(LLineIndex) : LWidthArray(LLineIndex) = STemp(LSortIndex)
                        ARRAY DELETE STemp(LSortIndex)
                        REDIM PRESERVE STemp(UBOUND(STemp)-1)
                        EXIT FOR
                    END IF
                NEXT
                '********************************************************************************************************
                IF LPieceCount MOD 4 = 0 THEN CopySourceToDestin STemp(), STemp2() _
                    ELSE CopySourceToDestin STemp(), STemp1()
                ''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''2
                IF LPieceCount MOD 4 = 1 THEN CopySourceToDestin STemp2(), STemp() _
                    ELSE CopySourceToDestin STemp1(), STemp()

                LFlipped = IIF(UMarkerExtras(LPieceIndex).SExtraData <> UMarkerExtras(LPieceIndex1).SExtraData, %MARKER_PIECE_CANBEFLIPPED, %MARKER_PIECE_FLAPPED)
                '********************************************************************************************************
                FOR LSortIndex = UBOUND(STemp) TO 1 STEP -1
                    IF UMarkerStatus(STemp(LSortIndex)).SHorizontal = LFlipped AND UMarkerIndex(STemp(LSortIndex)).SHorizontal = LSize THEN      '%MARKER_PIECE_FLAPPED
                        INCR LLineIndex
                        REDIM PRESERVE LWidthArray(LLineIndex) : LWidthArray(LLineIndex) = STemp(LSortIndex)
                        ARRAY DELETE STemp(LSortIndex)
                        REDIM PRESERVE STemp(UBOUND(STemp)-1)
                        EXIT FOR
                    END IF
                NEXT
                '********************************************************************************************************
                IF LPieceCount MOD 4 = 1 THEN CopySourceToDestin STemp(), STemp2() _
                    ELSE CopySourceToDestin STemp(), STemp1()
                ''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''3
                IF LPieceCount MOD 4 = 1 THEN CopySourceToDestin STemp2(), STemp() _
                    ELSE CopySourceToDestin STemp1(), STemp()

                LFlipped = IIF(UMarkerExtras(LPieceIndex).SExtraData <> UMarkerExtras(LPieceIndex1).SExtraData, %MARKER_PIECE_FLIPPED, (%MARKER_PIECE_FLIPPED XOR %MARKER_PIECE_FLAPPED))
                '********************************************************************************************************
                FOR LSortIndex = UBOUND(STemp) TO 1 STEP -1
                    IF UMarkerStatus(STemp(LSortIndex)).SHorizontal = LFlipped AND UMarkerIndex(STemp(LSortIndex)).SHorizontal = LSize THEN '(%MARKER_PIECE_FLIPPED XOR %MARKER_PIECE_FLAPPED)
                        INCR LLineIndex
                        REDIM PRESERVE LWidthArray(LLineIndex) : LWidthArray(LLineIndex) = STemp(LSortIndex)
                        ARRAY DELETE STemp(LSortIndex)
                        REDIM PRESERVE STemp(UBOUND(STemp)-1)
                        EXIT FOR
                    END IF
                NEXT
                '********************************************************************************************************
                IF LPieceCount MOD 4 = 1 THEN CopySourceToDestin STemp(), STemp2() _
                    ELSE CopySourceToDestin STemp(), STemp1()
                ''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''4
                IF LPieceCount MOD 4 = 0 THEN CopySourceToDestin STemp2(), STemp() _
                    ELSE CopySourceToDestin STemp1(), STemp()
                '********************************************************************************************************
                FOR LSortIndex = %Vert_First TO UBOUND(STemp)
                    IF UMarkerStatus(STemp(LSortIndex)).SHorizontal = %MARKER_PIECE_CANBEFLIPPED THEN
                        INCR LLineIndex
                        REDIM PRESERVE LWidthArray(LLineIndex) : LWidthArray(LLineIndex) = STemp(LSortIndex)
                        ARRAY DELETE STemp(LSortIndex)
                        REDIM PRESERVE STemp(UBOUND(STemp)-1)
                        EXIT FOR
                    END IF
                NEXT
                '********************************************************************************************************
                IF LPieceCount MOD 4 = 0 THEN CopySourceToDestin STemp(), STemp2() _
                    ELSE CopySourceToDestin STemp(), STemp1()
                ''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
            ELSE
                ''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''1
                IF LPieceCount MOD 4 = 2 THEN CopySourceToDestin STemp2(), STemp() _
                    ELSE CopySourceToDestin STemp1(), STemp()
                '********************************************************************************************************
                FOR LSortIndex = %Vert_First TO UBOUND(STemp)
                    IF UMarkerStatus(STemp(LSortIndex)).SHorizontal = (%MARKER_PIECE_FLIPPED XOR %MARKER_PIECE_FLAPPED) THEN
                        INCR LLineIndex
                        REDIM PRESERVE LWidthArray(LLineIndex) : LWidthArray(LLineIndex) = STemp(LSortIndex)
                        ARRAY DELETE STemp(LSortIndex)
                        REDIM PRESERVE STemp(UBOUND(STemp)-1)
                        EXIT FOR
                    END IF
                NEXT
                '********************************************************************************************************
                IF LPieceCount MOD 4 = 2 THEN CopySourceToDestin STemp(), STemp2() _
                    ELSE CopySourceToDestin STemp(), STemp1()
                ''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''2
                IF LPieceCount MOD 4 = 3 THEN CopySourceToDestin STemp2(), STemp() _
                    ELSE CopySourceToDestin STemp1(), STemp()

                LFlipped = IIF(UMarkerExtras(LPieceIndex).SExtraData <> UMarkerExtras(LPieceIndex1).SExtraData, %MARKER_PIECE_FLAPPED, %MARKER_PIECE_CANBEFLIPPED)
                '********************************************************************************************************
                FOR LSortIndex = UBOUND(STemp) TO 1 STEP -1
                    IF UMarkerStatus(STemp(LSortIndex)).SHorizontal = LFlipped AND UMarkerIndex(STemp(LSortIndex)).SHorizontal = LSize THEN  '%MARKER_PIECE_CANBEFLIPPED
                        INCR LLineIndex
                        REDIM PRESERVE LWidthArray(LLineIndex) : LWidthArray(LLineIndex) = STemp(LSortIndex)
                        ARRAY DELETE STemp(LSortIndex)
                        REDIM PRESERVE STemp(UBOUND(STemp)-1)
                        EXIT FOR
                    END IF
                NEXT
                '********************************************************************************************************
                IF LPieceCount MOD 4 = 3 THEN CopySourceToDestin STemp(), STemp2() _
                    ELSE CopySourceToDestin STemp(), STemp1()
                ''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''3
                IF LPieceCount MOD 4 = 3 THEN CopySourceToDestin STemp2(), STemp() _
                    ELSE CopySourceToDestin STemp1(), STemp()

                LFlipped = IIF(UMarkerExtras(LPieceIndex).SExtraData <> UMarkerExtras(LPieceIndex1).SExtraData, (%MARKER_PIECE_FLIPPED XOR %MARKER_PIECE_FLAPPED), %MARKER_PIECE_FLIPPED)
                '********************************************************************************************************
                FOR LSortIndex = UBOUND(STemp) TO 1 STEP -1
                    IF UMarkerStatus(STemp(LSortIndex)).SHorizontal = LFlipped AND UMarkerIndex(STemp(LSortIndex)).SHorizontal = LSize THEN   '%MARKER_PIECE_FLIPPED
                        INCR LLineIndex
                        REDIM PRESERVE LWidthArray(LLineIndex) : LWidthArray(LLineIndex) = STemp(LSortIndex)
                        ARRAY DELETE STemp(LSortIndex)
                        REDIM PRESERVE STemp(UBOUND(STemp)-1)
                        EXIT FOR
                    END IF
                NEXT
                '********************************************************************************************************
                IF LPieceCount MOD 4 = 3 THEN CopySourceToDestin STemp(), STemp2() _
                    ELSE CopySourceToDestin STemp(), STemp1()
                ''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''4
                IF LPieceCount MOD 4 = 2 THEN CopySourceToDestin STemp2(), STemp() _
                    ELSE CopySourceToDestin STemp1(), STemp()
                '********************************************************************************************************
                FOR LSortIndex = %Vert_First TO UBOUND(STemp)
                    IF UMarkerStatus(STemp(LSortIndex)).SHorizontal = %MARKER_PIECE_FLAPPED THEN
                        INCR LLineIndex
                        REDIM PRESERVE LWidthArray(LLineIndex) : LWidthArray(LLineIndex) = STemp(LSortIndex)
                        ARRAY DELETE STemp(LSortIndex)
                        REDIM PRESERVE STemp(UBOUND(STemp)-1)
                        EXIT FOR
                    END IF
                NEXT
                '********************************************************************************************************
                IF LPieceCount MOD 4 = 2 THEN CopySourceToDestin STemp(), STemp2() _
                    ELSE CopySourceToDestin STemp(), STemp1()
                ''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
            END IF

            SPredictWidth = GetMaxWidthOfThePatterns( STR$(LWidthArray(1))+","+STR$(LWidthArray(2))+","+STR$(LWidthArray(3))+","+STR$(LWidthArray(4)), 1)

            SFabWidth = SMarkerEnviron(%SINGLE_MARKER_FABRICWIDTH) - SMarkerEnviron(%SINGLE_MARKER_FABRICRIGHTALLOWANCE)

            IF SMaxWidth < SPredictWidth AND SPredictWidth <= SFabWidth THEN SMaxWidth = SPredictWidth : LSize1 = SFoundSize : LSize2 = LSize

            EXIT FOR
        NEXT
    NEXT

    REDIM STemp1(0), STemp2(0)

    FOR LMarkerIndex = UBOUND(LPieceArray) TO %Vert_First STEP -1
        '   Get the Piece Index from Storage area
        LoadCurveDataSingle UMarkerIndex(), LPieceArray(LMarkerIndex), SIndexOfSize, SIndexOfPiece, 0, SIndexOfGarment
        IF SIndexOfPiece = LPieceIndex THEN
            REDIM PRESERVE STemp1(UBOUND(STemp1)+1)
            STemp1(UBOUND(STemp1)) = LPieceArray(LMarkerIndex)

            ARRAY DELETE LPieceArray(LMarkerIndex)

            REDIM PRESERVE LPieceArray(UBOUND(LPieceArray)-1)

        ELSEIF SIndexOfPiece = LPieceIndex1 THEN
            REDIM PRESERVE STemp2(UBOUND(STemp2)+1)
            STemp2(UBOUND(STemp2)) = LPieceArray(LMarkerIndex)

            ARRAY DELETE LPieceArray(LMarkerIndex)

            REDIM PRESERVE LPieceArray(UBOUND(LPieceArray)-1)
        END IF
    NEXT LMarkerIndex

    FOR LMarkerIndex = %Vert_First TO UBOUND(STemp1)
        '   Get the Piece Index from Storage area
        LoadCurveDataSingle UMarkerIndex(), STemp1(LMarkerIndex), SIndexOfSize, SIndexOfPiece, 0, SIndexOfGarment
        IF SIndexOfSize = LSize1 THEN
            REDIM PRESERVE LPieceArray(UBOUND(LPieceArray)+1)
            LPieceArray(UBOUND(LPieceArray)) = STemp1(LMarkerIndex)
        END IF
    NEXT

    FOR LMarkerIndex = %Vert_First TO UBOUND(STemp1)
        '   Get the Piece Index from Storage area
        LoadCurveDataSingle UMarkerIndex(), STemp1(LMarkerIndex), SIndexOfSize, SIndexOfPiece, 0, SIndexOfGarment
        IF SIndexOfSize <> LSize1 AND SIndexOfSize <> LSize2 THEN
            REDIM PRESERVE LPieceArray(UBOUND(LPieceArray)+1)
            LPieceArray(UBOUND(LPieceArray)) = STemp1(LMarkerIndex)
        END IF
    NEXT

    FOR LMarkerIndex = %Vert_First TO UBOUND(STemp1)
        '   Get the Piece Index from Storage area
        LoadCurveDataSingle UMarkerIndex(), STemp1(LMarkerIndex), SIndexOfSize, SIndexOfPiece, 0, SIndexOfGarment
        IF SIndexOfSize = LSize2 THEN
            REDIM PRESERVE LPieceArray(UBOUND(LPieceArray)+1)
            LPieceArray(UBOUND(LPieceArray)) = STemp1(LMarkerIndex)
        END IF
    NEXT

    FOR LMarkerIndex = %Vert_First TO UBOUND(STemp2)
        '   Get the Piece Index from Storage area
        LoadCurveDataSingle UMarkerIndex(), STemp2(LMarkerIndex), SIndexOfSize, SIndexOfPiece, 0, SIndexOfGarment
        IF SIndexOfSize = LSize1 THEN
            REDIM PRESERVE LPieceArray(UBOUND(LPieceArray)+1)
            LPieceArray(UBOUND(LPieceArray)) = STemp2(LMarkerIndex)
        END IF
    NEXT

    FOR LMarkerIndex = %Vert_First TO UBOUND(STemp2)
        '   Get the Piece Index from Storage area
        LoadCurveDataSingle UMarkerIndex(), STemp2(LMarkerIndex), SIndexOfSize, SIndexOfPiece, 0, SIndexOfGarment
        IF SIndexOfSize <> LSize1 AND SIndexOfSize <> LSize2 THEN
            REDIM PRESERVE LPieceArray(UBOUND(LPieceArray)+1)
            LPieceArray(UBOUND(LPieceArray)) = STemp2(LMarkerIndex)
        END IF
    NEXT

    FOR LMarkerIndex = %Vert_First TO UBOUND(STemp2)
        '   Get the Piece Index from Storage area
        LoadCurveDataSingle UMarkerIndex(), STemp2(LMarkerIndex), SIndexOfSize, SIndexOfPiece, 0, SIndexOfGarment
        IF SIndexOfSize = LSize2 THEN
            REDIM PRESERVE LPieceArray(UBOUND(LPieceArray)+1)
            LPieceArray(UBOUND(LPieceArray)) = STemp2(LMarkerIndex)
        END IF
    NEXT

    'MSGBOX STR$(SMaxWidth)+","+STR$(LSize1)+","+STR$(LSize2)
END SUB

SUB CopySourceToDestin(BYREF SSource(), BYREF SSDestin())
    '   Redim the storage
    REDIM SSDestin(UBOUND(SSource))
    '   Parse thru all values
    FOR LSourceIndex = %Vert_First TO UBOUND(SSource)
        '   Copy source to Destin
        SSDestin(LSourceIndex) = SSource(LSourceIndex)
    NEXT
END SUB

FUNCTION GetMaxWidthOfThePatterns(BYVAL TPieceIndexes, BYVAL LSymStatus) AS SINGLE
    '   Storage for piece indexes
    REDIM TExtractIndex(1 TO PARSECOUNT(TPieceIndexes))
    '   Extract pice index string to array
    PARSE TPieceIndexes, TExtractIndex()
    '   Define the default width & others
    SMaxWidth = 0 : SXCenter = 0 : SYCenter = 0
    '   Get the bump tolerance
    SBumpTolerance = SModule(%SINGLE_MODULE_BUMP_TOLERANCE)
    '   Storage for all piece top points
    REDIM UPrevTop(%Vert_Zero) AS DataBaseDataEnvironmentType
    '   Parse thru all pieces
    FOR LPieceIndex = 1 TO UBOUND(TExtractIndex)
        '   Get the piece index
        LMarkerIndex = VAL(TExtractIndex(LPieceIndex))
        '   Get the Piece Index from Storage area
        LoadCurveDataSingle UMarkerIndex(), LMarkerIndex, SIndexOfSize, SIndexOfPiece, 0, SIndexOfGarment
        '   Get the Piece Flags from storage area
        LoadCurveDataSingle UMarkerStatus(), LMarkerIndex, SPieceFlipped, SPieceBlocked, LPieceFolded, SPiecePaired
        '   Get the placement of the piece
        LoadCurveDataSingle UMarkerPlacement(), LMarkerIndex, 0, 0, LPieceOrientation, SRotationAngle
        '   Load this piece from the Data File
        LoadSizeFromStorageAreaWithoutEnviron SIndexOfPiece, SIndexOfSize
        '   Define the Bias of the piece
        SRotationAngle = ABS(SRotationAngle)
        '   Check with angle flag for(+ or -)
        SRotationAngle *= IIF(UMarkerOthers(LMarkerIndex).LDataType = 0, 1, UMarkerOthers(LMarkerIndex).LDataType)
        '   Check the flip and rotate
        IF SRotationAngle <> 0 AND (UMarkerStatus(LMarkerIndex).SHorizontal = %MARKER_PIECE_CANBEFLIPPED OR UMarkerStatus(LMarkerIndex).SHorizontal = (%MARKER_PIECE_FLIPPED XOR %MARKER_PIECE_FLAPPED)) THEN
            UMarkerPlacement(LMarkerIndex).SExtraData = -SRotationAngle
        END IF
        '   If angle is negative then store negative flag else positive flag
        UMarkerOthers(LMarkerIndex).LDataType = IIF(SRotationAngle < 0, -1, 1)
        '   Get the symmetric direction
        SSymmetric = UMarkerExtras(SIndexOfPiece).SExtraData
        '   Skip if not
        IF SSymmetric = 1 AND LSymStatus = 1 THEN
            '   Check the flip of the piece & convert the piece
            SELECT CASE SPieceFlipped
                CASE %MARKER_PIECE_CANBEFLIPPED : UMarkerStatus(LMarkerIndex).SHorizontal = %MARKER_PIECE_FLIPPED
                CASE %MARKER_PIECE_FLIPPED      : UMarkerStatus(LMarkerIndex).SHorizontal = %MARKER_PIECE_CANBEFLIPPED
                CASE %MARKER_PIECE_FLAPPED      : UMarkerStatus(LMarkerIndex).SHorizontal = %MARKER_PIECE_FLIPFLAPPED
                CASE %MARKER_PIECE_FLIPFLAPPED  : UMarkerStatus(LMarkerIndex).SHorizontal = %MARKER_PIECE_FLAPPED
            END SELECT
            '   Replace the flip in variable
            SPieceFlipped = UMarkerStatus(LMarkerIndex).SHorizontal
        ELSEIF SSymmetric = 3 AND LSymStatus = 1 THEN
            '   Check the flip of the piece & convert the piece
            SELECT CASE SPieceFlipped
                CASE %MARKER_PIECE_CANBEFLIPPED : UMarkerStatus(LMarkerIndex).SHorizontal = %MARKER_PIECE_FLIPFLAPPED
                CASE %MARKER_PIECE_FLIPPED      : UMarkerStatus(LMarkerIndex).SHorizontal = %MARKER_PIECE_FLAPPED
                CASE %MARKER_PIECE_FLAPPED      : UMarkerStatus(LMarkerIndex).SHorizontal = %MARKER_PIECE_FLIPPED
                CASE %MARKER_PIECE_FLIPFLAPPED  : UMarkerStatus(LMarkerIndex).SHorizontal = %MARKER_PIECE_CANBEFLIPPED
            END SELECT
            '   Replace the flip in variable
            SPieceFlipped = UMarkerStatus(LMarkerIndex).SHorizontal
        ELSEIF SSymmetric = 4 AND LSymStatus = 1 THEN
            SELECT CASE SPieceFlipped
                CASE %MARKER_PIECE_CANBEFLIPPED : UMarkerStatus(LMarkerIndex).SHorizontal = %MARKER_PIECE_FLAPPED
                CASE %MARKER_PIECE_FLIPPED      : UMarkerStatus(LMarkerIndex).SHorizontal = %MARKER_PIECE_FLIPFLAPPED
                CASE %MARKER_PIECE_FLAPPED      : UMarkerStatus(LMarkerIndex).SHorizontal = %MARKER_PIECE_CANBEFLIPPED
                CASE %MARKER_PIECE_FLIPFLAPPED  : UMarkerStatus(LMarkerIndex).SHorizontal = %MARKER_PIECE_FLIPPED
            END SELECT
            '   Replace the flip in variable
            SPieceFlipped = UMarkerStatus(LMarkerIndex).SHorizontal
        END IF
        '   Redefine the Dimensions & Area of the Piece
        SaveTheDimensionOfThePiece LMarkerIndex
        '   Get the Tubular Flags from Storage area
        LoadCurveDataSingle UMarkerExtras(), SIndexOfPiece, STubularTop, STubularBottom, LLayPercentage, 0
        '   Get the Center of the Pattern
        SSeamXCen = UMarkerDimension(LMarkerIndex).SHorizontal / 2 : SSeamYCen = UMarkerDimension(LMarkerIndex).SVertical / 2
        '   Calculate the center point
        SPieceXStart = SMaxWidth + SSeamXCen : SPieceYStart = SSeamYCen
        '   Add the piece width to maxwidth
        SMaxWidth += UMarkerDimension(LMarkerIndex).SHorizontal
        '   Get the placement of the piece
        LoadCurveDataSingle UMarkerPlacement(), LMarkerIndex, 0, 0, LPieceOrientation, SRotationAngle
        '   Set the placement of the piece
        SaveCurveDataSingle UMarkerPlacement(), LMarkerIndex, SPieceXStart, SPieceYStart, LPieceOrientation, SRotationAngle
        '   Transform our Pattern to the new location
        MakeSecondaryTransformationExt USeamPattern(), SPieceXStart, SPieceYStart
        '   Get the dimension of the curve
        GetPatternDimension USeamPattern(), SxMin, SyMin, SxMax, SyMax
        '   Storage for max and min for the dimensions
        REDIM UDimensions(%Vert_Zero) AS DataBaseDataEnvironmentType
        '   Get the max and min for the dimensions
        GetPatternDimensionExt USeamPattern(), SxMin, SyMin, SxMax, SyMax, UDimensions()
        '   Get the piece informations
        GetPieceCurveForPiece LMarkerIndex
        '   Storage for all side points
        REDIM UTop(%Vert_Zero) AS DataBaseDataEnvironmentType  : REDIM UBtm(%Vert_Zero) AS DataBaseDataEnvironmentType
        REDIM ULeft(%Vert_Zero) AS DataBaseDataEnvironmentType : REDIM URight(%Vert_Zero) AS DataBaseDataEnvironmentType
        '   Get each side points
        GetAllSidePoints USeamPattern(), UDimensions(), UTop(), UBtm(), ULeft(), URight(), UMarkerStatus(LMarkerIndex).SHorizontal
        '   Define the defaults
        SXDistance = 1E10
        '   Get the down moving distance
        FindMovingDistanceY UBtm(), UPrevTop(), 0, SMarkerEnviron(%SINGLE_MARKER_FABRICWIDTH), SXDistance
        '   Deviate the distance
        SXDistance = IIF(SXDistance = 1E10, 0, SXDistance)
        SXDistance = IIF(SXDistance > SBumpTolerance, SXDistance - SBumpTolerance, 0)   '0.5
        '   Reduce the bump width in max width
        SMaxWidth = (SMaxWidth - SXDistance) + 1

'        PrintTheCurveToFile "F:\UBtm.txt", UBtm()
'        MSGBOX STR$(LMarkerIndex)+","+STR$(SXDistance)+"------"+STR$(SMaxWidth)+$CRLF+STR$(UMarkerStatus(LMarkerIndex).SHorizontal)

        FOR LAllPoints = 1 TO UBOUND(UTop)
            UTop(LAllPoints).SHorizontal -= SXDistance
        NEXT
        '   Storage for top points of the pattern
        REDIM UPrevTop(%Vert_Zero) AS DataBaseDataEnvironmentType
        '   Store the points in storage
        AddSourceCurveToDestin UTop(), UPrevTop()
        '   Skip if not
        IF SSymmetric = 1 AND LSymStatus = 1 THEN
            '   Check the flip of the piece & convert the piece
            SELECT CASE UMarkerStatus(LMarkerIndex).SHorizontal
                CASE %MARKER_PIECE_CANBEFLIPPED : UMarkerStatus(LMarkerIndex).SHorizontal = %MARKER_PIECE_FLIPPED
                CASE %MARKER_PIECE_FLIPPED      : UMarkerStatus(LMarkerIndex).SHorizontal = %MARKER_PIECE_CANBEFLIPPED
                CASE %MARKER_PIECE_FLAPPED      : UMarkerStatus(LMarkerIndex).SHorizontal = %MARKER_PIECE_FLIPFLAPPED
                CASE %MARKER_PIECE_FLIPFLAPPED  : UMarkerStatus(LMarkerIndex).SHorizontal = %MARKER_PIECE_FLAPPED
            END SELECT
        ELSEIF SSymmetric = 3 AND LSymStatus = 1 THEN
            '   Check the flip of the piece & convert the piece
            SELECT CASE SPieceFlipped
                CASE %MARKER_PIECE_CANBEFLIPPED : UMarkerStatus(LMarkerIndex).SHorizontal = %MARKER_PIECE_FLIPFLAPPED
                CASE %MARKER_PIECE_FLIPPED      : UMarkerStatus(LMarkerIndex).SHorizontal = %MARKER_PIECE_FLAPPED
                CASE %MARKER_PIECE_FLAPPED      : UMarkerStatus(LMarkerIndex).SHorizontal = %MARKER_PIECE_FLIPPED
                CASE %MARKER_PIECE_FLIPFLAPPED  : UMarkerStatus(LMarkerIndex).SHorizontal = %MARKER_PIECE_CANBEFLIPPED
            END SELECT
        ELSEIF SSymmetric = 4 AND LSymStatus = 1 THEN
            SELECT CASE SPieceFlipped
                CASE %MARKER_PIECE_CANBEFLIPPED : UMarkerStatus(LMarkerIndex).SHorizontal = %MARKER_PIECE_FLAPPED
                CASE %MARKER_PIECE_FLIPPED      : UMarkerStatus(LMarkerIndex).SHorizontal = %MARKER_PIECE_FLIPFLAPPED
                CASE %MARKER_PIECE_FLAPPED      : UMarkerStatus(LMarkerIndex).SHorizontal = %MARKER_PIECE_CANBEFLIPPED
                CASE %MARKER_PIECE_FLIPFLAPPED  : UMarkerStatus(LMarkerIndex).SHorizontal = %MARKER_PIECE_FLIPPED
            END SELECT
        END IF
    NEXT
    '   Return the width
    FUNCTION = IIF(SMaxWidth <> 0, SMaxWidth - 1, SMaxWidth)
END FUNCTION

'   Apply bump for the piece
SUB ApplyBumpForThePiece(BYVAL LIndex, BYVAL LoopStep, BYVAL LTubFlag, BYVAL SCombiLoop)
    '   Storage for all side points
    REDIM UTop(%Vert_Zero) AS DataBaseDataEnvironmentType, UBtm(%Vert_Zero) AS DataBaseDataEnvironmentType
    REDIM ULeft(%Vert_Zero) AS DataBaseDataEnvironmentType , URight(%Vert_Zero) AS DataBaseDataEnvironmentType

    LBumpPos = IIF(LoopStep = 1, %COMMAND_TOOL_BUMP_BOTTOM, %COMMAND_TOOL_BUMP_TOP)

    IF LMarkerEnviron(%LONG_MARKER_MARKERCOMBINATION) THEN
        SELECT CASE SCombiLoop
            CASE 5,8,9
                IF LTubFlag <> 3 THEN CheckAndMoveThePiece LIndex, LoopStep, LBumpPos, UTop(), UBtm(), ULeft(), URight()                    '   Down/Top
                CheckAndMoveThePiece LIndex, LoopStep, %COMMAND_TOOL_BUMP_LEFT, UTop(), UBtm(), ULeft(), URight()                           '   Left
                IF LTubFlag <> 3 THEN CheckAndMoveThePiece LIndex, LoopStep, LBumpPos, UTop(), UBtm(), ULeft(), URight()                    '   Down/Top
            CASE 1,2,3,4,7
                CheckAndMoveThePiece LIndex, LoopStep, %COMMAND_TOOL_BUMP_LEFT, UTop(), UBtm(), ULeft(), URight()                           '   Left
                IF LTubFlag <> 3 THEN CheckAndMoveThePiece LIndex, LoopStep, LBumpPos, UTop(), UBtm(), ULeft(), URight()                    '   Down/Top
            CASE 6,9
                IF LTubFlag <> 3 THEN CheckAndMoveThePiece LIndex, LoopStep, LBumpPos, UTop(), UBtm(), ULeft(), URight()                    '   Down/Top
                IF LTubFlag <> 3 THEN CheckAndMoveThePiece LIndex, LoopStep, LBumpPos, UTop(), UBtm(), ULeft(), URight()                    '   Down/Top
        END SELECT
    ELSE
        SELECT CASE SCombiLoop
            CASE 1,2,5,6,9,10,13,14,17,18,22,23,26,27,30,31,34,35,38,39,42
                IF LTubFlag <> 3 THEN CheckAndMoveThePiece LIndex, LoopStep, LBumpPos, UTop(), UBtm(), ULeft(), URight()                    '   Down/Top
                CheckAndMoveThePiece LIndex, LoopStep, %COMMAND_TOOL_BUMP_LEFT, UTop(), UBtm(), ULeft(), URight()                           '   Left
                IF LTubFlag <> 3 THEN CheckAndMoveThePiece LIndex, LoopStep, LBumpPos, UTop(), UBtm(), ULeft(), URight()                    '   Down/Top
            CASE 3,4,7,8,11,12,15,16,19,20,21,24,25,28,29,32,33,36,37,40,41
                CheckAndMoveThePiece LIndex, LoopStep, %COMMAND_TOOL_BUMP_LEFT, UTop(), UBtm(), ULeft(), URight()                           '   Left
                IF LTubFlag <> 3 THEN CheckAndMoveThePiece LIndex, LoopStep, LBumpPos, UTop(), UBtm(), ULeft(), URight()                    '   Down/Top
            CASE 43
                IF LTubFlag <> 3 THEN CheckAndMoveThePiece LIndex, LoopStep, LBumpPos, UTop(), UBtm(), ULeft(), URight()                    '   Down/Top
                IF LTubFlag <> 3 THEN CheckAndMoveThePiece LIndex, LoopStep, LBumpPos, UTop(), UBtm(), ULeft(), URight()                    '   Down/Top
            CASE 44
                LoopStep = 1
                IF LTubFlag <> 3 THEN CheckAndMoveThePiece LIndex, LoopStep, %COMMAND_TOOL_BUMP_BOTTOM, UTop(), UBtm(), ULeft(), URight()   '   Down
                'CheckAndMoveThePiece LIndex, LoopStep, %COMMAND_TOOL_BUMP_LEFT, UTop(), UBtm(), ULeft(), URight()                           '   Left
                'IF LTubFlag <> 3 THEN CheckAndMoveThePiece LIndex, LoopStep, %COMMAND_TOOL_BUMP_BOTTOM, UTop(), UBtm(), ULeft(), URight()   '   Down
            CASE 100
                WHILE %True

                    WHILE %True
                        '   Get the placement of the piece
                        LoadCurveDataSingle UMarkerPlacement(), LIndex, SHorizontalBefore, SVerticalBefore, 0, 0
                        '   Storage for all side points
                        REDIM UTop(%Vert_Zero) AS DataBaseDataEnvironmentType, UBtm(%Vert_Zero) AS DataBaseDataEnvironmentType
                        REDIM ULeft(%Vert_Zero) AS DataBaseDataEnvironmentType , URight(%Vert_Zero) AS DataBaseDataEnvironmentType
                        '   Bump the piece to down
                        CheckAndMoveThePiece LIndex, LoopStep, LBumpPos, UTop(), UBtm(), ULeft(), URight()                         '   down
                        '   Bump the piece to left
                        CheckAndMoveThePiece LIndex, LoopStep, %COMMAND_TOOL_BUMP_LEFT, UTop(), UBtm(), ULeft(), URight()                           '   Left
                        '   Get the placement of the piece
                        LoadCurveDataSingle UMarkerPlacement(), LIndex, SHorizontalAfter, SVerticalAfter, 0, 0
                        '   Check if same place then exit
                        IF SHorizontalBefore = SHorizontalAfter AND SVerticalBefore = SVerticalAfter THEN EXIT
                    WEND

                    WHILE %True
                        '   Get the placement of the piece
                        LoadCurveDataSingle UMarkerPlacement(), LIndex, SHorizontalBefore, SVerticalBefore, 0, 0
                        '   Storage for all side points
                        REDIM UTop(%Vert_Zero) AS DataBaseDataEnvironmentType, UBtm(%Vert_Zero) AS DataBaseDataEnvironmentType
                        REDIM ULeft(%Vert_Zero) AS DataBaseDataEnvironmentType , URight(%Vert_Zero) AS DataBaseDataEnvironmentType
                        '   Bump the piece to top
                        CheckAndMoveThePiece LIndex, LoopStep, LBumpPos, UTop(), UBtm(), ULeft(), URight()                            '   top
                        '   Bump the piece to left
                        CheckAndMoveThePiece LIndex, LoopStep, %COMMAND_TOOL_BUMP_LEFT, UTop(), UBtm(), ULeft(), URight()                           '   Left
                        '   Get the placement of the piece
                        LoadCurveDataSingle UMarkerPlacement(), LIndex, SHorizontalAfter1, SVerticalAfter1, 0, 0
                        '   Check if same place then exit
                        IF SHorizontalBefore = SHorizontalAfter1 AND SVerticalBefore = SVerticalAfter1 THEN EXIT
                    WEND

                    WHILE %True
                        '   Get the placement of the piece
                        LoadCurveDataSingle UMarkerPlacement(), LIndex, SHorizontalBefore, SVerticalBefore, 0, 0
                        '   Bump the piece to down
                        CheckAndMoveThePiece LIndex, LoopStep, LBumpPos, UTop(), UBtm(), ULeft(), URight()                         '   down
                        '   Bump the piece to left
                        CheckAndMoveThePiece LIndex, LoopStep, %COMMAND_TOOL_BUMP_LEFT, UTop(), UBtm(), ULeft(), URight()                           '   Left
                        '   Get the placement of the piece
                        LoadCurveDataSingle UMarkerPlacement(), LIndex, SHorizontalAfter2, SVerticalAfter2, 0, 0
                        '   Check if same place then exit
                        IF SHorizontalBefore = SHorizontalAfter2 AND SVerticalBefore = SVerticalAfter2 THEN EXIT
                    WEND

                    IF SHorizontalAfter1 = SHorizontalAfter2 AND SVerticalAfter1 = SVerticalAfter2 THEN EXIT

                    IF SHorizontalAfter = SHorizontalAfter2 AND SVerticalAfter = SVerticalAfter2 THEN EXIT

                WEND
        END SELECT
    END IF
    '   Store the piece left and right side for ease bump
    StoreThePointsBackToStorageX ULeft(), URight(), LIndex
    '   Store the piece top and bottom side for ease bump
    StoreThePointsBackToStorageY UTop(), UBtm(), LIndex
    '   Refresh the Placed and the Hanging List
    'LModule(%LONG_MODULE_REDRAW) = %COMMAND_REDRAW_MARKER_LASTPLACED
    '   Make the actual redraw
    'UpdateTheModule %False
    '   Get the Placed Piece Information
    'ComputeMarkerInformation
END SUB

SUB CheckAndMoveThePiece (BYVAL LIndex, BYVAL LoopStep, BYVAL LBumpTool, BYREF UTop() AS DataBaseDataEnvironmentType, BYREF UBtm() AS DataBaseDataEnvironmentType, _
    BYREF ULeft() AS DataBaseDataEnvironmentType, BYREF URight() AS DataBaseDataEnvironmentType)
    '   Get the Pattern Curve for the Pattern to Bump
    GetPieceCurveForPiece LIndex
    '   Get the dimension of the curve
    GetPatternDimension USeamPattern(), SxMin, SyMin, SxMax, SyMax
    '   Storage for max and min for the dimensions
    REDIM UDimensions(%Vert_Zero) AS DataBaseDataEnvironmentType
    '   Get the max and min for the dimensions
    GetPatternDimensionExt USeamPattern(), SxMin, SyMin, SxMax, SyMax, UDimensions()
    '   Storage for all side points
    REDIM UTop(%Vert_Zero) AS DataBaseDataEnvironmentType  : REDIM UBtm(%Vert_Zero) AS DataBaseDataEnvironmentType
    REDIM ULeft(%Vert_Zero) AS DataBaseDataEnvironmentType : REDIM URight(%Vert_Zero) AS DataBaseDataEnvironmentType
    '   Get each side points
    GetAllSidePoints USeamPattern(), UDimensions(), UTop(), UBtm(), ULeft(), URight(), UMarkerStatus(LIndex).SHorizontal
    '   Bump the piece
    IF LBumpTool = %COMMAND_TOOL_BUMP_LEFT THEN
        '   Find distance of left side to move piece
        GetPointDistOnLineX UTop(), UBtm(), ULeft(), URight(), LoopStep, LIndex, SxMin, SyMin, SxMax, SyMax
    ELSEIF LBumpTool = %COMMAND_TOOL_BUMP_TOP OR LBumpTool = %COMMAND_TOOL_BUMP_BOTTOM THEN
        '   Find distance of top/btm side to move piece
        GetPointDistOnLineY UTop(), UBtm(), ULeft(), URight(), LoopStep, LIndex, SxMin, SyMin, SxMax, SyMax
    END IF
END SUB
