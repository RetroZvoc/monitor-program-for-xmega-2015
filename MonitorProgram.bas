'Monitor Program for XMEGA 2015
'Copyright (c) 2016, Zvonimir Martiniæ
'All rights reserved.
'
'Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met:
'
'1. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer.
'
'2. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution.
'
'THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

'CONFIGURATION
$regfile = "xm128a1def.dat" ' ATXMEGA128A1
$Crystal=32000000 '32MHz crystal
$hwstack=4160 ' This is because I put SPH to $2F instead of $3F. In reality, this is 64 bytes
$swstack=64 ' 32 bytes after hardware stack
$framesize=64 ' Frame is at the beginning of the RAM
$baud = 115200 ' USART baudrate
$noramclear ' Don't clear RAM!
$LIB "hexval.lbx" ' Include hex to val conversion library
'Config Serialin = Buffered , Size = 80 ' USART input buffer
'Config Serialout = Buffered , Size = 254 ' USART output buffer
'Enable interrupts ' Enable interrupts for USART buffer
SPH=$2F ' Set stack pointer at 0x2FFF

'DECLARATIONS
Declare Sub UsartInput(byval Prompt As String, byref Target As String, byval MaxChars As Integer)
Declare Sub TokenizeInput()
Declare Sub FindStringInData(byref Addr As Word, byref Src As String, byref Index As Integer)
Declare Sub PrintFromFlash(byref Addr As Word)
Declare Sub SeekFromFlash(byref Addr As Word, byval Index As Word)
Declare Sub ReadStringSafe(byref Addr As Word, byref Dest As String)
Declare Sub ScanAddress(byval AddrString As String, byref AddrLong As Long, byref Result As Byte)
Dim UsartCommand As String * 79
Dim InputElements(16) As String * 15
Dim InputtedElements As Integer
Const InputElemLen=15
Const InputElems=16
Dim TotalCmds As Integer
Dim LastDumpFlag As Byte
Dim LastDumpStartAddr As Long
Dim LastDumpEndAddr As Long
Dim LastDumpSrc As Byte
Dim DumpedBytes(16) As Byte
Const DumpFromUndefined=0
Const DumpFromProg=1
Const DumpFromData=2
Const ScanAddress_Undefined = 0
Const ScanAddress_Okay = 1
Const ScanAddress_AddressTooBig = 2
Const ScanAddress_SyntaxError = 3
Const ScanAddress_NoAddress = 4
Dim TmpGlobalByte As Byte
Dim I As Integer
Dim J As Integer
Dim Q As Word
Dim W As Iram Word At 8 Overlay ' RESTORE pointer from BASCOM (contained in registers)
Dim Z As Iram Word At 30 Overlay
Dim QH As Word

'MP FUNCTIONS
Declare Sub MP_cmd()
Declare Sub MP_dump()
Declare Sub MP_insert()
Declare Sub MP_cls()
Declare Sub MP_reset()
Declare Sub MP_io()

'INIT
TotalCmds=-1
Q=LoadLabel(MainBinders_Commands)
Do
   Call ReadStringSafe(Q,UsartCommand)
   Incr TotalCmds
Loop Until UsartCommand=""
LastDumpFlag=0
LastDumpStartAddr=0
LastDumpEndAddr=0
LastDumpSrc=0
'MP MAIN PART
Print Chr(12); ' Clear screen
Print "Monitor Program for XMEGA 2015"
Print "Copyright (c) 2016, Zvonimir Martiniæ"
Print "All rights reserved."
Print "This software is licensed with the 2-Clause BSD License."
Print ""
Print "For list of commands, type ";Chr(34);"cmd 1";Chr(34);"."
Print ""
'MAIN LOOP
Do
Call UsartInput(">",UsartCommand,79)
Call TokenizeInput()
Q=LoadLabel(MainBinders_Commands)
Call FindStringInData(Q,InputElements(1),I)
If I>0 Then
   Z=LoadLabel(CommandVectors)
   Q=I-1
   Q=Q*4
   Z=Z+Q
   Z=Z/2
   $asm
   ICALL '(Z+(I-1)*4)/2
   $end asm
Else
   Print "No such command as ";Chr(34);InputElements(1);Chr(34);". To see what commands exist, type ";Chr(34);"cmd 1";Chr(34);"."
End if

Loop

End


Sub UsartInput(byval Prompt As String, byref Target As String, byval MaxChars As Integer)
   Local a As Byte
   Local whiteSpace As Byte
   Local index As Integer
   Local ii As Integer
   Local fitting As Integer
   if MaxChars>79 then
      MaxChars=79
   end if
   fitting=MaxChars+len(Prompt)
   if fitting>79 then
      fitting=79-len(Prompt)
   else
      fitting=MaxChars
   end if
   UsartInput_inputAgain:
   Target=""
   index=0
   Print Prompt;
   Do
   a=WaitKey()
   If a=8 Then
      If index>0 Then
         Print Chr(8);" ";Chr(8);
         Decr index
         Target=left(Target,index)
      End if
   Else
      If a>31 And a<127 Then
         If index<fitting Then
            Incr Index
            Target=Target+Chr(a)
            Print Chr(a);
         End If
      End If
   End If
   Loop Until a=13
   whiteSpace=1
   If index>0 Then
      For ii=1 to index
         If Mid(Target,ii,1)<>" " Then
            whiteSpace=0
         End if
      Next ii
   End If
   If whiteSpace=1 Then
      Print ""
      Goto UsartInput_inputAgain
   End If
   Print ""
End Sub

Sub TokenizeInput()
   Local i As Integer
   Local x As Integer
   Local whiteSpace As Byte
   Local cap As Integer
   for x=1 to InputElems
      InputElements(x)=""
   next x
   i=1
   x=0
   whiteSpace=1
   For i=1 to len(UsartCommand)
      if whiteSpace=1 and Mid(UsartCommand,i,1)<>" " then
         whiteSpace=0
         Incr x
         cap=0
      else
         if whiteSpace=0 and Mid(UsartCommand,i,1)=" " then
            whiteSpace=1
         end if
      end if
      if whiteSpace=0 then
         If cap<InputElemLen Then
            InputElements(x)=InputElements(x)+Mid(UsartCommand,i,1)
            Incr cap
         End if
      End if
   Next i
   InputtedElements=x
End Sub

Sub FindStringInData(byref Addr As Word, byref Src As String, byref Index As Integer)
   Local binder As String * 15
   Index=0
   FindStringInData_readagain:
   Incr Index
   Call ReadStringSafe(Addr, binder)
   if binder="" then
      Index=0
   else
      binder=lcase(binder)
      if binder<>lcase(Src) then
         goto FindStringInData_readagain
      end if
   end if
End Sub

Sub SeekFromFlash(byref Addr As Word, byval Index As Word)
   Local B As Byte
   Decr Index
   While Index>0
      R30=Low(Addr)
      R31=High(Addr)
      Do
         LoadAdr B,X
         $asm
         lpm
         ST X, R0
         ADIW R30, 1
         $end asm
      Loop until B=0
      LoadAdr Addr, X
      $asm
      ST X+, R30
      ST X, R31
      $end asm
      Decr Index
   Wend
End Sub

Sub PrintFromFlash(byref Addr As Word)
   Local B As Byte
   R30=Low(Addr)
   R31=High(Addr)
   Do
      LoadAdr B,X
      $asm
      lpm
      ST X, R0
      ADIW R30, 1
      PUSH R30
      PUSH R31
      $end asm
      Print Chr(B);
      $asm
      POP R31
      POP R30
      $end asm
   Loop until B=0
   Addr=R31
   Shift Addr, Left, 8
   Addr=Addr+R30
End Sub

Sub ReadStringSafe(byref Addr As Word, byref Dest As String)
   Local B As Byte
   R30=Low(Addr)
   R31=High(Addr)
   Dest=""
   Do
      LoadAdr B,X
      $asm
      lpm
      ST X, R0
      ADIW R30, 1
      $end asm
      if B<>0 Then Dest=Dest+Chr(B)
   Loop until B=0
   Addr=R31
   Shift Addr, Left, 8
   Addr=Addr+R30
End Sub

Sub ScanAddress(byref AddrString As String, byref AddrLong As Long, byref Result As Byte)
   Local TmpLong As Long
   Local TmpByte As Byte
   Local i As Integer
   Local Tmp0xString As String * 2
   TmpLong=0
   Tmp0xString=Left(AddrString,2)
   If LCase(Tmp0xString)="0x" Then
      i=Len(AddrString)-2
      AddrString=Right(AddrString,i)
   End if
   if len(AddrString)>6 Then
      Result=ScanAddress_AddressTooBig
   elseif len(AddrString)=0 Then
      Result=ScanAddress_NoAddress
   Else
      TmpLong=HexVal(AddrString)
      if ERR=1 Then
         Result=ScanAddress_SyntaxError
      else
         Result=ScanAddress_Okay
         AddrLong=TmpLong
      ENd if
   End if
End Sub

CommandVectors:
$asm
jmp MP_cmd_entry
JMP MP_dump_entry
JMP MP_insert_entry
JMP MP_cls_entry
JMP MP_reset_entry
JMP MP_io_entry
$end asm

MP_cmd_entry:
Call MP_cmd
'Print "MP_cmd ran :)"
return
MP_dump_entry:
Call MP_dump
'Print "MP_dump ran :)"
return
MP_insert_entry:
Call MP_insert
'Print "MP_insert ran :)"
return
MP_cls_entry:
Call MP_cls
'Print "MP_cls ran :)"
return
MP_reset_entry:
Call MP_reset
'Print "MP_reset ran :)"
return
MP_io_entry:
Call MP_io
'Print "MP_io ran :)"
return

Sub MP_cmd()
   Local CmdNum As Integer
   Local CmdName As String * 15
   Local Q2 As Word
   Local CmdCount As Integer
   Local PageCount As Integer
   Const CmdPerPage=8
   If InputtedElements<2 Then
      Print "Too little parameters. Try ";Chr(34);"cmd 1";Chr(34);"."
   Elseif InputtedElements>2 Then
      Print "Too much parameters. Try ";Chr(34);"cmd 1";Chr(34);"."
   Else
      CmdNum=Val(InputElements(2))
      If CmdNum<0 Then
         Print "Command page number cannot be negative."
      Elseif CmdNum>0 Then
         'Print "Not yet implemented."
         If TotalCmds=0 Then
            PageCount=0
         Else
            PageCount=TotalCmds
            PageCount=PageCount-1
            PageCount=PageCount/CmdPerPage
            PageCount=PageCount+1
         End if
         If CmdNum>PageCount Then
            Print "Page number too big. Number of pages is ";Str(PageCount);"."
         Else
            Print "List of commands (Page ";Str(CmdNum);" of ";Str(PageCount);"):"
            Q=LoadLabel(MainBinders_Commands)
            Q2=LoadLabel(MainBinders_Descriptions)
            CmdCount=CmdNum
            CmdCount=CmdCount*CmdPerPage
            I=CmdPerPage-1
            CmdCount=CmdCount-I
            For I=1 To 8
               If CmdCount>TotalCmds Then GoTo MP_cmd_StopListing
               Print Str(I);". ";
               Call PrintFromFlash(Q)
               Print ": ";
               Call PrintFromFlash(Q2)
               Print ""
               Incr CmdCount
            Next I
            MP_cmd_StopListing:
            Print "End of page ";Str(CmdNum);".";
            If CmdNum=PageCount Then
               Print ""
            Else
               I=CmdNum+1
               Print " For next page, type ";Chr(34);"cmd ";I;Chr(34);"."
            End If
         End If
      Else
         CmdName=lcase(InputElements(2))
         Q=LoadLabel(MainBinders_Commands)
         Call FindStringInData(Q,InputElements(2),I)
         If I>0 Then
            Print "Help for command: ";Chr(34);CmdName;Chr(34);":"
            Q=LoadLabel(MainBinders_Descriptions)
            Call SeekFromFlash(Q,I)
            Call PrintFromFlash(Q)
            Print ""
         Else
            Print "Command ";Chr(34);CmdName;Chr(34);" doesn't exist. To see what commands do, type ";Chr(34);"cmd 1";Chr(34);"."
         End if
      End If
   End if
End Sub

Sub MP_dump()
   Local DumpSrc As Byte
   Local StartAddr As Long
   Local EndAddr As Long
   Local Hex2LongResult As Byte
   Local GoodToRead As Byte
   Local TmpString As String*15
   Local TmpLong As Long
   Local TmpByte As Byte
   GoodToRead=0
   If InputtedElements<2 Then
      Print "Too little parameters."
   Elseif InputtedElements=2 Then
      TmpString=lcase(InputElements(2))
      If TmpString="preview" Then
         'Print "Preview not yet implemented"
         If LastDumpFlag=1 Then
            StartAddr=LastDumpStartAddr
            EndAddr=LastDumpEndAddr
            DumpSrc=LastDumpSrc
            GoodToRead=1
         Else
            Print "There is nothing to preview."
         End if
      Elseif TmpString="stack" then
         Print "Dumping stack not yet implemented."
      Elseif TmpString="prog" Then
         Print "Missing address(es)."
      Elseif TmpString="data" Then
         Print "Missing address(es)."
      Elseif TmpString="next" Then
         If LastDumpFlag=0 Then
            Print "There's nothing next because there wasn't anything previous."
         Else
            StartAddr=LastDumpEndAddr
            EndAddr=LastDumpEndAddr-LastDumpStartAddr
            EndAddr=EndAddr+LastDumpEndAddr
            DumpSrc=LastDumpSrc
            GoodToRead=1
         End if
      Else
         Print Chr(34);"dump ";InputElements(2);Chr(34);" doesn't mean anything to Monitor Program."
      End If
   Elseif InputtedElements>2 Then
      TmpString=lcase(InputElements(2))
      If TmpString="prog" Then
         DumpSrc=DumpFromProg
      Elseif TmpString="data" Then
         DumpSrc=DumpFromData
      Else
         DumpSrc=DumpFromUndefined
         Print "Dump source unknown. Only ";Chr(34);"prog";Chr(34);" and ";Chr(34);"data";Chr(34);" are supported."
      End If
      If DumpSrc<>DumpFromUndefined Then
         Call ScanAddress(InputElements(3),StartAddr,Hex2LongResult)
         Select Case Hex2LongResult
            Case ScanAddress_AddressTooBig:
               Print "Start address too big."
            Case ScanAddress_SyntaxError:
               Print "Start address isn't in hexadecimal format."
            Case ScanAddress_NoAddress:
               Print "Start address is missing."
            Case Else:
               If InputtedElements=3 Then
                  EndAddr=StartAddr+$FF
                  GoodToRead=1
               Elseif InputtedElements=4 Then
                  Call ScanAddress(InputElements(4),EndAddr,Hex2LongResult)
                  Select Case Hex2LongResult
                     Case ScanAddress_AddressTooBig:
                        Print "End address too big."
                     Case ScanAddress_SyntaxError:
                        Print "End address isn't in hexadecimal format."
                     Case ScanAddress_NoAddress:
                        Print "End address is missing."
                     Case Else:
                        If StartAddr>EndAddr Then
                           Print "Start address mustn't be greater than end address."
                        Else
                           GoodToRead=1
                        End if
                  End Select
               Else
                  Print "Too many parameters."
               End If
         End Select
      End if
   End if
   If GoodToRead=1 Then
      LastDumpFlag=1
      LastDumpStartAddr=StartAddr
      LastDumpEndAddr=EndAddr
      LastDumpSrc=DumpSrc
      'Print "Start=";Hex(StartAddr);" End=";Hex(EndAddr);" Src=";DumpSrc
      While StartAddr<=EndAddr
         TmpLong=EndAddr-StartAddr
         If TmpLong>=16 Then
            I=16
         Else
            I=TmpLong Mod 16
            Incr I
         End if
         J=DumpSrc
         Q=LowW(StartAddr)
         QH=HighW(StartAddr)
         LoadAdr Q,X
         LD R30, X+
         LD R31, X
         LDS R18, RAMPZ
         PUSH R18
         LDS R17, {QH}
         STS RAMPZ, R17
         LoadAdr DumpedBytes(1),X
         LDS R18, {I}
         lds R19, {J}
         CPI R19, DumpFromProg
         BREQ MP_dump_ProgLoop
         MP_dump_DumpDataLoop:
         LD R17, Z+
         ST X+, R17
         DEC R18
         CPI R18, 0
         BRNE MP_dump_DumpDataLoop
         JMP MP_dump_DumpLoopsOver
         MP_dump_ProgLoop:
         LPM
         LD R17, Z+
         MOV R17, R0
         ST X+, R17
         DEC R18
         CPI R18, 0
         BRNE MP_dump_ProgLoop
         MP_dump_DumpLoopsOver:
         POP R18
         STS RAMPZ, R18
         TmpString=Hex(StartAddr)
         TmpString=Right(TmpString,6)
         Select Case DumpSrc
            Case DumpFromProg:
               Print "prog ";
            Case DumpFromData:
               Print "data ";
            Case Else:
               Print "???? ";
         End Select
         Print "0x";TmpString;" ";
         For I=1 To 16
            J=I-1
            If J>TmpLong Then
               Print "   ";
            Else
               TmpString=Hex(DumpedBytes(I))
               Print TmpString;" ";
            End if
         Next I
         For I=1 To 16
            J=I-1
            If J>TmpLong Then
               Print " ";
            Else
               TmpByte=DumpedBytes(I)
               If TmpByte>=32 then
                  If TmpByte<>127 Then
                     If TmpByte <>255 Then
                        GoTo MP_dump_PrintChr
                     End if
                  End If
               End If
               MP_dump_PrintDot:
               Print ".";
               GoTo MP_dump_PrintOver
               MP_dump_PrintChr:
               Print Chr(TmpByte);
               MP_dump_PrintOver:
            End if
         Next I
         Print ""
         StartAddr=StartAddr+16
      Wend
   End if
End Sub

Sub MP_insert()
   Local TmpByte As Byte
   Local TmpInt As Integer
   Local Hex2LongResult As Byte
   Local Addr As Word
   Local DataType As Byte
   Local SomethingBad As Byte
   Local TmpWord As Word
   Const DataSynErr_None = 0
   Const DataSynErr_Quote_expected = 1
   Const DataSynErr_InvChHiNum = 2
   Const DataSynErr_InvChNegNum = 3
   Const DataSynErr_Invalid_char_number = 4
   Const DataSynErr_Invalid_number = 5
   Const DataSynErr_Dont_Print = 6
   Const DataType_None = 0
   Const DataType_Byte = 1
   Const DataType_Int = 2
   Local TmpString As String * 15

   DataType=DataType_None
   If InputtedElements<2 Then
      Print "Address and data missing."
   Elseif InputtedElements=2 Then
      Print "Missing data";
      Call ScanAddress(InputElements(2),Addr,Hex2LongResult)
      Select Case Hex2LongResult
         Case ScanAddress_AddressTooBig:
            Print " and input address too big."
         Case ScanAddress_SyntaxError:
            Print " and input address isn't in hexadecimal format."
         Case ScanAddress_NoAddress:
            Print " and input address is missing."
         Case Else:
            Print "."
      End Select
   Elseif InputtedElements>2 Then
      Call ScanAddress(InputElements(2),Addr,Hex2LongResult)
      Select Case Hex2LongResult
         Case ScanAddress_AddressTooBig:
            Print "Input address too big."
         Case ScanAddress_SyntaxError:
            Print "Input address isn't in hexadecimal format."
         Case ScanAddress_NoAddress:
            Print "Input address is missing."
         Case Else:
            SomethingBad=DataSynErr_None
            LastDumpSrc=DumpFromData
            LastDumpStartAddr=Addr
            LastDumpEndAddr=LastDumpStartAddr
            LastDumpFlag=1
            For I=3 To InputtedElements
               If Left(InputElements(I),1)="'" Then
                  If Right(InputElements(I),1)="'" Then
                     If Left(InputElements(I),2)="'\" Then
                        If Len(InputElements(I))>3 Then
                           J=Len(InputElements(I))-3
                           TmpString=Mid(InputElements(I),3,J)
                           TmpString=lcase(TmpString)
                           J=Val(TmpString)
                           If Err=1 Then
                              DataType=DataType_Byte
                              If TmpString="n" Then
                                 TmpByte=13
                              Elseif TmpString="a" Then
                                 TmpByte=7
                              Elseif TmpString="b" Then
                                 TmpByte=8
                              Elseif TmpString="f" Then
                                 TmpByte=12
                              Elseif TmpString="n" Then
                                 TmpByte=10
                              Elseif TmpString="r" Then
                                 TmpByte=13
                              Elseif TmpString="t" Then
                                 TmpByte=9
                              Elseif TmpString="v" Then
                                 TmpByte=11
                              Elseif TmpString="\" Then
                                 TmpByte=92
                              Elseif TmpString="'" Then
                                 TmpByte=39
                              Elseif Asc(TmpString,1)=34 Then
                                 TmpByte=34
                              Elseif TmpString="?" Then
                                 TmpByte=63
                              Elseif Left(TmpString,1)="x" Then
                                 Call ScanAddress(TmpString,TmpWord,Hex2LongResult)
                                 Select Case Hex2LongResult
                                    Case ScanAddress_AddressTooBig:
                                       Print "Input address too big."
                                       SomethingBad=DataSynErr_Dont_Print
                                    Case ScanAddress_SyntaxError:
                                       Print "Input address isn't in hexadecimal format."
                                       SomethingBad=DataSynErr_Dont_Print
                                    Case ScanAddress_NoAddress:
                                       Print "Input address is missing."
                                       SomethingBad=DataSynErr_Dont_Print
                                    Case Else:
                                       If TmpWord>255 Then
                                          SomethingBad=DataSynErr_InvChHiNum
                                       Elseif TmpWord<0 Then
                                          SomethingBad=DataSynErr_InvChNegNum
                                       Else
                                          TmpByte=TmpWord
                                       End if
                                 End Select
                                 If SomethingBad<>DataSynErr_None Then
                                    Exit For
                                 End if
                              Else
                                 SomethingBad=DataSynErr_Invalid_char_number
                                 Exit For
                              End if
                           Elseif J>255 Then
                              SomethingBad=DataSynErr_InvChHiNum
                           Elseif J<0 Then
                              SomethingBad=DataSynErr_InvChNegNum
                           Else
                              DataType=DataType_Byte
                              TmpByte=J
                           End if
                        Elseif InputElements(I)="'\'" Then
                           DataType=DataType_Byte
                           TmpByte=92
                        End if
                     Else
                        DataType=DataType_Byte
                        TmpByte=Asc(InputElements(I),2)
                     End if
                  Else
                     SomethingBad=DataSynErr_Quote_expected
                     Exit For
                  End if
               Else
                  TmpString=Right(InputElements(I),1)
                  TmpString=lcase(TmpString)
                  If TmpString="s" Then
                     TmpInt=Len(InputElements(I))-1
                     TmpString=Left(TmpString,TmpInt)
                     DataType=DataType_Byte
                  Else
                     DataType=DataType_Int
                  End if
                  J=Val(InputElements(I))
                  If Err=1 Then
                     SomethingBad=DataSynErr_Invalid_number
                     Exit For
                  Elseif DataType=DataType_Byte Then
                     DataType=DataType_Byte
                     TmpByte=J
                  Else
                     DataType=DataType_Int
                     TmpInt=J
                  End if
               End if
               If SomethingBad=0 Then
                  If DataType=DataType_Byte Then
                     TmpGlobalByte=TmpByte
                     Q=LowW(Addr)
                     QH=HighW(Addr)
                     LoadAdr Q,X
                     LD R30, X+
                     LD R31, X
                     LDS R18, RAMPZ
                     PUSH R18
                     'LDS R17, {QH}
                     LDI R17, 0
                     STS RAMPZ, R17
                     LDS R17,{TmpGlobalByte}
                     ST Z, R17
                     POP R18
                     STS RAMPZ, R18
                     Addr=Addr+1
                  Elseif DataType=DataType_Int Then
                     J=TmpInt
                     Q=LowW(Addr)
                     QH=HighW(Addr)
                     LoadAdr Q,X
                     LD R30, X+
                     LD R31, X
                     LDS R18, RAMPZ
                     PUSH R18
                     'LDS R17, {QH}
                     LDI R17, 0
                     STS RAMPZ, R17
                     LDS R17,{J}
                     ST Z+, R17
                     LDS R17,{J+1}
                     ST Z, R17
                     POP R18
                     STS RAMPZ, R18
                     Addr=Addr+2
                  Else
                     Print "Undefined data type."
                  End if
               Else
                  Print "Error in input and the FOR loop wasn't exited!"
               End if
            Next I
            LastDumpEndAddr=Addr-1
            if LastDumpStartAddr=LastDumpEndAddr Then
               LastDumpFlag=0
            else
               Print "Success! To see what has been written, type ";Chr(34);"dump preview";Chr(34);"."
            End if
      End Select
      If SomethingBad=1 Then
         J=I-2
         Print "Not all bytes were written due to error in data parameter ";Str(J);" saying ";Chr(34);InputElements(I);Chr(34);"."
         Select Case SomethingBad
            Case DataSynErr_None:
               Print "There was no error so why say there was in the first place?"
            Case DataSynErr_Quote_expected:
               Print "Quote expected at the end."
            Case DataSynErr_InvChHiNum:
               Print "Invalid character because the number is over 255."
            Case DataSynErr_InvChNegNum:
               Print "Invalid character because the number is negative."
            Case DataSynErr_Invalid_char_number:
               Print "Invalid character number."
            Case DataSynErr_Invalid_number:
               Print "Invalid number."
            Case DataSynErr_Dont_Print:
               Print "Already explained."
         End Select
      End if
   End if
End Sub

Sub MP_cls()
Print Chr(12);
End Sub

Sub MP_reset()
$asm
CLI
LDI R17, $D8
STS CPU_CCP, R17
SBI RST_CTRL, 0
SEI
$end asm
End Sub

Sub MP_io()
'6. io - upravlja sadržajem registara perifernih sklopova
'Sintaksa #1: io (ime perifernog sklopa) - ispisuje sadržaj registara navedenog perifernog sklopa
'Podržani periferni sklopovi su:
'PORTA, PORTB, PORTC, PORTD, PORTE, PORTF, PORTH, PORTJ, PORTK, PORTQ, USARTC0, USARTC1, USARTD0,
'USARTD1, USARTE0, USARTE1, USARTF0, USARTF1, TCC0, TCC1, TCD0, TCD1, TCE0, TCE1, TCF0, TCF1
'Sintaksa #2: io (ime perifernog sklopa).(ime registra perifernog sklopa) - ispisuje sadržaj
'navedenog registra navedenog perifernog sklopa
'Sintaksa #3: io (ime perifernog sklopa).(ime registra perifernog sklopa)=(8-bitni podatak ili
'16-bitni za 16-bitne registre) - upisuje navedeni podatak u navedeni registar navedenog
'perifernog sklopa
   Local IOName As String * 16
   Local IOIndex As Integer
   Local RegName As String * 16
   Local Addr As Word
   Local Addr2 As Word
   Local AddrSum As Word
   Local IOType As Byte
   Local TmpString As String * 4
   Local Lines As Byte
   Local SumtinBad As Byte
   If InputtedElements<2 Then
      Print "IO peripheral not specified. There are peripherals:"
      Print "Ports: ";
      Restore PortBinders_Names
      Read IOName
      While IOName<>""
         Print IOName;
         Read IOName
         If IOName<>"" Then
            Print ", ";
         Else
            Print "."
         End if
      Wend
      Print "Port registers: "
      Restore PortRegisters_Names
      Read RegName
      While RegName<>""
         Print RegName;
         Read RegName
         If RegName<>"" Then
            Print ", ";
         Else
            Print "."
         End if
      Wend
      Print "USARTs: ";
      Restore UsartBinders_Names
      Read IOName
      While IOName<>""
         Print IOName;
         Read IOName
         If IOName<>"" Then
            Print ", ";
         Else
            Print "."
         End if
      Wend
      Print "USART registers: "
      Restore UsartRegisters_Names
      Read RegName
      While RegName<>""
         Print RegName;
         Read RegName
         If RegName<>"" Then
            Print ", ";
         Else
            Print "."
         End if
      Wend
      Print "Timer/Counter-s: ";
      Restore TCBinders_Names
      Read IOName
      While IOName<>""
         Print IOName;
         Read IOName
         If IOName<>"" Then
            Print ", ";
         Else
            Print "."
         End if
      Wend
      Print "Timer/Counter registers: "
      Restore TCRegisters_Names
      Read RegName
      While RegName<>""
         Print RegName;
         Read RegName
         If RegName<>"" Then
            Print ", ";
         Else
            Print "."
         End if
      Wend
   Else
      IOName=""
      I=1
      IOType=255
      Q=LoadLabel(PortBinders_Names)
      Call FindStringInData(Q,InputElements(2),I)
      If I>0 Then
         IOIndex=I
         IOType=0
      Else
         Q=LoadLabel(UsartBinders_Names)
         Call FindStringInData(Q,InputElements(2),I)
         If I>0 Then
            IOIndex=I
            IOType=1
         Else
            Q=LoadLabel(TCBinders_Names)
            Call FindStringInData(Q,InputElements(2),I)
            If I>0 Then
               IOIndex=I
               IOType=2
            Else
               Print "IO peripheral ";Chr(34);InputElements(2);Chr(34);" doesn't exist or isn't yet supported."
            End if
         End if
      End if
      If IOType=255 Then Goto MP_io_end
      Select Case IOType
         Case 0:
            Restore PortBinders_Addresses
         Case 1:
            Restore UsartBinders_Addresses
         Case 2:
            Restore TCBinders_Addresses
         Case Else:
            Print "Unknown IO type. Oops."
      End Select
      For I=1 To IOIndex
         Read Addr
      Next I
      Select Case IOType
         Case 0:
            Restore PortBinders_Names
         Case 1:
            Restore UsartBinders_Names
         Case 2:
            Restore TCBinders_Names
         Case Else:
            Print "Unknown IO type. Oops."
      End Select
      For I=1 To IOIndex
         Read IOName
      Next I
      If InputtedElements = 2 Then
         Print IOName; ":"
         Select Case IOType
            Case 0:
               Q=LoadLabel(PortRegisters_Names)
               Restore PortRegisters_Offsets
            Case 1:
               Q=LoadLabel(UsartRegisters_Names)
               Restore UsartRegisters_Offsets
            Case 2:
               Q=LoadLabel(TCRegisters_Names)
               Restore TCRegisters_Offsets
         End Select
         Lines=0
         Read Addr2
         While Addr2<65535
            Call PrintFromFlash(Q)
            QH=Addr+Addr2
            LoadAdr QH,Z
            LD R26, Z+
            ld R27, Z
            LD R17, X
            STS {TmpGlobalByte}, R17
            TmpString=Hex(TmpGlobalByte)
            Print "=0x";TmpString;
            Incr Lines
            J=Lines Mod 5
            If J = 0 Then
               Print ""
            Else
               Print "  ";
            End if
            Read Addr2
         Wend
         Print ""
      Elseif InputtedElements <= 4 Then
         Select Case IOType
         Case 0:
            Q=LoadLabel(PortRegisters_Names)
            Restore PortRegisters_Offsets
         Case 1:
            Q=LoadLabel(UsartRegisters_Names)
            Restore UsartRegisters_Offsets
         Case 2:
            Q=LoadLabel(TCRegisters_Names)
            Restore TCRegisters_Offsets
         End Select
         Call FindStringInData(Q,InputElements(3),I)
         If I>0 Then
            If InputtedElements = 3 Then Print IOName; ".";
            Select Case IOType
            Case 0:
               Q=LoadLabel(PortRegisters_Names)
               Restore PortRegisters_Offsets
            Case 1:
               Q=LoadLabel(UsartRegisters_Names)
               Restore UsartRegisters_Offsets
            Case 2:
               Q=LoadLabel(TCRegisters_Names)
               Restore TCRegisters_Offsets
            End Select
            If InputtedElements = 3 Then
               Call SeekFromFlash(Q,I)
               Call PrintFromFlash(Q)
            End if
            For J=1 To I
               Read Addr2
            Next J
            QH=Addr+Addr2
            SumtinBad=0
            If InputtedElements = 4 Then
               TmpString=Left(InputElements(4),2)
               If lcase(TmpString)="0x" Then
                  I=Len(InputElements(4))
                  I=I-2
                  TmpString=Mid(InputElements(4),3,I)
                  TmpGlobalByte=HexVal(TmpString)
                  If Err=1 Then
                     Print "Invalid hex number"
                     SumtinBad=1
                  End if
               Elseif lcase(TmpString)="0b" Then
                  I=Len(InputElements(4))
                  I=I-2
                  TmpString=Mid(InputElements(4),3,I)
                  TmpGlobalByte=BinVal(TmpString)
                  If Err=1 Then
                     Print "Invalid binary number"
                     SumtinBad=1
                  End if
               Else
                  TmpGlobalByte=Val(InputElements(4))
                  If Err=1 Then
                     Print "Invalid value"
                     SumtinBad=1
                  End if
               End if
            End if
            If SumtinBad=0 Then
               LoadAdr QH,Z
               LD R26, Z+
               ld R27, Z
               LDS R17, {InputtedElements}
               CPI R17, 4
               BRNE MP_io_PrintByte_Tramp
               JMP MP_io_PrintByte_Tramp_Over
               MP_io_PrintByte_Tramp:
               JMP MP_io_PrintByte
               MP_io_PrintByte_Tramp_Over:
               LDS R17, {TmpGlobalByte}
               ST X, R17
               Print "Byte ";InputElements(4);" has been written to ";InputElements(2);".";InputElements(3);"."
               JMP MP_io_end
               MP_io_PrintByte:
               LD R17, X
               STS {TmpGlobalByte}, R17
               TmpString=Hex(TmpGlobalByte)
               Print "=0x";TmpString;
               Print ""
            End if
         Else
            Print Chr(34);InputElements(3);Chr(34);" is not a register of the peripheral ";Chr(34);IOName;Chr(34);"."
         End if
      Else
         Print "Too many parameters"
      End if
   End if
   MP_io_end:
End Sub

MainBinders_Commands:
Data "cmd", "dump", "insert", "cls", "reset", "io", "" 'END

MainBinders_Descriptions:
Data "Prints list of commands with descriptions what each does"
Data "Prints contents of memory"
Data "Inserts data into data memory"
Data "Clears the screen"
Data "Resets the microcontroller"
Data "Controls the contents of I/O registers"
Data "" ' END

PortBinders_Names:
Data "PORTA", "PORTB", "PORTC", "PORTD", "PORTE", "PORTF", "PORTH", "PORTJ", "PORTK", "PORTQ", "" 'END
PortBinders_Addresses:
Data 1536%, 1568%, 1600%, 1632%, 1664%, 1696%, 1760%, 1792%, 1824%, 1984%, 2016%, 65535% 'END

UsartBinders_Names:
Data "USARTC0", "USARTC1", "USARTD0", "USARTD1", "USARTE0", "USARTE1", "USARTF0", "USARTF1", "" 'END
UsartBinders_Addresses:
Data 2208%, 2224%, 2464%, 2480%, 2720%, 2736%, 2816%, 2880%, 65535% 'END

TCBinders_Names:
Data "TCC0", "TCC1", "TCD0", "TCD1", "TCE0", "TCE1", "TCF0", "TCF1" 'END
TCBinders_Addresses:
Data 2048%, 2112%, 2304%, 2368%, 2560%, 2816%, 65535% 'END

PortRegisters_Names:
Data "DIR", "DIRSET", "DIRCLR", "DIRTGL", "OUT", "OUTSET", "OUTCLR", "OUTTGL"
Data "IN", "INTCTRL", "INT0MASK", "INT1MASK", "INTFLAGS", "PIN0CTRL", "PIN1CTRL", "PIN2CTRL"
Data "PIN3CTRL", "PIN4CTRL", "PIN5CTRL", "PIN6CTRL", "PIN7CTRL", "" 'END
PortRegisters_Offsets:
Data 0%,1%,2%,3%,4%,5%,6%,7%,8%,9%,10%,11%,12%,16%,17%,18%,19%,20%,21%,22%,23%, 65535% 'END

UsartRegisters_Names:
Data "DATA", "STATUS", "CTRLA", "CTRLB", "CTRLC", "BAUDCTRLA", "BAUDCTRLB", "" 'END
UsartRegisters_Offsets:
Data 0%,1%,3%,4%,5%,6%,7%,65535% 'END

TCRegisters_Names:
Data "CTRLA", "CTRLB", "CTRLC", "CTRLD", "CTRLE", "INTCTRLA", "INTCTRLB", "CTRLFCLR"
Data "CTRLFSET", "CTRLGCLR", "CTRLGSET", "INTFLAGS", "TEMP"
Data "CNTL", "CNTH", "PERL", "PERH", "CCAL", "CCAH", "CCBL", "CCBH", "CCCL", "CCCH", "CCDL", "CCDH", "" 'END
TCRegisters_Offsets:
Data 0%,1%,2%,3%,4%,6%,7%,8%,9%,10%,11%,12%,15%,32%,33%,38%,39%,40%,41%,42%,43%,44%,45%,46%,47%, 65535% 'END