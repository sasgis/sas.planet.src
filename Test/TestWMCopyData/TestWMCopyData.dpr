program TestWMCopyData;

{$APPTYPE CONSOLE}

uses
  Windows,
  Messages,
  SysUtils,
  u_CmdLineArgProcessorAPI in '..\..\u_CmdLineArgProcessorAPI.pas';

type
  TFNWndEnumProc = function(hWnd: HWND; lParam: LPARAM): BOOL; stdcall;

function EnumWindows(lpEnumFunc: TFNWndEnumProc; lParam: LPARAM): BOOL; stdcall; external  user32;

var
  GWindowsCount: Int64 = 0;

const
  WM_IFF = u_CmdLineArgProcessorAPI.WM_FRIEND_OR_FOE;

function EnumWindowsProc(hWnd: HWND; lParam: LPARAM): BOOL; stdcall;
var
  VOutVal: DWORD;
  VRetVal: LRESULT;
begin
  Inc(GWindowsCount);

  Write(Format('[%d] Send message to 0x%s: ', [GWindowsCount, IntToHex(hWnd, 8)]));

  VRetVal :=
    SendMessageTimeout(
      hWnd,
      WM_IFF,
      0,
      0,
      SMTO_BLOCK or SMTO_ABORTIFHUNG,
      200,
      VOutVal
    );

  if VRetVal <> 0 then begin
    if VOutVal = WM_IFF then begin
      PHandle(lParam)^ := hWnd;
      Result := False;
      Writeln('OK - Detect SASPlanet window!');
    end else begin
      Result := True;
      Writeln('SKIP - Unknown window');
    end;
  end else begin
    Result := True;
    Writeln('FAIL - ' + SysErrorMessage(GetLastError));
  end;
end;

procedure SendArgs(const Args: AnsiString);
var
  VHandle: HWND;
  VRetVal: LRESULT;
  VCopyData: TCopyDataStruct;
  VRecievedStr: AnsiString;
begin
  VHandle := INVALID_HANDLE_VALUE;

  EnumWindows(EnumWindowsProc, LPARAM(@VHandle));

  if VHandle <> INVALID_HANDLE_VALUE then begin

    if Args[Length(Args)] <> #0 then begin
      VRecievedStr := Args + #0;
    end else begin
      VRecievedStr := Args;
    end;

    VCopyData.dwData := 0;
    VCopyData.cbData := Length(VRecievedStr) - 1;
    VCopyData.lpData := PAnsiChar(VRecievedStr);

    Writeln('Send WM_COPYDATA: ' + Args);

    VRetVal := SendMessage(VHandle, WM_COPYDATA, 0, LPARAM(@VCopyData));
    
    if VRetVal = 0 then begin
      Writeln('Done!');
    end else begin
      Writeln(
        'Done with error: ' +
        'RetVal = 0x' + IntToHex(VRetVal, 8) + '; ' +
        'Error = ' + u_CmdLineArgProcessorAPI.GetErrorFromCode(VRetVal)
      );
    end;
  end else begin
    Writeln('Done with error: SASPlanet window not found');
  end;
end;

var
  I: Integer;
  VArgStr: AnsiString;
begin
  try
    VArgStr := '';

    for I := 1 to ParamCount do begin
      if SameText('-h', ParamStr(I)) or SameText('--help', ParamStr(I)) then begin
        Writeln(u_CmdLineArgProcessorAPI.GetHelp(ExtractFileName(ParamStr(0))));
        Exit;
      end;
      if VArgStr <> '' then begin
        VArgStr := VArgStr + ' "' + ParamStr(I) + '"';
      end else begin
        VArgStr := '"' + ParamStr(I) + '"';
      end;
    end;

    if VArgStr <> '' then begin
      SendArgs(VArgStr);
    end else begin
      Writeln(u_CmdLineArgProcessorAPI.GetHelp(ExtractFileName(ParamStr(0))));
      Exit;
    end;
  except
    on E:Exception do
      Writeln(E.Classname, ': ', E.Message);
  end;
end.
