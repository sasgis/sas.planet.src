{******************************************************************************}
{* SAS.Planet (SAS.Планета)                                                   *}
{* Copyright (C) 2007-2012, SAS.Planet development team.                      *}
{* This program is free software: you can redistribute it and/or modify       *}
{* it under the terms of the GNU General Public License as published by       *}
{* the Free Software Foundation, either version 3 of the License, or          *}
{* (at your option) any later version.                                        *}
{*                                                                            *}
{* This program is distributed in the hope that it will be useful,            *}
{* but WITHOUT ANY WARRANTY; without even the implied warranty of             *}
{* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the              *}
{* GNU General Public License for more details.                               *}
{*                                                                            *}
{* You should have received a copy of the GNU General Public License          *}
{* along with this program.  If not, see <http://www.gnu.org/licenses/>.      *}
{*                                                                            *}
{* http://sasgis.ru                                                           *}
{* az@sasgis.ru                                                               *}
{******************************************************************************}

unit u_GarbageCollectorThread;

interface

uses
  Windows,
  Classes,
  i_NotifierTTLCheck;

type
  TGarbageCollectorThread = class(TThread)
  private
    FList: INotifierTTLCheck;
    FListInternal: INotifierTTLCheckInternal;
    FSleepTime: Cardinal;
  protected
    procedure Execute; override;
  public
    constructor Create(
      const AList: INotifierTTLCheckInternal;
      ASleepTime: Cardinal
    );
    destructor Destroy; override;
    property List: INotifierTTLCheck read FList;
  end;

implementation

uses
  u_ReadableThreadNames;

constructor TGarbageCollectorThread.Create(
  const AList: INotifierTTLCheckInternal;
  ASleepTime: Cardinal
);
begin
  inherited Create(false);
  FListInternal := AList;
  FList := FListInternal;
  FSleepTime := ASleepTime;
end;

destructor TGarbageCollectorThread.Destroy;
begin
  FList := nil;
  inherited;
end;

procedure TGarbageCollectorThread.Execute;
var
  VNextCheck: Cardinal;
  VNow: Cardinal;
begin
  SetCurrentThreadName(AnsiString(Self.ClassName));
  VNextCheck := 0;
  while not Terminated do begin
    VNow := GetTickCount;
    if (VNextCheck = 0) or (VNextCheck <= VNow) or ((VNextCheck > (1 shl 30)) and (VNow < (1 shl 29))) then begin
      VNextCheck := FListInternal.ProcessCheckAndGetNextTime;
      if Terminated then begin
        Break;
      end;
    end;
    Sleep(FSleepTime);
  end;
end;

end.
