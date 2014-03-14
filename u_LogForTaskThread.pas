{******************************************************************************}
{* SAS.Planet (SAS.Планета)                                                   *}
{* Copyright (C) 2007-2014, SAS.Planet development team.                      *}
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
{* http://sasgis.org                                                          *}
{* info@sasgis.org                                                            *}
{******************************************************************************}

unit u_LogForTaskThread;

interface

uses
  SysUtils,
  WideStrings,
  i_LogSimple,
  i_LogSimpleProvider,
  u_BaseInterfacedObject;

type
  TLogSimpleProvider = class(TBaseInterfacedObject, ILogSimpleProvider, ILogSimple)
  private
    FMinLogLevel: Integer;
    FNextId: Cardinal;
    FMaxRowsCount: Cardinal;
    FLock: IReadWriteSync;
    FList: TWideStringList;
    FLinesSeparator: WideString;
  private
    procedure WriteText(
      const AMessage: WideString;
      ALogLevel: integer
    ); safecall;
  private
    function GetLastMessages(
      AMaxRowsCount: Cardinal;
      var ALastId: Cardinal;
      out AcntLines: Cardinal
    ): WideString; safecall;
  public
    constructor Create(
      AMaxLinesCount: Cardinal;
      AMinLogLevel: Integer
    );
    destructor Destroy; override;
  end;

implementation

uses
  u_Synchronizer;

{ TLogForTaskThread }

constructor TLogSimpleProvider.Create(
  AMaxLinesCount: Cardinal;
  AMinLogLevel: Integer
);
var
  i: Integer;
begin
  inherited Create;
  FMinLogLevel := AMinLogLevel;
  FMaxRowsCount := AMaxLinesCount;
  FLock := MakeSyncRW_Std(Self, False);
  FList := TWideStringList.Create;
  FList.Capacity := FMaxRowsCount;
  for i := 0 to FMaxRowsCount - 1 do begin
    FList.Add('');
  end;
  FLinesSeparator := #13#10;
end;

destructor TLogSimpleProvider.Destroy;
begin
  FLock := nil;
  FreeAndNil(FList);
  inherited;
end;

function TLogSimpleProvider.GetLastMessages(
  AMaxRowsCount: Cardinal;
  var ALastId: Cardinal;
  out AcntLines: Cardinal
): WideString;
var
  VNewRowsCount: Cardinal;
  i: Cardinal;
  VFirstLine: Boolean;
begin
  Result := '';
  if ALastId < FNextId then begin
    FLock.BeginRead;
    try
      VNewRowsCount := FNextId - ALastId;
      if VNewRowsCount > AMaxRowsCount then begin
        VNewRowsCount := AMaxRowsCount;
      end;
      if VNewRowsCount > FMaxRowsCount then begin
        VNewRowsCount := FMaxRowsCount;
      end;
      VFirstLine := true;
      for i := FNextId - VNewRowsCount to FNextId - 1 do begin
        if VFirstLine then begin
          Result := FList.Strings[i mod FMaxRowsCount];
          VFirstLine := False;
        end else begin
          Result := Result + FLinesSeparator + FList.Strings[i mod FMaxRowsCount];
        end;
      end;
      AcntLines := VNewRowsCount;
      ALastId := FNextId;
    finally
      FLock.EndRead;
    end;
  end else begin
    AcntLines := 0;
    ALastId := FNextId;
  end;
end;

procedure TLogSimpleProvider.WriteText(
  const AMessage: WideString;
  ALogLevel: integer
);
var
  VIndex: Cardinal;
begin
  if ALogLevel >= FMinLogLevel then begin
    FLock.BeginWrite;
    try
      VIndex := FNextId mod FMaxRowsCount;
      FList.Strings[VIndex] := AMessage;
      Inc(FNextId);
    finally
      FLock.EndWrite;
    end;
  end;
end;

end.
