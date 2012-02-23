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

unit u_GlobalInternetState;

interface

uses
  SyncObjs;

type
  TGlobalInternetState = class (TObject)
  private
    FCS: TCriticalSection;
    FTaskCount: Integer;
    function GetTaskCount(): Integer;
  public
    constructor Create;
    destructor Destroy; override;
    procedure IncTaskCount;
    procedure DecTaskCount;
    property TaskCount: Integer read GetTaskCount;
  end;

var
  GInternetState: TGlobalInternetState = nil;

implementation

{ TGlobalInternetState }

constructor TGlobalInternetState.Create;
begin
  inherited;
  FCS := TCriticalSection.Create;
end;

destructor TGlobalInternetState.Destroy;
begin
  FCS.Free;
  inherited Destroy;
end;

procedure TGlobalInternetState.IncTaskCount;
begin
  FCS.Acquire;
  try
    Inc(FTaskCount);
  finally
    FCS.Release;
  end;
end;

procedure TGlobalInternetState.DecTaskCount;
begin
  FCS.Acquire;
  try
    Dec(FTaskCount);
    if FTaskCount < 0 then begin
      FTaskCount := 0;
    end;
  finally
    FCS.Release;
  end;
end;

function TGlobalInternetState.GetTaskCount(): Integer;
begin
  FCS.Acquire;
  try
    Result := FTaskCount;
  finally
    FCS.Release;
  end;
end;

initialization
  GInternetState := TGlobalInternetState.Create;

finalization
  GInternetState.Free;

end.
