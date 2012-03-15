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
  SysUtils,
  i_GlobalInternetState;

type
  TGlobalInternetState = class (TInterfacedObject, IGlobalInternetState)
  private
    FCS: IReadWriteSync;
    FQueueCount: Integer;
    function GetQueueCount(): Integer;
  public
    constructor Create;
    destructor Destroy; override;
    procedure IncQueueCount;
    procedure DecQueueCount;
    property QueueCount: Integer read GetQueueCount;
  end;

implementation

uses
  u_Synchronizer;

{ TGlobalInternetState }

constructor TGlobalInternetState.Create;
begin
  inherited;
  FCS := MakeSyncMulti(Self);
  FQueueCount := 0;
end;

destructor TGlobalInternetState.Destroy;
begin
  FCS := nil;
  inherited Destroy;
end;

procedure TGlobalInternetState.IncQueueCount;
begin
  FCS.BeginWrite;
  try
    Inc(FQueueCount);
  finally
    FCS.EndWrite;
  end;
end;

procedure TGlobalInternetState.DecQueueCount;
begin
  FCS.BeginWrite;
  try
    Dec(FQueueCount);
    if FQueueCount < 0 then begin
      FQueueCount := 0;
    end;
  finally
    FCS.EndWrite;
  end;
end;

function TGlobalInternetState.GetQueueCount(): Integer;
begin
  FCS.BeginRead;
  try
    Result := FQueueCount;
  finally
    FCS.EndRead;
  end;
end;

end.
