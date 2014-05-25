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

unit u_BenchmarkItemSyncWrite;

interface

uses
  SysUtils,
  u_BenchmarkItemBase;

type
  TBenchmarkItemSyncWrite = class(TBenchmarkItemBase)
  private
    FSync: IReadWriteSync;
    FData: Integer;
  protected
    function RunOneStep: Integer; override;
  public
    constructor Create(
      const ASyncName: string;
      const ASync: IReadWriteSync
    );
  end;

implementation

const CRepeatCount = 1000;

{ TBenchmarkItemSyncWrite }

constructor TBenchmarkItemSyncWrite.Create(
  const ASyncName: string;
  const ASync: IReadWriteSync
);
begin
  inherited Create(
    Assigned(ASync),
    'Sync ' + ASyncName + ' Write Lock',
    CRepeatCount
  );
  FSync := ASync;
end;

function TBenchmarkItemSyncWrite.RunOneStep: Integer;
var
  i: Integer;
begin
  for i := 0 to CRepeatCount - 1 do begin
    FSync.BeginWrite;
    try
      Inc(FData);
    finally
      FSync.EndWrite;
    end;
  end;
  Result := FData;
end;

end.
