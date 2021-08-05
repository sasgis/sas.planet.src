{******************************************************************************}
{* This file is part of SAS.Planet project.                                   *}
{*                                                                            *}
{* Copyright (C) 2007-2021, SAS.Planet development team.                      *}
{*                                                                            *}
{* SAS.Planet is free software: you can redistribute it and/or modify         *}
{* it under the terms of the GNU General Public License as published by       *}
{* the Free Software Foundation, either version 3 of the License, or          *}
{* (at your option) any later version.                                        *}
{*                                                                            *}
{* SAS.Planet is distributed in the hope that it will be useful,              *}
{* but WITHOUT ANY WARRANTY; without even the implied warranty of             *}
{* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the               *}
{* GNU General Public License for more details.                               *}
{*                                                                            *}
{* You should have received a copy of the GNU General Public License          *}
{* along with SAS.Planet. If not, see <http://www.gnu.org/licenses/>.         *}
{*                                                                            *}
{* https://github.com/sasgis/sas.planet.src                                   *}
{******************************************************************************}

unit u_TileStorageSQLiteErrorLogger;

interface

uses
  Windows,
  SysUtils,
  i_TileStorageSQLiteErrorLogger,
  u_BaseInterfacedObject;

type
  TErrorLoggerGetFileNameEvent = procedure (
    var AFullLogFilename: String;
    var AForbidden: Boolean
  ) of object;

  TErrorLoggerToFile = class(TBaseInterfacedObject, IErrorLoggerToFile)
  private
    FFilename: String;
    FEvent: TErrorLoggerGetFileNameEvent;
    // internal
    FLogSync: IReadWriteSync;
    FLogFile: THandle;
  private
    procedure InternalOpenLog;
    procedure InternalCloseLog;
  private
    { IErrorLoggerToFile }
    procedure LogString(const AValue: String);
    procedure Close;
  public
    constructor Create(
      const AFilename: String;
      const AEvent: TErrorLoggerGetFileNameEvent
    );
    destructor Destroy; override;
  end;

implementation

uses
  NTFiles,
  u_Synchronizer;

{ TErrorLoggerToFile }

procedure TErrorLoggerToFile.Close;
begin
  FLogSync.BeginWrite;
  try
    InternalCloseLog;
  finally
    FLogSync.EndWrite;
  end;
end;

constructor TErrorLoggerToFile.Create(
  const AFilename: String;
  const AEvent: TErrorLoggerGetFileNameEvent
);
begin
  Assert(
    (0 < Length(AFilename))
    or
    (Assigned(AEvent))
  );

  inherited Create;

  FFilename := AFilename;
  FEvent := AEvent;
  FLogSync := GSync.SyncStd.Make(Self.ClassName); // spinlock(4096)
  FLogFile := 0;
end;

destructor TErrorLoggerToFile.Destroy;
begin
  Close;
  inherited;
end;

procedure TErrorLoggerToFile.InternalCloseLog;
begin
  if (FLogFile <> 0) then begin
    CloseHandle(FLogFile);
    FLogFile := 0;
  end;
end;

procedure TErrorLoggerToFile.InternalOpenLog;
var
  VFileName: String;
  VForbidden: Boolean;
begin
  if (0 = FLogFile) then begin
    // try to open log file
    VFileName := FFilename;
    if (0 < Length(VFileName)) then begin
      // use it
    end else begin
      // get by event
      if Assigned(FEvent) then begin
        VForbidden := False;
        FEvent(VFileName, VForbidden);
        if VForbidden or (0 = Length(VFileName)) then
          Exit;
      end else begin
        // neither filename nor event
        Exit;
      end;
    end;

    // open
    FLogFile := CreateOrOpenFileWriteOnly(VFileName, False);
    if (INVALID_HANDLE_VALUE = FLogFile) then begin
      FLogFile := 0;
    end;
  end;
end;

procedure TErrorLoggerToFile.LogString(const AValue: String);
var
  VNumberOfBytesWritten: DWORD;
begin
  FLogSync.BeginWrite;
  try
    InternalOpenLog;
    if (0 <> FLogFile) then begin
      SetFilePointer(FLogFile, 0, nil, FILE_END);
      WriteFile(FLogFile, AValue[1], Length(AValue)*SizeOf(Char), VNumberOfBytesWritten, nil);
    end;
  finally
    FLogSync.EndWrite;
  end;
end;

end.