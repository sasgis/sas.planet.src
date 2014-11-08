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

unit u_ReadWriteSyncCriticalSection;

interface

uses
  Windows,
  SysUtils,
  i_ReadWriteSyncFactory;

type
  TSynchronizerCS = class(TInterfacedObject, IReadWriteSync)
  private
    FLock: TRTLCriticalSection;
  protected
    { IReadWriteSync }
    procedure BeginRead;
    procedure EndRead;
    function BeginWrite: Boolean;
    procedure EndWrite;
  public
    constructor Create;
    destructor Destroy; override;
  end;

  TSynchronizerCSSC = class(TInterfacedObject, IReadWriteSync)
  private
    FLock: TRTLCriticalSection;
  protected
    { IReadWriteSync }
    procedure BeginRead;
    procedure EndRead;
    function BeginWrite: Boolean;
    procedure EndWrite;
  public
    constructor Create(const ASpinCount: Cardinal);
    destructor Destroy; override;
  end;

  TSynchronizerCSFactory = class(TInterfacedObject, IReadWriteSyncFactory)
  private
    function Make(const AName: string): IReadWriteSync;
  end;

  TSynchronizerCSSCFactory = class(TInterfacedObject, IReadWriteSyncFactory)
  private
    FSpinCount: Cardinal;
  private
    function Make(const AName: string): IReadWriteSync;
  public
    constructor Create(const ASpinCount: Cardinal);
  end;

implementation

{ TSynchronizerCS }

constructor TSynchronizerCS.Create;
begin
  inherited Create;
  InitializeCriticalSection(FLock);
end;

destructor TSynchronizerCS.Destroy;
begin
  DeleteCriticalSection(FLock);
  inherited;
end;

procedure TSynchronizerCS.BeginRead;
begin
  EnterCriticalSection(FLock);
end;

function TSynchronizerCS.BeginWrite: Boolean;
begin
  EnterCriticalSection(FLock);
  Result := True;
end;

procedure TSynchronizerCS.EndRead;
begin
  LeaveCriticalSection(FLock);
end;

procedure TSynchronizerCS.EndWrite;
begin
  LeaveCriticalSection(FLock);
end;

{ TSynchronizerCSSC }

constructor TSynchronizerCSSC.Create(
  const ASpinCount: Cardinal
);
begin
  inherited Create;
  InitializeCriticalSectionAndSpinCount(FLock, ASpinCount);
end;

destructor TSynchronizerCSSC.Destroy;
begin
  DeleteCriticalSection(FLock);
  inherited;
end;

procedure TSynchronizerCSSC.BeginRead;
begin
  EnterCriticalSection(FLock);
end;

function TSynchronizerCSSC.BeginWrite: Boolean;
begin
  EnterCriticalSection(FLock);
  Result := TRUE;
end;

procedure TSynchronizerCSSC.EndRead;
begin
  LeaveCriticalSection(FLock);
end;

procedure TSynchronizerCSSC.EndWrite;
begin
  LeaveCriticalSection(FLock);
end;

{ TSynchronizerCSFactory }

function TSynchronizerCSFactory.Make(const AName: string): IReadWriteSync;
begin
  Result := TSynchronizerCS.Create;
end;

{ TSynchronizerCSSCFactory }

constructor TSynchronizerCSSCFactory.Create(const ASpinCount: Cardinal);
begin
  inherited Create;
  FSpinCount := ASpinCount;
end;

function TSynchronizerCSSCFactory.Make(const AName: string): IReadWriteSync;
begin
  Result := TSynchronizerCSSC.Create(FSpinCount);
end;

end.
