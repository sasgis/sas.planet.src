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

unit u_TTLCheckNotifier;

interface

uses
  Windows,
  Classes,
  SysUtils,
  i_TTLCheckListener,
  i_TTLCheckNotifier;

type
  TTTLCheckNotifier = class(TInterfacedObject, ITTLCheckNotifier)
  private
    FList: TList;
    FSync: IReadWriteSync;
    FNextCheck: Cardinal;
  private
    procedure Add(const AListener: ITTLCheckListener);
    procedure Remove(const AListener: ITTLCheckListener);
    procedure ProcessObjectsTrim;
    function GetNextCheck: Cardinal;
  public
    constructor Create;
    destructor Destroy; override;
  end;

implementation

{ TTTLCheckNotifier }

constructor TTTLCheckNotifier.Create;
begin
  inherited Create;
  FSync := TMultiReadExclusiveWriteSynchronizer.Create;
  FList := TList.Create;
end;

destructor TTTLCheckNotifier.Destroy;
var
  i: integer;
begin
  FSync := nil;
  for i := 0 to FList.Count - 1 do begin
    ITTLCheckListener(FList.Items[i])._Release;
  end;
  FreeAndNil(FList);
  inherited;
end;

procedure TTTLCheckNotifier.Add(const AListener: ITTLCheckListener);
begin
  FSync.BeginWrite;
  try
    AListener._AddRef;
    FList.Add(Pointer(AListener));
  finally
    FSync.EndWrite;
  end;
end;

function TTTLCheckNotifier.GetNextCheck: Cardinal;
begin
  Result := FNextCheck;
end;

procedure TTTLCheckNotifier.ProcessObjectsTrim;
var
  i: integer;
  VNow: Cardinal;
  VObj: ITTLCheckListener;
  VNextCheck: Cardinal;
  VObjNextCheck: Cardinal;
begin
  VNow := GetTickCount;
  VNextCheck := 0;
  FSync.BeginRead;
  try
    for i := 0 to FList.Count - 1 do begin
      VObj := ITTLCheckListener(FList.Items[i]);
      VObjNextCheck := VObj.CheckTTLAndGetNextCheckTime(VNow);
      if (VNextCheck <= 0) or (VNextCheck > VObjNextCheck) then begin
        VNextCheck := VObjNextCheck;
      end;
    end;
    FNextCheck := VNextCheck;
  finally
    FSync.EndRead;
  end;
end;

procedure TTTLCheckNotifier.Remove(const AListener: ITTLCheckListener);
begin
  FSync.BeginWrite;
  try
    FList.Remove(Pointer(AListener));
    AListener._Release;
  finally
    FSync.EndWrite;
  end;
end;

end.
