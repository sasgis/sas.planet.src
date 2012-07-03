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

unit u_NotifierTTLCheck;

interface

uses
  Windows,
  Classes,
  SysUtils,
  i_ListenerTTLCheck,
  i_NotifierTTLCheck;

type
  TTTLCheckNotifier = class(TInterfacedObject, INotifierTTLCheck)
  private
    FList: TList;
    FSync: IReadWriteSync;
    FNextCheck: Cardinal;
  private
    procedure Add(const AListener: IListenerTTLCheck);
    procedure Remove(const AListener: IListenerTTLCheck);
    procedure ProcessObjectsTrim;
    function GetNextCheck: Cardinal;
  public
    constructor Create;
    destructor Destroy; override;
  end;

implementation

uses
  u_Synchronizer;

{ TTTLCheckNotifier }

constructor TTTLCheckNotifier.Create;
begin
  inherited Create;
  FSync := MakeSyncRW_Big(Self, False);
  FList := TList.Create;
end;

destructor TTTLCheckNotifier.Destroy;
var
  i: integer;
begin
  for i := 0 to FList.Count - 1 do begin
    IListenerTTLCheck(FList.Items[i])._Release;
  end;
  FreeAndNil(FList);
  inherited;
end;

procedure TTTLCheckNotifier.Add(const AListener: IListenerTTLCheck);
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
  VObj: IListenerTTLCheck;
  VNextCheck: Cardinal;
  VObjNextCheck: Cardinal;
begin
  VNow := GetTickCount;
  VNextCheck := 0;
  FSync.BeginRead;
  try
    for i := 0 to FList.Count - 1 do begin
      VObj := IListenerTTLCheck(FList.Items[i]);
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

procedure TTTLCheckNotifier.Remove(const AListener: IListenerTTLCheck);
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

