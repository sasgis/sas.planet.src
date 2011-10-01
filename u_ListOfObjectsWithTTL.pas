{******************************************************************************}
{* SAS.Planet (SAS.Планета)                                                   *}
{* Copyright (C) 2007-2011, SAS.Planet development team.                      *}
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

unit u_ListOfObjectsWithTTL;

interface

uses
  Windows,
  Classes,
  SysUtils,
  i_ObjectWithTTL,
  i_ListOfObjectsWithTTL;

type
  TListOfObjectsWithTTL = class(TInterfacedObject, IListOfObjectsWithTTL)
  private
    FList: TList;
    FSync: IReadWriteSync;
    FNextCheck: Cardinal;
  public
    constructor Create;
    destructor Destroy; override;
    procedure AddObject(AObj: IObjectWithTTL);
    procedure RemoveObject(AObj: IObjectWithTTL);
    procedure ProcessObjectsTrim;
    function GetNextCheck: Cardinal;
  end;

implementation

{ TListOfObjectsWithTTL }

procedure TListOfObjectsWithTTL.AddObject(AObj: IObjectWithTTL);
begin
  FSync.BeginWrite;
  try
    AObj._AddRef;
    FList.Add(Pointer(AObj));
  finally
    FSync.EndWrite;
  end;
end;

constructor TListOfObjectsWithTTL.Create;
begin
  FSync := TMultiReadExclusiveWriteSynchronizer.Create;
  FList := TList.Create;
end;

destructor TListOfObjectsWithTTL.Destroy;
var
  i: integer;
begin
  FSync := nil;
  for i := 0 to FList.Count - 1 do begin
    IObjectWithTTL(FList.Items[i])._Release;
  end;
  FreeAndNil(FList);
  inherited;
end;

function TListOfObjectsWithTTL.GetNextCheck: Cardinal;
begin
  Result := FNextCheck;
end;

procedure TListOfObjectsWithTTL.ProcessObjectsTrim;
var
  i: integer;
  VNow: Cardinal;
  VObj: IObjectWithTTL;
  VNextCheck: Cardinal;
  VObjNextCheck: Cardinal;
begin
  VNow := GetTickCount;
  VNextCheck := 0;
  FSync.BeginRead;
  try
    for i := 0 to FList.Count - 1 do begin
      VObj := IObjectWithTTL(FList.Items[i]);
      VObjNextCheck := VObj.GetNextCheckTime;
      if (VObjNextCheck <= VNow) or ((VNow < 1 shl 29) and (VObjNextCheck > 1 shl 30)) then begin
        VObj.TrimByTTL;
        VObjNextCheck := VObj.GetNextCheckTime;
      end;
      if (VNextCheck <= 0) or (VNextCheck > VObjNextCheck) then begin
        VNextCheck := VObjNextCheck;
      end;
    end;
    FNextCheck := VNextCheck;
  finally
    FSync.EndRead;
  end;
end;

procedure TListOfObjectsWithTTL.RemoveObject(AObj: IObjectWithTTL);
begin
  FSync.BeginWrite;
  try
    FList.Remove(Pointer(AObj));
    AObj._Release;
  finally
    FSync.EndWrite;
  end;
end;

end.
