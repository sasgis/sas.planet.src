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

unit u_InternalDomainInfoProviderList;

interface

uses
  Classes,
  i_InternalDomainInfoProvider,
  u_BaseInterfacedObject;

type
  TInternalDomainInfoProviderList = class(TBaseInterfacedObject, IInternalDomainInfoProviderList)
  private
    FList: TStringList;
  private
    function GetByName(const AName: string): IInternalDomainInfoProvider;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Add(
      const AName: string;
      const ADomain: IInternalDomainInfoProvider
    );
  end;

implementation

uses
  SysUtils;

{ TInternalDomainInfoProviderList }

constructor TInternalDomainInfoProviderList.Create;
begin
  inherited Create;
  FList := TStringList.Create;
  FList.Sorted := True;
end;

destructor TInternalDomainInfoProviderList.Destroy;
var
  i: Integer;
  VItem: IInterface;
begin
  if Assigned(FList) then begin
    for i := 0 to FList.Count - 1 do begin
      VItem := IInterface(Pointer(FList.Objects[i]));
      FList.Objects[i] := nil;
      VItem._Release;
    end;
    FreeAndNil(FList);
  end;
  inherited;
end;

procedure TInternalDomainInfoProviderList.Add(
  const AName: string;
  const ADomain: IInternalDomainInfoProvider
);
var
  VIndex: Integer;
begin
  if not FList.Find(AName, VIndex) then begin
    ADomain._AddRef;
    FList.AddObject(AName, Pointer(ADomain));
  end;
end;

function TInternalDomainInfoProviderList.GetByName(
  const AName: string
): IInternalDomainInfoProvider;
var
  VIndex: Integer;
begin
  Result := nil;
  if FList.Find(AName, VIndex) then begin
    Result := IInternalDomainInfoProvider(Pointer(FList.Objects[VIndex]));
  end;
end;

end.
