{******************************************************************************}
{* SAS.Planet (SAS.Планета)                                                   *}
{* Copyright (C) 2007-2019, SAS.Planet development team.                      *}
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

unit u_TileStorageAbilities;

interface

uses
  t_CommonTypes,
  i_TileStorageAbilities,
  u_BaseInterfacedObject;

type
  TTileStorageAbilitiesType = (tsatRead, tsatScan, tsatAdd, tsatDelete, tsatReplace);
  TTileStorageAbilitiesTypes = set of TTileStorageAbilitiesType;

const
  CTileStorageReadOnly: TTileStorageAbilitiesTypes = [tsatRead, tsatScan];
  CTileStorageWriteOnly: TTileStorageAbilitiesTypes = [tsatAdd, tsatDelete, tsatReplace];
  CTileStorageReadWrite: TTileStorageAbilitiesTypes = [tsatRead, tsatScan, tsatAdd, tsatDelete, tsatReplace];

type
  TTileStorageAbilities = class(TBaseInterfacedObject, ITileStorageAbilities)
  private
    FAllowRead: Boolean;
    FAllowScan: Boolean;
    FAllowAdd: Boolean;
    FAllowDelete: Boolean;
    FAllowReplace: Boolean;
  private
    { ITileStorageAbilities }
    function GetAllowRead: Boolean;
    function GetAllowScan: Boolean;
    function GetAllowAdd: Boolean;
    function GetAllowDelete: Boolean;
    function GetAllowReplace: Boolean;
    function IsReadOnly: Boolean;
  public
    constructor Create(
      const AAllowRead: Boolean;
      const AAllowScan: Boolean;
      const AAllowAdd: Boolean;
      const AAllowDelete: Boolean;
      const AAllowReplace: Boolean
    ); overload;

    constructor Create(
      const AAbilitiesTypes: TTileStorageAbilitiesTypes
    ); overload;
  end;

  TTileStorageAbilitiesNoAccess = class(TBaseInterfacedObject, ITileStorageAbilities)
  private
    function GetAllowRead: Boolean;
    function GetAllowScan: Boolean;
    function GetAllowAdd: Boolean;
    function GetAllowDelete: Boolean;
    function GetAllowReplace: Boolean;
    function IsReadOnly: Boolean;
  end;

  TTileStorageTypeAbilities = class(TBaseInterfacedObject, ITileStorageTypeAbilities)
  private
    FBaseStorageAbilities: ITileStorageAbilities;
    FVersionSupport: TTileStorageTypeVersionSupport;
    FStorageClass: TTileStorageTypeClass;
    FSupportDifferentContentTypes: Boolean;
    FPathStringSupport: TStringTypeSupport;
  private
    function GetBaseStorageAbilities: ITileStorageAbilities;
    function GetVersionSupport: TTileStorageTypeVersionSupport;
    function GetStorageClass: TTileStorageTypeClass;
    function GetSupportDifferentContentTypes: Boolean;
    function GetPathStringSupport: TStringTypeSupport;
  public
    constructor Create(
      const ABaseStorageAbilities: ITileStorageAbilities;
      const AVersionSupport: TTileStorageTypeVersionSupport;
      const ASupportDifferentContentTypes: Boolean;
      const APathStringSupport: TStringTypeSupport;
      const AStorageClass: TTileStorageTypeClass
    );
  end;

  TTileStorageTypeAbilitiesNoAccess = class(TBaseInterfacedObject, ITileStorageTypeAbilities)
  private
    FBaseStorageAbilities: ITileStorageAbilities;
  private
    function GetBaseStorageAbilities: ITileStorageAbilities;
    function GetVersionSupport: TTileStorageTypeVersionSupport;
    function GetStorageClass: TTileStorageTypeClass;
    function GetSupportDifferentContentTypes: Boolean;
    function GetPathStringSupport: TStringTypeSupport;
  public
    constructor Create;
  end;

implementation

{ TTileStorageAbilities }

constructor TTileStorageAbilities.Create(
  const AAllowRead: Boolean;
  const AAllowScan: Boolean;
  const AAllowAdd: Boolean;
  const AAllowDelete: Boolean;
  const AAllowReplace: Boolean
);
begin
  inherited Create;
  FAllowRead := AAllowRead;
  FAllowScan := AAllowScan;
  FAllowAdd := AAllowAdd;
  FAllowDelete := AAllowDelete;
  FAllowReplace := AAllowReplace;
end;

constructor TTileStorageAbilities.Create(
  const AAbilitiesTypes: TTileStorageAbilitiesTypes
);
begin
  inherited Create;

  // Read Abilities
  if tsatRead in AAbilitiesTypes then begin
    FAllowRead := True;
  end;
  if tsatScan in AAbilitiesTypes then begin
    FAllowScan := True;
  end;

  // Write Abilities
  if tsatAdd in AAbilitiesTypes then begin
    FAllowAdd := True;
  end;
  if tsatDelete in AAbilitiesTypes then begin
    FAllowDelete := True;
  end;
  if tsatReplace in AAbilitiesTypes then begin
    FAllowReplace := True;
  end;
end;

function TTileStorageAbilities.GetAllowAdd: Boolean;
begin
  Result := FAllowAdd;
end;

function TTileStorageAbilities.GetAllowDelete: Boolean;
begin
  Result := FAllowDelete;
end;

function TTileStorageAbilities.GetAllowRead: Boolean;
begin
  Result := FAllowRead;
end;

function TTileStorageAbilities.GetAllowReplace: Boolean;
begin
  Result := FAllowReplace;
end;

function TTileStorageAbilities.GetAllowScan: Boolean;
begin
  Result := FAllowScan;
end;

function TTileStorageAbilities.IsReadOnly: Boolean;
begin
  Result := not FAllowAdd and not FAllowDelete and not FAllowReplace;
end;

{ TTileStorageTypeAbilities }

constructor TTileStorageTypeAbilities.Create(
  const ABaseStorageAbilities: ITileStorageAbilities;
  const AVersionSupport: TTileStorageTypeVersionSupport;
  const ASupportDifferentContentTypes: Boolean;
  const APathStringSupport: TStringTypeSupport;
  const AStorageClass: TTileStorageTypeClass
);
begin
  inherited Create;
  FBaseStorageAbilities := ABaseStorageAbilities;
  FVersionSupport := AVersionSupport;
  FStorageClass := AStorageClass;
  FSupportDifferentContentTypes := ASupportDifferentContentTypes;
  FPathStringSupport := APathStringSupport;
end;

function TTileStorageTypeAbilities.GetBaseStorageAbilities: ITileStorageAbilities;
begin
  Result := FBaseStorageAbilities;
end;

function TTileStorageTypeAbilities.GetPathStringSupport: TStringTypeSupport;
begin
  Result := FPathStringSupport;
end;

function TTileStorageTypeAbilities.GetStorageClass: TTileStorageTypeClass;
begin
  Result := FStorageClass;
end;

function TTileStorageTypeAbilities.GetSupportDifferentContentTypes: Boolean;
begin
  Result := FSupportDifferentContentTypes;
end;

function TTileStorageTypeAbilities.GetVersionSupport: TTileStorageTypeVersionSupport;
begin
  Result := FVersionSupport;
end;

{ TTileStorageAbilitiesNoAccess }

function TTileStorageAbilitiesNoAccess.GetAllowAdd: Boolean;
begin
  Result := False;
end;

function TTileStorageAbilitiesNoAccess.GetAllowDelete: Boolean;
begin
  Result := False;
end;

function TTileStorageAbilitiesNoAccess.GetAllowRead: Boolean;
begin
  Result := False;
end;

function TTileStorageAbilitiesNoAccess.GetAllowReplace: Boolean;
begin
  Result := False;
end;

function TTileStorageAbilitiesNoAccess.GetAllowScan: Boolean;
begin
  Result := False;
end;

function TTileStorageAbilitiesNoAccess.IsReadOnly: Boolean;
begin
  Result := True;
end;

{ TTileStorageTypeAbilitiesNoAccess }

constructor TTileStorageTypeAbilitiesNoAccess.Create;
begin
  inherited Create;
  FBaseStorageAbilities := TTileStorageAbilitiesNoAccess.Create;
end;

function TTileStorageTypeAbilitiesNoAccess.GetBaseStorageAbilities: ITileStorageAbilities;
begin
  Result := FBaseStorageAbilities;
end;

function TTileStorageTypeAbilitiesNoAccess.GetPathStringSupport: TStringTypeSupport;
begin
  Result := stsUnicode;
end;

function TTileStorageTypeAbilitiesNoAccess.GetStorageClass: TTileStorageTypeClass;
begin
  Result := tstcOther;
end;

function TTileStorageTypeAbilitiesNoAccess.GetSupportDifferentContentTypes: Boolean;
begin
  Result := False;
end;

function TTileStorageTypeAbilitiesNoAccess.GetVersionSupport: TTileStorageTypeVersionSupport;
begin
  Result := tstvsVersionIgnored;
end;

end.
