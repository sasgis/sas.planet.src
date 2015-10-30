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

unit u_TileStorageAbilities;

interface

uses
  i_TileStorageAbilities,
  u_BaseInterfacedObject;

type
  TTileStorageAbilities = class(TBaseInterfacedObject, ITileStorageAbilities)
  private
    FIsReadOnly: Boolean;
    FAllowRead: Boolean;
    FAllowAdd: Boolean;
    FAllowDelete: Boolean;
    FAllowReplace: Boolean;
    FAllowScan: boolean;
  private
    function GetAllowRead: Boolean;
    function GetIsReadOnly: Boolean;
    function GetAllowAdd: Boolean;
    function GetAllowDelete: Boolean;
    function GetAllowReplace: Boolean;
    function GetAllowScan: boolean;
  public
    constructor Create(
      const AIsReadOnly: Boolean;
      const AAllowRead: Boolean;
      const AAllowScan: boolean;
      const AAllowAdd: Boolean;
      const AAllowDelete: Boolean;
      const AAllowReplace: Boolean
    );
  end;

  TTileStorageAbilitiesNoAccess = class(TBaseInterfacedObject, ITileStorageAbilities)
  private
    function GetAllowRead: Boolean;
    function GetIsReadOnly: Boolean;
    function GetAllowAdd: Boolean;
    function GetAllowDelete: Boolean;
    function GetAllowReplace: Boolean;
    function GetAllowScan: boolean;
  end;

  TTileStorageTypeAbilities = class(TBaseInterfacedObject, ITileStorageTypeAbilities)
  private
    FBaseStorageAbilities: ITileStorageAbilities;
    FVersionSupport: TTileStorageTypeVersionSupport;
    FStorageClass: TTileStorageTypeClass;
    FSupportDifferentContentTypes: Boolean;
  private
    function GetBaseStorageAbilities: ITileStorageAbilities;
    function GetVersionSupport: TTileStorageTypeVersionSupport;
    function GetStorageClass: TTileStorageTypeClass;
    function GetSupportDifferentContentTypes: Boolean;
  public
    constructor Create(
      const ABaseStorageAbilities: ITileStorageAbilities;
      const AVersionSupport: TTileStorageTypeVersionSupport;
      const ASupportDifferentContentTypes: Boolean;
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
  public
    constructor Create;
  end;

implementation

{ TTileStorageAbilities }

constructor TTileStorageAbilities.Create(
  const AIsReadOnly: Boolean;
  const AAllowRead: Boolean;
  const AAllowScan: boolean;
  const AAllowAdd: Boolean;
  const AAllowDelete: Boolean;
  const AAllowReplace: Boolean
);
begin
  inherited Create;
  FAllowRead := AAllowRead;
  FAllowScan := AAllowScan;
  FIsReadOnly := AIsReadOnly or not (AAllowAdd or AAllowDelete or AAllowReplace);
  FAllowAdd := AAllowAdd and not FIsReadOnly;
  FAllowDelete := AAllowDelete and not FIsReadOnly;
  FAllowReplace := AAllowReplace and not FIsReadOnly;
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

function TTileStorageAbilities.GetAllowScan: boolean;
begin
  Result := FAllowScan;
end;

function TTileStorageAbilities.GetIsReadOnly: Boolean;
begin
  Result := FIsReadOnly;
end;

{ TTileStorageTypeAbilities }

constructor TTileStorageTypeAbilities.Create(
  const ABaseStorageAbilities: ITileStorageAbilities;
  const AVersionSupport: TTileStorageTypeVersionSupport;
  const ASupportDifferentContentTypes: Boolean;
  const AStorageClass: TTileStorageTypeClass
);
begin
  inherited Create;
  FBaseStorageAbilities := ABaseStorageAbilities;
  FVersionSupport := AVersionSupport;
  FStorageClass := AStorageClass;
  FSupportDifferentContentTypes := ASupportDifferentContentTypes;
end;

function TTileStorageTypeAbilities.GetBaseStorageAbilities: ITileStorageAbilities;
begin
  Result := FBaseStorageAbilities;
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

function TTileStorageAbilitiesNoAccess.GetAllowScan: boolean;
begin
  Result := False;
end;

function TTileStorageAbilitiesNoAccess.GetIsReadOnly: Boolean;
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
