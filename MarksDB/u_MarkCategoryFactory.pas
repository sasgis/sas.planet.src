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

unit u_MarkCategoryFactory;

interface

uses
  i_MarkCategory,
  i_MarkCategoryFactoryConfig,
  i_MarkCategoryFactory,
  u_BaseInterfacedObject;

type
  TMarkCategoryFactory = class(TBaseInterfacedObject, IMarkCategoryFactory)
  private
    FConfig: IMarkCategoryFactoryConfig;
  private
    function CreateNew(const AName: string): IMarkCategory;
    function Modify(
      const ASource: IMarkCategory;
      const AName: string;
      AVisible: Boolean;
      AAfterScale: integer;
      ABeforeScale: integer
    ): IMarkCategory;
    function ModifyVisible(
      const ASource: IMarkCategory;
      AVisible: Boolean
    ): IMarkCategory;
    function GetConfig: IMarkCategoryFactoryConfig;
  public
    constructor Create(
      const AConfig: IMarkCategoryFactoryConfig
    );
  end;

implementation

uses
  StrUtils,
  u_MarkCategory;

{ TMarkCategoryFactory }

constructor TMarkCategoryFactory.Create(
  const AConfig: IMarkCategoryFactoryConfig
);
begin
  inherited Create;
  FConfig := AConfig;
end;

function TMarkCategoryFactory.CreateNew(const AName: string): IMarkCategory;
var
  VName: string;
  VAfterScale, VBeforeScale: Integer;
begin
  VName := AName;
  FConfig.LockRead;
  try
    if VName = '' then begin
      VName := FConfig.DefaultName.Value;
    end else if RightStr(VName, 1) = '\' then begin
      VName := VName + FConfig.DefaultName.Value;
    end;

    VAfterScale := FConfig.AfterScale;
    VBeforeScale := FConfig.BeforeScale;
  finally
    FConfig.UnlockRead;
  end;

  Result :=
    TMarkCategory.Create(
      VName,
      True,
      VAfterScale,
      VBeforeScale
    );
end;

function TMarkCategoryFactory.GetConfig: IMarkCategoryFactoryConfig;
begin
  Result := FConfig;
end;

function TMarkCategoryFactory.Modify(
  const ASource: IMarkCategory;
  const AName: string;
  AVisible: Boolean;
  AAfterScale, ABeforeScale: integer
): IMarkCategory;
var
  VName: string;
begin
  VName := AName;
  FConfig.LockRead;
  try
    if VName = '' then begin
      VName := FConfig.DefaultName.Value;
    end;
  finally
    FConfig.UnlockRead;
  end;

  Result :=
    TMarkCategory.Create(
      VName,
      AVisible,
      AAfterScale,
      ABeforeScale
    );
end;

function TMarkCategoryFactory.ModifyVisible(
  const ASource: IMarkCategory;
  AVisible: Boolean
): IMarkCategory;
begin
  Result :=
    TMarkCategory.Create(
      ASource.Name,
      AVisible,
      ASource.AfterScale,
      ASource.BeforeScale
    );
end;

end.
