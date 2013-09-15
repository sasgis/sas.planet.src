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

unit u_MarksDrawConfigStatic;

interface

uses
  Types,
  t_Hash,
  i_MarksDrawConfig,
  u_BaseInterfacedObject;

type
  TCaptionDrawConfigStatic = class(TBaseInterfacedObject, ICaptionDrawConfigStatic)
  private
    FShowPointCaption: Boolean;
    FUseSolidCaptionBackground: Boolean;
  private
    function GetHash: THashValue;
    function GetShowPointCaption: Boolean;
    function GetUseSolidCaptionBackground: Boolean;
  public
    constructor Create(
      AShowPointCaption: Boolean;
      AUseSolidCaptionBackground: Boolean
    );
  end;

  TMarksDrawConfigStatic = class(TBaseInterfacedObject, IMarksDrawConfigStatic)
  private
    FCaptionDrawConfig: ICaptionDrawConfigStatic;
    FUseSimpleDrawOrder: Boolean;
    FOverSizeRect: TRect;
  private
    function GetCaptionDrawConfig: ICaptionDrawConfigStatic;
    function GetUseSimpleDrawOrder: Boolean;
    function GetOverSizeRect: TRect;
  public
    constructor Create(
      const ACaptionDrawConfig: ICaptionDrawConfigStatic;
      AUseSimpleDrawOrder: Boolean;
      AOverSizeRect: TRect
    );
  end;

implementation

{ TMarksDrawConfigStatic }

constructor TMarksDrawConfigStatic.Create(
  const ACaptionDrawConfig: ICaptionDrawConfigStatic;
  AUseSimpleDrawOrder: Boolean;
  AOverSizeRect: TRect
);
begin
  inherited Create;
  FCaptionDrawConfig := ACaptionDrawConfig;
  FUseSimpleDrawOrder := AUseSimpleDrawOrder;
  FOverSizeRect := AOverSizeRect;
end;

function TMarksDrawConfigStatic.GetCaptionDrawConfig: ICaptionDrawConfigStatic;
begin
  Result := FCaptionDrawConfig;
end;

function TMarksDrawConfigStatic.GetOverSizeRect: TRect;
begin
  Result := FOverSizeRect;
end;

function TMarksDrawConfigStatic.GetUseSimpleDrawOrder: Boolean;
begin
  Result := FUseSimpleDrawOrder;
end;

{ TCaptionDrawConfigStatic }

constructor TCaptionDrawConfigStatic.Create(
  AShowPointCaption,
  AUseSolidCaptionBackground: Boolean
);
begin
  inherited Create;
  FShowPointCaption := AShowPointCaption;
  FUseSolidCaptionBackground := AUseSolidCaptionBackground;
end;

function TCaptionDrawConfigStatic.GetHash: THashValue;
var
  VHash: THashValue;
begin
  VHash := $162e192b2957163d;
  if not FShowPointCaption then begin
    VHash := not VHash;
  end;
  Result := VHash;
  VHash := $f3786a4b25827c1;
  if not FUseSolidCaptionBackground then begin
    VHash := not VHash;
  end;
  Result := Result xor VHash;
end;

function TCaptionDrawConfigStatic.GetShowPointCaption: Boolean;
begin
  Result := FShowPointCaption;
end;

function TCaptionDrawConfigStatic.GetUseSolidCaptionBackground: Boolean;
begin
  Result := FUseSolidCaptionBackground;
end;

end.
