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
  i_MarksDrawConfig,
  u_BaseInterfacedObject;

type
  TMarksDrawConfigStatic = class(TBaseInterfacedObject, IMarksDrawConfigStatic)
  private
    FShowPointCaption: Boolean;
    FUseSolidCaptionBackground: Boolean;
    FUseSimpleDrawOrder: Boolean;
    FOverSizeRect: TRect;
  private
    function GetShowPointCaption: Boolean;
    function GetUseSolidCaptionBackground: Boolean;
    function GetUseSimpleDrawOrder: Boolean;
    function GetOverSizeRect: TRect;
  public
    constructor Create(
      AShowPointCaption: Boolean;
      AUseSolidCaptionBackground: Boolean;
      AUseSimpleDrawOrder: Boolean;
      AOverSizeRect: TRect
    );
  end;

implementation

{ TMarksDrawConfigStatic }

constructor TMarksDrawConfigStatic.Create(
  AShowPointCaption: Boolean;
  AUseSolidCaptionBackground: Boolean;
  AUseSimpleDrawOrder: Boolean;
  AOverSizeRect: TRect
);
begin
  inherited Create;
  FShowPointCaption := AShowPointCaption;
  FUseSolidCaptionBackground := AUseSolidCaptionBackground;
  FUseSimpleDrawOrder := AUseSimpleDrawOrder;
  FOverSizeRect := AOverSizeRect;
end;

function TMarksDrawConfigStatic.GetOverSizeRect: TRect;
begin
  Result := FOverSizeRect;
end;

function TMarksDrawConfigStatic.GetShowPointCaption: Boolean;
begin
  Result := FShowPointCaption;
end;

function TMarksDrawConfigStatic.GetUseSimpleDrawOrder: Boolean;
begin
  Result := FUseSimpleDrawOrder;
end;

function TMarksDrawConfigStatic.GetUseSolidCaptionBackground: Boolean;
begin
  Result := FUseSolidCaptionBackground;
end;

end.
