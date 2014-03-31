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

unit u_ShortCutSingleConfig;

interface

uses
  Classes,
  TB2Item,
  i_Bitmap32Static,
  i_Bitmap32StaticFactory,
  i_ConfigDataProvider,
  i_ConfigDataWriteProvider,
  i_ShortCutSingleConfig,
  u_ConfigDataElementBase;

type
  TShortCutSingleConfig = class(TConfigDataElementBase, IShortCutSingleConfig)
  private
    FIconBitmap: IBitmap32Static;
    FMenuItem: TTBCustomItem;
    FDefShortCut: TShortCut;
    FShortCut: TShortCut;
    function GetBitmap(
      const ABitmapFactory: IBitmap32BufferFactory;
      AMenu: TTBCustomItem
    ): IBitmap32Static;
  protected
    procedure DoReadConfig(const AConfigData: IConfigDataProvider); override;
    procedure DoWriteConfig(const AConfigData: IConfigDataWriteProvider); override;
  private
    function GetCaption: String;
    function GetIconBitmap: IBitmap32Static;
    function GetShortCut: TShortCut;
    procedure SetShortCut(AValue: TShortCut);
    procedure ResetToDefault;
    procedure ResetShortCut;
    procedure ApplyShortCut;
  public
    constructor Create(
      const ABitmapFactory: IBitmap32BufferFactory;
      AMenuItem: TTBCustomItem
    );
  end;

implementation

uses
  Types,
  Graphics,
  GR32;

{ TShortCutSingleConfig }

constructor TShortCutSingleConfig.Create(
  const ABitmapFactory: IBitmap32BufferFactory;
  AMenuItem: TTBCustomItem
);
begin
  inherited Create;
  FMenuItem := AMenuItem;
  FDefShortCut := AMenuItem.ShortCut;
  FShortCut := FDefShortCut;
  FIconBitmap := GetBitmap(ABitmapFactory, AMenuItem);
end;

procedure TShortCutSingleConfig.DoReadConfig(
  const AConfigData: IConfigDataProvider
);
begin
  inherited;
  SetShortCut(AConfigData.ReadInteger(FMenuItem.name, FShortCut));
end;

procedure TShortCutSingleConfig.DoWriteConfig(
  const AConfigData: IConfigDataWriteProvider
);
begin
  inherited;
  AConfigData.WriteInteger(FMenuItem.Name, FMenuItem.ShortCut);
end;

procedure TShortCutSingleConfig.ApplyShortCut;
begin
  LockWrite;
  try
    if FMenuItem.ShortCut <> FShortCut then begin
      FMenuItem.ShortCut := FShortCut;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

function TShortCutSingleConfig.GetBitmap(
  const ABitmapFactory: IBitmap32BufferFactory;
  AMenu: TTBCustomItem
): IBitmap32Static;
var
  VBitmap: TBitmap32;
  VBmp: TBitmap;
begin
  Result := nil;
  if (AMenu.Images <> nil) and (AMenu.ImageIndex >= 0) then begin
    VBitmap := TBitmap32.Create;
    try
      VBmp := TBitmap.Create;
      try
        AMenu.Images.GetBitmap(AMenu.ImageIndex, VBmp);
        VBitmap.Assign(VBmp);
      finally
        VBmp.Free;
      end;
      Result :=
        ABitmapFactory.Build(
          Types.Point(VBitmap.Width, VBitmap.Height),
          VBitmap.Bits
        );
    finally
      VBitmap.Free;
    end;
  end;
end;

function TShortCutSingleConfig.GetCaption: String;
var
  Menu: TTBCustomItem;
  AddName: String;
begin
  Result := '';
  LockRead;
  try
    Menu := FMenuItem;
    repeat
      AddName := Menu.Caption;
      if Pos('&', AddName) <> 0 then begin
        Delete(AddName, Pos('&', AddName), 1);
      end;
      if Result = '' then begin
        Result := AddName;
      end else begin
        if AddName <> '' then begin
          Result := AddName + ' -> ' + Result;
        end;
      end;

      if Menu.HasParent then begin
        Menu := Menu.Parent;
      end else begin
        Menu := nil;
      end;
    until not Assigned(Menu)
  finally
    UnlockRead;
  end;
end;

function TShortCutSingleConfig.GetIconBitmap: IBitmap32Static;
begin
  Result := FIconBitmap;
end;

function TShortCutSingleConfig.GetShortCut: TShortCut;
begin
  LockRead;
  try
    Result := FShortCut;
  finally
    UnlockRead;
  end;
end;

procedure TShortCutSingleConfig.ResetShortCut;
begin
  LockWrite;
  try
    if FShortCut <> FMenuItem.ShortCut then begin
      FShortCut := FMenuItem.ShortCut;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

procedure TShortCutSingleConfig.ResetToDefault;
begin
  LockWrite;
  try
    if FShortCut <> FDefShortCut then begin
      FShortCut := FDefShortCut;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;

end;

procedure TShortCutSingleConfig.SetShortCut(AValue: TShortCut);
begin
  LockWrite;
  try
    if FShortCut <> AValue then begin
      FShortCut := AValue;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

end.
