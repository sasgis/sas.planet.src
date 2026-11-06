{******************************************************************************}
{* This file is part of SAS.Planet project.                                   *}
{*                                                                            *}
{* Copyright (C) 2007-Present, SAS.Planet development team.                   *}
{*                                                                            *}
{* SAS.Planet is free software: you can redistribute it and/or modify         *}
{* it under the terms of the GNU General Public License as published by       *}
{* the Free Software Foundation, either version 3 of the License, or          *}
{* (at your option) any later version.                                        *}
{*                                                                            *}
{* SAS.Planet is distributed in the hope that it will be useful,              *}
{* but WITHOUT ANY WARRANTY; without even the implied warranty of             *}
{* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the               *}
{* GNU General Public License for more details.                               *}
{*                                                                            *}
{* You should have received a copy of the GNU General Public License          *}
{* along with SAS.Planet. If not, see <http://www.gnu.org/licenses/>.         *}
{*                                                                            *}
{* https://github.com/sasgis/sas.planet.src                                   *}
{******************************************************************************}

unit u_ShortCutSingleConfig;

interface

uses
  Classes,
  TB2Item,
  i_Bitmap32Static,
  i_Bitmap32BufferFactory,
  i_ConfigDataProvider,
  i_ConfigDataWriteProvider,
  i_ShortCutSingleConfig,
  u_ConfigDataElementBase;

type
  TShortCutSingleConfig = class(TConfigDataElementBase, IShortCutSingleConfig)
  private
    FIconBitmap: IBitmap32Static;
    FMenuItem: TTBCustomItem;
    FCaptionPrefix: string;
    FScope: TShortCutScope;
    FDefShortCut: TShortCut;
    FShortCut: TShortCut;
    FAppliedShortCut: TShortCut;
    function GetBitmap(
      const ABitmapFactory: IBitmap32StaticFactory;
      AMenu: TTBCustomItem
    ): IBitmap32Static;
    function GetActionShortCut: TShortCut;
    procedure ApplyToAction;
    procedure ApplyToMenuItem;
  protected
    procedure DoReadConfig(const AConfigData: IConfigDataProvider); override;
    procedure DoWriteConfig(const AConfigData: IConfigDataWriteProvider); override;
  private
    { IShortCutSingleConfig }
    function GetScope: TShortCutScope;
    function GetCaption: string;
    function GetIconBitmap: IBitmap32Static;
    function GetShortCut: TShortCut;
    procedure SetShortCut(AValue: TShortCut);
    procedure ResetToDefault;
    procedure ResetShortCut;
    procedure ApplyShortCut;
  public
    constructor Create(
      const ABitmapFactory: IBitmap32StaticFactory;
      AMenuItem: TTBCustomItem;
      const ACaptionPrefix: string;
      const AScope: TShortCutScope
    );
  end;

implementation

uses
  Types,
  Graphics,
  ActnList,
  GR32;

{ TShortCutSingleConfig }

constructor TShortCutSingleConfig.Create(
  const ABitmapFactory: IBitmap32StaticFactory;
  AMenuItem: TTBCustomItem;
  const ACaptionPrefix: string;
  const AScope: TShortCutScope
);
begin
  inherited Create;
  FMenuItem := AMenuItem;
  FCaptionPrefix := ACaptionPrefix;
  FScope := AScope;
  FDefShortCut := AMenuItem.ShortCut;
  FShortCut := FDefShortCut;
  FAppliedShortCut := FDefShortCut;
  FIconBitmap := GetBitmap(ABitmapFactory, AMenuItem);
end;

procedure TShortCutSingleConfig.DoReadConfig(
  const AConfigData: IConfigDataProvider
);
begin
  inherited;
  SetShortCut(AConfigData.ReadInteger(FMenuItem.Name, FShortCut));
end;

procedure TShortCutSingleConfig.DoWriteConfig(
  const AConfigData: IConfigDataWriteProvider
);
begin
  inherited;
  AConfigData.WriteInteger(FMenuItem.Name, FShortCut);
end;

function TShortCutSingleConfig.GetActionShortCut: TShortCut;
begin
  if FMenuItem.Action is TCustomAction then begin
    Result := TCustomAction(FMenuItem.Action).ShortCut;
  end else begin
    Result := 0;
  end;
end;

procedure TShortCutSingleConfig.ApplyToAction;
begin
  if FMenuItem.Action is TCustomAction then begin
    if TCustomAction(FMenuItem.Action).ShortCut <> FShortCut then begin
      TCustomAction(FMenuItem.Action).ShortCut := FShortCut;
      SetChanged;
    end;
  end;
end;

procedure TShortCutSingleConfig.ApplyToMenuItem;
var
  VShortCut: TShortCut;
begin
  VShortCut := FShortCut;
  if VShortCut = 0 then begin
    VShortCut := GetActionShortCut;
  end;
  if FMenuItem.ShortCut <> VShortCut then begin
    FMenuItem.ShortCut := VShortCut;
    SetChanged;
  end;
end;

procedure TShortCutSingleConfig.ApplyShortCut;
begin
  LockWrite;
  try
    FAppliedShortCut := FShortCut;
    // popup items always keep their hotkey separately from the main menu one,
    // even when both are linked to the same action
    if (FScope = scMainMenu) and Assigned(FMenuItem.Action) then begin
      ApplyToAction;
    end else begin
      ApplyToMenuItem;
    end;
  finally
    UnlockWrite;
  end;
end;

function TShortCutSingleConfig.GetBitmap(
  const ABitmapFactory: IBitmap32StaticFactory;
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

function TShortCutSingleConfig.GetScope: TShortCutScope;
begin
  Result := FScope;
end;

function TShortCutSingleConfig.GetCaption: string;
var
  I: Integer;
  VMenu: TTBCustomItem;
  VName: string;
begin
  Result := '';
  LockRead;
  try
    VMenu := FMenuItem;
    repeat
      VName := VMenu.Caption;
      I := Pos('&', VName);
      if I > 0 then begin
        Delete(VName, I, 1);
      end;
      if Result = '' then begin
        Result := VName;
      end else begin
        if VName <> '' then begin
          Result := VName + ' -> ' + Result;
        end;
      end;

      if VMenu.HasParent then begin
        VMenu := VMenu.Parent;
      end else begin
        VMenu := nil;
      end;
    until not Assigned(VMenu);

    if (Result <> '') and (FCaptionPrefix <> '') then begin
      Result := FCaptionPrefix + ' -> ' + Result;
    end;
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
    if FShortCut <> FAppliedShortCut then begin
      FShortCut := FAppliedShortCut;
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
