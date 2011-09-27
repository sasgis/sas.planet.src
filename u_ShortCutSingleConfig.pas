{******************************************************************************}
{* SAS.Планета (SAS.Planet)                                                   *}
{* Copyright (C) 2007-2011, авторы программы SAS.Планета (SAS.Planet).        *}
{* Это программа является свободным программным обеспечением. Вы можете       *}
{* распространять и/или модифицировать её согласно условиям Стандартной       *}
{* Общественной Лицензии GNU, опубликованной Фондом Свободного Программного   *}
{* Обеспечения, версии 3. Эта программа распространяется в надежде, что она   *}
{* будет полезной, но БЕЗ ВСЯКИХ ГАРАНТИЙ, в том числе подразумеваемых        *}
{* гарантий ТОВАРНОГО СОСТОЯНИЯ ПРИ ПРОДАЖЕ и ГОДНОСТИ ДЛЯ ОПРЕДЕЛЁННОГО      *}
{* ПРИМЕНЕНИЯ. Смотрите Стандартную Общественную Лицензию GNU версии 3, для   *}
{* получения дополнительной информации. Вы должны были получить копию         *}
{* Стандартной Общественной Лицензии GNU вместе с программой. В случае её     *}
{* отсутствия, посмотрите http://www.gnu.org/licenses/.                       *}
{*                                                                            *}
{* http://sasgis.ru/sasplanet                                                 *}
{* az@sasgis.ru                                                               *}
{******************************************************************************}

unit u_ShortCutSingleConfig;

interface

uses
  Classes,
  Graphics,
  TB2Item,
  i_ConfigDataProvider,
  i_ConfigDataWriteProvider,
  i_ShortCutSingleConfig,
  u_ConfigDataElementBase;

type
  TShortCutSingleConfig = class(TConfigDataElementBase, IShortCutSingleConfig)
  private
    FIconBitmap: TBitmap;
    FMenuItem: TTBCustomItem;
    FDefShortCut: TShortCut;
    FShortCut: TShortCut;
    function GetBitmap(AMenu: TTBCustomItem): TBitmap;
  protected
    procedure DoReadConfig(AConfigData: IConfigDataProvider); override;
    procedure DoWriteConfig(AConfigData: IConfigDataWriteProvider); override;
  protected
    function GetCaption: String;
    function GetIconBitmap: TBitmap;
    function GetShortCut: TShortCut;
    procedure SetShortCut(AValue: TShortCut);
    procedure ResetToDefault;
    procedure ResetShortCut;
    procedure ApplyShortCut;
  public
    constructor Create(AMenuItem: TTBCustomItem);
    destructor Destroy; override;
  end;

implementation

uses
  SysUtils;

{ TShortCutSingleConfig }

constructor TShortCutSingleConfig.Create(AMenuItem: TTBCustomItem);
begin
  inherited Create;
  FMenuItem := AMenuItem;
  FDefShortCut := AMenuItem.ShortCut;
  FShortCut := FDefShortCut;
  FIconBitmap := GetBitmap(AMenuItem);
end;

destructor TShortCutSingleConfig.Destroy;
begin
  FreeAndNil(FIconBitmap);
  inherited;
end;

procedure TShortCutSingleConfig.DoReadConfig(AConfigData: IConfigDataProvider);
begin
  inherited;
  SetShortCut(AConfigData.ReadInteger(FMenuItem.name, FShortCut));
end;

procedure TShortCutSingleConfig.DoWriteConfig(
  AConfigData: IConfigDataWriteProvider);
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

function TShortCutSingleConfig.GetBitmap(AMenu: TTBCustomItem): TBitmap;
begin
  Result := nil;
  if AMenu.ImageIndex >= 0 then begin
    Result := TBitmap.Create;
    AMenu.Images.GetBitmap(AMenu.ImageIndex, Result);
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
        Result := AddName
      end else begin
        if AddName <> '' then begin
          Result :=AddName + ' -> ' + Result;
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

function TShortCutSingleConfig.GetIconBitmap: TBitmap;
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
