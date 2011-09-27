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

unit u_ActiveMapTBXItem;

interface

uses
  Graphics,
  Classes,
  TB2Item,
  TBX,
  i_JclNotify,
  i_ActiveMapsConfig;

type
  TActiveMapTBXItem = class(TTBXCustomItem)
  private
    FMapActive: IActiveMapSingle;
    FListener: IJclListener;
    procedure OnMapChangeState(Sender: TObject);
    procedure AdjustFont(Item: TTBCustomItem;
      Viewer: TTBItemViewer; Font: TFont; StateFlags: Integer);
  public
    constructor Create(AOwner: TComponent; AMapActive: IActiveMapSingle); reintroduce;
    destructor Destroy; override;
  end;

implementation

uses
  u_NotifyEventListener;

{ TMiniMapTBXITem }

constructor TActiveMapTBXItem.Create(AOwner: TComponent;
  AMapActive: IActiveMapSingle);
begin
  inherited Create(AOwner);
  FMapActive := AMapActive;
  OnAdjustFont := Self.AdjustFont;
  FListener := TNotifyEventListener.Create(Self.OnMapChangeState);
  FMapActive.GetChangeNotifier.Add(FListener);
  OnMapChangeState(nil);
end;

destructor TActiveMapTBXItem.Destroy;
begin
  FMapActive.GetChangeNotifier.Remove(FListener);
  FListener := nil;
  inherited;
end;

procedure TActiveMapTBXItem.AdjustFont(Item: TTBCustomItem; Viewer: TTBItemViewer;
  Font: TFont; StateFlags: Integer);
begin
  if Self.Checked then begin
    Self.FontSettings.Bold := tsTrue;
  end else begin
    Self.FontSettings.Bold := tsDefault;
  end;
end;

procedure TActiveMapTBXItem.OnMapChangeState(Sender: TObject);
begin
  Self.Checked := FMapActive.GetIsActive;
end;

end.
