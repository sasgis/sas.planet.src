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

unit u_MainWindowPositionConfig;

interface

uses
  Types,
  i_ConfigDataProvider,
  i_ConfigDataWriteProvider,
  u_ConfigDataElementBase,
  i_MainWindowPosition;

type
  TMainWindowPositionConfig = class(TConfigDataElementBase, IMainWindowPosition)
  private
    FIsFullScreen: Boolean;
    FIsMaximized: Boolean;
    FBoundsRect: TRect;
  protected
    procedure DoReadConfig(AConfigData: IConfigDataProvider); override;
    procedure DoWriteConfig(AConfigData: IConfigDataWriteProvider); override;
  protected
    function GetIsFullScreen: Boolean;
    function GetIsMaximized: Boolean;
    function GetBoundsRect: TRect;
    procedure SetFullScreen;
    procedure SetNoFullScreen;
    procedure SetMaximized;
    procedure SetNormalWindow;
    procedure SetWindowPosition(ARect: TRect);
  public
    constructor Create(AStartRect: TRect);
  end;

implementation

{ TMainWindowPositionConfig }

constructor TMainWindowPositionConfig.Create(AStartRect: TRect);
begin
  inherited Create;
  FBoundsRect := AStartRect;
  FIsFullScreen := False;
  FIsMaximized := False;
end;

procedure TMainWindowPositionConfig.DoReadConfig(AConfigData: IConfigDataProvider);
begin
  inherited;
  if AConfigData <> nil then begin
    FBoundsRect := Bounds(
      AConfigData.ReadInteger('Left', FBoundsRect.Left),
      AConfigData.ReadInteger('Top', FBoundsRect.Top),
      AConfigData.ReadInteger('Width', FBoundsRect.Right -  FBoundsRect.Top),
      AConfigData.ReadInteger('Height', FBoundsRect.Bottom - FBoundsRect.Top)
    );
    FIsMaximized := AConfigData.ReadBool('Maximized', FIsMaximized);
    FIsFullScreen := AConfigData.ReadBool('FullScreen', FIsFullScreen);
    SetChanged;
  end;
end;

procedure TMainWindowPositionConfig.DoWriteConfig(
  AConfigData: IConfigDataWriteProvider);
begin
  inherited;
  AConfigData.WriteBool('FullScreen', FIsFullScreen);
  AConfigData.WriteBool('Maximized', FIsMaximized);
  AConfigData.WriteInteger('Left', FBoundsRect.Left);
  AConfigData.WriteInteger('Top', FBoundsRect.Top);
  AConfigData.WriteInteger('Width', FBoundsRect.Right - FBoundsRect.Left);
  AConfigData.WriteInteger('Height', FBoundsRect.Bottom - FBoundsRect.Top);
end;

function TMainWindowPositionConfig.GetBoundsRect: TRect;
begin
  LockRead;
  try
    Result := FBoundsRect;
  finally
    UnlockRead;
  end;
end;

function TMainWindowPositionConfig.GetIsFullScreen: Boolean;
begin
  LockRead;
  try
    Result := FIsFullScreen;
  finally
    UnlockRead;
  end;
end;

function TMainWindowPositionConfig.GetIsMaximized: Boolean;
begin
  LockRead;
  try
    Result := FIsMaximized;
  finally
    UnlockRead;
  end;
end;

procedure TMainWindowPositionConfig.SetFullScreen;
begin
  LockWrite;
  try
    if not FIsFullScreen then begin
      FIsFullScreen := True;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

procedure TMainWindowPositionConfig.SetMaximized;
begin
  LockWrite;
  try
    if FIsFullScreen or not FIsMaximized then begin
      FIsFullScreen := False;
      FIsMaximized := True;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

procedure TMainWindowPositionConfig.SetNoFullScreen;
begin
  LockWrite;
  try
    if FIsFullScreen then begin
      FIsFullScreen := False;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

procedure TMainWindowPositionConfig.SetNormalWindow;
begin
  LockWrite;
  try
    if FIsFullScreen or FIsMaximized then begin
      FIsFullScreen := False;
      FIsMaximized := False;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

procedure TMainWindowPositionConfig.SetWindowPosition(ARect: TRect);
begin
  LockWrite;
  try
    if FIsFullScreen or FIsMaximized then begin
      FIsFullScreen := False;
      FIsMaximized := False;
      SetChanged;
    end;
    if not EqualRect(FBoundsRect, ARect) then begin
      FBoundsRect := ARect;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

end.
