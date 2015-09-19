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

unit u_SensorViewTextTBXPanel;

interface

uses
  Windows,
  Classes,
  ImgList,
  GR32_Image,
  TB2Item,
  TB2Dock,
  TBX,
  TBXControls,
  i_Notifier,
  i_NotifierTime,
  i_SimpleFlag,
  i_ListenerNotifierLinksList,
  i_LanguageManager,
  i_SatellitesInViewMapDraw,
  i_ValueToStringConverter,
  i_SensorList,
  i_Sensor,
  u_BaseInterfacedObject;

type
  TSensorViewTBXPanelBase = class(TBaseInterfacedObject, ISensorView)
  private
    FListEntity: ISensorListEntity;
    FSensor: ISensor;
    FOwner: TComponent;
    FDefaultDoc: TTBDock;
    FParentMenu: TTBCustomItem;
    FImages: TCustomImageList;
    FImageIndexReset: TImageIndex;
    FLinksList: IListenerNotifierLinksList;

    FBar: TTBXToolWindow;
    FpnlTop: TTBXAlignmentPanel;
    FlblCaption: TTBXLabel;
    FbtnReset: TTBXButton;

    FResetItem: TTBXItem;
    FVisibleItem: TTBXCustomItem;
    FVisibleItemWithReset: TTBXSubmenuItem;

    FValueChangeCounter: ICounter;
    FValueShowId: Integer;

    function GuidToComponentName(
      const APrefix: string;
      const AGUID: TGUID
    ): string;
    procedure CreateMenu;
    procedure UpdateControls;

    procedure OnBarVisibleChanged(Sender: TObject);
    procedure OnVisibleItemClick(Sender: TObject);
    procedure OnResetClick(Sender: TObject);
    procedure OnTimer;
    procedure OnSensorChange;
    procedure OnSensorDataUpdate;
  protected
    procedure CreatePanel; virtual;
    procedure UpdateDataView; virtual; abstract;
  protected
    function GetSensor: ISensor;
  public
    constructor Create(
      const AListEntity: ISensorListEntity;
      const ATimerNoifier: INotifierTime;
      AOwner: TComponent;
      ADefaultDoc: TTBDock;
      AParentMenu: TTBCustomItem;
      AImages: TCustomImageList;
      AImageIndexReset: TImageIndex
    );
    destructor Destroy; override;
  end;

  TSensorViewTextTBXPanel = class(TSensorViewTBXPanelBase)
  private
    FSensor: ISensorText;
    FlblValue: TTBXLabel;
    FLastText: string;
  protected
    procedure CreatePanel; override;
    procedure UpdateDataView; override;
  public
    constructor Create(
      const AListEntity: ISensorListEntity;
      const ATimerNoifier: INotifierTime;
      AOwner: TComponent;
      ADefaultDoc: TTBDock;
      AParentMenu: TTBCustomItem;
      AImages: TCustomImageList;
      AImageIndexReset: TImageIndex
    );
  end;

  TSensorViewSpeedTBXPanel = class(TSensorViewTBXPanelBase)
  private
    FValueConverter: IValueToStringConverterChangeable;
    FSensor: ISensorSpeed;
    FlblValue: TTBXLabel;
  protected
    procedure CreatePanel; override;
    procedure UpdateDataView; override;
  public
    constructor Create(
      const AListEntity: ISensorListEntity;
      const ATimerNoifier: INotifierTime;
      const AValueConverter: IValueToStringConverterChangeable;
      AOwner: TComponent;
      ADefaultDoc: TTBDock;
      AParentMenu: TTBCustomItem;
      AImages: TCustomImageList;
      AImageIndexReset: TImageIndex
    );
  end;

  TSensorViewBatteryLifePercentTBXPanel = class(TSensorViewTBXPanelBase)
  private
    FSensor: ISensorBatteryLifePercent;
    FlblValue: TTBXLabel;
  protected
    procedure CreatePanel; override;
    procedure UpdateDataView; override;
  public
    constructor Create(
      const AListEntity: ISensorListEntity;
      const ATimerNoifier: INotifierTime;
      const ALanguageManager: ILanguageManager;
      AOwner: TComponent;
      ADefaultDoc: TTBDock;
      AParentMenu: TTBCustomItem;
      AImages: TCustomImageList;
      AImageIndexReset: TImageIndex
    );
  end;

  TSensorViewLengthTBXPanel = class(TSensorViewTBXPanelBase)
  private
    FValueConverter: IValueToStringConverterChangeable;
    FSensor: ISensorDistance;
    FlblValue: TTBXLabel;
  protected
    procedure CreatePanel; override;
    procedure UpdateDataView; override;
  public
    constructor Create(
      const AListEntity: ISensorListEntity;
      const ATimerNoifier: INotifierTime;
      const AValueConverter: IValueToStringConverterChangeable;
      AOwner: TComponent;
      ADefaultDoc: TTBDock;
      AParentMenu: TTBCustomItem;
      AImages: TCustomImageList;
      AImageIndexReset: TImageIndex
    );
  end;

  TSensorViewDegreesTBXPanel = class(TSensorViewTBXPanelBase)
  private
    FSensor: ISensorDegrees;
    FlblValue: TTBXLabel;
  protected
    procedure CreatePanel; override;
    procedure UpdateDataView; override;
  public
    constructor Create(
      const AListEntity: ISensorListEntity;
      const ATimerNoifier: INotifierTime;
      AOwner: TComponent;
      ADefaultDoc: TTBDock;
      AParentMenu: TTBCustomItem;
      AImages: TCustomImageList;
      AImageIndexReset: TImageIndex
    );
  end;

  TSensorViewDoubleTBXPanel = class(TSensorViewTBXPanelBase)
  private
    FSensor: ISensorDouble;
    FlblValue: TTBXLabel;
  protected
    procedure CreatePanel; override;
    procedure UpdateDataView; override;
  public
    constructor Create(
      const AListEntity: ISensorListEntity;
      const ATimerNoifier: INotifierTime;
      AOwner: TComponent;
      ADefaultDoc: TTBDock;
      AParentMenu: TTBCustomItem;
      AImages: TCustomImageList;
      AImageIndexReset: TImageIndex
    );
  end;

  TSensorViewTimeTBXPanel = class(TSensorViewTBXPanelBase)
  private
    FValueConverter: IValueToStringConverterChangeable;
    FSensor: ISensorTime;
    FlblValue: TTBXLabel;
  protected
    procedure CreatePanel; override;
    procedure UpdateDataView; override;
  public
    constructor Create(
      const AListEntity: ISensorListEntity;
      const ATimerNoifier: INotifierTime;
      const AValueConverter: IValueToStringConverterChangeable;
      AOwner: TComponent;
      ADefaultDoc: TTBDock;
      AParentMenu: TTBCustomItem;
      AImages: TCustomImageList;
      AImageIndexReset: TImageIndex
    );
  end;

  TSensorViewPositionTBXPanel = class(TSensorViewTBXPanelBase)
  private
    FValueConverter: IValueToStringConverterChangeable;
    FSensor: ISensorPosition;
    FlblValue: TTBXLabel;
  protected
    procedure CreatePanel; override;
    procedure UpdateDataView; override;
  public
    constructor Create(
      const AListEntity: ISensorListEntity;
      const ATimerNoifier: INotifierTime;
      const AValueConverter: IValueToStringConverterChangeable;
      AOwner: TComponent;
      ADefaultDoc: TTBDock;
      AParentMenu: TTBCustomItem;
      AImages: TCustomImageList;
      AImageIndexReset: TImageIndex
    );
  end;

  TSensorViewGPSSatellitesTBXPanel = class(TSensorViewTBXPanelBase)
  private
    FMapDraw: ISatellitesInViewMapDraw;
    FSensor: ISensorGPSSatellites;
    FImage: TImage32;
    procedure ImageResize(Sender: TObject);
  protected
    procedure CreatePanel; override;
    procedure UpdateDataView; override;
  public
    constructor Create(
      const AListEntity: ISensorListEntity;
      const ATimerNoifier: INotifierTime;
      const AMapDraw: ISatellitesInViewMapDraw;
      AOwner: TComponent;
      ADefaultDoc: TTBDock;
      AParentMenu: TTBCustomItem;
      AImages: TCustomImageList;
      AImageIndexReset: TImageIndex
    );
  end;

implementation

uses
  Graphics,
  Controls,
  Math,
  SysUtils,
  t_GeoTypes,
  i_GPS,
  u_SimpleFlagWithInterlock,
  u_ListenerNotifierLinksList,
  u_ListenerTime,
  u_ListenerByEvent,
  u_GeoFunc,
  u_GeoToStrFunc,
  u_ResStrings;

{ TSensorViewTBXPanelBase }

constructor TSensorViewTBXPanelBase.Create(
  const AListEntity: ISensorListEntity;
  const ATimerNoifier: INotifierTime;
  AOwner: TComponent;
  ADefaultDoc: TTBDock;
  AParentMenu: TTBCustomItem;
  AImages: TCustomImageList;
  AImageIndexReset: TImageIndex
);
begin
  inherited Create;
  FListEntity := AListEntity;
  FSensor := FListEntity.GetSensor;
  FOwner := AOwner;
  FValueChangeCounter := TCounterInterlock.Create;
  FValueShowId := 0;
  Assert(FOwner is TWinControl);
  FDefaultDoc := ADefaultDoc;
  FParentMenu := AParentMenu;
  FImages := AImages;
  FImageIndexReset := AImageIndexReset;

  FLinksList := TListenerNotifierLinksList.Create;

  FLinksList.Add(
    TListenerTimeCheck.Create(Self.OnTimer, 1000),
    ATimerNoifier
  );

  FLinksList.Add(
    TNotifyNoMmgEventListener.Create(Self.OnSensorChange),
    FListEntity.ChangeNotifier
  );

  FLinksList.Add(
    TNotifyNoMmgEventListener.Create(Self.OnSensorDataUpdate),
    FSensor.ChangeNotifier
  );

  CreatePanel;
  CreateMenu;
  UpdateControls;

  FLinksList.ActivateLinks;
  OnSensorDataUpdate;
  OnBarVisibleChanged(nil);
end;

destructor TSensorViewTBXPanelBase.Destroy;
begin
  if Assigned(FLinksList) then begin
    FLinksList.DeactivateLinks;
    FLinksList := nil;
  end;
  FreeAndNil(FBar);
  FSensor := nil;
  inherited;
end;

procedure TSensorViewTBXPanelBase.CreateMenu;
begin
  if Supports(FSensor, ISensorResetable) then begin
    FVisibleItemWithReset := TTBXSubmenuItem.Create(FBar);
    FVisibleItemWithReset.DropdownCombo := True;
    FVisibleItem := FVisibleItemWithReset;

    FResetItem := TTBXItem.Create(FBar);

    FResetItem.Name := GuidToComponentName('SensorReset_', FListEntity.GetGUID);
    FResetItem.OnClick := Self.OnResetClick;
    FResetItem.Hint := '';
    FResetItem.Images := FImages;
    FResetItem.ImageIndex := FImageIndexReset;
    FVisibleItemWithReset.Add(FResetItem);
  end else begin
    FVisibleItem := TTBXItem.Create(FBar);
    FVisibleItemWithReset := nil;
  end;
  FVisibleItem.Name := GuidToComponentName('Sensor_', FListEntity.GetGUID);
  FVisibleItem.AutoCheck := True;
  FVisibleItem.OnClick := Self.OnVisibleItemClick;
  FParentMenu.Add(FVisibleItem);
end;

procedure TSensorViewTBXPanelBase.CreatePanel;
begin
  FBar := TTBXToolWindow.Create(FOwner);
  FpnlTop := TTBXAlignmentPanel.Create(FBar);
  FlblCaption := TTBXLabel.Create(FBar);

  FBar.Name := GuidToComponentName('Sensor_', FListEntity.GetGUID);
  FBar.Align := alTop;
  FBar.ActivateParent := True;
  FBar.DefaultDock := FDefaultDoc;
  FBar.UseLastDock := False;
  FBar.ClientAreaHeight := 32;
  FBar.ClientAreaWidth := 150;
  FBar.DockRow := FDefaultDoc.GetHighestRow(False) + 1;
  FBar.DockPos := 0;
  FBar.Stretch := True;
  FBar.OnVisibleChanged := Self.OnBarVisibleChanged;
  FBar.Visible := True;
  FBar.Parent := FDefaultDoc;
  FBar.CurrentDock := FDefaultDoc;

  FpnlTop.Parent := FBar;
  FpnlTop.Left := 0;
  FpnlTop.Top := 0;
  FpnlTop.Width := 150;
  FpnlTop.Height := 18;
  FpnlTop.Align := alTop;

  if Supports(FSensor, ISensorResetable) then begin
    FbtnReset := TTBXButton.Create(FBar);
    FbtnReset.Parent := FpnlTop;
    FbtnReset.Left := 133;
    FbtnReset.Top := 0;
    FbtnReset.Width := 18;
    FbtnReset.Height := 18;
    FbtnReset.Align := alRight;
    FbtnReset.TabStop := False;
    FbtnReset.Images := FImages;
    FbtnReset.ImageIndex := FImageIndexReset;
    FbtnReset.ButtonStyle := bsFlat;

    FbtnReset.OnClick := Self.OnResetClick;
  end;

  FlblCaption.Parent := FpnlTop;
  FlblCaption.Left := 0;
  FlblCaption.Top := 0;
  FlblCaption.Width := 133;
  FlblCaption.Height := 18;
  FlblCaption.Align := alClient;
  FlblCaption.Wrapping := twEndEllipsis;
end;

function TSensorViewTBXPanelBase.GetSensor: ISensor;
begin
  Result := FSensor;
end;

function TSensorViewTBXPanelBase.GuidToComponentName(
  const APrefix: string;
  const AGUID: TGUID
): string;
var
  VGUIDStr: string;
begin
  VGUIDStr := GUIDToString(AGUID);
  VGUIDStr := StringReplace(VGUIDStr, '{', '', [rfReplaceAll]);
  VGUIDStr := StringReplace(VGUIDStr, '}', '', [rfReplaceAll]);
  VGUIDStr := StringReplace(VGUIDStr, '-', '_', [rfReplaceAll]);
  Result := APrefix + VGUIDStr;
end;

procedure TSensorViewTBXPanelBase.OnBarVisibleChanged(Sender: TObject);
begin
  FVisibleItem.Checked := FBar.Visible;
end;

procedure TSensorViewTBXPanelBase.OnResetClick(Sender: TObject);
var
  VSensorResetable: ISensorResetable;
begin
  if Supports(FSensor, ISensorResetable, VSensorResetable) then begin
    if (MessageBox(TWinControl(FOwner).Handle, pchar(SAS_MSG_ResetSensorAsk), pchar(SAS_MSG_coution), 36) = IDYES) then begin
      VSensorResetable.Reset;
      OnTimer;
    end;
  end;
end;

procedure TSensorViewTBXPanelBase.OnSensorChange;
begin
  UpdateControls;
end;

procedure TSensorViewTBXPanelBase.OnSensorDataUpdate;
begin
  FValueChangeCounter.Inc;
end;

procedure TSensorViewTBXPanelBase.OnTimer;
var
  VId: Integer;
begin
  if FBar.Visible then begin
    VId := FValueChangeCounter.GetValue;
    if VId <> FValueShowId then begin
      UpdateDataView;
      FValueShowId := VId;
    end;
  end;
end;

procedure TSensorViewTBXPanelBase.OnVisibleItemClick(Sender: TObject);
begin
  FBar.Visible := FVisibleItem.Checked;
end;

procedure TSensorViewTBXPanelBase.UpdateControls;
begin
  FVisibleItem.Caption := FListEntity.GetMenuItemName;
  FBar.Caption := FListEntity.GetCaption;
  FBar.Hint := FListEntity.GetDescription;
  FlblCaption.Caption := FListEntity.GetCaption;
  if FResetItem <> nil then begin
    FResetItem.Caption := SAS_STR_SensorReset;
  end;
  if FbtnReset <> nil then begin
    FbtnReset.Hint := SAS_STR_SensorReset;
  end;
end;

{ TSensorViewTextTBXPanel }

constructor TSensorViewTextTBXPanel.Create(
  const AListEntity: ISensorListEntity;
  const ATimerNoifier: INotifierTime;
  AOwner: TComponent;
  ADefaultDoc: TTBDock;
  AParentMenu: TTBCustomItem;
  AImages: TCustomImageList;
  AImageIndexReset: TImageIndex
);
begin
  inherited;
  if not Supports(FListEntity.GetSensor, ISensorText, FSensor) then begin
    raise Exception.Create('Неподдерживаемый тип сенсора');
  end;
end;

procedure TSensorViewTextTBXPanel.CreatePanel;
begin
  inherited;
  FlblValue := TTBXLabel.Create(FBar);

  FlblValue.Parent := FBar;
  FlblValue.AutoSize := True;
  FlblValue.Left := 0;
  FlblValue.Top := 17;
  FlblValue.Width := 150;
  FlblValue.Height := 15;
  FlblValue.Align := alTop;
  FlblValue.Font.Height := -16;
  FlblValue.Font.Name := 'Arial';
  FlblValue.Font.Style := [fsBold];
  FlblValue.ParentFont := False;
  FlblValue.Wrapping := twEndEllipsis;
  FlblValue.Caption := '';

  FBar.ClientAreaHeight := FlblValue.Top + FlblValue.Height + 2;
end;

procedure TSensorViewTextTBXPanel.UpdateDataView;
var
  VText: string;
begin
  VText := FSensor.GetText;
  if FLastText <> VText then begin
    FLastText := VText;
    FlblValue.Caption := FLastText;
  end;
end;

{ TSensorViewBitmapTBXPanel }

constructor TSensorViewGPSSatellitesTBXPanel.Create(
  const AListEntity: ISensorListEntity;
  const ATimerNoifier: INotifierTime;
  const AMapDraw: ISatellitesInViewMapDraw;
  AOwner: TComponent;
  ADefaultDoc: TTBDock;
  AParentMenu: TTBCustomItem;
  AImages: TCustomImageList;
  AImageIndexReset: TImageIndex
);
begin
  inherited Create(AListEntity, ATimerNoifier, AOwner, ADefaultDoc, AParentMenu, AImages, AImageIndexReset);
  FMapDraw := AMapDraw;
  if not Supports(FListEntity.GetSensor, ISensorGPSSatellites, FSensor) then begin
    raise Exception.Create('Неподдерживаемый тип сенсора');
  end;
end;

procedure TSensorViewGPSSatellitesTBXPanel.CreatePanel;
begin
  inherited;
  FImage := TImage32.Create(FBar);

  FImage.Parent := FBar;
  FImage.AutoSize := True;
  FImage.Left := 0;
  FImage.Top := 17;
  FImage.Width := 150;
  FImage.Height := 45;
  FImage.Align := alClient;
  FImage.BitmapAlign := baTopLeft;
  FImage.Bitmap.SetSizeFrom(FImage);
  FImage.OnResize := ImageResize;

  FBar.ClientAreaHeight := FImage.Top + FImage.Height + 2;
end;

procedure TSensorViewGPSSatellitesTBXPanel.ImageResize(Sender: TObject);
begin
  FImage.Bitmap.Lock;
  try
    FImage.Bitmap.SetSizeFrom(FImage);
    UpdateDataView;
  finally
    FImage.Bitmap.Unlock;
  end;
end;

procedure TSensorViewGPSSatellitesTBXPanel.UpdateDataView;
var
  VSatellites: IGPSSatellitesInView;
begin
  VSatellites := FSensor.Info;
  FImage.Bitmap.Lock;
  try
    FMapDraw.Draw(FImage.Bitmap, VSatellites);
  finally
    FImage.Bitmap.Unlock;
  end;
end;

{ TSensorViewSpeedTBXPanel }

constructor TSensorViewSpeedTBXPanel.Create(
  const AListEntity: ISensorListEntity;
  const ATimerNoifier: INotifierTime;
  const AValueConverter: IValueToStringConverterChangeable;
  AOwner: TComponent;
  ADefaultDoc: TTBDock;
  AParentMenu: TTBCustomItem;
  AImages: TCustomImageList;
  AImageIndexReset: TImageIndex
);
begin
  inherited Create(AListEntity, ATimerNoifier, AOwner, ADefaultDoc, AParentMenu, AImages, AImageIndexReset);
  FValueConverter := AValueConverter;
  if not Supports(FListEntity.GetSensor, ISensorSpeed, FSensor) then begin
    raise Exception.Create('Неподдерживаемый тип сенсора');
  end;
  FLinksList.Add(
    TNotifyNoMmgEventListener.Create(Self.OnSensorDataUpdate),
    FValueConverter.ChangeNotifier
  );
end;

procedure TSensorViewSpeedTBXPanel.CreatePanel;
begin
  inherited;
  FlblValue := TTBXLabel.Create(FBar);

  FlblValue.Parent := FBar;
  FlblValue.AutoSize := True;
  FlblValue.Left := 0;
  FlblValue.Top := 17;
  FlblValue.Width := 150;
  FlblValue.Height := 15;
  FlblValue.Align := alTop;
  FlblValue.Font.Height := -16;
  FlblValue.Font.Name := 'Arial';
  FlblValue.Font.Style := [fsBold];
  FlblValue.ParentFont := False;
  FlblValue.Wrapping := twEndEllipsis;
  FlblValue.Caption := '';

  FBar.ClientAreaHeight := FlblValue.Top + FlblValue.Height + 2;
end;

procedure TSensorViewSpeedTBXPanel.UpdateDataView;
var
  VValue: Double;
  VText: string;
begin
  VValue := FSensor.GetValue;
  if IsNan(VValue) then begin
    VText := '~';
  end else begin
    VText := FValueConverter.GetStatic.SpeedConvert(VValue);
  end;
  FlblValue.Caption := VText;
end;

{ TSensorViewLengthTBXPanel }

constructor TSensorViewLengthTBXPanel.Create(
  const AListEntity: ISensorListEntity;
  const ATimerNoifier: INotifierTime;
  const AValueConverter: IValueToStringConverterChangeable;
  AOwner: TComponent;
  ADefaultDoc: TTBDock;
  AParentMenu: TTBCustomItem;
  AImages: TCustomImageList;
  AImageIndexReset: TImageIndex
);
begin
  inherited Create(AListEntity, ATimerNoifier, AOwner, ADefaultDoc, AParentMenu, AImages, AImageIndexReset);
  FValueConverter := AValueConverter;
  if not Supports(FListEntity.GetSensor, ISensorDistance, FSensor) then begin
    raise Exception.Create('Неподдерживаемый тип сенсора');
  end;
  FLinksList.Add(
    TNotifyNoMmgEventListener.Create(Self.OnSensorDataUpdate),
    FValueConverter.ChangeNotifier
  );
end;

procedure TSensorViewLengthTBXPanel.CreatePanel;
begin
  inherited;
  FlblValue := TTBXLabel.Create(FBar);

  FlblValue.Parent := FBar;
  FlblValue.AutoSize := True;
  FlblValue.Left := 0;
  FlblValue.Top := 17;
  FlblValue.Width := 150;
  FlblValue.Height := 15;
  FlblValue.Align := alTop;
  FlblValue.Font.Height := -16;
  FlblValue.Font.Name := 'Arial';
  FlblValue.Font.Style := [fsBold];
  FlblValue.ParentFont := False;
  FlblValue.Wrapping := twEndEllipsis;
  FlblValue.Caption := '';

  FBar.ClientAreaHeight := FlblValue.Top + FlblValue.Height + 2;
end;

procedure TSensorViewLengthTBXPanel.UpdateDataView;
var
  VValue: Double;
  VText: string;
begin
  VValue := FSensor.GetValue;
  if IsNan(VValue) then begin
    VText := '~';
  end else begin
    VText := FValueConverter.GetStatic.DistConvert(FSensor.GetValue);
  end;
  FlblValue.Caption := VText;
end;

{ TSensorViewDegreesTBXPanel }

constructor TSensorViewDegreesTBXPanel.Create(
  const AListEntity: ISensorListEntity;
  const ATimerNoifier: INotifierTime;
  AOwner: TComponent;
  ADefaultDoc: TTBDock;
  AParentMenu: TTBCustomItem;
  AImages: TCustomImageList;
  AImageIndexReset: TImageIndex
);
begin
  inherited Create(AListEntity, ATimerNoifier, AOwner, ADefaultDoc, AParentMenu, AImages, AImageIndexReset);
  if not Supports(FListEntity.GetSensor, ISensorDegrees, FSensor) then begin
    raise Exception.Create('Неподдерживаемый тип сенсора');
  end;
end;

procedure TSensorViewDegreesTBXPanel.CreatePanel;
begin
  inherited;
  FlblValue := TTBXLabel.Create(FBar);

  FlblValue.Parent := FBar;
  FlblValue.AutoSize := True;
  FlblValue.Left := 0;
  FlblValue.Top := 17;
  FlblValue.Width := 150;
  FlblValue.Height := 15;
  FlblValue.Align := alTop;
  FlblValue.Font.Height := -16;
  FlblValue.Font.Name := 'Arial';
  FlblValue.Font.Style := [fsBold];
  FlblValue.ParentFont := False;
  FlblValue.Wrapping := twEndEllipsis;
  FlblValue.Caption := '';

  FBar.ClientAreaHeight := FlblValue.Top + FlblValue.Height + 2;
end;

procedure TSensorViewDegreesTBXPanel.UpdateDataView;
var
  VValue: Double;
  VText: string;
begin
  VValue := FSensor.GetValue;
  if IsNan(VValue) then begin
    VText := '~';
  end else begin
    VText := RoundEx(FSensor.GetValue, 2) + '°';
  end;
  FlblValue.Caption := VText;
end;

{ TSensorViewTimeTBXPanel }

constructor TSensorViewTimeTBXPanel.Create(
  const AListEntity: ISensorListEntity;
  const ATimerNoifier: INotifierTime;
  const AValueConverter: IValueToStringConverterChangeable;
  AOwner: TComponent;
  ADefaultDoc: TTBDock;
  AParentMenu: TTBCustomItem;
  AImages: TCustomImageList;
  AImageIndexReset: TImageIndex
);
begin
  inherited Create(AListEntity, ATimerNoifier, AOwner, ADefaultDoc, AParentMenu, AImages, AImageIndexReset);
  FValueConverter := AValueConverter;
  if not Supports(FListEntity.GetSensor, ISensorTime, FSensor) then begin
    raise Exception.Create('Неподдерживаемый тип сенсора');
  end;
  FLinksList.Add(
    TNotifyNoMmgEventListener.Create(Self.OnSensorDataUpdate),
    FValueConverter.ChangeNotifier
  );
end;

procedure TSensorViewTimeTBXPanel.CreatePanel;
begin
  inherited;
  FlblValue := TTBXLabel.Create(FBar);

  FlblValue.Parent := FBar;
  FlblValue.AutoSize := True;
  FlblValue.Left := 0;
  FlblValue.Top := 17;
  FlblValue.Width := 150;
  FlblValue.Height := 15;
  FlblValue.Align := alTop;
  FlblValue.Font.Height := -16;
  FlblValue.Font.Name := 'Arial';
  FlblValue.Font.Style := [fsBold];
  FlblValue.ParentFont := False;
  FlblValue.Wrapping := twEndEllipsis;
  FlblValue.Caption := '';

  FBar.ClientAreaHeight := FlblValue.Top + FlblValue.Height + 2;
end;

procedure TSensorViewTimeTBXPanel.UpdateDataView;
var
  VValue: TDateTime;
  VText: string;
begin
  VValue := FSensor.GetValue;
  if IsNan(VValue) or (VValue = 0) then begin
    VText := '';
  end else begin
    VText := FormatDateTime('hh:nn:ss', VValue);
  end;
  FlblValue.Caption := VText;
end;

{ TSensorViewPositionTBXPanel }

constructor TSensorViewPositionTBXPanel.Create(
  const AListEntity: ISensorListEntity;
  const ATimerNoifier: INotifierTime;
  const AValueConverter: IValueToStringConverterChangeable;
  AOwner: TComponent;
  ADefaultDoc: TTBDock;
  AParentMenu: TTBCustomItem;
  AImages: TCustomImageList;
  AImageIndexReset: TImageIndex
);
begin
  inherited Create(AListEntity, ATimerNoifier, AOwner, ADefaultDoc, AParentMenu, AImages, AImageIndexReset);
  FValueConverter := AValueConverter;
  if not Supports(FListEntity.GetSensor, ISensorPosition, FSensor) then begin
    raise Exception.Create('Неподдерживаемый тип сенсора');
  end;
  FLinksList.Add(
    TNotifyNoMmgEventListener.Create(Self.OnSensorDataUpdate),
    FValueConverter.ChangeNotifier
  );
end;

procedure TSensorViewPositionTBXPanel.CreatePanel;
begin
  inherited;
  FlblValue := TTBXLabel.Create(FBar);

  FlblValue.Parent := FBar;
  FlblValue.AutoSize := True;
  FlblValue.Left := 0;
  FlblValue.Top := 17;
  FlblValue.Width := 150;
  FlblValue.Height := 15;
  FlblValue.Align := alTop;
  FlblValue.Font.Height := -16;
  FlblValue.Font.Name := 'Arial';
  FlblValue.Font.Style := [fsBold];
  FlblValue.ParentFont := False;
  FlblValue.Wrapping := twEndEllipsis;
  FlblValue.Caption := '';

  FBar.ClientAreaHeight := FlblValue.Top + FlblValue.Height + 2;
end;

procedure TSensorViewPositionTBXPanel.UpdateDataView;
var
  VValue: TDoublePoint;
  VText: string;
begin
  VValue := FSensor.GetValue;
  if PointIsEmpty(VValue) then begin
    VText := '~';
  end else begin
    VText := FValueConverter.GetStatic.LonLatConvert(FSensor.GetValue);
  end;
  FlblValue.Caption := VText;
end;

{ TSensorViewDoubleTBXPanel }

constructor TSensorViewDoubleTBXPanel.Create(
  const AListEntity: ISensorListEntity;
  const ATimerNoifier: INotifierTime;
  AOwner: TComponent;
  ADefaultDoc: TTBDock;
  AParentMenu: TTBCustomItem;
  AImages: TCustomImageList;
  AImageIndexReset: TImageIndex
);
begin
  inherited Create(AListEntity, ATimerNoifier, AOwner, ADefaultDoc, AParentMenu, AImages, AImageIndexReset);
  if not Supports(FListEntity.GetSensor, ISensorDouble, FSensor) then begin
    raise Exception.Create('Неподдерживаемый тип сенсора');
  end;
end;

procedure TSensorViewDoubleTBXPanel.CreatePanel;
begin
  inherited;
  FlblValue := TTBXLabel.Create(FBar);

  FlblValue.Parent := FBar;
  FlblValue.AutoSize := True;
  FlblValue.Left := 0;
  FlblValue.Top := 17;
  FlblValue.Width := 150;
  FlblValue.Height := 15;
  FlblValue.Align := alTop;
  FlblValue.Font.Height := -16;
  FlblValue.Font.Name := 'Arial';
  FlblValue.Font.Style := [fsBold];
  FlblValue.ParentFont := False;
  FlblValue.Wrapping := twEndEllipsis;
  FlblValue.Caption := '';

  FBar.ClientAreaHeight := FlblValue.Top + FlblValue.Height + 2;
end;

procedure TSensorViewDoubleTBXPanel.UpdateDataView;
var
  VValue: Double;
  VText: string;
begin
  VValue := FSensor.GetValue;
  if IsNan(VValue) then begin
    VText := '~';
  end else begin
    VText := RoundEx(FSensor.GetValue, 1);
  end;
  FlblValue.Caption := VText;
end;

{ TSensorViewBatteryLifePercentTBXPanel }

constructor TSensorViewBatteryLifePercentTBXPanel.Create(
  const AListEntity: ISensorListEntity;
  const ATimerNoifier: INotifierTime;
  const ALanguageManager: ILanguageManager;
  AOwner: TComponent;
  ADefaultDoc: TTBDock;
  AParentMenu: TTBCustomItem;
  AImages: TCustomImageList;
  AImageIndexReset: TImageIndex
);
begin
  inherited Create(AListEntity, ATimerNoifier, AOwner, ADefaultDoc, AParentMenu, AImages, AImageIndexReset);
  if not Supports(FListEntity.GetSensor, ISensorBatteryLifePercent, FSensor) then begin
    raise Exception.Create('Неподдерживаемый тип сенсора');
  end;
  FLinksList.Add(
    TNotifyNoMmgEventListener.Create(Self.OnSensorDataUpdate),
    ALanguageManager.ChangeNotifier
  );
end;

procedure TSensorViewBatteryLifePercentTBXPanel.CreatePanel;
begin
  inherited;
  FlblValue := TTBXLabel.Create(FBar);

  FlblValue.Parent := FBar;
  FlblValue.AutoSize := True;
  FlblValue.Left := 0;
  FlblValue.Top := 17;
  FlblValue.Width := 150;
  FlblValue.Height := 15;
  FlblValue.Align := alTop;
  FlblValue.Font.Height := -16;
  FlblValue.Font.Name := 'Arial';
  FlblValue.Font.Style := [fsBold];
  FlblValue.ParentFont := False;
  FlblValue.Wrapping := twEndEllipsis;
  FlblValue.Caption := '';

  FBar.ClientAreaHeight := FlblValue.Top + FlblValue.Height + 2;
end;

procedure TSensorViewBatteryLifePercentTBXPanel.UpdateDataView;
var
  VText: string;
  VValue: Byte;
begin
  VValue := FSensor.GetValue;
  case VValue of
    0..100: begin
      VText := inttostr(VValue) + '%';
    end;
    101: begin
      VText := SAS_STR_BattaryStateCharge;
    end;
    200: begin
      VText := SAS_STR_BattaryStateOnLine;
    end;
  else begin
    VText := SAS_STR_BattaryStateUnknown;
  end;
  end;
  FlblValue.Caption := VText;
end;

end.
