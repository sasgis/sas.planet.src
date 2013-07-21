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

unit u_MapLayerGPSTrackConfig;

interface

uses
  Classes,
  GR32,
  i_InterfaceListSimple,
  i_ConfigDataProvider,
  i_ConfigDataWriteProvider,
  i_ThreadConfig,
  i_MapLayerGPSTrackConfig,
  u_ConfigDataElementBase,
  u_ConfigDataElementComplexBase,
  u_BaseInterfacedObject;

type
  TSpeedRangeItem = class(TBaseInterfacedObject, ISpeedRangeItem)
  private
    FSpeed: Double;
    FMinSpeedColor: TColor32;
    FMaxSpeedColor: TColor32;
  private
    function GetSpeed: Double;
    function GetMinSpeedColor: TColor32;
    function GetMaxSpeedColor: TColor32;
  public
    constructor Create(
      ASpeed: Double;
      AMinColor, AMaxColor: TColor32
    );
  end;

  TTrackColorerStatic = class(TBaseInterfacedObject, ITrackColorerStatic)
  private
    FCount: Integer;
    FSpeedArray: array of Double;
    FMinColorArray: TArrayOfColor32;
    FMaxColorArray: TArrayOfColor32;
    function GetColor(
      AColorMin, AColorMax: TColor32;
      ASpeedMin, ASpeedMax, ASpeed: Double
    ): TColor32;
  private
    function GetColorForSpeed(ASpeed: Double): TColor32;
  public
    constructor Create(AList: IInterfaceListSimple);
  end;

  TTrackColorerConfig = class(TConfigDataElementComplexWithStaticBase, ITrackColorerConfig)
  private
    FList: IInterfaceListSimple;
    procedure CreateDefault;
  protected
    function CreateStatic: IInterface; override;
  protected
    procedure DoReadConfig(const AConfigData: IConfigDataProvider); override;
    procedure DoWriteConfig(const AConfigData: IConfigDataWriteProvider); override;
  private
    function GetStatic: ITrackColorerStatic;

    function GetSpeedRangeCount: Integer;

    function GetSpeedRangeItem(AIndex: Integer): ISpeedRangeItem;
    function AddSpeedRangeItem(
      ASpeed: Double;
      AMinColor, AMaxColor: TColor32
    ): Integer;
    procedure ClearItems;
  public
    constructor Create;
  end;


  TMapLayerGPSTrackConfig = class(TConfigDataElementComplexBase, IMapLayerGPSTrackConfig)
  private
    FVisible: Boolean;
    FLineWidth: Double;
    FLastPointCount: Integer;
    FTrackColorerConfig: ITrackColorerConfig;
    FThreadConfig: IThreadConfig;
  protected
    procedure DoReadConfig(const AConfigData: IConfigDataProvider); override;
    procedure DoWriteConfig(const AConfigData: IConfigDataWriteProvider); override;
  private
    function GetVisible: Boolean;
    procedure SetVisible(AValue: Boolean);

    function GetLineWidth: Double;
    procedure SetLineWidth(AValue: Double);

    function GetLastPointCount: Integer;
    procedure SetLastPointCount(AValue: Integer);

    function GetTrackColorerConfig: ITrackColorerConfig;
    function GetThreadConfig: IThreadConfig;
  public
    constructor Create;
  end;

implementation

uses
  SysUtils,
  u_InterfaceListSimple,
  u_ConfigSaveLoadStrategyBasicUseProvider,
  u_ConfigSaveLoadStrategyBasicProviderSubItem,
  u_ThreadConfig,
  u_ConfigProviderHelpers;

{ TSpeedRangeItem }

constructor TSpeedRangeItem.Create(
  ASpeed: Double;
  AMinColor,
  AMaxColor: TColor32
);
begin
  inherited Create;
  FSpeed := ASpeed;
  FMinSpeedColor := AMinColor;
  FMaxSpeedColor := AMaxColor;
end;

function TSpeedRangeItem.GetMaxSpeedColor: TColor32;
begin
  Result := FMaxSpeedColor;
end;

function TSpeedRangeItem.GetMinSpeedColor: TColor32;
begin
  Result := FMinSpeedColor;
end;

function TSpeedRangeItem.GetSpeed: Double;
begin
  Result := FSpeed;
end;

{ TTrackColorerConfig }

constructor TTrackColorerConfig.Create;
begin
  inherited Create;
  FList := TInterfaceListSimple.Create;
  CreateDefault;
end;

procedure TTrackColorerConfig.CreateDefault;
var
  VItem: ISpeedRangeItem;
begin
  FList.Clear;
  VItem := TSpeedRangeItem.Create(10, SetAlpha(clBlack32, 64), SetAlpha(clBlack32, 192));
  FList.Add(VItem);
  VItem := TSpeedRangeItem.Create(60, SetAlpha(clGreen32, 192), SetAlpha(clYellow32, 192));
  FList.Add(VItem);
  VItem := TSpeedRangeItem.Create(100, SetAlpha(clYellow32, 192), SetAlpha(clRed32, 192));
  FList.Add(VItem);
  VItem := TSpeedRangeItem.Create(140, SetAlpha(clLime32, 192), SetAlpha(clTeal32, 192));
  FList.Add(VItem);
end;

function TTrackColorerConfig.CreateStatic: IInterface;
var
  VStatic: ITrackColorerStatic;
begin
  VStatic := TTrackColorerStatic.Create(FList);
  Result := VStatic;
end;

procedure TTrackColorerConfig.DoReadConfig(const AConfigData: IConfigDataProvider);
var
  VSpeed: Double;
  VColorMin: TColor32;
  VColorMax: TColor32;
  i: Integer;
  VSufix: string;
  VCleared: Boolean;
begin
  inherited;
  if AConfigData <> nil then begin
    i := 0;
    VCleared := False;
    while True do begin
      VSufix := IntToStr(i);
      VSpeed := AConfigData.ReadFloat('Speed_' + VSufix, -1);
      if VSpeed < 0 then begin
        Break;
      end else begin
        if not VCleared then begin
          ClearItems;
          VCleared := True;
        end;
        VColorMin := ReadColor32(AConfigData, 'MinColor_' + VSufix, clBlack32);
        VColorMax := ReadColor32(AConfigData, 'MaxColor_' + VSufix, clBlack32);
        AddSpeedRangeItem(VSpeed, VColorMin, VColorMax);
      end;
      Inc(i);
    end;
  end;
end;

procedure TTrackColorerConfig.DoWriteConfig(
  const AConfigData: IConfigDataWriteProvider
);
var
  i: Integer;
  VSufix: string;
  VItem: ISpeedRangeItem;
begin
  inherited;
  AConfigData.DeleteValues;
  for i := 0 to FList.Count - 1 do begin
    VItem := ISpeedRangeItem(FList.Items[i]);
    VSufix := IntToStr(i);
    AConfigData.WriteFloat('Speed_' + VSufix, VItem.GetSpeed);
    WriteColor32(AConfigData, 'MinColor_' + VSufix, VItem.GetMinSpeedColor);
    WriteColor32(AConfigData, 'MaxColor_' + VSufix, VItem.GetMaxSpeedColor);
  end;
end;

function TTrackColorerConfig.AddSpeedRangeItem(
  ASpeed: Double;
  AMinColor,
  AMaxColor: TColor32
): Integer;
var
  VItemNew: ISpeedRangeItem;
  VItemExists: ISpeedRangeItem;
  VH: Integer;
  VL: Integer;
  VI: Integer;
  VC: Integer;
  VDelta: Double;
begin
  LockWrite;
  try
    VItemNew := TSpeedRangeItem.Create(ASpeed, AMinColor, AMaxColor);
    if FList.Count > 0 then begin
      VL := 0;
      VH := FList.Count - 1;
      VC := -1;
      while VL <= VH do begin
        VI := (VL + VH) shr 1;
        VItemExists := ISpeedRangeItem(FList[VI]);
        VDelta := VItemExists.GetSpeed - ASpeed;
        if Abs(VDelta) < 0.01 then begin
          VC := 0;
        end else if VDelta > 0 then begin
          VC := 1;
        end else begin
          VC := -1;
        end;
        if VC < 0 then begin
          VL := VI + 1;
        end else begin
          VH := VI - 1;
          if VC = 0 then begin
            VL := VI;
          end;
        end;
      end;
      if VC = 0 then begin
        FList.Items[VL] := VItemNew;
        Result := VL;
      end else begin
        if VL < 0 then begin
          VL := 0;
        end;
        Result := VL;
        FList.Insert(Result, VItemNew);
      end;
    end else begin
      Result := FList.Add(VItemNew);
    end;
    SetChanged;
  finally
    UnlockWrite;
  end;
end;

procedure TTrackColorerConfig.ClearItems;
begin
  LockWrite;
  try
    FList.Clear;
    SetChanged;
  finally
    UnlockWrite;
  end;
end;

function TTrackColorerConfig.GetSpeedRangeCount: Integer;
begin
  LockRead;
  try
    Result := FList.Count;
  finally
    UnlockRead;
  end;
end;

function TTrackColorerConfig.GetSpeedRangeItem(
  AIndex: Integer): ISpeedRangeItem;
begin
  LockRead;
  try
    Result := ISpeedRangeItem(FList.Items[AIndex]);
  finally
    UnlockRead;
  end;
end;

function TTrackColorerConfig.GetStatic: ITrackColorerStatic;
begin
  Result := ITrackColorerStatic(GetStaticInternal);
end;

{ TTrackColorerStatic }

constructor TTrackColorerStatic.Create(AList: IInterfaceListSimple);
var
  i: Integer;
  VItem: ISpeedRangeItem;
begin
  inherited Create;
  FCount := AList.Count;

  SetLength(FSpeedArray, FCount);
  SetLength(FMinColorArray, FCount);
  SetLength(FMaxColorArray, FCount);

  for i := 0 to FCount - 1 do begin
    VItem := ISpeedRangeItem(AList.Items[i]);
    FSpeedArray[i] := VItem.GetSpeed;
    FMinColorArray[i] := VItem.GetMinSpeedColor;
    FMaxColorArray[i] := VItem.GetMaxSpeedColor;
  end;
end;

function TTrackColorerStatic.GetColor(
  AColorMin, AColorMax: TColor32;
  ASpeedMin,
  ASpeedMax, ASpeed: Double
): TColor32;
var
  VRatio: Double;
  VRMin, VGMin, VBMin, VAMin: Byte;
  VRMax, VGMax, VBMax, VAMax: Byte;
  VR, VG, VB, VA: Byte;
begin
  if AColorMin = AColorMax then begin
    Result := AColorMin;
    Exit;
  end;
  if ASpeedMin >= ASpeedMax then begin
    Result := AColorMin;
    Exit;
  end;
  if ASpeed <= ASpeedMin then begin
    Result := AColorMin;
    Exit;
  end;
  if ASpeed >= ASpeedMax then begin
    Result := AColorMax;
    Exit;
  end;
  VRatio := (ASpeed - ASpeedMin) / (ASpeedMax - ASpeedMin);
  Color32ToRGBA(AColorMin, VRMin, VGMin, VBMin, VAMin);
  Color32ToRGBA(AColorMax, VRMax, VGMax, VBMax, VAMax);
  if VRMin = VRMax then begin
    VR := VRMin;
  end else begin
    VR := Trunc(VRMin + (VRMax - VRMin) * VRatio);
  end;
  if VGMin = VGMax then begin
    VG := VGMin;
  end else begin
    VG := Trunc(VGMin + (VGMax - VGMin) * VRatio);
  end;
  if VBMin = VBMax then begin
    VB := VBMin;
  end else begin
    VB := Trunc(VBMin + (VBMax - VBMin) * VRatio);
  end;
  if VAMin = VAMax then begin
    VA := VAMin;
  end else begin
    VA := Trunc(VAMin + (VAMax - VAMin) * VRatio);
  end;
  Result := Color32(VR, VG, VB, VA);
end;

function TTrackColorerStatic.GetColorForSpeed(ASpeed: Double): TColor32;
var
  L, H, I, C: Integer;
  VIndex: Integer;
begin
  if FCount > 0 then begin
    if ASpeed >= FSpeedArray[FCount - 1] then begin
      Result := FMaxColorArray[FCount - 1];
    end else if ASpeed <= FSpeedArray[0] then begin
      Result := GetColor(FMinColorArray[0], FMaxColorArray[0], 0, FSpeedArray[0], ASpeed);
    end else begin
      L := 0;
      H := FCount - 1;
      while L <= H do begin
        I := (L + H) shr 1;
        if FSpeedArray[I] < ASpeed then begin
          C := -1;
        end else begin
          C := 1;
        end;
        if C < 0 then begin
          L := I + 1;
        end else begin
          H := I - 1;
        end;
      end;
      VIndex := L;
      Result := GetColor(FMinColorArray[VIndex], FMaxColorArray[VIndex], FSpeedArray[VIndex - 1], FSpeedArray[VIndex], ASpeed);
    end;
  end else begin
    Result := clBlack32;
  end;
end;

{ TMapLayerGPSTrackConfig }

constructor TMapLayerGPSTrackConfig.Create;
begin
  inherited Create;
  FVisible := True;
  FLineWidth := 5;
  FLastPointCount := 5000;
  FTrackColorerConfig := TTrackColorerConfig.Create;
  Add(FTrackColorerConfig, TConfigSaveLoadStrategyBasicProviderSubItem.Create('SpeedGrid'));

  FThreadConfig := TThreadConfig.Create(tpNormal);
  Add(FThreadConfig, TConfigSaveLoadStrategyBasicUseProvider.Create);
end;

procedure TMapLayerGPSTrackConfig.DoReadConfig(
  const AConfigData: IConfigDataProvider
);
begin
  inherited;
  if AConfigData <> nil then begin
    FVisible := AConfigData.ReadBool('Visible', FVisible);
    FLineWidth := AConfigData.ReadFloat('LineWidth', FLineWidth);
    FLastPointCount := AConfigData.ReadInteger('LastPointsCount', FLastPointCount);
    SetChanged;
  end;
end;

procedure TMapLayerGPSTrackConfig.DoWriteConfig(
  const AConfigData: IConfigDataWriteProvider
);
begin
  inherited;
  AConfigData.WriteBool('Visible', FVisible);
  AConfigData.WriteFloat('LineWidth', FLineWidth);
  AConfigData.WriteInteger('LastPointsCount', FLastPointCount);
end;

function TMapLayerGPSTrackConfig.GetLastPointCount: Integer;
begin
  LockRead;
  try
    Result := FLastPointCount;
  finally
    UnlockRead;
  end;
end;

function TMapLayerGPSTrackConfig.GetLineWidth: Double;
begin
  LockRead;
  try
    Result := FLineWidth;
  finally
    UnlockRead;
  end;
end;

function TMapLayerGPSTrackConfig.GetThreadConfig: IThreadConfig;
begin
  Result := FThreadConfig;
end;

function TMapLayerGPSTrackConfig.GetTrackColorerConfig: ITrackColorerConfig;
begin
  Result := FTrackColorerConfig;
end;

function TMapLayerGPSTrackConfig.GetVisible: Boolean;
begin
  LockRead;
  try
    Result := FVisible;
  finally
    UnlockRead;
  end;
end;

procedure TMapLayerGPSTrackConfig.SetLastPointCount(AValue: Integer);
begin
  LockWrite;
  try
    if FLastPointCount <> AValue then begin
      FLastPointCount := AValue;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

procedure TMapLayerGPSTrackConfig.SetLineWidth(AValue: Double);
begin
  LockWrite;
  try
    if FLineWidth <> AValue then begin
      FLineWidth := AValue;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

procedure TMapLayerGPSTrackConfig.SetVisible(AValue: Boolean);
begin
  LockWrite;
  try
    if FVisible <> AValue then begin
      FVisible := AValue;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

end.
