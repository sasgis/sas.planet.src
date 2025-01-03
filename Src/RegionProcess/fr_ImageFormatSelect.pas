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

unit fr_ImageFormatSelect;

interface

uses
  Classes,
  Controls,
  StdCtrls,
  SysUtils,
  ExtCtrls,
  Spin,
  i_MapType,
  i_BitmapTileSaveLoad,
  i_BitmapTileSaveLoadFactory,
  i_ContentTypeInfo,
  i_LanguageManager,
  u_CommonFormAndFrameParents;

type
  TImageFormatType = (
    iftAuto,
    iftBmp,
    iftGif,
    iftJpeg,
    iftPng8bpp,
    iftPng24bpp,
    iftPng32bpp,
    iftWebp,
    iftWebpLossless
  );

  TImageFormatTypeSet = set of TImageFormatType;

const
  CImageFormatAll: TImageFormatTypeSet = [
    iftAuto,
    iftBmp,
    iftGif,
    iftJpeg,
    iftPng8bpp,
    iftPng24bpp,
    iftPng32bpp,
    iftWebp,
    iftWebpLossless
  ];

type
  TfrImageFormatSelect = class(TFrame)
    pnlImageFormat: TPanel;
    lblImageFormat: TLabel;
    cbbImageFormat: TComboBox;
    lblCompression: TLabel;
    seCompression: TSpinEdit;
    procedure cbbImageFormatChange(Sender: TObject);
  private
    seQuality: TSpinEdit;
  private
    FOnChange: TNotifyEvent;
    FSelectedFormat: TImageFormatType;
    FImageFormats: TImageFormatTypeSet;
    FIndexByImageFormat: array[TImageFormatType] of Integer;
    FBitmapTileSaveLoadFactory: IBitmapTileSaveLoadFactory;
    procedure SetupUI;
    function GetSelectedFormat: TImageFormatType;
    procedure SetSelectedFormat(const AValue: TImageFormatType);
    function IsSupportedContentType(const AContentType: IContentTypeInfoBitmap): Boolean;
  public
    function GetBitmapTileSaver(const AMap, AOverlay: IMapType): IBitmapTileSaver; overload;
    function GetBitmapTileSaver(const AFormat: TImageFormatType): IBitmapTileSaver; overload;
    function GetContentType(const AMap, AOverlay: IMapType): AnsiString;
    property SelectedFormat: TImageFormatType read GetSelectedFormat;
  protected
    procedure OnShow(const AIsFirsTime: Boolean); override;
    procedure RefreshTranslation; override;
  public
    constructor Create(
      const ALanguageManager: ILanguageManager;
      const ABitmapTileSaveLoadFactory: IBitmapTileSaveLoadFactory;
      const AImageFormats: TImageFormatTypeSet;
      const ASelectedFormat: TImageFormatType = iftAuto;
      const AOnChange: TNotifyEvent = nil
    ); reintroduce;
    procedure Show(AParent: TWinControl);
  end;

implementation

uses
  gnugettext,
  u_ContentTypeFunc;

{$R *.dfm}

resourcestring
  rsImageFormatTypeAuto = 'Auto';

var
  CImageFormatCaption: array[TImageFormatType] of string = (
    rsImageFormatTypeAuto,
    'BMP',
    'GIF',
    'JPEG',
    'PNG (8 BPP)',
    'PNG (24 BPP)',
    'PNG (32 BPP)',
    'WEBP',
    'WEBP (Lossless)'
  );

const
  CImageFormatContentType: array[TImageFormatType] of AnsiString = (
    '',
    'image/bmp',
    'image/gif',
    'image/jpeg',
    'image/png', // 8 bpp
    'image/png', // 24 bpp
    'image/png', // 32 bpp
    'image/webp',
    'image/webp' // lossless
  );

{ TfrImageFormatSelect }

constructor TfrImageFormatSelect.Create(
  const ALanguageManager: ILanguageManager;
  const ABitmapTileSaveLoadFactory: IBitmapTileSaveLoadFactory;
  const AImageFormats: TImageFormatTypeSet;
  const ASelectedFormat: TImageFormatType;
  const AOnChange: TNotifyEvent
);
begin
  Assert(ASelectedFormat in AImageFormats);

  inherited Create(ALanguageManager);

  FBitmapTileSaveLoadFactory := ABitmapTileSaveLoadFactory;
  FImageFormats := AImageFormats;
  FSelectedFormat := ASelectedFormat;
  FOnChange := AOnChange;

  seQuality := TSpinEdit.Create(Self);
  seQuality.Name := 'seQuality';
  seQuality.Parent := pnlImageFormat;
  seQuality.MinValue := 1;
  seQuality.MaxValue := 100;
  seQuality.Value := 75;
  seQuality.Top := seCompression.Top;
  seQuality.Left := seCompression.Left;
  seQuality.Height := seCompression.Height;
  seQuality.Width := seCompression.Width;
  seQuality.TabOrder := seCompression.TabOrder;

  SetupUI;

  FPropertyState := CreateComponentPropertyState(
    Self, [], [], True, False, True, True
  );
end;

procedure TfrImageFormatSelect.cbbImageFormatChange(Sender: TObject);
var
  VQuality: Boolean;
  VCompression: Boolean;
  VFormat: TImageFormatType;
begin
  VFormat := GetSelectedFormat;

  VQuality := VFormat in [iftJpeg, iftWebp];
  VCompression := VFormat in [iftPng8bpp, iftPng24bpp, iftPng32bpp];

  if VQuality then begin
    lblCompression.Caption := _('Quality:');
  end else
  if VCompression then begin
    lblCompression.Caption := _('Compression:');
  end;

  seQuality.Visible := VQuality;
  seCompression.Visible := VCompression;

  lblCompression.Visible := VQuality or VCompression;

  if Assigned(Sender) and Assigned(FOnChange) then begin
    FOnChange(Self);
  end;
end;

procedure TfrImageFormatSelect.SetupUI;
var
  I: TImageFormatType;
begin
  cbbImageFormat.Clear;
  CImageFormatCaption[iftAuto] := _(rsImageFormatTypeAuto);

  for I := Low(TImageFormatType) to High(TImageFormatType) do begin
    if I in FImageFormats then begin
      FIndexByImageFormat[I] := cbbImageFormat.Items.AddObject(CImageFormatCaption[I], TObject(I));
    end else begin
      FIndexByImageFormat[I] := -1;
    end;
  end;
  cbbImageFormat.DropDownCount := cbbImageFormat.Items.Count;
end;

procedure TfrImageFormatSelect.Show(AParent: TWinControl);
begin
  Self.Parent := AParent;
end;

procedure TfrImageFormatSelect.OnShow(const AIsFirsTime: Boolean);
begin
  inherited; // restore state

  if AIsFirsTime then begin
    if cbbImageFormat.ItemIndex < 0 then begin
      SetSelectedFormat(FSelectedFormat);
    end;
    cbbImageFormatChange(nil);
  end;
end;

procedure TfrImageFormatSelect.SetSelectedFormat(const AValue: TImageFormatType);
var
  I: Integer;
begin
  I := FIndexByImageFormat[AValue];
  if I >= 0 then begin
    cbbImageFormat.ItemIndex := I;
  end else begin
    raise Exception.Create('ImageFormatType index is out of range!');
  end;
end;

function TfrImageFormatSelect.GetSelectedFormat: TImageFormatType;
var
  VIndex: Integer;
begin
  VIndex := cbbImageFormat.ItemIndex;
  if VIndex >= 0 then begin
    Result := TImageFormatType(cbbImageFormat.Items.Objects[VIndex]);
  end else begin
    raise Exception.Create('Invalid item index!');
  end;
end;

function GetContentTypeInfo(const AMap, AOverlay: IMapType): IContentTypeInfoBitmap;
var
  VMap: IMapType;
begin
  if AMap <> nil then begin
    VMap := AMap;
  end else begin
    VMap := AOverlay;
  end;
  if (VMap = nil) or not Supports(VMap.ContentType, IContentTypeInfoBitmap, Result) then begin
    Result := nil;
  end;
end;

function TfrImageFormatSelect.GetContentType(const AMap, AOverlay: IMapType): AnsiString;
var
  VFormat: TImageFormatType;
  VContentType: IContentTypeInfoBitmap;
begin
  VFormat := GetSelectedFormat;
  if VFormat <> iftAuto then begin
    Result := CImageFormatContentType[VFormat];
  end else begin
    VContentType := GetContentTypeInfo(AMap, AOverlay);
    if IsSupportedContentType(VContentType) then begin
      Result := VContentType.GetContentType;
    end else begin
      Result := '';
    end;
  end;
end;

function TfrImageFormatSelect.IsSupportedContentType(const AContentType: IContentTypeInfoBitmap): Boolean;
var
  VContentType: AnsiString;
begin
  Result := False;
  if AContentType <> nil then begin
    VContentType := AContentType.GetContentType;
    if IsJpegContentType(VContentType) then begin
      Result := iftJpeg in FImageFormats;
    end else
    if IsPngContentType(VContentType) then begin
      Result :=
        (iftPng8bpp in FImageFormats) or
        (iftPng24bpp in FImageFormats) or
        (iftPng32bpp in FImageFormats);
    end else
    if IsBmpContentType(VContentType) then begin
      Result := iftBmp in FImageFormats;
    end else
    if IsGifContentType(VContentType) then begin
      Result := iftGif in FImageFormats;
    end else
    if IsWebpContentType(VContentType) then begin
      Result :=
        (iftWebp in FImageFormats) or
        (iftWebpLossless in FImageFormats);
    end;
  end;
end;

function TfrImageFormatSelect.GetBitmapTileSaver(const AMap, AOverlay: IMapType): IBitmapTileSaver;
var
  VFormat: TImageFormatType;
  VContentType: IContentTypeInfoBitmap;
begin
  VFormat := GetSelectedFormat;
  if VFormat = iftAuto then begin
    VContentType := GetContentTypeInfo(AMap, AOverlay);
    if IsSupportedContentType(VContentType) then begin
      Result := VContentType.GetSaver;
    end else begin
      Result := nil;
    end;
  end else begin
    Result := GetBitmapTileSaver(VFormat);
  end;
end;

function TfrImageFormatSelect.GetBitmapTileSaver(const AFormat: TImageFormatType): IBitmapTileSaver;
var
  VValue: Integer;
begin
  if (AFormat = iftAuto) or not (AFormat in FImageFormats) then begin
    Result := nil;
    Assert(False);
    Exit;
  end;

  if seQuality.Visible then begin
    VValue := seQuality.Value;
  end else
  if seCompression.Visible then begin
    VValue := seCompression.Value;
  end else begin
    VValue := 0;
  end;

  case AFormat of
    iftBmp: Result := FBitmapTileSaveLoadFactory.CreateBmpSaver;
    iftGif: Result := FBitmapTileSaveLoadFactory.CreateGifSaver;
    iftJpeg: Result := FBitmapTileSaveLoadFactory.CreateJpegSaver(VValue);
    iftPng8bpp: Result := FBitmapTileSaveLoadFactory.CreatePngSaver(i8bpp, VValue);
    iftPng24bpp: Result := FBitmapTileSaveLoadFactory.CreatePngSaver(i24bpp, VValue);
    iftPng32bpp: Result := FBitmapTileSaveLoadFactory.CreatePngSaver(i32bpp, VValue);
    iftWebp: Result := FBitmapTileSaveLoadFactory.CreateWebpSaver(VValue);
    iftWebpLossless: Result := FBitmapTileSaveLoadFactory.CreateWebpLosslessSaver;
  else
    raise Exception.CreateFmt('Unexpected TImageFormatType value: %d', [Integer(AFormat)]);
  end;
end;

procedure TfrImageFormatSelect.RefreshTranslation;
var
  I: Integer;
begin
  inherited;

  I := cbbImageFormat.ItemIndex;
  SetupUI;
  cbbImageFormat.ItemIndex := I;
end;

end.
