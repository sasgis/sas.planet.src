{******************************************************************************}
{* This file is part of SAS.Planet project.                                   *}
{*                                                                            *}
{* Copyright (C) 2007-2022, SAS.Planet development team.                      *}
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
    iftWebp
  );

  TImageFormatTypeSet = set of TImageFormatType;

const
  CImageFormatAll: TImageFormatTypeSet =
    [iftAuto, iftBmp, iftGif, iftJpeg, iftPng8bpp, iftPng24bpp, iftPng32bpp, iftWebp];

type
  TfrImageFormatSelect = class(TFrame)
    pnlImageFormat: TPanel;
    lblImageFormat: TLabel;
    cbbImageFormat: TComboBox;
    lblCompression: TLabel;
    seCompression: TSpinEdit;
    procedure cbbImageFormatChange(Sender: TObject);
  private
    FOnChange: TNotifyEvent;
    FImageFormats: TImageFormatTypeSet;
    FIndexByImageFormat: array[TImageFormatType] of Integer;
    FBitmapTileSaveLoadFactory: IBitmapTileSaveLoadFactory;
    procedure SetupUI;
    function GetSelectedFormat: TImageFormatType;
    procedure SetSelectedFormat(const AValue: TImageFormatType);
  public
    function GetBitmapTileSaver(const AMap, AOverlay: IMapType): IBitmapTileSaver;
    function GetContentType(const AMap, AOverlay: IMapType): AnsiString;
    property SelectedFormat: TImageFormatType read GetSelectedFormat;
  public
    constructor Create(
      const ALanguageManager: ILanguageManager;
      const ABitmapTileSaveLoadFactory: IBitmapTileSaveLoadFactory;
      const AImageFormats: TImageFormatTypeSet;
      const ASelectedFormat: TImageFormatType = iftAuto;
      const AOnChange: TNotifyEvent = nil
    ); reintroduce;
    procedure RefreshTranslation; override;
    procedure Show(AParent: TWinControl);
  end;

implementation

uses
  gnugettext;

{$R *.dfm}

resourcestring
  rsImageFormatTypeAuto = 'Auto';

var
  CImageFormatCaption: array[TImageFormatType] of string = (
    rsImageFormatTypeAuto,
    'BMP',
    'GIF',
    'JPEG',
    'PNG 8 BPP',
    'PNG 24 BPP',
    'PNG 32 BPP',
    'WEBP'
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
    'image/webp'
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
  FOnChange := AOnChange;

  SetupUI;
  SetSelectedFormat(ASelectedFormat);
  cbbImageFormatChange(nil);
end;

procedure TfrImageFormatSelect.cbbImageFormatChange(Sender: TObject);
var
  VQuality: Boolean;
  VCompression: Boolean;
  VFormat: TImageFormatType;
begin
  VFormat := GetSelectedFormat;

  VQuality := VFormat in [iftJpeg, iftWebp];
  VCompression := VFormat in [iftPng24bpp, iftPng32bpp];

  if VQuality then begin
    lblCompression.Caption := _('Quality:');
    seCompression.MinValue := 0;
    seCompression.MaxValue := 100;
    seCompression.Value := 75;
  end else
  if VCompression then begin
    lblCompression.Caption := _('Compression:');
    seCompression.MinValue := 0;
    seCompression.MaxValue := 9;
    seCompression.Value := 2;
  end;

  seCompression.Visible := VQuality or VCompression;
  lblCompression.Visible := seCompression.Visible;

  if Assigned(FOnChange) then begin
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
end;

procedure TfrImageFormatSelect.Show(AParent: TWinControl);
begin
  Self.Parent := AParent;
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
    if VContentType <> nil then begin
      Result := VContentType.GetContentType;
    end else begin
      Result := '';
    end;
  end;
end;

function TfrImageFormatSelect.GetBitmapTileSaver(const AMap, AOverlay: IMapType): IBitmapTileSaver;
var
  VFormat: TImageFormatType;
  VContentType: IContentTypeInfoBitmap;
begin
  VFormat := GetSelectedFormat;
  case VFormat of
    iftAuto: begin
      VContentType := GetContentTypeInfo(AMap, AOverlay);
      if VContentType <> nil then begin
        Result := VContentType.GetSaver;
      end else begin
        Result := nil;
      end;
    end;
    iftBmp: Result := FBitmapTileSaveLoadFactory.CreateBmpSaver;
    iftGif: Result := FBitmapTileSaveLoadFactory.CreateGifSaver;
    iftJpeg: Result := FBitmapTileSaveLoadFactory.CreateJpegSaver(seCompression.Value);
    iftPng8bpp: Result := FBitmapTileSaveLoadFactory.CreatePngSaver(i8bpp, seCompression.Value);
    iftPng24bpp: Result := FBitmapTileSaveLoadFactory.CreatePngSaver(i24bpp, seCompression.Value);
    iftPng32bpp: Result := FBitmapTileSaveLoadFactory.CreatePngSaver(i32bpp, seCompression.Value);
    iftWebp: Result := FBitmapTileSaveLoadFactory.CreateWebpSaver(seCompression.Value);
  else
    raise Exception.CreateFmt('Unexpected TImageFormatType value: %d', [Integer(VFormat)]);
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
