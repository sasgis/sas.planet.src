{******************************************************************************}
{* SAS.Planet (SAS.Планета)                                                   *}
{* Copyright (C) 2007-2015, SAS.Planet development team.                      *}
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

unit u_BitmapMapCombinerRAW;

interface

uses
  i_InternalPerformanceCounter,
  i_BitmapMapCombiner,
  i_RegionProcessParamsFrame,
  u_BitmapMapCombinerFactoryBase;

type
  TBitmapMapCombinerFactoryRAW = class(TBitmapMapCombinerFactoryBase)
  private
    FSaveRectCounter: IInternalPerformanceCounter;
    FPrepareDataCounter: IInternalPerformanceCounter;
    FGetLineCounter: IInternalPerformanceCounter;
  protected
    function PrepareMapCombiner(
      const AParams: IRegionProcessParamsFrameMapCombine;
      const AProgressInfo: IBitmapCombineProgressUpdate
    ): IBitmapMapCombiner; override;
  public
    constructor Create(
      const ACounterList: IInternalPerformanceCounterList
    );
  end;

implementation

uses
  SysUtils,
  Classes,
  Types,
  gnugettext,
  ALString,
  t_Bitmap32,
  t_CommonTypes,
  t_MapCombineOptions,
  i_NotifierOperation,
  i_BitmapTileProvider,
  i_ImageLineProvider,
  u_BaseInterfacedObject,
  u_ImageLineProvider,
  u_GeoFunc;

type
  TBitmapMapCombinerRAW = class(TBaseInterfacedObject, IBitmapMapCombiner)
  private
    FProgressUpdate: IBitmapCombineProgressUpdate;
    FSaveRectCounter: IInternalPerformanceCounter;
    FPrepareDataCounter: IInternalPerformanceCounter;
    FGetLineCounter: IInternalPerformanceCounter;
    FBgColor: TColor32;
    FWithAlpha: Boolean;
    FOperationID: Integer;
    FCancelNotifier: INotifierOperation;
  private
    procedure SaveRect(
      AOperationID: Integer;
      const ACancelNotifier: INotifierOperation;
      const AFileName: string;
      const AImageProvider: IBitmapTileProvider;
      const AMapRect: TRect
    );
  public
    constructor Create(
      const AProgressUpdate: IBitmapCombineProgressUpdate;
      const ASaveRectCounter: IInternalPerformanceCounter;
      const APrepareDataCounter: IInternalPerformanceCounter;
      const AGetLineCounter: IInternalPerformanceCounter;
      const ABgColor: TColor32;
      AWithAlpha: Boolean
    );
  end;

constructor TBitmapMapCombinerRAW.Create(
  const AProgressUpdate: IBitmapCombineProgressUpdate;
  const ASaveRectCounter: IInternalPerformanceCounter;
  const APrepareDataCounter: IInternalPerformanceCounter;
  const AGetLineCounter: IInternalPerformanceCounter;
  const ABgColor: TColor32;
  AWithAlpha: Boolean
);
begin
  inherited Create;
  FProgressUpdate := AProgressUpdate;
  FSaveRectCounter := ASaveRectCounter;
  FPrepareDataCounter := APrepareDataCounter;
  FGetLineCounter := AGetLineCounter;
  FBgColor := ABgColor;
  FWithAlpha := AWithAlpha;
end;

procedure TBitmapMapCombinerRAW.SaveRect(
  AOperationID: Integer;
  const ACancelNotifier: INotifierOperation;
  const AFileName: string;
  const AImageProvider: IBitmapTileProvider;
  const AMapRect: TRect
);
var
  i: Integer;
  VRawFile: TFileStream;
  VMetaFile: TFileStream;
  VLine: Pointer;
  VSize: TPoint;
  VLineProvider: IImageLineProvider;
  VBytesPerPix: Byte;
  VMetaInfo: AnsiString;
  VBgColor: TColor32Entry;
  VContext: TInternalPerformanceCounterContext;
begin
  FOperationID := AOperationID;
  FCancelNotifier := ACancelNotifier;

  VContext := FSaveRectCounter.StartOperation;
  try
  VSize := RectSize(AMapRect);

  VRawFile := TFileStream.Create(AFileName, fmCreate);
  try
    if FWithAlpha then begin
      VBytesPerPix := 4;
      VLineProvider :=
        TImageLineProviderRGBA.Create(
          FPrepareDataCounter,
          FGetLineCounter,
          AImageProvider,
          AMapRect
        );
    end else begin
      VBytesPerPix := 3;
      VLineProvider :=
        TImageLineProviderRGB.Create(
          FPrepareDataCounter,
          FGetLineCounter,
          AImageProvider,
          AMapRect
        );
    end;
    VBgColor.ARGB := FBgColor;
    VMetaInfo := '';
    VMetaInfo := VMetaInfo + 'Width=' + ALIntToStr(VSize.X) + #13#10;
    VMetaInfo := VMetaInfo + 'Height=' + ALIntToStr(VSize.Y) + #13#10;
    if FWithAlpha then begin
      VMetaInfo := VMetaInfo + 'Bit/pixel=32' + #13#10;
      VMetaInfo := VMetaInfo + 'ByteOrder=RGBA' + #13#10;
      VMetaInfo := VMetaInfo + 'DefaultFill=#' + AlIntToHex(VBgColor.R, 2) + AlIntToHex(VBgColor.G, 2) + AlIntToHex(VBgColor.B, 2) + AlIntToHex(VBgColor.A, 2) + #13#10;
    end else begin
      VMetaInfo := VMetaInfo + 'Bit/pixel=24' + #13#10;
      VMetaInfo := VMetaInfo + 'ByteOrder=RGB' + #13#10;
      VMetaInfo := VMetaInfo + 'DefaultFill=#' + AlIntToHex(VBgColor.R, 2) + AlIntToHex(VBgColor.G, 2) + AlIntToHex(VBgColor.B, 2) + #13#10;
    end;

    VMetaFile := TFileStream.Create(AFileName + '.meta', fmCreate);
    try
      VMetaFile.WriteBuffer(VMetaInfo[1], Length(VMetaInfo));
    finally
      FreeAndNil(VMetaFile);
    end;

    for i := 0 to VSize.Y - 1 do begin
      VLine := VLineProvider.GetLine(AOperationID, ACancelNotifier, i);
      if VLine <> nil then begin
        VRawFile.WriteBuffer(VLine^, VSize.X * VBytesPerPix);
      end else begin
        raise Exception.Create(_('RAW: Fill line failure!'));
      end;

      if FCancelNotifier.IsOperationCanceled(FOperationID) then begin
        Break;
      end;
      if i mod 256 = 0 then begin
        FProgressUpdate.Update(i / VSize.Y);
      end;
    end;
  finally
    VRawFile.Free;
  end;
  finally
    FSaveRectCounter.FinishOperation(VContext);
  end;
end;

{ TBitmapMapCombinerFactoryRAW }

constructor TBitmapMapCombinerFactoryRAW.Create(
  const ACounterList: IInternalPerformanceCounterList
);
var
  VCounterList: IInternalPerformanceCounterList;
begin
  inherited Create(
    Point(0, 0),
    Point(1000000, MaxInt),
    stsUnicode,
    'raw',
    gettext_NoExtract('RAW (Simple bitmap graphic)'),
    [mcAlphaCheck]
  );
  VCounterList := ACounterList.CreateAndAddNewSubList('RAW');
  FSaveRectCounter := VCounterList.CreateAndAddNewCounter('SaveRect');
  FPrepareDataCounter := VCounterList.CreateAndAddNewCounter('PrepareData');
  FGetLineCounter := VCounterList.CreateAndAddNewCounter('GetLine');
end;

function TBitmapMapCombinerFactoryRAW.PrepareMapCombiner(
  const AParams: IRegionProcessParamsFrameMapCombine;
  const AProgressInfo: IBitmapCombineProgressUpdate
): IBitmapMapCombiner;
begin
  Result :=
    TBitmapMapCombinerRAW.Create(
      AProgressInfo,
      FSaveRectCounter,
      FPrepareDataCounter,
      FGetLineCounter,
      AParams.BGColor,
      AParams.CustomOptions.IsSaveAlfa
    );
end;

end.
