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

unit u_BitmapMapCombinerBMP;

interface

uses
  i_InternalPerformanceCounter,
  i_BitmapMapCombiner,
  i_RegionProcessParamsFrame,
  u_BitmapMapCombinerFactoryBase;


type
  TBitmapMapCombinerFactoryBMP = class(TBitmapMapCombinerFactoryBase)
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
  Types,
  gnugettext,
  t_CommonTypes,
  i_ImageLineProvider,
  i_NotifierOperation,
  i_BitmapTileProvider,
  u_ImageLineProvider,
  u_BmpWriter,
  u_BaseInterfacedObject,
  u_GeoFunc,
  u_ResStrings;

const
  BMP_MAX_WIDTH = 32768;
  BMP_MAX_HEIGHT = 32768;

type
  TBitmapMapCombinerBMP = class(TBaseInterfacedObject, IBitmapMapCombiner)
  private
    FProgressUpdate: IBitmapCombineProgressUpdate;
    FSaveRectCounter: IInternalPerformanceCounter;
    FPrepareDataCounter: IInternalPerformanceCounter;
    FGetLineCounter: IInternalPerformanceCounter;
  private
    procedure SaveRect(
      AOperationID: Integer;
      const ACancelNotifier: INotifierOperation;
      const AFileName: string;
      const AImageProvider: IBitmapTileProvider;
      const AMapRect: TRect;
      const ACombinerCustomParams: IInterface
    );
  public
    constructor Create(
      const AProgressUpdate: IBitmapCombineProgressUpdate;
      const ASaveRectCounter: IInternalPerformanceCounter;
      const APrepareDataCounter: IInternalPerformanceCounter;
      const AGetLineCounter: IInternalPerformanceCounter
    );
  end;

constructor TBitmapMapCombinerBMP.Create(
  const AProgressUpdate: IBitmapCombineProgressUpdate;
  const ASaveRectCounter: IInternalPerformanceCounter;
  const APrepareDataCounter: IInternalPerformanceCounter;
  const AGetLineCounter: IInternalPerformanceCounter
);
begin
  inherited Create;
  FProgressUpdate := AProgressUpdate;
  FSaveRectCounter := ASaveRectCounter;
  FPrepareDataCounter := APrepareDataCounter;
  FGetLineCounter := AGetLineCounter;
end;

procedure TBitmapMapCombinerBMP.SaveRect(
  AOperationID: Integer;
  const ACancelNotifier: INotifierOperation;
  const AFileName: string;
  const AImageProvider: IBitmapTileProvider;
  const AMapRect: TRect;
  const ACombinerCustomParams: IInterface
);
var
  I: Integer;
  VBMP: TBitmapFile;
  VLineBGR: Pointer;
  VSize: TPoint;
  VLineProvider: IImageLineProvider;
  VContext: TInternalPerformanceCounterContext;
begin
  VContext := FSaveRectCounter.StartOperation;
  try
    VSize := RectSize(AMapRect);

    if (VSize.X >= BMP_MAX_WIDTH) or (VSize.Y >= BMP_MAX_HEIGHT) then begin
      raise Exception.CreateFmt(
        SAS_ERR_ImageResolutionIsTooHigh,
        ['BMP', VSize.X, BMP_MAX_WIDTH, VSize.Y, BMP_MAX_HEIGHT]
      );
    end;

    VBMP := TBitmapFile.Create(AFileName, VSize.X, VSize.Y);
    try
      VLineProvider :=
        TImageLineProviderBGR.Create(
          FPrepareDataCounter,
          FGetLineCounter,
          AImageProvider,
          AMapRect
        );
      for I := 0 to VSize.Y - 1 do begin
        VLineBGR := VLineProvider.GetLine(AOperationID, ACancelNotifier, I);
        if VLineBGR <> nil then begin
          if not VBMP.WriteLine(I, VLineBGR) then begin
            raise Exception.Create(_('BMP: Line write failure!'));
          end;
        end else begin
          raise Exception.Create(_('BMP: Fill line failure!'));
        end;

        if ACancelNotifier.IsOperationCanceled(AOperationID) then begin
          Break;
        end;
        if I mod 256 = 0 then begin
          FProgressUpdate.Update(I / VSize.Y);
        end;
      end;
    finally
      VBMP.Free;
    end;
  finally
    FSaveRectCounter.FinishOperation(VContext);
  end;
end;

{ TBitmapMapCombinerFactoryBMP }

constructor TBitmapMapCombinerFactoryBMP.Create(
  const ACounterList: IInternalPerformanceCounterList
);
var
  VCounterList: IInternalPerformanceCounterList;
begin
  inherited Create(
    Point(0, 0),
    Point(BMP_MAX_WIDTH, BMP_MAX_HEIGHT),
    stsUnicode,
    'bmp',
    gettext_NoExtract('BMP (Bitmap Picture)')
  );
  VCounterList := ACounterList.CreateAndAddNewSubList('BMP');
  FSaveRectCounter := VCounterList.CreateAndAddNewCounter('SaveRect');
  FPrepareDataCounter := VCounterList.CreateAndAddNewCounter('PrepareData');
  FGetLineCounter := VCounterList.CreateAndAddNewCounter('GetLine');
end;

function TBitmapMapCombinerFactoryBMP.PrepareMapCombiner(
  const AParams: IRegionProcessParamsFrameMapCombine;
  const AProgressInfo: IBitmapCombineProgressUpdate
): IBitmapMapCombiner;
begin
  Result :=
    TBitmapMapCombinerBMP.Create(
      AProgressInfo,
      FSaveRectCounter,
      FPrepareDataCounter,
      FGetLineCounter
    );
end;

end.
