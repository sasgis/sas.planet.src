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

unit u_ECWJP2Write;

interface

uses
  SysUtils,
  libecwj2,
  t_ECW,
  i_NotifierOperation;

type
  TLineRGB = array[0..0] of single;
  PLineRGB = ^TLineRGB;

  TEcwRead =
    function(
      Line: Integer;
      var lineR, LineG, LineB: PLineRGB
    ): boolean of object;

  TECWCompressionStatistics = record
    // Actual compression rate achieved - ratio of input data size to output file size
    ActualCompression: Single;
    // Time taken to perform the complete compression, in seconds
    CompressionSeconds: Double;
    // MB/s throughput during the compression process
    CompressionMBSec: Double;
    // Total size of the output file in bytes
    OutputSize: Int64;
  end;
  PECWCompressionStatistics = ^TECWCompressionStatistics;

type
  TECWWrite = class
  private
    FReadDelegate: TEcwRead;
    FOperationID: Integer;
    FCancelNotifier: INotifierOperation;
  public
    constructor Create;
    function Encode(
      const AOperationID: Integer;
      const ACancelNotifier: INotifierOperation;
      const AFileName: AnsiString;
      const AWidth: Cardinal;
      const AHeight: Cardinal;
      const ACompressRatio: Single;
      const AHint: CompressHint;
      const AReadDelegate: TEcwRead;
      const ADatum: AnsiString;
      const AProjection: AnsiString;
      const ASizeUnits: TCellSizeUnits;
      const ACellIncrementX: Double;
      const ACellIncrementY: Double;
      const AOriginX: Double;
      const AOriginY: Double;
      const AOutEncodeInfo: PECWCompressionStatistics = nil
    ): Integer;
    class function ErrorCodeToString(const AErrorCode: Integer): string;
  end;

implementation

constructor TECWWrite.Create;
begin
  inherited Create;
  if not InitLibEcw then begin
    raise Exception.Create('InitLibEcw error!');
  end;
end;

function ReadCallbackFunc(
  pClient: PNCSEcwCompressClient;
  nNextLine: Cardinal;
  InputArray: Pointer
): Boolean; cdecl;
type
  TPtr = array [0..2] of Pointer;
  PPtr = ^TPtr;
var
  VECWWrite: TECWWrite;
begin
  VECWWrite := TECWWrite(pClient.pClientData);
  Result := VECWWrite.FReadDelegate(
    nNextLine,
    PLineRGB(PPtr(InputArray)[0]),
    PLineRGB(PPtr(InputArray)[1]),
    PLineRGB(PPtr(InputArray)[2])
  );
end;

function CancelCallbackFunc(pClient: PNCSEcwCompressClient): Boolean; cdecl;
var
  VECWWrite: TECWWrite;
begin
  VECWWrite := TECWWrite(pClient.pClientData);
  Result := VECWWrite.FCancelNotifier.IsOperationCanceled(VECWWrite.FOperationID);
end;

function TECWWrite.Encode(
  const AOperationID: Integer;
  const ACancelNotifier: INotifierOperation;
  const AFileName: AnsiString;
  const AWidth: Cardinal;
  const AHeight: Cardinal;
  const ACompressRatio: Single;
  const AHint: CompressHint;
  const AReadDelegate: TEcwRead;
  const ADatum: AnsiString;
  const AProjection: AnsiString;
  const ASizeUnits: TCellSizeUnits;
  const ACellIncrementX: Double;
  const ACellIncrementY: Double;
  const AOriginX: Double;
  const AOriginY: Double;
  const AOutEncodeInfo: PECWCompressionStatistics
): Integer;
var
  VNCSError: NCSError;
  VEcwData: PNCSEcwCompressClient;
begin
  FReadDelegate := AReadDelegate;
  FOperationID := AOperationID;
  FCancelNotifier := ACancelNotifier;

  {$IF CompilerVersion < 23}
  VNCSError := NCS_MAX_ERROR_NUMBER; // prevent compiler warning
  {$IFEND}

  VEcwData := NCSEcwCompressAllocClient();
  try
    VEcwData.pClientData := Self;
    VEcwData.nInputBands := 3;
    VEcwData.nInOutSizeX := AWidth;
    VEcwData.nInOutSizeY := AHeight;
    VEcwData.eCompressFormat := COMPRESS_RGB;
    VEcwData.eCompressHint := AHint;
    VEcwData.fTargetCompression := ACompressRatio;
    VEcwData.eCellSizeUnits := CellSizeUnits(ASizeUnits);
    VEcwData.fCellIncrementX := ACellIncrementX;
    VEcwData.fCellIncrementY := ACellIncrementY;
    VEcwData.fOriginX := AOriginX;
    VEcwData.fOriginY := AOriginY;
    VEcwData.pReadCallback := ReadCallbackFunc;
    VEcwData.pStatusCallback := nil;
    VEcwData.pCancelCallback := CancelCallbackFunc;

    if Length(ADatum) < Length(VEcwData.szDatum) then begin
      FillChar(VEcwData.szDatum[0], Length(VEcwData.szDatum), 0);
      Move(ADatum[1], VEcwData.szDatum[0], Length(ADatum));
    end else begin
      raise Exception.Create('ECW Encode: Datum string is too long!');
    end;

    if Length(AProjection) < Length(VEcwData.szProjection) then begin
      FillChar(VEcwData.szProjection[0], Length(VEcwData.szProjection), 0);
      Move(AProjection[1], VEcwData.szProjection[0], Length(AProjection));
    end else begin
      raise Exception.Create('ECW Encode: Projection string is too long!');
    end;

    if Length(AFileName) < Length(VEcwData.szOutputFilename) then begin
      Move(AFileName[1], VEcwData.szOutputFilename[0], Length(AFileName));
    end else begin
      raise Exception.Create('ECW Encode: FileName string is too long!');
    end;

    VNCSError := NCSEcwCompressOpen(VEcwData, False);
    if VNCSError = NCS_SUCCESS then begin
      VNCSError := NCSEcwCompress(VEcwData);
      if VNCSError = NCS_SUCCESS then begin
        VNCSError := NCSEcwCompressClose(VEcwData);
        if (VNCSError = NCS_SUCCESS) and (AOutEncodeInfo <> nil) then begin
          AOutEncodeInfo.ActualCompression := VEcwData.fActualCompression;
          AOutEncodeInfo.CompressionSeconds := VEcwData.fCompressionSeconds;
          AOutEncodeInfo.CompressionMBSec := VEcwData.fCompressionMBSec;
          AOutEncodeInfo.OutputSize := VEcwData.nOutputSize;
        end;
      end;
    end;
  finally
    NCSEcwCompressFreeClient(VEcwData);
  end;

  Result := Integer(VNCSError);
end;

class function TECWWrite.ErrorCodeToString(const AErrorCode: Integer): string;
begin
  if (AErrorCode >= Integer(NCS_SUCCESS)) and (AErrorCode <= Integer(NCS_MAX_ERROR_NUMBER)) then begin
    Result := NCSErrorTextArray[NCSError(AErrorCode)];
  end else begin
    Result := 'Unknown error code!';
  end;
end;

end.
