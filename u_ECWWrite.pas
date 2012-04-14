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

unit u_ECWWrite;

interface

uses
  SysUtils,
  LibECW,
  t_GeoTypes,
  i_OperationNotifier;

type
  TlineRGB = array[0..0] of single;
  PlineRGB = ^TlineRGB;

  TEcwRead = function(Line: Integer; var lineR,LineG,LineB:PLineRGB):boolean of object;

type
  TECWWrite = class
  private
    FReadDelegate: TEcwRead;
    FOperationID: Integer;
    FCancelNotifier: IOperationNotifier;
  public
    constructor Create;
    function Encode(
      AOperationID: Integer;
      const ACancelNotifier: IOperationNotifier;
      const FileName: string;
      Width,Height: cardinal;
      CompressRatio: Single;
      Hint: CompressHint;
      AReadDelegate: TEcwRead;
      const Datum, Projection: string;
      SizeUnits: TCellSizeUnits;
      CellIncrementX, CellIncrementY, OriginX, OriginY: double
    ): integer;
    property ReadDelegate: TEcwRead read FReadDelegate;
    property OperationID: Integer read FOperationID;
    property CancelNotifier: IOperationNotifier read FCancelNotifier;
  end;

implementation

constructor TECWWrite.Create;
begin
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
  Result := VECWWrite.ReadDelegate(
    nNextLine,
    PlineRGB(PPtr(InputArray)[0]),
    PlineRGB(PPtr(InputArray)[1]),
    PlineRGB(PPtr(InputArray)[2])
  );
end;

function CancelCallbackFunc(pClient: PNCSEcwCompressClient): Boolean; cdecl;
var
  VECWWrite: TECWWrite;
begin
  VECWWrite := TECWWrite(pClient.pClientData);
  Result := VECWWrite.CancelNotifier.IsOperationCanceled(VECWWrite.OperationID);
end;

function TECWWrite.Encode(
  AOperationID: Integer;
  const ACancelNotifier: IOperationNotifier;
  const FileName:string;
  Width, Height: cardinal;
  CompressRatio: Single;
  Hint: CompressHint;
  AReadDelegate: TEcwRead;
  const Datum, Projection: string;
  SizeUnits: TCellSizeUnits;
  CellIncrementX, CellIncrementY, OriginX, OriginY: double
): Integer;
var
  VNCSError: NCSError;
  VEcwData: PNCSEcwCompressClient;
begin
  Result := integer(NCS_MAX_ERROR_NUMBER);
  FReadDelegate := AReadDelegate;
  FOperationID := AOperationID;
  FCancelNotifier := ACancelNotifier;
  VEcwData := NCSEcwCompressAllocClient();
  try
    FillChar(VEcwData^, SizeOf(NCSEcwCompressClient), 0);

    VEcwData.pClientData := Self;
    VEcwData.nInputBands := 3;
    VEcwData.nInOutSizeX := Width;
    VEcwData.nInOutSizeY := Height;
    VEcwData.eCompressFormat := COMPRESS_RGB;
    VEcwData.eCompressHint := Hint;
    VEcwData.fTargetCompression := CompressRatio;
    VEcwData.eCellSizeUnits := CellSizeUnits(SizeUnits);
    VEcwData.fCellIncrementX := CellIncrementX;
    VEcwData.fCellIncrementY := CellIncrementY;
    VEcwData.fOriginX := OriginX;
    VEcwData.fOriginY := OriginY;
    VEcwData.pReadCallback := ReadCallbackFunc;
    VEcwData.pStatusCallback := nil;
    VEcwData.pCancelCallback := CancelCallbackFunc;

    if Length(Datum) < Length(VEcwData.szDatum) then begin
      Move(Datum[1], VEcwData.szDatum[0], Length(Datum));
    end else begin
      raise Exception.Create('ECW Encode: Datum string is too long!');
    end;
    if Length(Projection) < Length(VEcwData.szProjection) then begin
      Move(Projection[1], VEcwData.szProjection[0], Length(Projection));
    end else begin
      raise Exception.Create('ECW Encode: Projection string is too long!');
    end;
    if Length(FileName) < Length(VEcwData.szOutputFilename) then begin
      Move(FileName[1], VEcwData.szOutputFilename[0], Length(FileName));
    end else begin
      raise Exception.Create('ECW Encode: FileName string is too long!');
    end;

    VNCSError:= NCSEcwCompressOpen(VEcwData, False);
    if VNCSError = NCS_SUCCESS then begin
      VNCSError := NCSEcwCompress(VEcwData);
      if VNCSError = NCS_SUCCESS then begin
        VNCSError := NCSEcwCompressClose(VEcwData);
      end;
    end;
    Result := Integer(VNCSError);
  finally
    NCSEcwCompressFreeClient(VEcwData);
  end; 
end;

end.
