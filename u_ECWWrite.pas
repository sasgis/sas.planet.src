{******************************************************************************}
{* SAS.Planet (SAS.Планета)                                                   *}
{* Copyright (C) 2007-2011, SAS.Planet development team.                      *}
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
  ECWwriter,
  ECWreader,
  t_GeoTypes,
  i_EcwDll,
  i_OperationNotifier;

type
  TlineRGB = array[0..0] of single;
  PlineRGB = ^TlineRGB;

  TEcwRead = function(Line:cardinal; var lineR,LineG,LineB:PLineRGB):boolean of object;

type
  TECWWrite = class
  private
    FEcwDll: IEcwDll;
    FEcwData: PNCSEcwCompressClient;
    FReadDelegate: TEcwRead;
    FOperationID: Integer;
    FCancelNotifier: IOperationNotifier;
  public
    constructor Create(AEcwDll: IEcwDll);
    function Encode(
      AOperationID: Integer;
      ACancelNotifier: IOperationNotifier;
      FileName:string;
      Width,Height:cardinal;
      CompressRatio:Single;
      Hint:CompressHint;
      AReadDelegate:TEcwRead;
      Datum,Projection:string;
      SizeUnits:TCellSizeUnits;
      CellIncrementX,CellIncrementY,OriginX,OriginY:double
    ):integer;
  end;

implementation

constructor TECWWrite.Create(AEcwDll: IEcwDll);
begin
  FEcwDll := AEcwDll;
  FEcwData := FEcwDll.CompressAllocClient;
end;

function ReadCallbackFunc(pClient:PNCSEcwCompressClient;nNextLine:cardinal;InputArray:Pointer):boolean; cdecl;
type
  Tptr = array [0..2] of pointer;
  Pptr=^Tptr;
var
  VECWWrite: TECWWrite;
begin
  VECWWrite := TECWWrite(pClient.pClientData);
  result := VECWWrite.FReadDelegate(nNextLine,PlineRGB(PPtr(InputArray)[0]),PlineRGB(PPtr(InputArray)[1]),PlineRGB(PPtr(InputArray)[2]));
end;

function cancel(pClient:PNCSEcwCompressClient):boolean; cdecl;
var
  VECWWrite: TECWWrite;
begin
  VECWWrite := TECWWrite(pClient.pClientData);
  result:= VECWWrite.FCancelNotifier.IsOperationCanceled(VECWWrite.FOperationID);
end;

function TECWWrite.Encode(
  AOperationID: Integer;
  ACancelNotifier: IOperationNotifier;
  FileName:string;
  Width,Height:cardinal;
  CompressRatio:Single;
  Hint:CompressHint;
  AReadDelegate:TEcwRead;
  Datum,Projection:string;
  SizeUnits: TCellSizeUnits;
  CellIncrementX,CellIncrementY,OriginX,OriginY:double
):integer;
var
  i:integer;
  VNCSError:NCSError;
begin
  FReadDelegate := AReadDelegate;
  FOperationID := AOperationID;
  FCancelNotifier := ACancelNotifier;
  FEcwData^.pClientData := Self;
  FEcwData^.nInputBands:=3;
  FEcwData^.nInOutSizeX:=Width;
  FEcwData^.nInOutSizeY:=Height;
  FEcwData^.eCompressFormat := COMPRESS_RGB;
  FEcwData^.eCompressHint := Hint;
  FEcwData^.fTargetCompression:=CompressRatio;
  FEcwData^.eCellSizeUnits:= CellSizeUnits(SizeUnits);
  FEcwData^.fCellIncrementX:=CellIncrementX;
  FEcwData^.fCellIncrementY:=CellIncrementY;
  FEcwData^.fOriginX:=OriginX;
  FEcwData^.fOriginY:=OriginY;

  for i:=0 to 15 do FEcwData^.szDatum[i]:=#0;
  for i:=0 to 15 do FEcwData^.szProjection[i]:=#0;
  for i:=1 to 16 do begin
    FEcwData^.szDatum[i-1]:=Datum[i];
    FEcwData^.szProjection[i-1]:=Projection[i];
  end;

  for i:=1 to length(filename) do
   FEcwData^.szOutputFilename[i-1]:=FileName[i];

  FEcwData^.pReadCallback := ReadCallbackFunc;
  FEcwData^.pStatusCallback:=nil;
  FEcwData^.pCancelCallback:=cancel;
  VNCSError:= FEcwDll.CompressOpen(FEcwData, false);
  if VNCSError = NCS_SUCCESS then begin
    VNCSError:=FEcwDll.Compress(FEcwData);
    FEcwDll.CompressClose(FEcwData);
  end;
  FEcwDll.CompressFreeClient(FEcwData);

  result:=integer(VNCSError);
end;

end.
