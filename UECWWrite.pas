unit UECWWrite;

interface

uses
  Windows,
  Dialogs,
  ECWwriter,
  ECWreader,
  t_GeoTypes;

type
  TlineRGB = array[0..0] of single;
  PlineRGB = ^TlineRGB;

  TEcwRead = function(Line:cardinal; var lineR,LineG,LineB:PLineRGB):boolean of object;
  TEcwStatus = procedure(Line:cardinal) of object;
  TEcwCancel = function(): Boolean of object;

type
  TECWWrite = class
  private
    FDllHandle: LongWord;
    FEcwData: PNCSEcwCompressClient;
    FReadDelegate: TEcwRead;
    FCancelDelegate: TEcwCancel;
    FStatusDelegate: TEcwStatus;
  public
    constructor Create(ALibPath: string);
    function Encode(
      FileName:string;
      Width,Height:cardinal;
      CompressRatio:Single;
      Hint:CompressHint;
      AReadDelegate:TEcwRead;
      ACancelDelegate:TEcwCancel;
      AStatusDelegate:TEcwStatus;
      Datum,Projection:string;
      SizeUnits:TCellSizeUnits;
      CellIncrementX,CellIncrementY,OriginX,OriginY:double
    ):integer;
  end;

implementation

uses
  u_GlobalState;

constructor TECWWrite.Create(ALibPath: string);
var _NCSEcwCompressAllocClient:NCSEcwCompressAllocClient;
begin
  inherited create;
  FDllHandle := LoadLibrary(PChar(ALibPath + 'NCSEcwC.dll'));
  if FDllHandle = 0 then begin
    ShowMessage('Ошибка при загрузке библиотеки NCSEcwC.dll');
    Halt
  end;
  @_NCSEcwCompressAllocClient := GetProcAddress(FDllHandle, 'NCSEcwCompressAllocClient');
  FEcwData := _NCSEcwCompressAllocClient;
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
  result:= VECWWrite.FCancelDelegate;
end;

function TECWWrite.Encode(
  FileName:string;
  Width,Height:cardinal;
  CompressRatio:Single;
  Hint:CompressHint;
  AReadDelegate:TEcwRead;
  ACancelDelegate:TEcwCancel;
  AStatusDelegate:TEcwStatus;
  Datum,Projection:string;
  SizeUnits: TCellSizeUnits;
  CellIncrementX,CellIncrementY,OriginX,OriginY:double
):integer;
var
  i:integer;
  _NCSEcwCompress:NCSEcwCompress;
  _NCSEcwCompressOpen:NCSEcwCompressOpen;
  _NCSEcwCompressClose:NCSEcwCompressClose;
  _NCSEcwCompressFreeClient:NCSEcwCompressFreeClient;
  VNCSError:NCSError;
begin
  @_NCSEcwCompress := GetProcAddress(FDllHandle, 'NCSEcwCompress');
  @_NCSEcwCompressOpen := GetProcAddress(FDllHandle, 'NCSEcwCompressOpen');
  @_NCSEcwCompressClose := GetProcAddress(FDllHandle, 'NCSEcwCompressClose');
  @_NCSEcwCompressFreeClient := GetProcAddress(FDllHandle, 'NCSEcwCompressFreeClient');

  FReadDelegate := AReadDelegate;
  FStatusDelegate := AStatusDelegate;
  FCancelDelegate := ACancelDelegate;
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
  VNCSError:=_NCSEcwCompressOpen(FEcwData, false);
  if VNCSError = NCS_SUCCESS then begin
    VNCSError:=_NCSEcwCompress(FEcwData);
    _NCSEcwCompressClose(FEcwData);
  end;
  _NCSEcwCompressFreeClient(FEcwData);

	if(VNCSError = NCS_SUCCESS) then begin
     FreeLibrary(FDllHandle);
  end;
  result:=integer(VNCSError);
end;

end.
