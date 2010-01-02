unit UECWWrite;

interface

uses
  ECWwriter,
  ECWreader,
  Windows,
  Messages,
  SysUtils,
  Variants,
  Classes,
  Graphics,
  JPEG,
  Controls,
  Forms,
  Dialogs,
  StdCtrls,
  ExtCtrls;

type
  TlineRGB = array[0..0] of single;
  PlineRGB = ^TlineRGB;

  TEcwRead = function(Sender:TObject;Line:cardinal; var lineR,LineG,LineB:PLineRGB):boolean;
  TEcwStatus = procedure(Line:cardinal);
  PNCSEcwCompressClient = ^NCSEcwCompressClient;

type
  TECWWrite = class
  private
    FDllHandle: LongWord;
    FEcwData: PNCSEcwCompressClient;
    FEcwRead:TEcwRead;
    FSender:TObject;
  public
    constructor Create();
    function Encode(ASender:TObject;FileName:string; Width,Height:cardinal;  CompressRatio:Single;
             Hint:CompressHint; ReadDelegate:TEcwRead; StatusDelegate:TEcwStatus;
             Datum,Projection:string; SizeUnits:CellSizeUnits;
             CellIncrementX,CellIncrementY,OriginX,OriginY:double):integer;
  end;

implementation

uses
  Unit1,
  UImgFun,
  u_GlobalState,
  UThreadScleit,
  GR32;

constructor TECWWrite.Create;
var _NCSEcwCompressAllocClient:NCSEcwCompressAllocClient;
begin
  inherited create;
  FDllHandle := LoadLibrary(PAnsiChar(GState.ProgramPath + 'NCSEcwC.dll'));
  if FDllHandle = 0 then begin
    ShowMessage('Ошибка при загрузке библиотеки NCSEcwC.dll');
    Halt
  end;
  @_NCSEcwCompressAllocClient := GetProcAddress(FDllHandle, 'NCSEcwCompressAllocClient');
  FEcwData := _NCSEcwCompressAllocClient;
end;

function ReadCallbackFunc(pClient:Pointer;nNextLine:cardinal;InputArray:Pointer):boolean; cdecl;
type
  Tptr = array [0..2] of pointer;
  Pptr=^Tptr;
var
  VECWWrite: TECWWrite;
begin
  VECWWrite := TECWWrite(PNCSEcwCompressClient(pClient).pClientData);
  VECWWrite.FEcwRead(VECWWrite.FSender, nNextLine,PlineRGB(PPtr(InputArray)[0]),PlineRGB(PPtr(InputArray)[1]),PlineRGB(PPtr(InputArray)[2]));
  result:=true;
end;

function cancel(pClient:Pointer):boolean; cdecl;
var
  VECWWrite: TECWWrite;
begin
  VECWWrite := TECWWrite(PNCSEcwCompressClient(pClient).pClientData);
  result:=not(ThreadScleit(VECWWrite.FSender).Fprogress.Visible);
end;

function TECWWrite.Encode(ASender:TObject;FileName:string; Width,Height:cardinal;  CompressRatio:Single;
             Hint:CompressHint; ReadDelegate:TEcwRead; StatusDelegate:TEcwStatus;
             Datum,Projection:string; SizeUnits:CellSizeUnits;
             CellIncrementX,CellIncrementY,OriginX,OriginY:double):integer;
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

  Fsender:=Asender;
  FEcwRead:=ReadDelegate;
  FEcwData^.pClientData := Self;
  FEcwData^.nInputBands:=3;
  FEcwData^.nInOutSizeX:=Width;
  FEcwData^.nInOutSizeY:=Height;
  FEcwData^.eCompressFormat := COMPRESS_RGB;
  FEcwData^.eCompressHint := Hint;
  FEcwData^.fTargetCompression:=CompressRatio;
  FEcwData^.eCellSizeUnits:= SizeUnits;
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
  FEcwData^.pCancelCallback:=@cancel;
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
