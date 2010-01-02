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
    Handle: LongWord;
    pClient: PNCSEcwCompressClient;
    x:NCSError;
  public
    constructor Create();
    function Encode(ASender:TObject;FileName:string; Width,Height:cardinal;  CompressRatio:Single;
             Hint:CompressHint; ReadDelegate:TEcwRead; StatusDelegate:TEcwStatus;
             Datum,Projection:string; SizeUnits:CellSizeUnits;
             CellIncrementX,CellIncrementY,OriginX,OriginY:double):integer;
  end;


var
  EcwRead:TEcwRead;
  Imwidth,ImHeight:cardinal;
  Sender:TObject;

implementation

uses
  Unit1,
  UImgFun,
  UThreadScleit,
  GR32;

constructor TECWWrite.Create;
var _NCSEcwCompressAllocClient:NCSEcwCompressAllocClient;
begin
  inherited create;
  Handle := LoadLibrary(PAnsiChar(ExtractFilePath(paramstr(0))+'NCSEcwC.dll'));
  if Handle = 0 then
  begin
    ShowMessage('Ошибка при загрузке библиотеки NCSEcwC.dll');
    Halt
  end;
  @_NCSEcwCompressAllocClient := GetProcAddress(Handle, 'NCSEcwCompressAllocClient');
  pClient := _NCSEcwCompressAllocClient;
end;

function ReadCallbackFunc(pClient:Pointer;nNextLine:cardinal;InputArray:Pointer):boolean; cdecl;
  type
    Tptr = array [0..2] of pointer;
    Pptr=^Tptr;
begin
 EcwRead(Sender,nNextLine,PlineRGB(PPtr(InputArray)[0]),PlineRGB(PPtr(InputArray)[1]),PlineRGB(PPtr(InputArray)[2]));
 result:=true;
end;

function cancel:boolean;
begin
 result:=not(ThreadScleit(Sender).Fprogress.Visible);
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
begin
  @_NCSEcwCompress := GetProcAddress(Handle, 'NCSEcwCompress');
  @_NCSEcwCompressOpen := GetProcAddress(Handle, 'NCSEcwCompressOpen');
  @_NCSEcwCompressClose := GetProcAddress(Handle, 'NCSEcwCompressClose');
  @_NCSEcwCompressFreeClient := GetProcAddress(Handle, 'NCSEcwCompressFreeClient');

  sender:=Asender;
  EcwRead:=ReadDelegate;
  Imwidth:=Width;
  ImHeight:=Height;
  pClient^.nInputBands:=3;
  pClient^.nInOutSizeX:=Width;
  pClient^.nInOutSizeY:=Height;
  pClient^.eCompressFormat := COMPRESS_RGB;
  pClient^.eCompressHint := Hint;
  pClient^.fTargetCompression:=CompressRatio;
  pClient^.eCellSizeUnits:= SizeUnits;
  pClient^.fCellIncrementX:=CellIncrementX;
  pClient^.fCellIncrementY:=CellIncrementY;
  pClient^.fOriginX:=OriginX;
  pClient^.fOriginY:=OriginY;

  for i:=0 to 15 do pClient^.szDatum[i]:=#0;
  for i:=0 to 15 do pClient^.szProjection[i]:=#0;
  for i:=1 to 16 do
   begin
    pClient^.szDatum[i-1]:=Datum[i];
    pClient^.szProjection[i-1]:=Projection[i];
   end;

  for i:=1 to length(filename) do
   pClient^.szOutputFilename[i-1]:=FileName[i];

  pClient^.pReadCallback := ReadCallbackFunc;
  pClient^.pStatusCallback:=nil;
  pClient^.pCancelCallback:=@cancel;
  x:=_NCSEcwCompressOpen(pClient,false);
  if x = NCS_SUCCESS then
   begin
    x:=_NCSEcwCompress(pClient);
    _NCSEcwCompressClose(pClient);
   end;
  _NCSEcwCompressFreeClient(pClient);

	if(x = NCS_SUCCESS) then
   begin
     FreeLibrary(Handle);
	 end;
  result:=integer(x);
end;

end.
