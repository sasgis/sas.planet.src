unit ECWWrite;
interface
 uses ECWwriter,ECWreader,
  Windows, Messages, SysUtils, Variants, Classes, Graphics, JPEG, Controls, Forms,
  Dialogs, StdCtrls,ExtCtrls;

 type
  TlineRGB = array[0..0] of single;
  PlineRGB = ^TlineRGB;

 type Tptr = array [0..2] of pointer;
 ptr=^Tptr;
 TEcwRead = function(Sender:TObject;Line:cardinal; var lineR,LineG,LineB:PLineRGB):boolean;
 TEcwStatus = procedure(Line:cardinal);

 type TECWWrite = class
  private
   Handle: LongWord;
   pClient:Pointer;
   x:NCSError;
  public
   constructor Create;
   //ReadCallbackFunc(pClient:Pointer;nNextLine:cardinal;InputArray:ptr):boolean;
   function Encode(ASender:TObject;FileName:string; Width,Height:cardinal;  CompressRatio:Single;
             Hint:CompressHint; ReadDelegate:TEcwRead; StatusDelegate:TEcwStatus;
             Datum,Projection:string; SizeUnits:CellSizeUnits;
             CellIncrementX,CellIncrementY,OriginX,OriginY:double):integer;
 end;


var
  EcwRead:TEcwRead;
  Imwidth,ImHeight:cardinal;
  lineR,lineG,lineB:PLineRGB;
  Sender:TObject;
  cancelflag:boolean;
//  function ReadCallbackFunc(pClient:Pointer;nNextLine:cardinal;InputArray:ptr):boolean;

implementation
uses Unit1,UImgFun,UThreadScleit,GR32;

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

function ReadCallbackFunc(pClient:Pointer;nNextLine:cardinal;InputArray:ptr):boolean;
begin
 EcwRead(Sender,nNextLine,PlineRGB(InputArray[0]),PlineRGB(InputArray[1]),PlineRGB(InputArray[2]));
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
  cancelflag:=false;
  NCSEcwCompressClient(pClient^).nInputBands:=3;
  NCSEcwCompressClient(pClient^).nInOutSizeX:=Width;
  NCSEcwCompressClient(pClient^).nInOutSizeY:=Height;
  NCSEcwCompressClient(pClient^).eCompressFormat := COMPRESS_RGB;
  NCSEcwCompressClient(pClient^).eCompressHint := Hint;
  NCSEcwCompressClient(pClient^).fTargetCompression:=CompressRatio;
  NCSEcwCompressClient(pClient^).eCellSizeUnits:= SizeUnits;
  NCSEcwCompressClient(pClient^).fCellIncrementX:=CellIncrementX;
  NCSEcwCompressClient(pClient^).fCellIncrementY:=CellIncrementY;
  NCSEcwCompressClient(pClient^).fOriginX:=OriginX;
  NCSEcwCompressClient(pClient^).fOriginY:=OriginY;

  for i:=0 to 15 do NCSEcwCompressClient(pClient^).szDatum[i]:=#0;
  for i:=0 to 15 do NCSEcwCompressClient(pClient^).szProjection[i]:=#0;
  for i:=1 to 16 do
   begin
    NCSEcwCompressClient(pClient^).szDatum[i-1]:=Datum[i];
    NCSEcwCompressClient(pClient^).szProjection[i-1]:=Projection[i];
   end;

  for i:=1 to length(filename) do
   NCSEcwCompressClient(pClient^).szOutputFilename[i-1]:=FileName[i];//FileName;//StringToOleStr('test.ecw');

  NCSEcwCompressClient(pClient^).pReadCallback:= @ReadCallbackFunc;
  NCSEcwCompressClient(pClient^).pStatusCallback:=nil;
  NCSEcwCompressClient(pClient^).pCancelCallback:=@cancel;
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
	 end; {else
   begin
     FreeLibrary(Handle);
   end;    }
  result:=integer(x);
end;

end.
