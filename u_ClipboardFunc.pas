unit u_ClipboardFunc;

interface

uses
  i_Bitmap32Static;

procedure CopyBitmapToClipboard(AHandle: THandle; ABitmap: IBitmap32Static);
procedure CopyStringToClipboard(AHandle: THandle; const s: string);

implementation

uses
  Windows,
  Graphics,
  GR32,
  u_BitmapFunc;

procedure CopyBitmapToClipboard(AHandle: THandle; ABitmap: IBitmap32Static);
var
  btm:TBitmap32;
  btm1:TBitmap;
  hSourcDC, hDestDC, hBM, hbmOld: THandle;
begin
  btm := TBitmap32.Create;
  try
    AssignStaticToBitmap32(btm, ABitmap);
    btm1:=TBitmap.Create;
    try
      btm1.Width:=btm.Width;
      btm1.Height:=btm.Height;
      btm.DrawTo(btm1.Canvas.Handle,0,0);
      hSourcDC := btm1.Canvas.Handle;
      hDestDC := CreateCompatibleDC(hSourcDC);
      hBM := CreateCompatibleBitmap(hSourcDC, btm1.width, btm1.height);
      hbmold:= SelectObject(hDestDC, hBM);
      BitBlt(hDestDC, 0, 0, btm1.width, btm1.height, hSourcDC, 0, 0, SRCCopy);
      OpenClipBoard(AHandle);
      EmptyClipBoard;
      SetClipBoardData(CF_Bitmap, hBM);
      CloseClipBoard;
      SelectObject(hDestDC,hbmold);
      DeleteObject(hbm);
      DeleteDC(hDestDC);
      DeleteDC(hSourcDC);
    finally
      btm1.Free;
    end;
  finally
    btm.Free;
  end;
end;

procedure CopyStringToClipboard(AHandle: THandle; const s: string);
var
  VStr: WideString;
  hg: THandle;
  P: Pointer;
  VLen: Integer;
begin
  if OpenClipboard(AHandle) then
  begin
    try
      EmptyClipBoard;
      VStr := s;
      VLen := (Length(VStr)+1) * SizeOf(VStr[1]);
      hg:=GlobalAlloc(GMEM_DDESHARE or GMEM_MOVEABLE, VLen);
      try
        P:=GlobalLock(hg);
        try
          Move(VStr[1], P^, VLen);
          SetClipboardData(CF_UNICODETEXT, hg);
        finally
          GlobalUnlock(hg);
        end;
      except
        GlobalFree(hg);
        raise
      end;
    finally
      CloseClipboard;
    end;
  end
end;



end.
