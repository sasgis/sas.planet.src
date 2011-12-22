unit u_ThreadMapCombineBMP;

interface

uses
  Windows,
  Types,
  SysUtils,
  Classes,
  GR32,
  u_GeoFun,
  u_ResStrings,
  u_ThreadMapCombineBase,
  LibBMP;

type
  TThreadMapCombineBMP = class(TThreadMapCombineBase)
  protected
    procedure SaveRect; override;
  end;

implementation

uses
  gnugettext,
  i_LocalCoordConverter;

procedure TThreadMapCombineBMP.SaveRect;
const
  BMP_MAX_WIDTH = 32768;
  BMP_MAX_HEIGHT = 32768;
var
  iWidth, iHeight: integer;
  i: Integer;
  VBMP: TBitmapFile;
  VLineBGR: PArrayBGR;
begin
  sx := (FCurrentPieceRect.Left mod 256);
  sy := (FCurrentPieceRect.Top mod 256);
  ex := (FCurrentPieceRect.Right mod 256);
  ey := (FCurrentPieceRect.Bottom mod 256);

  iWidth := FMapPieceSize.X;
  iHeight := FMapPieceSize.y;

  if (iWidth >= BMP_MAX_WIDTH) or (iHeight >= BMP_MAX_HEIGHT) then begin
    raise Exception.CreateFmt(SAS_ERR_ImageIsTooBig, ['BMP', iWidth, BMP_MAX_WIDTH, iHeight, BMP_MAX_HEIGHT, 'BMP']);
  end;

  VBMP := TBitmapFile.Create(FCurrentFileName, iWidth, iHeight);
  try
    GetMem(VLineBGR, iWidth * 3);

    GetMem(FArray256BGR, 256 * sizeof(P256ArrayBGR));
    for i := 0 to 255 do begin
      GetMem(FArray256BGR[i], (iWidth + 1) * 3);
    end;
    try
      btmm := TCustomBitmap32.Create;
      try
        btmm.Width := 256;
        btmm.Height := 256;

        for i := 0 to iHeight - 1 do begin

          if ReadLine(i, VLineBGR, FArray256BGR) then begin

            if not VBMP.WriteLine(i, VLineBGR) then begin
              raise Exception.Create( _('BMP: Line write failure!') );
            end;

          end else begin
            raise Exception.Create( _('BMP: Fill line failure!') );
          end;

          if CancelNotifier.IsOperationCanceled(OperationID) then begin
            Break;
          end;
        end;
      finally
        btmm.Free;
      end;
    finally
      for i := 0 to 255 do begin
        FreeMem(FArray256BGR[i]);
      end;
      FreeMem(FArray256BGR);
      FreeMem(VLineBGR);
    end;
  finally
    VBMP.Free;
  end;
end;

end.
