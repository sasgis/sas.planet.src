unit u_TileFileNameSQLite;

interface

uses
  Types,
  u_TileFileNameBase;

type
  TTileFileNameSQLite = class(TTileFileNameBase)
  protected
    function GetTileFileName(
      AXY: TPoint;
      AZoom: Byte
    ): string; override;

    function GetTilePoint(
      const ATileFileName: AnsiString;
      out ATileXY: TPoint;
      out ATileZoom: Byte
    ): Boolean; override;
  end;

implementation

uses
  RegExpr,
  SysUtils;

const
  cSQLiteRegExpr  = '^(.+\\)?[zZ](\d\d?)\\\d+\\\d+\\(\d+)\.(\d+)(\..+)?$';

{ TTileFileNameSQLite }

function TTileFileNameSQLite.GetTileFileName(
  AXY: TPoint;
  AZoom: Byte
): string;
begin
  Result := Format(
    'z%d' + PathDelim +
    '%d' + PathDelim +
    '%d' + PathDelim +
    '%d' + '.' + '%d',
    [
      AZoom + 1,
      AXY.x shr 10,
      AXY.y shr 10,
      AXY.x shr 8,
      AXY.y shr 8
    ]
  );
end;

function TTileFileNameSQLite.GetTilePoint(
  const ATileFileName: AnsiString;
  out ATileXY: TPoint;
  out ATileZoom: Byte
): Boolean;
var
  VRegExpr: TRegExpr;
begin
  VRegExpr := TRegExpr.Create;
  try
    VRegExpr.Expression := cSQLiteRegExpr;
    if VRegExpr.Exec(ATileFileName) then begin
      ATileZoom := StrToInt(VRegExpr.Match[2]) - 1;
      ATileXY.X := StrToInt(VRegExpr.Match[3]) shl 8;
      ATileXY.Y := StrToInt(VRegExpr.Match[4]) shl 8;
      Result := True;
    end else begin
      Result := False;
    end;
  finally
    VRegExpr.Free;
  end;
end;

end.
