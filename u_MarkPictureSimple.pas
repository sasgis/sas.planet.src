{******************************************************************************}
{* SAS.Планета (SAS.Planet)                                                   *}
{* Copyright (C) 2007-2011, авторы программы SAS.Планета (SAS.Planet).        *}
{* Это программа является свободным программным обеспечением. Вы можете       *}
{* распространять и/или модифицировать её согласно условиям Стандартной       *}
{* Общественной Лицензии GNU, опубликованной Фондом Свободного Программного   *}
{* Обеспечения, версии 3. Эта программа распространяется в надежде, что она   *}
{* будет полезной, но БЕЗ ВСЯКИХ ГАРАНТИЙ, в том числе подразумеваемых        *}
{* гарантий ТОВАРНОГО СОСТОЯНИЯ ПРИ ПРОДАЖЕ и ГОДНОСТИ ДЛЯ ОПРЕДЕЛЁННОГО      *}
{* ПРИМЕНЕНИЯ. Смотрите Стандартную Общественную Лицензию GNU версии 3, для   *}
{* получения дополнительной информации. Вы должны были получить копию         *}
{* Стандартной Общественной Лицензии GNU вместе с программой. В случае её     *}
{* отсутствия, посмотрите http://www.gnu.org/licenses/.                       *}
{*                                                                            *}
{* http://sasgis.ru/sasplanet                                                 *}
{* az@sasgis.ru                                                               *}
{******************************************************************************}

unit u_MarkPictureSimple;

interface

uses
  Types,
  Classes,
  GR32,
  i_BitmapTileSaveLoad,
  i_MarkPicture;

type
  TMarkPictureSimple = class(TInterfacedObject, IMarkPicture)
    FFullFileName: string;
    FName: string;
    FBitmap: TCustomBitmap32;
    FBitmapSize: TPoint;
  protected
    function GetName: string;
    procedure ExportToStream(AStream: TStream);
    procedure LoadBitmap(ABmp: TCustomBitmap32);
    function GetPointInPicture: TPoint;
    function GetTextAlignment: TAlignment;
    function GetTextVerticalAlignment: TVerticalAlignment;
    function GetBitmapSize: TPoint;
  public
    constructor Create(AFullFileName: string; AName: string; ALoader: IBitmapTileLoader);
    destructor Destroy; override;
  end;

implementation

uses
  SysUtils,
  GR32_LowLevel;

{ TMarkPictureSimple }
constructor TMarkPictureSimple.Create(AFullFileName: string; AName: string; ALoader: IBitmapTileLoader);
begin
  FFullFileName := AFullFileName;
  FName := AName;
  FBitmap := TCustomBitmap32.Create;
  ALoader.LoadFromFile(FFullFileName, FBitmap);
  FBitmapSize := Point(FBitmap.Width, FBitmap.Height);
end;

destructor TMarkPictureSimple.Destroy;
begin
  FreeAndNil(FBitmap);
  inherited;
end;

procedure TMarkPictureSimple.ExportToStream(AStream: TStream);
var
  VMemStream: TMemoryStream;
  VOwnStream: Boolean;
begin
  if AStream is TMemoryStream then begin
    VMemStream := TMemoryStream(AStream);
    VOwnStream := False;
  end else begin
    VMemStream := TMemoryStream.Create;
    VOwnStream := True;
  end;
  try
    VMemStream.LoadFromFile(FFullFileName);
    if VOwnStream then begin
      VMemStream.SaveToStream(AStream);
    end;
  finally
    if VOwnStream then begin
      VMemStream.Free;
    end;
  end;
end;

function TMarkPictureSimple.GetPointInPicture: TPoint;
begin
  Result.X := FBitmapSize.X div 2;
  Result.Y := FBitmapSize.Y;
end;

function TMarkPictureSimple.GetTextAlignment: TAlignment;
begin
  Result := taRightJustify;
end;

function TMarkPictureSimple.GetTextVerticalAlignment: TVerticalAlignment;
begin
  Result := taVerticalCenter;
end;

procedure TMarkPictureSimple.LoadBitmap(ABmp: TCustomBitmap32);
begin
  ABmp.SetSize(FBitmapSize.X, FBitmapSize.Y);
  if not FBitmap.Empty then
    MoveLongword(FBitmap.Bits[0], ABmp.Bits[0], FBitmapSize.X * FBitmapSize.Y);
end;

function TMarkPictureSimple.GetName: string;
begin
  Result := FName;
end;

function TMarkPictureSimple.GetBitmapSize: TPoint;
begin
  result:=FBitmapSize;
end;


end.
