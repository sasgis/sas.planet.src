unit u_MapVersionFactoryGE;

interface

uses
  i_MapVersionInfo,
  i_MapVersionConfig;

type
  IMapVersionFactoryGEInternal = interface(IMapVersionFactory)
    ['{F5A6FBE0-D633-4BCB-999A-BB0428CB1EF2}']
    function CreateByGE(AVer: Word; ARes1: Byte; ADate: string): IMapVersionInfo;
  end;

  TMapVersionFactoryGE = class(TInterfacedObject, IMapVersionFactory, IMapVersionFactoryGEInternal)
  private
    function CreateByStoreString(AValue: string): IMapVersionInfo;
    function CreateByMapVersion(AValue: IMapVersionInfo): IMapVersionInfo;
    function CreateByGE(AVer: Word; ARes1: Byte; ADate: string): IMapVersionInfo;
  end;

implementation

uses
  SysUtils,
  i_MapVersionInfoGE;

type
  TMapVersionInfoGE = class(TInterfacedObject, IMapVersionInfo, IMapVersionInfoGE)
  private
    FVer: Word;
    FRes1: Byte;
    FDate: string;
  protected
    function GetUrlString: string;
    function GetStoreString: string;
    function GetCaption: string;
    function GetVer: Word;
    function GetRes1: Byte;
  public
    constructor Create(
      AVer: Word;
      ARes1: Byte;
      ADate: string
    );
  end;

{ TMapVersionInfoGE }

constructor TMapVersionInfoGE.Create(
  AVer: Word;
  ARes1: Byte;
  ADate: string
);
begin
  FVer := AVer;
  FRes1 := ARes1;
  FDate := ADate;
end;

function TMapVersionInfoGE.GetCaption: string;
begin
  if (FRes1 = 0) and (FVer = 0) then begin
    Result := '';
  end else begin
    if FDate <> '' then begin
      Result := FDate + '('+ IntToStr(FRes1) + '\' + IntToStr(FVer) + ')';
    end else begin
      Result :=  IntToStr(FRes1) + '\' + IntToStr(FVer);
    end;
  end;
end;

function TMapVersionInfoGE.GetRes1: Byte;
begin
  Result := FRes1;
end;

function TMapVersionInfoGE.GetStoreString: string;
begin
  if (FRes1 = 0) and (FVer = 0) then begin
    Result := '';
  end else begin
    Result := IntToStr(FRes1) + '\' + IntToStr(FVer);
  end;
end;

function TMapVersionInfoGE.GetUrlString: string;
begin
  Result := '';
end;

function TMapVersionInfoGE.GetVer: Word;
begin
  Result := FVer;
end;

{ TMapVersionFactoryGE }

function TMapVersionFactoryGE.CreateByGE(AVer: Word; ARes1: Byte;
  ADate: string): IMapVersionInfo;
begin
  Result := TMapVersionInfoGE.Create(AVer, ARes1, ADate);
end;

function TMapVersionFactoryGE.CreateByMapVersion(
  AValue: IMapVersionInfo): IMapVersionInfo;
begin
  if not Supports(AValue, IMapVersionInfoGE, Result) then begin
    if AValue <> nil then begin
      Result := CreateByStoreString(AValue.StoreString);
    end else begin
      Result := CreateByStoreString('');
    end;
  end;
end;

function TMapVersionFactoryGE.CreateByStoreString(
  AValue: string
): IMapVersionInfo;

  procedure _StrToWord(const ASrc: String; var w: Word);
  var v: Integer;
  begin
    if (0<Length(ASrc)) then
    if TryStrToInt(Trim(ASrc), v) then
    if (v>0) and (v<=$FFFF) then
      w := v;
  end;

  procedure _StrToByte(const ASrc: String; var b: Byte);
  var v: Integer;
  begin
    if (0<Length(ASrc)) then
    if TryStrToInt(Trim(ASrc), v) then
    if (v>0) and (v<=$FF) then
      b := v;
  end;
var
  VVer: Word;
  VRes1: Byte;
  VLen: Integer;
  VPos: Integer;
begin
  VVer := 0;
  VRes1 := 0;
  VLen := Length(AValue);
  if VLen > 0 then begin
    VPos := Pos('\', AValue);
    if (VPos > 0) then begin
      if VPos > 1 then begin
        _StrToByte(System.Copy(AValue, 1, (VPos - 1)), VRes1);
      end;
      if VPos + 1 < VLen then begin
        _StrToWord(System.Copy(AValue, VPos + 1, VLen - (VPos + 1)), VVer);
      end;
    end else begin
      _StrToByte(AValue, VRes1);
    end;
  end;
  Result := TMapVersionInfoGE.Create(VVer, VRes1, '');
end;

end.
