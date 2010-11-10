unit u_ContentTypeManagerSimple;

interface

uses
  Classes,
  i_ContentTypeInfo,
  i_IContentConverter,
  i_IContentTypeManager;

type
  TContentTypeManagerSimple = class(TInterfacedObject, IContentTypeManager)
  private
    FExtList: TStringList;
    FTypeList: TStringList;
    FBitmapExtList: TStringList;
    FBitmapTypeList: TStringList;
    FKmlExtList: TStringList;
    FKmlTypeList: TStringList;
    FConvertersBySourceTypeList: TStringList;
  protected
    procedure AddByType(AInfo: IContentTypeInfoBasic; AType: string);
    procedure AddByExt(AInfo: IContentTypeInfoBasic; AExt: string);
  protected
    function GetInfo(AType: WideString): IContentTypeInfoBasic;
    function GetInfoByExt(AExt: WideString): IContentTypeInfoBasic;
    function GetIsBitmapType(AType: WideString): Boolean;
    function GetIsBitmapExt(AExt: WideString): Boolean;
    function GetIsKmlType(AType: WideString): Boolean;
    function GetIsKmlExt(AExt: WideString): Boolean;
    function GetConverter(ATypeSource, ATypeTarget: WideString): IContentConverter;
  public
    constructor Create();
    destructor Destroy; override;
  end;

implementation


{ TContentTypeManagerSimple }

procedure TContentTypeManagerSimple.AddByExt(AInfo: IContentTypeInfoBasic;
  AExt: string);
begin

end;

procedure TContentTypeManagerSimple.AddByType(AInfo: IContentTypeInfoBasic;
  AType: string);
begin

end;

constructor TContentTypeManagerSimple.Create;
begin

end;

destructor TContentTypeManagerSimple.Destroy;
begin

  inherited;
end;

function TContentTypeManagerSimple.GetConverter(ATypeSource,
  ATypeTarget: WideString): IContentConverter;
begin

end;

function TContentTypeManagerSimple.GetInfo(
  AType: WideString): IContentTypeInfoBasic;
begin

end;

function TContentTypeManagerSimple.GetInfoByExt(
  AExt: WideString): IContentTypeInfoBasic;
begin

end;

function TContentTypeManagerSimple.GetIsBitmapExt(AExt: WideString): Boolean;
begin

end;

function TContentTypeManagerSimple.GetIsBitmapType(AType: WideString): Boolean;
begin

end;

function TContentTypeManagerSimple.GetIsKmlExt(AExt: WideString): Boolean;
begin

end;

function TContentTypeManagerSimple.GetIsKmlType(AType: WideString): Boolean;
begin

end;

end.
