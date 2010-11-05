unit u_TileInfoBasic;

interface

uses
  i_ITileInfoBasic;

type
  TTileInfoBasicBase = class(TInterfacedObject, ITileInfoBasic)
  private
    FDate: TDateTime;
    FVersion: Variant;
  protected
    function GetIsExists: Boolean; virtual; abstract;
    function GetIsExistsTNE: Boolean; virtual; abstract;
    function GetLoadDate: TDateTime; virtual;
    function GetSize: Cardinal; virtual; abstract;
    function GetVersion: Variant; virtual;
  public
    constructor Create(ADate: TDateTime; AVersion: Variant);
  end;

  TTileInfoBasicNotExists = class(TTileInfoBasicBase)
  protected
    function GetIsExists: Boolean; override;
    function GetIsExistsTNE: Boolean; override;
    function GetSize: Cardinal; override;
  end;

  TTileInfoBasicTNE = class(TTileInfoBasicBase)
  protected
    function GetIsExists: Boolean; override;
    function GetIsExistsTNE: Boolean; override;
    function GetSize: Cardinal; override;
  end;

  TTileInfoBasicExists = class(TTileInfoBasicBase)
  private
    FSize: Cardinal;
  protected
    function GetIsExists: Boolean; override;
    function GetIsExistsTNE: Boolean; override;
    function GetSize: Cardinal; override;
  public
    constructor Create(ADate: TDateTime; ASize: Cardinal; AVersion: Variant);
  end;

implementation

{ TTileInfoBasicBase }

constructor TTileInfoBasicBase.Create(ADate: TDateTime; AVersion: Variant);
begin
  FDate := ADate;
  FVersion := AVersion;
end;

function TTileInfoBasicBase.GetLoadDate: TDateTime;
begin
  Result := FDate;
end;

function TTileInfoBasicBase.GetVersion: Variant;
begin
  Result := FVersion;
end;

{ TTileInfoBasicTNE }

function TTileInfoBasicTNE.GetIsExists: Boolean;
begin
  Result := False;
end;

function TTileInfoBasicTNE.GetIsExistsTNE: Boolean;
begin
  Result := True;
end;

function TTileInfoBasicTNE.GetSize: Cardinal;
begin
  Result := 0;
end;

{ TTileInfoBasicExists }

constructor TTileInfoBasicExists.Create(ADate: TDateTime; ASize: Cardinal;
  AVersion: Variant);
begin
  inherited Create(ADate, AVersion);
  FSize := ASize;
end;

function TTileInfoBasicExists.GetIsExists: Boolean;
begin
  Result := True;
end;

function TTileInfoBasicExists.GetIsExistsTNE: Boolean;
begin
  Result := False;
end;

function TTileInfoBasicExists.GetSize: Cardinal;
begin
  Result := FSize;
end;

{ TTileInfoBasicNotExists }

function TTileInfoBasicNotExists.GetIsExists: Boolean;
begin
  Result := False;
end;

function TTileInfoBasicNotExists.GetIsExistsTNE: Boolean;
begin
  Result := False;
end;

function TTileInfoBasicNotExists.GetSize: Cardinal;
begin
  Result := 0;
end;

end.
