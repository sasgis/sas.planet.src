unit u_CoordConverterListStatic;

interface

uses
  SysUtils,
  Classes,
  i_CoordConverter,
  i_CoordConverterList,
  u_BaseInterfacedObject;

type
  TCoordConverterListStatic = class(TBaseInterfacedObject, ICoordConverterList)
  private
    FList: TStringList;
    FCS: IReadWriteSync;
  protected
    procedure Add(
      const AItem: ICoordConverter;
      const ACaption: string
    );
  private
    function Count: Integer;
    function Get(AIndex: Integer): ICoordConverter;
    function GetCaption(AIndex: Integer): string;
  public
    constructor Create;
    destructor Destroy; override;
  end;

implementation

uses
  u_Synchronizer;

{ TCoordConverterListStatic }

constructor TCoordConverterListStatic.Create;
begin
  inherited;
  FCS := MakeSyncRW_Std(Self, TRUE);
  FList := TStringList.Create;
end;

destructor TCoordConverterListStatic.Destroy;
var
  i: Integer;
begin
  if FList <> nil then begin
    for i := 0 to FList.Count - 1 do begin
      if FList.Objects[i] <> nil then begin
        IInterface(Pointer(FList.Objects[i]))._Release;
        FList.Objects[i] := nil;
      end;
    end;
  end;
  FreeAndNil(FList);
  FCS := nil;
  inherited;
end;

procedure TCoordConverterListStatic.Add(const AItem: ICoordConverter;
  const ACaption: string);
begin
  FCS.BeginWrite;
  try
    FList.AddObject(ACaption, TObject(Pointer(AItem)));
    AItem._AddRef;
  finally
    FCS.EndWrite;
  end;
end;

function TCoordConverterListStatic.Count: Integer;
begin
  FCS.BeginRead;
  try
    Result := FList.Count;
  finally
    FCS.EndRead;
  end;
end;

function TCoordConverterListStatic.Get(AIndex: Integer): ICoordConverter;
begin
  FCS.BeginRead;
  try
    Result := ICoordConverter(Pointer(FList.Objects[AIndex]));
  finally
    FCS.EndRead;
  end;
end;

function TCoordConverterListStatic.GetCaption(AIndex: Integer): string;
begin
  FCS.BeginRead;
  try
    Result := FList.Strings[AIndex];
  finally
    FCS.EndRead;
  end;
end;

end.
