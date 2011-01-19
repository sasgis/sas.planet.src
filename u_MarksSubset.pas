unit u_MarksSubset;

interface

uses
  Classes,
  ActiveX,
  t_GeoTypes,
  i_MarksSimple;

type
  TMarksSubset = class(TInterfacedObject, IMarksSubset)
  private
    FList: IInterfaceList;
  protected
    function GetSubsetByLonLatRect(ARect: TDoubleRect): IMarksSubset;
    function GetEnum: IEnumUnknown;
    function IsEmpty: Boolean;
  public
    constructor Create(AList: IInterfaceList);
  end;

implementation

uses
  u_EnumUnknown;

{ TMarksSubset }

constructor TMarksSubset.Create(AList: IInterfaceList);
begin
  FList := AList;
end;

function TMarksSubset.GetEnum: IEnumUnknown;
begin
  Result := TEnumUnknown.Create(FList);
end;

function TMarksSubset.GetSubsetByLonLatRect(ARect: TDoubleRect): IMarksSubset;
var
  VNewList: IInterfaceList;
  i: Integer;
  VMark: IMarkFull;
  VMarkLonLatRect: TDoubleRect;
begin
  VNewList := TInterfaceList.Create;
  VNewList.Lock;
  try
    for i := 0 to FList.Count - 1 do begin
      VMark := IMarkFull(FList.Items[i]);
      VMarkLonLatRect := VMark.LLRect;
      if(
        (ARect.Right >= VMarkLonLatRect.Left)and
        (ARect.Left <= VMarkLonLatRect.Right)and
        (ARect.Bottom <= VMarkLonLatRect.Top)and
        (ARect.Top >= VMarkLonLatRect.Bottom))
      then begin
        VNewList.Add(VMark);
      end;
    end;
  finally
    VNewList.Unlock;
  end;
  Result := TMarksSubset.Create(VNewList);
end;

function TMarksSubset.IsEmpty: Boolean;
begin
  Result := FList.Count = 0;
end;

end.
