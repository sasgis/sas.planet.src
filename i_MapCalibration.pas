unit i_MapCalibration;

interface

uses
  Types,
  i_CoordConverter;

type
  IMapCalibration = interface
    ['{08085422-4267-49EC-913C-3A47866A46E9}']
    // Имя для вывода в листбоксе для выбора при экспорте.
    function GetName: WideString; safecall;
    // Более детальное описание привязки
    function GetDescription: WideString; safecall;
    // Генерирует привязку для склеенной карты.
    procedure SaveCalibrationInfo(AFileName: WideString; xy1, xy2: TPoint; Azoom: byte; AConverter: ICoordConverter); safecall;
  end;

  IMapCalibrationList = interface
    ['{9D1740E4-498E-4A5E-B722-C929DB6C759B}']
    function GetCount: Integer;
    property Count: Integer read GetCount;

    function Get(AIndex: Integer): IMapCalibration;
    property Items[Index: Integer]: IMapCalibration read Get; default;
  end;

implementation

end.
