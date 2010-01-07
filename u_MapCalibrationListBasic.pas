unit u_MapCalibrationListBasic;

interface

uses
  Classes;

type
  TMapCalibrationListBasic =  class(TInterfaceList)
  public
    constructor Create();
  end;

implementation

uses
  u_MapCalibrationOzi,
  u_MapCalibrationDat,
  u_MapCalibrationKml,
  u_MapCalibrationTab,
  u_MapCalibrationWorldFiles;

{ TMapCalibrationListBasic }

constructor TMapCalibrationListBasic.Create;
begin
  inherited;
  Add(TMapCalibrationOzi.Create);
  Add(TMapCalibrationDat.Create);
  Add(TMapCalibrationKml.Create);
  Add(TMapCalibrationTab.Create);
  Add(TMapCalibrationWorldFiles.Create);
end;

end.
