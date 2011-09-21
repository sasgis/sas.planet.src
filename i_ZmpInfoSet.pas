unit i_ZmpInfoSet;

interface

uses
  ActiveX,
  i_ZmpInfo;

type
  IZmpInfoSet = interface
    ['{0F26DDB7-EEFB-4CCA-9CCD-6D24BED683E5}']
    function GetZmpByGUID(AGUID: TGUID): IZmpInfo;
    function GetIterator: IEnumGUID;
  end;

implementation

end.
