unit i_IListOfObjectsWithTTL;

interface

uses
  Types,
  i_IObjectWithTTL;

type
  IListOfObjectsWithTTL = interface
  ['']
    procedure AddObject(AObj: IObjectWithTTL);
    procedure RemoveObject(AObj: IObjectWithTTL);
    procedure ProcessObjectsTrim;
  end;
implementation

end.
