unit i_MarkCategoryDBSmlInternal;

interface

uses
  i_MarkCategory;

type
  IMarkCategoryDBSmlInternal = interface
    ['{44BC212B-F9A2-4304-9E04-D44D16817445}']
    function SaveCategory2File: boolean;
    procedure LoadCategoriesFromFile;
    function GetCategoryByID(id: integer): IMarkCategory;
  end;

implementation

end.
