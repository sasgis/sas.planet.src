{ **** UBPFD *********** by delphibase.endimus.com ****
>> Удаление непустого каталога вместе с подкаталогами

Удаление подкаталогов рекурсивное - функция вызывает саму себя.
Описание назначения агрументов:

-DeleteAllFilesAndFolder - если TRUE то функцией будут предприняты
попытки для установки атрибута faArchive любому файлу или папке
перед его(её) удалением;

-StopIfNotAllDeleted - если TRUE то работа функции моментально
прекращается если возникла ошибка удаления хотя бы одного файла или папки;

-RemoveRoot - если TRUE, указывает на необходимость удаления корня.

Зависимости: FileCtrl, SysUtils
Автор:       lipskiy, lipskiy@mail.ru, ICQ:51219290, Санкт-Петербург
Copyright:   Собственное написание (lipskiy)
Дата:        26 апреля 2002 г.
***************************************************** }

unit u_TreeFolderRemover;

interface

uses
  Windows,
  {$WARNINGS OFF}
  FileCtrl,
  {$WARNINGS ON}
  SysUtils;

function FullRemoveDir(Dir: string; DeleteAllFilesAndFolders,
  StopIfNotAllDeleted, RemoveRoot: boolean): Boolean;

implementation

{$WARNINGS OFF} // Disable specific to a platform warnings
function FullRemoveDir(Dir: string; DeleteAllFilesAndFolders,
  StopIfNotAllDeleted, RemoveRoot: boolean): Boolean;
var
  i: Integer;
  SRec: TSearchRec;
  FN: string;
begin
  Result := False;
  if not DirectoryExists(Dir) then
    exit;
  Result := True;
  // Добавляем слэш в конце и задаем маску - "все файлы и директории"
  Dir := IncludeTrailingBackslash(Dir);
  i := FindFirst(Dir + '*', faAnyFile, SRec);
  try
    while i = 0 do
    begin
      // Получаем полный путь к файлу или директорию
      FN := Dir + SRec.Name;
      // Если это директория
      if ((SRec.Attr and faDirectory) = faDirectory) then
      begin
        // Рекурсивный вызов этой же функции с ключом удаления корня
        if (SRec.Name <> '') and (SRec.Name <> '.') and (SRec.Name <> '..') then
        begin
          if DeleteAllFilesAndFolders then
            FileSetAttr(FN, faArchive);
          Result := FullRemoveDir(FN, DeleteAllFilesAndFolders,
            StopIfNotAllDeleted, True);
          if not Result and StopIfNotAllDeleted then
            exit;
        end;
      end
      else // Иначе удаляем файл
      begin
        if DeleteAllFilesAndFolders then
          FileSetAttr(FN, faArchive);
        Result := SysUtils.DeleteFile(FN);
        if not Result and StopIfNotAllDeleted then
          exit;
      end;
      // Берем следующий файл или директорию
      i := FindNext(SRec);
    end;
  finally
    SysUtils.FindClose(SRec);
  end;
  if not Result then
    exit;
  if RemoveRoot then // Если необходимо удалить корень - удаляем
    if not RemoveDir(Dir) then
      Result := false;
end;
{$WARNINGS ON}

// Пример использования:
//
// FullRemoveDir('C:\a', true, true, true);
// Полное удаление папки C:\a со всем её содержимым,
// и с последующим удалением самой c:\a

end.