dxgettext -r --delphi --nonascii --useignorepo -o:msgid -o ..\SAS.Translate
cd ..\SAS.Translate
msgremove default.po -i ignore.po -o output.po
del default.po
rename output.po default.po