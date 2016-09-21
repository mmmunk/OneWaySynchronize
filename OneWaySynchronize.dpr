{
OneWaySynchronize - Fast file backup and mirroring
Copyright (C) 2008-2015 Thomas Munk, thomas () mmmunk . dk

This software is licensed under the ZLib License:

This software is provided 'as-is', without any express or implied warranty.
In no event will the authors be held liable for any damages arising from
the use of this software.

Permission is granted to anyone to use this software for any purpose,
including commercial applications, and to alter it and redistribute
it freely, subject to the following restrictions:

1. The origin of this software must not be misrepresented; you must not
   claim that you wrote the original software. If you use this software
   in a product, an acknowledgment in the product documentation would be
   appreciated but is not required.

2. Altered source versions must be plainly marked as such, and must not
   be misrepresented as being the original software.

3. This notice may not be removed or altered from any source distribution.
}

program OneWaySynchronize;  { Last compiled with Delphi XE6 }
{$APPTYPE CONSOLE}
{$R *.res}

uses
  Winapi.Windows, Winapi.ShLwApi, Winapi.ShellAPI, System.SysUtils, System.Classes;

{ ---------- Global variables ------------------------------------------------ }

const
  AppVersion = '0.9';  { Must match [Project Options]/[Version Info] for both 32 and 64 bit }

var
  { User parameters }
  SrcDir: string = '';  { Directory to copy files from - files are not changed }
  DstDir: string = '';  { Directory to copy files to - files are changed }
  InclDirSpecs: TStringList = nil;   { List of directory names which are the only ones to sync }
  InclFileSpecs: TStringList = nil;  { List of file names which are the only ones to sync }
  ExclDirSpecs: TStringList = nil;   { List of directory names which are not included }
  ExclFileSpecs: TStringList = nil;  { List of file names which are not included }
//NoRename: Boolean = False;    { Do not rename dirs or files if only case differs }
  NoResetAttr: Boolean = False; { Do not reset destination file attributes }
  NoDelete: Boolean = False;    { Do not delete files on destination }
  NoOverwrite: Boolean = False; { Do not overwrite existing files on destination }
  TimeStampDiff: Integer = 2;   { Difference in seconds between src and dst timestamps must be larger than this value to be considered different }
  LogDisplay: Boolean = True;   { Log directory and file names to screen }
  LogFileName: string = '';     { Log to file }
  LogAppend: Boolean;           { Overwrite or append to log file }
  { Runtime globals }
  MaxTimeStampDiff100ns: UInt64;
  Level1: Boolean;
  CurrDir: string;
  CurrDirIsNew: Boolean;
  LogFileHandle: THandle;
  { Counters }
  CountChangeDir: NativeUInt = 0;
  CountDeleteDir: NativeUInt = 0;
  CountDeleteFile: NativeUInt = 0;
  CountCreateDir: NativeUInt = 0;
  CountCopyFileNew: NativeUInt = 0;
  CountCopyFileChanged: NativeUInt = 0;
  CountError: NativeUInt = 0;

{ ---------- TFileInfo ------------------------------------------------------- }

type
  TFileInfo = class(TObject)
    FileSize1, FileSize2: Cardinal;
    TimeStamp: UInt64;
    constructor Create(const FindData: TWin32FindData);
    function IsEqualTo(Other: TFileInfo): Boolean;
  end;

constructor TFileInfo.Create(const FindData: TWin32FindData);
begin
  with FindData, Self do
  begin
    FileSize1:=nFileSizeLow;
    FileSize2:=nFileSizeHigh;
    TimeStamp:=ftLastWriteTime.dwHighDateTime*$100000000+ftLastWriteTime.dwLowDateTime;
  end;
end;

function TFileInfo.IsEqualTo(Other: TFileInfo): Boolean;
begin
  Result:=(FileSize1 = Other.FileSize1) and (FileSize2 = Other.FileSize2) and (Abs(TimeStamp-Other.TimeStamp) <= MaxTimeStampDiff100ns);
end;

{ ---------- Helper Functions ------------------------------------------------ }

procedure SemicolonListToSpecsList(const Value: string; var SpecsList: TStringList);
  procedure AddToList(S: string);
  begin
    if (S = '') or (S = '*') or (S = '*.') or (S = '*.*') then Exit;
    if SpecsList = nil then SpecsList:=TStringList.Create;
    if S[1] <> '\' then
      SpecsList.Add(S)
    else
    begin
      Delete(S, 1, 1);
      SpecsList.AddObject(S, TObject(Pointer(1)));
    end;
  end;
var
  i, P, L: Integer;
  ValidChar: Boolean;
  C: Char;
begin
  P:=1;
  ValidChar:=False;
  L:=Length(Value);
  for i:=1 to L do
  begin
    C:=Value[i];
    if C = ';' then
    begin
      if ValidChar then AddToList(Copy(Value, P, i-P));
      P:=i+1;
      ValidChar:=False;
    end
    else
    if C > ' ' then
      ValidChar:=True;
  end;
  if ValidChar then AddToList(Copy(Value, P, L-P+1));
end;

procedure DeleteSpecsListLevel1Items(var SpecsList: TStringList);
var
  i: Integer;
begin
  if SpecsList = nil then Exit;
  i:=SpecsList.Count-1;
  while i >= 0 do
  begin
    if SpecsList.Objects[i] <> nil then SpecsList.Delete(i);
    Dec(i);
  end;
  if SpecsList.Count = 0 then FreeAndNil(SpecsList);
end;

function MatchSpecs(FileName: PChar; SpecsList: TStringList): Boolean;
var
  S: string;
begin
  for S in SpecsList do
    if PathMatchSpec(FileName, PChar(S)) then
      Exit(True);
  Result:=False;
end;

function DeleteDirectory(const Dir: string): Boolean;
var
  FileOp: TSHFileOpStruct;
begin
  FileOp.Wnd:=0;
  FileOp.wFunc:=FO_DELETE;
  FileOp.pFrom:=PChar(Dir+#0);
  FileOp.pTo:=nil;
  FileOp.fFlags:=FOF_NOCONFIRMATION or FOF_NOERRORUI or FOF_SILENT;
  FileOp.fAnyOperationsAborted:=False;
  FileOp.lpszProgressTitle:=nil;
  Result:=(SHFileOperation(FileOp) = 0) and (not FileOp.fAnyOperationsAborted);
end;

//ExtendedLengthPath is disabled because SHFileOperation does not support extended-prefix
//TODO: Enable again and let DeleteDirectory remove prefix if remaining path lenght is below MAX_PATH and use SHFileOperation.
//If path is longer than MAX_PATH then use traditional recursive delete all files/dirs (+ remove readonly attribs first)
//Use fx System.IOUtils.TDirectory.Delete(Path, True) - Maybe create MasterDelDir which calls SubDelDir1 or SubDelDir2

//function ExtendedLengthPath(const Path: string): string;
//const
//  PrefixLocal = '\\?\';
//  PrefixUNC = PrefixLocal+'UNC\';
//begin
//  if Copy(Path, 1, 4) = PrefixLocal then Exit(Path);
//  if PathIsUNC(PChar(Path)) then
//    Result:=PrefixUNC+Copy(Path, 3, Length(Path)-2)
//  else
//    Result:=PrefixLocal+Path;
//end;

function SetThreadExecutionState(esFlags: DWORD): DWORD; stdcall; external kernel32;

{ ---------- Help and Command Line Parameters --------------------------------------------- }

procedure ShowHelp;
var
  MP, YP, YL: Integer;
begin
  WriteLn;
  WriteLn('--- Basic usage ---');
  WriteLn;
  WriteLn(ExtractFileName(ParamStr(0)), ' Source Destination [Options]');
  WriteLn;
  WriteLn('- Source and Destination must be fully qualified directory paths');
  WriteLn('- Source will never be written to or changed in any way');
  WriteLn('- Destination will be modified in every possible way to match Source (WARNING)');
  WriteLn;
  WriteLn('--- Options ---');
  WriteLn;
  WriteLn('Inclusion/exclusion of directories/files. Wildcards allowed. Excludes come');  //TODO: Is "Excludes come before includes" needed?
  WriteLn('before includes. Defaults are to include everything and exclude nothing:');
  //TODO: Add: Only names of files/dirs - no paths
  //TODO: Add: Prefix with "\" means first level only
  WriteLn;
  WriteLn('/ID:name[;name]  List of directory names to include only');
  WriteLn('/IF:name[;name]  List of file names to include only');
  WriteLn('/ED:name[;name]  List of directory names to not include');
  WriteLn('/EF:name[;name]  List of file names to not include');
  WriteLn;
  WriteLn('Disabling of standard behaviors:');
  WriteLn;
//WriteLn('/NR          No rename of directories/files. Delete and re-copy');   //TODO: Correct text?
  WriteLn('/NA          No reset of file attributes');
  WriteLn('/ND          No delete of directories/files');
  WriteLn('/NW          No overwrite of existing files');
  WriteLn('/TD:seconds  Maximum difference for timestamps to be considered equal');
  WriteLn('             Default is 2 seconds (FAT time resolution)');
  WriteLn;
  WriteLn('Logging:');
  WriteLn;
  WriteLn('/LQ           Quiet. Log only errors to display (stdout)');
  WriteLn('/LO:filename  Log to file. Overwrite existing file');
  WriteLn('/LA:filename  Log to file. Append to existing file');
  if Pos('MMM', FormatSettings.ShortDateFormat) = 0 then
  begin
    MP:=Pos('MM', FormatSettings.ShortDateFormat);
    if MP > 0 then
    begin
      YP:=Pos('yyyy', FormatSettings.ShortDateFormat);
      if YP > 0 then
        YL:=4
      else
      begin
        YP:=Pos('yy', FormatSettings.ShortDateFormat);
        YL:=2;
      end;
      if YP > 0 then
        WriteLn('              To have fx a monthly file: /LA:%date:~', YP-1, ',', YL, '%-%date:~', MP-1, ',2%.log');
    end;
  end;
end;

function ParseParameters: Boolean;
var
  L: TStringList;
  i, P: Integer;
  Option, Value: string;
begin
  Result:=False;
  L:=TStringList.Create;
  try
    { Extract command line parameters }
    for i:=1 to ParamCount do L.Add(ParamStr(i));
    { Help requested? }
    if (L.Count < 2) or (L.IndexOf('/?') >= 0) then
    begin
      ShowHelp;
      Exit;
    end;
    { Valid source dir? }
    SrcDir:=ExcludeTrailingPathDelimiter(L[0]);
    if PathIsRelative(PChar(SrcDir)) or (not DirectoryExists(SrcDir)) then  //TODO: Fix: "\." and paths without drive (or UNC) works
    begin
      WriteLn(sLineBreak+'Invalid source directory: ', L[0]);
      Exit;
    end;
    { Valid destination dir? }
    DstDir:=ExcludeTrailingPathDelimiter(L[1]);
    if PathIsRelative(PChar(DstDir)) or (not DirectoryExists(DstDir)) or PathIsSystemFolder(PChar(DstDir), 0) then
    begin
      WriteLn(sLineBreak+'Invalid destination directory: ', L[1]);
      Exit;
    end;
    { Are source and destionation dirs parts of each other? }
    if SameText(SrcDir, DstDir) then  //TODO: Much more checking here! How to find dirs inside each other?
    begin
      WriteLn(sLineBreak+'Invalid combination of source and destination directories');
      Exit;
    end;
    { Parse options }
    for i:=2 to L.Count-1 do
    begin
      Option:=L[i];
      if (Length(Option) < 2) or (Option[1] <> '/') or (not (AnsiChar(Option[2]) in ['a'..'z', 'A'..'Z'])) then
      begin
        WriteLn(sLineBreak+'Invalid option: ', Option);
        Exit;
      end;
      P:=Pos(':', Option);
      if P >= 3 then
      begin
        Value:=Copy(Option, P+1, Length(Option)-P);
        Option:=Copy(Option, 2, P-2);
      end
      else
      begin
        Value:='';
        Delete(Option, 1, 1);
      end;
      Option:=UpperCase(Option);
      if Option = 'ID' then
        SemicolonListToSpecsList(Value, InclDirSpecs)
      else
      if Option = 'IF' then
        SemicolonListToSpecsList(Value, InclFileSpecs)
      else
      if Option = 'ED' then
        SemicolonListToSpecsList(Value, ExclDirSpecs)
      else
      if Option = 'EF' then
        SemicolonListToSpecsList(Value, ExclFileSpecs)
      else
      //if Option = 'NR' then
      //  NoRename:=True
      //else
      if Option = 'NA' then
        NoResetAttr:=True
      else
      if Option = 'ND' then
        NoDelete:=True
      else
      if Option = 'NW' then
        NoOverwrite:=True
      else
      if Option = 'TD' then
      begin
        TimeStampDiff:=StrToIntDef(Value, TimeStampDiff);
        if TimeStampDiff < 0 then TimeStampDiff:=0 else if TimeStampDiff > $FFFF then TimeStampDiff:=$FFFF;
      end
      else
      if Option = 'LQ' then
        LogDisplay:=False
      else
      if Option = 'LO' then
      begin
        LogFileName:=Value;
        LogAppend:=False;
      end
      else
      if Option = 'LA' then
      begin
        LogFileName:=Value;
        LogAppend:=True;
      end
      else
      begin
        WriteLn(sLineBreak+'Unknown option: /', Option);
        Exit;
      end;
    end;
  finally
    L.Free;
  end;
  Result:=True;
end;

{ ---------- Log Functions --------------------------------------------------- }

procedure Log_Error;
begin
  WriteLn(sLineBreak+'Log file error: ', SysErrorMessage(GetLastError));
end;

procedure Log_OpenFile;
var
  Creation: Cardinal;
begin
  if LogFileName = '' then
    LogFileHandle:=INVALID_HANDLE_VALUE
  else
  begin
    if LogAppend then Creation:=OPEN_ALWAYS else Creation:=CREATE_ALWAYS;
    LogFileHandle:=CreateFile(PChar(LogFileName), GENERIC_WRITE, FILE_SHARE_READ, nil, Creation, FILE_ATTRIBUTE_NORMAL, 0);
    if LogFileHandle = INVALID_HANDLE_VALUE then
      Log_Error
    else
    if LogAppend then
      SetFilePointer(LogFileHandle, 0, nil, FILE_END);
  end;
end;

procedure Log_WriteLn(const S: string; Display: Boolean);
  procedure WriteToFile;
  var
    Buffer: packed array[0..1023] of AnsiChar;
    BufferSize, P, BytesWritten: Cardinal;
  begin
    BufferSize:=UnicodeToUtf8(Buffer, SizeOf(Buffer)-1, PChar(S), Length(S));
    if BufferSize > 0 then P:=BufferSize-1 else P:=0;
    Buffer[P]:=#13;
    Buffer[P+1]:=#10;
    BufferSize:=P+2;
    if not WriteFile(LogFileHandle, Buffer[0], BufferSize, BytesWritten, nil) then Log_Error;
  end;
begin
  if Display then WriteLn(S);
  if LogFileHandle <> INVALID_HANDLE_VALUE then WriteToFile;
end;

procedure Log_CloseFile;
begin
  if LogFileHandle <> INVALID_HANDLE_VALUE then CloseHandle(LogFileHandle);
end;

{ ---------- SyncEvent ------------------------------------------------------- }

type
  TSyncEventType = (seNone, seChangeDir, seDeleteDir, seDeleteFile, seCreateDir, seCopyFileNew, seCopyFileChanged, seError);

procedure SyncEvent(EventType: TSyncEventType; const S: string);
begin
  if CurrDirIsNew and (EventType <> seChangeDir) then
  begin
    Log_WriteLn(sLineBreak+'Directory: '+CurrDir, LogDisplay);  //TODO: PathCanonicalize here if extended-length-path?
    CurrDirIsNew:=False;
  end;
  case EventType of
    seChangeDir:
    begin
      CurrDir:=S;
      CurrDirIsNew:=True;
      Inc(CountChangeDir);
    end;
    seDeleteDir:
    begin
      Log_WriteLn('-['+S+']', LogDisplay);
      Inc(CountDeleteDir);
    end;
    seDeleteFile:
    begin
      Log_WriteLn('- '+S, LogDisplay);
      Inc(CountDeleteFile);
    end;
    seCreateDir:
    begin
      Log_WriteLn('+['+S+']', LogDisplay);
      Inc(CountCreateDir);
    end;
    seCopyFileNew:
    begin
      Log_WriteLn('+ '+S, LogDisplay);
      Inc(CountCopyFileNew);
    end;
    seCopyFileChanged:
    begin
      Log_WriteLn('  '+S, LogDisplay);
      Inc(CountCopyFileChanged);
    end;
    seError:
    begin
      Log_WriteLn('ERROR: '+S, True);
      Inc(CountError);
      //TODO: Stop execution after a defined number of errors
    end;
  end;
end;

{ ---------- FileNameList Functions ------------------------------------------ }

procedure FileNameList_Create(var List: TStringList);
begin
  List:=TStringList.Create;
  List.Sorted:=True;          { Add filenames sorted }
  List.CaseSensitive:=False;  { Do not distinguish between different cases of same filename }
  List.Duplicates:=dupError;  { Throw an exception if different cases of same filename }
end;

procedure FileNameList_Add(List: TStringList; const FileName: string; Obj: TObject = nil);
begin
  try
    List.AddObject(FileName, Obj);
  except
    Obj.Free;
    if List.IndexOf(FileName) >= 0 then
      SyncEvent(seError, 'Duplicate file or directory name: '+FileName)
    else
      raise
  end;
end;

procedure FileNameList_Destroy(List: TStringList; FreeObjects: Boolean = False);
var
  i: Integer;
begin
  if FreeObjects then
    for i:=0 to List.Count-1 do
      List.Objects[i].Free;
  List.Free;
end;

{ ---------- The Synchronization Itself -------------------------------------- }

procedure SyncDirPair(const SrcPath, DstPath: unicodestring);
var
  SrcDirList, SrcFileList, DstDirList, DstFileList: TStringList;
  Handle: THandle;
  FD: TWin32FindData;
  SrcName, DstName: string;
  SrcIndex, DstIndex: Integer;
  Event: TSyncEventType;
const
  //MaxPathLength = MAX_PATH-1;  { http://msdn.microsoft.com/en-us/library/windows/desktop/aa365247(v=vs.85).aspx#maxpath }
  NonIncludeAttribs =
    FILE_ATTRIBUTE_DEVICE or
    FILE_ATTRIBUTE_HIDDEN or
    FILE_ATTRIBUTE_OFFLINE or
    FILE_ATTRIBUTE_REPARSE_POINT or
    FILE_ATTRIBUTE_SPARSE_FILE or
    FILE_ATTRIBUTE_SYSTEM or
    FILE_ATTRIBUTE_TEMPORARY;  //TODO: Give user a chance to include these files: Create new parameter to manually set/override NonIncludeAttribs with a hex value.
    //TODO: Create a parameter to enable display of files/dirs being skipped because of NonIncludeAttribs
begin
  SyncEvent(seChangeDir, DstPath);
  //NOTE: MaxPathLength: Currently no check here for lengths of SrcPath/DstPath because we're going to use \\?\ extended-length syntax

  { Scan source }
  Handle:=FindFirstFile(PChar(SrcPath+'*'), FD);
  if Handle = INVALID_HANDLE_VALUE then
  begin
    SyncEvent(seError, 'Scanning source files: '+SysErrorMessage(GetLastError));
    Exit;  { No objects created yet }
  end;
  FileNameList_Create(SrcDirList);
  FileNameList_Create(SrcFileList);
  repeat
    { If filename starts with dot then investigate further }
    if FD.cFileName[0] = '.' then
      if (FD.cFileName[1] = #0) or ((FD.cFileName[1] = '.') and (FD.cFileName[2] = #0)) then
        Continue;
    { Is this file/dir included in synchronization? }
    if (FD.dwFileAttributes and NonIncludeAttribs) > 0 then
      Continue;
    if (FD.dwFileAttributes and FILE_ATTRIBUTE_DIRECTORY) > 0 then
    begin
      { Directory }
      if (ExclDirSpecs <> nil) and MatchSpecs(FD.cFileName, ExclDirSpecs) then
        Continue;
      if (InclDirSpecs = nil) or MatchSpecs(FD.cFileName, InclDirSpecs) then
        FileNameList_Add(SrcDirList, FD.cFileName);
    end
    else
    begin
      { File }
      if (ExclFileSpecs <> nil) and MatchSpecs(FD.cFileName, ExclFileSpecs) then
        Continue;
      if (InclFileSpecs = nil) or MatchSpecs(FD.cFileName, InclFileSpecs) then
        FileNameList_Add(SrcFileList, FD.cFileName, TFileInfo.Create(FD));
    end;
  until not FindNextFile(Handle, FD);  //TODO: Can it happen that we end with an error here?
  Winapi.Windows.FindClose(Handle);

  { Scan destination }
  Handle:=FindFirstFile(PChar(DstPath+'*'), FD);
  if Handle = INVALID_HANDLE_VALUE then
  begin
    SyncEvent(seError, 'Scanning destination files: '+SysErrorMessage(GetLastError));
    FileNameList_Destroy(SrcFileList, True);
    FileNameList_Destroy(SrcDirList);
    Exit;
  end;
  FileNameList_Create(DstDirList);
  FileNameList_Create(DstFileList);
  repeat
    { If filename starts with dot then investigate further }
    if FD.cFileName[0] = '.' then
      if (FD.cFileName[1] = #0) or ((FD.cFileName[1] = '.') and (FD.cFileName[2] = #0)) then
        Continue;
    { Is this file/dir included in synchronization? }
    if (FD.dwFileAttributes and NonIncludeAttribs) > 0 then
      Continue;
    if (FD.dwFileAttributes and FILE_ATTRIBUTE_DIRECTORY) > 0 then
      FileNameList_Add(DstDirList, FD.cFileName)  { Directory }
    else
      FileNameList_Add(DstFileList, FD.cFileName, TFileInfo.Create(FD));  { File }
  until not FindNextFile(Handle, FD);  //TODO: Can it happen that we end with an error here?
  Winapi.Windows.FindClose(Handle);

  //TODO: Implement rename of dirs and files if only case is different and dir/file is present at both src and dst - controlled by NoRename

  { Delete destination dirs which are not in source }
  if not NoDelete then
    for DstName in DstDirList do
      if SrcDirList.IndexOf(DstName) < 0 then
        if DeleteDirectory(DstPath+DstName) then
          SyncEvent(seDeleteDir, DstName)
        else
          SyncEvent(seError, 'Deleting directory: '+DstName);
//    ELSE: if (not NoRename) and (SrcName <> DstName) then RenameFile + event for this with a rename-symbol
//    OK to be dependent of NoDelete?

  { Delete destination files which are not in source }
  if not NoDelete then
    for DstName in DstFileList do
      if SrcFileList.IndexOf(DstName) < 0 then
        if Winapi.Windows.DeleteFile(PChar(DstPath+DstName)) then
          SyncEvent(seDeleteFile, DstName)
        else
          SyncEvent(seError, 'Deleting file: '+DstName+'; '+SysErrorMessage(GetLastError));
//    ELSE: (rename...)  //TODO

  { Create dirs from source which are missing on destination }
  for SrcName in SrcDirList do
    if DstDirList.IndexOf(SrcName) < 0 then
      if Winapi.Windows.CreateDirectory(PChar(DstPath+SrcName), nil) then
        SyncEvent(seCreateDir, SrcName)
      else
        SyncEvent(seError, 'Creating directory: '+SrcName+'; '+SysErrorMessage(GetLastError));
        //TODO: Do not continue? Or remove dir from SrcDirList? Or just continue and let CopyFile fail (maybe) as well?

  { Copy files from source which are either missing or different on destination }
  for SrcIndex:=0 to SrcFileList.Count-1 do
  begin
    SrcName:=SrcFileList[SrcIndex];
    DstIndex:=DstFileList.IndexOf(SrcName);
    if DstIndex < 0 then
      Event:=seCopyFileNew
    else
    if TFileInfo(SrcFileList.Objects[SrcIndex]).IsEqualTo(  TFileInfo(DstFileList.Objects[DstIndex])    ) then
      Event:=seNone
    else
      Event:=seCopyFileChanged;
    if (Event = seCopyFileNew) or ((Event = seCopyFileChanged) and (not NoOverwrite)) then
    begin
      DstName:=DstPath+SrcName;
      //TODO: Create own CopyFile which (based on FileSize) either calls Windows.CopyFile
      //      or does copying itself in chunks while showing progress in percent.
      if CopyFile(PChar(SrcPath+SrcName), PChar(DstName), False) then
      begin
        SyncEvent(Event, SrcName);
        if NoResetAttr then
          if not SetFileAttributes(PChar(DstName), FILE_ATTRIBUTE_NORMAL) then
            SyncEvent(seError, 'Setting attributes on file: '+SrcName+'; '+SysErrorMessage(GetLastError));
      end
      else
        SyncEvent(seError, 'Copying file: '+SrcName+'; '+SysErrorMessage(GetLastError));
    end;
  end;

  { Clean up but keep list of source dirs }
  FileNameList_Destroy(DstFileList, True);
  FileNameList_Destroy(DstDirList);
  FileNameList_Destroy(SrcFileList, True);
  Finalize(DstName);

  { Clean up level 1 includes/excludes }
  if Level1 then
  begin
    DeleteSpecsListLevel1Items(InclDirSpecs);
    DeleteSpecsListLevel1Items(InclFileSpecs);
    DeleteSpecsListLevel1Items(ExclDirSpecs);
    DeleteSpecsListLevel1Items(ExclFileSpecs);
    Level1:=False;
  end;

  { Recursively synchronize all sub-directories from source }
  for SrcName in SrcDirList do SyncDirPair(SrcPath+SrcName+PathDelim, DstPath+SrcName+PathDelim);
  FileNameList_Destroy(SrcDirList);
end;

{ ---------- Main ------------------------------------------------------------ }

begin
  {$ifdef DEBUG}
  ReportMemoryLeaksOnShutdown:=True;
  WriteLn('(DEBUG version)');
  {$endif}
  { Info }
  WriteLn(sLineBreak+'OneWaySynchronize '+AppVersion);
  WriteLn('Fast file backup and mirroring');
  WriteLn('Copyright (C) 2008-2015 Thomas Munk');
  WriteLn('http://mmmunk.dk/onewaysynchronize');
  { Check parameters }
  if ParseParameters then
  begin
    { Setup things }
    SetThreadExecutionState($80000000 {ES_CONTINUOUS} or $00000001 {ES_SYSTEM_REQUIRED});  { Keep Windows awake for the lifetime of this thread }
    MaxTimeStampDiff100ns:=TimeStampDiff*10000000;
    Level1:=True;  { Starting at 1. directory level in source dir }
    Log_OpenFile;
    Log_WriteLn(FormatDateTime('"---------- Start: "dddd, dddddd, tt" ----------"', Now), False);
    { Run synchronization }
    try
      SyncDirPair({ExtendedLengthPath}SrcDir+PathDelim, {ExtendedLengthPath}DstDir+PathDelim);
      { Statistics }
      Log_WriteLn(sLineBreak+'Directories scanned: '+IntToStr(CountChangeDir), True);
      if CountDeleteDir > 0 then Log_WriteLn('Directories deleted: '+IntToStr(CountDeleteDir), True);
      if CountCreateDir > 0 then Log_WriteLn('Directories created: '+IntToStr(CountCreateDir), True);
      if CountDeleteFile > 0 then Log_WriteLn('Files deleted:       '+IntToStr(CountDeleteFile), True);
      if CountCopyFileChanged > 0 then Log_WriteLn('Files overwritten:   '+IntToStr(CountCopyFileChanged), True);
      if CountCopyFileNew > 0 then Log_WriteLn('New files copied:    '+IntToStr(CountCopyFileNew), True);
      if CountError > 0 then Log_WriteLn('Errors:              '+IntToStr(CountError), True);
      if (CountDeleteDir = 0) and (CountCreateDir = 0) and (CountDeleteFile = 0) and (CountCopyFileChanged = 0) and (CountCopyFileNew = 0) and (CountError = 0) then
        Log_WriteLn('Source and destination match', True);
      {$ifdef DEBUG}
      WriteLn(sLineBreak, 'Press Enter to continue...');
      ReadLn;
      {$endif}
    except
      on E: Exception do Log_WriteLn(sLineBreak+'EXCEPTION: '+E.Message+'; synchronization terminated!', True);
    end;
    Log_WriteLn('Stop: '+TimeToStr(Time)+sLineBreak, False);
    Log_CloseFile;
  end;
  { Clean up }
  ExclFileSpecs.Free;
  ExclDirSpecs.Free;
  InclFileSpecs.Free;
  InclDirSpecs.Free;
end.
