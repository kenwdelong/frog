unit WindowMgr;

interface

uses FrogObj, Registry, Forms, Classes;

type
  TWindowManager = class(TFrogObject)
    private
      mRegistry: TRegistry;
      mUseDefaults: Boolean;
      mAppPath: string;
      mDefaultFrogPath: string;
      mDefaultFrogPathNull: Boolean;
      mDefaultTadPath: string;
      mDefaultTadPathNull: Boolean;
      mUserLogDir: string;
      procedure NavigateToFrog;
      procedure CloseRegistry;
      procedure SaveToReg(pForm: TForm);
      procedure LoadFromReg(pForm: TForm);
      procedure OpenThisForm(pForm: TForm);
      procedure LoadDefaultFrogPath;
      procedure SetDefaultFrogPath(pPath: string);
      procedure LoadDefaultTadPath;
      procedure SetDefaultTadPath(pPath: string);
    public
      procedure InitializeThisWindow(pForm: TForm);
      procedure SaveAsDefaults(pForm: TForm);
      function LoadDefaultAlgoCals: TStringList;
      procedure SaveDefaultAlgoCals(pAlgoCals: TStringList);
      function GetOutputFormat: integer;
      procedure SaveOutputFormat(format: integer);
      property UseDefaults: Boolean write mUseDefaults;
      property AppPath: string read mAppPath;
      property DefaultFrogPath: string read mDefaultFrogPath write mDefaultFrogPath;
      property DefaultFrogPathNull: Boolean read mDefaultFrogPathNull;
      property DefaultTadPath: string read mDefaultTadPath write mDefaultTadPath;
      property DefaultTadPathNull: Boolean read mDefaultTadPathNull;
      constructor Create;
      destructor Destroy; override;
  end;

var
   // Semi-singleton: created and destroyed in MainForm.
  WindowManager: TWindowManager;

const
  LEFT_KEY = 'Left';
  TOP_KEY = 'Top';
  HEIGHT_KEY = 'Height';
  WIDTH_KEY = 'Width';
  DEFAULT_FROG_PATH_KEY = 'Default Frog Path';
  DEFAULT_TAD_PATH_KEY = 'Default Tad Path';
  NULL_KEY = 'null';
  GRID_SIZE = 'grid';
  RUN_TYPE = 'runtype';
  NLO_TYPE = 'nlotype';
  OUTPUT_FORMAT = 'outputformat';


implementation

uses SysUtils;

constructor TWindowManager.Create;
var
  homepath, homedrive: string;
begin
  inherited Create;
  mAppPath := GetCurrentDir;

  homepath := GetEnvironmentVariable('HOMEPATH');
  homedrive := GetEnvironmentVariable('HOMEDRIVE');
  mUserLogDir := homedrive + homepath + '\Femtosoft\FROG\';
  // Create the directory just in case it wasn't already created, otherwise it throws an error
  SysUtils.ForceDirectories(mUserLogDir);

  // Set up the default FROG path
  mDefaultFrogPath := NULL_KEY;
  LoadDefaultFrogPath;
  mDefaultFrogPathNull := False;
  if mDefaultFrogPath = NULL_KEY then mDefaultFrogPathNull := True;

  // Set up the default tad path
  mDefaulttadPath := NULL_KEY;
  LoadDefaulttadPath;
  mDefaultTadPathNull := False;
  if mDefaultTadPath = NULL_KEY then mDefaultTadPathNull := True;
end;

destructor TWindowManager.Destroy;
begin
  SetDefaultFrogPath(mDefaultFrogPath);
  SetDefaultTadPath(mDefaultTadPath);
  inherited;
end;

// This is the public interface
procedure TWindowManager.InitializeThisWindow(pForm: TForm);
var
  existing: Boolean;
begin
  // The forms still get inited to their default values coming in.

  // This is crappy design but. . .
  if mUseDefaults then
  begin
    SaveAsDefaults(pForm);
    Exit;
  end;

  // Try opening this form.  Any problems, and give it a miss.
  try
    OpenThisForm(pForm);
  except
    Exit;
  end;

  // Check for the existance of the values.  If they are not there, then save
  // the form's values as the defaults.
  existing := mRegistry.ValueExists('Left');
  try
    if existing then
      LoadFromReg(pForm)
    else
      SaveToReg(pForm);
  except
    // if there was any error accessing the registry, then abort
  end;

  CloseRegistry;
end;

// This opens the key for the specified form, and throws an exception at the
// slightest hint of trouble.
procedure TWindowManager.OpenThisForm(pForm: TForm);
var
  opened: Boolean;
begin
  // Check to see that we are properly positioned on the FROG key
  NavigateToFrog;
  if mRegistry = nil then raise Exception.Create('FROG not found');

  // Otherwise, open the key that has the same name as the form's title
  opened := mRegistry.OpenKey(pForm.Caption, True);
  if not opened then raise Exception.Create('Key not found');
end;

// Opens the registry key to FROG
procedure TWindowManager.NavigateToFrog;
var
  new: Boolean;
begin
  mRegistry := TRegistry.Create;
  new := mRegistry.OpenKey('Software', False);
  if new then new := mRegistry.OpenKey('Femtosoft Technologies', True);
  if new then new := mRegistry.OpenKey('FROG', True);

  // If it failed, then trash the registry object.  That's our signal
  // to use the default values.
  if not new then
  begin
    mRegistry.Free;
    mRegistry := nil;
  end;
end;

// Closes the registry.  Note that it doesn't save the key!
procedure TWindowManager.CloseRegistry;
begin
  mRegistry.Free;
  mRegistry := nil;
end;

procedure TWindowManager.SaveAsDefaults(pForm: TForm);
begin
  // Try opening this form.  Any problems, and give it a miss.
  try
    OpenThisForm(pForm);
  except
    Exit;
  end;

  try
    SaveToReg(pForm);
  except
  end;

  CloseRegistry;
end;

procedure TWindowManager.LoadFromReg(pForm: TForm);
var
  newHeight, newWidth, newLeft, newTop: integer;
begin
  // The mReg is already pointing at the proper key
  newLeft := mRegistry.ReadInteger(LEFT_KEY);
  newTop := mRegistry.ReadInteger(TOP_KEY);
  newHeight := mRegistry.ReadInteger(HEIGHT_KEY);
  newWidth := mRegistry.ReadInteger(WIDTH_KEY);

  // If all is succesful (no exceptions raised)
  pForm.Left := newLeft;
  pForm.Top := newTop;
  pForm.Height := newHeight;
  pForm.Width := newWidth;
end;

procedure TWindowManager.SaveToReg(pForm: TForm);
begin
  // Pre: the reg is already pointing at the proper key
  mRegistry.WriteInteger(LEFT_KEY, pForm.Left);
  mRegistry.WriteInteger(TOP_KEY, pForm.Top);
  mRegistry.WriteInteger(HEIGHT_KEY, pForm.Height);
  mRegistry.WriteInteger(WIDTH_KEY, pForm.Width);

  // If there's no errors (no exceptions) then save it
  mRegistry.CloseKey;
end;

// *************** These bits manage the app path ***********************
procedure TWindowManager.LoadDefaultFrogPath;
begin
  if mDefaultFrogPath <> NULL_KEY then Exit;

  NavigateToFrog;
  if mRegistry = nil then Exit;

  try
    if mRegistry.ValueExists(DEFAULT_FROG_PATH_KEY) then
      mDefaultFrogPath := mRegistry.ReadString(DEFAULT_FROG_PATH_KEY)
    else
      mDefaultFrogPath := mUserLogDir;
  except
  end;
  CloseRegistry;
end;

procedure TWindowManager.SetDefaultFrogPath(pPath: string);
begin
  NavigateToFrog;
  if mRegistry = nil then Exit;

  try
    mRegistry.WriteString(DEFAULT_FROG_PATH_KEY, pPath);
  except
  end;
  CloseRegistry;
end;

procedure TWindowManager.LoadDefaultTadPath;
begin
  if mDefaultTadPath <> NULL_KEY then Exit;

  NavigateToFrog;
  if mRegistry = nil then Exit;

  try
    if mRegistry.ValueExists(DEFAULT_TAD_PATH_KEY) then
      mDefaultTadPath := mRegistry.ReadString(DEFAULT_TAD_PATH_KEY)
    else
      mDefaultTadPath := mUserLogDir;
  except
  end;
  CloseRegistry;
end;

procedure TWindowManager.SetDefaultTadPath(pPath: string);
begin
  NavigateToFrog;
  if mRegistry = nil then Exit;

  try
    mRegistry.WriteString(DEFAULT_TAD_PATH_KEY, pPath);
  except
  end;
  CloseRegistry;
end;

function TWindowManager.LoadDefaultAlgoCals: TStringList;
var
   algoCals: TStringList;
begin
  NavigateToFrog;
  if mRegistry = nil then Exit;

  algoCals := TStringList.Create;
  try
  begin
    if mRegistry.ValueExists(GRID_SIZE) then
      algoCals.Add(mRegistry.ReadString(GRID_SIZE))
    else
      algoCals.Add('64');

    if mRegistry.ValueExists(RUN_TYPE) then
      algoCals.Add(mRegistry.ReadString(RUN_TYPE))
    else
      algoCals.Add('0');

    if mRegistry.ValueExists(NLO_TYPE) then
      algoCals.Add(mRegistry.ReadString(NLO_TYPE))
    else
      algoCals.Add('0');
  end;
  except
  end;
  CloseRegistry;
  LoadDefaultAlgoCals := algoCals;
end;

procedure TWindowManager.SaveDefaultAlgoCals(pAlgoCals: TStringList);
begin
  NavigateToFrog;
  if mRegistry = nil then Exit;

  try
  begin
    mRegistry.WriteString(GRID_SIZE, pAlgoCals.Strings[0]);
    mRegistry.WriteString(RUN_TYPE, pAlgoCals.Strings[1]);
    mRegistry.WriteString(NLO_TYPE, pAlgoCals.Strings[2]);
    pAlgoCals.Free;
  end;
  except
  end;
  CloseRegistry;
end;


// Used in TOutputCals
function TWindowManager.GetOutputFormat: integer;
begin
  NavigateToFrog;
  if mRegistry = nil then Exit;

  try
  begin
    if mRegistry.ValueExists(OUTPUT_FORMAT) then
      GetOutputFormat := (mRegistry.ReadInteger(OUTPUT_FORMAT))
    else
      GetOutputFormat := 0;
  end;
  except
  end;
  CloseRegistry;
end;

procedure TWindowManager.SaveOutputFormat(format: integer);
begin
  NavigateToFrog;
  if mRegistry = nil then Exit;

  try
  begin
    mRegistry.WriteInteger(OUTPUT_FORMAT, format);
  end;
  except
  end;
  CloseRegistry;
end;

end.
