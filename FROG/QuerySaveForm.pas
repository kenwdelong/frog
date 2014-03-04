unit QuerySaveForm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, FileCtrl;

type
  TfrmQuerySave = class(TForm)
    cmdProceed: TButton;
    Label1: TLabel;
    Label2: TLabel;
    rbtOverwrite: TRadioButton;
    rbtDontSave: TRadioButton;
    rbtNewDirectory: TRadioButton;
    dirBox: TDirectoryListBox;
    Label3: TLabel;
    edtDirName: TEdit;
    procedure cmdProceedClick(Sender: TObject);
    procedure rbtNewDirectoryClick(Sender: TObject);
    procedure rbtOverwriteClick(Sender: TObject);
    procedure rbtDontSaveClick(Sender: TObject);
  private
    { Private declarations }
    mDoSave: boolean;
    mNewDir: String;
		procedure DisableDirectoryBox;
  public
    { Public declarations }
    property DoSave: boolean read mDoSave;
    property NewDir: String read mNewDir write mNewDir;
    constructor Create(pDir: String);
  end;

var
  frmQuerySave: TfrmQuerySave;

implementation

{$R *.DFM}

constructor TfrmQuerySave.Create(pDir: String);
begin
	inherited Create(Application);
  dirBox.Directory := pDir;
  mNewDir := pDir;
  mDoSave := True;
  edtDirName.Text := pDir;
end;

procedure TfrmQuerySave.cmdProceedClick(Sender: TObject);
begin
	if dirBox.Enabled then
  	mNewDir := dirBox.Directory;
	ModalResult := mrOK;
end;

procedure TfrmQuerySave.rbtNewDirectoryClick(Sender: TObject);
begin
	 dirBox.Visible := True;
  dirBox.Enabled := True;
  mDoSave := True;
end;

procedure TfrmQuerySave.DisableDirectoryBox;
begin
	 dirBox.Visible := False;
  dirBox.Enabled := False;
end;

procedure TfrmQuerySave.rbtOverwriteClick(Sender: TObject);
begin
  DisableDirectoryBox;
  mDoSave := True;
end;

procedure TfrmQuerySave.rbtDontSaveClick(Sender: TObject);
begin
	 DisableDirectoryBox;
  mDoSave := False;
end;

end.
