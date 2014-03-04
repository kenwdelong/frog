unit AlgoControl;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, FrogAlgo;

type
  TFrmControlAlgo = class(TForm)
    BtnBeginPauseResume: TButton;
    BtnQuitStop: TButton;
    EdtStatus: TEdit;
    MemFROGError: TMemo;
    LblAlgo: TLabel;
    LblFROGError: TLabel;
    cmdForceChange: TButton;
    Label1: TLabel;
    edtBestError: TEdit;
    procedure BtnBeginPauseResumeClick(Sender: TObject);
    procedure BtnQuitStopClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure cmdForceChangeClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    { Private declarations }
    // Events
    mOnBegin: TNotifyEvent;
    mOnPause: TNotifyEvent;
    mOnResume: TNotifyEvent;
    mOnStop: TNotifyEvent;
    mOnQuit: TNotifyEvent;
    mOnForceChange: TNotifyEvent;
    mClosingProperly: boolean;
  public
    { Public declarations }
    //procedure TheBossIs(pAlgoMgr: TFrogAlgorithmDirector);
    procedure StatusMessage(pMess: string);
    procedure Clear;
    procedure Add(pMessage: string);
    procedure SetBestError(pError: double);
    property OnBegin: TNotifyEvent read mOnBegin write mOnBegin;
    property OnPause: TNotifyEvent read mOnPause write mOnPause;
    property OnResume: TNotifyEvent read mOnResume write mOnResume;
    property OnQuit: TNotifyEvent read mOnQuit write mOnQuit;
    property OnStop: TNotifyEvent read mOnStop write mOnStop;
    property OnForceChange: TNotifyEvent read mOnForceChange write mOnForceChange;
  end;

var
  FrmControlAlgo: TFrmControlAlgo;

implementation

uses WindowMgr;

{$R *.DFM}

procedure TFrmControlAlgo.BtnBeginPauseResumeClick(Sender: TObject);
begin
	if BtnBeginPauseResume.Caption = '&Begin' then
	begin
		BtnBeginPauseResume.Caption := '&Pause';
    btnQuitStop.Caption := '&Stop';
    btnQuitStop.SetFocus;
    if Assigned(mOnBegin) then mOnBegin(Self);
	end
	else if BtnBeginPauseResume.Caption = '&Pause' then
	begin
 		BtnBeginPauseResume.Caption := '&Resume';
    if Assigned(mOnPause) then mOnPause(Self);
	end
	else if BtnBeginPauseResume.Caption = '&Resume' then
	begin
  	BtnBeginPauseResume.Caption := '&Pause';
    if Assigned(mOnResume) then mOnResume(Self);
	end;
end;

procedure TFrmControlAlgo.BtnQuitStopClick(Sender: TObject);
begin
	if (BtnQuitStop.Caption = '&Quit') then
	begin
    if Assigned(mOnQuit) then mOnQuit(Self);
    mClosingProperly := True;
    Close;
  end
	else {if BtnQuitStop.Caption = 'Stop' then}
	begin
    btnQuitStop.Caption := '&Quit';
    BtnBeginPauseResume.Caption := '&Begin';
    if Assigned(mOnStop) then mOnStop(Self);
	end;
end;

procedure TFrmControlAlgo.FormCreate(Sender: TObject);
var
	count: Integer;
begin
	//Height := 223;
	//Width := 318;
 mClosingProperly := False;

  // The label controls have problems with large and small fonts
  // This seems to help
  for count := 0 to ComponentCount - 1 do
  	if Components[count] is TLabel then
    begin
    	(Components[count] as TLabel).AutoSize := False;
    	(Components[count] as TLabel).AutoSize := True;
    end;
end;

procedure TFrmControlAlgo.StatusMessage(pMess: string);
begin
  edtStatus.Text := pMess;
end;

procedure TFrmControlAlgo.Clear;
begin
  memFrogError.Lines.Clear;
end;

procedure TFrmControlAlgo.Add(pMessage: string);
begin
  memFrogError.Lines.Add(pMessage);
end;

procedure TFrmControlAlgo.SetBestError(pError: double);
begin
     edtBestError.Text := FloatToStrF(pError, ffGeneral, 5, 5);
end;

procedure TFrmControlAlgo.cmdForceChangeClick(Sender: TObject);
begin
  if Assigned(mOnForceChange) then mOnForceChange(Self);
end;

procedure TFrmControlAlgo.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  if not mClosingProperly then
    Action := caNone
  else
    Action := caFree;
end;

end.
