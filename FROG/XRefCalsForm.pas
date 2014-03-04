unit XRefCalsForm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  CalsForm, Grids, FileCtrl, StdCtrls, ComCtrls, ExtCtrls, RunTypeU;

type
  TfrmXRefCals = class(TfrmCalibrations)
    lblPrevious: TLabel;
    procedure FormCreate(Sender: TObject); override;
    procedure rdgDataSourceClick(Sender: TObject);
  private
    { Private declarations }
    mRefFieldAvailable: boolean;
    procedure CheckForPresetReferenceWithoutRefField;
  protected
    procedure RefreshExperTab; override;
  public
    { Public declarations }
    property RefFieldAvailable: boolean read mRefFieldAvailable write mRefFieldAvailable;
  end;

var
  frmXRefCals: TfrmXRefCals;

implementation

uses AlgoCals;

{$R *.DFM}


procedure TfrmXRefCals.FormCreate(Sender: TObject);
begin
  inherited;

  // These will be set during the test field calibrations
  tbsOutputs.TabVisible := False;
  tbsStrategies.TabVisible := False;
  tbsFirstGuess.TabVisible := False;

  // Disable other thingys, too
  rdgNLO.Enabled := False;
  rdgNLO.Visible := False;
  rdgGridSize.Enabled := False;
  rdgGridSize.Visible := False;
end;

procedure TfrmXRefCals.rdgDataSourceClick(Sender: TObject);
begin
  //inherited;
  CheckForPresetReferenceWithoutRefField;
  case rdgDataSource.ItemIndex of
    Ord(rteTheory):
      begin
      AllCals.AlgoCals.RunTypeEnum := rteTheory;
      tbsInitialField.TabVisible := True;
      tbsExpData.TabVisible := False;
      tbsInputData.TabVisible := False;
      tbsMarginals.TabVisible := False;
      pgcCals.ActivePage := tbsInitialField;
      pgcCals.Visible := True;
      RefreshInitialFieldTab;
      end;
    Ord(rteExperimental):
      begin
      AllCals.AlgoCals.RunTypeEnum := rteExperimental;
      tbsInitialField.TabVisible := False;
      tbsExpData.TabVisible := False;
      tbsInputData.TabVisible := False;
      tbsMarginals.TabVisible := False;
      pgcCals.ActivePage := tbsExpData;
      pgcCals.Visible := False;
      if not mRefFieldAvailable then
      begin
        ShowMessage('No previous FROG retrieval available.');
        cmdOK.Enabled := False;
        lblPrevious.Visible := False;
      end
      else
      begin
        cmdOK.Enabled := True;
        lblPrevious.Visible := True;
      end;
      end;
    Ord(rteReadIn):
      begin
      AllCals.AlgoCals.RunTypeEnum := rteReadIn;
      tbsInitialField.TabVisible := False;
      tbsExpData.TabVisible := False;
      tbsInputData.TabVisible := True;
      tbsMarginals.TabVisible := False;
      pgcCals.ActivePage := tbsInputData;
      pgcCals.Visible := True;
      RefreshReadInTab;
      end;
  end;
end;

procedure TfrmXRefCals.RefreshExperTab;
begin
  inherited;
  if (rdgDataSource.ItemIndex = Ord(rteExperimental))
      and mRefFieldAvailable then
        cmdOK.Enabled := True;

end;

procedure TfrmXRefCals.CheckForPresetReferenceWithoutRefField;
begin
  if (rdgDataSource.ItemIndex = Ord(rteExperimental))
      and not mRefFieldAvailable and not Self.Visible then
        rdgDataSource.ItemIndex := Ord(rteTheory);
end;

end.
