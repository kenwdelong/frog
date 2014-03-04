unit XTestCalsForm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  CalsForm, Grids, FileCtrl, StdCtrls, ComCtrls, ExtCtrls, RunTypeU;

type
  TfrmXTestCals = class(TfrmCalibrations)
    procedure FormCreate(Sender: TObject);  override;
    procedure rdgDataSourceClick(Sender: TObject);
    procedure rdgNLOClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmXTestCals: TfrmXTestCals;

implementation

uses XStratCals, AlgoCals, NLO;

{$R *.DFM}

procedure TfrmXTestCals.FormCreate(Sender: TObject);
begin
  inherited;

  tbsMarginals.TabVisible := False;

  AllCals.StrategyCals.Free;
  AllCals.StrategyCals := TXStrategyCals.Create;
  AllCals.StrategyCals := AllCals.StrategyCals;
  InitStratTab;
  RefreshStratTab;

end;

procedure TfrmXTestCals.rdgDataSourceClick(Sender: TObject);
begin
  //inherited;
  case rdgDataSource.ItemIndex of
    Ord(rteTheory):
      begin
      AllCals.AlgoCals.RunTypeEnum := rteTheory;
      tbsInitialField.TabVisible := True;
      tbsExpData.TabVisible := False;
      tbsInputData.TabVisible := False;
      pgcCals.ActivePage := tbsInitialField;
      RefreshInitialFieldTab;
      RefreshFirstGuessTab;
      end;
    Ord(rteExperimental):
      begin
      AllCals.AlgoCals.RunTypeEnum := rteExperimental;
      tbsInitialField.TabVisible := False;
      tbsExpData.TabVisible := True;
      tbsInputData.TabVisible := False;
      pgcCals.ActivePage := tbsExpData;
      RefreshExperTab;
      end;
    Ord(rteReadIn):
      begin
      AllCals.AlgoCals.RunTypeEnum := rteReadIn;
      tbsInitialField.TabVisible := False;
      tbsExpData.TabVisible := False;
      tbsInputData.TabVisible := True;
      pgcCals.ActivePage := tbsInputData;
      RefreshReadInTab;
      end;
  end;
end;

procedure TfrmXTestCals.rdgNLOClick(Sender: TObject);
begin
  inherited;
  case rdgNLO.ItemIndex of
    Ord(nlDFG1): AllCals.AlgoCals.NLOType := nlDFG1;
    Ord(nlDFG2): AllCals.AlgoCals.NLOType := nlDFG2;
  end;
end;

end.
