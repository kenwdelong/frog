unit NewFrogImage;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls;

type
  TfrmNewFrogImage = class(TForm)
    pnlPhase: TPanel;
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    { Private declarations }
    mResized: Boolean;
    mImgImage: TfrmNewFrogImage;
  public
    { Public declarations }
    property Resized: Boolean read mResized;
    property ImgImage: TfrmNewFrogImage read mImgImage;
  end;

var
  frmNewFrogImage: TfrmNewFrogImage;

implementation

{$R *.DFM}

procedure TfrmNewFrogImage.FormCreate(Sender: TObject);
begin
	Height := 190;
	Width := 150;
  mImgImage := Self;
end;

procedure TfrmNewFrogImage.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  Action := caNone;
end;

end.
