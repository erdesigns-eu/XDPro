unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, Menus,
  StdCtrls, IdHTTP, Process, JsonTools, Contnrs;

type

  TInstance = class
  private
    FID          : Integer;  // Instance ID
    FName        : string;   // Instance name
    FAPIKey      : string;   // Instance API Key
    FFolder      : string;   // Download folder
    FUseragent   : string;   // Downloader Useragent
    FLimit       : Integer;  // Download speed limit
    FConnections : Boolean;  // Check available connections
  public
    constructor Create(const AID: Integer; const AName: string; const AAPIKey: string; AFolder: string; AUseragent: string; ALimit: Integer; AConnections: Boolean); virtual;

    property ID: Integer read FID;
    property Name: string read FName;
    property APIKey: string read FAPIKey;
    property Folder: string read FFolder;
    property Useragent: string read FUseragent;
    property Limit: Integer read FLimit;
    property Connections: Boolean read FConnections;
  end;

  { TForm1 }
  TForm1 = class(TForm)
    btnLogin: TButton;
    btnStart: TButton;
    btnStop: TButton;
    cbInstances: TComboBox;
    edtUsername: TEdit;
    API: TIdHTTP;
    Download: TIdHTTP;
    edtPassword: TEdit;
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    Image1: TImage;
    About: TMenuItem;
    Label1: TLabel;
    Label2: TLabel;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    N1: TMenuItem;
    TrayPopup: TPopupMenu;
    TrayIcon: TTrayIcon;
    procedure AboutClick(Sender: TObject);
    procedure btnLoginClick(Sender: TObject);
    procedure btnStartClick(Sender: TObject);
    procedure btnStopClick(Sender: TObject);
    procedure cbInstancesChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormWindowStateChange(Sender: TObject);
    procedure MenuItem1Click(Sender: TObject);
    procedure MenuItem2Click(Sender: TObject);
  private
    FInstances : TFPObjectList;
    FActive    : Boolean;

    procedure SetActive(const B: Boolean);
  public
    property Instances: TFPObjectList read FInstances;
    property Active: Boolean read FActive write SetActive;

    function Icon : string;
    function Alert(const ATitle: string; const AText: string) : Boolean;
    function Login(const Username: string; const Password: string) : Boolean;

    procedure SaveSettings;
    procedure LoadSettings(const LoggedIn: Boolean = false);
  end;

var
  Form1: TForm1;

const
  ApplicationTitle = 'XD-Pro by IPTV-Tools.com';
  AlertTitle       = 'XD-Pro';
  AboutText        = 'XD-Pro v1.0 (for Linux)' + sLineBreak + 'By IPTV-Tools.com' + sLineBreak + sLineBreak + 'a product of ERDesigns - Ernst Reidinga.' + sLineBreak +  sLineBreak + 'https://iptv-tools.com' + sLineBreak + 'https://erdesigns.eu' + sLineBreak + 'https://fb.me/erdesignseu';

implementation

{$R *.frm}

{ TForm1 }

// XD Pro Instances
constructor TInstance.Create(const AID: Integer; const AName: string; const AAPIKey: string; AFolder: string; AUseragent: string; ALimit: Integer; AConnections: Boolean);
begin
  inherited Create;
  FID          := AID;
  FName        := AName;
  FAPIKey      := AAPIKey;
  FFolder      := AFolder;
  FUseragent   := AUseragent;
  FLimit       := ALimit;
  FConnections := AConnections;
end;

// Set active setter
procedure TForm1.SetActive(const B: Boolean);
begin
  FActive             := B;
  btnStart.Enabled    := not FActive;
  btnStop.Enabled     := FActive;
  cbInstances.Enabled := not Active;
end;

// Icon for alert
function TForm1.Icon : string;
begin
  Result := '--icon=' + ExtractFilePath(Paramstr(0)) + 'Icon.png';
end;

// Alert
function TForm1.Alert(const ATitle: string; const AText: string) : Boolean;
var
  S : AnsiString;
begin
  Result := RunCommand('notify-send',[ATitle, AText, Icon], S);
end;

// Login
function TForm1.Login(const Username: string; const Password: string) : Boolean;
var
  PN : TJsonNode;
  PS : TStringList;
  RN : TJsonNode;
  RS : TStringStream;
  I  : TJsonNode;
begin
  PN := TJsonNode.Create;
  PS := TStringList.Create;
  RN := TJsonNode.Create;
  RS := TStringStream.Create;
  try
    PN.Add('username', Username);
    PN.Add('password', Password);
    PS.Text := PN.AsJson;
    API.Post('http://xd.iptv-tools.com/authenticate', PS, RS);
    if RN.TryParse(RS.DataString) then
    begin
      Result := RN.Find('status').AsBoolean;
      if Result then
      begin
        // Clear instances list
        Instances.Clear;
        // Add instances
        for I in RN.Find('data').AsArray do
        begin
          Instances.Add(TInstance.Create(
            Trunc(I.Find('id').AsNumber),
            I.Find('name').AsString,
            I.Find('api_key').AsString,
            I.Find('download_folder').AsString,
            I.Find('useragent').AsString,
            Trunc(I.Find('speed_limit').AsNumber),
            I.Find('check_connections').AsBoolean
          ));
        end;
      end;
    end else
      Result := False;
  finally
    PN.Free;
    PS.Free;
    RN.Free;
    RS.Free;
  end;
end;

// Save settings to JSON file
procedure TForm1.SaveSettings;
var
  SF : TStringList;
  SJ : TJsonNode;
  FN : string;
begin
  SF := TStringList.Create;
  SJ := TJsonNode.Create;
  FN := ExtractFilePath(Paramstr(0)) + 'xdpro.conf';
  try
    SJ.Add('username', edtUsername.Text);
    SJ.Add('password', edtPassword.Text);
    SJ.Add('instance', cbInstances.ItemIndex);
    SF.Text := SJ.AsJson;
    SF.SaveToFile(FN);
  finally
    SF.Free;
    SJ.Free;
  end;
end;

// Load settings from JSON file
procedure TForm1.LoadSettings(const LoggedIn: Boolean = false);
var
  SF : TStringList;
  SJ : TJsonNode;
  FN : string;
begin
  SF := TStringList.Create;
  SJ := TJsonNode.Create;
  FN := ExtractFilePath(Paramstr(0)) + 'xdpro.conf';
  try
    if FileExists(FN) then
    begin
      SF.LoadFromFile(FN);
      if SJ.TryParse(SF.Text) then
      begin
        if LoggedIn then
          cbInstances.ItemIndex := Trunc(SJ.Find('instance').AsNumber)
        else
        begin
          edtUsername.Text := SJ.Find('username').AsString;
          edtPassword.Text := SJ.Find('password').AsString;
        end;
      end;
    end;
  finally
    SF.Free;
    SJ.Free;
  end;
end;

procedure TForm1.btnLoginClick(Sender: TObject);
var
  I : Integer;
begin
  if login(edtUsername.text, edtPassword.text) then
  begin
    Alert(AlertTitle, 'Logged in as ' + edtUsername.Text);
    edtUsername.Enabled := False;
    edtPassword.Enabled := False;
    btnLogin.Enabled    := False;
    cbInstances.Clear;
    for I := 0 to Instances.Count -1 do
    begin
      cbInstances.AddItem((Instances[I] as TInstance).Name, (Instances[I] as TInstance));
    end;
    cbInstances.Enabled := cbInstances.Items.Count > 0;
    btnStart.Enabled    := cbInstances.Enabled and (cbInstances.ItemIndex > -1);
    if cbInstances.Enabled then LoadSettings(True);
  end else
    Alert(AlertTitle, 'Invalid username/password combination!');
end;

// Start downloader
procedure TForm1.btnStartClick(Sender: TObject);
begin
  Active := True;
end;

// Stop downloader
procedure TForm1.btnStopClick(Sender: TObject);
begin
  Active := False;
end;

// Enable start button if there is a instance selected
procedure TForm1.cbInstancesChange(Sender: TObject);
begin
  if not Active then
  btnStart.Enabled := (cbInstances.ItemIndex > -1);
end;

// Form create
procedure TForm1.FormCreate(Sender: TObject);
begin
  Caption    := ApplicationTitle;
  FInstances := TFPObjectList.Create(True);
  LoadSettings;
end;

// Form destroy
procedure TForm1.FormDestroy(Sender: TObject);
begin
  SaveSettings;
  FInstances.Free;
end;

// Hide on minimize
procedure TForm1.FormWindowStateChange(Sender: TObject);
begin
   if Form1.WindowState = wsMinimized then
   begin
      Form1.WindowState := wsNormal;
      Form1.Hide;
      Form1.ShowInTaskBar := stNever;
   end;
end;

// Show about
procedure TForm1.AboutClick(Sender: TObject);
begin
  ShowMessage(AboutText);
end;

// Restore window
procedure TForm1.MenuItem1Click(Sender: TObject);
begin
  Form1.Visible     := True;
  Form1.WindowState := wsNormal;
  BringToFront;
end;

// Exit
procedure TForm1.MenuItem2Click(Sender: TObject);
begin
  Active := False;
  Application.Terminate;
end;

end.

