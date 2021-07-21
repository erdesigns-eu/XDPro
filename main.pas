unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, Menus,
  StdCtrls, IdHTTP, IdInterceptThrottler, Process, JsonTools, Contnrs,
  IdComponent;

type

  TInstance = class
  private
    FID          : Integer;  // Instance ID
    FName        : string;   // Instance name
    FAPIKey      : string;   // Instance HTTP_API Key
    FFolder      : string;   // HTTP_Download folder
    FUseragent   : string;   // Downloader Useragent
    FLimit       : Integer;  // HTTP_Download speed limit
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

  TDownload = class
  private
    FID            : Integer; // HTTP_Download ID
    FURL           : string;  // HTTP_Download URL
    FHost          : string;  // IPTV Host
    FPort          : string;  // IPTV Port
    FUsername      : string;  // IPTV Username
    FPassword      : string;  // IPTV Password
    FFilename      : string;  // Filename
    FFileExtension : string;  // File Extension
    FType          : Integer; // HTTP_Download type (Movie/Series/Catch-Up)
    FFolder        : string;  // HTTP_Download folder
  public
    constructor Create(const AID: Integer; const AURL: string; const AHost: string; const APort: string; const AUsername: string; const APassword: string; const AFilename: string; const AFileExtension: string; const AType: Integer; const AFolder: string); virtual;

    property ID: Integer read FID;
    property URL: string read FURL;
    property Host: string read FHost;
    property Port: string read FPort;
    property Username: string read FUsername;
    property Password: string read FPassword;
    property Filename: string read FFilename;
    property FileExtension: string read FFileExtension;
    property FileType: Integer read FType;
    property Folder: string read FFolder;
  end;

  { TfrmMain }
  TfrmMain = class(TForm)
    btnLogin: TButton;
    btnStart: TButton;
    btnStop: TButton;
    cbInstances: TComboBox;
    edtUsername: TEdit;
    HTTP_API: TIdHTTP;
    HTTP_Download: TIdHTTP;
    edtPassword: TEdit;
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    Throttler: TIdInterceptThrottler;
    Image1: TImage;
    About: TMenuItem;
    Label1: TLabel;
    Label2: TLabel;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    N1: TMenuItem;
    DownloadTimer: TTimer;
    TrayPopup: TPopupMenu;
    TrayIcon: TTrayIcon;
    procedure AboutClick(Sender: TObject);
    procedure btnLoginClick(Sender: TObject);
    procedure btnStartClick(Sender: TObject);
    procedure btnStopClick(Sender: TObject);
    procedure cbInstancesChange(Sender: TObject);
    procedure DownloadTimerTimer(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormWindowStateChange(Sender: TObject);
    procedure HTTP_DownloadDisconnected(Sender: TObject);
    procedure HTTP_DownloadWork(ASender: TObject; AWorkMode: TWorkMode;
      AWorkCount: Int64);
    procedure HTTP_DownloadWorkBegin(ASender: TObject; AWorkMode: TWorkMode;
      AWorkCountMax: Int64);
    procedure HTTP_DownloadWorkEnd(ASender: TObject; AWorkMode: TWorkMode);
    procedure MenuItem1Click(Sender: TObject);
    procedure MenuItem2Click(Sender: TObject);
  private
    FInstances : TFPObjectList;
    FActive    : Boolean;
    FDownload  : TDownload;

    FDownloadMax: Int64;
    FDownloadProgress: Int64;
    FProgress: Integer;
    FDownloading: Boolean;

    procedure SetActive(const B: Boolean);
    procedure SetDownload(const D: TDownload);
  public
    property Instances: TFPObjectList read FInstances;
    property Active: Boolean read FActive write SetActive;
    property Download: TDownload read FDownload write SetDownload;
    property Progress: Integer read FProgress;
    property IsDownloading: Boolean read FDownloading write FDownloading;

    function Icon : string;
    function Alert(const ATitle: string; const AText: string) : Boolean;
    function Login(const Username: string; const Password: string) : Boolean;

    procedure SaveSettings;
    procedure LoadSettings(const LoggedIn: Boolean = false);

    procedure GetDownload;
    function CheckConnection : Boolean;
    procedure StartDownload;
    procedure UpdateProgress(const P: Integer);
  end;

var
  frmMain: TfrmMain;

const
  ApplicationTitle = 'XD-Pro by IPTV-Tools.com';
  AlertTitle       = 'XD-Pro';
  AboutText        = 'XD-Pro v1.0 (for Linux)' + sLineBreak + 'By IPTV-Tools.com' + sLineBreak + sLineBreak + 'a product of ERDesigns - Ernst Reidinga.' + sLineBreak +  sLineBreak + 'https://iptv-tools.com' + sLineBreak + 'https://erdesigns.eu' + sLineBreak + 'https://fb.me/erdesignseu';

implementation

{$R *.frm}

{ TfrmMain }

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

// XD Pro HTTP_Download
constructor TDownload.Create(const AID: Integer; const AURL: string; const AHost: string; const APort: string; const AUsername: string; const APassword: string; const AFilename: string; const AFileExtension: string; const AType: Integer; const AFolder: string);
begin
  inherited Create;
  FID            := AID;
  FURL           := AURL;
  FHost          := AHost;
  FPort          := APort;
  FUsername      := AUsername;
  FPassword      := APassword;
  FFilename      := AFilename;
  FFileExtension := AFileExtension;
  FType          := AType;
  FFolder        := AFolder;
end;

// Set active setter
procedure TfrmMain.SetActive(const B: Boolean);
const
  M: Int64 = 1048576;
begin
  if (cbInstances.ItemIndex = -1) then Exit;

  FActive             := B;
  btnStart.Enabled    := not FActive;
  btnStop.Enabled     := FActive;
  cbInstances.Enabled := not Active;

  // If stopped - stop download
  if (not B) and HTTP_Download.Connected then HTTP_Download.Disconnect(True);

  // Set useragent for downloader
  HTTP_Download.Request.UserAgent := (cbInstances.Items.Objects[cbInstances.ItemIndex] as TInstance).Useragent;

  // Set throttle (if download limit is set)
  case (cbInstances.Items.Objects[cbInstances.ItemIndex] as TInstance).Limit of

    // No download limit
    0 : begin
          HTTP_Download.Intercept := nil;
        end;

    // 20Mbs limit
    1 : begin
          Throttler.BitsPerSec := M * 20;
          HTTP_Download.Intercept := Throttler;
        end;

    // 15 Mbs limit
    2 : begin
          Throttler.BitsPerSec := M * 15;
          HTTP_Download.Intercept := Throttler;
        end;

    // 10 Mbs limit
    3 : begin
          Throttler.BitsPerSec := M * 10;
          HTTP_Download.Intercept := Throttler;
        end;

    // 5 Mbs limit
    4 : begin
          Throttler.BitsPerSec := M * 5;
          HTTP_Download.Intercept := Throttler;
        end;
  end;

  // Enable timer - get download
  DownloadTimer.Enabled := B;
end;

// Set download
procedure TfrmMain.SetDownload(const D: TDownload);
begin
  if Assigned(FDownload) then FDownload.Free;
  if Assigned(D) then
  begin
    FDownload := D;
    StartDownload;
  end;
end;

// Icon for alert
function TfrmMain.Icon : string;
begin
  Result := '--icon=' + ExtractFilePath(Paramstr(0)) + 'Icon.png';
end;

// Alert
function TfrmMain.Alert(const ATitle: string; const AText: string) : Boolean;
var
  S : AnsiString;
begin
  Result := RunCommand('notify-send',[ATitle, AText, Icon], S);
end;

// Login
function TfrmMain.Login(const Username: string; const Password: string) : Boolean;
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
    HTTP_API.Post('http://xd.iptv-tools.com/authenticate', PS, RS);
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
procedure TfrmMain.SaveSettings;
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
procedure TfrmMain.LoadSettings(const LoggedIn: Boolean = false);
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

// Get (next) HTTP_Download
procedure TfrmMain.GetDownload;
var
  PN : TJsonNode;
  PS : TStringList;
  RN : TJsonNode;
  RS : TStringStream;
begin
  // Exit if the downloader is not active (started)
  if (not Active) or (cbInstances.ItemIndex = -1) then Exit;

  PN := TJsonNode.Create;
  PS := TStringList.Create;
  RN := TJsonNode.Create;
  RS := TStringStream.Create;
  try
    PN.Add('api_key', (cbInstances.Items.Objects[cbInstances.ItemIndex] as TInstance).APIKey);
    PS.Text := PN.AsJson;
    HTTP_API.Post('http://xd.iptv-tools.com/download', PS, RS);
    if RN.TryParse(RS.DataString) then
    begin
      if RN.Find('status').AsBoolean then
      begin
        DownloadTimer.Enabled := False;
        Download := TDownload.Create(
          Trunc(RN.Find('data/id').AsNumber),
          RN.Find('data/download_url').AsString,
          RN.Find('data/download_host').AsString,
          RN.Find('data/download_port').AsString,
          RN.Find('data/download_username').AsString,
          RN.Find('data/download_password').AsString,
          RN.Find('data/filename').AsString,
          RN.Find('data/file_extension').AsString,
          Trunc(RN.Find('data/type').AsNumber),
          RN.Find('data/download_folder').AsString
        );
      end;
    end
  finally
    PN.Free;
    PS.Free;
    RN.Free;
    RS.Free;
  end;
end;

// Check if there is a free connection
function TfrmMain.CheckConnection : Boolean;
var
  Res : string;
  RN  : TJsonNode;
begin
  Result := True;
  if (not Download.Host.IsEmpty) and (not Download.Username.IsEmpty) and (not Download.Password.IsEmpty) then
  begin
    // Get connection information
    if (not Download.Port.IsEmpty) then
      Res := HTTP_API.Get(Format('http://%s:%s/player_api.php?username=%s&password=%s', [Download.Host, Download.Port, Download.Username, Download.Password]))
    else
      Res := HTTP_API.Get(Format('http://%s/player_api.php?username=%s&password=%s', [Download.Host, Download.Username, Download.Password]));
    // Try parse JSON result
    RN := TJsonNode.Create;
    try
      if RN.TryParse(Res) then
      begin
        if RN.Find('user_info/auth').AsBoolean then
        begin
          Result := Trunc(RN.Find('user_info/max_connections').AsNumber) > Trunc(RN.Find('user_info/active_cons').AsNumber);
        end else Result := False;
      end else Result := False;
    finally
      RN.Free;
    end;
  end;
end;

// Start downloading file
procedure TfrmMain.StartDownload;
var
  DS : TFileStream;
  PN : TJsonNode;
  PS : TStringList;
  RN : TJsonNode;
  RS : TStringStream;
  FN : string;
  FD : string;
begin
  // Exit if the downloader is not active (started)
  if (not Active) or (cbInstances.ItemIndex = -1) then Exit;

  // Check for free connections
  if (cbInstances.Items.Objects[cbInstances.ItemIndex] as TInstance).Connections then
  begin
    if not CheckConnection then
    begin
      DownloadTimer.Enabled := True;
      Exit;
    end;
  end;

  // Alert server that we are starting this download
  PN := TJsonNode.Create;
  PS := TStringList.Create;
  RN := TJsonNode.Create;
  RS := TStringStream.Create;
  try
    PN.Add('api_key', (cbInstances.Items.Objects[cbInstances.ItemIndex] as TInstance).APIKey);
    PN.Add('id', Download.ID.ToString);
    PS.Text := PN.AsJson;
    HTTP_API.Post('http://xd.iptv-tools.com/start', PS, RS);
    if RN.TryParse(RS.DataString) then
    begin
      if RN.Find('status').AsBoolean then
      begin

        // Download Folder
        if Download.Folder.IsEmpty then
          FD := IncludeTrailingPathDelimiter((cbInstances.Items.Objects[cbInstances.ItemIndex] as TInstance).Folder)
        else
          FD := IncludeTrailingPathDelimiter(Download.Folder);

        // Check if folder exists or create the directory
        if not DirectoryExists(FD) then ForceDirectories(FD);

        // Download Filename
        FN := Download.Filename;

        // Create download stream and start download
        Alert(AlertTitle, 'Starting download: ' + Download.Filename);
        DS := TFileStream.Create(FD + FN, fmCreate or fmShareExclusive);
        try
          HTTP_Download.Get(Download.URL, DS);
        finally
          DS.Free;
        end;

      end else
        DownloadTimer.Enabled := True;
    end else
      DownloadTimer.Enabled := True;
  finally
    PN.Free;
    PS.Free;
    RN.Free;
    RS.Free;
  end;
end;

// Send progression to the server
procedure TfrmMain.UpdateProgress(const P : Integer);
var
  PN : TJsonNode;
  PS : TStringList;
  RS : TStringStream;
begin
  // Exit if the downloader is not active (started)
  if (not Active) or (cbInstances.ItemIndex = -1) then Exit;

  // Update progression
  FProgress := P;

  // Send progress to the server
  PN := TJsonNode.Create;
  PS := TStringList.Create;
  RS := TStringStream.Create;
  try
    PN.Add('id', Download.ID);
    PN.Add('progress', P);
    PS.Text := PN.AsJson;
    HTTP_API.Post('http://xd.iptv-tools.com/progress', PS, RS);

  finally
    PN.Free;
    PS.Free;
    RS.Free;
  end;

  Application.ProcessMessages;
end;

procedure TfrmMain.btnLoginClick(Sender: TObject);
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
    cbInstancesChange(nil);
  end else
    Alert(AlertTitle, 'Invalid username/password combination!');
end;

// Start downloader
procedure TfrmMain.btnStartClick(Sender: TObject);
begin
  Active := True;
end;

// Stop downloader
procedure TfrmMain.btnStopClick(Sender: TObject);
begin
  Active := False;
end;

// Enable start button if there is a instance selected
procedure TfrmMain.cbInstancesChange(Sender: TObject);
begin
  if not Active then
  btnStart.Enabled := (cbInstances.ItemIndex > -1);
end;

// Get download
procedure TfrmMain.DownloadTimerTimer(Sender: TObject);
begin
  if IsDownloading then Exit;
  GetDownload;
end;

// Form create
procedure TfrmMain.FormCreate(Sender: TObject);
begin
  Caption    := ApplicationTitle;
  FInstances := TFPObjectList.Create(True);
  LoadSettings;
end;

// Form destroy
procedure TfrmMain.FormDestroy(Sender: TObject);
begin
  SaveSettings;
  FInstances.Free;
end;

// Hide on minimize
procedure TfrmMain.FormWindowStateChange(Sender: TObject);
begin
   if frmMain.WindowState = wsMinimized then
   begin
      frmMain.WindowState := wsNormal;
      frmMain.Hide;
      frmMain.ShowInTaskBar := stNever;
   end;
end;

// Finished / Failed
procedure TfrmMain.HTTP_DownloadDisconnected(Sender: TObject);
var
  PN : TJsonNode;
  PS : TStringList;
  RS : TStringStream;
begin
  // Exit if the downloader is not active (started)
  if (not Active) or (cbInstances.ItemIndex = -1) then Exit;

  // Send progress to the server
  PN := TJsonNode.Create;
  PS := TStringList.Create;
  RS := TStringStream.Create;
  try

    // Notify server that there was a error during downloading
    if (FDownloadMax > 0) and (FDownloadProgress < FDownloadMax) then
    begin
      PN.Add('id', Download.ID);
      PN.Add('error', 'Download is canceled by the server!');
      PS.Text := PN.AsJson;
      HTTP_API.Post('http://xd.iptv-tools.com/error', PS, RS);
      Alert(AlertTitle, 'Download is canceled by the server!');
    end;

  finally
    PN.Free;
    PS.Free;
    RS.Free;
  end;

  // Enable download timer
  DownloadTimer.Enabled := True;
end;

// Download progression
procedure TfrmMain.HTTP_DownloadWork(ASender: TObject; AWorkMode: TWorkMode;
  AWorkCount: Int64);
var
  P : Integer;
begin
  FDownloadProgress := AWorkCount;
  if (AWorkCount > 0) and (FDownloadMax > 0) then
  P := Trunc((AWorkCount / FDownloadMax) * 100);
  if (Progress <> P) then UpdateProgress(P);
end;

// Set filesize for download progression
procedure TfrmMain.HTTP_DownloadWorkBegin(ASender: TObject; AWorkMode: TWorkMode;
  AWorkCountMax: Int64);
begin
  IsDownloading := True;
  FProgress     := 0;
  FDownloadMax  := AWorkCountMax;
end;

procedure TfrmMain.HTTP_DownloadWorkEnd(ASender: TObject; AWorkMode: TWorkMode);
var
  PN : TJsonNode;
  PS : TStringList;
  RS : TStringStream;
begin
  // Exit if the downloader is not active (started)
  if (not Active) or (cbInstances.ItemIndex = -1) then Exit;

  // Send progress to the server
  PN := TJsonNode.Create;
  PS := TStringList.Create;
  RS := TStringStream.Create;
  try

    // Notify server that there was a error during downloading
    if (FDownloadMax > 0) and (FDownloadProgress < FDownloadMax) then
    begin
      PN.Add('id', Download.ID);
      PN.Add('error', 'Download is canceled by the server!');
      PS.Text := PN.AsJson;
      HTTP_API.Post('http://xd.iptv-tools.com/error', PS, RS);
      Alert(AlertTitle, 'Download is canceled by the server!');
    end else

    // Notify server that we finished
    if (FDownloadMax > 0) and (FDownloadProgress >= FDownloadMax) then
    begin
      PN.Add('id', Download.ID);
      PS.Text := PN.AsJson;
      HTTP_API.Post('http://xd.iptv-tools.com/finish', PS, RS);
      Alert(AlertTitle, 'Download finished: ' + Download.Filename);
    end;

  finally
    PN.Free;
    PS.Free;
    RS.Free;
  end;

  // We are finished
  IsDownloading := False;
end;

// Show about
procedure TfrmMain.AboutClick(Sender: TObject);
begin
  ShowMessage(AboutText);
end;

// Restore window
procedure TfrmMain.MenuItem1Click(Sender: TObject);
begin
  frmMain.Visible     := True;
  frmMain.WindowState := wsNormal;
  BringToFront;
end;

// Exit
procedure TfrmMain.MenuItem2Click(Sender: TObject);
begin
  Active := False;
  Application.Terminate;
end;

end.

