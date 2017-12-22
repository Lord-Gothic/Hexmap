{******************************************************************************
  HEXMAP COMPONENT  version 1.2

  This component implements a Hexagonal Grid, similer to grids used in
  various wargames.

  All source copyright 1997  Patrick Kemp ( Cynewulf@qadas.com ) and
   Bob Daton (bob.dalton@mustang.com)

  Direct questions and/or comments to:

   Bob Dalton
   Developer Relations Manager
   Mustang Software Inc. (www.mustang.com or
     http://online.mustang.com/devsite/devnews.htm)
   email to:  bob.dalton@mustang.com

  Last modified - 5/25/97

  If you make some improvements to this component
   why not send a copy of the code to both of us
   at the email addresses above and maybe we will incorporate
   it into the next revision of this component.

   Remember:  We all benefit when we share our knowledge and expertise!!

***********************************************************************
***********************************************************************}
{Note:  Place this component in a scroll box!!!}

unit Hexmap;


interface

 uses
    SysUtils,
    WinTypes,
    WinProcs,
    Messages,
    Classes,
    IniFiles,
    Graphics,
    Controls,
    Menus,
    ExtCtrls;

 type TPointType = (ptRowCol,ptXY); { Used in the Convertcoords function }

 type
   THexMap = class(TGraphicControl)
   private
     FSavedImage:Boolean;   { Are we using a previously saved image}
     FHexColumns:Integer;   { Number of Columns in the map }
     FHexRows:Integer;      { Number of rows in the map }
     FHexRadius:Integer;    { The radius of one hexagon }
     FHexShowLabels:Boolean;{ If True, lables are displayed in a hex }
     FHexMapOn:Boolean;     { Do we want the hex grid shown on the map?}
     FHex3d:Boolean;        { If true, hexes are shaded like buttons }
     FHexColor: TColor;     { Color of a Hexagon }
     FBackColor: TColor;    { Backround color of bitmap }
     FLineColor: Tcolor;    { Color of lines used to draw a hexagon}
     FHexMapName:String;    { Name of background map for canvas}
     FDrawSolidMap:Boolean; { Make a solid color map}
     TempMap:TBitMap;       {Used as a drawing surface, then copied to control }
     BitMap1:TBitMap;       {Used to store a background image}

     Rise:integer; {Distance from center to top of hex, need to compute each}
                   {Time the radius changes}

     function findrange(Bpoint:Tpoint;Epoint:TPoint):Integer;
     procedure SetSavedImage(Value:Boolean);
     procedure SetSolidHexMap(Value:Boolean);
     procedure SetHexColumns(Value: Integer);
     procedure SetBackGroundMap(Value: String);
     procedure SetHexRows(Value: Integer);
     procedure SetHexRadius(Value: Integer);
     procedure SetHexShowLabels(Value:Boolean);
     procedure SetHex3d(Value:Boolean);
     procedure SetHexColor(Value: TColor);
     procedure SetBackColor(Value: TColor);
     procedure SetLineColor(Value: TColor);
     procedure SetHexGrid(Value: Boolean);
     procedure MakeSolidMap;
     procedure MakeBackMap;
     procedure DrawSolidHex(Target:TCanvas;         { Canvas to draw hex on }
                            Fillstyle : TBrushStyle; { How to fill the hex }
                            Fillcolor : TColor;      { Color to fill it }
                            Linestyle : TPenStyle;   { Wwhat kind of lines }
                            LineColor : Tcolor;      { What color for lines }
                            x,y,radius: integer;     { Position and size of Hex }
                            button    : boolean);    { Make hexes look like buttons? }

     procedure DrawHexOutline(Target:TCanvas;         { Canvas to draw hex on }
                              Linestyle : TPenStyle;   { Wwhat kind of lines }
                              LineColor : Tcolor;      { What color for lines }
                              x,y,radius: integer;     { Position and size of Hex }
                              button    : boolean);    { Make hexes look like buttons? }

   protected
     procedure Paint; Override;

   public
     constructor Create(AOwner: TComponent); Override;
     destructor destroy; OverRide;

     {returns the range in hexes from Bpoint to EPoint}
     function RangeInHexes(Bpoint:Tpoint;
                           Epoint:TPoint)
                           :Integer;

     function ConvertCoords(point:Tpoint;        { pair to be converted }
                           pointtype:Tpointtype) { Type to be converted }
                           :Tpoint;              { result of conversion }

     procedure PaintAHex(HexColorWanted : Tcolor;
                         HexPatternWanted: TBrushStyle;
                         MapLocation:TPoint);

     procedure SaveHexMap(MapFileName : String);

     function LoadHexMap(MapFileName : String):Boolean;


   published
     property UseSavedMap: Boolean read FSavedImage write SetSavedImage;
     property MapGridOn: Boolean read FHexMapOn write SetHexGrid;
     property MapImageName: String read FHexMapName write SetBackGroundMap;
     property NoMapImage: Boolean read FDrawSolidMap write SetSolidHexMap;
     property HexColumns: Integer read FHexColumns write SetHexColumns;
     property HexRows: Integer read FHexRows write SetHexRows;
     property HexRadius: Integer read FHexRadius write SetHexRadius;
     property HexShowLabels: Boolean read FHexShowLabels write SetHexShowLabels;
     property Hex3d: Boolean read FHex3d write SetHex3d;
     property HexColor: TColor read FHexColor write SetHexColor;
     property BackColor: TColor read FBackColor write SetBackColor;
     property LineColor: TColor read FLineColor write SetLineColor;

     {Inherited properties}
     property Align;
     property Visible;
     property Enabled;
     property Font;
     property DragCursor;
     property DragMode;
     property OnDragDrop;
     property OnDragOver;
     Property OnEndDrag;
     property OnMouseDown;
     property OnMouseMove;
     property OnMouseUp;
     property OnClick;
     property OnDblClick;
     property PopupMenu;

 end;

 procedure Register;


implementation

 constructor THexMap.Create(AOwner: Tcomponent);
 begin
   inherited Create(AOwner);

   {Draw a solid color hex grid map}
   FDrawSolidMap := True;

   {we want the grid displayed on our map}
   FHexMapOn := True;

   {the background map we want to display under the hex grid}
   FHexMapName := 'Rome.bmp';

   TempMap := TBitMap.create;  {prepare the offscreen (temp) bitmap}
   BitMap1 := TBitMap.create;

   FSavedImage := False; {we are not using a previously saved BMP image}

   {Set intial property values for component}
   FHexColumns := 8;
   FHexRows := 8;
   FHexRadius := 30;
   FHexShowLabels := False;
   FHex3d := True;
   FHexColor := clGray;
   FBackColor := clTeal;
   FLineColor := clBlack;

   rise := round(Sqrt(sqr(FHexRadius)-sqr(FhexRadius/2)));

   {creates the grid on temp bitmap}
   if FDrawSolidMap = False then MakeBackMap;
   if FDrawSolidMap = True then MakeSolidMap;

 end;

{******************************************************************************}
{ Free any resources allocated to component                                    }
 destructor ThexMap.Destroy;
 begin
   TempMap.free;
   BitMap1.free;
   inherited Destroy;
 end;

{******************************************************************************}
{Solid Hexagon drawing function}
procedure THexMap.DrawSolidHex(Target:TCanvas;
                          Fillstyle : TBrushStyle;
                          Fillcolor : TColor;
                          Linestyle : TPenStyle;
                          LineColor : Tcolor;
                          x,y,radius: integer;
                          button    : boolean);
 var
   p0,p1,p2,p3,p4,p5,p6:TPoint;
 Begin
   p0 := Point(x,y);

   {compute each point of hex based on center coordinate (p0) }
   p1.x := p0.x - round(Radius /2);
   p1.y := p0.y - Rise;
   p2.x := p0.x + round(Radius /2);
   p2.y := p1.y;
   p3.x := p0.x + Radius;
   p3.y := p0.y;
   p4.x := p2.x;
   p4.y := p0.y + rise;
   p5.x := p1.x;
   p5.y := p4.y;
   p6.x := p0.x - Radius;
   p6.y := p0.y;

   {set color / Style of lines}
   target.pen.color := linecolor;
   target.pen.style := linestyle;

   {Set color and style of hex }
   target.brush.color := FillColor;
   target.brush.style := FillStyle;

   {draw the hex}
   target.polygon([p1,p2,p3,p4,p5,p6]);

   {If Desired, draw the borders for the hex}
   if button = true then
   begin
     with target do
     begin
       pen.mode:=pmcopy;
       pen.color:=clWhite;
       moveto(p5.x+1,p5.y-1);
       lineto(p6.x+1,p6.y);
       lineto(p1.x+1,p1.y+1);
       lineto(p2.x-1,p2.y+1);
       pen.color:=clBlack;
       lineto(p3.x-1,p3.y);
       lineto(p4.x-1,p4.y-1);
       lineto(p5.x+1,p5.y-1);
     end;
   end;
 end;

{******************************************************************************}
{Hexagon Outline drawing function}
 procedure THexMap.DrawHexOutline(Target:TCanvas;
                          Linestyle : TPenStyle;
                          LineColor : Tcolor;
                          x,y,radius: integer;
                          button    : boolean);
 var
   p0,p1,p2,p3,p4,p5,p6:TPoint;
 Begin
   p0 := Point(x,y);

   {compute each point of hex based on center coordinate (p0) }
   p1.x := p0.x - round(Radius /2);
   p1.y := p0.y - Rise;
   p2.x := p0.x + round(Radius /2);
   p2.y := p1.y;
   p3.x := p0.x + Radius;
   p3.y := p0.y;
   p4.x := p2.x;
   p4.y := p0.y + rise;
   p5.x := p1.x;
   p5.y := p4.y;
   p6.x := p0.x - Radius;
   p6.y := p0.y;

   {set color / Style of lines}
   target.pen.color := linecolor;
   target.pen.style := linestyle;

   {draw the hex}
   target.polyline([p1,p2,p3,p4,p5,p6]);

   {If Desired, draw the borders for the hex}
   if button = true then
   begin
     with target do
     begin
       pen.mode:=pmcopy;
       pen.color:=clWhite;
       moveto(p5.x+1,p5.y-1);
       lineto(p6.x+1,p6.y);
       lineto(p1.x+1,p1.y+1);
       lineto(p2.x-1,p2.y+1);
       pen.color:=clBlack;
       lineto(p3.x-1,p3.y);
       lineto(p4.x-1,p4.y-1);
       lineto(p5.x+1,p5.y-1);
     end;
   end;
 end;
{******************************************************************************}
{ This Function Draws the map                                                  }
 procedure THexMap.paint;
 Begin
   {Copy everything to the Control}
   Canvas.CopyMode := cmSrcCopy;
   Canvas.Draw(0,0,TempMap);
 end;


{******************************************************************************}
{  This function loads a previously created hex map.  This
   will allow you to work with saved images already possessing
   a hexgrid instead of dynamically creating them each time.
}
function THexMap.LoadHexMap(MapFileName : String):Boolean;
begin
 if FileExists(MapFileName) then
  begin
   TempMap.LoadFromFile(MapFileName);{first we load the map}
   width := TempMap.width; {make the control width the same}
   height := TempMap.height;{make the control height the same}
   LoadHexMap:=true; {tell the app we were successful}
   UseSavedMap:=True; {yep, we are using a pre-drawn map}
  end
 else
  LoadHexMap:=false; {the file does not exist}
end;

{******************************************************************************}
{  This function saves a created hex map and creates an INI file called
   HexMap.Ini to store the information on the saved HexMap

   Important Note:  Since these bitmaps are not DIBs be sure to change
   your screen resolution to 256 color or lower prior to editing a
   grid map and saving it or the image file size will be way to large!
}
procedure THexMap.SaveHexMap(MapFileName : String);
var
 HexMapIni:TIniFile;
begin
 {So you won't have to try and remember what the critical stats were
  we are creating a HexMap.Ini in the Windows directory to store the
  information for you and then saving the image file to the CURRENT
  directory.
 }
 HexMapIni := TIniFile.Create('HexMap.Ini');
 HexMapIni.WriteString('Options', 'ImageName', MapFileName);
 HexMapIni.WriteInteger('Options', 'HexColumns', HexColumns);
 HexMapIni.WriteInteger('Options', 'HexRows', HexRows);
 HexMapIni.WriteInteger('Options', 'HexRadius', Hexradius);
 HexMapIni.Free;
 TempMap.SaveToFile(MapFileName);
end;

{******************************************************************************}
{  This function paints an individual hex whatever color is wanted }
procedure THexMap.PaintAHex(HexColorWanted : Tcolor;
                            HexPatternWanted: TBrushStyle;
                            MapLocation:TPoint);
var
 p0:tpoint;
begin
 if (FHexMapOn = True) and (FDrawSolidMap = True) and
  (UseSavedMap = False) then
  begin
   with TempMap.canvas do
   p0 := convertcoords(point(MapLocation.x,MapLocation.y),ptROWCOL);
   drawsolidhex(tempmap.canvas,HexPatternWanted,
    hexcolorwanted,psSolid,linecolor,p0.x,p0.y,hexradius,hex3d);
    Refresh;
  end;
end;
{******************************************************************************}
{  This function constructs the grid                                           }
 procedure THexMap.MakeSolidMap;
 var
   p0:tpoint;
   looprow,loopcol:integer;
   hex_id : string;
 begin
  if (UseSavedMap = False) then
   begin
   {Size component in order to show all rows / cols}
   width := ((HexColumns-1)* round((1.5*HexRadius)))+ (2*Hexradius);
   {div 2) * (3*HexRadius) + (4*HexRadius);}
   height :=((HexRows)*(2*rise))+rise;
   { ((2*hexRadius) * HexRows) + (4*HexRadius);}



   {Size bitmap (Should match component size) }
   TempMap.width := width;
   TempMap.height := height;

   with TempMap.canvas do
   begin
     {set backround color of bitmap}
     brush.color := BackColor;
     fillrect(rect(0,0,TempMap.width,TempMap.height));

     {draw hexes left to right / top to bottom}
     for looprow := 1 to HexRows do
      begin
       for loopcol := 1 to HexColumns do
        begin
         {compute center coordinates}
        p0 := convertcoords(point(Loopcol,Looprow),ptROWCOL);

         {draw the hex}
         if MapGridOn = True then drawsolidhex(tempmap.canvas,bsSolid,
           hexcolor,psSolid,linecolor,p0.x,p0.y,hexradius,hex3d);
         {If desired, draw label for hex}
         if HexShowLabels = true then
         begin
           hex_id := format('%.2d%.2d',[loopcol,looprow]);
           {font := self.font;}
           textout(p0.x - (trunc(textwidth(hex_id) / 2)),
                     p0.y - (textheight(hex_id)),hex_id);


         end;
        end;
      end;
    end;
   end;
  end;

{******************************************************************************}
{  This function constructs the hex grid map with background}
 procedure THexMap.MakeBackMap;
 var
   p0:tpoint;
   looprow,loopcol:integer;
   xr0:Integer;
   yr0:Integer;
   hex_id : string;
 begin
  if (UseSavedMap = False) then
   begin
   xr0:=trunc(BitMap1.width / (1.5*HexRadius));
   FHexColumns :=xr0;
   yr0:=trunc(BitMap1.height /(2*Rise));
   FHexRows := yr0;

   TempMap.width:=BitMap1.width;
   TempMap.height:=BitMap1.height;
   width:=BitMap1.width;
   height:=BitMap1.height;

   with TempMap.canvas do
   begin
    {set backround color of bitmap}
     brush.color := BackColor;
     fillrect(rect(0,0,TempMap.width,TempMap.height));

     CopyMode := cmSrcCopy;
     Draw(0,0,BitMap1);

     {draw hexes left to right / top to bottom}
     for looprow := 1 to HexRows do
     begin
       for loopcol := 1 to HexColumns do
       begin
         {compute center coordinates}
         p0 := convertcoords(point(Loopcol,Looprow),ptROWCOL);
         {draw the hex}
         if MapGridOn = True then drawhexoutline(tempMap.canvas,psSolid,
          linecolor, p0.x,p0.y,hexradius,hex3d);

         {If desired, draw label for hex}
         if HexShowLabels = true then
         begin
           hex_id := format('%.2d%.2d',[loopcol,looprow]);
           {font := self.font;}
           textout(p0.x - (trunc(textwidth(hex_id) / 2)),
                     p0.y - (textheight(hex_id)),hex_id);


         end;
       end;
     end;
   end;
  end;
 end;

{******************************************************************************}
{  This function will return the Row / Col pair based on a given X/Y
   for a using application that calls it}
 function THexMap.ConvertCoords(point:Tpoint;pointtype:Tpointtype):Tpoint;
 var
   temp:TPoint;
 begin
   case PointType of
     ptXY: {Convert from x/y to Row/Col}
     Begin

       temp.x:= round( (point.x + (HexRadius/2) ) / (1.5 * Hexradius));



       if odd(temp.x) then
          temp.y := round( (point.y + rise) / (rise*2))
       else
          temp.y := round( point.y  / (2*rise));

       { This section insures row / col is good}
      if (temp.x < 1) or (temp.y < 1) then
         begin
           temp.x := 0;
           temp.y := 0;
          end
       else if (temp.y > HexRows) or (temp.x > HexColumns) then
         begin
           temp.y := 0;
           temp.x := 0;
         end;

       ConvertCoords := temp;
     end;

     ptRowCol:  { Converts Row/Col to X/Y }
     begin
       if point.x=1 then
        temp.x:= HexRadius
       else
        temp.x := HexRadius+(point.x-1) * (round(1.5 * hexradius));

       if odd(point.x) then
        if point.y=1 then
           temp.y:= rise
        else
           temp.y := rise+(point.y-1) * (2 * rise)
       else
         temp.y := (point.y * (2*rise));

       ConvertCoords := temp;
     end;

   end;
 end;

function THexMap.findrange(Bpoint:Tpoint;Epoint:TPoint):Integer;
 var
  bdx, bdy:integer;
  edx, edy:integer;
  AddToX:Boolean;
  HexCount:Integer;
  GoalReached:Boolean;
  loopcount:integer;
  StopX, StopY:Boolean;
 begin
  loopcount:=HexColumns * Hexrows;
  AddToX:=False;
  HexCount:=0;
  GoalReached := False;
  StopX:=False;
  StopY:=False;
  {bpoint is position you clicked on}
  if Bpoint.y>Epoint.y
   then
    begin
     bdy:=Bpoint.y;
     bdx:=Bpoint.x;
     edy:=Epoint.y;
     edx:=Epoint.x;
     if bdx<edx then AddToX := True;
    end
   else
    begin
     bdy:=Epoint.y;
     bdx:=Epoint.x;
     edy:=Bpoint.y;
     edx:=Bpoint.x;
     if bdx<edx then AddToX := True;
    end;
  Repeat
   begin
    dec(loopcount);
    if not odd(bdx) then
     begin
      inc(HexCount);
      if bdx<>edx then
       begin
        if addtox = true then bdx:=bdx+1;
        if addtox = false then bdx:=bdx-1;
        if bdx=edx then StopX:=True;
       end
      else
       if bdy<>edy then bdy:=bdy-1;
     end
    else
     begin
      inc(HexCount);
      if bdx<>edx then
       begin
        if addtox = true then bdx:=bdx+1;
        if addtox = false then bdx:=bdx-1;
        if bdx=edx then StopX:=True;
       end;
      if bdy<>edy then bdy:=bdy-1;
      if bdy=edy then StopY:=True;
     end;
   end;
  if (bdx=edx) and (bdy=edy) then GoalReached:=True;
  until (GoalReached = True) or (loopcount<=0);
  findrange:=abs(HexCount);
 end;

{******************************************************************************}
{This function will return the range in hexes between a starting hex
 point and a ending hex point for a using application that calls it}
function THexMap.RangeInHexes(Bpoint:Tpoint;Epoint:TPoint):Integer;
var
 dx, tdx, tempdx:integer;
 dy:integer;
 dist:integer;
begin
  {if it's in the same column or row}
  if (Epoint.x-Bpoint.x = 0) or (Epoint.y-Bpoint.y = 0)
  then
   Begin
    dx:=Epoint.x-Bpoint.x;
    dy:=Epoint.y-Bpoint.y;
    dist:=abs(dx)+abs(dy);
   end
 else
  begin
    {it's not in the same column or row}
    dist:=findrange(Bpoint, Epoint);
  end;
 RangeInHexes := dist;
end;

{******************************************************************************}
{ Hexmap Field Implementations                                                 }

 {***}
 Procedure THexMap.SetSolidHexMap(Value:Boolean);
 begin
   if Value <> FDrawSolidMap then
   begin
     FDrawSolidMap := Value;
     if FDrawSolidMap = False then
      Begin
       BitMap1.LoadFromFile(FHexMapName);
       MakeBackMap;
       Refresh;
      End;
     if FDrawSolidMap = True then
      Begin
       MakeSolidMap;
       Refresh;
      End;
   End;
 End;

 {***}
 Procedure THexMap.SetBackGroundMap(Value: String);
 begin
   if Value <> FHexMapName then
   begin
     FHexMapName := Value;
     if (FDrawSolidMap = False) and (UseSavedMap = False) then
      Begin
       BitMap1.LoadFromFile(FHexMapName);
       MakeBackMap;
       Refresh;
      End;
   End;
 End;

 {***}
 Procedure THexMap.SetSavedImage(Value:Boolean);
 begin
   if Value <> FSavedImage then
   begin
     FSavedImage:= Value;
     Refresh;
   End;
 End;

 {***}
 Procedure THexMap.SetHexGrid(Value:Boolean);
 begin
   if Value <> FHexMapOn then
   begin
     FHexMapOn := Value;
     if FDrawSolidMap = False then MakeBackMap;
     if FDrawSolidMap = True then MakeSolidMap;
     Refresh;
   End;
 End;

 Procedure THexMap.SetHexColumns(Value:Integer);
 begin
   if Value <> FHexColumns then
   begin
     FHexColumns := Value;
     if FDrawSolidMap = False then MakeBackMap;
     if FDrawSolidMap = True then MakeSolidMap;
     Refresh;
   End;
 End;

 {***}
 Procedure THexMap.SetHexRows(Value:Integer);
 begin
   if Value <> FHexRows then
   begin
     FHexRows := Value;
     if FDrawSolidMap = False then MakeBackMap;
     if FDrawSolidMap = True then MakeSolidMap;
     Refresh;
   End;
 End;

 {***}
 Procedure THexMap.SetHexRadius(Value:Integer);
 begin
   if Value <> FHexRadius then
   begin
     FHexRadius := Value;
     if Odd(FHexRadius) then
       inc(FHexRadius); { Even values tend to work better }
     {Compute the new rise value}
     rise:=round( Sqrt( sqr(FHexRadius)- sqr(FhexRadius/2)));
     if FDrawSolidMap = False then MakeBackMap;
     if FDrawSolidMap = True then MakeSolidMap;
     Refresh;
   End;
 End;

 {***}
 Procedure THexMap.SetHexShowLabels(Value:Boolean);
 begin
   if Value <> FHexShowLabels then
   begin
     FHexShowLabels := Value;
     if FDrawSolidMap = False then MakeBackMap;
     if FDrawSolidMap = True then MakeSolidMap;
     Refresh;
   End;
 End;

 {***}
 Procedure THexMap.SetHex3d(Value:Boolean);
 begin
   if Value <> FHex3d then
   begin
     FHex3d := Value;
     if FDrawSolidMap = False then MakeBackMap;
     if FDrawSolidMap = True then MakeSolidMap;
     Refresh;
   End;
 End;

 {***}
 Procedure THexMap.SetHexColor(Value: TColor);
 begin
   if Value <> FHexColor then
   begin
     FHexColor := Value;
     if FDrawSolidMap = False then MakeBackMap;
     if FDrawSolidMap = True then MakeSolidMap;
     Refresh;
   End;
 End;

 {***}
 Procedure THexMap.SetBackColor(Value: TColor);
 begin
   if Value <> FBackColor then
   begin
     FBackColor := Value;
     if FDrawSolidMap = False then MakeBackMap;
     if FDrawSolidMap = True then MakeSolidMap;
     Refresh;
   End;
 End;

 {***}
 Procedure THexMap.SetLineColor(Value: TColor);
 begin
   if Value <> FLineColor then
   begin
     FLineColor := Value;
     if FDrawSolidMap = False then MakeBackMap;
     if FDrawSolidMap = True then MakeSolidMap;
     Refresh;
   End;
 End;

{******************************************************************************}
{ Registor procedure to place component on Delphi Component Pallet             }
 procedure Register;
 begin
   RegisterComponents('Game', [THexMap]);
 end;

end.
