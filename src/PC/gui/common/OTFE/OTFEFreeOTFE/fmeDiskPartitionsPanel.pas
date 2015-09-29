unit fmeDiskPartitionsPanel;

interface

uses
  //delphi and 3rd party libs - layer 0
  SysUtils, Variants, Classes, Windows, Graphics, Messages, Controls, Dialogs, Forms, Vcl.StdCtrls,
  //sdu  - layer 1
  SDUGeneral,
  //LibreCrypt utils -also layer 1
  OTFEFreeOTFEBase_U,  MainSettings,
  CommonSettings,
  PartitionTools,
  // LibreCrypt forms/frames - layer 2
    fmeSDUBlocks, fmeSDUDiskPartitions
  //main form - layer 3
  ;

type

     OBSOLETE replaced TfmeDiskPartitionsPanel with TfmeSDUDiskPartitions 
  TfmeDiskPartitionsPanel = class (TfmeSDUDiskPartitions)
  private

  protected



  public


  published




  end;

implementation

{$R *.dfm}



end.
