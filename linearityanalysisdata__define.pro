pro LinearityAnalysisData::Read_ComecerCyklotron
  
  ;this procedure reads csv-data from Comecer radionuclide calibrators
  ;used at the cyklotron unit. Software version is currently unknown
  
  self.activityUnits = 'GBq'
  self.timeUnits = 'h'
  
  fullPath = self.pathToFile + self.fileName
  
  openr,lun,fullpath,/get_lun
  
  str = ''
  rowIx = 0
  A = []
  t = []
  while ~EOF(lun) do begin
    
    readf,lun,str
    strs = strsplit(str,';',/extract) ;csv data
    
    if rowIx eq 0 then begin
      splits = strsplit(strs[0],'_',/extract)
      self.measStartEndString[0] = splits[0] + ' ' + strjoin(strsplit(splits[1],'-',/extract),':')
    endif
    
    t = [t,double(strs[1])]
    unit = strs[3]
    case strupcase(unit) of
      'BQ' : mltpl = 1d-9
      'KBQ': mltpl = 1d-6
      'MBQ': mltpl = 0.001d
      'GBQ': mltpl = 1d
    endcase
    A = [A,double(strs[2])*mltpl]   ;activity in GBq
    rowIx++
    
  endwhile
  free_lun,lun
  
  ;save last row, i.e. measurement end
  splits = strsplit(strs[0],'_',/extract) 
  self.measStartEndString[1] = splits[0] + ' ' + strjoin(strsplit(splits[1],'-',/extract),':')
  
  t = t/(3600D)  ;time in hours
  
  *self.activity = A
  *self.time = t
  
end

pro LinearityAnalysisData::Read_Capintec
  
  ;this procedure reads csv-data from logger program written by Mikael Peterson
  ;used to log measurement data from Capintec activity meters
  
  self.activityUnits = 'GBq'
  self.timeUnits = 'h'

  fullPath = self.pathToFile + self.fileName
  openr,lun,fullpath,/get_lun

  str = ''
  rowIx = 0
  A = []
  t = []
  
  ;get date delimiter from first line
  readf,lun,str
  strs = strsplit(str,' ' ,/extract)
  if n_elements(strsplit(strs[1],'-',/extract)) eq 3 then dateDelim = '-' else dateDelim = '/'
  
  ;read rest of file
  while ~EOF(lun) do begin
    readf,lun,str
    
    if strmid(str,0,1) eq '#' then continue   ;comment? don't know if needed anymore
    
    strs = strsplit(str,';',/extract)
    date = strsplit(strs[0],dateDelim,/extract)
    if dateDelim eq '/' then begin ;american date format
      year  = date[2]
      month = date[1]
      day   = date[0]
    endif else begin ;normal date format
      year  = date[0]
      month = date[1]
      day   = date[2]
    endelse
    
    time = strsplit(strs[1],':',/extract)
    dateTime = greg2jul(month,day,year,time[0],time[1],time[2])
    if rowIx eq 0 then self.measStartEndString[0] = strjoin([year,month,day],'-') + ' ' + strjoin([time[0],time[1],time[2]],':')
    t = [t,dateTime]
    
    unit = strs[3]
    case strupcase(unit) of
      'BQ' : mltpl = 1d-9
      'KBQ': mltpl = 1d-6
      'MBQ': mltpl = 0.001d
      'GBQ': mltpl = 1d
    endcase
    
    ;replace "," with "." if present to get correct decimal separator
    aString = strjoin(strsplit(strs[2],',',/extract),'.')
    A = [A,double(aString)*mltpl]   ;activity in GBq, check self.activityUnits above
    rowIx++
    
  endwhile
  free_lun,lun
  
  self.measStartEndString[1] = strjoin([year,month,day],'-') + ' ' + strjoin([time[0],time[1],time[2]],':') ;save datetime for first and last datapoint
  t = (t-t[0])*24d   ;set t = 0 at the first measurement and adjust time to hours (see self.timeUnits above)
  
  *self.activity = A
  *self.time = t
  
end

pro LinearityAnalysisData::Read_Fidelis

end

pro LinearityAnalysisData::Read_IBCQMM


end

function LinearityAnalysisData::init, fileName = fileName, pathToFile = pathToFile

  if n_elements(fileName)   then self.fileName   = fileName
  if n_elements(pathToFile) then self.pathToFile = pathToFile

  self.time     = ptr_new(/allocate_heap)
  self.activity = ptr_new(/allocate_heap)

  return,1

end

pro LinearityAnalysisData__define

  void = {LinearityAnalysisData                 ,$
          fileName:''                           ,$
          pathToFile:''                         ,$
          time:ptr_new()                        ,$  ;time vector with first element = 0
          timeUnits:''                          ,$
          timeDelta:dblarr(3)                   ,$  ;min,avg,max of delta between sample points
          activity:ptr_new()                    ,$  ;activity vector
          activityUnits:''                      ,$
          measStartEndString:strarr(2)           $
          }

end