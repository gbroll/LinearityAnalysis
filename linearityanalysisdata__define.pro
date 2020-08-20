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
  call_method,'CalcTimeDelta',self
  
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
  call_method,'CalcTimeDelta',self
  
end

pro LinearityAnalysisData::Read_Fidelis

  ;this procedure reads data log from Fidelis (Southern Scientific)
  ;data format is .csv

  self.activityUnits = 'pA'
  self.timeUnits = 'h'

  fullPath = self.pathToFile + self.fileName
  openr,lun,fullpath,/get_lun

  str = ''
  rowIx = 0
  A = []
  t = []

  readf,lun,str  ;headers
  
  ;determine csv delimiter
  strs = strsplit(str,';',/extract)
  if n_elements(strs) eq 1 then csvDelim = ',' else csvDelim = ','

  while ~EOF(lun) do begin

    readf,lun,str
    strs = strsplit(str,csvDelim,/extract) ;csv data

    if rowIx eq 0 then begin
      if n_elements(strsplit(strs[0],'-',/extract)) eq 3 then dateDelim = '-' else dateDelim = '/'

    endif
    date = strsplit(strs[0],dateDelim,/extract)
    time = strsplit(strs[1],':',/extract)

    year   = strtrim(date[2],2)
    month  = strtrim(date[1],2)
    day    = strtrim(date[0],2)
    hour   = strtrim(time[0],2)
    minute = strtrim(time[1],2)
    second = strtrim(time[2],2)

    if rowIx eq 0 then self.measStartEndString[0] = strjoin([year,month,day],'-') + ' ' + strjoin([time[0],time[1],time[2]],':')

    dateTime = greg2jul(month,day,year,hour,minute,second)
    t = [t,dateTime]

    ;for Fidelis, we use measured current rather than activity
    paNew = double(strs[8])
    paBkg = double(strs[30])
    paNew = paNew-paBkg

    ;print,paNew,paBkg

    A = [A,paNew]
    rowIx++
    
  endwhile
  
  t = (t-t[0])*24d   ;set t = 0 at the first measurement and adjust time to hours
  ;print,max(A),min(A)
      
  self.measStartEndString[1] = strjoin([year,month,day],'-') + ' ' + strjoin([time[0],time[1],time[2]],':') ;save datetime for first and last datapoint
      
  *self.activity = A
  *self.time = t    
  call_method,'CalcTimeDelta',self
  
end

pro LinearityAnalysisData::Read_IBCQMM

  ;this procedure reads linearity report ("details") from 
  ;IBC (4.2.0)/QMM (2.3.0) 
  ;data format is .csv, but the file is messy and careful cleaning is needed

  self.activityUnits = 'GBq'
  self.timeUnits = 'h'

  fullPath = self.pathToFile + self.fileName
  openr,lun,fullpath,/get_lun

  str = ''
  rowIx = 0
  A = []
  t = []
  
  ;try to determine csv-delimiter
  readf,lun,str
  strs = strsplit(str,';',/extract)
  if n_elements(strs) eq 1 then csvDelim = ',' else csvDelim = ','
  ;print,'csv delimiter: ',csvDelim
   
  sampleCounter = 0  ;use index number to identify measurement data in messy csv
  
  while ~EOF(lun) do begin
    on_ioerror,noData  ;use this flag to get rid of type conversion error messages
    
    readf,lun,str
    strs = strsplit(str,csvDelim,/extract) ;csv data
    
    ;need some way to find data strings
    sampleNum = fix(strs[0])
    
    if (sampleNum eq 0) or (sampleNum ne sampleCounter+1) then continue  ;meas data should be an integer sequence, else skip to next line

    sampleCounter++
    ;print,sampleNum,sampleCounter
    
    dateTime = strsplit(strs[1],' ',/extract)
    date = dateTime[0]
    if n_elements(strsplit(date,'-',/extract)) eq 3 then dateDelim = '-' else dateDelim = '/'
    date     = strsplit(date,dateDelim,/extract)
    time     = strsplit(dateTime[1],':',/extract)
    
    year   = strtrim(date[0],2)
    month  = strtrim(date[1],2)
    day    = strtrim(date[2],2)
    hour   = strtrim(time[0],2)
    minute = strtrim(time[1],2)
    if n_elements(time) gt 2 then second = strtrim(time[2],2) else second = '00'
    
    if sampleNum eq 1 then self.measStartEndString[0] = strjoin([year,month,day],'-') + ' ' + strjoin([hour,minute,second],':')

    julDateTime = greg2Jul(month,day,year,hour,minute,second)
    ;print,julDateTime
    t = [t,julDateTime]

    ;get measured activity
    aString = strsplit(strs[2],' ',/extract)
    unit = aString[1]
    case strupcase(unit) of
      'BQ' : mltpl = 1d-9
      'KBQ': mltpl = 1d-6
      'MBQ': mltpl = 0.001d
      'GBQ': mltpl = 1d
    endcase
    
      A = [A,double(aString[0])*mltpl]

      noData: ;do nothing, just get rid of the type conversion errors

  endwhile

  self.measStartEndString[1] = strjoin([year,month,day],'-') + ' ' + strjoin([hour,minute,second],':') ;save datetime for first and last datapoint
  t = (t-t[0])*24d   ;set t = 0 at the first measurement and adjust time to hours

  *self.activity = A
  *self.time = t    
  call_method,'CalcTimeDelta',self

end

pro LinearityAnalysisData::CalcTimeDelta

 ; this procedure calculates delta-t [min,average,max]
  t = *self.time
  
  deltaTVec = (shift(t,-1)-t)[0:-2]
  self.timeDelta = [min(deltaTVec),mean(deltaTVec),max(deltaTVec)]

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