pro LinearityAnalysisData::Read_ComecerCyklotron
  
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