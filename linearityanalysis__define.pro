function LinearityAnalysisMonoExp,x,a

  xpn = exp(-a[1] * x)  ;just to avoid calculating the exponential many times

  y = a[0]*xpn+a[2]

  return, y

  ;If the procedure is called with four parameters, calculate the
  ;partial derivatives.
;  if n_params() ge 4 then $
;    pder = [[xpn], [a[0] * (-x) * xpn], [replicate(1.0,n_elements(x))]]

end

function LinearityAnalysisBiExp,x,a

  xpn1 = exp(-a[1] * x)
  xpn2 = exp(-a[3] * x)
  
  y = a[0] * xpn1 + a[2] * xpn2
  
  return, y
  
;  if n_params() ge 4 then $
;    pder = [[xpn1],[a[0] * (-x) * xpn1],[xpn2],[a[2] * (-x) * xpn2]]

end

function LinearityAnalysis::GenerateFitCurve,tVec = tVec

  atData = self.atData

  tVec = (*atData.Time)[self.iLimsFit[0]:self.iLimsFit[1]]
  fParams = self.fitRes
  
  aVec = fParams[0]*exp(-fParams[1]*tVec)     ;standard exponential fit without background correction
  
  nParams = n_elements(fParams)
  aVec =  (nParams gt 2)? aVec + fParams[2]:aVec  ;add background term if it has been fitted 
  
  return,aVec
  
end

pro LinearityAnalysis::ReadData

  if obj_valid(self.atData) then tempPath = self.atData.pathToFile
  atData = obj_new('LinearityAnalysisData')  ;init empty new data object

  fileName = dialog_pickfile(path = tempPath, get_path = path,filter = ['*.txt','*.csv'])

  ;check that file exist and open it for reading
  if ~file_test(fileName,/read,/regular) then begin
    a = dialog_message('The selected file is not readable.')
    return
  endif

  ;save path and file name in data object
  atData.pathToFile = file_dirname(fileName,/mark_dir)
  atData.fileName   = file_basename(fileName)
  

  ;get selected data format
  wRef   = widget_info(self.tlb,find_by_uname = 'SelectFormatDlist')
  format = widget_info(wRef,/droplist_select)
  ;print,format 

  case format of 
    0:dataReadMethod = 'Read_ComecerCyklotron'
    1:dataReadMethod = 'Read_Capintec'
    2:dataReadMethod = 'Read_Fidelis'
    3:dataReadMethod = 'Read_IBCQMM'
  endcase

  ;read data
  call_method,dataReadMethod,atData

  

;case format of
;  
;endcase
; 
;  0:begin  ;Comecer (unknown software version) in cyklotron
;    
;    
;  
;  1:begin ;Capintec
;    
;    self.activityUnits = 'GBq'
;    
;    readf,lun,str   
;    strs = strsplit(str,' ' ,/extract)
;    if n_elements(strsplit(strs[1],'-',/extract)) eq 3 then dateDelim = '-' else dateDelim = '/' 
;    
;    while ~EOF(lun) do begin
;
;      readf,lun,str
;      if strmid(str,0,1) eq '#' then continue
;      
;      strs = strsplit(str,';',/extract)
;      date = strsplit(strs[0],dateDelim,/extract)
;      if dateDelim eq '/' then begin
;        year  = date[2]
;        month = date[1]
;        day   = date[0]
;      endif else begin
;        year  = date[0]
;        month = date[1]
;        day   = date[2]
;      endelse
;      
;      time = strsplit(strs[1],':',/extract)
;          
;      if rowCounter eq 0 then self.measStartEnd[0] = strjoin([year,month,day],'-') + ' ' + strjoin([time[0],time[1],time[2]],':')
;      
;      dateTime = greg2jul(month,day,year,time[0],time[1],time[2])
;      
;      
;      t = [t,dateTime]
;      unit = strs[3]
;      case strupcase(unit) of
;        'BQ' : mltpl = 1d-9
;        'KBQ': mltpl = 1d-6
;        'MBQ': mltpl = 0.001d
;        'GBQ': mltpl = 1d
;      endcase
;      ;replace , with . if present to get correct decimal separator
;      aString = strjoin(strsplit(strs[2],',',/extract),'.')
;      A = [A,double(aString)*mltpl]   ;activity in GBq
;      rowCounter++
;    endwhile
;    free_lun,lun
;    
;    
;   self.measStartEnd[1] = strjoin([year,month,day],'-') + ' ' + strjoin([time[0],time[1],time[2]],':') ;save datetime for first and last datapoint
;   t = (t-t[0])*24d   ;set t = 0 at the first measurement and adjust time to hours
;  end
;
;  2:begin  ;Fidelis CSV-file
;    
;    self.activityUnits = 'pA'
;  
;    readf,lun,str  ;headers
;    strs = strsplit(str,';',/extract) 
;    if n_elements(strs) eq 1 then csvDelim = ',' else csvDelim = ','
;    ;strs = strsplit(str,csvDelim,/extract)   
;    
;    while ~EOF(lun) do begin
;      
;      readf,lun,str
;      strs = strsplit(str,csvDelim,/extract) ;csv data 
;      
;      if rowCounter eq 0 then begin
;        if n_elements(strsplit(strs[0],'-',/extract)) eq 3 then dateDelim = '-' else dateDelim = '/'
;       
;      endif
;      date = strsplit(strs[0],dateDelim,/extract)
;      time = strsplit(strs[1],':',/extract)
;      
;      year   = strtrim(date[2],2)
;      month  = strtrim(date[1],2)
;      day    = strtrim(date[0],2)
;      hour   = strtrim(time[0],2)
;      minute = strtrim(time[1],2)
;      second = strtrim(time[2],2)
;      
;      if rowCounter eq 0 then self.measStartEnd[0] = strjoin([year,month,day],'-') + ' ' + strjoin([time[0],time[1],time[2]],':')
;      
;      dateTime = greg2jul(month,day,year,hour,minute,second)
;      t = [t,dateTime]
;      
;      ;for Fidelis, we use measured current rather than activity
;      paNew = double(strs[8])
;      paBkg = double(strs[30])
;      paNew = paNew-paBkg
;      
;      print,paNew,paBkg
;      
;      A = [A,paNew]
;      rowCounter++
;    endwhile
;    
;    t = (t-t[0])*24d   ;set t = 0 at the first measurement and adjust time to hours
;    print,max(A),min(A)
;    
;    self.measStartEnd[1] = strjoin([year,month,day],'-') + ' ' + strjoin([time[0],time[1],time[2]],':') ;save datetime for first and last datapoint
;    
;    end
;    
;    3: begin  ;IBC (4.2.0)/QMM (2.3.0) csv-file (messy)
;
;      self.activityUnits = 'GBq'
;      ;try to determine csv-delimiter
;      readf,lun,str 
;      strs = strsplit(str,';',/extract)
;      if n_elements(strs) eq 1 then csvDelim = ',' else csvDelim = ','
;     
;      print,'csv delimiter: ',csvDelim
;     
;      sampleCounter = 0
;      while ~EOF(lun) do begin
;        on_ioerror,noData
;
;        readf,lun,str
;        strs = strsplit(str,csvDelim,/extract) ;csv data 
;      
;        ;need some way to find data strings
;        sampleNum = fix(strs[0])
;        ;print,sampleNum
;        
;        if (sampleNum eq 0) or (sampleNum ne sampleCounter+1) then continue  ;meas data should be an integer sequence 
;        
;          sampleCounter++
;          print,sampleNum,sampleCounter
;
;          dateTime = strsplit(strs[1],' ',/extract)
;          date = dateTime[0]
;          if n_elements(strsplit(date,'-',/extract)) eq 3 then dateDelim = '-' else dateDelim = '/' 
;          date     = strsplit(date,dateDelim,/extract)
;          time     = strsplit(dateTime[1],':',/extract)
;      
;          year   = strtrim(date[0],2)
;          month  = strtrim(date[1],2)
;          day    = strtrim(date[2],2)
;          hour   = strtrim(time[0],2)
;          minute = strtrim(time[1],2)
;          if n_elements(time) gt 2 then second = strtrim(time[2],2) else second = '00'
;        
;          if sampleNum eq 1 then self.measStartEnd[0] = strjoin([year,month,day],'-') + ' ' + strjoin([hour,minute,second],':')
;        
;          julDateTime = greg2Jul(month,day,year,hour,minute,second)
;          print,julDateTime
;          t = [t,julDateTime]
;          
;          ;get measured activity
;          aString = strsplit(strs[2],' ',/extract)
;          unit = aString[1]
;          case strupcase(unit) of
;            'BQ' : mltpl = 1d-9
;            'KBQ': mltpl = 1d-6
;            'MBQ': mltpl = 0.001d
;            'GBQ': mltpl = 1d
;          endcase
;          A = [A,double(aString[0])*mltpl]
;      
;          noData: ;do nothing, just get rid of the type conversion errors
;        
;      endwhile
;
;    self.measStartEnd[1] = strjoin([year,month,day],'-') + ' ' + strjoin([hour,minute,second],':') ;save datetime for first and last datapoint
;    t = (t-t[0])*24d   ;set t = 0 at the first measurement and adjust time to hours
;
;    end
;
;  else: begin
;    a = dialog_message('Unknown format: ' + strcompress(format,/rem),/error)
;    return
;  end
;
;endcase
;
;
;free_lun,lun
;
;*self.time = t
;*self.act  = A
;
;;calculate deltaT [min,average,max]
;deltaTVec = (shift(t,-1)-t)[0:-2]
;self.deltaT = [min(deltaTVec),mean(deltaTVec),max(deltaTVec)]
;
;stop
;
;plot the data
wRef = widget_info(self.TLB, find_by_uname = 'plotWin')
widget_control,wRef,get_value = oRef
oRef.select
oRef.erase
self.plotObj[0] = plot(*atData.time,*atData.activity,/ylog,/current, xtitle = 'Elapsed time(h)', ytitle = 'Measured activity (' + atData.activityUnits + ')')
textObj         = text(0.25,0.8,file_basename(atData.fileName),/normal)


;set activity limits to max and min
call_method, 'SetActLims',self,atData,init = 1

;sensitize appropriate widgets
widgets = ['SelectNuclideDlist','DataAverageTextbox','ButtonRun','BkgEstDlist','BkgModelDlist']
foreach widget,widgets do begin
  wRef = widget_info(self.TLB, find_by_uname = widget)
  widget_control,wRef,sensitive = 1
endforeach

;SAVE data object in this object
self.atData = atData

end

pro LinearityAnalysis::SetActLims, atData,init = init

if keyword_set(init) then begin
  
  wRef = widget_info(self.TLB, find_by_uname = 'ActLow')
  widget_control,wRef,set_value = string(min(*atData.activity),format = '(g-12.3)'),sensitive = 1
  wRef = widget_info(self.TLB, find_by_uname = 'ActHigh')
  widget_control,wRef,set_value = string(max(*atData.activity),format = '(g-12.3)' ), sensitive = 1
  
  self.actLims = [min(*atData.activity),max(*atData.activity)]
  
  wRef = widget_info(self.TLB, find_by_uname = 'FitLow')
  widget_control,wRef,set_value = string(min(*atData.activity),format = '(g-12.3)'),sensitive = 1
  wRef = widget_info(self.TLB, find_by_uname = 'FitHigh')
  widget_control,wRef,set_value = string(max(*atData.activity),format = '(g-12.3)' ), sensitive = 1
  
  self.actLimsFit = [min(*atData.activity),max(*atData.activity)]
  
endif

;get value in boxes
;analysis range
wRef = widget_info(self.TLB, find_by_uname = 'ActLow')
widget_control,wRef,get_value = aMin
wRef = widget_info(self.TLB, find_by_uname = 'ActHigh')
widget_control,wRef,get_value = aMax

aMin  = double(aMin[0])
aMax = double(aMax[0]) 
if (aMin lt min(*atData.activity)) or (aMin ge aMax) then aMin = min(*atData.activity)
if (aMax gt max(*atData.activity)) or (aMax le aMin) then aMax = max(*atData.activity)
self.actLims = [aMin,aMax]

;fitting range
wRef = widget_info(self.TLB, find_by_uname = 'FitLow')
widget_control,wRef,get_value = aMinFit
wRef = widget_info(self.TLB, find_by_uname = 'FitHigh')
widget_control,wRef,get_value = aMaxFit

aMinFit  = double(aMinFit[0])
aMaxFit = double(aMaxFit[0])
if (aMinFit lt min(*atData.activity)) or (aMinFit ge aMax) then aMinFit = min(*atData.activity)
if (aMaxFit gt max(*atData.activity)) or (aMaxFit le aMin) then aMaxFit = max(*atData.activity)
self.actLimsFit = [aMinFit,aMaxFit]


;set value in boxes
wRef = widget_info(self.TLB, find_by_uname = 'ActLow')
widget_control,wRef,set_value = string(aMin,format = '(g-12.3)'),sensitive = 1
wRef = widget_info(self.TLB, find_by_uname = 'ActHigh')
widget_control,wRef,set_value = string(aMax,format = '(g-12.3)' ), sensitive = 1

wRef = widget_info(self.TLB, find_by_uname = 'FitLow')
widget_control,wRef,set_value = string(aMinFit,format = '(g-12.3)'),sensitive = 1
wRef = widget_info(self.TLB, find_by_uname = 'FitHigh')
widget_control,wRef,set_value = string(aMaxFit,format = '(g-12.3)' ), sensitive = 1

;plot a box to indicate analysis limits
tMin = INTERPOL(*atData.time,*atData.activity, aMin)    ;get time for min activity
tMax = INTERPOL(*atData.time,*atData.activity, aMax)    ;get time for max activity 

wRef = widget_info(self.TLB, find_by_uname = 'plotWin')
widget_control,wRef,get_value = oRef
oRef.select

if obj_valid(self.plotObj[1]) then (self.plotObj[1]).delete 
self.plotObj[1] = polygon([[tMin,tMin,tMax,tMax],[aMax,aMin,aMin,aMax]],/data,/current,target = self.plotObj,color = 'red',thick = 2,fill_background = 0)


;get the indices of max and min activity
;analysis
ix = where(*atData.activity le aMax) & maxIx = min(ix)
ix = where(*atData.activity ge aMin) & minIx = max(ix)
self.iLims = [maxIx,minIx]

;fitting
ix = where(*atData.activity le aMaxFit) & maxIx = min(ix)
ix = where(*atData.activity ge aMinFit) & minIx = max(ix)
self.iLimsFit = [maxIx,minIx]


;print,self.iLims 


end

pro LinearityAnalysis::RetrieveWidgetSettings

  ;this routine gets some widget settings and saves them in the object
  ;note that the analysis limits are already saved in object by the SetActLims procedure

  ;get the half life for the selected nuclide
  wRef      = widget_info(self.tlb,find_by_uname = 'SelectNuclideDlist')
  nuclideIx = widget_info(wRef,/droplist_select)
  halfLife = (*self.halflifelist)[nuclideIx]
  self.nuclideIx = nuclideIx ;save in object

  ;get the background corection settings
  wRef = widget_info(self.tlb,find_by_uname = 'BkgEstDlist')
  self.bkgCorr[0] = widget_info(wRef,/droplist_select)

  wRef = widget_info(self.tlb,find_by_uname = 'BkgModelDlist')
  self.bkgCorr[1] = widget_info(wRef,/droplist_select)
  
  ;get optional averageing settings
  wRef      = widget_info(self.tlb,find_by_uname = 'DataAverageTextbox')
  widget_control,wRef,get_value = avgWin
  self.avg[0] = avgWin

end

pro LinearityAnalysis::AnalyzeData

;unpack data object
atData = self.atData

self.devAct = dblarr(2,2) ;make sure to set to zero before starting
self.fitRes = dblarr(2)
self.nuclideIx = -1
self.avg = dblarr(2)        

;retrieve widget settings and save in object
call_method,'RetrieveWidgetSettings',self

halfLife = (*self.halflifelist)[self.nuclideIx]

;extract data
aData = (*atData.activity)[self.iLims[0]:self.iLims[1]]
tData = (*atData.time)[self.iLims[0]:self.iLims[1]]

aDataFit = (*atData.activity)[self.iLimsFit[0]:self.iLimsFit[1]]
tDataFit = (*atData.time)[self.iLimsFit[0]:self.iLimsFit[1]]

;optional background correction
if self.bkgCorr[0] gt 0 then begin

  ;perform a non-linear fit to data to account for background 
  
  case self.bkgCorr[1] of
    0:begin
        ;model = ;a0*exp(-a1*t)+a2, where a2 is the background term
        
        parinfo = replicate({value:0.D, fixed:0, limited:[0,0], limits:[0.D,0]}, 3)

        parInfo[1].fixed   = 1             ;fix halflife of nuclide
        parInfo[2].limited = 1             ;limit the background a0 
        parInfo[2].limits  = [0,5d-3]      ;to maximum 5 MBq

        parInfo[*].value = [aDataFit[0],alog(2)/halfLife,1d-4] ;initial guess
        weights = 1.0/*self.act                                ;poisson weighting
        
        parms = MPFITFUN('LinearityAnalysisMonoExp', *self.time, *self.act, ERR, coeff, weights = weights, parInfo = parInfo)
        self.bkgEstimate = [parms[2],0]
        print,'Bkg (MBq): ',self.bkgEstimate[0] * 1000
        bkgCorrArr = replicate(self.bkgEstimate[0],n_elements(tData))
        
      end
      
    1:begin
      ;model = a0*exp(-a1*t) + a2*exp(-a3*t)
       
       parinfo = replicate({value:0.D, fixed:0, limited:[0,0], limits:[0.D,0]}, 4)
       
       parInfo[1].fixed   = 1              ;fix halflife of nuclide
       parInfo[2].limited = [1,1]          ;limit the background a0 
       parInfo[2].limits  = [0,5d-3]       ;to maximum 5 MBq
       parInfo[3].limited = [1,1]          ;limit the background halflife 
       parInfo[3].limits  = [0,alog(2)/10] ;to minimum 10h 
       
       parInfo[*].value = [aDataFit[0],alog(2)/halfLife,1d-4,alog(2)/24] ;initial guess
             
       weights = 1.0/*self.act                        ;poisson weighting
       parms = MPFITFUN('LinearityAnalysisBiExp', *self.time, *self.act, ERR, coeff, weights = weights, parInfo = parInfo,ftol = 1d-15)

       self.bkgEstimate = parms[2:3]        
       print,'Bkg (MBq,h): ',self.bkgEstimate[0] * 1000,alog(2)/self.bkgEstimate[1]
       bkgCorrArr = self.bkgEstimate[0]*exp(-self.bkgEstimate[1]*tData)
      end
  endcase

endif else begin
  self.bkgEstimate = [0,0]
  bkgCorrArr  = dblarr(n_elements(tData))  ;no background correction
endelse

;generate expected reading (used only for plotting)
*self.actExp = (aData[0]-bkgCorrArr[0])*exp(-alog(2)/halfLife*tData) + bkgCorrArr

;over the selected interval, generate activity readings corrected for decay and optionally background
corrAct = (aData - bkgCorrArr) * exp(alog(2)/halfLife*tData)


if self.avg[0] gt 0 then begin
  
  nAvgPoints = round((self.avg[0]/60d)/self.deltaT[1])  ;use mean delta t
  self.avg[1] = nAvgPoints
  
 if nAvgPoints gt 1 then begin
  
    corrAct = smooth(corrAct,nAvgPoints)
    
    ;edge values needs to be excluded, or these noise points will determine the results
    nExl = fix(nAvgPoints/2.+.5)+1
    
    corrAct      = corrAct[nExl:-nExl]
    *self.actExp = (*self.actExp)[nExl:-nExl]
  
 endif
  
endif

;****************************************************************************
dev = (corrAct - mean(corrAct))/mean(corrAct) * 100  ;dev in percent
;****************************************************************************

;calculate activity ranges with deviation less than specified. Where to start the search? 
;perhaps centroid of all points where dev<1%

ix = where(dev lt 1)
startIx = fix(mean(ix)+.5)
;print,startIx,(*self.actExp)[startIx]

for i = 0,1 do begin  ;1 and 5% dev

  for j = startIx,n_elements(*self.actExp)-1 do begin   
    if abs(dev[j]) gt self.devLevels[i] then begin
      ;stop
      self.devAct[i,0] = (*self.actExp)[j]
      break
    endif
  endfor
  
  if self.devAct[i,0] eq 0 then self.devAct[i,0] = (*self.actExp)[-1]
  
  for j = startIx,0,-1 do begin
    if abs(dev[j]) gt self.devLevels[i] then begin
      ;stop
      self.devAct[i,1] = (*self.actExp)[j]
      break
    endif
  endfor

  if self.devAct[i,1] eq 0 then self.devAct[i,1] = (*self.actExp)[0]
  
endfor

print,self.devAct
self.linValue = max(abs(dev))                                 ;maximum deviation in percent
*self.devArr = dev

fitCoeff = regress(tDataFit,alog(aDataFit),const = const)   ;linear fit of log(activity, no background correction for the fit)
;print,-alog(2)/fitCoeff[0],fitCoeff[0]
self.fitRes = [exp(const),-fitCoeff[0]]


call_method,'CreateReport',self

end

pro LinearityAnalysis::CreateReport
  
  atData = self.atData
  
  fontSize = 8
  w = window(dimensions = [600,800],title = 'Radionuclide ionization chamber linearity')
  
  aData = (*atData.activity)[self.iLims[0]:self.iLims[1]]
  tData = (*atData.time)[self.iLims[0]:self.iLims[1]]
  tMin  = min(tData,max = tMax)
  aMin  = min(aData, max = aMax)

  str = call_method('BuildResultStringArr',self,aMin,aMax,tMin,tMax)  ;get the string array to print in report
  fontSize = 8
  t  = text(0.53,0.58,str,/current,font_size = fontSize)  ;print string in report
  
  ;scale y range in dev plot if needed 
  if self.linValue lt 5 then yrange = [-10,10] else yrange = [-max(abs(*self.devArr)),max(abs(*self.devArr))]   
  
  ;plot measured data
  p = plot(*atData.time,*atData.activity,/ylog,thick = 2,color = 'blue',xtitle = 'Elapsed time (hours)',ytitle = 'Activity ('+atData.activityUnits +')',/current,name = 'Measured',$
          layout = [1,2,1],margin = [0.15, 0.05, 0.5, 0.2],ytickformat = '(e9.1)',xtickfont_size = fontSize,ytickfont_size = fontSize)
  
  ;plot analysis range
  q = polygon([[tMin,tMin,tMax,tMax],[aMax,aMin,aMin,aMax]],/data,/current,target = p,color = 'red',thick = 2,fill_background = 0,linestyle = 2)
  r = plot([0],[0],color = 'red',thick = 2,/nodata,/overplot,name = 'Analysis limits',xtickfont_size = fontSize,ytickfont_size = fontSize,linestyle = 2) ;just to enable legend 

  ;plot the fitted line
  aVec = call_method('GenerateFitCurve', self, tVec = tVec)
  s    = plot(tVec,aVec,color = 'cyan',thick = 3,/overplot,name = 'Fit')
  
  ;set legend target
  target = [p,q,r,s]
  
  ;optionally plot the estimated background 
  if self.bkgEstimate[0] gt 0 then begin
    
    if self.bkgEstimate[1] gt 0 then bkgCurve = self.bkgEstimate[0]*exp(-self.bkgEstimate[1]*(*self.Time)) else bkgCurve = replicate(self.bkgEstimate[0],n_elements(*self.Time))
    bkgPlotObj = plot(*self.time,bkgCurve,/overplot,thick = 2,name = 'Background est.')
    target = [target,bkgPlotObj]
  endif
  ;include legend
  l = legend(target = target,position = [0.77,0.58],font_size = 7,sample_width = 0.15,linestyle = 6,shadow=0,transparency = 100)

  ;linearity plot
  qq = plot(*self.actExp,*self.devArr,sym = '.',xtitle = 'Activity ('+atData.activityUnits +')',ytitle = 'Deviation (%)',layout = [1,2,2],/current, margin = [0.12, 0.2, 0.1, 0.1],$
          yrange = yrange,/xlog,sym_size = 1.8,xtickfont_size = fontSize,ytickfont_size = fontSize)

  ;plot limits of linear range calculation
  x  = [(*self.actExp)[0],(*self.actExp)[-1]]
  for i = 0,1 do begin
    for j = 0,1 do begin
      qq = plot(x,replicate(self.devLevels[i],2)*(-1)^j,color = 'red',thick = 1,/overplot,/xlog,xstyle = 1)
    endfor
  endfor

  resStr = 'Maximum deviation (%): ' + string(self.linValue,format = '(f-4.1)')
  t = text(0.3,0.4,resStr,/current,font_size = 10,font_style = 1)

  str = ['Program version: ' + self.programVersion]
  str = [str,'Author: Gustav Brolin, Radiation physics, SUS']
  t   = text(0.06,0.03,str,font_size = 6)

  ;tt = text(0.5,0.87,'Estimated halflife: ' + number_formatter(p[1]*60D,decimal = 2)+' min',/overplot)
  ;q = plot(replicate(tRef,2),replicate(aRef,2),/overplot,sym = 'o',name = 'Referenspunkt',sym_thick = 2,sym_size = 0.8,thick = 2,linestyle = 6)
  ;s = plot(t,Ac,/overplot,name = 'Calculated activity'+' HL = '+number_formatter(HL,decimal = 2)+' min',color = 'red',thick = 2)
  
  ;l = legend(target = [pl,q,s],position = [0.85,0.866])
  ;
  ;
  ;qq = plot(Ac,dev,sym = '.',xtitle = ' Calculated activity (GBq)',ytitle = 'Deviation (%)',layout = [1,2,2],/current, margin = [0.12, 0.2, 0.1, 0.08], xrange = [aMin,aMax],yrange = [-5,5],/xlog,sym_size = 1.5)
  ;tt  = text(450,300,['Maximum deviation = '+number_formatter(Lin,decimal = 1)+' %','range: '+number_formatter(min(A[q1:q2]),decimal = 3)+'-'+number_formatter(max(A[q1:q2]),decimal = 2)+' GBq'],/current,/device)
  ;

end

function LinearityAnalysis::BuildResultStringArr,aMin,aMax,tMin,tMax

  atData = self.atData

  wRef = widget_info(self.TLB, find_by_uname = 'CommentTextbox')
  widget_control,wRef,get_value = comment

  ;data to include in report
  str = ['Data file: ' + atData.fileName]
  str = [str,'Measurement started: ' + atData.measStartEndString[0]]
  str = [str,'Measurement ended: ' + atData.measStartEndString[1]]
  str = [str,'Measurement length (h): ' + string(max(*atData.time),format = '(f-8.1)')]
  str = [str,'N. samples: ' + strcompress(n_elements(*atData.activity),/rem)]
  str = [str,'']
  str = [str,'Analysis range (' + atData.activityUnits +'): ' + strtrim(string(aMin,format = '(f-10.3)')) + ' - ' + strtrim(string(aMax,format = '(f-10.3)'))]
  str = [str,'N. samples in calc.: ' + strcompress(n_elements(*self.devArr))]
  str = [str,'Nuclide, assumed half life (h): ' + (*self.nuclideList)[self.nuclideIx] + ',' + string((*self.halfLifeList)[self.nuclideIx],format = '(f-7.3)' )]

  str = [str,'']

  if self.bkgCorr[0] gt 0 then strBkg = string(self.bkgEstimate[0],format = '(e-10.3)') else strBkg = 'None'  
  str = [str,'Estimated bkg ('+atData.activityUnits+'): ' + strBkg] 

  str = [str,'Decay constant fit (h$^{-1}$): ' + string(abs(((self.fitRes))[1]),format = '(f-7.4)')]
  str = [str,'Half-life fit (h): ' + string(alog(2)/(self.fitRes)[1], format = '(f-7.3)')]
  str = [str,'']
  str = [str,'Linear range ('+string(self.devLevels[0],format = '(f-3.1)') + '%): ' + strtrim(string(self.devAct[0,0],format ='(f-10.3)')) + ' - ' + strtrim(string(self.devAct[0,1],format ='(f-10.3)'))]
  str = [str,'Linear range ('+string(self.devLevels[1],format = '(f-3.1)') + '%): ' + strtrim(string(self.devAct[1,0],format ='(f-10.3)')) + ' - ' + strtrim(string(self.devAct[1,1],format ='(f-10.3)'))]

  if (self.avg)[0] gt 0 then str = [str,'Averaging filter (min,nPoints): ' + string((self.avg)[0],format = '(f-7.2)') + ', ' + string((self.avg)[1],format = '(I-7)')] else $
    str = [str,'']

  str = [str,'Maximum dev (%): ' + string(self.linValue,format = '(f-8.1)')]
  str = [str,'']
  str = [str,'Comment: ' + comment]
  str = [str,'']
  str = [str,'Date: ' + strcompress(today(),/rem)]
  str = [str,'']
  str = [str,'Sign: ........................................................']

  return, str

end

pro LinearityAnalysisEventHandler,event

print,event
widget_control,event.id,get_uvalue = cmd
uName = widget_info(event.id,/uname)

if n_elements(cmd) gt 0 then begin
  
  call_method,cmd.method,cmd.object
  
endif

end

pro LinearityAnalysis::Abouts, calledFromInit = calledFromInit

;change log 
;1.0    - inital version with widget, created on the basis of a script used for many years
;1.1    - measurement start and end is saved and printed in the report, for both formats of data file
;1.2    - added support for fidelis log file.
;       - added support to change radionuclide (i.e. half-life)
;       - added reports of estimated decay constant, and linearity within 1% and 5% according to NPL GPG 93 
;       - added support for a time window for averaging of data, to minimize statistical influence
;1.3    - added support to compensate for background signal (e.g. from long-lived radionuclides in the sample)
;       - data model is mono-exponential + constant bkg OR mono-exponential background
;       - changed widget font and decimals in report
    

self.programVersion = '1.3'

if ~keyword_set(CalledFromInit) then begin
    str = ['Version ' + self.programVersion,'Author: Gustav Brolin, Radiation Physics, Skane University Hospital']
    str = [str, 'This software is provided "as is" without warranty of any kind, either express or implied, including, but not limited to, ' $
                + 'the implied warranties of fitness for a purpose, or the warranty of non-infringement. Without limiting the foregoing, the Author makes no warranty that: ']
    str = [str, 'the software will meet your requirements, ']
    str = [str, 'the results that may be obtained from the use of the software will be effective, accurate or reliable, ']
    str = [str, 'the quality of the software will meet your expectations.']
    str = [str, 'In no event shall the Author be liable to you or any third parties for any special, punitive, incidental, indirect or consequential damages of any kind, or any damages whatsoever, including, without limitation, those resulting from loss of use, data or profits, whether or not the Authour has been advised of the possibility of such damages, and on any theory of liability, arising out of or in connection with the use of this software.']

    a = dialog_message(str,/information)
  endif

end

function LinearityAnalysis::init,widget = widget

self.formatList   = ptr_new(/allocate_heap)
self.actExp       = ptr_new(/allocate_heap)
self.devArr       = ptr_new(/allocate_heap)
self.nuclideList  = ptr_new(/allocate_heap)
self.halflifelist = ptr_new(/allocate_heap) 


call_method, 'Abouts',self,/calledFromInit ;get program verion

*self.formatList = ['Comecer (Cyklotron)','Capintec (MP)','Fidelis','Comecer QMM/IBC']

*self.nuclideList   = ['F-18','Tc-99m']
*self.HalfLifeList  = [109.723/60d,6.007d] ; [h]F-18 halflife from Applied Radiation and Isotopes 68 (2010) 1561–1565), good agreement with LHNB and NPL recommended values
                                           ; Tc-99m halflife from LHNB
self.devLevels = [1d,5d]                   ; deviation levels for reporting of linearity

;realize widget
if keyword_set(widget) then begin
  call_method,'WidgetDefine',self,*self.formatList,*self.nuclideList
  xmanager,'void',self.TLB,event_handler = 'LinearityAnalysisEventHandler',/no_block
endif

return,1
end 

pro LinearityAnalysis__define

void = {LinearityAnalysis                       ,$
        programVersion:''                       ,$
        TLB: 0L                                 ,$
        atData:obj_new()                        ,$
        formatList:ptr_new()                    ,$
        plotObj:objarr(2)                       ,$
        halfLifeList:ptr_new()                  ,$   ;halflives for selectable nuclides
        nuclideList:ptr_new()                   ,$   ;selectable nuclides
        nuclideIx:0                             ,$   ;index of selected nuclide
        actExp: ptr_new()                       ,$   ;expected activity in GBq or pA, used for deviation plotting
        avg:dblarr(2)                           ,$   ;time averaging [time(min),nPoints)
        actLims:dblarr(2)                       ,$   ;activity limits [min,max] for linearity calculation
        actLimsFit:dblarr(2)                    ,$   ;activity limits for linear fit to logarithm of data
        devLevels:dblarr(2)                     ,$   ;deviation levels for reporting of linearity, set in ::init
        devAct:dblarr(2,2)                      ,$   ;ranges of activity where deviations are less than specified deviation levels (%)
        iLims:lonarr(2)                         ,$   ;array indices corresponding to [max,min] activity
        iLimsFit:lonarr(2)                      ,$   ;array inidices for limits of fit
        fitRes:dblarr(2)                        ,$   ;parameters of linear fit to log data (following GPG93)
        bkgCorr:bytarr(2)                       ,$   ;[off/on,const/exp bkg]
        bkgEstimate:dblarr(2)                   ,$   ;background estimate [A0 (GBq/pA),halflife (h)], half-life = 0 for constant background
        devArr:ptr_new()                        ,$   ;deviations (%) of expected reading, cropped to analysis limits iLims
        linValue:0d                              $   ;calculated linearity value
       }

end