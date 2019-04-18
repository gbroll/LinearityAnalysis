pro LinearityAnalysis::ReadData

str = ''
t = []  ;time in seconds
A = []  ;Activity in Becquerel

self.fileName = dialog_pickfile(path = self.path, get_path = path,filter = ['*.txt','*.csv'])
;check that file exist and open it for reading
if ~file_test(self.fileName,/read,/regular) then begin
  a = dialog_message('The selected file is not readable.')
  return
endif

self.path = path
print,self.fileName
openr,lun,self.fileName,/get_lun


;get selected data format
wRef   = widget_info(self.tlb,find_by_uname = 'SelectFormatDlist')
format = widget_info(wRef,/droplist_select)
print,format 

rowCounter = 0
case format of
 
  0:begin  ;Comecer
    while ~EOF(lun) do begin
      readf,lun,str
      strs = strsplit(str,';',/extract) ;csv data
      
      if rowCounter eq 0 then begin
        splits = strsplit(strs[0],'_',/extract)
        self.measStartEnd[0] = splits[0] + ' ' + strjoin(strsplit(splits[1],'-',/extract),':')
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
      rowCounter++
    endwhile
    splits = strsplit(strs[0],'_',/extract) ;save last row, i.e. measurement end
    self.measStartEnd[1] = splits[0] + ' ' + strjoin(strsplit(splits[1],'-',/extract),':')
    free_lun,lun
    t = t/(3600D)  ;time in hours
  end
  
  1:begin ;Capintec
    readf,lun,str   
    strs = strsplit(str,' ' ,/extract)
    if n_elements(strsplit(strs[1],'-',/extract)) eq 3 then dateDelim = '-' else dateDelim = '/' 
    
    while ~EOF(lun) do begin

      readf,lun,str
      if strmid(str,0,1) eq '#' then continue
      

      
      
      strs = strsplit(str,';',/extract)
      date = strsplit(strs[0],dateDelim,/extract)
      if dateDelim eq '/' then begin
        year  = date[2]
        month = date[1]
        day   = date[0]
      endif else begin
        year  = date[0]
        month = date[1]
        day   = date[2]
      endelse
      
      time = strsplit(strs[1],':',/extract)
          
      if rowCounter eq 0 then self.measStartEnd[0] = strjoin([year,month,day],'-') + ' ' + strjoin([time[0],time[1],time[2]],':')
      
      dateTime = greg2jul(month,day,year,time[0],time[1],time[2])
      
      
      t = [t,dateTime]
      unit = strs[3]
      case strupcase(unit) of
        'BQ' : mltpl = 1d-9
        'KBQ': mltpl = 1d-6
        'MBQ': mltpl = 0.001d
        'GBQ': mltpl = 1d
      endcase
      ;replace , with . if present to get correct decimal separator
      aString = strjoin(strsplit(strs[2],',',/extract),'.')
      A = [A,double(aString)*mltpl]   ;activity in GBq
      rowCounter++
    endwhile
    free_lun,lun
    
    
   self.measStartEnd[1] = strjoin([year,month,day],'-') + ' ' + strjoin([time[0],time[1],time[2]],':') ;save datetime for first and last datapoint
    t = (t-t[0])*24d   ;set t = 0 at the first measurement and adjust time to hours
  end

  else: begin
    a = dialog_message('Unknown format: ' + strcompress(format,/rem),/error)
    return
  end

endcase


free_lun,lun

*self.time = t
*self.act  = A

;plot the data
wRef = widget_info(self.TLB, find_by_uname = 'plotWin')
widget_control,wRef,get_value = oRef
oRef.select
oRef.erase
self.plotObj[0] = plot(t,A,/ylog,/current, xtitle = 'Elapsed time(h)', ytitle = 'Measured activity (GBq)')

;set activity limits to max and min
call_method, 'SetActLims',self,init = 1

;sensitizy run button
wRef = widget_info(self.TLB, find_by_uname = 'ButtonRun')
widget_control,wRef,sensitive = 1

end

pro LinearityAnalysis::SetActLims, init = init

if keyword_set(init) then begin
  
  wRef = widget_info(self.TLB, find_by_uname = 'ActLow')
  widget_control,wRef,set_value = string(min(*self.Act),format = '(g-12.3)'),sensitive = 1
  wRef = widget_info(self.TLB, find_by_uname = 'ActHigh')
  widget_control,wRef,set_value = string(max(*self.Act),format = '(g-12.3)' ), sensitive = 1
  
  self.actLims = [min(*self.act),max(*self.act)]
  
endif

;get value in boxes
wRef = widget_info(self.TLB, find_by_uname = 'ActLow')
widget_control,wRef,get_value = aMin
wRef = widget_info(self.TLB, find_by_uname = 'ActHigh')
widget_control,wRef,get_value = aMax

aMin  = double(aMin[0])
aMax = double(aMax[0]) 
if (aMin lt min(*self.Act)) or (aMin ge aMax) then aMin = min(*self.act)
if (aMax gt max(*self.Act)) or (aMax le aMin) then aMax = max(*self.act)
self.actLims = [aMin,aMax]

;set value in boxes
wRef = widget_info(self.TLB, find_by_uname = 'ActLow')
widget_control,wRef,set_value = string(aMin,format = '(g-12.3)'),sensitive = 1
wRef = widget_info(self.TLB, find_by_uname = 'ActHigh')
widget_control,wRef,set_value = string(aMax,format = '(g-12.3)' ), sensitive = 1

;plot a box to indicate analysis limits
tMin = INTERPOL(*self.time,*self.act, aMin)    ;get time for min activity
tMax = INTERPOL(*self.time,*self.act, aMax)    ;get time for max activity 

wRef = widget_info(self.TLB, find_by_uname = 'plotWin')
widget_control,wRef,get_value = oRef
oRef.select

if obj_valid(self.plotObj[1]) then (self.plotObj[1]).delete 
self.plotObj[1] = polygon([[tMin,tMin,tMax,tMax],[aMax,aMin,aMin,aMax]],/data,/current,target = self.plotObj,color = 'red',thick = 2,fill_background = 0)


;get the indices of max and min activity
ix = where(*self.act le aMax) & maxIx = min(ix)
ix = where(*self.act ge aMin) & minIx = max(ix)

self.iLims = [maxIx,minIx]
print,self.iLims 


end

pro LinearityAnalysis::AnalyzeData

aData = (*self.act)[self.iLims[0]:self.iLims[1]]
tData = (*self.time)[self.iLims[0]:self.iLims[1]]

;p = plot(tData,aData,ylog=1)

dcyCorrAct = aData * exp(alog(2)/(self.halflife/60D)*(tData))

dev = (dcyCorrAct - mean(dcyCorrAct))/mean(dcyCorrAct) * 100  ;dev in percent
print,mean(dev)

self.linValue = max(abs(dev))                                 ;maximum deviation in percent
*self.devArr = dev

;p = plot(aData,dev,xlog = 1)



call_method,'CreateReport',self

end

pro LinearityAnalysis::CreateReport

fontSize = 8

;get comment
wRef = widget_info(self.TLB, find_by_uname = 'CommentTextbox')
widget_control,wRef,get_value = comment

aData = (*self.act)[self.iLims[0]:self.iLims[1]]
tData = (*self.time)[self.iLims[0]:self.iLims[1]]
tMin  = min(tData,max = tMax)
aMin  = min(aData, max = aMax)

;data to include in report
str = ['Program version: ' + self.programVersion]
str = [str,'Data file: ' + file_basename(self.fileName)]
str = [str,'Measurement started: ' + self.measStartEnd[0]]
str = [str,'Measurement ended: ' + self.measStartEnd[1]]
str = [str,'Measurement length (h): ' + string(max(*self.time),format = '(f-8.1)')]
str = [str,'N. samples: ' + strcompress(n_elements(*self.act),/rem)]
str = [str,'']
str = [str,''] 
str = [str,'Analysis activity range (GBq): ' + string(aMin,format = '(f-7.4)') + ' - ' + string(aMax,format = '(f-6.1)')]
str = [str,'N. samples in calc.: ' + strcompress(n_elements(aData))]
str = [str,'Calculated linearity (%): ' + string(self.linValue,format = '(f-8.1)')]
str = [str,'']
str = [str,'']
str = [str,'Comment: ' + comment]
str = [str,'']
str = [str,'Date: ' + strcompress(today(),/rem)]
str = [str,'']
str = [str,'Sign: ........................................................']

if self.linValue lt 5 then yrange = [-5,5] else yrange = []   ;scale y range in dev plot if needed

w = window(dimensions = [600,800],title = 'Radionuclide ionization chamber linearity')
t  = text(0.53,0.66,str,/current,font_size = fontSize)



p = plot(*self.Time,*self.act,/ylog,thick = 2,color = 'blue',xtitle = 'Elapsed time (hours)',ytitle = 'Activity (GBq)',/current,name = 'Measured activity',$
          layout = [1,2,1],margin = [0.15, 0.05, 0.5, 0.2],ytickformat = '(e9.1)',xtickfont_size = fontSize,ytickfont_size = fontSize)
q = polygon([[tMin,tMin,tMax,tMax],[aMax,aMin,aMin,aMax]],/data,/current,target = p,color = 'red',thick = 2,fill_background = 0,linestyle = 2)
r = plot([0],[0],color = 'red',thick = 2,/nodata,/overplot,name = 'Analysis limits',xtickfont_size = fontSize,ytickfont_size = fontSize,linestyle = 2)

l = legend(target = [p,q,r],position = [0.8,0.58],font_size = fontSize,sample_width = 0.15,linestyle = 6,shadow=0)

qq = plot(aData,*self.devArr,sym = '.',xtitle = 'Activity (GBq)',ytitle = 'Deviation (%)',layout = [1,2,2],/current, margin = [0.12, 0.2, 0.1, 0.1],$
          yrange = yrange,/xlog,sym_size = 1.8,xtickfont_size = fontSize,ytickfont_size = fontSize)

resStr = 'Calculated Linearity (%): ' + string(self.linValue,format = '(f-4.1)')
t = text(0.3,0.4,resStr,/current,font_size = 10,font_style = 1)


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
;1.0    inital version with widget, created on the basis of a script used for many years
;1.1    measurement start and end is saved and printed in the report, for both formats of data file

self.programVersion = '1.1'

if ~keyword_set(CalledFromInit) then begin
    str = ['Version ' + self.programVersion,'Author: Gustav Brolin, Radiation Physics, Skane University Hospital']
    a = dialog_message(str,/information)
  endif

end

function LinearityAnalysis::init,widget = widget

self.formatList  = ptr_new(/allocate_heap)
self.time        = ptr_new(/allocate_heap)
self.act         = ptr_new(/allocate_heap)
self.devArr      = ptr_new(/allocate_heap)


call_method, 'Abouts',self,/calledFromInit ;get program verion

*self.formatList = ['Comecer','Capintec (MP)']
self.HalfLife = 109.723   ;F-18 halflife in minutes  (ref Applied Radiation and Isotopes 68 (2010) 1561–1565)


;realize widget
if keyword_set(widget) then begin
  call_method,'WidgetDefine',self,*self.formatList
  xmanager,'void',self.TLB,event_handler = 'LinearityAnalysisEventHandler',/no_block
endif

return,1
end 

pro LinearityAnalysis__define

void = {LinearityAnalysis                       ,$
        programVersion:''                       ,$
        TLB: 0L                                 ,$
        plotObj:objarr(2)                       ,$
        halfLife:0d                             ,$   ;halflife used to calculate analysis
        formatList:ptr_new()                    ,$
        fileName:''                             ,$   ;full path to data file
        path:''                                 ,$   ;path in which to look for data files
        measStartEnd:strarr(2)                  ,$   ;date and time for measurement start and end, as strings to be printed directly in the report
        time:ptr_new()                          ,$   ;time in hours from first data point
        act: ptr_new()                          ,$   ;measured activity in Gigabecquerel
        actLims:dblarr(2)                       ,$   ;activity limits [min,max] for linearity calculation
        iLims:lonarr(2)                         ,$   ;array indices corresponding to [max,min] activity
        devArr:ptr_new()                        ,$   ;deviations (%) of expected reading, cropped to analysis limits iLims
        linValue:0d                              $   ;calculated linearity value
       }

end