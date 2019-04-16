pro LinearityAnalysis::WidgetDefine,formatList

labelW = 128

self.TLB = widget_base(title = 'Linearity analysis',column = 2,uvalue = self)

  base0 = widget_base(self.TLB,/column)
    base = widget_base(base0,/row)
      label = widget_label(base,value = '1. Select file format: ',xsize = labelW)
      dList  = widget_droplist(base,value = formatList,uname = 'SelectFormatDlist')
    base   = widget_base(base0,/row)
      label = widget_label(base,value = '2. Read data file: ',xsize = labelW)
      button = widget_button(base,value = 'Browse...',uvalue={object:self,method:'ReadData'}) 
    base = widget_base(base0,/row)
      label = widget_label(base,value = '3. Comment for report: ',xsize = labelW)
      txt  = widget_text(base,editable=1,uname = 'CommentTextbox')
    base = widget_base(base0,/row)
      label = widget_label(base,value = '4. Set activity limits (GBq): ',xsize = labelW)
      base1 = widget_base(base,/column)
        label = widget_label(base1,value = 'Low:')
        txt   = widget_text(base1,editable = 1,uvalue={object:self,method:'SetActLims'},uname = 'ActLow',sensitive = 0,xsize = 8)
      base1 = widget_base(base,/column)
        label = widget_label(base1,value = 'High:')
        txt   = widget_text(base1,editable = 1,uvalue={object:self,method:'SetActLims'},uname = 'ActHigh',sensitive = 0,xsize = 8)  
    
    base = widget_base(base0,/row)
      label = widget_label(base,value = '5. Run analysis: ',xsize = labelW)
      button  = widget_button(base,value='Run...',uvalue={object:self,method:'AnalyzeData'},sensitive = 0,uname = 'ButtonRun')
  
base0 = widget_base(self.TLB,/row)
  wwindow = widget_window(base0,uname = 'plotWin')
  
widget_control,self.TLB,/realize

end