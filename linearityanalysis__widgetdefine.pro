pro LinearityAnalysis::WidgetDefine,formatList,nuclideList

labelW = 256
font = 'Courier New*14'
self.TLB = widget_base(title = 'Linearity analysis',column = 2,uvalue = self)

  base0 = widget_base(self.TLB,/column)
    base = widget_base(base0,/row)
      label = widget_label(base,value = '1. Select file format:              ',xsize = labelW,font = font)
      dList  = widget_droplist(base,value = formatList,uname = 'SelectFormatDlist',font = font)
    base   = widget_base(base0,/row)
      label = widget_label(base,value = '2. Read data file:                  ',xsize = labelW,font = font)
      button = widget_button(base,value = 'Browse...',uvalue={object:self,method:'ReadData'}, font = font) 
    base = widget_base(base0,/row)
      label = widget_label(base,value = '3. Specify radionuclide:            ',xsize = labelW,font = font)
      dList  = widget_droplist(base,value = nuclideList,uname = 'SelectNuclideDlist',font = font,sensitive = 0)
    base = widget_base(base0,/row)
      label = widget_label(base,value = '4. Comment for report:              ',xsize = labelW,font = font)
      txt  = widget_text(base,editable=1,uname = 'CommentTextbox',font = font)
    base = widget_base(base0,/row)
      label = widget_label(base,value = '5. Set analysis limits (GBq or pA): ',xsize = labelW,font = font)
      base1 = widget_base(base,/column)
        label = widget_label(base1,value = 'Low:',font = font)
        txt   = widget_text(base1,editable = 1,uvalue={object:self,method:'SetActLims'},uname = 'ActLow',sensitive = 0,xsize = 8,font = font)
      base1 = widget_base(base,/column)
        label = widget_label(base1,value = 'High:',font = font)
        txt   = widget_text(base1,editable = 1,uvalue={object:self,method:'SetActLims'},uname = 'ActHigh',sensitive = 0,xsize = 8,font = font)        
    base = widget_base(base0,/row)
        label = widget_label(base,value = '6. Set limits of fit (GBq or pA): ',xsize = labelW,font = font)
        base1 = widget_base(base,/column)
        label = widget_label(base1,value = 'Low:',font = font)
        txt   = widget_text(base1,editable = 1,uvalue={object:self,method:'SetActLims'},uname = 'FitLow',sensitive = 0,xsize = 8,font = font)
        base1 = widget_base(base,/column)
        label = widget_label(base1,value = 'High:',font = font)
        txt   = widget_text(base1,editable = 1,uvalue={object:self,method:'SetActLims'},uname = 'FitHigh',sensitive = 0,xsize = 8,font = font)  
    base = widget_base(base0,/row)
        label = widget_label(base,value = '7. Set data averaging (min):              ',xsize = labelW,font = font)
        txt  = widget_text(base,editable=1,uname = 'DataAverageTextbox',value = '0',sensitive = 0,font = font)
    base = widget_base(base0,/row)
        label = widget_label(base,value = '8. Background est. and corr.:             ',xsize = labelW,font = font)
        dList  = widget_droplist(base,value = ['Off','On'],uname = 'BkgEstDlist',sensitive = 0,font = font)
    base = widget_base(base0,/row)
        label = widget_label(base,value = 'Background model.:                        ',xsize = labelW,font = font)
        dList  = widget_droplist(base,value = ['Constant','Exponential'],uname = 'BkgModelDlist',sensitive = 0,font = font)
      
    
    base = widget_base(base0,/row)
      label = widget_label(base,value = '8. Run analysis: ',xsize = labelW,font = font)
      button  = widget_button(base,value='Run...',uvalue={object:self,method:'AnalyzeData'},sensitive = 0,uname = 'ButtonRun',font = font)
  
base0 = widget_base(self.TLB,/row)
  wwindow = widget_window(base0,uname = 'plotWin')
  
widget_control,self.TLB,/realize

end