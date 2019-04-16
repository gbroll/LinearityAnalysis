function Today

;get the system time
sTime = systime(/julian)
jul2greg,sTime,month,day,year

return,timestamp(year = year,month = month,day = day)


end