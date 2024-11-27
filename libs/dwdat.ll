;#################################################
;## DEALING WITH DATE AND TIME                  ##
;## (c) 2017 MICHAUD Yannick & KsyNET           ##
;## needs stdlib.ll                             ##
;#################################################

;INITIALIZATIONS
;#Quick Load
(needsLibraries '(LIB.IO LIB.Conv NIL.System))
(require 'libs/java '$java_)
(require 'libs/stdlib '$stdlib_)

(if (not (existsVariable 'timeZone)) (setq timeZone "Europe/Paris"))

(let dwdatNC needsClone)
# Need it because of 'WARNING: Illegal reflective access by NIL.NilObject (rsrc:./) to method sun.util.calendar.ZoneInfo.clone()'
(register 'needsClone nil)
(setq $dwdat_timeZone (TimeZone.getTimeZone timeZone))
(register 'needsClone dwdatNC)
(unlet dwdatNC)
(setq $dwdat_locale locale)

;# MAIN FUNCTIONS
(undef getToday)
(defun getToday ()
   (let c (Calendar.getInstance $dwdat_timeZone $dwdat_locale))
   c
)
(undef getDateFromTimestamp)
(defun getDateFromTimestamp (ts)
  (let td (getToday))
  (td.setTimeInMillis ts)
  td
)

(undef getDate)
(defun getDate (day month year h?0 m?0 s?0 ampm?0 millis?0)
   (let d (getToday))
   (d.set Calendar.DAY_OF_MONTH day)
   (d.set Calendar.MONTH (decr month))
   (if (== (length year) 2) (let year (+ year 2000)))
   (d.set Calendar.YEAR year)
   (let d (setTime d h m s ampm millis))
   (d.getTime)
   d
)
(undef setTime)
(defun setTime (cal h m s ampm?0 millis?0)
   (cal.set Calendar.HOUR_OF_DAY (+ h (* ampm 12)))
   (cal.set Calendar.HOUR h)
   (cal.set Calendar.MINUTE m)
   (cal.set Calendar.SECOND s)
   (cal.set Calendar.AM_PM ampm)
   (cal.set Calendar.MILLISECOND millis)
   cal
)

(undef formatDT)
(defun formatDT (cal format?"HH:mm:ss")
   (format (cal.getTime) format)
)
(undef addDays)
(defun addDays (cal amount)
   (let cal2 (cal.Clone))
   (cal2.add Calendar.DAY_OF_MONTH amount)
   cal2
)
(undef addWeeks)
(defun addWeeks (cal amount)
   (addDays cal (* amount 7))
)
(undef addMonths)
(defun addMonths (cal amount)
   (let cal2 (cal.Clone))
   (cal2.add Calendar.MONTH amount)
   cal2
)
(undef addYears)
(defun addYears (cal amount)
   (let cal2 (cal.Clone))
   (cal2.add Calendar.YEAR amount)
   cal2
)
(undef addMSeconds)
(defun addMSeconds (cal amount)
   (let cal2 (cal.Clone))
   (cal2.add Calendar.MILLISECOND amount)
   cal2
)
(undef addSeconds)
(defun addSeconds (cal amount)
   (let cal2 (cal.Clone))
   (cal2.add Calendar.SECOND amount)
   cal2
)
(undef addMinutes)
(defun addMinutes (cal amount)
   (let cal2 (cal.Clone))
   (cal2.add Calendar.MINUTE amount)
   cal2
)
(undef addHours)
(defun addHours (cal amount)
   (let cal2 (cal.Clone))
   (cal2.add Calendar.HOUR amount)
   cal2
)

(undef parseDate)
(defun parseDate (str)
   (let td (getToday))
   (td.setTimeInMillis ((td.getTime).parse str))
   td
)
(undef diffDate)
(defun diffDate (d1 d2 v?t)
   (let delta ((/ (abs (- (d1.getTimeInMillis) (d2.getTimeInMillis))) 1000).longValue))
   (let diff (Calendar.getInstance))
   (diff.setTimeInMillis (* delta 1000))
   (let diff (addYears diff -1970))
   (if v
      (let y (Int (conv delta second year)))
      (print y " year" (cond ((> y 1) "s") (t "")))
      (let m (Int (conv delta second month)))
      (print m " month" (cond ((> m 1) "s") (t "")))
      (let d (conv delta second day))
      (print d " day" (cond ((> d 1) "s") (t "")))
      (let h (conv delta second hour))
      (print h " hour" (cond ((> h 1) "s") (t "")))
      (print delta " second" (cond ((> delta 1) "s") (t "")))
   )
   diff
)
(undef $dwdat_calcDurations)
(defun $dwdat_calcDurations (nMillis)
   (let rez nil)
   (let seconds ((/ (setPrecision nMillis 3) 1000).longValue))
   (let tmp (divideAndRemainder seconds 31556952))
   (let rez (cons (car tmp) rez))
   (print (car tmp))
   (let tmp (divideAndRemainder (cadr tmp) 2629746))
   (let rez (cons (car tmp) rez))
   (print (car tmp))
   (let tmp (divideAndRemainder (cadr tmp) 86400))
   (let rez (cons (car tmp) rez))
   (print (car tmp))
   (let tmp (divideAndRemainder (cadr tmp) 3600))
   (let rez (cons (car tmp) rez))
   (print (car tmp))
   (let tmp (divideAndRemainder (cadr tmp) 60))
   (let rez (cons (car tmp) rez))
   (let rez (cons (cadr tmp) rez))
   (print (car tmp))
   (print (cadr tmp))
   rez
)
(undef $dwdat_getDays)
(defun $dwdat_getDays (nMillis)
   (let rez nil)

   (let seconds ((/ (setPrecision nMillis 3) 1000).longValue))
   (let tmp (divideAndRemainder seconds 86400))
   (let rez (cons (car tmp) rez))
   (print (car tmp))
   (let tmp (divideAndRemainder (cadr tmp) 3600))
   (let rez (cons (car tmp) rez))
   (print (car tmp))
   (let tmp (divideAndRemainder (cadr tmp) 60))
   (let rez (cons (car tmp) rez))
   (let rez (cons (cadr tmp) rez))
   (print (car tmp))
   (print (cadr tmp))
   rez
   
)
(undef getAge)
(defun getAge (dob)
   (let td (getToday))
   (td.getTime)
   (dob.getTime)
   (let delta (abs (- ((getToday).getTimeInMillis) (dob.getTimeInMillis))))
   (let d ($dwdat_calcDurations delta))
   (print d)
   (let seconds (car d))
   (let minutes (cadr d))
   (let hours (caddr d))
   (let days (cadddr d))
   (let months (c_r "caddddr" d))
   (let years (c_r "cadddddr" d))

   (+ years " year" (cond ((> years 1) "s ") (t " ")) months " month" (cond ((> days 1) "s ") (t " ")) days " day" (cond ((> days 1) "s ") (t " ")) hours " hour" (cond ((> hours 1) "s ") (t " ")) minutes " minute" (cond ((> minutes 1) "s ") (t " ")) seconds " second" (cond ((> seconds 1) "s") (t "")))
)





;(addAlias 'sys 'date 'getDate)

;# setting help
(setUserHelp "getDate" (+ "Usage: getDate DAY MONTH [19|20]YEAR [HOURS [MINUTES [SECONDS [AM/PM=0/1 [MILLISECONDS]]]]]" crlf tab "Returns a " Calendar " object set to what specified."))
(setUserHelp "getToday" (+ "Usage: getToday" crlf tab "Returns a " Calendar " object set to today at 00:00:00am."))
(setUserHelp "getFormatDT" (+ "Usage: formatDT" crlf tab "Returns a " Calendar " object set to today at 00:00:00am."))
(setUserHelp "diffDate" (+ "Usage: diffDate d1 d2 [V?t]" crlf tab "Returns a " Calendar " object set to the difference between d1 and d2. Prints also conversions if V is set." crlf tab "Note that the year is set to the difference i.e. fictitious year is set less 1970."))

;# END
(print "Date/time utilities loaded.")
(makeQuickLoad 'ldwdat scriptFile)
