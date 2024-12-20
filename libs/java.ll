;## JAVA SHORTCUTS FOR NIL
;## (c) 1997-2022 MICHAUD Yannick & KsyNET


;JAVA STANDARD
(setq Runtime 'java.lang.Runtime)
(setq System 'java.lang.System)
(setq File 'java.io.File)
(setq Charset 'java.nio.charset.Charset)
(setq String 'java.lang.String)
(setq JMath 'java.lang.Math)
(setq MathContext 'java.math.MathContext)
(setq StringReader 'java.io.StringReader)
(setq StringWriter 'java.io.StringWriter)
(setq Integer 'java.lang.Integer)
(setq Long 'java.lang.Long)
(setq Double 'java.lang.Double)
(setq BigInteger 'java.math.BigInteger)
(setq BigDecimal 'java.math.BigDecimal)
(setq Class 'java.lang.Class)
(setq Constructor 'java.lang.reflect.Constructor)
(setq Method 'java.lang.reflect.Method)
(setq Field 'java.lang.reflect.Field)
(setq ParsePosition 'java.text.ParsePosition)
(setq Date 'java.util.Date)
(setq Calendar 'java.util.Calendar)
(setq DateFormat 'java.text.DateFormat)
(setq SimpleDateFormat 'java.text.SimpleDateFormat)
(setq ParsePosition 'java.text.ParsePosition)
(setq Properties 'java.util.Properties)
(setq TimeZone 'java.util.TimeZone)
(setq Locale 'java.util.Locale)
(setq Pattern 'java.util.regex.Pattern)
(setq Matcher 'java.util.regex.Matcher)

;APACHE HTTP
(setq URL 'java.net.URL)
(setq URI 'java.net.URI)

;API TIME
(setq LocalDate 'java.time.LocalDate)
(setq LocalDateTime 'java.time.LocalDateTime)
(setq LocalTime 'java.time.LocalTime)
(setq Instant 'java.time.Instant)
(setq Duration 'java.time.Duration)
(setq DayOfWeek 'java.time.DayOfWeek)
(setq Month 'java.time.Month)
(setq MonthDay 'java.time.MonthDay)
(setq Year 'java.time.Year)
(setq YearMonth 'java.time.YearMonth)
(setq ChronoUnit 'java.time.temporal.ChronoUnit)
(setq TemporalAdjusters 'java.time.temporal.TemporalAdjusters)


;JSON API
(setq Json 'javax.json.Json)
(setq JsonParser 'javax.json.JsonParser)

;NIL STANDARD
(setq NilVM 'NIL.NilVM)
(setq Bean 'NIL.NilBean)
(setq Number 'NIL.NilNumber)
(setq Complex 'NIL.maths.Complex)
(setq BigDecimalMath 'ch.obermuhlner.math.big.BigDecimalMath)
(setq Object 'NIL.NilObject)
(setq LazyObject 'NIL.NilLObject)
(setq StreamWrapper 'NIL.streams.NilStreamWrapper)
(setq CharArray 'NIL.CharArray)
(setq IntArray 'NIL.IntArray)
(setq ByteArray 'NIL.ByteArray)
(setq Constants 'NIL.NilConstants)
(setq Library 'NIL.NilLibrary)
(setq List 'NIL.List)
(setq Rational 'NIL.maths.Rational)
(setq Matrix 'NIL.maths.Matrix)
(setq Vec3D 'NIL.maths.Vec3D)
(setq NilSystem 'NIL.System)
(setq ThreadObject 'NIL.NilThreadObject)
(setq JavaHandler 'NIL.JavaHandler)
(setq Erreur 'NIL.Erreur)
(setq Observer 'NIL.NilObjectObserver)
(setq Listener 'NIL.NilListener)
(setq Codecs 'NIL.Codecs)
(setq Stream 'NIL.streams.Stream)
(setq Cons 'NIL.streams.Cons)

;AWT GRAPHICS // NilPanel
(setq NilPanel 'NIL.gui.NilPanel)
(setq Graphics 'java.awt.Graphics)
(setq Graphics2D 'java.awt.Graphics2D)
(setq BasicStroke 'java.awt.BasicStroke)
(setq Component 'java.awt.Component)
(setq Rectangle 'java.awt.Rectangle)
(setq Point2D 'java.awt.geom.Point2D)
(setq Line2D 'java.awt.geom.Line2D)
(setq Window 'java.awt.Window)
(setq Container 'java.awt.Container)
(setq Image 'java.awt.Image)
(setq Label 'java.awt.Label)
(setq Panel 'java.awt.Panel)
(setq Frame 'java.awt.Frame)
(setq TextArea 'java.awt.TextArea)
(setq Menu 'java.awt.Menu)
(setq MenuBar 'java.awt.MenuBar)
(setq Font 'java.awt.Font)
(setq Color 'java.awt.Color)
(setq Dimension 'java.awt.Dimension)

;AWT LAYOUTS
(setq BorderLayout 'java.awt.BorderLayout)
(setq BoxLayout 'java.awt.BoxLayout)


(setq SystemTray 'java.awt.SystemTray)
(setq TrayIcon 'java.awt.TrayIcon)
(setq Desktop 'java.awt.Desktop)

;SWING GRAPHIC
(setq JFrame 'javax.swing.JFrame)
(setq JPanel 'javax.swing.JPanel)
(setq JMenuBar 'javax.swing.JMenuBar)
(setq JMenu 'javax.swing.JMenu)
(setq JMenuItem 'javax.swing.JMenuItem)
(setq JTextField 'javax.swing.JTextField)
(setq JButton 'javax.swing.JButton)
(setq ImageIcon 'javax.swing.ImageIcon)
(setq JOptionPane 'javax.swing.JOptionPane)

;EVENTS
(setq ActionListener 'java.awt.event.ActionListener)
(setq KeyListener 'java.awt.event.KeyListener)
(setq MouseListener 'jawa.awt.event.MouseListener)
(setq MouseAdapter 'jawa.awt.event.MouseAdapter)
(setq MouseWheelListener 'java.awt.event.MouseWheelListener)
(setq MouseMotionListener 'java.awt.event.MouseMotionListener)
(setq MouseMotionAdapter 'java.awt.event.MouseMotionAdapter)
(setq ActionEvent 'java.awt.event.ActionEvent)
(setq KeyEvent 'java.awt.event.KeyEvent)
(setq KeyAdapter 'jawa.awt.event.KeyAdapter)
(setq MouseEvent 'jawa.awt.event.MouseEvent)
(setq MouseWheelEvent 'java.awt.event.MouseWheelEvent)

;COLOR
(cond (isLinux
    (setq Black (new Color 0 0 0))
    (setq White (new Color 255 255 255))
    (setq Red (new Color 255 0 0))
    (setq MiddleRed (new Color 192 0 0))
    (setq DarkRed (new Color 128 0 0))
    (setq Green (new Color 0 255 0))
    (setq DarkGreen (new Color 0 128 0))
    (setq Blue (new Color 0 0 255))
    (setq DarkBlue (new Color 0 0 128))
    (setq Yellow (new Color 0 255 255))
    (setq DarkYellow (new Color 0 128 128))
    (setq Orange (new Color 255 255 0))
    (setq DarkOrange (new Color 128 128 0))
    (setq Purple (new Color 255 0 255))
    (setq DarkPurple (new Color 128 0 128))
    (setq Grey (new Color 192 192 192))
    (setq DarkGrey (new Color 96 96 96))
  )
  (t
    (setq Black (new Color 0 0 0))
    (setq White (new Color 1 1 1))
    (setq Red (new Color 1 0 0))
    (setq MiddleRed (new Color 0.75 0 0))
    (setq DarkRed (new Color 0.5 0 0))
    (setq Green (new Color 0 1 0))
    (setq DarkGreen (new Color 0 0.5 0))
    (setq Blue (new Color 0 0 1))
    (setq DarkBlue (new Color 0 0 0.5))
    (setq Yellow (new Color 0 1 1))
    (setq DarkYellow (new Color 0 0.5 0.5))
    (setq Orange (new Color 1 1 0))
    (setq DarkOrange (new Color 0.5 0.5 0))
    (setq Purple (new Color 1 0 1))
    (setq DarkPurple (new Color 0.5 0 0.5))
    (setq Grey (new Color 0.75 0.75 0.75))
    (setq DarkGrey (new Color 0.375 0.375 0.375))
  )
)



;Some Useful variables
(setq runtime (Runtime.getRuntime))
(setq console (System.console))
(setenv 'user.langague language)
(setenv 'user.country country)
(Locale.setDefault (new Locale language country))
(setq locale (Locale.getDefault))
; Fonctionne sous Linux
;(setq tray (SystemTray.getSystemTray))
;(setq icon ((new ImageIcon '/ian/nil/nil_logo.png).getImage))
;(setq trayIcon (new TrayIcon icon "Bulle NIL"))
;(tray.add trayIcon)


;## END
(print "Java classnames shortcuts loaded.")
(makeQuickLoad 'lj scriptFile)
