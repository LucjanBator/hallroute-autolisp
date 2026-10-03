;;; hallroute.lsp — AutoCAD <-> Hallroute bridge (AutoLISP, AutoCAD 2014+ and AutoCAD LT 2024+)
;;; Prompts in English, German or Polish: run HRLANG.
;;; Status: NOT yet tested inside AutoCAD. Always try it on a COPY of your drawing first.
;;;
;;; HREXPORT  select machines (blocks or closed polylines), pick the hall's bottom-left corner,
;;;           save a CSV you can load in Hallroute as "Machines".
;;; HRIMPORT  select the SAME machines, pick the SAME corner, open the CSV from Hallroute's
;;;           "Download for AutoCAD" zip (hallroute-layout.csv). Machines are moved and rotated.
;;;
;;; Names: a block attribute with tag NAME, TAG, ID or LABEL is used; otherwise the block name.
;;; Repeated names get " 2", " 3"... in drawing-handle order, so export and import match.
;;; Machines on a layer whose name contains FIX are exported as fixed.

(vl-load-com)

;;; Language of prompts: "en", "de" or "pl". Change with the command HRLANG.
(if (null *hr-lang*) (setq *hr-lang* "en"))
(defun hr:m (en de pl) (cond ((= *hr-lang* "de") de) ((= *hr-lang* "pl") pl) (t en)))
(defun c:HRLANG (/ k)
  (initget "EN DE PL")
  (setq k (getkword "\nLanguage / Sprache / Jezyk [EN/DE/PL] <EN>: "))
  (setq *hr-lang* (strcase (if k k "EN") t))
  (princ (hr:m "\nHallroute: English." "\nHallroute: Deutsch." "\nHallroute: polski."))
  (princ))

(defun hr:split (s d / p r)
  (while (setq p (vl-string-search d s))
    (setq r (cons (substr s 1 p) r)
          s (substr s (+ p 1 (strlen d)))))
  (reverse (cons s r)))

(defun hr:trimq (s)
  (setq s (vl-string-trim " \t" s))
  (if (and (> (strlen s) 1) (= (substr s 1 1) "\"") (= (substr s (strlen s) 1) "\""))
    (substr s 2 (- (strlen s) 2))
    s))

(defun hr:bbox (e / o mn mx)
  (setq o (vlax-ename->vla-object e))
  (vla-getboundingbox o 'mn 'mx)
  (list (vlax-safearray->list mn) (vlax-safearray->list mx)))

(defun hr:basename (e / o nm atts a tag)
  (setq o (vlax-ename->vla-object e) nm nil)
  (if (= (vla-get-objectname o) "AcDbBlockReference")
    (progn
      (if (= (vla-get-hasattributes o) :vlax-true)
        (foreach a (vlax-invoke o 'getattributes)
          (setq tag (strcase (vla-get-tagstring a)))
          (if (and (null nm) (member tag '("NAME" "TAG" "ID" "LABEL")) (/= (vla-get-textstring a) ""))
            (setq nm (vla-get-textstring a)))))
      (if (null nm) (setq nm (vla-get-effectivename o))))
    (setq nm "Machine"))
  (vl-string-translate "," " " nm))

(defun hr:hexlt (a b)
  (if (/= (strlen a) (strlen b)) (< (strlen a) (strlen b)) (< a b)))

(defun hr:items (ss / i e lst out seen nm k)
  ;; returns list of (name . ename), sorted by handle, names made unique
  (setq i 0 lst nil)
  (while (< i (sslength ss))
    (setq e (ssname ss i) lst (cons e lst) i (1+ i)))
  (setq lst (vl-sort lst '(lambda (a b) (hr:hexlt (cdr (assoc 5 (entget a))) (cdr (assoc 5 (entget b)))))))
  (setq seen nil out nil)
  (foreach e lst
    (setq nm (hr:basename e)
          k (cdr (assoc nm seen)))
    (if k
      (setq seen (subst (cons nm (1+ k)) (assoc nm seen) seen) nm (strcat nm " " (itoa (1+ k))))
      (setq seen (cons (cons nm 1) seen)))
    (setq out (cons (cons nm e) out)))
  (reverse out))

(defun hr:scale (/ u s)
  (setq u (getvar "INSUNITS"))
  (setq s (cond ((= u 4) 0.001) ((= u 5) 0.01) ((= u 6) 1.0) ((= u 1) 0.0254) ((= u 2) 0.3048) (t nil)))
  (if (null s)
    (progn (setq s (getreal (hr:m "\nMetres per drawing unit (1 = metres, 0.001 = millimetres) <1>: " "\nMeter pro Zeichnungseinheit (1 = Meter, 0.001 = Millimeter) <1>: " "\nMetry na jednostke rysunku (1 = metry, 0.001 = milimetry) <1>: ")))
           (if (null s) (setq s 1.0))))
  s)

(defun hr:select ()
  (prompt (hr:m "\nSelect the machines (blocks or closed polylines): " "\nMaschinen waehlen (Bloecke oder geschlossene Polylinien): " "\nWybierz maszyny (bloki lub zamkniete polilinie): "))
  (ssget '((0 . "INSERT,LWPOLYLINE,POLYLINE"))))

(defun c:HREXPORT (/ ss pt sc fn f bb mn mx x y w d lay fixed n)
  (if (and (setq ss (hr:select))
           (setq pt (getpoint (hr:m "\nPick the hall's bottom-left corner: " "\nLinke untere Hallenecke waehlen: " "\nWskaz lewy dolny naroznik hali: ")))
           (setq sc (hr:scale))
           (setq fn (getfiled (hr:m "Save machines for Hallroute" "Maschinen fuer Hallroute speichern" "Zapisz maszyny dla Hallroute") "hallroute-machines" "csv" 1)))
    (progn
      (setq f (open fn "w") n 0)
      (write-line "name,x,y,width,depth,fixed" f)
      (foreach it (hr:items ss)
        (setq bb (hr:bbox (cdr it)) mn (car bb) mx (cadr bb)
              x (* sc (- (/ (+ (car mn) (car mx)) 2.0) (car pt)))
              y (* sc (- (/ (+ (cadr mn) (cadr mx)) 2.0) (cadr pt)))
              w (* sc (- (car mx) (car mn)))
              d (* sc (- (cadr mx) (cadr mn)))
              lay (strcase (cdr (assoc 8 (entget (cdr it)))))
              fixed (if (vl-string-search "FIX" lay) "1" "0"))
        (write-line (strcat (car it) "," (rtos x 2 3) "," (rtos y 2 3) "," (rtos w 2 3) "," (rtos d 2 3) "," fixed) f)
        (setq n (1+ n)))
      (close f)
      (princ (strcat "\nHallroute: " (itoa n) (hr:m " machines written to " " Maschinen gespeichert in " " maszyn zapisano do ") fn))))
  (princ))

(defun hr:col (hdr name / i r)
  (setq i 0 r nil)
  (foreach h hdr (if (and (null r) (= (strcase (hr:trimq h)) (strcase name))) (setq r i)) (setq i (1+ i)))
  r)

(defun c:HRIMPORT (/ ss pt sc fn f line hdr ix iy ir items row nm e bb mn mx c tx ty o moved missing)
  (if (and (setq ss (hr:select))
           (setq pt (getpoint (hr:m "\nPick the SAME bottom-left corner as for the export: " "\nDIESELBE linke untere Ecke wie beim Export waehlen: " "\nWskaz TEN SAM lewy dolny naroznik co przy eksporcie: ")))
           (setq sc (hr:scale))
           (setq fn (getfiled (hr:m "Open hallroute-layout.csv" "hallroute-layout.csv oeffnen" "Otworz hallroute-layout.csv") "" "csv" 0)))
    (progn
      (setq items (hr:items ss) f (open fn "r") hdr (hr:split (read-line f) ",")
            ix (hr:col hdr "x") iy (hr:col hdr "y") ir (hr:col hdr "rotated") moved 0 missing nil)
      (if (not (and ix iy))
        (princ (hr:m "\nHallroute: this CSV has no x and y columns." "\nHallroute: diese CSV hat keine Spalten x und y." "\nHallroute: ten plik CSV nie ma kolumn x i y."))
        (progn
          (vla-startundomark (vla-get-activedocument (vlax-get-acad-object)))
          (while (setq line (read-line f))
            (setq row (hr:split line ",") nm (hr:trimq (nth 0 row)) e (cdr (assoc nm items)))
            (if (null e)
              (setq missing (cons nm missing))
              (progn
                (setq bb (hr:bbox e) mn (car bb) mx (cadr bb)
                      c (list (/ (+ (car mn) (car mx)) 2.0) (/ (+ (cadr mn) (cadr mx)) 2.0) 0.0)
                      tx (+ (car pt) (/ (atof (nth ix row)) sc))
                      ty (+ (cadr pt) (/ (atof (nth iy row)) sc))
                      o (vlax-ename->vla-object e))
                (vla-move o (vlax-3d-point c) (vlax-3d-point (list tx ty 0.0)))
                (if (and ir (= (hr:trimq (nth ir row)) "1"))
                  (vla-rotate o (vlax-3d-point (list tx ty 0.0)) (/ pi 2.0)))
                (setq moved (1+ moved)))))
          (vla-endundomark (vla-get-activedocument (vlax-get-acad-object)))
          (princ (strcat "\nHallroute: " (itoa moved) (hr:m " machines placed." " Maschinen platziert." " maszyn ustawiono.")))
          (if missing (princ (strcat (hr:m "\nNot found in the selection: " "\nNicht in der Auswahl gefunden: " "\nNie znaleziono w wyborze: ") (itoa (length missing)) (hr:m " name(s), e.g. " " Name(n), z. B. " " nazw, np. ") (car missing))))))
      (close f)))
  (princ))

(princ "\nHallroute: HREXPORT (drawing/Zeichnung/rysunek -> Hallroute), HRIMPORT (Hallroute -> drawing/Zeichnung/rysunek), HRLANG (EN/DE/PL).")
(princ)
