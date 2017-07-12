#!/usr/bin/sbcl --script

(load "../src/load")
(load "../src/test-utils")

(setf *print-pretty* t)
;(setf *random-state* (make-random-state t))


(defun test-utils ()
  (do-test
    (math:norm '(3 0))
    '(1.0 0.0))

  (do-test
    (math:sub '(1 2) '(2 3))
    '(-1 -1))

  (do-test
    (math:add '(1 2) '(2 3))
      '(3 5))

  (do-test
    (math:nsub '(1 2) '(2 10))
    '(-0.12403473 -0.99227786))

  (do-test
    (math:len2 '(1 2))
    5)

  (do-test
    (math:len '(1 2))
    2.236068)

  (do-test
    (math:len '(1.0d0 2.0d0))
    2.23606797749979d0)

  (do-test
    (math:dst '(1 2) '(1 3))
    1.0)

  (do-test
    (math:mid '(1 2) '(3 4))
    '(2 3))

  (do-test
    (math:lmid '((1 2) (3 4) (5 6)))
    '(3 4))

  ;(do-test
  ;  (lget '((1 2) (3 4) (5 6)) '(0 2))
  ;  '((1 2) (5 6)))

  (do-test
    (math:inc 0.1 0.4)
    0.5)

  (do-test
    (math:inc 0.1 -0.4)
    0.7)

  (do-test
    (math:linspace 0 10 1)
    (list 0.0))

  (do-test
    (math:linspace 0 10 3)
    (list 0.0 5.0 10.0))

  (do-test
    (math:linspace 0 10 2 :end nil)
    (list 0.0 5.0))

  (do-test
    (math:linspace 0 10 2 :end t)
    (list 0.0 10.0))

  (do-test
    (math:range 2 5)
    (list 2 3 4))

  (do-test
    (math:range 5)
    (list 0 1 2 3 4)))


(defun test-rnd ()

  (do-test
    (length (rnd:rndspace 0 10 10))
    10)

  (do-test
    (rnd:rndspace 0 10 10)
     '(3.152262934102661d0 9.411859332177082d0 9.143334482781892d0
        9.707515698488775d0 6.005604715628142d0 2.8377247312878073d0
        8.435221790928992d0 8.314710996278352d0 5.844153198534443d0
        9.189848934771323d0))

  (do-test
    (rnd:rndspace 0 10 10 :order t)
     '(0.7142292146110663d0 3.109552134181708d0 3.1148128311978818d0
        3.4237318221390423d0 3.5746993898250556d0 4.862646815859608d0
        5.154807478401608d0 6.982586020701715d0 7.8445379456298925d0
        8.67986375197924d0))

  (do-test
    (rnd:rndspacei 0 10 10)
     '(7 9 2 0 0 2 4 4 9 9))

  (do-test
    (rnd:rndspacei 0 10 10 :order t)
     '(1 1 3 3 4 6 7 8 8 9)))


(defun test-bzspl ()
(let ((pts-a '((-20 99) (0 1) (10 20) (100 100)))
      (pts-b'((-20 99) (0 1) (10 20) (100 100) (-3 -17) (0 4))))
    (do-test
      (bzspl:pos* (bzspl:make pts-a) (math:linspace 0 1 10))
      '((-20.0d0 99.0d0)
        (-11.851851851851851d0 60.75308641975309d0)
        (-5.185185185185184d0 33.12345679012347d0)
        (-1.7763568394002505d-15 16.11111111111112d0)
        (3.703703703703706d0 9.716049382716065d0)
        (7.160493827160495d0 13.481481481481485d0)
        (17.77777777777777d0 24.666666666666664d0)
        (36.79012345679013d0 42.814814814814824d0)
        (64.19753086419752d0 67.92592592592591d0)
        (100.0d0 100.0d0)))

    (do-test
      (bzspl:pos* (bzspl:make pts-b) (math:linspace 0 1 10))
      '((-20.0d0 99.0d0)
        (-5.185185185185184d0 33.12345679012347d0)
        (3.703703703703706d0 9.716049382716065d0)
        (12.777777777777775d0 20.22222222222222d0)
        (36.9753086419753d0 43.728395061728385d0)
        (70.23456790123457d0 72.91358024691358d0)
        (72.11111111111111d0 69.55555555555556d0)
        (37.72839506172839d0 29.481481481481474d0)
        (8.098765432098773d0 1.0370370370370452d0)
        (0.0d0 4.0d0)))

    (do-test
      (bzspl:pos* (bzspl:make pts-a :closed t) (math:linspace 0 1 10))
      '((-10.0d0 50.0d0)
        (-2.098765432098765d0 18.000000000000007d0)
        (3.8271604938271615d0 9.111111111111121d0)
        (12.777777777777775d0 20.22222222222222d0)
        (36.9753086419753d0 43.728395061728385d0)
        (69.81481481481481d0 75.77777777777779d0)
        (68.33333333333334d0 95.33333333333331d0)
        (27.53086419753086d0 98.79012345679011d0)
        (-5.061728395061721d0 83.97530864197532d0)
        (-10.0d0 50.0d0)))

    (do-test
      (bzspl:pos* (bzspl:make pts-b :closed t) (math:linspace 0 1 10))
      '((-10.0d0 50.0d0) (1.1111111111111098d0 10.666666666666671d0)
        (12.777777777777777d0 20.22222222222222d0)
        (55.0d0 60.0d0)
        (72.11111111111111d0 69.55555555555554d0)
        (20.055555555555543d0 10.16666666666666d0)
        (-1.5d0 -6.5d0)
        (-4.611111111111115d0 23.944444444444468d0)
        (-14.444444444444446d0 72.44444444444446d0)
        (-10.0d0 50.0d0)))))


(defun test-hset ()

  (let ((hs (hset:make)))

    (do-test
      (hset:add hs 1)
      t)

    (do-test
      (hset:add hs 1)
      nil)

    (do-test
      (hset:add hs 20)
      t)

    (do-test
      (hset:add hs 40)
      t)

    (do-test
      (hset:add hs 73)
      t)

    (do-test
      (hset:num hs)
      4)

    (do-test
      (hset:del hs 1)
      t)

    (do-test
      (hset:del hs 1)
      nil)

    (do-test
      (hset:mem hs 40)
      t)

    (do-test
      (hset:mem* hs (list 40 88))
      (list t nil))

    (do-test
      (sort (hset:to-list hs) #'<)
      (list 20 40 73)))


  (let ((hs (hset:make :init (list 1 2 3))))

    (do-test
      (hset:to-list hs)
      (list 1 2 3))))


(defun test-graph ()

  (let ((grph (graph:make)))

    (do-test
      (graph:add grph 1 1)
      t)

    (do-test
      (graph:add grph 1 2)
      t)

    (do-test
      (graph:add grph 1 2)
      nil)

    (do-test
      (graph:add grph 2 1)
      nil)

    (do-test
      (graph:get-num-edges grph)
      4)

    (do-test
      (graph:get-edges grph)
      '#((1 1) (1 2)))

    (do-test
      (graph:add grph 20 5)
      t)

    (do-test
      (graph:get-edges grph)
      '#((1 1) (1 2) (5 20)))

    (do-test
      (graph:del grph 1 2)
      t)

    (do-test
      (graph:del grph 1 2)
      nil)

    (do-test
      (graph:get-edges grph)
      '#((1 1) (5 20)))

    (do-test
      (graph:get-num-edges grph)
      4)

    (do-test
      (graph:mem grph 1 4)
      nil)

    (do-test
      (graph:mem grph 1 1)
      t)

    (do-test
      (sort (graph:get-verts grph) #'<)
      '(1 5 20))

    (do-test
      (graph:del grph 1 1)
      t)

    (do-test
      (graph:get-edges grph)
      '#((5 20)))

    (do-test
      (sort (graph:get-verts grph) #'<)
      '(5 20))

    (do-test
      (graph:del grph 5 20)
      t)

    (do-test
      (sort (graph:get-verts grph) #'<)
      nil)))


(defun main ()
  (test-title (test-utils))
  (test-title (test-rnd))
  (test-title (test-bzspl))
  (test-title (test-hset))
  (test-title (test-graph))
  (test-title (test-summary)))

(main)

