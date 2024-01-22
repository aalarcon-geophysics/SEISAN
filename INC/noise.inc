      integer nf            ! number of frequencies in spectra
      parameter (nf=500)
      integer ndb           ! number of db values
      parameter (ndb=150)
      real f_low,f_high,db_low,db_high
c next two had been commented out ??? lo
c      parameter (f_low=.002)
c      parameter (f_high=100.)
c
      parameter (db_low=-200.)
      parameter (db_high=-50.)

c
c   petersen noise curves, log f and db
c
      real low_f(22),high_f(14),low_l(22),high_l(14) ! petersen noise
      data high_f/   1.000000,    6.575773E-01,    4.948500E-01,
     *           9.691001E-02,   -5.797836E-01,   -6.627579E-01,
     *           -7.993407E-01,  -8.976271E-01,   -1.187521,
     *           -1.301030,       -2.549984,       -4.000000,
     *            -4.477121, -5.0/
      data high_l/ -91.5,-97.4,-110.5,-120.0,-97.9,-96.5,-101.0,
     *             -113.5,-120.0,-138.5,-126.0,-80.1,-65.0,-48.5/

      data low_f/    1.000000,    7.695511E-01,    3.979400E-01,
     *             9.691001E-02, -9.342168E-02,   -3.802112E-01,
     *            -6.334685E-01,  -6.989700E-01,  -7.781512E-01,
     *             -1.000000,       -1.079181,       -1.193125,
     *              -1.340444, -1.50, -1.65, -1.89, -2.0,
     *              -2.19,-2.51,-2.78,-4.0,-5.0/
      data low_l/   -168.0,-166.7,-166.7,-169.2,-163.7,-148.6,
     *              -141.1,-141.1,-149.0,-163.8,-166.2,-162.1,
     *              -177.5,-185.0,-187.5,-187.5,-185.0,-184.9,
     *              -187.5,-184.4,-151.9,-103.1/

