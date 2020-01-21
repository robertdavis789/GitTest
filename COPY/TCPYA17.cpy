
      *********************************************************
      *   COPYBOOK TCPYA17 Changed for a demo   Change for demo
      *************Again***************************************
      *********************************************************
       01  STAR-LINE-1.
           05 FILLER                  PIC X(20).
           05 FILLER                  PIC X(90) VALUE ALL '*'.
       01  FLASH-LINE.
           05 FILLER                  PIC X(20).
           05 FILLER                  PIC X(5) VALUE ALL '*'.
           05 FILLER                  PIC X(17)
              VALUE 'FLASH FLASH FLASH'.
           05 FILLER                  PIC X(26).
           05 FILLER                  PIC X(5) VALUE ALL '*'.
           05 FILLER                  PIC X(5) VALUE ALL '*'.
           05 FILLER                  PIC X(5) VALUE ALL '*'.
      *   THIS IS A TRAINING COPY BOOK FOR PETER NEW VERS
       01  BODY-LINE.
           05 FILLER                  PIC X(20).
           05 FILTER                  PIC X(10).
           05 FILLER                  PIC X(5) VALUE ALL '*'.
           05 BODY-TEXT               PIC X(80).
           05 FILLER                  PIC X(5) VALUE ALL '*'.
       01  MESSAGE-LINE.
           05 FILLER                  PIC X(44) VALUE
           ' A VERY IMPORTANT MESSAGE BROUGHT TO YOU BY '.
           05 MESSANGER               PIC X(36).