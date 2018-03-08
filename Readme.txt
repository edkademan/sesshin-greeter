1 Generate sesshin documents
════════════════════════════

1.1 Description
───────────────

  The sdocs program is a command-line utility that usually takes just
  two arguments: an input directory and an output directory. The input
  directory contains the source information in the form of
  tab-separated-variable tables and the output directory holds the pdf
  results. The test-data/good subdirectory included in this distribution
  contains examples. The only files necessary are

  • jobs.tsv
  • rooms.tsv
  • roster.tsv
  • showers.tsv
  • shower-times.tsv

  The roster.tsv file is the most important and is probably the only one
  that will change from sesshin to sesshin. It lists for each individual
  participant that participant's room, possible shower, possible shower
  time, job, and so on. The other files simply describe their respective
  items. For example, rooms.tsv lists the allowable room numbers, and
  shower-times.tsv lists the possible shower times.  In what follows the
  word "showers" refers both to showers and to the soaking baths.

  You can assign to each individual participant a particular shower but
  you don't have to. If you don't the system will assign one for
  you. The same is true for shower times. If you leave that field blank
  the system will pick a time for that individual. The shower that it
  picks will be close to that person's bedroom and the time that it
  picks will not conflict with that person's job. Also, you can specify
  a "shower time" that is actually a long span that implies several
  possible shower time slots. For example, if you describe the beginning
  and ending shower times as "8:00am 2:00pm" the program will pick some
  slot either in the rest period immediately after the morning work
  period or in the period immediately after lunch, and will not pick one
  later in the day. To summarize, you can assign

  • a shower and a shower time,
  • a shower with no shower time,
  • a shower time with no shower or
  • neither a shower or shower time.

  In addition to generating the pdf documents sdocs checks to make sure
  the information is coherent and complains if:

  • someone doesn't have a job, or has a job that it doesn't recognize
  • there is a job for which no one was assigned
  • someone wasn't assigned a room or was assigned a room that it
    doesn't recognize
  • someone wasn't assigned a shower or was assigned a shower that it
    doesn't recognize
  • someone wasn't assigned a shower time or was assigned one that it
    doesn't recognize
  • more people were assigned to take a bath at the same time than that
    bath can accommodate.

  It will try to generate the documents even in the presence of what it
  sees as problems since they may not actually be problems. (You might
  be allowing the program to choose shower assignments in the way
  described above for example.)


1.2 Operation
─────────────

  You must open a command prompt window with the Windows CMD
  program. Once inside navigate to the "bin" directory in this
  distribution using cd and do

  ┌────
  │ sdocs -i <input directory> -o <output directory>
  └────

  where you replace <input directory> and <output directory> with the
  actual directory names. The output directory must exist already.

  A realistic-looking invocation with command prompt might look like

  ┌────
  │ > sdocs -i C:\Users\Ed\2018-01-sesshin -o C:\Users\Ed\pdfs
  └────

  You don't have to make the bin directory current before you do this
  but if you don't then you need to qualify sdocs with the directory
  path so Windows can find it. For example, instead of "sdocs" you might
  type "sesshin-greeter\bin\sdocs".

  sdocs puts a title on each of the pages it generates. So in addition
  to specifying input and output directories on the command line you can
  also specify a title if you don't like the default one it chooses.

  ┌────
  │ > sdocs -i <...> -o <...> -t "April 2018 7-day Sesshin"
  └────
