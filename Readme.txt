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
  participant that participant's room, shower, shower time, job, and so
  on. The other files simply describe their respective items. For
  example, rooms.tsv lists the allowable room numbers, and
  shower-times.tsv lists the possible shower times.

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
  sees as problems since they may not actually be problems. (Roshi
  doesn't have an assigned shower time for example.)


1.2 Operation
─────────────

  You must open a command prompt window with the Windows CMD
  program. Once inside navigate to the "bin" directory in this
  distribution using cd and do

  sdocs -i <input directory> -o <output directory>

  where you replace <input directory> and <output directory> with the
  actual directory names. The output directory must exist already.

  A realistic-looking invocation with command prompt might look like

  > sdocs -i C:\Users\Ed\2018-01-sesshin -o C:\Users\Ed\pdfs

  You don't have to make the bin directory current before you do this
  but if you don't then you need to qualify sdocs with the directory
  path so Windows can find it. For example, instead of "sdocs" you might
  type "sesshin-greeter\bin\sdocs".