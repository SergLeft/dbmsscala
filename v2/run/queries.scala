package dbms.v2.run

import dbms.v2.misc.Variant
import dbms.v2.store.Table


/** Returns a [[Table]] containing all known exams with their respective dates and subjects.
 *
 *  @param students a [[Table]] containing student info (studentID, name, matriculationYear, ...)
 *  @param exams a [[Table]] containing a list of exam results (studentID, subject, date, grade, ...)
 *  
 *  This query extracts unique exam information (date and subject) from the exams table.
 */
def knownExams(students: Table, exams: Table): Table = {
  // Project to get only date and subject columns, then remove duplicates
  exams
    .project(Seq("date", "subject"))
    .distinct
}

/** Returns a [[Table]] containing all students scoring the highest grade on the software design exam.
 *
 *  @param students a [[Table]] containing student info
 *  @param exams a [[Table]] containing a list of exam results
 *  
 *  Steps:
 *  1. Filter exams to only "Software Design" subject
 *  2. Find the best (lowest) grade
 *  3. Filter to students with that grade
 *  4. Join with students to get student info
 */
def topStudents(students: Table, exams: Table): Table = {
  // Filter exams for Software Design
  val softwareDesignExams = exams.filterByScan("subject", Variant("Software Design"))
  
  // Find the best grade (lowest number = best grade in German system)
  val bestGrade = softwareDesignExams
    .iterator
    .map(record => record.getValue("grade"))
    .min
  
  // Filter to only top-scoring exams
  val topExams = softwareDesignExams.filterByScan("grade", bestGrade)
  
  // Join with students to get full student info
  students.naturalJoin(topExams)
}

/** Returns a [[Table]] containing all students who were matriculated 2022 or later and also passed the DSEA exam.
 *
 *  @param students a [[Table]] containing student info (with matriculationYear attribute)
 *  @param exams a [[Table]] containing a list of exam results
 *  
 *  Steps:
 *  1. Filter students matriculated 2022 or later
 *  2. Filter exams for DSEA subject with passing grade (≤ 4.0)
 *  3. Join the results
 */
def earlyBirds(students: Table, exams: Table): Table = {
  // Filter students matriculated 2022 or later
  val recentStudents = students.filterRangeByScan("matriculationYear", Variant(2022L), Variant(Long.MaxValue))
  
  // Filter exams for DSEA with passing grade (in German system, ≤ 4.0 is passing)
  val dseaExams = exams
    .filterByScan("subject", Variant("DSEA"))
    .filterRangeByScan("grade", Variant(0.0), Variant(4.1))  // 4.0 or below is passing
  
  // Join to get students who passed DSEA and were recently matriculated
  recentStudents.naturalJoin(dseaExams)
}

/** Returns a [[Table]] containing all exam results for the specified student.
 *
 *  @param students a [[Table]] containing student info
 *  @param exams a [[Table]] containing a list of exam results
 *  @param studentName the student to create the transcript for
 *  
 *  Steps:
 *  1. Find the student by name
 *  2. Get their studentID
 *  3. Filter exams by that studentID
 */
def personalTranscript(students: Table, exams: Table, studentName: String): Table = {
  // Find the student by name
  val student = students.filterByScan("name", Variant(studentName))
  
  // Join with exams to get all their exam results
  student.naturalJoin(exams)
}
