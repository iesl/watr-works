package edu.umass.cs.iesl.watr
package databasics

import doobie.imports._

import scalaz.std.list._
import scalaz.syntax.traverse._
import scalaz.syntax.apply._

trait DoobiePredef {

  def putStrLn(s: => String): ConnectionIO[Unit] =
    FC.delay(println(s))



  def defineTrigger(tablename: Fragment, name: Fragment, decls: Fragment, body: Fragment, onInsert: Boolean): Update0 = {
    val funcname = name ++ fr0"_" ++ tablename
    val triggerName = name ++ fr0"_" ++ tablename ++ fr0"_trigger"
    val onClause = if (onInsert) fr" BEFORE INSERT " else fr" AFTER DELETE "

    val frag = fr"""
       CREATE OR REPLACE FUNCTION """ ++ funcname ++ fr0"""() RETURNS TRIGGER AS $$func$$
         DECLARE """ ++ decls ++ fr""";
         BEGIN
         """ ++ body ++ fr"""
         END;
       $$func$$ LANGUAGE plpgsql;

       DROP TRIGGER IF EXISTS """ ++ triggerName ++ fr""" ON """ ++ tablename ++ fr""";
       CREATE TRIGGER """ ++ triggerName ++ onClause ++ fr"ON" ++ tablename ++ fr"""
         FOR EACH ROW EXECUTE PROCEDURE """ ++ funcname ++ fr"""();
     """
    println(frag.toString())
    frag.update
  }

  def defineOrderingTriggers(table: Fragment, keycols: Fragment*): ConnectionIO[Int] = {
    val aliases = List(
      fr"maxrank integer",
      fr"nextrank integer",
      fr0"rec ALIAS FOR NEW"
    ).intercalate(fr"; ")

    // val keycols: List[Fragment] = List()
    val keytest = keycols
      .toList.map(col =>
        col ++fr0"""=rec.""" ++ col
      ).intercalate(fr" AND")

    // val keytest =  keycol ++fr0"""=rec.""" ++ keycol

    val insertTrigger = defineTrigger(
      table,
      fr0"insert_row_func",
      aliases,
      fr"""
         maxrank := (SELECT MAX(rank) FROM """ ++ table ++ fr""" WHERE """ ++ keytest ++ fr""");

         IF maxrank IS NULL THEN
             nextrank := 0;
         ELSE
             nextrank := maxrank+1;
         END IF;

         IF rec.rank IS NULL THEN
             rec.rank := nextrank;
         ELSEIF rec.rank > nextrank THEN
             rec.rank := nextrank;
         ELSEIF rec.rank < 0 THEN
             rec.rank := 0;
         END IF;

         UPDATE """ ++ table ++ fr""" SET rank = rank+1 WHERE rank >= rec.rank AND """ ++ keytest ++ fr0""";

         RETURN rec;
      """, true
    )
    val aliases1 = List(
      fr0"rec ALIAS FOR OLD"
    ).intercalate(fr"; ")

    val updateTrigger = defineTrigger(
      table,
      fr0"delete_row_func",
      aliases1,
      fr"""
        UPDATE """ ++ table ++ fr""" SET rank = rank-1 WHERE rank > rec.rank AND""" ++ keytest ++ fr0""";
        RETURN rec;
      """, false
    )

    (insertTrigger.run *> updateTrigger.run)

  }

  /// DANGER!!!!! really drops everything!
  def veryUnsafeDropDatabase(): Update0 = {
    // http://stackoverflow.com/questions/3327312/drop-all-tables-in-postgresql
    sql"""
        DROP SCHEMA public CASCADE;
        CREATE SCHEMA public;
        GRANT ALL ON SCHEMA public TO postgres;
        GRANT ALL ON SCHEMA public TO public;
        COMMENT ON SCHEMA public IS 'standard public schema';
      """.update
  }
}
