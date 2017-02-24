package edu.umass.cs.iesl.watr
package databasics

import doobie.imports._
import shapeless._
import geometry._

import TypeTags._
import utils.EnrichNumerics._
import scalaz.std.list._
import scalaz.syntax.traverse._
import scalaz.syntax.apply._

trait DoobiePredef {

  def putStrLn(s: => String): ConnectionIO[Unit] =
    FC.delay(println(s))


  type Int4 = Int :: Int :: Int :: Int :: HNil

  implicit val LTBoundsMeta: Composite[LTBounds] =
    Composite[Int4].xmap({
      case l :: t :: w :: h :: HNil =>
        val (bl, bt, bw, bh) = (itod(l), itod(t), itod(w), itod(h))
        LTBounds(bl, bt, bw, bh)
    },{ltb =>
      val LTBounds(l, t, w, h) = ltb
      val (bl, bt, bw, bh) = (dtoi(l), dtoi(t), dtoi(w), dtoi(h))
      bl :: bt :: bw :: bh :: HNil
    })



  implicit val StrDocumentIDMeta: Meta[String @@ DocumentID] =
    Meta[String].nxmap(
      str => DocumentID(str),
      docId => docId.unwrap
    )

  import scala.reflect.runtime.universe._


  def TypeTagMeta[T: TypeTag](
    f: Int => Int@@T)(
    implicit T: TypeTag[Int@@T]
  ): Meta[Int@@T] = Meta[Int].xmap(n => f(n), _.unwrap)


  implicit val DocumentIDMeta   : Meta[Int@@DocumentID   ] = TypeTagMeta[DocumentID   ](DocumentID  (_))
  implicit val TextReflowIDMeta : Meta[Int@@TextReflowID ] = TypeTagMeta[TextReflowID ](TextReflowID(_))
  implicit val RegionIDMeta     : Meta[Int@@RegionID     ] = TypeTagMeta[RegionID     ](RegionID    (_))
  implicit val PageIDMeta       : Meta[Int@@PageID       ] = TypeTagMeta[PageID       ](PageID      (_))
  implicit val ImageIDMeta      : Meta[Int@@ImageID      ] = TypeTagMeta[ImageID      ](ImageID     (_))
  implicit val PageNumMeta      : Meta[Int@@PageNum      ] = TypeTagMeta[PageNum      ](PageNum     (_))
  implicit val ZoneIDMeta       : Meta[Int@@ZoneID       ] = TypeTagMeta[ZoneID       ](ZoneID      (_))
  implicit val LabelIDMeta      : Meta[Int@@LabelID      ] = TypeTagMeta[LabelID      ](LabelID     (_))

  def defineTrigger(tablename: Fragment, name: Fragment, decls: Fragment, body: Fragment, onInsert: Boolean): Update0 = {
    val funcname = name ++ fr0"_" ++ tablename
    val triggerName = name ++ fr0"_" ++ tablename ++ fr0"_trigger"
    val onClause = if (onInsert) fr"INSERT" else fr"DELETE"

    val frag = fr"""
       CREATE OR REPLACE FUNCTION """ ++ funcname ++ fr0"""() RETURNS TRIGGER AS $$func$$
         DECLARE """ ++ decls ++ fr""";
         BEGIN
         """ ++ body ++ fr"""
         END;
       $$func$$ LANGUAGE plpgsql;

       DROP TRIGGER IF EXISTS """ ++ triggerName ++ fr""" ON """ ++ tablename ++ fr""";
       CREATE TRIGGER """ ++ triggerName ++ fr"""
         BEFORE """ ++ onClause ++ fr"ON" ++ tablename ++ fr"""
         FOR EACH ROW EXECUTE PROCEDURE """ ++ funcname ++ fr"""();
     """
    // println(frag.toString())
    frag.update
  }

  def defineOrderingTriggers(table: Fragment, keycol: Fragment): ConnectionIO[Int] = {
    val aliases = List(
      fr"maxrank integer",
      fr"nextrank integer",
      fr0"rec ALIAS FOR NEW"
    ).intercalate(fr"; ")

    val keytest =  keycol ++fr0"""=rec.""" ++ keycol

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
