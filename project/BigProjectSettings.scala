// Copyright (C) 2015 - 2016 Sam Halliday
// Licence: http://www.apache.org/licenses/LICENSE-2.0
package fommil

import java.io.IOException
import java.nio.file.attribute.BasicFileAttributes
import java.nio.file._
import java.util.ResourceBundle
import java.util.concurrent.ConcurrentHashMap
import java.util.concurrent.atomic.AtomicBoolean

import org.apache.ivy.core.module.descriptor.ModuleDescriptor
import org.apache.ivy.core.module.id.ModuleRevisionId
import sbt.Scoped.DefinableTask
import sbt._
import Keys._
import sbt.inc.Analysis
import sbt.inc.LastModified

import scala.concurrent._
import scala.concurrent.duration._
import scala.concurrent.{Await, Future}
import scala.util.{Failure, Success, Try}

import ExecutionContext.Implicits.global

/**
 * Publicly exposed keys for settings and tasks that the user may wish
 * to use.
 */
object BigProjectKeys {
  /**
   * The user must tell us when a breaking change has been introduced
   * in a module. It will invalidate the caches of all dependent
   * project.
   */
  val breakingChange = TaskKey[Unit](
    "breakingChange",
    "Inform the build that a breaking change was introduced in this project."
  )

  /**
   * Detects source / resource changes in the Compile configuration
   * and treats their project as breaking changes. Ideal after merging
   * or rebasing to (hopefully) minimise your compile.
   */
  val breakOnChanges = TaskKey[Unit](
    "breakOnChanges",
    "Run breakingChange if sources / resources are more recent than their jar."
  )

  /**
   * Teams that use Eclipse often put tests in separate packages.
   *
   * WORKAROUND: https://bugs.eclipse.org/bugs/show_bug.cgi?id=224708
   */
  val eclipseTestsFor = SettingKey[Option[ProjectReference]](
    "eclipseTestsFor",
    "When defined, points to the project that this project is testing."
  )

  /**
   * A version of the last compilable jar is made available, so that
   * developer tools always have some reference to use for indexing
   * purposes. Recall that packageBin is deleted before each compile.
   *
   * Windows users may still experience stale jars as a result of
   * SI-9632 and similar bugs in tooling.
   *
   * Enabled by default, set to None to disable (e.g. to marginally
   * speed up CI compiles and save disk space).
   *
   * ENSIME use:
   *
   * {{{
   * val ensimeLastCompilableJarTask: Def.Initialize[Task[Option[File]]] =
   *   (state, (artifactPath in packageBin), BigProjectKeys.lastCompilableJar).map { (s, jar, lastOpt) =>
   *     BigProjectSettings.createOrUpdateLast(s.log, jar, lastOpt.get)
   *     lastOpt
   *   }
   * }}}
   */
  val lastCompilableJar = TaskKey[Option[File]](
    "lastCompilableJar",
    "Points to a copy of packageBin that is updated when the packageBin is recreated."
  )
}

object BigProjectSettings extends Plugin {
  import BigProjectKeys._

  /**
   * All the existing jars associated to a project, including the
   * transient dependency jars if this is an Eclipse-style test
   * project.
   */
  private def allPackageBins(
    structure: BuildStructure,
    log: Logger,
    proj: ProjectRef,
    skipEclipseMain: Boolean = false
  ): Seq[File] =
    for {
      p <- proj +: { if (skipEclipseMain) Nil else ((eclipseTestsFor in proj) get structure.data).get.toSeq }
      configs <- ((ivyConfigurations in p) get structure.data).toSeq
      config <- configs
      /* whisky in the */ jar <- (artifactPath in packageBin in config in p) get structure.data
      if jar.exists()
    } yield jar

  /**
   * Try our best to delete a file that may be referenced by a stale
   * scala-compiler file handle (affects Windows).
   */
  def deleteLockedFile(log: Logger, file: File): Unit = {
    log.debug(s"Deleting $file")
    if (file.exists() && !file.delete()) {
      log.debug(s"Failed to delete $file")
      System.gc()
      System.runFinalization()
      System.gc()
      file.delete()
    }
    if (file.exists()) {
      throw new IllegalStateException(s"Failed to delete $file")
    }
  }

  /**
   * TrackLevel.TrackIfMissing will not invalidate or rebuild jar
   * files if the user explicitly recompiles a project. We delete the
   * packageBin associated to a project when compiling that project so
   * that we never have stale jars.
   */
  def deletePackageBinTask = (artifactPath in packageBin, state).map { (jar, s) =>
    deleteLockedFile(s.log, jar)
  }

  /**
   * The location of the packageBin, but under a subfolder named "last".
   */
  private def lastCompilableJarTask = (artifactPath in packageBin).map { jar =>
    Option(jar.getParentFile / "last" / jar.getName)
  }

  /**
   * Similar to deletePackageBinTask but works for all configurations
   * of the current project.
   */
  private def deleteAllPackageBinTask = (thisProjectRef, state).map { (proj, s) =>
    val structure = Project.extract(s).structure
    allPackageBins(structure, s.log, proj).foreach { jar =>
      deleteLockedFile(s.log, jar)
    }
  }

  private def changes(dirs: Seq[File], lastModified: Long) = {
    val newChanges = new AtomicBoolean(false)
    dirs.map(dir => Future {
      Files.walkFileTree(dir.toPath, new SimpleFileVisitor[Path] {
        override def visitFile(file: Path, attrs: BasicFileAttributes): FileVisitResult = {
          super.visitFile(file, attrs)
          if (attrs.lastModifiedTime.toMillis > lastModified) {
            newChanges.getAndSet(true)
          }

          if (newChanges.get()) {
            FileVisitResult.TERMINATE
          } else {
            FileVisitResult.CONTINUE
          }
        }
      })
    }).foreach(Await.ready(_, 10.minutes))
    newChanges.get()
  }

  // Experimental alternative to deleteAllPackageBinTask. Question is
  // if the .lastModified calls are faster than rebuilding... NIO
  // would be much faster.
  private def outdatedPackageBins(
    structure: BuildStructure,
    log: Logger,
    proj: ProjectRef
  ): Seq[File] =
    for {
      p <- proj +: ((eclipseTestsFor in proj) get structure.data).get.toSeq
      configs <- ((ivyConfigurations in p) get structure.data).toSeq
      config <- configs
      // by going through the directories ourselves, we can exit early if we hit a change
      srcDirs <- ((sourceDirectories in p in config) get structure.data).toSeq
      resourceDirs <- ((resourceDirectories in p in config) get structure.data).toSeq
      /* whisky in the */ jar <- (artifactPath in packageBin in config in p) get structure.data
      if jar.exists() && changes(srcDirs ++ resourceDirs, jar.lastModified())
    } yield jar

  private def deleteOutdatedPackageBinsTask = (thisProjectRef, state).map { (proj, s) =>
    val structure = Project.extract(s).structure
    outdatedPackageBins(structure, s.log, proj).foreach { jar =>
      deleteLockedFile(s.log, jar)
    }
  }

  // WORKAROUND https://github.com/sbt/sbt/issues/2417
  implicit class NoMacroTaskSupport[T](val t: TaskKey[T]) extends AnyVal {
    def theTask: SettingKey[Task[T]] = Scoped.scopedSetting(t.scope, t.key)
  }

  // turn T => Task[T]
  def task[T](t: T): Task[T] = Task[T](Info(), Pure(() => t, true))

  /**
   * packageBin causes traversals of dependency projects.
   *
   * Caching must be evicted for a project when:
   *
   * - anything (e.g. source, config, packageBin) changes
   *
   * which we implement by deleting the packageBinFile on every
   * compile.
   *
   * However, dependent project caches must only be evicted if a
   * dependency introduced a breaking change.
   *
   * We trust the developer to inform us of breaking API changes
   * manually using the breakingChange task.
   *
   * We use the file's existence as the cache.
   */
  private def dynamicPackageBinTask: Def.Initialize[Task[File]] = (
    classDirectory,
    (artifactPath in packageBin),
    lastCompilableJar,
    (streams in packageBin),
    packageOptions,
    compile.theTask,
    copyResources.theTask,
    state
  ).flatMap { (classes, jar, lastOpt, s, options, compileTask, copyResourcesTask, st) =>
      if (jar.exists) {
        lastOpt.foreach { last => createOrUpdateLast(s.log, jar, last) }
        task(jar)
      } else {
        (compileTask, copyResourcesTask).map { _ =>
          FastPackage(classes, jar, options, s.log)
          lastOpt.foreach { last => createOrUpdateLast(s.log, jar, last) }

          val clazz = Class.forName("sbt.classpath.ClassLoaderCache")
          val field = clazz.getDeclaredField("delegate")
          field.setAccessible(true)
          val delegate = field.get(st.classLoaderCache).asInstanceOf[java.util.HashMap[_, _]]
          delegate.clear() // could close the values too, but they are also private

          jar
        }
      }
    }

  def createOrUpdateLast(log: Logger, jar: File, last: File): Unit =
    // since this is for IDE's, which may have a performacne cost to
    // pay every time this file is updated, update every 12 hours.
    if (jar.exists() && (!last.exists() || jar.lastModified >= (last.lastModified + 12 * 60 * 60 * 1000L))) {
      log.info(s"Backing up ${jar.getName}")
      deleteLockedFile(log, last)
      last.getParentFile().mkdirs()
      Files.copy(jar.toPath, last.toPath)
    }

  /**
   * transitiveUpdate causes traversals of dependency projects
   *
   * Cache must be evicted for a project and all its dependents when:
   *
   * - changes to the ivy definitions
   * - any inputs to an update phase are changed (changes to generated inputs?)
   */
  private val transitiveUpdateCache = new ConcurrentHashMap[ProjectReference, Seq[UpdateReport]]()
  private def dynamicTransitiveUpdateTask: Def.Initialize[Task[Seq[UpdateReport]]] =
    (thisProject, transitiveUpdate.theTask).flatMap {
      (proj, transitiveUpdateTask) =>
        val key = LocalProject(proj.id)
        val cached = transitiveUpdateCache.get(key)
        if (cached != null) task(cached)
        else (transitiveUpdateTask).map { calculated =>
          transitiveUpdateCache.put(key, calculated)
          calculated
        }
    }

  /**
   * dependencyClasspath causes traversals of dependency projects.
   *
   * Cache must be evicted for a project and all its dependents when:
   *
   * - anything (e.g. source, config) changes and the packageBin is not recreated
   *
   * we implement invalidation by checking that all files in the
   * cached classpath exist, if any are missing, we do the work.
   */
  private val dependencyClasspathCache = new ConcurrentHashMap[(ProjectReference, Configuration), Classpath]()
  private def dynamicDependencyClasspathTask: Def.Initialize[Task[Classpath]] =
    (thisProject, configuration, dependencyClasspath.theTask).flatMap {
      (proj, config, dependencyClasspathTask) =>
        val key = (LocalProject(proj.id), config)
        val cached = dependencyClasspathCache.get(key)
        if (cached != null && cached.forall(_.data.exists())) task(cached)
        else (dependencyClasspathTask).map { calculated =>
          dependencyClasspathCache.put(key, calculated)
          calculated
        }
    }

  /**
   * Gets invoked when the dependencyClasspath cache misses. We use
   * this to avoid invoking compile:compile unless the jar file is
   * missing for ThisProject.
   */
  val exportedProductsCache = new ConcurrentHashMap[(ProjectReference, Configuration), Classpath]()
  def dynamicExportedProductsTask: Def.Initialize[Task[Classpath]] =
    (thisProject, configuration, artifactPath in packageBin, exportedProducts.theTask).flatMap {
      (proj, config, jar, exportedProductsTask) =>
        val key = (LocalProject(proj.id), config)
        val cached = exportedProductsCache.get(key)
        if (jar.exists() && cached != null) task(cached)
        else exportedProductsTask.map { calculated =>
          exportedProductsCache.put(key, calculated)
          calculated
        }
    }

  /**
   * projectDescriptors causes traversals of dependency projects.
   *
   * Cache must be evicted for a project and all its dependents when:
   *
   * - any project changes, all dependent project's caches must be cleared
   */
  private val projectDescriptorsCache = new ConcurrentHashMap[ProjectReference, Map[ModuleRevisionId, ModuleDescriptor]]()
  private def dynamicProjectDescriptorsTask: Def.Initialize[Task[Map[ModuleRevisionId, ModuleDescriptor]]] =
    (thisProject, projectDescriptors.theTask).flatMap { (proj, projectDescriptorsTask) =>
      val key = LocalProject(proj.id)
      val cached = projectDescriptorsCache.get(key)
      if (cached != null) task(cached)
      else (projectDescriptorsTask).map { calculated =>
        projectDescriptorsCache.put(key, calculated)
        calculated
      }
    }

  /**
   * Returns the exhaustive set of projects that depend on the given one
   * (not including itself).
   */
  private[fommil] def dependents(structure: BuildStructure, thisProj: ProjectRef): Seq[ProjectRef] = {
    val dependents = {
      for {
        proj <- structure.allProjects
        upstream <- proj.dependencies
        resolved <- Project.getProject(upstream.project, structure)
      } yield (resolved, proj)
    }.groupBy {
      case (parent, child) => parent
    }.map {
      case (parent, grouped) => (parent, grouped.map(_._2).toSet)
    }

    def deeper(p: ResolvedProject): Set[ResolvedProject] = {
      val deps = dependents.getOrElse(p, Set.empty)
      deps ++ deps.flatMap(deeper)
    }

    // optimised projectRef lookup
    val refs: Map[String, ProjectRef] = structure.allProjectRefs.map { ref =>
      (ref.project, ref)
    }.toMap
    val proj = Project.getProject(thisProj, structure).get
    deeper(proj).map { resolved => refs(resolved.id) }(collection.breakOut)
  }

  private def downstreamAndSelfJars(structure: BuildStructure, log: Logger, proj: ProjectRef): Set[File] = {
    val downstream = dependents(structure, proj).toSet
    for {
      p <- (downstream + proj)
      jar <- allPackageBins(structure, log, p, true)
    } yield jar
  }

  /**
   * Deletes all the packageBins of dependent projects.
   */
  def breakingChangeTask: Def.Initialize[Task[Unit]] =
    (state, thisProjectRef).map { (s, proj) =>
      val structure = Project.extract(s).structure
      downstreamAndSelfJars(structure, s.log, proj).foreach { jar =>
        s.log.info(s"deleting $jar")
        deleteLockedFile(s.log, jar)
      }
    }

  /**
   * Deletes all dependent jars if any inputs are more recent than the
   * oldest output.
   */
  def breakOnChangesTask: Def.Initialize[Task[Unit]] =
    (state, thisProjectRef, sourceDirectories in Compile, resourceDirectories in Compile).map { (s, proj, srcs, ress) =>
      // note, we do not use `sources' or `resources' because they can
      // have transient dependencies on compile.
      val structure = Project.extract(s).structure

      // wasteful that we do this many times when aggregating
      val jars = downstreamAndSelfJars(structure, s.log, proj)

      // this is the expensive bit, we do it exactly as much as we need
      if (jars.nonEmpty) {
        val oldest = jars.map(_.lastModified()).min
        val inputs = for {
          dir <- (srcs ++ ress)
          input <- (dir ** "*").filter(_.isFile).get
        } yield input

        inputs.find(_.lastModified() > oldest).foreach { _ =>
          for {
            jar <- jars
          } deleteLockedFile(s.log, jar)
        }
      }
    }

  /**
   * We want to be sure that this is the last collection of Settings
   * that runs on each project, so we require that the user manually
   * apply these overrides.
   */
  def overrideProjectSettings(configs: Configuration*): Seq[Setting[_]] = Seq(
    eclipseTestsFor := None,
    forcegc := false, // class-monkey
    exportJars := true,
    trackInternalDependencies := TrackLevel.TrackIfMissing,
    transitiveUpdate <<= dynamicTransitiveUpdateTask,
    projectDescriptors <<= dynamicProjectDescriptorsTask,
    breakingChange <<= breakingChangeTask,
    breakOnChanges <<= breakOnChangesTask
  ) ++ configs.flatMap { config =>
      inConfig(config)(
        Seq(
          discoveredMainClasses := Nil,
          lastCompilableJar <<= lastCompilableJarTask,
          packageBin <<= dynamicPackageBinTask,
          dependencyClasspath <<= dynamicDependencyClasspathTask,
          exportedProducts <<= dynamicExportedProductsTask,
          runMain <<= runMain dependsOn deletePackageBinTask
        ) ++ {
            if (config == Test || config.extendsConfigs.contains(Test)) Seq(
              // race condition, compiles start to fail if we do this...
              // compile <<= compile dependsOn deleteAllPackageBinTask,
              test <<= test dependsOn deleteOutdatedPackageBinsTask,
              testOnly <<= testOnly dependsOn deleteOutdatedPackageBinsTask
            )
            else Seq(
              compile <<= compile dependsOn deletePackageBinTask
            )
          }
      )
    }

}

/**
 * Replacement for Package, using NIO and multiple threads.
 *
 * Options are ignored (avoids constructing `Seq[(File, String)]`), we just package up the jar.
 */
object FastPackage {
  import java.io._
  import java.util.zip.{ZipEntry, CRC32}
  import java.util.jar._
  import java.nio._
  import java.nio.file._
  import java.nio.file.attribute._
  import java.util.concurrent.ArrayBlockingQueue

  import scala.concurrent._
  import scala.concurrent.duration._
  import scala.util._
  import scala.collection.JavaConverters._

  import ExecutionContext.Implicits.global

  type Reading = Future[(JarEntry, Array[Byte])]

  /**
   * @param classes the directory containing the classes
   * @param jar     the output file, must not exist
   * @param log     for sbt
   */
  def apply(classes: File, jar: File, options: Seq[PackageOption], log: Logger): Unit = {
    // poor man's fan-out SCP
    val finished: Reading = Future.successful((null, null))
    val failure: Reading = Future.successful((null, null))

    if (!classes.isDirectory) {
      log.warn(s"$classes doesn't exist, dummy package?")
      classes.mkdirs()
    }

    val entries = new ArrayBlockingQueue[Reading](8)
    val base = classes.toPath

    log.debug(s"scanning $classes")
    Future { Files.walkFileTree(base, new Visitor(base, entries)) }.onComplete {
      case Success(_) => entries.put(finished)
      case Failure(_) => entries.put(failure)
    }

    log.info(s"Packaging ${jar.getName}")
    jar.getParentFile.mkdirs()

    val manifest = new Manifest
    val main = manifest.getMainAttributes
    for (option <- options) {
      option match {
        case Package.JarManifest(mergeManifest) => Package.mergeManifests(manifest, mergeManifest)
        case Package.MainClass(mainClassName) => main.put(Attributes.Name.MAIN_CLASS, mainClassName)
        case Package.ManifestAttributes(attributes @ _*) => main.asScala ++= attributes
        case _ => log.warn("Ignored unknown package option " + option)
      }
    }
    Package.setVersion(main)

    val file = new BufferedOutputStream(new FileOutputStream(jar), 10000000)
    val out = new JarOutputStream(file, manifest)

    try {
      out.setLevel(9)

      var done = false
      while (!done) {
        val next = entries.take()
        if (next == finished) done = true
        else if (next == failure) throw new IOException(s"problem scanning $classes")
        else {
          val (entry, data) = Await.result(next, Duration.Inf)

          out.putNextEntry(entry)
          out.write(data)
          out.closeEntry()
        }
      }

    } finally {
      out.close()
    }
  }

  private class Visitor(
    base: Path,
    queue: ArrayBlockingQueue[Reading]
  ) extends SimpleFileVisitor[Path] {
    override def visitFile(file: Path, attrs: BasicFileAttributes): FileVisitResult = {
      // work begins immediately
      val reader = Future {
        val bytes = Files.readAllBytes(file)
        val entry = new JarEntry(base.relativize(file).toString.replace("\\", "/"))

        // don't compress small classes or anything else
        if (attrs.size < 2048 || !file.endsWith(".class")) {
          val crc = new CRC32
          crc.update(bytes)
          entry.setSize(bytes.length.toLong)
          entry.setCrc(crc.getValue)
          entry.setMethod(ZipEntry.STORED)
        }

        (entry, bytes)
      }

      // may block here
      queue.put(reader)

      FileVisitResult.CONTINUE
    }
  }
}
