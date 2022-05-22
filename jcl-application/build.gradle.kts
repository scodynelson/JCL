description = "JCL Application"

sourceSets {
    main {
        resources {
            srcDir("src/main/lisp")
        }
    }
}

dependencies {
    implementation(project(":jcl-core"))
    implementation(project(":jcl-compiler"))
    implementation(project(":jcl-reader"))
    implementation(project(":jcl-functions"))

    implementation("org.apache.commons:commons-lang3")
    compileOnly("org.projectlombok:lombok")
    annotationProcessor("org.projectlombok:lombok")

    implementation("info.picocli:picocli")

    implementation("org.apache.logging.log4j:log4j-api")

    runtimeOnly("org.apache.logging.log4j:log4j-core")
    runtimeOnly(fileTree(mapOf("dir" to "compiled-lisp", "include" to "*.jar")))

    testImplementation("org.junit.jupiter:junit-jupiter")
    testImplementation("org.assertj:assertj-core")
}

tasks.jar {
    manifest {
        attributes["Main-Class"] = "jcl.system.JCL"
    }
}

//def java_home = System.getenv('JAVA_HOME')
//task link(type: Exec) {
//	dependsOn 'clean'
//	dependsOn 'jar'
//
//	workingDir 'build'
//
//	commandLine "${java_home}/bin/jlink", '--module-path', "libs${File.pathSeparatorChar}${java_home}/jmods",
//			'--add-modules', 'cli', '--launcher', 'cli=cli/cli.Main', '--output', 'dist', '--strip-debug',
//			'--compress', '2', '--no-header-files', '--no-man-pages'
//}

//********* LISP JAR GENERATION *********//

//def lispSourceTree = fileTree(dir: 'src/main/lisp', include: '**/*.lisp')
val lispCompiledTree = fileTree(mapOf("dir" to "compiled-lisp", "include" to "**/*.jar"))

tasks.create("cleanLispJars") {
    val compiledLispDirectory = "$projectDir/compiled-lisp"
    File(compiledLispDirectory).mkdirs()

    lispCompiledTree.forEach { file ->
        doLast {
            println("Removing Lisp Jar")
            println(file.absolutePath)

            file.delete()
        }
    }
}
tasks.named<Delete>("clean") {
    dependsOn("cleanLispJars")
}

val lispSourceFiles = listOf(
    "jcl/compiler/base-macro-lambdas.lisp",
    "jcl/sequences/sequences.lisp",
    "jcl/compiler/macros.lisp",
    "jcl/iterators/iterators.lisp",
    "jcl/characters/characters.lisp",
    "jcl/pathnames/pathnames.lisp",
    "jcl/symbols/symbols.lisp",
    "jcl/reader/reader.lisp",
    "jcl/strings/strings.lisp",
    "jcl/streams/streams.lisp",
    "jcl/packages/packages.lisp",
    "jcl/lists/lists.lisp",
    "jcl/numbers/numbers.lisp",
    "jcl/hashtables/hashtables.lisp",
    "jcl/environment/environment.lisp",
    "jcl/structures/structures.lisp"
)

fun createLispGenerationTask(taskName: String, lispSourceFile: String): Task {
    return tasks.create(taskName, JavaExec::class) {
        mainClass.set("jcl.system.JCL")
        classpath = sourceSets["main"].runtimeClasspath
        args(
            "--compileFileSrcDir=$projectDir/src/main/lisp/${lispSourceFile}",
            "--compileFileDestDir=$projectDir/compiled-lisp/"
        )
        // TODO: Fix the need to prepend a '/' to the directory name. This is a problem with the Pathname object.
    }
}

//def createJarUnpackingTask(def lispSourceFileName, def taskName, def dependencies) {
//	return tasks.create(name: taskName, type: Copy, dependsOn: dependencies) {
//		from zipTree("$projectDir/compiled-lisp/${lispSourceFileName}.jar")
//		into compileJava.destinationDir
//	}
//}

tasks.create("generateLispSource") {
    dependsOn("classes")
//	var generationLispSourceDependencies = []
//	var unpackingLispSourceDependencies = []
    var previousGenerationTask = "classes"
//	var previousUnpackingTask = []

    lispSourceFiles.forEach { lispSourceFile ->
        val lispSourceFileName = lispSourceFile.substring(
            lispSourceFile.lastIndexOf("/") + 1,
            lispSourceFile.lastIndexOf(".")
        )

        val generationTaskName = "generate-${lispSourceFileName}-jar"
        val generatedTask: Task = createLispGenerationTask(generationTaskName, lispSourceFile)

        generatedTask.dependsOn(previousGenerationTask)

        previousGenerationTask = generationTaskName
//		generationLispSourceDependencies.add(generationTaskName)

//		String unpackingTaskName = "unpack-${lispSourceFileName}-Jar"
//		createJarUnpackingTask(lispSourceFileName, unpackingTaskName, generationLispSourceDependencies + unpackingLispSourceDependencies)
//		unpackingLispSourceDependencies.add(unpackingTaskName)
    }
    tasks.named<DefaultTask>("assemble") {
        dependsOn(previousGenerationTask)
    }
}
//assemble.dependsOn(generateLispSource)