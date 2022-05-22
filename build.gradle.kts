plugins {
    id("com.github.ben-manes.versions") version "0.42.0"
    id("java")
    id("jacoco")
}

// ************************************************
// Build Configurations
// ************************************************

configurations.all {
    resolutionStrategy {
        // fail eagerly on version conflict (includes transitive dependencies)
        // e.g. multiple different versions of the same dependency (group and name are equal)
        failOnVersionConflict()

        force("org.checkerframework:checker-qual:3.9.1")
        force("org.apache.commons:commons-lang3:3.12.0")
    }
}

// ************************************************
// Plugin Configurations
// ************************************************

java {
    sourceCompatibility = JavaVersion.VERSION_18
    targetCompatibility = JavaVersion.VERSION_18
    withSourcesJar()
//		withJavadocJar()
}

jacoco {
    toolVersion = "0.8.8"
}

// ************************************************
// Project Description
// ************************************************

group = "jcl"
version = "1.0-SNAPSHOT"
description = "JCL Application"

// ************************************************
// Dependency Configuration
// ************************************************

repositories {
    mavenCentral()
}

dependencies {
    implementation("com.google.guava:guava:31.1-jre")
    implementation("com.ibm.icu:icu4j:71.1")
    implementation("commons-io:commons-io:2.11.0")
    implementation("org.apache.commons:commons-collections4:4.4")
    implementation("org.apache.commons:commons-lang3:3.12.0")
    implementation("org.apache.commons:commons-math3:3.6.1")
    implementation("org.apache.commons:commons-text:1.9")
    implementation("org.apfloat:apfloat:1.10.1")
    implementation("org.benf:cfr:0.152")
    implementation("org.glassfish.external:javahelp:2.0.06")
    implementation("org.ow2.asm:asm:9.3")
    implementation("org.ow2.asm:asm-util:9.3")
    implementation("info.picocli:picocli:4.6.3")

    compileOnly("org.projectlombok:lombok:1.18.24")
    annotationProcessor("org.projectlombok:lombok:1.18.24")

    testImplementation("org.junit.jupiter:junit-jupiter:5.8.2")
    testImplementation("org.assertj:assertj-core:3.22.0")

    implementation("org.apache.logging.log4j:log4j-api:2.17.2")
    runtimeOnly("org.apache.logging.log4j:log4j-core:2.17.2")
    testRuntimeOnly("org.apache.logging.log4j:log4j-core:2.17.2")

    runtimeOnly(fileTree(mapOf("dir" to "compiled-lisp", "include" to "*.jar")))
}

// ************************************************
// Java Compiler
// ************************************************

tasks.compileJava {
    options.compilerArgs.addAll(
        arrayOf(
            "-Xlint:serial",
            "-Xlint:varargs",
            "-Xlint:cast",
            "-Xlint:classfile",
            "-Xlint:dep-ann",
            "-Xlint:divzero",
            "-Xlint:empty",
            "-Xlint:finally",
            "-Xlint:overrides",
            "-Xlint:path",
            "-Xlint:static",
            "-Xlint:try",
            "-Xlint:fallthrough",
            "-Xlint:rawtypes",
            "-Xlint:deprecation",
            "-Xlint:unchecked",
            "-Xlint:-options"
            //, "-Xlint:processing",
            // "-Werror"
        )
    )
}

tasks.compileTestJava {
    options.compilerArgs.addAll(
        arrayOf(
            "-Xlint:serial",
            "-Xlint:varargs",
            "-Xlint:cast",
            "-Xlint:classfile",
            "-Xlint:dep-ann",
            "-Xlint:divzero",
            "-Xlint:empty",
            "-Xlint:finally",
            "-Xlint:overrides",
            "-Xlint:path",
            "-Xlint:static",
            "-Xlint:try",
            "-Xlint:fallthrough",
            "-Xlint:rawtypes",
            "-Xlint:deprecation",
            "-Xlint:unchecked",
            "-Xlint:-options"
            //, "-Xlint:processing",
            // "-Werror"
        )
    )
}

// ************************************************
// Lisp Source Sets
// ************************************************

sourceSets {
    main {
        resources {
            srcDir("src/main/lisp")
        }
    }
}

// ************************************************
// Lisp JAR Generation
// ************************************************

//val lispSourceTree = fileTree(mapOf("dir" to "src/main/lisp", "include" to "**/*.lisp"))
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
tasks.clean {
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

fun createJarUnpackingTask(taskName: String, lispSourceFileName: String): Task {
    return tasks.create(taskName, Copy::class) {
//        group = "unzip"
        from(zipTree("$projectDir/compiled-lisp/${lispSourceFileName}.jar"))
        include("**/*.class")
        into("build/classes/java/main")
    }
}

tasks.create("generateLispSource") {
    dependsOn("classes")
    var lastGeneratedTask = "classes"

    lispSourceFiles.forEach { lispSourceFile ->
        val lispSourceFileName = lispSourceFile.substring(
            lispSourceFile.lastIndexOf("/") + 1,
            lispSourceFile.lastIndexOf(".")
        )

        val generationTaskName = "generate-${lispSourceFileName}-jar"
        val generatedTask: Task = createLispGenerationTask(generationTaskName, lispSourceFile)

        generatedTask.dependsOn(lastGeneratedTask)

        // TODO: eventaully enable unpacking
//        val unpackTaskName = "unpack-${lispSourceFileName}-jar"
//        val unpackTask: Task = createJarUnpackingTask(unpackTaskName, lispSourceFileName.replace(".lisp", ".jar"))

//        unpackTask.dependsOn(generatedTask)

//        lastGeneratedTask = unpackTaskName
        lastGeneratedTask = generationTaskName
    }
    tasks.assemble {
        dependsOn(lastGeneratedTask)
    }
}

// ************************************************
// JavaDoc
// ************************************************

tasks.javadoc {
    options {
        memberLevel = JavadocMemberLevel.PRIVATE
        header = project.name
        isFailOnError = true
    }
}

// ************************************************
// Tests
// ************************************************

tasks.test {
    systemProperty("java.awt.headless", "true")

    useJUnitPlatform()

    finalizedBy(tasks.jacocoTestReport) // report is always generated after tests run
}

// ************************************************
// JaCoCo
// ************************************************

tasks.jacocoTestReport {
    dependsOn("test") // tests are required to run before generating the report
    reports {
        xml.required.set(true)
        csv.required.set(false)
        html.required.set(true)
    }
}

// ************************************************
// Jar - Main Class
// ************************************************

tasks.jar {
    manifest {
        attributes["Main-Class"] = "jcl.system.JCL"
    }
}

// ************************************************
// Gradle Wrapper
// ************************************************

tasks.wrapper {
    gradleVersion = "7.4.2"
    distributionType = Wrapper.DistributionType.ALL
}