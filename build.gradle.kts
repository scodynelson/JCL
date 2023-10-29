plugins {
    id("com.github.ben-manes.versions") version "0.49.0"
    id("java")
    id("jacoco")
    id("org.sonarqube") version "4.4.1.3373"
    id("com.github.johnrengelman.shadow") version "8.1.1"
}

// ************************************************
// Build Configurations
// ************************************************

configurations.all {
    resolutionStrategy {
        // fail eagerly on version conflict (includes transitive dependencies)
        // e.g. multiple different versions of the same dependency (group and name are equal)
        failOnVersionConflict()
    }
}

// ************************************************
// Plugin Configurations
// ************************************************

java {
    sourceCompatibility = JavaVersion.VERSION_21
    targetCompatibility = JavaVersion.VERSION_21
    withSourcesJar()
//		withJavadocJar()
}

jacoco {
    toolVersion = "0.8.11"
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
    implementation(platform("org.apache.logging.log4j:log4j-bom:2.21.1"))
    implementation(platform("org.ow2.asm:asm-bom:9.6"))
    implementation(platform("org.junit:junit-bom:5.10.0"))

    implementation("com.google.guava:guava:32.1.3-jre")
    implementation("com.ibm.icu:icu4j:73.2")
    implementation("commons-io:commons-io:2.15.0")
    implementation("org.apache.commons:commons-collections4:4.4")
    implementation("org.apache.commons:commons-lang3:3.13.0")
    implementation("org.apache.commons:commons-math3:3.6.1")
    implementation("org.apache.commons:commons-text:1.11.0")
    implementation("org.apfloat:apfloat:1.12.0")
    implementation("org.benf:cfr:0.152")
    implementation("org.ow2.asm:asm")
    implementation("org.ow2.asm:asm-util")
    implementation("info.picocli:picocli:4.7.5")

    compileOnly("org.projectlombok:lombok:1.18.30")
    annotationProcessor("org.projectlombok:lombok:1.18.30")

    testImplementation("org.junit.jupiter:junit-jupiter")
    testRuntimeOnly("org.junit.platform:junit-platform-launcher")

    testImplementation("org.assertj:assertj-core:3.24.2")

    implementation("org.apache.logging.log4j:log4j-api")
    runtimeOnly("org.apache.logging.log4j:log4j-core")
    testRuntimeOnly("org.apache.logging.log4j:log4j-core")

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
    "jcl/base-macro-lambdas.lisp",
    "jcl/sequences.lisp",
    "jcl/macros.lisp",
    "jcl/iterators.lisp",
    "jcl/conditions.lisp",
    "jcl/characters.lisp",
    "jcl/pathnames.lisp",
    "jcl/symbols.lisp",
    "jcl/strings.lisp",
    "jcl/streams.lisp",
    "jcl/reader.lisp",
    "jcl/packages.lisp",
    "jcl/lists.lisp",
    "jcl/numbers.lisp",
    "jcl/hashtables.lisp",
    "jcl/setf.lisp",
    "jcl/files.lisp",
    "jcl/environment.lisp",
    "jcl/structures.lisp"
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
    useJUnitPlatform()
    finalizedBy(tasks.jacocoTestReport) // report is always generated after tests run
}

// ************************************************
// JaCoCo
// ************************************************

tasks.jacocoTestReport {
    dependsOn(tasks.test) // tests are required to run before generating the report
    reports {
        xml.required = true
        csv.required = false
        html.required = true
    }
}

// ************************************************
// SonarQube
// ************************************************

sonarqube {
    properties {
        property("sonar.projectKey", "scodynelson_JCL")
        property("sonar.organization", "scodynelson")
        property("sonar.host-url", "https://sonarcloud.io")
        property("sonar.skipCompile", true)
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
// Shadow Jar
// ************************************************

tasks.shadowJar {
    dependsOn("assemble")
}

// ************************************************
// Gradle Wrapper
// ************************************************

tasks.wrapper {
    gradleVersion = "8.4"
    distributionType = Wrapper.DistributionType.ALL
}