plugins {
    id("com.github.ben-manes.versions") version "0.42.0"
}

allprojects {
    group = "jcl"
    version = "1.0-SNAPSHOT"

    configurations.all {
        resolutionStrategy {
            // fail eagerly on version conflict (includes transitive dependencies)
            // e.g. multiple different versions of the same dependency (group and name are equal)
            failOnVersionConflict()

            force("org.checkerframework:checker-qual:3.9.1")
            force("org.apache.commons:commons-lang3:3.12.0")
        }
    }
}

subprojects {
    apply(plugin = "java")
    apply(plugin = "jacoco")

    configure<JavaPluginExtension> {
        sourceCompatibility = JavaVersion.VERSION_18
        targetCompatibility = JavaVersion.VERSION_18
        withSourcesJar()
//		withJavadocJar()
    }

    tasks.named<JavaCompile>("compileJava") {
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
                //, "-Xlint:processing" , "-Werror"
            )
        )
    }

    tasks.named<JavaCompile>("compileTestJava") {
        options.compilerArgs.addAll(
            arrayOf(
                "-Xlint:serial",
                "-Xlint:-varargs",
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
                "-Xlint:-fallthrough",
                "-Xlint:-rawtypes",
                "-Xlint:-deprecation",
                "-Xlint:-unchecked",
                "-Xlint:-options"
                //, "-Xlint:processing"
            )
        )
        options.compilerArgs.add("-parameters")
    }

    tasks.named<Test>("test") {
        systemProperty("java.awt.headless", "true")

        useJUnitPlatform()
    }

    repositories {
        mavenCentral()
    }

    val annotationProcessor by configurations
    val compileOnly by configurations
    val implementation by configurations
    val runtimeOnly by configurations
    val testImplementation by configurations
    val testRuntimeOnly by configurations

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
    }

    tasks.named<Jar>("jar") {
        manifest.attributes["Created-By"] =
            "${System.getProperty("java.version")} (${System.getProperty("java.specification.vendor")})"
        manifest.attributes["Implementation-Title"] = name
        manifest.attributes["Implementation-Version"] = archiveVersion
    }

    tasks.named<Javadoc>("javadoc") {
        options {
            memberLevel = JavadocMemberLevel.PRIVATE
            header = project.name
            isFailOnError = true
        }
    }

    configure<JacocoPluginExtension> {
        toolVersion = "0.8.8"
    }

    tasks.named<JacocoReport>("jacocoTestReport") {
        dependsOn("test") // tests are required to run before generating the report
        reports {
            xml.required.set(true)
            csv.required.set(false)
            html.required.set(true)
        }
    }
}

tasks.named<Wrapper>("wrapper") {
    gradleVersion = "7.4.2"
    distributionType = Wrapper.DistributionType.ALL
}