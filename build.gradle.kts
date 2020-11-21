plugins {
	id("com.github.ben-manes.versions") version "0.36.0"
}

allprojects {
	group = "jcl"
	version = "1.0-SNAPSHOT"

	configurations.all {
		resolutionStrategy {
			// fail eagerly on version conflict (includes transitive dependencies)
			// e.g. multiple different versions of the same dependency (group and name are equal)
			failOnVersionConflict()
		}
	}
}

subprojects {
	apply(plugin = "java")
	apply(plugin = "jacoco")

	configure<JavaPluginExtension> {
		sourceCompatibility = JavaVersion.VERSION_15
		targetCompatibility = JavaVersion.VERSION_15
		withSourcesJar()
		withJavadocJar()
	}

	tasks.named<JavaCompile>("compileJava") {
		options.compilerArgs.addAll(arrayOf(
				"-Xlint:serial", "-Xlint:varargs", "-Xlint:cast", "-Xlint:classfile", "-Xlint:dep-ann", "-Xlint:divzero",
				"-Xlint:empty", "-Xlint:finally", "-Xlint:overrides", "-Xlint:path", "-Xlint:static", "-Xlint:try",
				"-Xlint:fallthrough", "-Xlint:rawtypes", "-Xlint:deprecation", "-Xlint:unchecked", "-Xlint:-options"
				//, "-Xlint:processing" , "-Werror"
		))
	}

	tasks.named<JavaCompile>("compileTestJava") {
		options.compilerArgs.addAll(arrayOf(
				"-Xlint:serial", "-Xlint:-varargs", "-Xlint:cast", "-Xlint:classfile", "-Xlint:dep-ann", "-Xlint:divzero",
				"-Xlint:empty", "-Xlint:finally", "-Xlint:overrides", "-Xlint:path", "-Xlint:static", "-Xlint:try",
				"-Xlint:-fallthrough", "-Xlint:-rawtypes", "-Xlint:-deprecation", "-Xlint:-unchecked", "-Xlint:-options"
				//, "-Xlint:processing"
		))
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
		implementation("com.google.guava:guava:30.0-jre")
		implementation("com.ibm.icu:icu4j:68.1")
		implementation("commons-io:commons-io:2.8.0")
		implementation("org.apache.commons:commons-collections4:4.4")
		implementation("org.apache.commons:commons-lang3:3.11")
		implementation("org.apache.commons:commons-math3:3.6.1")
		implementation("org.apache.commons:commons-text:1.9")
		implementation("org.apfloat:apfloat:1.9.1")
		implementation("org.benf:cfr:0.150")
		implementation("org.glassfish.external:javahelp:2.0.06")
		implementation("org.ow2.asm:asm:9.0")
		implementation("org.ow2.asm:asm-util:9.0")

		compileOnly("org.projectlombok:lombok:1.18.16")
		annotationProcessor("org.projectlombok:lombok:1.18.16")

		testImplementation("org.junit.jupiter:junit-jupiter:5.7.0")

		implementation("org.apache.logging.log4j:log4j-api:2.14.0")
		runtimeOnly("org.apache.logging.log4j:log4j-core:2.14.0")
		testRuntimeOnly("org.apache.logging.log4j:log4j-core:2.14.0")
	}

	tasks.named<Jar>("jar") {
		manifest.attributes["Created-By"] = "${System.getProperty("java.version")} (${System.getProperty("java.specification.vendor")})"
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
		toolVersion = "0.8.6"
	}

	tasks.named<JacocoReport>("jacocoTestReport") {
		reports {
			xml.isEnabled = true
			xml.destination = file("${buildDir}/reports/jacoco/report.xml")
			csv.isEnabled = false
			html.isEnabled = true
			html.destination = file("${buildDir}/reports/jacoco/html")
		}
	}
}

tasks.named<Wrapper>("wrapper") {
	gradleVersion = "6.7.1"
	distributionType = Wrapper.DistributionType.ALL
}