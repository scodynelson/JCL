description = "JCL Reader"

dependencies {
	implementation(project(":jcl-core"))

	compileOnly("org.projectlombok:lombok")
	annotationProcessor("org.projectlombok:lombok")
	implementation("org.apache.logging.log4j:log4j-api")

	testImplementation("org.junit.jupiter:junit-jupiter:5.6.2")
	testRuntimeOnly("org.apache.logging.log4j:log4j-core")
}