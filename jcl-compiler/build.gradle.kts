description = "JCL Compiler"

dependencies {
    implementation(project(":jcl-core"))
    implementation(project(":jcl-reader"))

    implementation("commons-io:commons-io")
    implementation("org.benf:cfr")
    implementation("org.ow2.asm:asm")
    implementation("org.ow2.asm:asm-util")
    compileOnly("org.projectlombok:lombok")
    annotationProcessor("org.projectlombok:lombok")
    implementation("org.apache.logging.log4j:log4j-api")

    testImplementation("org.junit.jupiter:junit-jupiter")
    testRuntimeOnly("org.apache.logging.log4j:log4j-core")
}