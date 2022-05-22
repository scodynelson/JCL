description = "JCL Core"

dependencies {
    implementation("com.ibm.icu:icu4j")
    implementation("commons-io:commons-io")
    implementation("com.google.guava:guava")
    implementation("org.ow2.asm:asm")
    implementation("org.apache.commons:commons-lang3")
    implementation("org.apache.commons:commons-text")
    implementation("org.apache.commons:commons-math3")
    implementation("org.apache.commons:commons-collections4")
    implementation("org.apfloat:apfloat")
    compileOnly("org.projectlombok:lombok")
    annotationProcessor("org.projectlombok:lombok")
    implementation("org.apache.logging.log4j:log4j-api")

    testImplementation("org.junit.jupiter:junit-jupiter")
    testRuntimeOnly("org.apache.logging.log4j:log4j-core")
}